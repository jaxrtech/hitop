#include <iostream>
#include <iomanip>
#include <fstream>
#include <sstream>
#include <ctime>

#include <thrust/device_new.h>
#include <thrust/device_delete.h>
#include <thrust/host_vector.h>
#include <thrust/device_vector.h>
#include <thrust/random.h>

#include "util.h"

bool try_read_file(std::string input_path, thrust::host_vector<uint8_t>* data, std::streamsize* size)
{
	// open the file
	std::ifstream file(input_path, std::ios::binary);
	if (!file.is_open()) {
		std::cerr << "error: cannot open input file" << std::endl;
		return false;
	}

	// prevent eating new lines in binary mode
	file.unsetf(std::ios::skipws);

	// get its size:
	file.seekg(0, std::ios::end);
	*size = file.tellg();
	file.seekg(0, std::ios::beg);

	// reserve capacity
	data->clear();
	data->reserve(*size);

	// read the data:
	data->insert(data->begin(),
			     std::istream_iterator<uint8_t>(file),
			     std::istream_iterator<uint8_t>());

	return true;
}

namespace hitop {
namespace algo {

namespace util {

__host__ __device__
uint32_t hash32(uint32_t a)
{
	a = (a + 0x7ed55d16) + (a << 12);
	a = (a ^ 0xc761c23c) ^ (a >> 19);
	a = (a + 0x165667b1) + (a << 5);
	a = (a + 0xd3a2646c) ^ (a << 9);
	a = (a + 0xfd7046c5) + (a << 3);
	a = (a ^ 0xb55a4f09) ^ (a >> 16);
	return a;
}

__host__ __device__
uint64_t hash64(uint64_t key)
{
	key = (~key) + (key << 21); // key = (key << 21) - key - 1;
	key = key ^ (key >> 24);
	key = (key + (key << 3)) + (key << 8); // key * 265
	key = key ^ (key >> 14);
	key = (key + (key << 2)) + (key << 4); // key * 21
	key = key ^ (key >> 28);
	key = key + (key << 31);
	return key;
}

__host__ __device__
uint32_t hash64_32(uint64_t key)
{
	key = (~key) + (key << 18); // key = (key << 18) - key - 1;
	key = key ^ (key >> 31);
	key = key * 21; // key = (key + (key << 2)) + (key << 4);
	key = key ^ (key >> 11);
	key = key + (key << 6);
	key = key ^ (key >> 22);
	return static_cast<uint32_t>(key);
}

} // namespace util

namespace program_descriptor {

struct ProgramDescriptor
{
	size_t pos;
	size_t length;
};


struct Settings
{
	size_t min_program_size;
	size_t max_program_size;
	size_t block_size;
};

struct create_random : public thrust::unary_function<size_t, ProgramDescriptor>
{
private:
	program_descriptor::Settings* settings;

public:
	thrust::device_ptr<program_descriptor::Settings> settings_ptr;

	create_random(thrust::device_ptr<program_descriptor::Settings> settings_ptr)
		: settings_ptr(settings_ptr)
		, settings(settings_ptr.get())
	{ }

	__host__ __device__
		ProgramDescriptor operator()(size_t index)
	{
		uint32_t seed = hitop::algo::util::hash64_32(index);

		thrust::default_random_engine rng(seed);
		thrust::uniform_int_distribution<size_t> program_length(settings->min_program_size, settings->max_program_size);

		ProgramDescriptor descriptor;
		descriptor.pos = settings->block_size * index;
		descriptor.length = program_length(rng);

		return descriptor;
	}
};

} // namespace program_descriptor

namespace program {

struct fill : public thrust::unary_function<program_descriptor::ProgramDescriptor, uint8_t>
{
	thrust::device_ptr<uint8_t> pool;

	fill(thrust::device_ptr<uint8_t> pool)
		: pool(pool)
	{
		
	}

	__host__ __device__
		uint8_t operator()(program_descriptor::ProgramDescriptor descriptor)
	{
		uint32_t seed = hitop::algo::util::hash64_32(descriptor.pos);

		thrust::default_random_engine rng(seed);
		thrust::uniform_int_distribution<uint8_t> byte_range(0, UINT8_MAX);

		for (size_t i = descriptor.pos;
			 i < descriptor.pos + descriptor.length;
			 i++) {

			pool[i] = byte_range(rng);
		}

		return 0;
	}
};

} // namespace program

}
}

int main(int argc, char* argv[])
{
	hitop::util::AppSettings settings;
	if (!hitop::util::try_parse_settings(argc, argv, settings)) {
		hitop::util::output_usage(std::cerr);
		return -1;
	}

	thrust::host_vector<uint8_t> target_h;
	std::streamsize target_length;
	bool result = try_read_file(settings.input_path, &target_h, &target_length);
	if (!result) {
		return -1;
	}

	if (target_length <= 0) {
		std::cerr << "error: file is empty and cannot be compressed" << std::endl;
		return -1;
	}

	// Actually do stuff
	hitop::util::output_header(std::cout);

	std::cout
		<< "info: started at " << hitop::util::get_timestamp_readable() 
		<< std::endl;

	std::cout
		<< "input file: '" << settings.input_path << std::endl
		<< "size: " << target_length << std::endl;

	// Copy to device
	std::cout
		<< "debug: copying target file to device..."
		<< std::endl;

	thrust::device_vector<char> target_d = target_h;

	std::cout
		<< "debug: done"
		<< std::endl
		<< std::endl;

	//
	// Run the algo
	//

	// Settings
	using namespace hitop::algo;
	using hitop::algo::program_descriptor::ProgramDescriptor;

	const size_t population_count = 100;
	assert(population_count > 0);

	program_descriptor::Settings program_settings_h;
	program_settings_h.min_program_size = target_length * (0.75);
	program_settings_h.max_program_size = target_length;
	program_settings_h.block_size = target_length;

	auto program_settings_d = thrust::device_new<program_descriptor::Settings>();
	cudaMemcpy(program_settings_d.get(), &program_settings_h, sizeof(program_descriptor::Settings), cudaMemcpyHostToDevice);

	const size_t program_pool_size = population_count * target_length;
	assert(program_pool_size > 0);
	
	// Setup vectors
	const size_t program_descriptors_data_size = sizeof(program_descriptor::ProgramDescriptor) * population_count;

	std::cout
		<< "debug: allocating "
		<< population_count << " program descriptors "
		<< "(" << program_descriptors_data_size << " bytes)..."
		<< std::endl;

	thrust::device_vector<program_descriptor::ProgramDescriptor> program_descriptors(population_count);

	std::cout
		<< "debug: done"
		<< std::endl
		<< std::endl;

	//

	const size_t program_pool_data_size = sizeof(uint8_t) * program_pool_size;

	std::cout
		<< "debug: allocating program pool "
		<< "(" << program_pool_data_size << " bytes)..."
		<< std::endl;

	thrust::device_vector<uint8_t> program_pool(program_pool_size);
	
	std::cout
		<< "debug: done"
		<< std::endl
		<< std::endl;

	// Initialize program descriptors to random lengths
	std::cout
		<< "debug: initializing program descriptors to random lengths"
		<< std::endl;

	thrust::transform(thrust::counting_iterator<size_t>(0),
					  thrust::counting_iterator<size_t>(program_descriptors.size()),
					  program_descriptors.begin(),
					  program_descriptor::create_random(program_settings_d));
	
	std::cout
		<< "debug: done"
		<< std::endl
		<< std::endl;

	// Fill program pool with random values per each program descriptor
	std::cout
		<< "debug: initializing all programs to random data"
		<< std::endl;

	thrust::for_each(program_descriptors.begin(),
					 program_descriptors.end(),
					 program::fill(program_pool.data()));

	std::cout
		<< "debug: done"
		<< std::endl
		<< std::endl;

	//
	// End algo
	//

	// Clear any `device_ptr`s
	thrust::device_delete(program_settings_d);

	std::cout
		<< "info: algorithm ran to completion!" << std::endl
		<< "press any key to exit..." << std::endl;

	std::cin.ignore();
	return 0;
}

