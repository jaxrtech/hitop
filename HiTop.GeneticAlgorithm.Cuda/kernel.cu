#include <iostream>
#include <iomanip>
#include <fstream>
#include <sstream>
#include <ctime>

#include <thrust/device_new.h>
#include <thrust/device_delete.h>
#include <thrust/host_vector.h>
#include <thrust/device_vector.h>

#include <thrust/sort.h>
#include <thrust/logical.h>
#include <thrust/random.h>

#include "util.h"

namespace hitop {
namespace algo {

struct Settings
{
	size_t min_program_size;
	size_t max_program_size;
	size_t block_size;
};

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

typedef float score_t;

struct ProgramDescriptor
{
	size_t pos;
	size_t length;
	score_t score;
};


struct create_random : public thrust::unary_function<size_t, ProgramDescriptor>
{
private:
	algo::Settings* settings;
	thrust::device_ptr<algo::Settings> settings_ptr;

public:
	create_random(thrust::device_ptr<algo::Settings> settings_ptr)
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
		descriptor.score = NAN;

		return descriptor;
	}
};

struct greater_score : public thrust::binary_function<ProgramDescriptor, ProgramDescriptor, bool>
{
	__host__ __device__
		bool operator()(const ProgramDescriptor& a, const ProgramDescriptor& b)
	{
		return a.score > b.score;
	}
};

struct score : public thrust::unary_function<ProgramDescriptor, score_t>
{
	__host__ __device__
		score_t operator()(const ProgramDescriptor& x)
	{
		return x.score;
	}
};

} // namespace program_descriptor

namespace selection_result {

enum class Mode { Single, Pair };

struct SelectionResult
{
	Mode mode;

	union {
		struct { size_t single_index; };

		struct {
			size_t parent_index_a;
			size_t parent_index_b;
		};
	};
};

__host__ __device__
SelectionResult create_single_index(const size_t index)
{
	SelectionResult result;
	result.mode = Mode::Single;
	result.single_index = index;

	return result;
}

__host__ __device__
SelectionResult create_pair_indices(const size_t parent_index_a, const size_t parent_index_b)
{
	SelectionResult result;
	result.mode = Mode::Pair;
	result.parent_index_a = parent_index_a;
	result.parent_index_b = parent_index_b;

	return result;
}

struct from_index : public thrust::unary_function<size_t, SelectionResult>
{
	__host__ __device__
		SelectionResult operator()(size_t index)
	{
		return create_single_index(index);
	}
};

} // namespace selection_result

namespace program {

struct fill : public thrust::unary_function<program_descriptor::ProgramDescriptor&, void>
{
private:
	thrust::device_ptr<uint8_t> pool;

public:
	fill(thrust::device_ptr<uint8_t> pool)
		: pool(pool)
	{ }

	__host__ __device__
		void operator()(program_descriptor::ProgramDescriptor& descriptor)
	{
		uint32_t seed = hitop::algo::util::hash64_32(descriptor.pos);

		thrust::default_random_engine rng(seed);
		thrust::uniform_int_distribution<uint8_t> byte_range(0, UINT8_MAX);

		for (size_t i = descriptor.pos;
			 i < descriptor.pos + descriptor.length;
			 i++) {

			pool[i] = byte_range(rng);
		}
	}
};

struct score : public thrust::unary_function<program_descriptor::ProgramDescriptor&, void>
{
private:
	thrust::device_ptr<uint8_t> pool;

public:
	score(thrust::device_ptr<uint8_t> pool)
		: pool(pool)
	{
	}

	__host__ __device__
		void operator()(program_descriptor::ProgramDescriptor& descriptor)
	{
		// TODO: Dummy scoring function

		uint32_t seed = hitop::algo::util::hash64_32(descriptor.pos);

		thrust::default_random_engine rng(seed);
		thrust::uniform_real_distribution<float> score_range(0, descriptor.length);

		descriptor.score = score_range(rng);
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
	bool result = hitop::util::try_read_file(settings.input_path, &target_h, &target_length);
	if (!result) {
		return -1;
	}

	if (target_length <= 0) {
		std::cerr << "error: todo: file is empty and will not be compressed" << std::endl;
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

	// Wait for CUDA to initialize
	std::cout
		<< "info: waiting for CUDA to initialize and warm up..."
		<< std::endl;

	cudaFree(0);

	std::cout
		<< "info: done"
		<< std::endl
		<< std::endl;

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
	// Set algorithm Settings
	//

	using namespace hitop;
	using namespace hitop::algo;
	using algo::selection_result::SelectionResult;
	using algo::program_descriptor::ProgramDescriptor;

	const size_t population_count = 100;
	assert(population_count > 0);

	const size_t selection_elites = 2;

	const bool enable_stats_output = true;
	const size_t generations_per_stats_output = 1;

	algo::Settings program_settings_h;
	program_settings_h.min_program_size = target_length * (0.75);
	program_settings_h.max_program_size = target_length;
	program_settings_h.block_size = target_length;

	thrust::device_ptr<algo::Settings> program_settings_d = thrust::device_new<algo::Settings>();
	cudaMemcpy(program_settings_d.get(), &program_settings_h, sizeof(algo::Settings), cudaMemcpyHostToDevice);

	const size_t program_pool_size = population_count * target_length;
	assert(program_pool_size > 0);
	
	size_t generation_num = 0;
	
	//
	// Allocate device_vectors
	//

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
		<< "debug: allocating program pools "
		<< "(" << program_pool_data_size << " bytes)..."
		<< std::endl;

	thrust::device_vector<uint8_t> program_pool(program_pool_size);

	thrust::device_vector<uint8_t> program_pool_temp(program_pool_size);
	
	std::cout
		<< "debug: done"
		<< std::endl
		<< std::endl;

	//

	const size_t selection_results_data_size = sizeof(SelectionResult) * program_pool_size;

	std::cout
		<< "debug: allocating selection results "
		<< "(" << selection_results_data_size << " bytes)..."
		<< std::endl;

	thrust::device_vector<SelectionResult> selection_results(program_pool_size);

	std::cout
		<< "debug: done"
		<< std::endl
		<< std::endl;

	//
	// Initialize initial generation
	//

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
	// Evaluate fitness of initial generation 
	//

	thrust::for_each(program_descriptors.begin(),
					 program_descriptors.end(),
					 program::score(program_pool.data()));

	//
	// Sort program descriptors by their scores descending
	//

	using namespace thrust::placeholders;

	thrust::sort(program_descriptors.begin(),
				 program_descriptors.end(),
				 program_descriptor::greater_score());

	//
	// Calculate statistics on current scores if necessary
	//

	if (enable_stats_output
		&& generation_num % generations_per_stats_output == 0) {

		ProgramDescriptor best = program_descriptors.front();
		ProgramDescriptor worst = program_descriptors.back();

		auto sum = thrust::transform_reduce(program_descriptors.begin(),
					                        program_descriptors.end(),
											program_descriptor::score(),
								            0,
								            thrust::plus<program_descriptor::score_t>());

		auto avg = sum / program_descriptors.size();

		std::cout
			<< "gen " << generation_num << ": "
			<< "best = " << best.score << " | "
			<< "avg = " << avg << " | "
			<< "worst = " << worst.score
			<< std::endl;
	}

	//
	// Selection process
	//


	//
	// Select elites if setting specified
	//

	auto selection_results_start = selection_results.begin();

	if (selection_elites > 0) {
		thrust::transform(thrust::counting_iterator<size_t>(0),
						  thrust::counting_iterator<size_t>(selection_elites),
						  selection_results_start,
						  selection_result::from_index());

		thrust::advance(selection_results_start, selection_elites);
	}

	//
	// Run the selection method on the rest
	//

	// TODO: Finish the rest of this

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

