#include <iostream>
#include <iomanip>
#include <fstream>
#include <sstream>

#include <thrust/device_new.h>
#include <thrust/device_delete.h>
#include <thrust/host_vector.h>
#include <thrust/device_vector.h>

#include <thrust/sort.h>
#include <thrust/logical.h>
#include <thrust/random.h>

#include "util.h"

// Monkey patch onto thrust
namespace thrust {

template<typename T>
thrust::device_ptr<T> device_new(const T& host_ptr)
{
	return thrust::device_new<T>(thrust::device_malloc<T>(1),
								 host_ptr);
}

}

namespace hitop {
namespace algo {

typedef std::streamsize buffer_size_t;

struct Settings
{
	buffer_size_t min_program_size;
	buffer_size_t max_program_size;
	buffer_size_t block_size;
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

__host__ __device__
uint32_t get_entropy_with_ptr(const void* address)
{
	auto entropy = reinterpret_cast<uintptr_t>(address);
	uint32_t seed = util::hash32(entropy) ^ util::hash32(clock());

	return seed;
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
	Settings* settings_;

	//algo::util::RandomContext* random_context_;

public:
	create_random(thrust::device_ptr<algo::Settings> settings_ptr)
		: settings_(settings_ptr.get())
	{ }

	__host__ __device__
		ProgramDescriptor operator()(size_t index)
	{
		uint32_t seed = util::get_entropy_with_ptr(this);
		thrust::default_random_engine rng(seed);
		thrust::uniform_int_distribution<size_t> program_length(settings_->min_program_size, settings_->max_program_size);

		ProgramDescriptor descriptor;
		descriptor.pos = settings_->block_size * index;
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

// Import type into `algo` namespace
typedef typename program_descriptor::score_t score_t;

namespace selection_result {

struct SelectionResult
{
	enum class Mode { Single, Pair };

	Mode mode;
	
	// The destination index of the child program descriptor
	size_t destination_index;

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
	result.mode = SelectionResult::Mode::Single;
	result.destination_index = index;
	result.single_index = index;

	return result;
}

__host__ __device__
SelectionResult create_pair_indices(const size_t destination_index,
									const size_t parent_index_a,
									const size_t parent_index_b)
{
	SelectionResult result;
	result.mode = SelectionResult::Mode::Pair;
	result.destination_index = destination_index;
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


typedef selection_result::SelectionResult SelectionResult;
typedef program_descriptor::ProgramDescriptor ProgramDescriptor;

namespace selection_method {

struct tournament_selection : public thrust::unary_function<size_t, SelectionResult>
{
	struct Context
	{
		ProgramDescriptor* program_descriptors;
		size_t program_descriptors_count;

		uint32_t rounds;
	};


#define DEBUG_PRINT_LINE() printf("%d\n", __LINE__);

private:
	Context *context_;

	thrust::default_random_engine rng_;
	
	typedef thrust::uniform_int_distribution<size_t> indices_range_distribution;
	indices_range_distribution indices_range_;

	__host__ __device__
	size_t get_random_index()
	{
		return indices_range_(rng_);
	}

	__host__ __device__
	size_t get_next_winner_index()
	{
		ProgramDescriptor *program_descriptors = context_->program_descriptors;

		auto cur_idx = get_random_index();
		auto cur_score = program_descriptors[cur_idx].score;

		for (uint32_t i = 0; i < context_->rounds; i++) {
			auto other_idx = get_random_index();
			auto other_score = program_descriptors[other_idx].score;

			if (other_score > cur_score) {
				cur_idx = other_idx;
			}
		}

		return cur_idx;
	}


public:
	tournament_selection(thrust::device_ptr<Context> context_ptr)
		: context_(context_ptr.get())
	{
		rng_ = thrust::default_random_engine(util::get_entropy_with_ptr(this));
	}

	__host__ __device__
	SelectionResult operator()(size_t index)
	{
		// Only now can we dereference `context_` since we are now dereferencing a device pointer on
		// the device versus in the constructor which is solely on the host.
		indices_range_ = indices_range_distribution(0, context_->program_descriptors_count);

		return selection_result::create_pair_indices(index, index, get_next_winner_index());
	}
};

} // namespace selection_method

namespace crossover_method {

struct point_crossover : public thrust::unary_function<SelectionResult&, void>
{
	struct ProgramDesciptorPair
	{
		const ProgramDescriptor& a;
		const ProgramDescriptor& b;

		__host__ __device__
		ProgramDesciptorPair(const ProgramDescriptor& a_, const ProgramDescriptor& b_)
			: a(a_)
			, b(b_)
		{ }
	};

private:
	algo::Settings* settings_;

	ProgramDescriptor* descriptors_old_;
	ProgramDescriptor* descriptors_new_;

	uint8_t* pool_old_;
	uint8_t* pool_new_;
	
	size_t pool_length_;

	thrust::default_random_engine rng_;

	__host__ __device__
		ProgramDesciptorPair get_descriptor_pair(const SelectionResult& result)
	{
		if (result.mode != SelectionResult::Mode::Pair) {
			printf("error: get_descriptor_pair() called with result of `Mode::Single`");
			exit(-1);
		}

		size_t a_idx = result.parent_index_a;
		size_t b_idx = result.parent_index_b;

		ProgramDescriptor& a_descriptor = descriptors_old_[a_idx];
		ProgramDescriptor& b_descriptor = descriptors_old_[b_idx];

		return ProgramDesciptorPair(a_descriptor, b_descriptor);
	}

	__host__ __device__
		size_t get_child_length(const SelectionResult& result)
	{
		switch (result.mode)
		{
		case SelectionResult::Mode::Single: {
			auto i = result.single_index;

			ProgramDescriptor descriptor = descriptors_old_[i];
			return descriptor.length;
		}

		case SelectionResult::Mode::Pair: {
			auto pair = get_descriptor_pair(result);

			size_t min_length = thrust::min(pair.a.length, pair.b.length);
			size_t max_length = thrust::max(pair.a.length, pair.b.length);

			thrust::uniform_int_distribution<size_t> length_range(min_length, max_length);
			size_t child_length = length_range(rng_);

			return child_length;
		}

		}
	}

public:
	point_crossover(thrust::device_ptr<algo::Settings> settings,
					thrust::device_ptr<ProgramDescriptor> descriptors_old,
					thrust::device_ptr<uint8_t> pool_old,
					thrust::device_ptr<ProgramDescriptor> descriptors_new,
					thrust::device_ptr<uint8_t> pool_new,
					size_t pool_length)

		: settings_(settings.get())
		, descriptors_old_(descriptors_old.get())
		, descriptors_new_(descriptors_new.get())
		, pool_old_(pool_old.get())
		, pool_new_(pool_new.get())
		, pool_length_(pool_length)
	{
		rng_ = thrust::default_random_engine(util::get_entropy_with_ptr(this));
	}

	__host__ __device__
	void operator()(SelectionResult& result)
	{
		switch (result.mode)
		{
		case SelectionResult::Mode::Single: {
			size_t descriptor_idx = result.single_index;
			auto& descriptor = descriptors_old_[descriptor_idx];

			size_t pool_pos = settings_->block_size * result.destination_index;

			// Copy old program to new pool since there is nothing to cross over with

			// TODO: We are assuming the pool position will be the same
			size_t count = 0;
			size_t i = pool_pos;

			while (i < pool_length_
				   && count < descriptor.length) {

				pool_new_[i] = pool_old_[i];

				//

				i++;
				count++;
			}

			descriptors_new_[result.destination_index] = descriptors_old_[descriptor_idx];
			
			break;
		}

		case SelectionResult::Mode::Pair: {
			auto pair = get_descriptor_pair(result);
			size_t child_length = get_child_length(result);
			size_t child_pool_pos = settings_->block_size * result.destination_index;

			thrust::uniform_real_distribution<float> r01(0.0, 1.0);

			size_t count = 0;
			size_t a_idx = pair.a.pos;
			size_t b_idx = pair.b.pos;
			size_t x_idx = child_pool_pos;

			// Will be +1 from last element in array
			size_t end_idx = pool_length_;

			while (a_idx < end_idx
				   && b_idx < end_idx
				   && x_idx < end_idx
				   && count < child_length) {

				// Select parent randomly
				uint8_t value;
				if (r01(rng_) < 0.5) {
					value = pool_old_[a_idx];
				}
				else {
					value = pool_old_[b_idx];
				}

				pool_new_[x_idx] = value;

				//

				a_idx++;
				b_idx++;
				x_idx++;
				count++;
			}

			// Create the child program descriptor
			ProgramDescriptor child_descriptor;
			child_descriptor.pos = child_pool_pos;
			child_descriptor.length = child_length;
			child_descriptor.score = NAN;

			descriptors_new_[result.destination_index] = child_descriptor;
			break;
		}

		}

	}
};

} // namespace crossover_method

namespace program {

struct fill : public thrust::unary_function<ProgramDescriptor&, void>
{
private:
	thrust::device_ptr<uint8_t> pool_;

	thrust::default_random_engine rng_;

public:
	fill(thrust::device_ptr<uint8_t> pool)
		: pool_(pool)
	{
		rng_ = thrust::default_random_engine(util::get_entropy_with_ptr(this));
	}

	__host__ __device__
	void operator()(ProgramDescriptor& descriptor)
	{
		thrust::uniform_int_distribution<uint8_t> byte_range(0, UINT8_MAX);

		for (size_t i = descriptor.pos;
			 i < descriptor.pos + descriptor.length;
			 i++) {

			pool_[i] = byte_range(rng_);
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

} // namespace algo
} // namespace hitop



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
		<< "size: " << target_length
		<< std::endl
		<< std::endl;

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

	const std::streamsize program_pool_size = population_count * target_length;
	assert(program_pool_size > 0);

	size_t generation_num = 0;

	//

	static_assert(sizeof(std::streamsize) == sizeof(algo::buffer_size_t),
				  "Size types are not the same sizes");

	algo::Settings algo_settings_h;
	algo_settings_h.min_program_size = static_cast<algo::buffer_size_t>(target_length * (0.75));
	algo_settings_h.max_program_size = target_length;
	algo_settings_h.block_size = target_length;

	auto algo_settings_d = thrust::device_new<algo::Settings>(algo_settings_h);

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

	thrust::device_vector<program_descriptor::ProgramDescriptor> program_descriptors_temp(population_count);

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
	// Setup selection settings now that we have allocated everything
	//

	typedef typename algo::selection_method::tournament_selection::Context SelectionContext;

	SelectionContext selection_context_h;
	selection_context_h.program_descriptors = program_descriptors.data().get();
	selection_context_h.program_descriptors_count = program_descriptors.size();
	selection_context_h.rounds = 5;

	auto selection_context_d = thrust::device_new<SelectionContext>(selection_context_h);

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
					  program_descriptor::create_random(algo_settings_d));
	
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

	bool is_stop_requested = false;
	while (!is_stop_requested) {

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

			ProgramDescriptor best = *(program_descriptors.begin());
			ProgramDescriptor worst = *(program_descriptors.end() - 1);

			score_t sum = thrust::transform_reduce(program_descriptors.begin(),
												   program_descriptors.end(),
												   program_descriptor::score(),
												   0.0f,
												   thrust::plus<program_descriptor::score_t>());

			score_t avg = sum / program_descriptors.size();

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

		std::cout
			<< "debug: selecting elites..."
			<< std::endl;

		// Keep track of the position to start at in the case that we use elites to skip running the
		// selection method over some of the programs
		auto selection_results_start = selection_results.begin();
		auto program_descriptors_start = program_descriptors.begin();
		auto start_index = 0;

		if (selection_elites > 0) {
			thrust::transform(thrust::counting_iterator<size_t>(0),
							  thrust::counting_iterator<size_t>(selection_elites),
							  selection_results_start,
							  selection_result::from_index());

			start_index += selection_elites;
			thrust::advance(selection_results_start, selection_elites);
			thrust::advance(program_descriptors_start, selection_elites);
		}

		std::cout
			<< "debug: done"
			<< std::endl
			<< std::endl;

		//
		// Run the selection method on the rest
		//

		std::cout
			<< "debug: selecting the rest of population with selection method..."
			<< std::endl;

		thrust::transform(thrust::counting_iterator<size_t>(start_index),
						  thrust::counting_iterator<size_t>(program_descriptors.size()),
						  selection_results_start,
						  selection_method::tournament_selection(selection_context_d));

		std::cout
			<< "debug: done"
			<< std::endl
			<< std::endl;

		//
		// Crossover selection results
		//

		std::cout
			<< "debug: running crossover on selected population with crossover method..."
			<< std::endl;

		thrust::for_each(selection_results.begin(),
						 selection_results.end(),
						 crossover_method::point_crossover(
								 algo_settings_d,
								 program_descriptors.data(), program_pool.data(),
								 program_descriptors_temp.data(), program_pool_temp.data(),
								 program_pool.size()));

		std::cout
			<< "debug: done"
			<< std::endl
			<< std::endl;

		//
		// Mutate newly generated population
		//

		// TODO

		//
		// Swap newly created population with current population vector
		//

		program_descriptors = program_descriptors_temp;
		program_pool = program_pool_temp;

		//
		// Loop back with new generation
		//

		generation_num++;

		if (generation_num >= 100) {
			is_stop_requested = true;
		}
	}

	//
	// End algo
	//

	// Clear any `device_ptr`s
	
	// FIXME: Trying to delete the `thrust::device_ptr` is breaking the build
	//thrust::device_delete(program_settings_d);
	//thrust::device_delete(selection_context_d);
	//thrust::device_delete(random_context_d);

	std::cout
		<< "info: algorithm ran to completion!" << std::endl
		<< "press any key to exit..." << std::endl;

	std::cin.ignore();
	return 0;
}
