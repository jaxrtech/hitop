#include <thrust/device_vector.h>
#include <thrust/sort.h>
#include <thrust/transform.h>
#include "thrust_ext.cuh"

#include "hitop/util.h"
#include "hitop/algo.cuh"

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
