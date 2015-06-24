#pragma once

#include <thrust/functional.h>
#include <thrust/random.h>

#include "selection_result.cuh"
#include "program_descriptor.cuh"

namespace hitop {
namespace algo {
namespace selection_method {

struct tournament_selection : public thrust::unary_function<size_t, SelectionResult>
{
	struct Context
	{
		ProgramDescriptor* program_descriptors;
		size_t program_descriptors_count;

		uint32_t rounds;
	};


private:
	Context *context_;

	thrust::default_random_engine rng_;

	typedef thrust::uniform_int_distribution<size_t> indices_range_distribution;
	indices_range_distribution indices_range_;

	__host__ __device__
	size_t get_random_index();

	__host__ __device__
	size_t get_next_winner_index();

public:
	tournament_selection(thrust::device_ptr<Context> context_ptr);

	__host__ __device__
	SelectionResult operator()(size_t index);
};


} // namespace selection_method
} // namespace algo
} // namespace hitop