#pragma once

#include <thrust/functional.h>
#include <thrust/random.h>

#include "selection_result.cuh"
#include "program_descriptor.cuh"

namespace hitop {
namespace algo {
namespace crossover_method {

namespace detail {

struct ProgramDesciptorPair
{
	const ProgramDescriptor& a;
	const ProgramDescriptor& b;

	__host__ __device__
	ProgramDesciptorPair(const ProgramDescriptor& a_, const ProgramDescriptor& b_);
};

} // namespace detail


struct point_crossover : public thrust::unary_function<SelectionResult&, void>
{
private:
	algo::Settings* settings_;

	ProgramDescriptor* descriptors_old_;
	ProgramDescriptor* descriptors_new_;

	uint8_t* pool_old_;
	uint8_t* pool_new_;

	size_t pool_length_;

	thrust::default_random_engine rng_;

	__host__ __device__
	detail::ProgramDesciptorPair get_descriptor_pair(const SelectionResult& result);

	__host__ __device__
	size_t get_child_length(const SelectionResult& result);

public:
	point_crossover(thrust::device_ptr<algo::Settings> settings,
					thrust::device_ptr<ProgramDescriptor> descriptors_old,
					thrust::device_ptr<uint8_t> pool_old,
					thrust::device_ptr<ProgramDescriptor> descriptors_new,
					thrust::device_ptr<uint8_t> pool_new,
					size_t pool_length);

	__host__ __device__
	void operator()(SelectionResult& result);
};


} // namespace crossover_method
} // namespace algo
} // namespace hitop