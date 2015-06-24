#pragma once

#include <thrust/functional.h>
#include <thrust/random.h>

#include "program_descriptor.cuh"
#include "util.cuh"

namespace hitop {
namespace algo {
namespace program {


struct fill : public thrust::unary_function<ProgramDescriptor&, void>
{
private:
	thrust::device_ptr<uint8_t> pool_;

	thrust::default_random_engine rng_;

public:
	fill(thrust::device_ptr<uint8_t> pool);

	__host__ __device__
	void operator()(ProgramDescriptor& descriptor);
};


struct score : public thrust::unary_function<program_descriptor::ProgramDescriptor&, void>
{
private:
	thrust::device_ptr<uint8_t> pool;

public:
	score(thrust::device_ptr<uint8_t> pool);

	__host__ __device__
	void operator()(program_descriptor::ProgramDescriptor& descriptor);
};


} // namespace program
} // namespace algo
} // namespace hitop