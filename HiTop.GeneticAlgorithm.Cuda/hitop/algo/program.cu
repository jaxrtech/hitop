#include "program.cuh"

namespace hitop {
namespace algo {
namespace program {

fill::fill(thrust::device_ptr<uint8_t> pool)
	: pool_(pool)
{
	rng_ = thrust::default_random_engine(util::get_entropy_with_ptr(this));
}


__host__ __device__
void fill::operator()(ProgramDescriptor& descriptor)
{
	thrust::uniform_int_distribution<uint8_t> byte_range(0, UINT8_MAX);

	for (size_t i = descriptor.pos;
			i < descriptor.pos + descriptor.length;
			i++) {

		pool_[i] = byte_range(rng_);
	}
}

//

score::score(thrust::device_ptr<uint8_t> pool)
	: pool(pool)
{ }

__host__ __device__
void score::operator()(ProgramDescriptor& descriptor)
{
	// TODO: Dummy scoring function

	uint32_t seed = util::hash64_32(descriptor.pos);

	thrust::default_random_engine rng(seed);
	thrust::uniform_real_distribution<float> score_range(0, descriptor.length);

	descriptor.score = score_range(rng);
}


} // namespace program
} // namespace algo
} // namespace hitop