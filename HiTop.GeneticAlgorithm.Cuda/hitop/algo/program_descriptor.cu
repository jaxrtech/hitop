#include "program_descriptor.cuh"

#include <thrust/random.h>

#include "util.cuh"

namespace hitop {
namespace algo {
namespace program_descriptor {

create_random::create_random(thrust::device_ptr<algo::Settings> settings_ptr)
	: settings_(settings_ptr.get())
{ }

__host__ __device__
ProgramDescriptor create_random::operator()(size_t index)
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

//

__host__ __device__
bool greater_score::operator()(const ProgramDescriptor& a, const ProgramDescriptor& b)
{
	return a.score > b.score;
}

//

__host__ __device__
score_t score::operator()(const ProgramDescriptor& x)
{
	return x.score;
}

} // namespace program_descriptor
} // namespace algo
} // namespace hitop