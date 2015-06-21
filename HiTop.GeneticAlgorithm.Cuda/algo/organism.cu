#include "organism.cuh"

#include <cstdlib>
#include <ctime>
#include <thrust/random.h>

#include "util.cuh"

namespace hitop {
namespace algo {
namespace organism {


create_random::create_random(organism::Settings& settings) : settings(settings) {}

__host__ __device__
organism::device_program create_random::operator()(uint32_t thread_id)
{
	uint32_t seed = hitop::algo::util::hash(thread_id);

	thrust::default_random_engine rng(seed);
	thrust::uniform_int_distribution<uint32_t> program_length(settings.min_program_size, settings.max_program_size);
	thrust::uniform_int_distribution<uint8_t> byte_range(0, UINT8_MAX);

	organism::device_program program(program_length(rng));
	thrust::generate(program.begin(), program.end(), byte_range(rng));

	return program;
}

}
}
}