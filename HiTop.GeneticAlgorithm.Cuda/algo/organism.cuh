#include <cinttypes>
#include <thrust/device_vector.h>

namespace hitop {
namespace algo {
namespace organism {

typedef thrust::device_vector<uint8_t> device_program;

struct Settings
{
	uint32_t min_program_size;
	uint32_t max_program_size;
};

struct create_random : public thrust::unary_function<uint32_t, organism::device_program>
{
	organism::Settings& settings;

	create_random(organism::Settings& settings);

	__host__ __device__
	organism::device_program operator()(uint32_t seed);
};

}
}
}