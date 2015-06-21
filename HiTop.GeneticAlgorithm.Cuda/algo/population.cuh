#include <cinttypes>
#include <thrust/device_vector.h>

#include "organism.cuh"

namespace hitop {
namespace algo {
namespace population {

struct Settings
{
	uint32_t population_count;

	hitop::algo::organism::Settings organism_settings;

	hitop::algo::organism::device_program target;
};

thrust::device_vector<hitop::algo::organism::device_program> create(Settings& settings);

}
}
}