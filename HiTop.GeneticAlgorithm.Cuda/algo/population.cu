#include "population.cuh"

#include <thrust/random.h>
#include "organism.cuh"
#include "util.cuh"

namespace hitop {
namespace algo {
namespace population {

thrust::device_vector<hitop::algo::organism::device_program> create(population::Settings& settings)
{
	thrust::device_vector<organism::device_program> programs(settings.population_count);

	thrust::transform(thrust::counting_iterator<uint32_t>(0),
					  thrust::counting_iterator<uint32_t>(settings.population_count),
					  programs,
					  organism::create_random(settings.organism_settings));

	return programs;
}

}
}
}