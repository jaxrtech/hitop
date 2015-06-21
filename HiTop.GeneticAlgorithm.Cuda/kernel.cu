#include <iostream>
#include <iomanip>
#include <fstream>
#include <sstream>
#include <ctime>

#include <thrust/host_vector.h>
#include <thrust/device_vector.h>

#include "util.h"
#include "algo.cuh"


bool try_read_file(std::string input_path, thrust::host_vector<uint8_t>* data, std::streamsize* size)
{
	// open the file
	std::ifstream file(input_path, std::ios::binary);
	if (!file.is_open()) {
		std::cerr << "error: cannot open input file" << std::endl;
		return false;
	}

	// prevent eating new lines in binary mode
	file.unsetf(std::ios::skipws);

	// get its size:
	file.seekg(0, std::ios::end);
	*size = file.tellg();
	file.seekg(0, std::ios::beg);

	// reserve capacity
	data->clear();
	data->reserve(*size);

	// read the data:
	data->insert(data->begin(),
			     std::istream_iterator<uint8_t>(file),
			     std::istream_iterator<uint8_t>());

	return true;
}

int main(int argc, char* argv[])
{
	hitop::util::AppSettings settings;
	if (!hitop::util::try_parse_settings(argc, argv, settings)) {
		hitop::util::output_usage(std::cerr);
		return -1;
	}

	thrust::host_vector<uint8_t> target_h;
	std::streamsize target_length;
	bool result = try_read_file(settings.input_path, &target_h, &target_length);
	if (!result) {
		return -1;
	}

	// Actually do stuff
	hitop::util::output_header(std::cout);

	std::cout << "info: started at " << hitop::util::get_timestamp_readable() << std::endl;

	std::cout
		<< "input file: '" << settings.input_path << std::endl
		<< "size: " << target_length << std::endl;

	// Copy to device
	thrust::device_vector<char> target_d = target_h;

	// Algo settings
	hitop::algo::population::Settings algo_settings = {};
	algo_settings.population_count = 100;
	algo_settings.target = target_d;

	algo_settings.organism_settings = {};
	algo_settings.organism_settings.min_program_size = target_length * (0.80);
	algo_settings.organism_settings.max_program_size = target_length;

	// Generate first gen
	auto gen = hitop::algo::population::create(algo_settings);

	std::cout << std::endl << "done" << std::endl;
	std::cin.ignore();
	return 0;
}

