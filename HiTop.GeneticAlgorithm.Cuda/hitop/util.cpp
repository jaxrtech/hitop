#include "util.h"

#include <string>
#include <sstream>
#include <fstream>
#include <iomanip>
#include <cinttypes>

#include <thrust/host_vector.h>

namespace hitop {
namespace util {

void output_header(std::ostream& stream)
{
	stream <<
R"(
HiTop Compressor (Cuda) indev
Copyright (c) 2015 Josh Bowden & Patrick Ryan
CONFIDENTIAL MATERIAL. DO NOT REDISTRIBUTE.
)" << std::endl;
}

void output_usage(std::ostream& stream)
{
	stream << "usage: hitop FILE" << std::endl;
}

std::string get_timestamp_readable()
{
	// get current time
	time_t t = time(nullptr);
	tm *now = localtime(&t);

	std::ostringstream result;

	result
		<< now->tm_year + 1900 << '/'
		<< now->tm_mon + 1 << '/'
		<< now->tm_mday
		<< " "
		<< std::setfill('0')
		<< std::setw(2)
		<< now->tm_hour << ':'
		<< now->tm_min << ':'
		<< now->tm_sec;

	return result.str();
}

bool try_parse_settings(int argc, char* argv[], AppSettings& settings)
{
	// advance past executable name
	argc -= (argc > 0); argv += (argc > 0);

	const int arg_count = 1;
	if (argc != arg_count) {
		return false;
	}

	settings.input_path = argv[0];
	return true;
}

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

}
}