#pragma once

#include <string>
#include <cinttypes>
#include <thrust/host_vector.h>

namespace hitop {
	
namespace util {

struct AppSettings
{
	std::string input_path;
};

void output_header(std::ostream& stream);

void output_usage(std::ostream& stream);

std::string get_timestamp_readable();

bool try_parse_settings(int argc, char* argv[], AppSettings& settings);

bool try_read_file(std::string input_path, thrust::host_vector<uint8_t>* data, std::streamsize* size);

}
}