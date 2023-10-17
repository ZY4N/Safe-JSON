#include <iostream>
#include <chrono>
#include <fstream>
#include <sstream>
#include "../../../dynamic_json.hpp"

namespace js = ztu::json;
namespace jsd = js::dynamic;

std::string load_file(const char* filename) {
	auto is = std::ifstream(filename);
	if (not is.is_open()) {
		throw std::runtime_error("Could not open file");
	}
    std::ostringstream sstr;
    sstr << is.rdbuf();
    return sstr.str();
}

int main(int num_args, char* args[]) {

	using clock = std::chrono::high_resolution_clock;

	if (num_args < 2) {
		throw std::runtime_error("Filename argument missing");
	}

	auto text = load_file(args[1]);

	std::istringstream is(text);
	auto parser = jsd::parser(is);

	const auto begin_parse = clock::now();
	auto value = parser.parse();
	const auto end_parse = clock::now();


	std::ostringstream os;
	jsd::serializer serializer(os);

	const auto begin_serialize = clock::now();
	serializer.serialize(value);
	const auto end_serialize = clock::now();

	std::cout << ((end_parse - begin_parse).count() / 1000.0) << std::endl;
	std::cout << ((end_serialize - begin_serialize).count() / 1000.0) << std::endl;
}
