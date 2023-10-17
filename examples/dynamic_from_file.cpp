#include <fstream>
#include <iostream>
#include "../dynamic_json.hpp"

namespace jsd = ztu::json::dynamic;

int main(int num_args, char* args[]) {

	if (num_args < 2) {
		std::cout << "Filename argument missing" << std::endl;
		return -1;
	}

	auto is = std::ifstream(args[1]);
	if (not is.is_open()) {
		std::cout << "failed to open " << args[1] << std::endl;
		return -1;
	}

	auto parser = jsd::parser(is);
	auto value = parser.parse();
	is.close();

	std::cout << value << std::endl;
}
