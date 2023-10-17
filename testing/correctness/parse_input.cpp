#include <iostream>
#include <fstream>
#include <filesystem>
#include "../../dynamic_json.hpp"

namespace js = ztu::json;
namespace jsd = js::dynamic;

enum class file_detail {
	VALID, INVALID, IMPLEMENTATION_DETAIL
};

int main() {
	const std::filesystem::path input_dir{ "./input" };

	int parse_errors = 0, missed_errors = 0, correct = 0;

	for (auto const& file : std::filesystem::directory_iterator{ input_dir }) {
		const auto path = file.path();
		const auto filename = path.filename();

		using enum file_detail;

		file_detail detail;
		switch (filename.c_str()[0]) {
			case 'y': detail = VALID; break;
			case 'n': detail = INVALID; break;
			case 'i': detail = IMPLEMENTATION_DETAIL; break;
		}

		char const* error = nullptr;
		try {

			auto is = std::ifstream(path);
			auto parser = jsd::parser(is);
			auto  value = parser.parse();
			is.close();

			auto out_path = path;
			out_path.remove_filename();
			out_path = out_path / "../output" / filename;

			auto os = std::ofstream(out_path);
			auto serializer = jsd::serializer(os);
			serializer.serialize(value);
			os << std::endl;
			os.close();

		} catch (const std::exception& e) {
			error = e.what();
		}

		if (detail == VALID and error) {
			std::cout << "\u001b[31;1mfailed parsing of\u001b[0m: " << filename << ": " << error << std::endl;
			parse_errors++;
		} else if (detail == INVALID and not error) {
			std::cout << "\u001b[31;1mmissed error in\u001b[0m: " << filename << std::endl;
			missed_errors++;
		} else {
			std::cout << "\u001b[32;1mcorrect\u001b[0m: " << filename << std::endl;
			correct++;
		}
	}

	const auto to_percent = [total = correct + parse_errors + missed_errors](int count) {
		return 100.0 * double(count) / double(total);
	};

	std::cout << std::fixed << std::setprecision(2) << std::endl;
	std::cout << "correct:       " << std::setw(8) << to_percent(correct) << std::endl;
	std::cout << "parse_errors:  " << std::setw(8) << to_percent(parse_errors) << std::endl;
	std::cout << "missed_errors: " << std::setw(8) << to_percent(missed_errors) << std::endl;
}
