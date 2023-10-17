#include <vector>
#include <cinttypes>
#include <iostream>
#include "../safe_json.hpp"

namespace jss = ztu::json::safe;

struct color {
	uint8_t r, g, b;
	constexpr bool operator==(const color&) const = default;
};

namespace color_array_detail {

	struct string_color_array_converter {
		using x_t = jss::data_types::string;
		using y_t = std::vector<color>;

		static std::optional<y_t> convert(const x_t &x) {

			std::vector<color> colors;
			colors.reserve(x.length() / 4);

			uint8_t numHexChars = 0;
			std::array<uint8_t, 6> buffer{};

			const auto pushColor = [&]() {
				switch (numHexChars) {
				case 6:
					colors.emplace_back(
						static_cast<uint8_t>(16 * buffer[0] + buffer[1]),
						static_cast<uint8_t>(16 * buffer[2] + buffer[3]),
						static_cast<uint8_t>(16 * buffer[4] + buffer[5])
					);
					break;
				case 3:
					colors.emplace_back(
						static_cast<uint8_t>(17 * buffer[0]),
						static_cast<uint8_t>(17 * buffer[1]),
						static_cast<uint8_t>(17 * buffer[2])
					);
					break;
				default:
					return false;
				}

				std::fill(buffer.begin(), buffer.end(), 0);
				numHexChars = 0;

				return true;
			};

			for (const char c : x) {
				uint8_t byte = 0x80;
				if ('0' <= c && c <= '9') {
					byte = c - '0';
				} else if ('a' <= c && c <= 'f') {
					byte = 10 + c - 'a';
				} else if ('A' <= c && c <= 'F') {
					byte = 10 + c - 'A';
				}

				if (byte & 0x80) {
					if (not (c == ' ' and pushColor())) {
						return std::nullopt;
					}
				} else if (numHexChars == 6) {
					return std::nullopt;
				} else {
					buffer[numHexChars++] = byte;
				}
			}

			std::optional<y_t> y;

			if (pushColor()) {
				y.emplace(std::move(colors));
			}

			return y;
		};

		static std::optional<x_t> revert(const y_t &y) {

			x_t x{};
			x.reserve(y.size() * 4);

			for (const auto &[r, g, b] : y) {
				std::array<std::pair<uint8_t,uint8_t>, 3> digits;

				bool shortNotation = true;
				ztu::json_internal::for_each::indexed_argument(
					[&]<size_t Index>(const auto &channel) {
						auto &digit = digits[Index];
						digit.first = channel / 16;
						digit.second = channel - 16 * digit.first;
						shortNotation &= digit.second == 0;
						return false;
					},
					r, g, b
				);

				const auto hexChar = [](const uint8_t num) {
					return static_cast<char>(num < 10 ? '0' + num : 'a' + num - 10);
				};

				if (shortNotation) {
					ztu::json_internal::for_each::index<3>([&]<size_t Index>() {
						x += hexChar(digits[Index].first);
						return false;
					});
				} else {
					ztu::json_internal::for_each::index<3>([&]<size_t Index>() {
						x += hexChar(digits[Index].first);
						x += hexChar(digits[Index].second);
						return false;
					});
				}

				x += ' '; // additional space is removed later
			}

			if (not x.empty()) {
				x.resize(x.size() - 1);
			}

			return x;
		};
	};

	template<std::size_t StrLen>
	using string_color_array_adapter = jss::default_types::adapter<jss::default_types::string<StrLen>, std::vector<color>, string_color_array_converter>;


	template<jss::default_types::string DefaultValue>
	struct color_array : string_color_array_adapter<DefaultValue.size() + 1> {
		constexpr color_array() : string_color_array_adapter<DefaultValue.size() + 1>{ DefaultValue }{}
	};
}

namespace ztu::json::safe::default_types_internal {
	template<string DefaultValue>
	struct get_default_value_type<color_array_detail::color_array<DefaultValue>> : public type_container<ADAPTER> {};
}

template<jss::default_types::string DefaultValue>
using color_array = color_array_detail::color_array<DefaultValue>;


namespace my_object_builder {
	using namespace jss::literals;

	using my_default_object_t = object<
		set<"colors", color_array<
			"f00 ff0 0f0 0ff 00f f0f f00 ff0 0f0 0ff 00f f0f f00 ff0 0f0 0ff "
			"f00 ff0 0f0 0ff 00f f0f f00 ff0 0f0 0ff f00 ff0 0f0 0ff 00f f0f"_S
		>{}>{}
	>;
}

constexpr my_object_builder::my_default_object_t my_default_object{};


int main() {

	// missing colors wil be replaced with default values
	auto is = std::stringstream("{}");

	auto parser = jss::parser(is);
	auto value = parser.parse<my_default_object>();

	auto serializer = jss::serializer(std::cout);
	serializer.serialize<my_default_object>(value);
	std::cout << std::endl;

	auto &colors = value.get<"colors">();

	// The vector can be modified
	colors.push_back({ 0, 0, 0 });

	for (const auto& c : colors) {
		std::cout
			<< "rgb("
			<< int(c.r) << ", " << int(c.g) << ", " << int(c.b)
			<< ")\n";
	}
}
