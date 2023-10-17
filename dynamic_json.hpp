#pragma once

#include <utility>
#include <cstdint>
#include <unordered_map>
#include <variant>
#include <vector>
#include <string>
#include <array>
#include <tuple>
#include <span>
#include <string_view>
#include <istream>
#include <ostream>
#include <iomanip>
#include <format>
#include <algorithm>
#include <stdexcept>


#ifndef ZTU_JSON_DETAIL
#define ZTU_JSON_DETAIL

namespace ztu::json_internal {

namespace literal_config {

	constexpr char left_brace[] = "{";
	constexpr char right_brace[] = "}";
	constexpr char left_bracket[] = "[";
	constexpr char right_bracket[] = "]";
	constexpr char colon[] = ":";
	constexpr char comma[] = ",";
	constexpr char null[] = "null";
	constexpr char boolean_false[] = "false";
	constexpr char boolean_true[] = "true";
	constexpr char string_begin = '"';
	constexpr char string_end = '"';
	constexpr char variant_type[] = "_type";

} // namespace literal_config

#define ztu_ic inline constexpr
#define ztu_nic [[nodiscard]] inline constexpr

namespace for_each {

template<auto Size>
ztu_ic bool index(auto &&f) {
	return [&]<auto... Indices>(std::index_sequence<Indices...>) {
		return (f.template operator()<Indices>() || ...);
	}(std::make_index_sequence<Size>());
}

template<typename... Types>
ztu_ic bool type(auto &&f) {
	return (f.template operator()<Types>() || ...);
}

template<auto... Values>
ztu_ic bool value(auto &&f) {
	return (f.template operator()<Values>() || ...);
}

template<auto... Values>
ztu_ic bool indexed_value(auto &&f) {
	return [&]<auto... Indices>(std::index_sequence<Indices...>) {
		return (f.template operator()<Indices, Values>() || ...);
	}(std::make_index_sequence<sizeof...(Values)>());
}

template<typename... Args>
ztu_ic bool indexed_argument(auto &&f, Args&&... args) {
	return [&]<auto... Indices>(std::index_sequence<Indices...>) {
		return (f.template operator()<Indices>(std::forward<Args>(args)) || ...);
	}(std::make_index_sequence<sizeof...(Args)>());
}

} // namespace for_each


using std::size_t;
using ssize_t = std::make_signed_t<size_t>;

template<ssize_t N>
	requires (N > 0)
struct string_literal {
	static constexpr auto max_length = N - 1;

	ztu_ic string_literal() = default;

	ztu_ic string_literal(const char (&str)[N]);

	template<ssize_t M> requires (M > 0 and M <= N)
	ztu_ic string_literal(const char (&str)[M]);

	template<typename... Chars> requires (sizeof...(Chars) == max_length)
	ztu_ic string_literal(Chars... chars) : value{chars..., '\0' } {}

	template<ssize_t M>
	ztu_nic bool operator==(const string_literal<M> &other) const;

	ztu_nic const char *c_str() const;

	ztu_nic ssize_t size() const;

	ztu_nic auto begin();
	ztu_nic auto begin() const;

	ztu_nic auto end();
	ztu_nic auto end() const;

	ztu_nic char& operator[](size_t index);
	ztu_nic const char& operator[](size_t index) const;

	template<ssize_t M>
	inline friend std::ostream& operator<<(std::ostream &out, const string_literal<M>& str);

public:
	std::array<char,N> value{};
};

template<ssize_t N>
	requires (N > 0)
ztu_ic string_literal<N>::string_literal(const char (&str)[N]) {
	std::copy_n(std::begin(str), N, value.begin());
}

template<ssize_t N> requires (N > 0)
template<ssize_t M> requires (M > 0 and M <= N)
ztu_ic string_literal<N>::string_literal(const char (&str)[M]) {
	std::copy_n(std::begin(str), M, value.begin());
	value[M-1] = '\0';
}

template<ssize_t N>
	requires (N > 0)
template<ssize_t M>
ztu_ic bool string_literal<N>::operator==(const string_literal<M> &other) const {
	const auto length = std::min(N,M);
	for (ssize_t i = 0; i < length; i++) {
		if (value[i] != other.value[i]) {
			return false;
		}
	}
	return true;
}

template<ssize_t N> requires (N > 0)
ztu_nic const char *string_literal<N>::c_str() const { return value.data(); }

template<ssize_t N> requires (N > 0)
ztu_nic ssize_t string_literal<N>::size() const {
	ssize_t length = 0;
	while (length < N && value[length] != '\0')
		length++;
	return length;
}

template<ssize_t N> requires (N > 0)
ztu_nic auto string_literal<N>::begin() { return value.begin(); }

template<ssize_t N> requires (N > 0)
ztu_nic auto string_literal<N>::begin() const { return value.begin(); }

template<ssize_t N> requires (N > 0)
ztu_nic auto string_literal<N>::end() { return value.begin() + size(); }

template<ssize_t N> requires (N > 0)
ztu_nic auto string_literal<N>::end() const { return value.begin() + size(); }

template<ssize_t M>
inline std::ostream& operator<<(std::ostream &out, const string_literal<M>& str) {
	return out << str.c_str();
}

template<ssize_t N> requires (N > 0)
ztu_nic char& string_literal<N>::operator[](size_t index) {
	return value[index];
}

template<ssize_t N> requires (N > 0)
ztu_nic const char& string_literal<N>::operator[](size_t index) const {
	return value[index];
}

namespace string_literals {
	template<string_literal Str>
	constexpr auto operator"" _sl() {
		return Str;
	}
}


namespace transcoding_error {

	enum class codes {
		OK = 0,
		WRONG_SPELLING,
		UNKNOWN_SPELLING_INDEX,
		NONE_HEX_CHAR_IN_UNICODE_ESCAPE,
		UNKNOWN_ESCAPE_SEQUENCE,
		UNESCAPED_CHARACTER,
		CUT_OF_UTF8_CHARACTER,
		EOF_IN_TOKEN
	};

	struct category : std::error_category {
		const char* name() const noexcept override {
			return "transcoding";
		}
		std::string message(int ev) const override {
			switch (static_cast<codes>(ev)) {
			case codes::WRONG_SPELLING:
				return "Character sequence does not match any of the valid spellings for this token.";
			case codes::UNKNOWN_SPELLING_INDEX:
				return "The specified index does not correspond to a spelling.";
			case codes::NONE_HEX_CHAR_IN_UNICODE_ESCAPE:
				return "None hex character in unicode escape sequence.";
			case codes::UNKNOWN_ESCAPE_SEQUENCE:
				return "Unknown escape sequence.";
			case codes::UNESCAPED_CHARACTER:
				return "Unescaped character string literal.";
			case codes::EOF_IN_TOKEN:
				return "Reached end of file while parsing token.";
			case codes::CUT_OF_UTF8_CHARACTER:
				return "utf8 character begin without second byte.";
			default:
				using namespace std::string_literals;
				return "unrecognized error ("s + std::to_string(ev) + ")";
			}
		}
	};
} // namespace transcoding_error


inline std::error_category& transcoding_error_category() {
	static transcoding_error::category category;
	return category;
}

namespace transcoding_error {
	inline std::error_code make_error_code(codes e) {
		return { static_cast<int>(e), transcoding_error_category() };
	}
} // namespace transcoding_error

} // ztu::json_internal

template <>
struct std::is_error_code_enum<ztu::json_internal::transcoding_error::codes> : public std::true_type {};

namespace ztu::json_internal::transcoding {

template<class T, typename Enum>
concept token_concept = (
	std::same_as<Enum, typename T::enum_t> &&
	requires(const T::value_t& value, std::vector<char>& buffer, std::istream& is, std::ostream& os, std::error_code& e) {
		{ T::begins_with(is)		} -> std::same_as<bool>;
		{ T::read(is, buffer, e)	} -> std::same_as<typename T::value_t>;
		{ T::write(os, value, e)	} -> std::same_as<void>;
		{ T::name()				} -> std::same_as<std::string_view>;
		{ T::type()				} -> std::same_as<typename T::enum_t>;
	}
);

namespace token_internal {

template<typename Enum, Enum Type, string_literal Name, typename ValueType = std::monostate>
struct token_base {
	using enum_t = Enum;
	using value_t = ValueType;

	constexpr static inline Enum type() {
		return Type;
	}

	constexpr static inline std::string_view name() {
		return { Name.c_str() };
	}
};


template<auto Type, string_literal Name, string_literal Spelling>
struct literal_token : public token_base<decltype(Type), Type, Name> {
	using base_t = token_base<decltype(Type), Type, Name> ;
	using typename base_t::value_t;

	inline static bool begins_with(std::istream &is) {
		return is.peek() == Spelling[0U];
	}

	inline static value_t read(std::istream &is, std::vector<char> &, std::error_code& error) {
		for (const auto& c : Spelling) {
			if (is.peek() != c) {
				using transcoding_error::make_error_code;
				using enum transcoding_error::codes;
				error = make_error_code(WRONG_SPELLING);
				break;
			}
			is.get();
		}
		return {};
	}

	inline static void write(std::ostream &os, const value_t&, std::error_code& error) {
		os.write(Spelling.c_str(), Spelling.size());
	}
};

template<auto Type, string_literal Name, std::integral IndexType, string_literal... Spellings>
struct multi_literal_token : token_base<decltype(Type), Type, Name, IndexType> {
	using base_t = token_base<decltype(Type), Type, Name, IndexType>;
	using typename base_t::value_t;

	inline static bool begins_with(std::istream &is) {
		const auto c = is.peek();
		return for_each::value<Spellings...>(
			[&]<auto Spelling>() {
				return c == Spelling[0U];
			}
		);
	}

	inline static value_t read(std::istream &is, std::vector<char> &, std::error_code& error) {
		value_t index = -1;
		const auto found_spelling = for_each::indexed_value<Spellings...>(
			[&]<auto Index, auto Spelling>() {
				for (const auto& c : Spelling) {
					if (is.peek() != c) {
						return false;
					}
					is.get();
				}
				index = Index;
				return true;
			}
		);
		if (not found_spelling) {
			using transcoding_error::make_error_code;
			using enum transcoding_error::codes;
			error = make_error_code(WRONG_SPELLING);
		}
		return index;
	}

	inline static void write(std::ostream &os, const value_t &index, std::error_code& error) {
		const auto found_spelling = for_each::indexed_value<Spellings...>(
			[&]<auto Index, auto Spelling>() {
				if (index == Index) {
					os.write(Spelling.c_str(), Spelling.size());
					return true;
				}
				return false;
			}
		);
		if (not found_spelling) [[unlikely]] {
			using transcoding_error::make_error_code;
			using enum transcoding_error::codes;
			error = make_error_code(UNKNOWN_SPELLING_INDEX);
		}
	}
};

template<auto Type, string_literal Name, char BeginChar, char EndChar>
struct string_literal_token : public token_base<decltype(Type), Type, Name, std::string_view> {

	using base_t = token_base<decltype(Type), Type, Name, std::string_view>;
	using typename base_t::value_t;

	inline static bool begins_with(std::istream &is) {
		return is.peek() == BeginChar;
	}

	inline static auto backslash_escape_chars = {
		std::pair{ '\\', '\\' },
		std::pair{ 'b', '\b' },
		std::pair{ 'f', '\f' },
		std::pair{ 'n', '\n' },
		std::pair{ 'r', '\r' },
		std::pair{ 't', '\t' },
		std::pair{ 'v', '\v' },
		std::pair{ '"', '"' },
		std::pair{ '/', '/' }
	};

	inline static uint16_t parse_hex_short(std::istream& is, std::error_code& error) {
		using transcoding_error::make_error_code;
		using enum transcoding_error::codes;

		uint16_t value = 0;
		for (int i = 0; i < 4; i++) {
			unsigned char c = is.get();
			if ('0' <= c and c <= '9') {
				c -= '0';
			} else if ('a' <= c and c <= 'f') {
				c -= ('a' - 10);
			} else if ('A' <= c and c <= 'F') {
				c -= ('A' - 10);
			} else [[unlikely]] {
				error = make_error_code(NONE_HEX_CHAR_IN_UNICODE_ESCAPE);
				break;
			}
			value = (value << 4) | c;
		}
		return value;
	}

	inline static value_t read(std::istream &is, std::vector<char> &buffer, std::error_code& error) {
		using transcoding_error::make_error_code;
		using enum transcoding_error::codes;

		char c;

		const auto next = [&]() { c = is.get(); };

		next(); // ignore BeginChar

		while (true) {

			next();

			if (is.eof()) {
				error = make_error_code(EOF_IN_TOKEN);
				goto on_error;
			}

			if (c == '\\') [[unlikely]] {
				next();
				auto backslash_escape = false;
				for (const auto& [ letter, replace ] : backslash_escape_chars) {
					if (c == letter) {
						c = replace;
						backslash_escape = true;
						break;
					}
				}

				if (not backslash_escape) [[unlikely]] {
					if (c == 'u') {
						const auto raw_value = parse_hex_short(is, error);
						if (error) goto on_error;

						auto lsb = uint8_t(raw_value & 0xff);
						auto msb = uint8_t(raw_value >> 8);

						// encode to utf-8 multichar
						if (raw_value < 0x80) {
							buffer.push_back(char(lsb));
						} else if (raw_value < 0x800) {
							buffer.push_back(char(0xc0 | ((msb & 0x7) << 2) | (lsb >> 6)));
							buffer.push_back(char(0x80 | (lsb & 0x3f)));
						} else {
							buffer.push_back(char(0xe0 | (msb >> 4)));
							buffer.push_back(char(0x80 | ((msb & 0xf) << 2) | (lsb >> 6)));
							buffer.push_back(char(0x80 | (lsb & 0x3f)));
						}
						continue;
					} else {
						error = make_error_code(UNKNOWN_ESCAPE_SEQUENCE);
						goto on_error;
					}
				}
			} else [[likely]] {
				auto unescaped = false;
				for (const auto& escape : {
					'\b', '\f', '\n', '\r', '\t', '\v'
				}) {
					if (c == escape) {
						unescaped = true;
						break;
					}
				}
				if (unescaped) [[unlikely]] {
					error = make_error_code(UNESCAPED_CHARACTER);
					goto on_error;
				}
				if (c == EndChar) [[unlikely]] {
					break;
				}
			}
			buffer.push_back(c);
		}

		return { buffer.data(), buffer.data() + buffer.size() };

	on_error:
		is.setstate(std::ios_base::failbit);

		return { buffer.data(), buffer.data() };
	}

	inline static void write(std::ostream &os, const value_t &value, std::error_code& error) {
		using transcoding_error::make_error_code;
		using enum transcoding_error::codes;

		os.put(BeginChar);

		const auto length = static_cast<ssize_t>(value.length());

		for (ssize_t i = 0; i < length; i++) {
			auto c = value[i];
			auto backslash_escape = false;

			for (const auto& [ replace, letter ] : backslash_escape_chars) {
				if (c == letter) {
					c = replace;
					backslash_escape = true;
					break;
				}
			}

			if (backslash_escape) {
				os.put('\\');
				os.put(c);
				continue;
			}

			const auto first_bit = c & 0x80;
			const auto third_bit = c & 0x20;
			const auto fourth_bit = c & 0x10;

			// check third and fourth prefix bit to distinguish encoding
			const auto num_utf8_bytes = first_bit ? (third_bit ? (fourth_bit ? 4 : 3) : 2) : 1;

			if ((i + num_utf8_bytes) > length) [[unlikely]] {
				error = make_error_code(CUT_OF_UTF8_CHARACTER);
				return;
			}

			uint32_t raw_value;
			if (num_utf8_bytes == 1) {
				raw_value = uint8_t(
					value[i]
				);
			} else if (num_utf8_bytes == 2) {
				raw_value = uint32_t(
					((value[i] & 0x1f) << 6) | (value[i+1] & 0x3f)
				);
			} else if (num_utf8_bytes == 3) {
				raw_value = uint32_t(
					((value[i] & 0xf) << 12) | ((value[i+1] & 0x3f) << 6) | (value[i+2] & 0x3f)
				);
			} else if (num_utf8_bytes == 4) {
				raw_value = uint32_t(
					((value[i] & 0x7) << 18) | ((value[i+1] & 0x3f) << 12) | ((value[i+2] & 0x3f) << 6) | (value[i+3] & 0x3f)
				);
			}

			if (
				(0x0    <= raw_value and raw_value < 0x20  ) or
				(0xd800 <= raw_value and raw_value < 0xe000)
			) {
				os << "\\u" << std::setw(4) << std::setfill('0') << std::hex << raw_value << std::dec;
			} else {
				os.write(&value[i], num_utf8_bytes);
			}

			// i is also incremented in loop expression
			i += num_utf8_bytes - 1;
		}

		os.put(EndChar);
	}
};

template<auto Type, string_literal Name>
struct number_literal_token : public token_base<decltype(Type), Type, Name, double> {
	using base_t = token_base<decltype(Type), Type, Name, double>;
	using typename base_t::value_t;

	inline static bool begins_with(std::istream &is) {
		const auto c = is.peek();
		return std::isdigit(c) or c == '-' or c == '+';
	}

	inline static value_t read(std::istream &is, std::vector<char> &, std::error_code& error) {
		value_t d;
		is >> d;
		return d;
	}

	inline static void write(std::ostream &os, const value_t &value, std::error_code& error) {
		os << std::setprecision(std::numeric_limits<double>::max_digits10 - 1) << std::defaultfloat << value;
	}
};

template<auto Type, string_literal Name>
struct end_token : public token_base<decltype(Type), Type, Name> {
	using base_t = token_base<decltype(Type), Type, Name>;
	using typename base_t::value_t;

	inline static bool begins_with(std::istream &is) {
		return is.eof();
	}

	value_t static read(std::istream &, std::vector<char> &, std::error_code& error) {
		return {};
	}

	inline static void write(std::ostream &os, const value_t &, std::error_code& error) {
		os.flush();
	}
};

} // namespace token_internal

namespace tokens {

enum class type : unsigned char {
	END,
	LBRACE, RBRACE,
	COLON, COMMA,
	STRING, NUMBER, BOOLEAN,
	LBRACKET, RBRACKET,
	UNDEFINED,
};

using left_brace = token_internal::literal_token<type::LBRACE, "left-brace", literal_config::left_brace>;
using right_brace = token_internal::literal_token<type::RBRACE, "right-brace", literal_config::right_brace>;
using left_bracket = token_internal::literal_token<type::LBRACKET, "left-bracket", literal_config::left_bracket>;
using right_bracket = token_internal::literal_token<type::RBRACKET, "right-bracket", literal_config::right_bracket>;
using colon = token_internal::literal_token<type::COLON, "colon", literal_config::colon>;
using comma = token_internal::literal_token<type::COMMA, "comma", literal_config::comma>;
using null = token_internal::literal_token<type::UNDEFINED, "null", literal_config::null>;
using boolean = token_internal::multi_literal_token<type::BOOLEAN, "boolean", bool, literal_config::boolean_false, literal_config::boolean_true>;
using string = token_internal::string_literal_token<type::STRING, "string", literal_config::string_begin, literal_config::string_end>;
using number = token_internal::number_literal_token<type::NUMBER, "number">;
using end = token_internal::end_token<type::END, "end">;

} // namespace tokens


template<typename Types, token_concept<Types>... Tokens>
class token_instance {
public:
	template<typename... Args>
	ztu_ic token_instance(Args &&... args);

	template<Types Type, typename... Args>
	ztu_nic static decltype(auto) make(Args &&... args);

	ztu_nic static std::string_view name_of(Types type);

	template<Types Type, typename... Args>
	ztu_ic void set(Args &&... args);

	template<Types Type>
	ztu_nic decltype(auto) get();

	template<Types Type>
	ztu_nic decltype(auto) get() const;

	ztu_nic Types type() const;

private:
	using enum_t = std::underlying_type_t<Types>;
	inline constexpr static auto names = std::array{ Tokens::name()..., std::string_view("unknown") };

private:
	using value_t = std::variant<typename Tokens::value_t...>;
	value_t m_value;
};


template<typename Types, token_concept<Types>... Tokens>
template<typename... Args>
ztu_ic token_instance<Types, Tokens...>::token_instance(Args &&... args)
	: m_value(std::forward<Args>(args)...) {}

template<typename Types, token_concept<Types>... Tokens>
template<Types Type, typename... Args>
ztu_nic decltype(auto) token_instance<Types, Tokens...>::make(Args &&... args) {
	constexpr auto index = static_cast<size_t>(static_cast<enum_t>(Type));
	return token_instance(std::in_place_index<index>, std::forward<Args>(args)...);
}

template<typename Types, token_concept<Types>... Tokens>
ztu_nic std::string_view token_instance<Types, Tokens...>::name_of(Types type) {
	const auto index = static_cast<size_t>(static_cast<enum_t>(type));
	return names[std::min(index, sizeof...(Tokens))];
}

template<typename Types, token_concept<Types>... Tokens>
template<Types Type, typename... Args>
ztu_ic void token_instance<Types, Tokens...>::set(Args &&... args) {
	constexpr auto index = static_cast<size_t>(static_cast<enum_t>(Type));
	m_value.template emplace<index>(std::forward<Args>(args)...);
}

template<typename Types, token_concept<Types>... Tokens>
template<Types Type>
ztu_nic decltype(auto) token_instance<Types, Tokens...>::get() {
	constexpr auto index = static_cast<size_t>(static_cast<enum_t>(Type));
	return std::get<index>(m_value);
}

template<typename Types, token_concept<Types>... Tokens>
template<Types Type>
ztu_nic decltype(auto) token_instance<Types, Tokens...>::get() const {
	constexpr auto index = static_cast<size_t>(static_cast<enum_t>(Type));
	return std::get<index>(m_value);
}

template<typename Types, token_concept<Types>... Tokens>
ztu_nic Types token_instance<Types, Tokens...>::type() const {
	return static_cast<Types>(static_cast<enum_t>(m_value.index()));
}


using default_token_instance = token_instance<tokens::type,
	tokens::end,
	tokens::left_brace,
	tokens::right_brace,
	tokens::comma,
	tokens::colon,
	tokens::string,
	tokens::number,
	tokens::boolean,
	tokens::left_bracket,
	tokens::right_bracket,
	tokens::null
>;


struct source_location {
	size_t row, column;
	inline friend std::ostream &operator<<(std::ostream &os, const source_location &location);
};

std::ostream &operator<<(std::ostream &os, const source_location &location) {
	return os << '(' << location.row << ", " << location.column << ')';
}


template<typename Types, token_concept<Types>... Tokens>
class tokenizer {
private:
	using token_instance_t = token_instance<Types, Tokens...>;
	using this_t = tokenizer<Types, Tokens...>;

public:
	ztu_ic tokenizer();
	ztu_ic tokenizer(std::istream &is);

	ztu_ic void set_is(std::istream &is);

	ztu_ic this_t& operator>>(std::tuple<token_instance_t &, source_location &> dst);

	ztu_ic void accept();

	template<Types... Options>
	ztu_nic ssize_t ignore_until();

private:
	source_location m_location{ 1, 1 };
	std::istream *m_is{ nullptr };
	std::vector<char> m_buffer{};
};

template<typename Types, token_concept<Types>... Tokens>
ztu_ic tokenizer<Types, Tokens...>::tokenizer() = default;

template<typename Types, token_concept<Types>... Tokens>
ztu_ic tokenizer<Types, Tokens...>::tokenizer(std::istream &is) : m_is{ &is } {}

template<typename Types, token_concept<Types>... Tokens>
ztu_ic void tokenizer<Types, Tokens...>::set_is(std::istream &is) {
	m_is = &is;
	m_location = { 1, 1 };
}

template<typename Types, token_concept<Types>... Tokens>
ztu_ic tokenizer<Types, Tokens...>& tokenizer<Types, Tokens...>::operator>>(std::tuple<token_instance_t &, source_location &> dst) {

	auto &[token, location] = dst;

	while (not m_is->eof() and std::isspace(m_is->peek()))
		accept();

	// The guaranteed lifetime of the previous string token ends here
	m_buffer.clear();

	location = m_location;
	const auto before_pos = m_is->tellg();

	std::error_code token_error;

	const auto found_token = for_each::type<Tokens...>([&]<class Token>() {
		if (Token::begins_with(*m_is)) {
			token.template set<Token::type()>(
				Token::read(*m_is, m_buffer, token_error)
			);
			return true;
		}
		return false;
	});

	if (not found_token) {
		throw std::runtime_error(
			std::format(
				"Unexpected character {} at position ({}, {})",
				static_cast<char>(m_is->peek()),
				m_location.row,
				m_location.column
			)
		);
	}

	if (token_error) {
		throw std::runtime_error(
			std::format(
				"Error while parsing token {} token starting at position ({}, {}): {}",
				token_instance_t::name_of(token.type()),
				m_location.row,
				m_location.column,
				token_error.message()
			)
		);
	}


	const auto after_pos = m_is->tellg();
	if ((after_pos == -1 or m_is->fail()) and not (
		m_is->eof() and (
			token.type() == tokens::type::END or token.type() == tokens::type::NUMBER
		)
	)) {
		throw std::runtime_error(
			std::format(
				"Error while parsing {} token starting at position ({}, {})",
				token_instance_t::name_of(token.type()),
				m_location.row,
				m_location.column
			)
		);
	}

	// assuming there are no linebreaks within a token_concept
	m_location.column += after_pos - before_pos;

	return *this;
}

template<typename Types, token_concept<Types>... Tokens>
template<Types... Options>
ztu_nic ssize_t tokenizer<Types, Tokens...>::ignore_until() {
	// This function only works if the first character of each literal is unique
	// even across the rest of all other literals letters.
	ssize_t index = -1;
	while (true) {
		auto found_token = false;
		if (not std::isspace(m_is->peek())) {
			found_token = json_internal::for_each::indexed_value<Options...>([&]<auto Index, auto Option>() {
				return json_internal::for_each::type<Tokens...>([&]<typename Token>() {
					if constexpr (Option == Token::type()) {
						if (Token::begins_with(*m_is)) {
							index = Index;
							return true;
						}
					}
					return false;
				});
			});
		}
		accept();
		if (found_token) {
			break;
		}
	}
	return index;
}

template<typename Types, token_concept<Types>... Tokens>
ztu_ic void tokenizer<Types, Tokens...>::accept() {
	++m_location.column;
	if (m_is->get() == '\n') {
		++m_location.row;
		m_location.column = 1;
	}
}


enum class detokenizer_instructions {
	space, tab, indent, outdent, clear_indent, newline
};

struct detokenizer_format {
	std::string_view space, tab, newline;
};

namespace detokenizer_formats {

constexpr detokenizer_format pretty_tab{
	.space{ " " },
	.tab{ "\t" },
	.newline{ "\n" },
};

constexpr detokenizer_format pretty_space{
	.space{ " " },
	.tab{ " " },
	.newline{ "\n" },
};

constexpr detokenizer_format minimal{
	.space{ "" },
	.tab{ "" },
	.newline{ "" },
};

} // namespace detokenizer_formats

template<typename Types, token_concept<Types>... Tokens>
class detokenizer {
private:
	using token_instance_t = token_instance<Types, Tokens...>;
	using this_t = detokenizer<Types, Tokens...>;

public:
	ztu_ic detokenizer(const detokenizer_format &fmt = detokenizer_formats::pretty_tab);
	ztu_ic detokenizer(std::ostream &os, const detokenizer_format &fmt = detokenizer_formats::pretty_tab);

	ztu_nic std::ostream*& os();
	ztu_nic std::ostream*const& os() const;

	ztu_nic detokenizer_format& format();
	ztu_nic const detokenizer_format& format() const;

	ztu_ic this_t& operator<<(const token_instance_t &token);
	ztu_ic this_t& operator<<(const detokenizer_instructions &instruction);

private:
	std::ostream *m_os{ nullptr };
	detokenizer_format m_format;
	int m_depth{ 0 };
};

template<typename Types, token_concept<Types>... Tokens>
ztu_ic detokenizer<Types, Tokens...>::detokenizer(const detokenizer_format &fmt)
	: m_format{ fmt } {};

template<typename Types, token_concept<Types>... Tokens>
ztu_ic detokenizer<Types, Tokens...>::detokenizer(std::ostream &os, const detokenizer_format &fmt)
	: m_os{ &os }, m_format{ fmt } {}

template<typename Types, token_concept<Types>... Tokens>
ztu_nic std::ostream*& detokenizer<Types, Tokens...>::os() {
	return m_os;
}

template<typename Types, token_concept<Types>... Tokens>
ztu_nic std::ostream*const& detokenizer<Types, Tokens...>::os() const {
	return m_os;
}

template<typename Types, token_concept<Types>... Tokens>
ztu_nic detokenizer_format& detokenizer<Types, Tokens...>::format() {
	return m_format;
}

template<typename Types, token_concept<Types>... Tokens>
ztu_nic const detokenizer_format& detokenizer<Types, Tokens...>::format() const {
	return m_format;
}

template<typename Types, token_concept<Types>... Tokens>
ztu_ic detokenizer<Types, Tokens...>::this_t& detokenizer<Types, Tokens...>::operator<<(const detokenizer_instructions &instruction) {
	switch (instruction) {
		using
		enum detokenizer_instructions;
		case space: {
			(*m_os) << m_format.space;
			break;
		}
		case tab: {
			(*m_os) << m_format.tab;
			break;
		}
		case indent: {
			m_depth++;
			break;
		}
		case outdent: {
			if (m_depth > 0) {
				m_depth--;
			}
			break;
		}
		case clear_indent: {
			m_depth = 0;
			break;
		}
		case newline: {
			(*m_os) << m_format.newline;
			for (int i = 0; i < m_depth; i++) {
				(*m_os) << m_format.tab;
			}
			break;
		}
		default: {
			throw std::runtime_error(std::format("Unsupported instruction '{}'", int(instruction)));
		}
	}
	return *this;
}

template<typename Types, token_concept<Types>... Tokens>
ztu_ic detokenizer<Types, Tokens...>::this_t& detokenizer<Types, Tokens...>::operator<<(const token_instance_t &token) {

	std::error_code token_error;

	const auto type = token.type();
	const auto found_token = for_each::type<Tokens...>([&]<class Token>() {
		if (Token::type() == type) {
			Token::write(*m_os, token.template get<Token::type()>(), token_error);
			return true;
		}
		return false;
	});

	if (not found_token) {
		using namespace std::string_literals;
		throw std::runtime_error(
			std::format(
					"Unknown token '{}'",
					token_instance_t::name_of(type)
			)
		);
	}

	if (token_error) {
		throw std::runtime_error(
			std::format(
				"Error while serializing token {}: {}",
				token_instance_t::name_of(token.type()),
				token_error.message()
			)
		);
	}

	return *this;
}


template<typename Types, token_concept<Types>... Tokens>
class parser {
protected:
	using token_instance_t = token_instance<Types, Tokens...>;

public:
	ztu_ic parser();
	ztu_ic explicit parser(std::istream &is);

	ztu_ic void set_is(std::istream &is);

protected:
	ztu_ic void accept();

	ztu_ic token_instance_t accept(Types type);

protected:
	token_instance_t token;
	source_location location;
	tokenizer<Types, Tokens...> tokens;
};

template<typename Types, token_concept<Types>... Tokens>
ztu_ic parser<Types, Tokens...>::parser() : tokens() {};


template<typename Types, token_concept<Types>... Tokens>
ztu_ic void parser<Types, Tokens...>::parser::accept() {
	tokens >> std::tie(token, location);
}

template<typename Types, token_concept<Types>... Tokens>
ztu_ic parser<Types, Tokens...>::token_instance_t parser<Types, Tokens...>::accept(Types type) {
	if (token.type() != type) {
		throw std::runtime_error(
			std::format(
				"Expected token '{}' but got token '{}' at position ({}, {})",
				token_instance_t::name_of(type),
				token_instance_t::name_of(token.type()),
				location.row,
				location.column
			)
		);
	}
	auto old_token = std::move(token);
	if (old_token.type() != tokens::type::END) {
		accept();
	}
	return old_token;
}

template<typename Types, token_concept<Types>... Tokens>
ztu_ic parser<Types, Tokens...>::parser(std::istream &is) : tokens(is) {
	accept();
};

template<typename Types, token_concept<Types>... Tokens>
ztu_ic void parser<Types, Tokens...>::set_is(std::istream &is) {
	tokens.set_is(is);
	accept();
}


template<typename Types, token_concept<Types>... Tokens>
class serializer {
private:
	using token_instance_t = token_instance<Types, Tokens...>;

public:
	template<typename... Args>
	ztu_ic serializer(Args&&... args);

	ztu_nic std::ostream*& os();
	ztu_nic std::ostream*const& os() const;

	ztu_nic detokenizer_format& format();
	ztu_nic const detokenizer_format& format() const;

protected:
	using enum tokens::type;

	ztu_ic static auto
		lbrace = default_token_instance::make<LBRACE>(),
		rbrace = default_token_instance::make<RBRACE>(),
		lbracket = default_token_instance::make<LBRACKET>(),
		rbracket = default_token_instance::make<RBRACKET>(),
		colon = default_token_instance::make<COLON>(),
		comma = default_token_instance::make<COMMA>(),
		null = default_token_instance::make<UNDEFINED>(),
		end = default_token_instance::make<END>();

	ztu_nic static default_token_instance boolean(bool b);

	ztu_nic static default_token_instance string(std::string_view s);

	ztu_nic static default_token_instance number(double d);

protected:
	detokenizer<Types, Tokens...> tokens;
};

template<typename Types, token_concept<Types>... Tokens>
template<typename... Args>
ztu_ic serializer<Types, Tokens...>::serializer(Args&&... args) : tokens{ std::forward<Args>(args)...} {}

template<typename Types, token_concept<Types>... Tokens>
ztu_nic std::ostream*& serializer<Types, Tokens...>::os() {
	return tokens.os();
}

template<typename Types, token_concept<Types>... Tokens>
ztu_nic std::ostream*const& serializer<Types, Tokens...>::os() const {
	return tokens.os();
}

template<typename Types, token_concept<Types>... Tokens>
ztu_nic detokenizer_format& serializer<Types, Tokens...>::format() {
	return tokens.format();
}

template<typename Types, token_concept<Types>... Tokens>
ztu_nic const detokenizer_format& serializer<Types, Tokens...>::format() const {
	return tokens.format();
}

template<typename Types, token_concept<Types>... Tokens>
ztu_nic default_token_instance serializer<Types, Tokens...>::boolean(const bool b) {
	return default_token_instance::make<BOOLEAN>(b);
}

template<typename Types, token_concept<Types>... Tokens>
ztu_nic default_token_instance serializer<Types, Tokens...>::string(const std::string_view s) {
	return default_token_instance::make<STRING>(s);
}

template<typename Types, token_concept<Types>... Tokens>
ztu_nic default_token_instance serializer<Types, Tokens...>::number(const double d) {
	return default_token_instance::make<NUMBER>(d);
}


using default_base_parser = parser<tokens::type,
	tokens::end,
	tokens::left_brace,
	tokens::right_brace,
	tokens::comma,
	tokens::colon,
	tokens::string,
	tokens::number,
	tokens::boolean,
	tokens::left_bracket,
	tokens::right_bracket,
	tokens::null
>;

using default_base_serializer = serializer<tokens::type,
	tokens::end,
	tokens::left_brace,
	tokens::right_brace,
	tokens::comma,
	tokens::colon,
	tokens::string,
	tokens::number,
	tokens::boolean,
	tokens::left_bracket,
	tokens::right_bracket,
	tokens::null
>;

} // namespace transcoding


// exports
namespace ztu::json {
	using format = json_internal::transcoding::detokenizer_format;
	namespace formats = json_internal::transcoding::detokenizer_formats;
}

#endif

namespace ztu::json::dynamic {

enum class types {
	UNDEFINED = 0,
	BOOLEAN = 1,
	NUMBER = 2,
	STRING = 3,
	ARRAY = 4,
	OBJECT = 5,
	INVALID = 6
};

namespace data_types {

class value;
using null = std::monostate;
using boolean = bool;
using number = double;
using string = std::string;
using array = std::vector<value>;
using object = std::unordered_map<std::string, value>;

} // namespace data_types

class serializer;

namespace data_types_internal {
	using enum types;

	template<types EnumType>
	struct type_container {
		static constexpr auto value = EnumType;
	};

	template<typename T>
	struct get_value_type {
		static constexpr auto value = INVALID;
	};

	template<> struct get_value_type<data_types::null> : public type_container<UNDEFINED> {};
	template<> struct get_value_type<data_types::boolean> : public type_container<BOOLEAN> {};
	template<> struct get_value_type<data_types::number> : public type_container<NUMBER> {};
	template<> struct get_value_type<data_types::string> : public type_container<STRING> {};
	template<> struct get_value_type<data_types::array> : public type_container<ARRAY> {};
	template<> struct get_value_type<data_types::object> : public type_container<OBJECT> {};
}

template<typename T>
constexpr auto type_of_default(const T &) {
	return data_types_internal::get_value_type<std::remove_cvref_t<T>>::value;
}

namespace concepts {

template<class T>
concept type = data_types_internal::get_value_type<std::remove_cvref_t<T>>::value != types::INVALID;

template<class T, types EnumType>
concept of_type = data_types_internal::get_value_type<std::remove_cvref_t<T>>::value == EnumType;

template<class T>
concept undefined = of_type<T, types::UNDEFINED>;

template<class T>
concept boolean = of_type<T, types::BOOLEAN>;

template<class T>
concept number = of_type<T, types::NUMBER>;

template<class T>
concept string = of_type<T, types::STRING>;

template<class T>
concept array = of_type<T, types::ARRAY>;

template<class T>
concept object = of_type<T, types::OBJECT>;

template<class T>
concept primitive = (
	undefined<T> or boolean<T> or number<T> or string<T>
);

template<class T>
concept compounds = (
	array<T> or object<T>
);

} // namespace concepts

namespace data_types {

/**
 *  @brief	A container representing a dynamic json value.
 *
 * The value class represents a union of all the specified json data structures
 * (null, boolean, number, string, array and object) and can be dynamically assigned
 * to hold data of any single one of these types.
 * Access to the stored data is provided with direct accessors that forward
 * the types interface or a @c std::variant like interface to extract the data.
 */
class value {
public:
	/**
	 * @brief 	Construct a %value with internal type %null.
	 */
	ztu_ic value();

	/**
	 * @brief	Construct a %value storing the given json data structure.
	 */
	template<concepts::type T>
	ztu_ic value(T&& value);

	/**
	 * @brief	Destructs previously stored data and stores the given json data structure.
	 */
	template<concepts::type T>
	ztu_ic value& operator=(T&& value);

	/**
	 * @brief	Returns reference to specified json data structure.
	 *
	 * Return a reference to the specified stored json data structure
	 * or throw an error in case the specified json type does not match
	 * the dynamic json type of the @c value.
	 */
	template<concepts::type T>
	ztu_ic explicit operator T&();

	/**
	 * @brief	Returns constant reference to specified json data structure.
	 *
	 * @note	Will throw an error in case the specified json type does not match
	 * 			the dynamic json type of the @c value.
	 */
	template<concepts::type T>
	ztu_ic explicit operator const T&() const;

	/**
	 * @return	the enum type of the dynamically stored json data structure
	 */
	ztu_nic types type() const;

	/**
	 * @brief	Returns a reference to the stored json data structure.
	 *
	 * @note	Will throw an error in case the specified json type does not match
	 * 			the dynamic json type of the @c value.
	 *
	 * @tparam	Type specifies the json type to be accessed
	 *
	 * @return	reference to stored json structure
	 */
	template<types Type>
	ztu_nic auto& get();

	/**
	 * @brief	Returns a constant reference to the stored json data structure.
	 *
	 * @note	Will throw an error in case the specified json type does not match
	 * 			the dynamic json type of the @c value.
	 *
	 * @tparam	Type specifies the json type to be accessed
	 *
	 * @return	constant reference to stored json structure
	 */
	template<types Type>
	ztu_nic const auto& get() const;

	/**
	 * @brief	Returns a pointer to the stored json data structure.
	 *
	 * @tparam	Type specifies the json type to be accessed
	 *
	 * @return	pointer to stored json structure or nullptr
	 * 			in case the specified json type does not match
	 * 			the dynamic json type of the @c value.
	 */
	template<types Type>
	ztu_nic auto* get_if();

	/**
	 * @brief	Returns a constant pointer to the stored json data structure.
	 *
	 * @tparam	Type specifies the json type to be accessed
	 *
	 * @return	constant pointer to stored json structure or nullptr
	 * 			in case the specified json type does not match
	 * 			the dynamic json type of the @c value.
	 */
	template<types Type>
	ztu_nic const auto* get_if() const;

	/**
	 * @brief	Calls visitor with a reference to the stored json data structure.
	 */
	ztu_ic void visit(auto&& visitor);

	/**
	 * @brief	Calls visitor with a constant reference to the stored json data structure.
	 */
	ztu_ic void visit(auto&& visitor) const;

	/**
	 * @brief	Calls visitor with a reference to the stored json data structure and the structures type.
	 */
	ztu_ic void typed_visit(auto&& visitor);

	/**
	 * @brief	Calls visitor with a constant reference to the stored json data structure and the structures type.
	 */
	ztu_ic void typed_visit(auto&& visitor) const;

	/**
	 * @brief		Returns reference to stored @c array element at given index.
	 *
	 * @note		Will throw an error if the stored json data structure is not an @c array.
	 *
	 * @param index the index of the element to be accessed
	 *
	 * @return		Reference to stored @c array element at given index.
	 */
	ztu_nic value& operator[](int index);

	/**
	 * @brief		Returns constant reference to stored @c array element at given index.
	 *
	 * @note		Will throw an error if the stored json data structure is not an @c array.
	 *
	 * @param index the index of the element to be accessed
	 *
	 * @return		Constant reference to stored @c array element at given index.
	 */
	ztu_nic const value& operator[](int index) const;


	/**
	 * @brief		Returns reference to stored @c object element at given key.
	 *
	 * @note		Will throw an error if the stored json data structure is not an @c object.
	 *
	 * @param key	the key of the element to be accessed
	 *
	 * @return		Reference to stored @c object element at given key.
	 */
	ztu_nic value& operator[](const char* key);

	/**
	 *  @brief		Returns constant reference to stored @c object element at given key.
	 *
	 * @note		Will throw an error if the stored json data structure is not an @c object.
	 *
	 * @param key	the key of the element to be accessed
	 *
	 * @return		Constant reference to stored @cobject element at given key.
	 */
	ztu_nic const value& operator[](const char* key) const;

	/**
	 * @brief		Returns reference to stored @c object element at given key.
	 *
	 * @note		Will throw an error if the stored json data structure is not an @c object.
	 *
	 * @param key	the key of the element to be accessed
	 *
	 * @return		Reference to stored @cobject element at given key.
	 */
	ztu_nic value& operator[](const std::string& key);

	/**
	 * @brief		Returns constant reference to stored @c object element at given key.
	 *
	 * @note		Will throw an error if the stored json data structure is not an @c object.
	 *
	 * @param key	the key of the element to be accessed
	 *
	 * @return		Constant reference to stored @cobject element at given key.
	 */
	ztu_nic const value& operator[](const std::string& key) const;

	/**
	 * @brief	Returns size of stored @c array or @c string.
	 *
	 * @note	Will throw an error if the stored json data structure is neither an @c array or a @c string.
	 *
	 * @return	Constant reference to stored @cobject element at given key.
	 */
	ztu_nic size_t size() const;

	/**
	 * @brief	Returns length of stored @c string.
	 *
	 * @note	Will throw an error if the stored json data structure is not a @c string.
	 *
	 * @return	Length of stored @c string.
	 */
	ztu_nic size_t length() const;

	/**
	 * @brief Returns reference to the @c default_serializer format (used for default serialization).
	 *
	 * @return Reference to the @c default_serializer format.
	 */
	ztu_nic format& default_format();

	/**
	 * @brief Serializes the stored json structure to the given stream using the @c default_serializer.
	 *
	 * @return reference to given @c std::ostream
	 */
	inline friend std::ostream& operator<<(std::ostream&, const value&);

private:
	static constexpr size_t index_of(types type);
	static constexpr types type_of(size_t index);

	using variant_t = std::variant<null, boolean, number, string, array, object>;
	static constexpr size_t num_types = std::variant_size_v<variant_t>;
	variant_t data;
	static serializer default_serializer;
};


ztu_nic size_t value::index_of(types type) {
	using integral_t = std::underlying_type_t<types>;
	return static_cast<size_t>(static_cast<integral_t>(type));
}

ztu_nic types value::type_of(size_t index) {
	using integral_t = std::underlying_type_t<types>;
	return static_cast<types>(static_cast<integral_t>(index));
}

ztu_ic value::value()
	: data{ std::in_place_type<null> } {}

//ztu_ic value::value(value&& v) : data{ std::move(v.data) } {}

template<concepts::type T>
ztu_ic value::value(T&& value) : data{ std::move(value) } {}


template<concepts::type T>
ztu_ic value& value::operator=(T&& value) {
	data = value;
	return *this;
}

template<concepts::type T>
ztu_ic value::operator T&() {
	return std::get<T>(data);
}

template<concepts::type T>
ztu_ic value::operator const T&() const {
	return std::get<T>(data);
}

ztu_nic types value::type() const {
	return type_of(data.index());
}

template<types Type>
ztu_nic auto& value::get() {
	return std::get<index_of(Type)>(data);
}

template<types Type>
ztu_nic const auto& value::get() const {
	return std::get<index_of(Type)>(data);
}

template<types Type>
ztu_nic auto* value::get_if() {
	return std::get_if<index_of(Type)>(&data);
}

template<types Type>
ztu_nic const auto* value::get_if() const {
	return std::get_if<index_of(Type)>(&data);
}

ztu_ic void value::visit(auto&& visitor) {
	const auto index = data.index();
	json_internal::for_each::index<num_types>([&]<auto Index>() {
		if (Index == index) {
			visitor(*std::get_if<Index>(&data));
			return true;
		}
		return false;
	});
}

ztu_ic void value::typed_visit(auto&& visitor) {
	const auto index = data.index();
	json_internal::for_each::index<num_types>([&]<auto Index>() {
		static constexpr auto type = type_of(Index);
		if (Index == index) {
			visitor.template oprerator()<type>(
				*std::get_if<Index>(&data)
			);
			return true;
		}
		return false;
	});
}

ztu_ic void value::visit(auto&& visitor) const {
	const auto index = data.index();
	json_internal::for_each::index<num_types>([&]<auto Index>() {
		if (Index == index) {
			visitor(*std::get_if<Index>(&data));
			return true;
		}
		return false;
	});
}

ztu_ic void value::typed_visit(auto&& visitor) const {
	const auto index = data.index();
	json_internal::for_each::index<num_types>([&]<auto Index>() {
		static constexpr auto type = type_of(Index);
		if (Index == index) {
			visitor.template operator()<type>(
				*std::get_if<Index>(&data)
			);
			return true;
		}
		return false;
	});
}

ztu_nic value& value::operator[](const int index) {
	return this->get<types::ARRAY>()[index];
}

ztu_nic const value& value::operator[](const int index) const {
	return this->get<types::ARRAY>()[index];
}

ztu_nic value& value::operator[](const char* str) {
	return this->get<types::OBJECT>().at(str);
}

ztu_nic value& value::operator[](const std::string& str) {
	return this->get<types::OBJECT>().at(str);
}

ztu_nic const value& value::operator[](const char* str) const {
	return this->get<types::OBJECT>().at(str);
}

ztu_nic const value& value::operator[](const std::string& str) const {
	return this->get<types::OBJECT>().at(str);
}

ztu_nic size_t value::size() const {
	const auto array = this->get_if<types::ARRAY>();
	if (array) {
		return array->size();
	} else {
		return length();
	}
}

ztu_nic size_t value::length() const {
	return this->get<types::STRING>().length();
}
}

/**
 * @brief A parser for dynamic json values.
 *
 * @note The parser is not thread safe.
 */
class parser : public json_internal::transcoding::default_base_parser {
public:
	using json_internal::transcoding::default_base_parser::parser;

	/**
	 * @brief	Parses json structures fom the input stream and returns resulting value.
	 *
	 * @note	Will throw an error if given input stream represents a malformed json data structure.
	 *
	 * @return	The parsed json data structure.
	 */
	[[nodiscard]] inline data_types::value parse();

	/**
	 * @brief	Tries parsing a json @c array fom the input stream and returns resulting value.
	 *
	 * @note	Will throw an error if given input stream does not represent a json @c array.
	 *
	 * @return	The parsed json @c array.
	 */
	[[nodiscard]] inline data_types::array parse_array();

	/**
	 * @brief	Tries parsing a json @c object fom the input stream and returns resulting value.
	 *
	 * @note	Will throw an error if given input stream does not represent a json @c object.
	 *
	 * @return	The parsed json @c object.
	 */
	[[nodiscard]] inline data_types::object parse_object();

private:
	using token_type = json_internal::transcoding::tokens::type;

	template<token_type Type, typename T>
	inline T parse_primitive();
};


template<parser::token_type Type, typename T>
inline T parser::parse_primitive() {
	auto primitive = T(token.get<Type>());
	accept();
	return primitive;
}

data_types::object parser::parse_object() {
	using enum token_type;

	data_types::object obj;

	const auto parse_entry = [&]() {
		const auto key = std::string(accept(STRING).get<STRING>());
		accept(COLON);
		obj.insert_or_assign(std::move(key), parse());
	};

	accept(LBRACE);
	if (token.type() != RBRACE) {
		parse_entry();
		while (token.type() != RBRACE) {
			accept(COMMA);
			parse_entry();
		}
	}
	accept(RBRACE);

	return obj;
}

data_types::array parser::parse_array() {
	using enum token_type;

	data_types::array arr;

	accept(LBRACKET);
	if (token.type() != RBRACKET) {
		arr.push_back(parse());
		while (token.type() != RBRACKET) {
			accept(COMMA);
			arr.push_back(parse());
		}
	}
	accept(RBRACKET);

	return arr;
}

data_types::value parser::parse() {
	switch (token.type()) {
		using enum token_type;
		case LBRACE:
			return parse_object();
		case LBRACKET:
			return parse_array();
		case STRING:
			return parse_primitive<STRING, data_types::string>();
		case NUMBER:
			return parse_primitive<NUMBER, data_types::number>();
		case BOOLEAN:
			return parse_primitive<BOOLEAN, data_types::boolean>();
		case UNDEFINED:
			return parse_primitive<UNDEFINED, data_types::null>();
		default: {
			throw std::runtime_error(std::format("Unknown token '{}'", token_instance_t::name_of(token.type())));
		}
	}
}

/**
 * @brief A serializer for dynamic json values.
 *
 * @note The serializer is not thread safe.
 */
class serializer : public json_internal::transcoding::default_base_serializer {
public:
	using json_internal::transcoding::default_base_serializer::serializer;

	/**
	 * @brief	Serializes given json structures to the output stream.
	 */
	inline void serialize(const data_types::value& value);

	/**
	 * @brief	Serializes given json @c array to the output stream.
	 */
	inline void serialize_array(const data_types::array& value);

	/**
	 * @brief	Serializes given json @c object to the output stream.
	 */
	inline void serialize_object(const data_types::object& value);

private:
	using instructions = json_internal::transcoding::detokenizer_instructions;
};


inline void serializer::serialize_array(const data_types::array& value) {
	using enum instructions;

	tokens << lbracket;
	if (value.empty()) {
		tokens << space;
	} else {
		tokens << indent << newline;
		auto it = value.begin();
		while (it != value.end()) {
			serialize(*it);
			if (++it != value.end()) {
				tokens << comma << newline;
			}
		}
		tokens << outdent;
	}
	tokens << newline << rbracket;
}

inline void serializer::serialize_object(const data_types::object& value) {
	using enum instructions;

	tokens << lbrace << indent << newline;

	auto it = value.begin();
	while (it != value.end()) {
		tokens << string(it->first) << colon << space;
		serialize(it->second);
		if (++it != value.end()) {
			tokens << comma << newline;
		}
	}

	tokens << outdent << newline << rbrace;
}

inline void serializer::serialize(const data_types::value& value) {
	using token_type = json_internal::transcoding::tokens::type;
	using token_instance = json_internal::transcoding::default_token_instance;

	value.typed_visit([&]<types Type>(const auto& element) {
		if constexpr (Type == types::UNDEFINED) {
			tokens << null;
		} else if constexpr (Type == types::BOOLEAN) {
			tokens << token_instance::make<token_type::BOOLEAN>(element);
		} else if constexpr (Type == types::NUMBER) {
			tokens << token_instance::make<token_type::NUMBER>(element);
		} else if constexpr (Type == types::STRING) {
			tokens << token_instance::make<token_type::STRING>(element);
		} else if constexpr (Type == types::ARRAY) {
			serialize_array(element);
		} else if constexpr (Type == types::OBJECT) {
			serialize_object(element);
		}
	});
}

serializer data_types::value::default_serializer{ };

namespace data_types {

ztu_nic format& value::default_format() {
	return value::default_serializer.format();
}

std::ostream& operator<<(std::ostream& os, const data_types::value& value) {
	data_types::value::default_serializer.os() = &os;
	data_types::value::default_serializer.serialize(value);
	os.flush();
	return os;
}

} // namespace data_types

} // namespace ztu::json::dynamic
