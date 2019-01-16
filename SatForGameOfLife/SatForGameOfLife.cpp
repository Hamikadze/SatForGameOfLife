#include "pch.h"
#include <array>
#include <vector>
#include <bitset>
#include <fstream>
#include <functional>
#include <type_traits>
#include <string>
#include <sstream>
#include <iostream>
#include <cmath>

class GameOfLifeCnf
{
	template <size_t N>
	using Mask = std::bitset<N>;
	template <size_t N>
	using Masks = std::vector<Mask<N>>;

private:
	struct MaskSize
	{
		static const uint32_t REGULAR = 9, SIDE = 6, CORNER = 4;
	};

	enum class MaskType : uint_fast32_t {
		REGULAR,
		LEFT, TOP, RIGHT, BOTTOM,
		LEFT_TOP, RIGHT_TOP, LEFT_BOTTOM, RIGHT_BOTTOM
	};

	template <typename ...Bits>
	static constexpr auto convertMasks(const std::vector<std::bitset<MaskSize::REGULAR>> & masks, Bits... bits) {
		static_assert((std::is_convertible<Bits, uint32_t>::value && ...));

		std::vector<std::bitset<MaskSize::REGULAR - sizeof...(Bits)>> newMasks;
		for (const auto & mask : masks) {
			if ((mask.test(bits) && ...)) {
				std::bitset<MaskSize::REGULAR - sizeof...(Bits)> newMask;
				uint32_t index = 0;
				for (uint32_t i = 0; i < MaskSize::REGULAR; ++i) {
					if (((i != bits) && ...))
						newMask[index++] = mask.test(i);
				}
				newMasks.emplace_back(newMask);
			}
		}

		return newMasks;
	}

	template <MaskType Type = MaskType::REGULAR>
	auto getVarNumbers(uint32_t param1 = 0, uint32_t param2 = 0)
	{
		std::function<int32_t(uint32_t)> calcLiteral;
		if constexpr (Type == MaskType::REGULAR)
			calcLiteral = [&](const uint32_t index) constexpr->int32_t { return 1 + columns_ + 2 + 1 + (param1 + index / 3 - 1) * (columns_ + 2) + (param2 + index % 3 - 1); };
		else if constexpr (Type == MaskType::LEFT)
			calcLiteral = [&](const uint32_t index) constexpr->int32_t { return 1 + (param1 + index / 2 - 1) * (columns_ + 2) + index % 2; };
		else if constexpr (Type == MaskType::TOP)
			calcLiteral = [&](const uint32_t index) constexpr->int32_t { return param1 + index / 3 * (columns_ + 2) + index % 3; };
		else if constexpr (Type == MaskType::RIGHT)
			calcLiteral = [&](const uint32_t index) constexpr->int32_t { return 1 + columns_ + (param1 + index / 2 - 1) * (columns_ + 2) + index % 2; };
		else if constexpr (Type == MaskType::BOTTOM)
			calcLiteral = [&](const uint32_t index) constexpr->int32_t { return param1 + (rows_ + index / 3) * (columns_ + 2) + index % 3; };
		else if constexpr (Type == MaskType::LEFT_TOP)
			calcLiteral = [&](const uint32_t index) constexpr->int32_t { return 1 + index / 2 * (columns_ + 2) + index % 2; };
		else if constexpr (Type == MaskType::RIGHT_TOP)
			calcLiteral = [&](const uint32_t index) constexpr->int32_t { return 1 + columns_ + (index / 2)*(columns_ + 2) + index % 2; };
		else if constexpr (Type == MaskType::LEFT_BOTTOM)
			calcLiteral = [&](const uint32_t index) constexpr->int32_t { return 1 + (rows_ + index / 2) * (columns_ + 2) + index % 2; };
		else if constexpr (Type == MaskType::RIGHT_BOTTOM)
			calcLiteral = [&](const uint32_t index) constexpr->int32_t { return 1 + columns_ + (rows_ + index / 2) * (columns_ + 2) + index % 2; };
		else
			throw std::exception();

		constexpr uint32_t varNum = Type == MaskType::REGULAR ? MaskSize::REGULAR : Type == MaskType::LEFT || Type == MaskType::TOP || Type == MaskType::RIGHT || Type == MaskType::BOTTOM ? MaskSize::SIDE : MaskSize::CORNER;
		std::array<int32_t, varNum> literals;
		for (uint32_t i = 0; i < varNum; i++)
			literals[i] = calcLiteral(i);

		return literals;
	}

	// вывод литералов
	template <size_t N>
	void outputLiterals(const std::array<int32_t, N> & literals) {
		cnf_ << '(';
		for (size_t i = 0; i < literals.size(); i++) {
			if (literals[i] < 0) {
				cnf_ << '~' << 'x' << -literals[i];
			}
			else
				cnf_ << 'x' << literals[i];

			dimacs_ << literals[i] << " ";
			if (i != literals.size() - 1)
				cnf_ << '|';
		}
		dimacs_ << "0\n";
		cnf_ << ')';
	}

	template <size_t N>
	void handleMasks(const std::vector<std::bitset<N>> & masks, const std::array<int32_t, N> &varNumbers) {
		for (const auto &mask : masks) {
			auto literals = varNumbers;
			for (uint32_t i = 0; i < N; i++) {
				if (!mask.test(i))
					literals[i] = -literals[i];
			}
			outputLiterals(literals);
		}
	}

	// вспомогательные функции
	static constexpr auto numberOfSetBits(uint32_t i) {
		i = i - ((i >> 1) & 0x55555555);
		i = (i & 0x33333333) + ((i >> 2) & 0x33333333);
		return (((i + (i >> 4)) & 0x0F0F0F0F) * 0x01010101) >> 24;
	}

	static constexpr auto assembleMask(const uint8_t params, const bool e) {
		return ~(uint32_t(params & 0xF0) << 1 | (params & 0xF) | e << 4);
	}

	static Masks<MaskSize::REGULAR> cnfMasksLive, cnfMasksDead;
	static Masks<MaskSize::SIDE> cnfLeft, cnfTop, cnfRight, cnfBottom;
	static Masks<MaskSize::CORNER> cnfLeftTop, cnfRightTop, cnfLeftBottom, cnfRightBottom;

	size_t columns_, rows_;
	std::stringstream cnf_, dimacs_;

public:
	static void init()
	{
		// строим маски КНФ
		for (uint32_t i = 0; i < 1 << 8; i++) {
			const auto sb = numberOfSetBits(i);
			if (sb != 3) {
				cnfMasksLive.emplace_back(assembleMask(i, false));
				if (sb != 2)
					cnfMasksLive.emplace_back(assembleMask(i, true));
				else
					cnfMasksDead.emplace_back(assembleMask(i, true));
			}
			else {
				cnfMasksDead.emplace_back(assembleMask(i, true));
				cnfMasksDead.emplace_back(assembleMask(i, false));
			}
		}
		// маски добавочных клеток
		cnfLeft = convertMasks(cnfMasksDead, 0, 3, 6);
		cnfTop = convertMasks(cnfMasksDead, 0, 1, 2);
		cnfRight = convertMasks(cnfMasksDead, 2, 5, 8);
		cnfBottom = convertMasks(cnfMasksDead, 6, 7, 8);
		cnfLeftTop = convertMasks(cnfMasksDead, 0, 1, 2, 3, 6);
		cnfRightTop = convertMasks(cnfMasksDead, 0, 1, 2, 5, 8);
		cnfLeftBottom = convertMasks(cnfMasksDead, 0, 3, 6, 7, 8);
		cnfRightBottom = convertMasks(cnfMasksDead, 2, 5, 6, 7, 8);
	}

	GameOfLifeCnf(const size_t rows, const size_t columns, const std::vector<bool> & configuration) : columns_(columns), rows_(rows)
	{
		// пишем КНФ и DIMACS
		const auto liveNum = std::count_if(configuration.begin(), configuration.end(), [](const bool val) { return val; });
		const auto varNum = (rows + 2)*(columns + 2);
		const auto clauseNum = cnfMasksLive.size() * liveNum + cnfMasksDead.size() * (varNum - liveNum) +
			cnfLeftTop.size() + cnfRightTop.size() + cnfLeftBottom.size() + cnfRightBottom.size() + rows * (cnfLeft.size() + cnfRight.size()) + columns * (cnfTop.size() + cnfBottom.size());
		dimacs_ << "p cnf " << varNum << ' ' << clauseNum << '\n';

		// КНФ клеток конфигурации
		for (uint32_t i = 0; i < rows; i++) {
			for (uint32_t j = 0; j < columns; j++) {
				const auto & masks = configuration[i*columns + j] ? cnfMasksLive : cnfMasksDead;
				const auto varNumbers = getVarNumbers(i, j);
				handleMasks(masks, varNumbers);
			}
		}

		// КНФ внешних клеток
		auto varNumbers = getVarNumbers<MaskType::LEFT_TOP>();
		handleMasks(cnfLeftTop, varNumbers);
		varNumbers = getVarNumbers<MaskType::RIGHT_TOP>();
		handleMasks(cnfRightTop, varNumbers);
		varNumbers = getVarNumbers<MaskType::LEFT_BOTTOM>();
		handleMasks(cnfLeftBottom, varNumbers);
		varNumbers = getVarNumbers<MaskType::RIGHT_BOTTOM>();
		handleMasks(cnfRightBottom, varNumbers);

		for (uint32_t i = 1; i <= rows; i++) {
			auto varNumbers = getVarNumbers<MaskType::LEFT>(i);
			handleMasks(cnfLeft, varNumbers);
			varNumbers = getVarNumbers<MaskType::RIGHT>(i);
			handleMasks(cnfRight, varNumbers);
		}

		for (uint32_t i = 1; i <= columns; i++) {
			auto varNumbers = getVarNumbers<MaskType::TOP>(i);
			handleMasks(cnfTop, varNumbers);
			varNumbers = getVarNumbers<MaskType::BOTTOM>(i);
			handleMasks(cnfBottom, varNumbers);
		}
	}

	auto getCnf() const
	{
		return cnf_.str();
	}

	auto getDimacs() const
	{
		return dimacs_.str();
	}
};

GameOfLifeCnf::Masks<GameOfLifeCnf::MaskSize::REGULAR> GameOfLifeCnf::cnfMasksLive = {};
GameOfLifeCnf::Masks<GameOfLifeCnf::MaskSize::REGULAR> GameOfLifeCnf::cnfMasksDead = {};
GameOfLifeCnf::Masks<GameOfLifeCnf::MaskSize::SIDE>	GameOfLifeCnf::cnfLeft = {};
GameOfLifeCnf::Masks<GameOfLifeCnf::MaskSize::SIDE>	GameOfLifeCnf::cnfTop = {};
GameOfLifeCnf::Masks<GameOfLifeCnf::MaskSize::SIDE>	GameOfLifeCnf::cnfRight = {};
GameOfLifeCnf::Masks<GameOfLifeCnf::MaskSize::SIDE>	GameOfLifeCnf::cnfBottom = {};
GameOfLifeCnf::Masks<GameOfLifeCnf::MaskSize::CORNER> GameOfLifeCnf::cnfLeftTop = {};
GameOfLifeCnf::Masks<GameOfLifeCnf::MaskSize::CORNER> GameOfLifeCnf::cnfRightTop = {};
GameOfLifeCnf::Masks<GameOfLifeCnf::MaskSize::CORNER> GameOfLifeCnf::cnfLeftBottom = {};
GameOfLifeCnf::Masks<GameOfLifeCnf::MaskSize::CORNER> GameOfLifeCnf::cnfRightBottom = {};

int main() {
	auto out = std::ofstream("output.txt");
	auto outDimacs = std::ofstream("dimacs.cnf");
	auto input = std::ifstream("input.txt");
	std::vector<bool> input_v(0);

	int count_number = 0;
	int current_number = 0;
	while (input >> current_number) {
		input_v.push_back(((bool)current_number));
		count_number++;
	}

	try {
		GameOfLifeCnf::init();
		GameOfLifeCnf cnf(((int)sqrt(count_number)), ((int)sqrt(count_number)), input_v);

		out << cnf.getCnf();
		outDimacs << cnf.getDimacs();
	}
	catch (const std::exception & e)
	{
		std::cerr << e.what();
	}
	system("pause");
}