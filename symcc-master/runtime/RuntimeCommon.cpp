// This file is part of SymCC.
//
// SymCC is free software: you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the Free Software
// Foundation, either version 3 of the License, or (at your option) any later
// version.
//
// SymCC is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
// A PARTICULAR PURPOSE. See the GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along with
// SymCC. If not, see <https://www.gnu.org/licenses/>.

#include <Runtime.h>

#include <array>
#include <cassert>
#include <numeric>

#include "GarbageCollection.h"
#include "RuntimeCommon.h"
#include "Shadow.h"

namespace {

constexpr int kMaxFunctionArguments = 256;  // 符号表达式参数个数上限为256

/// Global storage for function parameters and the return value.
SymExpr g_return_value; // 函数返回值（符号表达式）
std::array<SymExpr, kMaxFunctionArguments> g_function_arguments;  // 符号表达式参数组（用数组存储）
// TODO make thread-local

} // namespace

// 注意，此处设置的返回值只能是符号化返回值(具体值无需存储)
void _sym_set_return_expression(SymExpr expr) { g_return_value = expr; }

// 获取返回的符号表达式
SymExpr _sym_get_return_expression(void) {
  auto *result = g_return_value;
  // TODO this is a safeguard that can eventually be removed
  g_return_value = nullptr;
  return result;
}

// 设置参数表达式（根据索引index和表达式expr）
void _sym_set_parameter_expression(uint8_t index, SymExpr expr) {
  g_function_arguments[index] = expr; // 直接在数组中赋值即可
}

// 获得index对应参数的表达式
SymExpr _sym_get_parameter_expression(uint8_t index) {
  return g_function_arguments[index];
}

// 效仿memcpy，对影子内存中的符号表达式进行拷贝（相当于符号化的memcpy）
void _sym_memcpy(uint8_t *dest, const uint8_t *src, size_t length) {
  if (isConcrete(src, length) && isConcrete(dest, length))
    return; // 若均为具体值，则无需处理

  // 否则
  ReadOnlyShadow srcShadow(src, length);    // 创建只读影子内存视图
  ReadWriteShadow destShadow(dest, length); // 创建可读写影子内存视图
  std::copy(srcShadow.begin(), srcShadow.end(), destShadow.begin());  // 进行拷贝
}

// 效仿memset，对影子内存中的符号表达式进行初始化设置
void _sym_memset(uint8_t *memory, SymExpr value, size_t length) {
  if ((value == nullptr) && isConcrete(memory, length))
    return;

  ReadWriteShadow shadow(memory, length);   // 创建可读写影子内存视图
  std::fill(shadow.begin(), shadow.end(), value); // 用fill进行填写
}

// 效仿memmove，对影子内存中的符号表达式进行复制操作（注意区别memcpy）
void _sym_memmove(uint8_t *dest, const uint8_t *src, size_t length) {
  if (isConcrete(src, length) && isConcrete(dest, length))
    return;

  ReadOnlyShadow srcShadow(src, length);
  ReadWriteShadow destShadow(dest, length);
  if (dest > src) // 若目标内存区在源内存区之后，则采用从后往前拷贝
    std::copy_backward(srcShadow.begin(), srcShadow.end(), destShadow.end());
  else            // 否则从前往后拷贝
    std::copy(srcShadow.begin(), srcShadow.end(), destShadow.begin());
}

// 读内存区（可区分大端还是小端法）
SymExpr _sym_read_memory(uint8_t *addr, size_t length, bool little_endian) {
  assert(length && "Invalid query for zero-length memory region");

#ifdef DEBUG_RUNTIME  // 若为DEBUG，打印DEBUG信息
  std::cerr << "Reading " << length << " bytes from address " << P(addr)
            << std::endl;
  dump_known_regions();
#endif

  // If the entire memory region is concrete, don't create a symbolic expression
  // at all.
  // 若整个内存区都是具体的，直接返回，无需创建符号表达式
  if (isConcrete(addr, length))
    return nullptr;

  ReadOnlyShadow shadow(addr, length);  // 创建只读影子内存视图
  // 遍历影子内存，对读取到的内存进行读取并合并
  return std::accumulate(shadow.begin_non_null(), shadow.end_non_null(),  // 起始位置和结束位置
                         static_cast<SymExpr>(nullptr),             // 累加的初值
                         [&](SymExpr result, SymExpr byteExpr) {    // 遍历时的合并操作
                           if (result == nullptr) // 若当前结果为空（也就是刚开始）
                             return byteExpr; // 直接返回当前获取到的表达式（result=byteExpr）

                           return little_endian   // 否则，按照大小端进行合并拼接
                                      ? _sym_concat_helper(byteExpr, result)
                                      : _sym_concat_helper(result, byteExpr);
                         });
}

// 写内存区（区分大小端）
void _sym_write_memory(uint8_t *addr, size_t length, SymExpr expr,
                       bool little_endian) {
  assert(length && "Invalid query for zero-length memory region");

#ifdef DEBUG_RUNTIME
  std::cerr << "Writing " << length << " bytes to address " << P(addr)
            << std::endl;
  dump_known_regions();
#endif

  if (expr == nullptr && isConcrete(addr, length))
    return; // 若表达式为空或对应的内存区域中均有具体值，则不需继续

  // 创建可读写影子内存视图进行写入操作
  ReadWriteShadow shadow(addr, length);
  if (expr == nullptr) {
    std::fill(shadow.begin(), shadow.end(), nullptr);
  } else {
    size_t i = 0;
    for (SymExpr &byteShadow : shadow) {  // 遍历目标影子内存区
      byteShadow = little_endian  // 根据大小端不同，进行写入
                       ? _sym_extract_helper(expr, 8 * (i + 1) - 1, 8 * i)
                       : _sym_extract_helper(expr, (length - i) * 8 - 1,
                                             (length - i - 1) * 8);
      i++;
    }
  }
}

//
SymExpr _sym_build_extract(SymExpr expr, uint64_t offset, uint64_t length,
                           bool little_endian) {
  size_t totalBits = _sym_bits_helper(expr);  // 获取位数
  assert((totalBits % 8 == 0) && "Aggregate type contains partial bytes");
  // 若不符合整除8，则表达式不完整（因为为字节序列）

  SymExpr result;
  if (little_endian) {  // 若为小端法
    result = _sym_extract_helper(expr, totalBits - offset * 8 - 1,
                                 totalBits - offset * 8 - 8);
    for (size_t i = 1; i < length; i++) {
      result = _sym_concat_helper(
          _sym_extract_helper(expr, totalBits - (offset + i) * 8 - 1,
                              totalBits - (offset + i + 1) * 8),
          result);
    }
  } else {
    result = _sym_extract_helper(expr, totalBits - offset * 8 - 1,
                                 totalBits - (offset + length) * 8);
  }

  return result;
}

SymExpr _sym_build_bswap(SymExpr expr) {
  size_t bits = _sym_bits_helper(expr);
  assert((bits % 16 == 0) && "bswap is not applicable");
  return _sym_build_extract(expr, 0, bits / 8, true);
}

SymExpr _sym_build_insert(SymExpr target, SymExpr to_insert, uint64_t offset,
                          bool little_endian) {
  size_t bitsToInsert = _sym_bits_helper(to_insert);
  assert((bitsToInsert % 8 == 0) &&
         "Expression to insert contains partial bytes");

  SymExpr beforeInsert =
      (offset == 0) ? nullptr : _sym_build_extract(target, 0, offset, false);
  SymExpr newPiece = little_endian ? _sym_build_bswap(to_insert) : to_insert;
  uint64_t afterLen =
      (_sym_bits_helper(target) / 8) - offset - (bitsToInsert / 8);
  SymExpr afterInsert =
      (afterLen == 0) ? nullptr
                      : _sym_build_extract(target, offset + (bitsToInsert / 8),
                                           afterLen, false);

  SymExpr result = beforeInsert;
  if (result == nullptr) {
    result = newPiece;
  } else {
    result = _sym_concat_helper(result, newPiece);
  }

  if (afterInsert != nullptr) {
    result = _sym_concat_helper(result, afterInsert);
  }

  return result;
}

void _sym_register_expression_region(SymExpr *start, size_t length) {
  registerExpressionRegion({start, length});
}
