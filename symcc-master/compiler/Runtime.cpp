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

#include "Runtime.h"

#include <llvm/ADT/StringSet.h>
#include <llvm/Config/llvm-config.h>
#include <llvm/IR/IRBuilder.h>

using namespace llvm;

namespace {

template <typename... ArgsTy>
SymFnT import(llvm::Module &M, llvm::StringRef name, llvm::Type *ret,
              ArgsTy... args) { // 在函数符号表中查找对应的函数，若存在且函数原型正确，则返回对应函数；若不存在或错误，则插入或修正
#if LLVM_VERSION_MAJOR >= 9 && LLVM_VERSION_MAJOR < 11
  return M.getOrInsertFunction(name, ret, args...).getCallee();
#else
  return M.getOrInsertFunction(name, ret, args...);
#endif
}

} // namespace

Runtime::Runtime(Module &M) {
  IRBuilder<> IRB(M.getContext());  // 根据模块的上下文构建IRbuilder
  auto *intPtrType = M.getDataLayout().getIntPtrType(M.getContext()); // 获取整数指针类型
  auto *ptrT = IRB.getInt8PtrTy();  // 获取指针类型
  auto *int8T = IRB.getInt8Ty();    // 获取8位int类型
  auto *voidT = IRB.getVoidTy();    // 获取void类型

  buildInteger = import(M, "_sym_build_integer", ptrT, IRB.getInt64Ty(), int8T);  // 获取int类型函数符号原型（若符号表中不存在或错误，则插入或修正），并将其返回
  buildInteger128 = import(M, "_sym_build_integer128", ptrT, IRB.getInt64Ty(),    // int128符号原型
                           IRB.getInt64Ty());
  buildFloat =
      import(M, "_sym_build_float", ptrT, IRB.getDoubleTy(), IRB.getInt1Ty());    // float符号原型
  buildNullPointer = import(M, "_sym_build_null_pointer", ptrT);                  // 空指针符号原型
  buildTrue = import(M, "_sym_build_true", ptrT);                                 // true符号原型
  buildFalse = import(M, "_sym_build_false", ptrT);                               // false符号原型
  buildBool = import(M, "_sym_build_bool", ptrT, IRB.getInt1Ty());                // bool符号原型
  buildSExt = import(M, "_sym_build_sext", ptrT, ptrT, int8T);                    //
  buildZExt = import(M, "_sym_build_zext", ptrT, ptrT, int8T);
  buildTrunc = import(M, "_sym_build_trunc", ptrT, ptrT, int8T);
  buildBswap = import(M, "_sym_build_bswap", ptrT, ptrT);
  buildIntToFloat = import(M, "_sym_build_int_to_float", ptrT, ptrT,
                           IRB.getInt1Ty(), IRB.getInt1Ty());
  buildFloatToFloat =
      import(M, "_sym_build_float_to_float", ptrT, ptrT, IRB.getInt1Ty());
  buildBitsToFloat =
      import(M, "_sym_build_bits_to_float", ptrT, ptrT, IRB.getInt1Ty());
  buildFloatToBits = import(M, "_sym_build_float_to_bits", ptrT, ptrT);
  buildFloatToSignedInt =
      import(M, "_sym_build_float_to_signed_integer", ptrT, ptrT, int8T);
  buildFloatToUnsignedInt =
      import(M, "_sym_build_float_to_unsigned_integer", ptrT, ptrT, int8T);
  buildFloatAbs = import(M, "_sym_build_fp_abs", ptrT, ptrT);
  buildBoolAnd = import(M, "_sym_build_bool_and", ptrT, ptrT, ptrT);
  buildBoolOr = import(M, "_sym_build_bool_or", ptrT, ptrT, ptrT);
  buildBoolXor = import(M, "_sym_build_bool_xor", ptrT, ptrT, ptrT);
  buildBoolToBits = import(M, "_sym_build_bool_to_bits", ptrT, ptrT, int8T);
  pushPathConstraint = import(M, "_sym_push_path_constraint", voidT, ptrT,
                              IRB.getInt1Ty(), intPtrType);

  setParameterExpression =
      import(M, "_sym_set_parameter_expression", voidT, int8T, ptrT);
  getParameterExpression =
      import(M, "_sym_get_parameter_expression", ptrT, int8T);
  setReturnExpression = import(M, "_sym_set_return_expression", voidT, ptrT);
  getReturnExpression = import(M, "_sym_get_return_expression", ptrT);

#define LOAD_BINARY_OPERATOR_HANDLER(constant, name)                           \
  binaryOperatorHandlers[Instruction::constant] =                              \
      import(M, "_sym_build_" #name, ptrT, ptrT, ptrT); // 将指令操作加入到函数符号表中，并将函数原型返回，构成“操作-符号原型”的映射

  LOAD_BINARY_OPERATOR_HANDLER(Add, add)                    // 加
  LOAD_BINARY_OPERATOR_HANDLER(Sub, sub)                    // 减
  LOAD_BINARY_OPERATOR_HANDLER(Mul, mul)                    // 乘
  LOAD_BINARY_OPERATOR_HANDLER(UDiv, unsigned_div)          // 无符号除
  LOAD_BINARY_OPERATOR_HANDLER(SDiv, signed_div)            // 有符号除
  LOAD_BINARY_OPERATOR_HANDLER(URem, unsigned_rem)          // 无符号取余
  LOAD_BINARY_OPERATOR_HANDLER(SRem, signed_rem)            // 有符号取余
  LOAD_BINARY_OPERATOR_HANDLER(Shl, shift_left)             // 向左移位
  LOAD_BINARY_OPERATOR_HANDLER(LShr, logical_shift_right)   // 逻辑右移
  LOAD_BINARY_OPERATOR_HANDLER(AShr, arithmetic_shift_right)// 算法右移
  LOAD_BINARY_OPERATOR_HANDLER(And, and)                    // 与
  LOAD_BINARY_OPERATOR_HANDLER(Or, or)                      // 或
  LOAD_BINARY_OPERATOR_HANDLER(Xor, xor)                    // 异或

  // Floating-point arithmetic
  LOAD_BINARY_OPERATOR_HANDLER(FAdd, fp_add)                // 浮点数加
  LOAD_BINARY_OPERATOR_HANDLER(FSub, fp_sub)                // 浮点数减
  LOAD_BINARY_OPERATOR_HANDLER(FMul, fp_mul)                // 浮点数乘
  LOAD_BINARY_OPERATOR_HANDLER(FDiv, fp_div)                // 浮点数除
  LOAD_BINARY_OPERATOR_HANDLER(FRem, fp_rem)                // 浮点数取余

#undef LOAD_BINARY_OPERATOR_HANDLER

#define LOAD_COMPARISON_HANDLER(constant, name)                                \
  comparisonHandlers[CmpInst::constant] =                                      \
      import(M, "_sym_build_" #name, ptrT, ptrT, ptrT); // 将比较操作加入到函数符号表中，并将函数原型返回，构成“操作-符号原型”的映射

  // 无序（Unordered）。当至少一个操作数是NaN时，返回True。若操作数都不是NaN时，返回False。
  // 有序（Ordered）。当操作数都不是Nan时，返回True。若至少一个操作数是NaN时，返回False。

  LOAD_COMPARISON_HANDLER(ICMP_EQ, equal)                   // equal
  LOAD_COMPARISON_HANDLER(ICMP_NE, not_equal)               // not equal
  LOAD_COMPARISON_HANDLER(ICMP_UGT, unsigned_greater_than)  // 无符号大于
  LOAD_COMPARISON_HANDLER(ICMP_UGE, unsigned_greater_equal) // 无符号大于等于
  LOAD_COMPARISON_HANDLER(ICMP_ULT, unsigned_less_than)     // 无符号小于
  LOAD_COMPARISON_HANDLER(ICMP_ULE, unsigned_less_equal)    // 无符号小于等于
  LOAD_COMPARISON_HANDLER(ICMP_SGT, signed_greater_than)    // 有符号大于
  LOAD_COMPARISON_HANDLER(ICMP_SGE, signed_greater_equal)   // 有符合大于等于
  LOAD_COMPARISON_HANDLER(ICMP_SLT, signed_less_than)       // 有符号小于
  LOAD_COMPARISON_HANDLER(ICMP_SLE, signed_less_equal)      // 有符号小于等于

  // Floating-point comparisons
  LOAD_COMPARISON_HANDLER(FCMP_OGT, float_ordered_greater_than)   // 浮点数大于       （ordered）
  LOAD_COMPARISON_HANDLER(FCMP_OGE, float_ordered_greater_equal)  // 浮点数大于等于   （ordered）
  LOAD_COMPARISON_HANDLER(FCMP_OLT, float_ordered_less_than)      // 浮点数小于       （ordered）
  LOAD_COMPARISON_HANDLER(FCMP_OLE, float_ordered_less_equal)     // 浮点数小于等于    （ordered）
  LOAD_COMPARISON_HANDLER(FCMP_OEQ, float_ordered_equal)          // 浮点数equal      （ordered）
  LOAD_COMPARISON_HANDLER(FCMP_ONE, float_ordered_not_equal)      // 浮点数not equal  （ordered）
  LOAD_COMPARISON_HANDLER(FCMP_ORD, float_ordered)                // 浮点数ordered，至少有一个操作数是NaN时返回False
  LOAD_COMPARISON_HANDLER(FCMP_UNO, float_unordered)              // 浮点数unordered，至少有一个操作数是NaN时返回True
  LOAD_COMPARISON_HANDLER(FCMP_UGT, float_unordered_greater_than) // 浮点数大于       （unordered）
  LOAD_COMPARISON_HANDLER(FCMP_UGE, float_unordered_greater_equal)// 浮点数大于等于   （unordered）
  LOAD_COMPARISON_HANDLER(FCMP_ULT, float_unordered_less_than)    // 浮点数小于       （unordered）
  LOAD_COMPARISON_HANDLER(FCMP_ULE, float_unordered_less_equal)   // 浮点数小于等于   （unordered）
  LOAD_COMPARISON_HANDLER(FCMP_UEQ, float_unordered_equal)        // 浮点数equal      （unordered）
  LOAD_COMPARISON_HANDLER(FCMP_UNE, float_unordered_not_equal)    // 浮点数not equal  （unordered）

#undef LOAD_COMPARISON_HANDLER

  memcpy = import(M, "_sym_memcpy", voidT, ptrT, ptrT, intPtrType);             // 设置memcpy符号原型
  memset = import(M, "_sym_memset", voidT, ptrT, ptrT, intPtrType);             // 设置memset符号原型
  memmove = import(M, "_sym_memmove", voidT, ptrT, ptrT, intPtrType);           // 设置memmove符号原型
  readMemory =
      import(M, "_sym_read_memory", ptrT, intPtrType, intPtrType, int8T);       // 设置readMemory符号原型
  writeMemory = import(M, "_sym_write_memory", voidT, intPtrType, intPtrType,   // 设置writeMemory符号原型
                       ptrT, int8T);
  buildInsert =
      import(M, "_sym_build_insert", ptrT, ptrT, ptrT, IRB.getInt64Ty(), int8T);
  buildExtract = import(M, "_sym_build_extract", ptrT, ptrT, IRB.getInt64Ty(),
                        IRB.getInt64Ty(), int8T);

  notifyCall = import(M, "_sym_notify_call", voidT, intPtrType);
  notifyRet = import(M, "_sym_notify_ret", voidT, intPtrType);
  notifyBasicBlock = import(M, "_sym_notify_basic_block", voidT, intPtrType);
}

/// Decide whether a function is called symbolically.确定是否以符号方式调用函数
bool isInterceptedFunction(const Function &f) {           // 判断是否是截取函数
  static const StringSet<> kInterceptedFunctions = {      // 截取函数集
      "malloc",   "calloc",  "mmap",    "mmap64", "open",   "read",    "lseek",
      "lseek64",  "fopen",   "fopen64", "fread",  "fseek",  "fseeko",  "rewind",
      "fseeko64", "getc",    "ungetc",  "memcpy", "memset", "strncpy", "strchr",
      "memcmp",   "memmove", "ntohl",   "fgets",  "fgetc", "getchar"};

  return (kInterceptedFunctions.count(f.getName()) > 0);
}
