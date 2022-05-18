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

#include "Pass.h"

#include <llvm/ADT/SmallVector.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Transforms/Utils/ModuleUtils.h>

#include "Runtime.h"
#include "Symbolizer.h"

using namespace llvm;

#ifndef NDEBUG
#define DEBUG(X)                                                               \
  do {                                                                         \
    X;                                                                         \
  } while (false)
#else
#define DEBUG(X) ((void)0)
#endif

char SymbolizePass::ID = 0;

bool SymbolizePass::doInitialization(Module &M) {   // 根据模块做初始工作
  DEBUG(errs() << "Symbolizer module init\n");

  // Redirect calls to external functions to the corresponding wrappers and
  // rename internal functions.
  for (auto &function : M.functions()) {      // 遍历模块中的函数
    auto name = function.getName();           // 获取函数名
    if (isInterceptedFunction(function))      // 若为截取函数（即设定的待插装函数）
      function.setName(name + "_symbolized"); // 进行改名处理
  }

  // Insert a constructor that initializes the runtime and any globals.
  // 插入一个构造函数，用于初始化所有运行时执行的函数和所有的目标
  Function *ctor;
  std::tie(ctor, std::ignore) = createSanitizerCtorAndInitFunctions(
      M, kSymCtorName, "_sym_initialize", {}, {});
      // 创建sanitizer构造函数，并从中调用sanitizer的init函数。返回:分别返回一对指向构造函数和init函数的指针。
  appendToGlobalCtors(M, ctor, 0);  // 将构造函数ctor加入到模块M的全局构造函数中

  return true;
}

bool SymbolizePass::runOnFunction(Function &F) {  // 随着Pass的执行而执行的函数
  auto functionName = F.getName();  // 获取函数名
  if (functionName == kSymCtorName) // 如果函数为系统构造函数，则跳过后续执行
    return false;

  DEBUG(errs() << "Symbolizing function ");
  DEBUG(errs().write_escaped(functionName) << '\n');

  SmallVector<Instruction *, 0> allInstructions;    // 指令集
  allInstructions.reserve(F.getInstructionCount()); // 初始化向量空间（按照函数中的IR指令数）
  for (auto &I : instructions(F))   // 遍历指令
    allInstructions.push_back(&I);  // 将指令压入指令集

  Symbolizer symbolizer(*F.getParent());
  symbolizer.symbolizeFunctionArguments(F); // 插入代码以获取函数参数的符号化表示

  for (auto &basicBlock : F)                // 遍历函数中的基本块
    symbolizer.insertBasicBlockNotification(basicBlock);  // 通过插入调用，使run-time库获得基本块的入口

  for (auto *instPtr : allInstructions)     // 遍历指令集并处理
    symbolizer.visit(instPtr);

  symbolizer.finalizePHINodes();    // 完成PHI节点的处理。（调用此函数将使符号表达式无效）
  symbolizer.shortCircuitExpressionUses();  // 重写符号计算，使其仅在存在操作数为符号的时候才发生；即对于纯具体的计算，不调用符号计算，跳过公式构造

  // DEBUG(errs() << F << '\n');
  assert(!verifyFunction(F, &errs()) &&
         "SymbolizePass produced invalid bitcode"); // 检查Pass过程是否有错误

  return true;
}
