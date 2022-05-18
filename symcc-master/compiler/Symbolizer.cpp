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

#include "Symbolizer.h"

#include <cstdint>
#include <llvm/ADT/SmallPtrSet.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/GetElementPtrTypeIterator.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/Transforms/Utils/BasicBlockUtils.h>

#include "Runtime.h"

using namespace llvm;

// 获取函数参数的符号表达式
void Symbolizer::symbolizeFunctionArguments(Function &F) {
  // The main function doesn't receive symbolic arguments.
  if (F.getName() == "main")
    return;

  IRBuilder<> IRB(F.getEntryBlock().getFirstNonPHI());  // 创建IRBuilder（此处为创建entry代码块）

  for (auto &arg : F.args()) {  // 遍历函数参数
    if (!arg.user_empty())      // 若不为空
      // 创建一个调用，以获取对应参数的符号表达式（通过去符号表中查找），随后按照其索引存入map中
      symbolicExpressions[&arg] = IRB.CreateCall(runtime.getParameterExpression,
                                                 IRB.getInt8(arg.getArgNo()));
  }
}

// 向运行库中插入基本块入口的相关通知
void Symbolizer::insertBasicBlockNotification(llvm::BasicBlock &B) {
  IRBuilder<> IRB(&*B.getFirstInsertionPt());   // 根据基本块中的第一条指令（跳过PHI等指令），作为入口创建IRBuilder
  IRB.CreateCall(runtime.notifyBasicBlock, getTargetPreferredInt(&B));  // 创建调用，插入入口通知
}

// 完成PHI节点的处理。（调用此函数将使符号表达式无效）
void Symbolizer::finalizePHINodes() {
  SmallPtrSet<PHINode *, 32> nodesToErase;  // 创建待清除的PHI结点集合

  for (auto *phi : phiNodes) {  // 遍历函数中所有的PHI结点（phiNodes存储了函数中所有的PHI结点）
    auto symbolicPHI = cast<PHINode>(symbolicExpressions[phi]); // 根据映射创建PHI节点对应的符号化PHI节点

    // A PHI node that receives only compile-time constants can be replaced by
    // a null expression.
    // 仅接受编译时常量的PHI节点可以替换为空表达式
    if (std::all_of(phi->op_begin(), phi->op_end(), [this](Value *input) {
          return (getSymbolicExpression(input) == nullptr);
        })) {
      nodesToErase.insert(symbolicPHI); // 添加到erase集合
      continue;
    }

    // 若不是，则继续处理
    for (unsigned incoming = 0, totalIncoming = phi->getNumIncomingValues();
         incoming < totalIncoming; incoming++) {  // 遍历每一个入边（即遍历每一个可能的值）
      symbolicPHI->setIncomingValue(
          incoming,
          getSymbolicExpressionOrNull(phi->getIncomingValue(incoming)));  // 获取入边对应的值的符号表达式，并将其加入到符号化PHI节点中
    }
  }

  for (auto *symbolicPHI : nodesToErase) {  //遍历erase集中的PHI结点，进行逐一清除
    symbolicPHI->replaceAllUsesWith(
        ConstantPointerNull::get(cast<PointerType>(symbolicPHI->getType()))); // 将所有指向指为空（同时会清除掉原指向）
    symbolicPHI->eraseFromParent(); // 从基本块中将对应PHI进行删除
  }

  // Replacing all uses has fixed uses of the symbolic PHI nodes in existing
  // code, but the nodes may still be referenced via symbolicExpressions. We
  // therefore invalidate symbolicExpressions, meaning that it cannot be used
  // after this point.
  symbolicExpressions.clear();  // 清除映射map
}

// 重写符号计算，使其仅在存在操作数为符号的时候才发生；即对于纯具体的计算，不调用符号计算，跳过公式构造（此处将具体值视为空）
void Symbolizer::shortCircuitExpressionUses() {
  for (auto &symbolicComputation : expressionUses) {  // 遍历可短路的表达式（例如全为具体值的)
    assert(!symbolicComputation.inputs.empty() &&
           "Symbolic computation has no inputs");

    IRBuilder<> IRB(symbolicComputation.firstInstruction);  // 以第一条指令为开始创建IRBuilder

    // Build the check whether any input expression is non-null (i.e., there
    // is a symbolic input).
    auto *nullExpression = ConstantPointerNull::get(IRB.getInt8PtrTy());  // 获得指向null的常量指针
    std::vector<Value *> nullChecks;  // 空指针对比集
    for (const auto &input : symbolicComputation.inputs) {  // 遍历符号计算中的input
      nullChecks.push_back(
          IRB.CreateICmpEQ(nullExpression, input.getSymbolicOperand()));  // 创建一个等值比较（空表达式，input的符号表达）
    }
    auto *allConcrete = nullChecks[0];  // 通过与操作，将所有的判断结果结合起来
    for (unsigned argIndex = 1; argIndex < nullChecks.size(); argIndex++) { // 遍历所有的子等值比较，进行与结合
      allConcrete = IRB.CreateAnd(allConcrete, nullChecks[argIndex]);       // allConcrete = allConcrete && nullChecks[i]
    }

    // The main branch: if we don't enter here, we can short-circuit the
    // symbolic computation. Otherwise, we need to check all input expressions
    // and create an output expression.
    // 若无法到达此位置，则表示可以短路（即触发了上述的空值检查，即操作数均为具体值，无需进行后续符号计算）
    // 若可以到达，则需检查处理所有的input表达式，并创建output表达式
    auto *head = symbolicComputation.firstInstruction->getParent(); // 获取首指令的父级调用
    auto *slowPath = SplitBlock(head, symbolicComputation.firstInstruction);// 将该指令和其父级调用进行分割
    // 原基本块分割为 该指令调用之前的部分(A) 和 以指令调用为开始的部分(B), A沿用之前的Old基本块，B存入一个新的基本块，用无条件goto进行连接
    // 返回新的基本块

    auto *tail = SplitBlock(slowPath,
                            symbolicComputation.lastInstruction->getNextNode());  // 根据尾指令同样进行分割，分割出后续处理基本块tail
    ReplaceInstWithInst(head->getTerminator(),
                        BranchInst::Create(tail, slowPath, allConcrete)); // 创建条件跳转指令，代替head基本块的原本结束指令
                        // 若allConcrete满足，则跳过slowPath，直接跳转到tail（执行后续操作）；若不满足，则跳到slowPath，执行正常符号计算操作

    // In the slow case, we need to check each input expression for null
    // (i.e., the input is concrete) and create an expression from the
    // concrete value if necessary.
    // 检查每个表达式的符号操作是否为空（即具体值），对于具体值，需要生成对应的符号表达式用于代替
    // 此处有两种具体值，一种是编译时就得知的具体值，一种是运行时的具体值。
    // 前者编译完成后查表即可得知，后者需要在运行时进行代码插桩判断
    auto numUnknownConcreteness = std::count_if(
        symbolicComputation.inputs.begin(), symbolicComputation.inputs.end(),
        [&](const Input &input) {
          return (input.getSymbolicOperand() != nullExpression);
        }); // 遍历inputs，计数得到不是具体值的表达式的个数
    for (unsigned argIndex = 0; argIndex < symbolicComputation.inputs.size();
         argIndex++) {  // 遍历inputs
      auto &argument = symbolicComputation.inputs[argIndex];          // 获取当前遍历到的input
      auto *originalArgExpression = argument.getSymbolicOperand();    // 获取input对应的符号操作表达式
      auto *argCheckBlock = symbolicComputation.firstInstruction->getParent();  // 获取父级调用

      // We only need a run-time check for concreteness if the argument isn't
      // known to be concrete at compile time already. However, there is one
      // exception: if the computation only has a single argument of unknown
      // concreteness, then we know that it must be symbolic since we ended up
      // in the slow path. Therefore, we can skip expression generation in
      // that case.
      bool needRuntimeCheck = originalArgExpression != nullExpression;// 若表达式不为空，则需要RuntimeCheck（进一步判断是否为运行时具体值）
      if (needRuntimeCheck && (numUnknownConcreteness == 1))  // 若不是具体值的input数量仅为1（也就是，当前这个必然不是具体值）
        continue; // 则跳过后续的表达式生成（此表达式生成针对具体值）

      if (needRuntimeCheck) { // 若需要（表达式不为编译时具体值）
        auto *argExpressionBlock = SplitBlockAndInsertIfThen(
            nullChecks[argIndex], symbolicComputation.firstInstruction,
            /* unreachable */ false); // 创建一条分割指令：在首指令的位置，对基本块进行分割，并插入条件跳转语句（条件为当前input和空值的等值判断）
            // 即若运行时其为具体值，则会执行新插入的基本块（执行完毕后回来）
        IRB.SetInsertPoint(argExpressionBlock); // 设置插入点（代码插桩点）为该分割指令（之后）
      } else {
        IRB.SetInsertPoint(symbolicComputation.firstInstruction); // 设置插入点（代码插桩点）为首指令（之后）
      }

      // 在设置了插入点后，生成处理代码并插入
      auto *newArgExpression =
          createValueExpression(argument.concreteValue, IRB); // 根据input的具体值，创建一个表达式用于代表运行时具体值，并返回到newArgExpression中

      Value *finalArgExpression;
      if (needRuntimeCheck) { // 若需要（此时插入点为分割指令之后，也就是ThenBlock的开头
        IRB.SetInsertPoint(symbolicComputation.firstInstruction); // 设置插入点回到首指令(之后)
        auto *argPHI = IRB.CreatePHI(IRB.getInt8PtrTy(), 2);      // 新建一个PHI点（预留2个入边位置）
        argPHI->addIncoming(originalArgExpression, argCheckBlock);// 第一个入边为从父级调用而来的input的符号表达式
        argPHI->addIncoming(newArgExpression, newArgExpression->getParent()); // 第二个入边为根据input的运行时的具体值生成的表达式
        finalArgExpression = argPHI;          // 最终表达式则是根据PHI点，从两个入边中选取正确的
                                              // 例如，运行时不为具体值，则选取originalArgExpression，即原始的符号表达式
                                              // 若运行时为具体值，则选取newArgExpression，即临时为该具体值生成的符号表达式
      } else {
        finalArgExpression = newArgExpression;// 若编译时就可确认为具体值，则直接使用根据具体值生成的符号表达式
      }

      argument.replaceOperand(finalArgExpression);  // 用修改后得到的符号表达式代替input
    }

    // Finally, the overall result (if the computation produces one) is null
    // if we've taken the fast path and the symbolic expression computed above
    // if short-circuiting wasn't possible.
    if (!symbolicComputation.lastInstruction->use_empty()) {  // 若仍有后续要处理的代码
      IRB.SetInsertPoint(&tail->front());           // 则在其开头设置插入点
      auto *finalExpression = IRB.CreatePHI(IRB.getInt8PtrTy(), 2);   // 创建PHI点，预留两条入边
      symbolicComputation.lastInstruction->replaceAllUsesWith(finalExpression);   // 重写输出为finalExpression
      finalExpression->addIncoming(ConstantPointerNull::get(IRB.getInt8PtrTy()),
                                   head);                     // 添加一条入边为空（由首指令的父级调用产生）
      finalExpression->addIncoming(
          symbolicComputation.lastInstruction,
          symbolicComputation.lastInstruction->getParent());  // 添加一条入边为原尾指令（由尾指令的父级调用产生）
    }
  }
}

// 处理内部调用（针对这些内部调用，尝试生成其对应的符号表达式）
void Symbolizer::handleIntrinsicCall(CallBase &I) {
  auto *callee = I.getCalledFunction();   // 获取调用的函数

  switch (callee->getIntrinsicID()) {     // 判断调用的函数的类型
  case Intrinsic::lifetime_start:
  case Intrinsic::lifetime_end:
  case Intrinsic::dbg_declare:
  case Intrinsic::dbg_value:
  case Intrinsic::is_constant:
  case Intrinsic::trap:
  case Intrinsic::invariant_start:
  case Intrinsic::invariant_end:
  case Intrinsic::assume:
    // These are safe to ignore.
    break;
  case Intrinsic::memcpy: {   // 如果是memcpy（内存拷贝）
    // declare void @llvm.memcpy.p0i8.p0i8.i32(i8* <dest>, i8* <src>, i32 <len>, i1 <isvolatile>)
    // https://llvm.org/docs/LangRef.html#int-memcpy
    IRBuilder<> IRB(&I);                  // 根据该函数创建IRBuilder

    tryAlternative(IRB, I.getOperand(0)); // 对dest进行求解（得到对应当前值的符号表达式）
    tryAlternative(IRB, I.getOperand(1)); // 对src进行求解（得到对应当前值的符号表达式）
    tryAlternative(IRB, I.getOperand(2)); // 对len进行求解（得到对应当前值的符号表达式）

    // The intrinsic allows both 32 and 64-bit integers to specify the length;
    // convert to the right type if necessary. This may truncate the value on
    // 32-bit architectures. However, what's the point of specifying a length to
    // memcpy that is larger than your address space?

    IRB.CreateCall(runtime.memcpy,
                   {I.getOperand(0), I.getOperand(1),
                    IRB.CreateZExtOrTrunc(I.getOperand(2), intPtrType)}); // 生成替代调用
    break;
  }
  case Intrinsic::memset: {   // 如果是memset
    // declare void @llvm.memset.p0i8.i32(i8* <dest>, i8 <val>, i32 <len>, i1 <isvolatile>)
    // https://llvm.org/docs/LangRef.html#llvm-memset-intrinsics
    IRBuilder<> IRB(&I);

    tryAlternative(IRB, I.getOperand(0)); // 对dest进行求解（得到对应当前值的符号表达式）
    tryAlternative(IRB, I.getOperand(2)); // 对len进行求解（得到对应当前值的符号表达式）

    // The comment on memcpy's length parameter applies analogously.

    IRB.CreateCall(runtime.memset,
                   {I.getOperand(0),
                    getSymbolicExpressionOrNull(I.getOperand(1)),
                    IRB.CreateZExtOrTrunc(I.getOperand(2), intPtrType)}); // 生成替代调用（val因为是具体值，直接获取其符号表达式）
    break;
  }
  case Intrinsic::memmove: {  // 如果是memmove
    // declare void @llvm.memmove.p0i8.p0i8.i32(i8* <dest>, i8* <src>, i32 <len>, i1 <isvolatile>)
    // https://llvm.org/docs/LangRef.html#llvm-memmove-intrinsic
    IRBuilder<> IRB(&I);

    tryAlternative(IRB, I.getOperand(0));
    tryAlternative(IRB, I.getOperand(1));
    tryAlternative(IRB, I.getOperand(2));

    // The comment on memcpy's length parameter applies analogously.

    IRB.CreateCall(runtime.memmove,
                   {I.getOperand(0), I.getOperand(1),
                    IRB.CreateZExtOrTrunc(I.getOperand(2), intPtrType)});
    break;
  }
  case Intrinsic::stacksave: {
    // The intrinsic returns an opaque pointer that should only be passed to
    // the stackrestore intrinsic later. We treat the pointer as a constant.
    break;
  }
  case Intrinsic::stackrestore:
    // Ignored; see comment on stacksave above.
    break;
  case Intrinsic::expect: // 如果是重载函数
    // https://llvm.org/docs/LangRef.html#llvm-expect-intrinsic
    // Just a hint for the optimizer; the value is the first parameter.
    if (auto *expr = getSymbolicExpression(I.getArgOperand(0)))
      symbolicExpressions[&I] = expr;
    break;
  case Intrinsic::fabs: { // 浮点数取绝对值
    // declare float     @llvm.fabs.f32(float  %Val)
    // https://llvm.org/docs/LangRef.html#llvm-fabs-intrinsic
    // Floating-point absolute value; use the runtime to build the
    // corresponding symbolic expression.

    IRBuilder<> IRB(&I);
    auto abs = buildRuntimeCall(IRB, runtime.buildFloatAbs, I.getOperand(0));
    registerSymbolicComputation(abs, &I);
    break;
  }
  case Intrinsic::cttz:
  case Intrinsic::ctpop:
  case Intrinsic::ctlz: {
    // Various bit-count operations. Expressing these symbolically is
    // difficult, so for now we just concretize.

    errs() << "Warning: losing track of symbolic expressions at bit-count "
              "operation "
           << I << "\n";
    break;
  }
  case Intrinsic::returnaddress: {
    // Obtain the return address of the current function or one of its parents
    // on the stack. We just concretize.

    errs() << "Warning: using concrete value for return address\n";
    break;
  }
  case Intrinsic::bswap: {  // 如果为为交换函数
    // https://llvm.org/docs/LangRef.html#llvm-bswap-intrinsics
    // Bswap changes the endian-ness of integer values.

    IRBuilder<> IRB(&I);
    auto swapped = buildRuntimeCall(IRB, runtime.buildBswap, I.getOperand(0));
    registerSymbolicComputation(swapped, &I);
    break;
  }
  default:
    errs() << "Warning: unhandled LLVM intrinsic " << callee->getName()
           << "; the result will be concretized\n";
    break;
  }
}

// 处理内联汇编（针对内联汇编，如果返回void，直接打印跳过信息；如果不是，打印警告信息）
// 符号执行无法处理内联汇编
void Symbolizer::handleInlineAssembly(CallInst &I) {
  if (I.getType()->isVoidTy()) {
    errs() << "Warning: skipping over inline assembly " << I << '\n';
    return;
  }

  errs() << "Warning: losing track of symbolic expressions at inline assembly "
         << I << '\n';
}

// 处理函数调用（将其进行符号化）
void Symbolizer::handleFunctionCall(CallBase &I, Instruction *returnPoint) {
  auto *callee = I.getCalledFunction(); // 获取调用的函数
  if (callee != nullptr && callee->isIntrinsic()) { // 若为内部调用，则跳转处理内部调用
    handleIntrinsicCall(I);
    return;
  }

  IRBuilder<> IRB(returnPoint); // 根据返回点的指令创建IRbuilder
  IRB.CreateCall(runtime.notifyRet, getTargetPreferredInt(&I));   // （在返回点）插入"返回通告"
  IRB.SetInsertPoint(&I);       // 在调用位置设置插桩点
  IRB.CreateCall(runtime.notifyCall, getTargetPreferredInt(&I));  // （在调用点）插入"调用通告"

  if (callee == nullptr)        // 即触发了空调用（可能造成程序问题，因此要求解对应路径约束以到达）
    tryAlternative(IRB, I.getCalledOperand());  // 对调用操作进行求解（得到对应调用操作的符号表达式）

  for (Use &arg : I.args()) // 遍历函数参数
    IRB.CreateCall(runtime.setParameterExpression,
                   {ConstantInt::get(IRB.getInt8Ty(), arg.getOperandNo()),
                    getSymbolicExpressionOrNull(arg)}); // 创建调用，设置参数表达式（getOperandNo是获取当前参数是第几个参数）

  if (!I.user_empty()) {    // 若当前函数的结果后续将会被使用（则需要确保获取到的返回结果一定是符号表达式）
    // The result of the function is used somewhere later on. Since we have no
    // way of knowing whether the function is instrumented (and thus sets a
    // proper return expression), we have to account for the possibility that
    // it's not: in that case, we'll have to treat the result as an opaque
    // concrete value. Therefore, we set the return expression to null here in
    // order to avoid accidentally using whatever is stored there from the
    // previous function call. (If the function is instrumented, it will just
    // override our null with the real expression.)
    IRB.CreateCall(runtime.setReturnExpression,
                   ConstantPointerNull::get(IRB.getInt8PtrTy())); // 设置返回结果为null（即将原本的具体结果不透明化）
    IRB.SetInsertPoint(returnPoint);    // 在返回指令前，进行插桩
    symbolicExpressions[&I] = IRB.CreateCall(runtime.getReturnExpression);  // 插入调取返回表达式（即若成功执行了插桩，则会返回符号表达式）
  }
}

// 对二元运算符进行符号化处理
void Symbolizer::visitBinaryOperator(BinaryOperator &I) {
  // Binary operators propagate into the symbolic expression.

  IRBuilder<> IRB(&I);    // 根据二元运算符创建IRBuilder
  SymFnT handler = runtime.binaryOperatorHandlers.at(I.getOpcode());  // 获取二元运算符所对应的符号表达式

  // Special case: the run-time library distinguishes between "and" and "or"
  // on Boolean values and bit vectors.
  // 对特殊情况进行处理（逻辑操作）
  if (I.getOperand(0)->getType() == IRB.getInt1Ty()) {  // 若第一个操作数为1位
    switch (I.getOpcode()) {  // 根据不同情况进行处理
    case Instruction::And:
      handler = runtime.buildBoolAnd;
      break;
    case Instruction::Or:
      handler = runtime.buildBoolOr;
      break;
    case Instruction::Xor:
      handler = runtime.buildBoolXor;
      break;
    default:
      errs() << "Can't handle Boolean operator " << I << '\n';
      llvm_unreachable("Unknown Boolean operator");
      break;
    }
  }

  assert(handler && "Unable to handle binary operator");  // 若转为符号表达式失败，则报错
  auto runtimeCall =
      buildRuntimeCall(IRB, handler, {I.getOperand(0), I.getOperand(1)}); // 否则，根据获取到的符号表达式，进行重写，将二进制操作转换为符号表达式
  registerSymbolicComputation(runtimeCall, &I); // 并将其进行注册（即加入要符号计算的队列）
}

// 处理选择指令操作（类似于 ?: ）
void Symbolizer::visitSelectInst(SelectInst &I) {
  // Select is like the ternary operator ("?:") in C. We push the (potentially
  // negated) condition to the path constraints and copy the symbolic
  // expression over from the chosen argument.

  IRBuilder<> IRB(&I);  // 根据指令创建IRBuilder
  auto runtimeCall = buildRuntimeCall(IRB, runtime.pushPathConstraint,
                                      {{I.getCondition(), true},
                                       {I.getCondition(), false},
                                       {getTargetPreferredInt(&I), false}});  // 创建压入路径约束的函数调用（约束就是跳转条件）
                                       // 注意：这里压入的约束是getCondition，true仅表示它是符号表达式（具体可看buildRuntimeCall函数实现）
  registerSymbolicComputation(runtimeCall); // 对路径约束注册符号计算，用于求解
}

// 处理整数的比较
void Symbolizer::visitCmpInst(CmpInst &I) {
  // ICmp is integer comparison, FCmp compares floating-point values; we
  // simply include either in the resulting expression.

  IRBuilder<> IRB(&I);  // 根据指令创建IRBuilder
  SymFnT handler = runtime.comparisonHandlers.at(I.getPredicate()); // 获取比较运算符所对应的符号表达式
  assert(handler && "Unable to handle icmp/fcmp variant");
  auto runtimeCall =
      buildRuntimeCall(IRB, handler, {I.getOperand(0), I.getOperand(1)}); // 根据符号表达式和两个操作数创建符号执行的调用
  registerSymbolicComputation(runtimeCall, &I); // 将其注册到符号计算中，以求解
}

// 处理return指令
void Symbolizer::visitReturnInst(ReturnInst &I) {
  // Upon return, we just store the expression for the return value.

  if (I.getReturnValue() == nullptr)  // 若为void（即单纯return），则直接跳过
    return;

  // We can't short-circuit this call because the return expression needs to
  // be set even if it's null; otherwise we break the caller. Therefore,
  // create the call directly without registering it for short-circuit
  // processing.（此处解释为什么之前都是register，而这个是直接CreateCall）
  IRBuilder<> IRB(&I);
  IRB.CreateCall(runtime.setReturnExpression,
                 getSymbolicExpressionOrNull(I.getReturnValue()));  // 创建调用，设置返回的表达式为 函数返回的值所对应的符号表达式
}

// 处理分支指令（即br跳转指令），处理方法和之前的处理选择指令（条件运算符）操作基本一致
void Symbolizer::visitBranchInst(BranchInst &I) {
  // Br can jump conditionally or unconditionally. We are only interested in
  // the former case, in which we push the branch condition or its negation to
  // the path constraints.

  if (I.isUnconditional())  // 若为无条件跳转，则直接return（无需做符号化处理）
    return;

  IRBuilder<> IRB(&I);
  auto runtimeCall = buildRuntimeCall(IRB, runtime.pushPathConstraint,
                                      {{I.getCondition(), true},
                                       {I.getCondition(), false},
                                       {getTargetPreferredInt(&I), false}});  // 创建压入路径约束的函数调用（约束就是跳转条件）
  registerSymbolicComputation(runtimeCall); // 对路径约束注册符号计算，用于求解
}

// 处理可变跳转指令（indirectbr），这类跳转跳转的目的label并非确定的（往往是一个集中的一个label），是根据计算进行跳转
void Symbolizer::visitIndirectBrInst(IndirectBrInst &I) {
  IRBuilder<> IRB(&I);
  tryAlternative(IRB, I.getAddress());
}

// 处理函数调用指令（即call指令）
void Symbolizer::visitCallInst(CallInst &I) {
  if (I.isInlineAsm())  // 若调用的是内联汇编
    handleInlineAssembly(I);  // 则处理内联汇编
  else
    handleFunctionCall(I, I.getNextNode()); // 其他的，则按正常函数调用进行处理
}

// 处理函数调用指令（invoke指令）
// 这个相较于call多了一个异常处理程序，若调用失败会进入异常处理
void Symbolizer::visitInvokeInst(InvokeInst &I) {
  // Invoke is like a call but additionally establishes an exception handler. We
  // can obtain the return expression only in the success case, but the target
  // block may have multiple incoming edges (i.e., our edge may be critical). In
  // this case, we split the edge and query the return expression in the new
  // block that is specific to our edge.
  auto *newBlock = SplitCriticalEdge(I.getParent(), I.getNormalDest()); // 在函数调用和成功返回之间进行分割（如果当前路径是割边）
  handleFunctionCall(I, newBlock != nullptr
                            ? newBlock->getFirstNonPHI()
                            : I.getNormalDest()->getFirstNonPHI());     // 若是割边（即成功分割）则从新中获取返回的表达式
                            // 若分割失败，则从目标块中获得返回表达式
}

// 处理分配内存指令（alloca指令）
void Symbolizer::visitAllocaInst(AllocaInst & /*unused*/) {
  // Nothing to do: the shadow for the newly allocated memory region will be
  // created on first write; until then, the memory contents are concrete.
}

// 处理读取内存指令（load指令）
void Symbolizer::visitLoadInst(LoadInst &I) {
  IRBuilder<> IRB(&I);

  auto *addr = I.getPointerOperand(); // 获取调用的地址
  tryAlternative(IRB, addr);          // 对地址进行求解（得到对应当前值的符号表达式）

  auto *dataType = I.getType();       // 获得要读取的数据类型
  // 创建调用获取数据（针对整数）
  auto *data = IRB.CreateCall(
      runtime.readMemory,
      {IRB.CreatePtrToInt(addr, intPtrType),
       ConstantInt::get(intPtrType, dataLayout.getTypeStoreSize(dataType)),
       ConstantInt::get(IRB.getInt8Ty(), isLittleEndian(dataType) ? 1 : 0)});

  if (dataType->isFloatingPointTy()) {// 若为指向浮点数的指针类型
    data = IRB.CreateCall(runtime.buildBitsToFloat,
                          {data, IRB.getInt1(dataType->isDoubleTy())});
  }

  symbolicExpressions[&I] = data;     // 符号表达式为获取到的数据
}

// 处理写入内存指令（store指令）
void Symbolizer::visitStoreInst(StoreInst &I) {
  IRBuilder<> IRB(&I);

  tryAlternative(IRB, I.getPointerOperand()); // 对地址进行求解（得到对应当前值的符号表达式）

  auto *data = getSymbolicExpressionOrNull(I.getValueOperand());  // 获取数据的符号表达式
  auto *dataType = I.getValueOperand()->getType();                // 获取数据的类型
  if (dataType->isFloatingPointTy()) {  // 若为浮点数
    data = IRB.CreateCall(runtime.buildFloatToBits, data);
  }

  // 若不是浮点数
  IRB.CreateCall(
      runtime.writeMemory,
      {IRB.CreatePtrToInt(I.getPointerOperand(), intPtrType),
       ConstantInt::get(intPtrType, dataLayout.getTypeStoreSize(dataType)),
       data,
       ConstantInt::get(IRB.getInt8Ty(), dataLayout.isLittleEndian() ? 1 : 0)});
}

// 处理获取对象地址的指令（getelementptr指令）
void Symbolizer::visitGetElementPtrInst(GetElementPtrInst &I) {
  // GEP performs address calculations but never actually accesses memory. In
  // order to represent the result of a GEP symbolically, we start from the
  // symbolic expression of the original pointer and duplicate its
  // computations at the symbolic level.

  // If everything is compile-time concrete, we don't need to emit code.
  if (getSymbolicExpression(I.getPointerOperand()) == nullptr &&
      std::all_of(I.idx_begin(), I.idx_end(), [this](Value *index) {
        return (getSymbolicExpression(index) == nullptr);
      })) { // 若指针获取操作的对象不存在参数且自身编译时即可确定是具体值
    return; // 则直接返回
  }

  // If there are no indices or if they are all zero we can return early as
  // well.
  if (std::all_of(I.idx_begin(), I.idx_end(), [](Value *index) {
        auto *ci = dyn_cast<ConstantInt>(index);
        return (ci != nullptr && ci->isZero());
      })) {
    symbolicExpressions[&I] = getSymbolicExpression(I.getPointerOperand());
    return;
  }

  IRBuilder<> IRB(&I);
  SymbolicComputation symbolicComputation;
  Value *currentAddress = I.getPointerOperand();

  for (auto type_it = gep_type_begin(I), type_end = gep_type_end(I);
       type_it != type_end; ++type_it) {
    auto *index = type_it.getOperand();
    std::pair<Value *, bool> addressContribution;

    // There are two cases for the calculation:
    // 1. If the indexed type is a struct, we need to add the offset of the
    //    desired member.
    // 2. If it is an array or a pointer, compute the offset of the desired
    //    element.
    if (auto *structType = type_it.getStructTypeOrNull()) {
      // Structs can only be indexed with constants
      // (https://llvm.org/docs/LangRef.html#getelementptr-instruction).

      unsigned memberIndex = cast<ConstantInt>(index)->getZExtValue();
      unsigned memberOffset =
          dataLayout.getStructLayout(structType)->getElementOffset(memberIndex);
      addressContribution = {ConstantInt::get(intPtrType, memberOffset), true};
    } else {
      if (auto *ci = dyn_cast<ConstantInt>(index);
          ci != nullptr && ci->isZero()) {
        // Fast path: an index of zero means that no calculations are
        // performed.
        continue;
      }

      // TODO optimize? If the index is constant, we can perform the
      // multiplication ourselves instead of having the solver do it. Also, if
      // the element size is 1, we can omit the multiplication.

      unsigned elementSize =
          dataLayout.getTypeAllocSize(type_it.getIndexedType());
      if (auto indexWidth = index->getType()->getIntegerBitWidth();
          indexWidth != ptrBits) {
        symbolicComputation.merge(forceBuildRuntimeCall(
            IRB, runtime.buildZExt,
            {{index, true},
             {ConstantInt::get(IRB.getInt8Ty(), ptrBits - indexWidth),
              false}}));
        symbolicComputation.merge(forceBuildRuntimeCall(
            IRB, runtime.binaryOperatorHandlers[Instruction::Mul],
            {{symbolicComputation.lastInstruction, false},
             {ConstantInt::get(intPtrType, elementSize), true}}));
      } else {
        symbolicComputation.merge(forceBuildRuntimeCall(
            IRB, runtime.binaryOperatorHandlers[Instruction::Mul],
            {{index, true},
             {ConstantInt::get(intPtrType, elementSize), true}}));
      }

      addressContribution = {symbolicComputation.lastInstruction, false};
    }

    symbolicComputation.merge(forceBuildRuntimeCall(
        IRB, runtime.binaryOperatorHandlers[Instruction::Add],
        {addressContribution,
         {currentAddress, (currentAddress == I.getPointerOperand())}}));
    currentAddress = symbolicComputation.lastInstruction;
  }

  registerSymbolicComputation(symbolicComputation, &I);
}

// 处理类型转换指令（每一位的值不做任何变化）
void Symbolizer::visitBitCastInst(BitCastInst &I) {
  if (I.getSrcTy()->isIntegerTy() && I.getDestTy()->isFloatingPointTy()) {  // 若为整数转为浮点数
    IRBuilder<> IRB(&I);
    auto conversion =
        buildRuntimeCall(IRB, runtime.buildBitsToFloat,
                         {{I.getOperand(0), true},
                          {IRB.getInt1(I.getDestTy()->isDoubleTy()), false}});
    registerSymbolicComputation(conversion, &I);
    return;
  }

  if (I.getSrcTy()->isFloatingPointTy() && I.getDestTy()->isIntegerTy()) {  // 若为浮点数转整数
    IRBuilder<> IRB(&I);
    auto conversion = buildRuntimeCall(IRB, runtime.buildFloatToBits,
                                       {{I.getOperand(0), true}});
    registerSymbolicComputation(conversion);
    return;
  }

  assert(I.getSrcTy()->isPointerTy() && I.getDestTy()->isPointerTy() &&
         "Unhandled non-pointer bit cast");                                 // 若为指针类型（无法处理）
  if (auto *expr = getSymbolicExpression(I.getOperand(0)))  // 获取符号表达式
    symbolicExpressions[&I] = expr;                         // 若可得则存储
}

// 处理截断指令（必须均为整数）
void Symbolizer::visitTruncInst(TruncInst &I) {
  IRBuilder<> IRB(&I);
  auto trunc = buildRuntimeCall(
      IRB, runtime.buildTrunc,
      {{I.getOperand(0), true},
       {IRB.getInt8(I.getDestTy()->getIntegerBitWidth()), false}});
  registerSymbolicComputation(trunc, &I);
}

// int转指针
void Symbolizer::visitIntToPtrInst(IntToPtrInst &I) {
  if (auto *expr = getSymbolicExpression(I.getOperand(0)))
    symbolicExpressions[&I] = expr;
  // TODO handle truncation and zero extension
}

// 指针转int
void Symbolizer::visitPtrToIntInst(PtrToIntInst &I) {
  if (auto *expr = getSymbolicExpression(I.getOperand(0)))
    symbolicExpressions[&I] = expr;
  // TODO handle truncation and zero extension
}

// 有符号整数转浮点数
void Symbolizer::visitSIToFPInst(SIToFPInst &I) {
  IRBuilder<> IRB(&I);
  auto conversion =
      buildRuntimeCall(IRB, runtime.buildIntToFloat,
                       {{I.getOperand(0), true},
                        {IRB.getInt1(I.getDestTy()->isDoubleTy()), false},
                        {/* is_signed */ IRB.getInt1(true), false}});
  registerSymbolicComputation(conversion, &I);
}

// 无符号整数转浮点数
void Symbolizer::visitUIToFPInst(UIToFPInst &I) {
  IRBuilder<> IRB(&I);
  auto conversion =
      buildRuntimeCall(IRB, runtime.buildIntToFloat,
                       {{I.getOperand(0), true},
                        {IRB.getInt1(I.getDestTy()->isDoubleTy()), false},
                        {/* is_signed */ IRB.getInt1(false), false}});
  registerSymbolicComputation(conversion, &I);
}

// 浮点数拓展
void Symbolizer::visitFPExtInst(FPExtInst &I) {
  IRBuilder<> IRB(&I);
  auto conversion =
      buildRuntimeCall(IRB, runtime.buildFloatToFloat,
                       {{I.getOperand(0), true},
                        {IRB.getInt1(I.getDestTy()->isDoubleTy()), false}});
  registerSymbolicComputation(conversion, &I);
}

// 浮点数截断
void Symbolizer::visitFPTruncInst(FPTruncInst &I) {
  IRBuilder<> IRB(&I);
  auto conversion =
      buildRuntimeCall(IRB, runtime.buildFloatToFloat,
                       {{I.getOperand(0), true},
                        {IRB.getInt1(I.getDestTy()->isDoubleTy()), false}});
  registerSymbolicComputation(conversion, &I);
}

// 浮点数转有符号整数
void Symbolizer::visitFPToSI(FPToSIInst &I) {
  IRBuilder<> IRB(&I);
  auto conversion = buildRuntimeCall(
      IRB, runtime.buildFloatToSignedInt,
      {{I.getOperand(0), true},
       {IRB.getInt8(I.getType()->getIntegerBitWidth()), false}});
  registerSymbolicComputation(conversion, &I);
}

// 浮点数转无符号整数
void Symbolizer::visitFPToUI(FPToUIInst &I) {
  IRBuilder<> IRB(&I);
  auto conversion = buildRuntimeCall(
      IRB, runtime.buildFloatToUnsignedInt,
      {{I.getOperand(0), true},
       {IRB.getInt8(I.getType()->getIntegerBitWidth()), false}});
  registerSymbolicComputation(conversion, &I);
}

// 处理拓展指令（分为ZExt和SExt）
void Symbolizer::visitCastInst(CastInst &I) {
  auto opcode = I.getOpcode();  // 获取指令的操作码
  if (opcode != Instruction::SExt && opcode != Instruction::ZExt) { // 若不是其中之一，则无法处理
    errs() << "Warning: unhandled cast instruction " << I << '\n';
    return;
  }

  IRBuilder<> IRB(&I);

  // LLVM bitcode represents Boolean values as i1. In Z3, those are a not a
  // bit-vector sort, so trying to cast one into a bit vector of any length
  // raises an error. The run-time library provides a dedicated conversion
  // function for this case.
  if (I.getSrcTy()->getIntegerBitWidth() == 1) {  // 若源操作数为1位
    auto boolToBitConversion = buildRuntimeCall(
        IRB, runtime.buildBoolToBits,
        {{I.getOperand(0), true},
         {IRB.getInt8(I.getDestTy()->getIntegerBitWidth()), false}}); // 则创建bool到bits的转换
    registerSymbolicComputation(boolToBitConversion, &I);
  } else {
    SymFnT target;

    switch (I.getOpcode()) {  // 根据拓展的种类（补零扩展还是补符号位拓展）创建target符号函数
    case Instruction::SExt:
      target = runtime.buildSExt;
      break;
    case Instruction::ZExt:
      target = runtime.buildZExt;
      break;
    default:
      llvm_unreachable("Unknown cast opcode");
    }

    auto symbolicCast =
        buildRuntimeCall(IRB, target,
                         {{I.getOperand(0), true},
                          {IRB.getInt8(I.getDestTy()->getIntegerBitWidth() -
                                       I.getSrcTy()->getIntegerBitWidth()),
                           false}});  // 根据target创建调用
    registerSymbolicComputation(symbolicCast, &I);
  }
}

// 处理phi结点
void Symbolizer::visitPHINode(PHINode &I) {
  // PHI nodes just assign values based on the origin of the last jump, so we
  // assign the corresponding symbolic expression the same way.

  // 先将当前结点压入phi集（最后会处理）
  phiNodes.push_back(&I); // to be finalized later, see finalizePHINodes

  IRBuilder<> IRB(&I);
  unsigned numIncomingValues = I.getNumIncomingValues();  // 获取入边数量
  auto *exprPHI = IRB.CreatePHI(IRB.getInt8PtrTy(), numIncomingValues); // 根据入边数量，创建phi结点
  for (unsigned incoming = 0; incoming < numIncomingValues; incoming++) { // 遍历各入边
    exprPHI->addIncoming(
        // The null pointer will be replaced in finalizePHINodes.
        ConstantPointerNull::get(cast<PointerType>(IRB.getInt8PtrTy())),
        I.getIncomingBlock(incoming));  // 加入各入边，以<传入值,对应基本块>的形式，但传入值初始为空
  }

  symbolicExpressions[&I] = exprPHI;    // 将此拟的结果加入映射关系表
}

// 处理插入指令（该指令可针对地对结构体中的某一个值进行赋值）
// <result> = insertvalue <aggregate type> <val>, <ty> <elt>, <idx>{, <idx>}*    ; yields <aggregate type>
// <aggregate type>定义了域(结构体)的形式，然后<val>表示该结构体的值（用于表明是哪一个结构体）
// <ty>表示待插入值类型，<elt>表示待插入值的值，<idx>表示其位置（整数，从0开始）
// 例如：%agg2 = insertvalue {i32, float} %agg1, float %val, 1
// 表示，对一个形如{i32,float}的结构体%agg1，用%val对其中的第'1'个元素进行赋值(也就是float=%val)
// (注意，元素是从0开始，也就是第'0'对应i32，第'1'对应float。且要插入的值的类型一定要和待插入位置的类型一致）
void Symbolizer::visitInsertValueInst(InsertValueInst &I) {
  IRBuilder<> IRB(&I);
  auto insert = buildRuntimeCall(
      IRB, runtime.buildInsert,
      {{I.getAggregateOperand(), true}, // 获取域(结构体)
       {I.getInsertedValueOperand(), true}, // 要插入的值
       {IRB.getInt64(aggregateMemberOffset(I.getAggregateOperand()->getType(),
                                           I.getIndices())),  // 获取偏移量
        false},
       {IRB.getInt8(isLittleEndian(I.getInsertedValueOperand()->getType()) ? 1 : 0), false}});
  registerSymbolicComputation(insert, &I);
}

// 和insertvalue对应，此处为取域中的某个值的指令
// <result> = extractvalue <aggregate type> <val>, <idx>{, <idx>}*
void Symbolizer::visitExtractValueInst(ExtractValueInst &I) {
  IRBuilder<> IRB(&I);
  auto extract = buildRuntimeCall(
      IRB, runtime.buildExtract,
      {{I.getAggregateOperand(), true}, // 获取域
       {IRB.getInt64(aggregateMemberOffset(I.getAggregateOperand()->getType(),
                                           I.getIndices())),
        false},                         // 计算偏移量
       {IRB.getInt64(dataLayout.getTypeStoreSize(I.getType())), false}, // 计算边界
       {IRB.getInt8(isLittleEndian(I.getType()) ? 1 : 0), false}});
  registerSymbolicComputation(extract, &I);
}

// 处理switch指令
void Symbolizer::visitSwitchInst(SwitchInst &I) {
  // Switch compares a value against a set of integer constants; duplicate
  // constants are not allowed
  // (https://llvm.org/docs/LangRef.html#switch-instruction).

  IRBuilder<> IRB(&I);
  auto *condition = I.getCondition(); // 获取条件
  auto *conditionExpr = getSymbolicExpression(condition); // 获取条件的符号表达式
  if (conditionExpr == nullptr)       // 若为空，则表示为编译时就确定的具体值，则无需后续处理
    return;

  // Build a check whether we have a symbolic condition, to be used later.
  auto *haveSymbolicCondition = IRB.CreateICmpNE(
      conditionExpr, ConstantPointerNull::get(IRB.getInt8PtrTy())); // 判断当前是否获得了符号表达式（即当前是否为具体值）
  auto *constraintBlock = SplitBlockAndInsertIfThen(haveSymbolicCondition, &I,
                                                    /* unreachable */ false); // 若获得了，则在switch之前进行分割并插入if-then
                                                    // 当前插入的结果为：if(condition已有符号表达式)then(执行constraintBlock)

  // In the constraint block, we push one path constraint per case.
  IRB.SetInsertPoint(constraintBlock);  // 设置插桩点为constraintBlock的开头
  for (auto &caseHandle : I.cases()) {  // 遍历所有的case
    auto *caseTaken = IRB.CreateICmpEQ(condition, caseHandle.getCaseValue()); // 创建判断，判断是否符合当前case
    auto *caseConstraint = IRB.CreateCall(
        runtime.comparisonHandlers[CmpInst::ICMP_EQ],
        {conditionExpr, createValueExpression(caseHandle.getCaseValue(), IRB)});  // 创建调用，生成符号表达式=当前case值的断言
    IRB.CreateCall(runtime.pushPathConstraint,
                   {caseConstraint, caseTaken, getTargetPreferredInt(&I)});       // 进行约束求解
  }
}

void Symbolizer::visitUnreachableInst(UnreachableInst & /*unused*/) {
  // Nothing to do here...
}

// 处理未知指令
void Symbolizer::visitInstruction(Instruction &I) {
  // Some instructions are only used in the context of exception handling, which
  // we ignore for now.
  if (isa<LandingPadInst>(I) || isa<ResumeInst>(I)) // 若为异常处理中的指令，则忽略
    return;

  errs() << "Warning: unknown instruction " << I
         << "; the result will be concretized\n";
}

// 创建一个表达式代表具体值
CallInst *Symbolizer::createValueExpression(Value *V, IRBuilder<> &IRB) {
  auto *valueType = V->getType();

  if (isa<ConstantPointerNull>(V)) {  // 如果具体值为空
    return IRB.CreateCall(runtime.buildNullPointer, {});  // 则返回创建一个空指针
  }

  if (valueType->isIntegerTy()) {     // 如果具体值类型为整数
    auto bits = valueType->getPrimitiveSizeInBits();  // 返回值的原始位数（即类型的size）
    if (bits == 1) {                  // 若位数为1
      // Special case: LLVM uses the type i1 to represent Boolean values, but
      // for Z3 we have to create expressions of a separate sort.
      return IRB.CreateCall(runtime.buildBool, {V});  // 返回创建一个bool类型
    } else if (bits <= 64) {          // 小于等于64位
      return IRB.CreateCall(runtime.buildInteger,
                            {IRB.CreateZExtOrBitCast(V, IRB.getInt64Ty()),
                             IRB.getInt8(valueType->getPrimitiveSizeInBits())});  // 返回创建一个int类型（注意截断）
    } else {
      // Anything up to the maximum supported 128 bits. Those integers are a bit
      // tricky because the symbolic backends don't support them per se. We have
      // a special function in the run-time library that handles them, usually
      // by assembling expressions from smaller chunks.
      return IRB.CreateCall(          // 大于64位的情况
          runtime.buildInteger128,
          {IRB.CreateTrunc(IRB.CreateLShr(V, ConstantInt::get(valueType, 64)),
                           IRB.getInt64Ty()),
           IRB.CreateTrunc(V, IRB.getInt64Ty())});  // 创建截断，实际上是将128位分为两个64位进行存储
    }
  }

  if (valueType->isFloatingPointTy()) { // 若为浮点数
    return IRB.CreateCall(runtime.buildFloat,
                          {IRB.CreateFPCast(V, IRB.getDoubleTy()),
                           IRB.getInt1(valueType->isDoubleTy())});  // 返回创建浮点数
  }

  if (valueType->isPointerTy()) {       // 若为指针类型
    return IRB.CreateCall(
        runtime.buildInteger,
        {IRB.CreatePtrToInt(V, IRB.getInt64Ty()), IRB.getInt8(ptrBits)}); // 返回创建指针
  }

  if (valueType->isStructTy()) {        // 若为结构体类型
    // In unoptimized code we may see structures in SSA registers. What we
    // want is a single bit-vector expression describing their contents, but
    // unfortunately we can't take the address of a register. We fix the
    // problem with a hack: we write the register to memory and initialize the
    // expression from there.
    //
    // An alternative would be to change the representation of structures in
    // SSA registers to "shadow structures" that contain one expression per
    // member. However, this would put an additional burden on the handling of
    // cast instructions, because expressions would have to be converted
    // between different representations according to the type.

    auto *memory = IRB.CreateAlloca(V->getType());  // 创建对应的内存空间（用于存储v）
    IRB.CreateStore(V, memory);           // 将v存入
    return IRB.CreateCall(
        runtime.readMemory,
        {IRB.CreatePtrToInt(memory, intPtrType),
         ConstantInt::get(intPtrType,
                          dataLayout.getTypeStoreSize(V->getType())),
         IRB.getInt8(0)});                // 创建一个对内存的读取
  }

  llvm_unreachable("Unhandled type for constant expression");
}

// 强制进行buildRuntimeCall，在run-time库中创建一个对指定函数的调用
Symbolizer::SymbolicComputation
Symbolizer::forceBuildRuntimeCall(IRBuilder<> &IRB, SymFnT function,
                                  ArrayRef<std::pair<Value *, bool>> args) {
  std::vector<Value *> functionArgs;          // 获取符号化参数输入
  for (const auto &[arg, symbolic] : args) {  // 遍历函数的参数，根据bool标志符symbolic判断是否为符号化
    functionArgs.push_back(symbolic ? getSymbolicExpressionOrNull(arg) : arg);  // 假如是符号化的，获取符号表达式；反之，获取具体值
  }
  auto *call = IRB.CreateCall(function, functionArgs);  // 根据函数和其对应的符号化参数创建IRBuilder调用

  std::vector<Input> inputs;  // 创建符号化的参数集合，用于后续符号计算（符号计算仅需计算符号表达式）
  for (unsigned i = 0; i < args.size(); i++) {  // 遍历参数
    const auto &[arg, symbolic] = args[i];
    if (symbolic) // 若为符号化的，将此参数加入
      inputs.push_back({arg, i, call});
  }

  return SymbolicComputation(call, call, inputs); // 根据符号化后的函数调用，创建符号计算，并进行返回
}

// 生成代码，使解算器根据V的值对其符号表达式(未求解)，生成断言约束（值=符号表达式），求解得到符合该值的符号表达式(求解完成)，进行对应存储
void Symbolizer::tryAlternative(IRBuilder<> &IRB, Value *V) {
  auto *destExpr = getSymbolicExpression(V);    // 获取值V的符号表达式
  if (destExpr != nullptr) {    // 若不为空
    auto *concreteDestExpr = createValueExpression(V, IRB); // 获取值表达式
    auto *destAssertion =
        IRB.CreateCall(runtime.comparisonHandlers[CmpInst::ICMP_EQ],
                       {destExpr, concreteDestExpr});       // 创建调用，创建destExpr==concreteDestExpr的映射表达式
    auto *pushAssertion = IRB.CreateCall(
        runtime.pushPathConstraint,
        {destAssertion, IRB.getInt1(true), getTargetPreferredInt(V)});  // 创建调用，压入路径约束（断言，符号表达式=值表达式）
    registerSymbolicComputation(SymbolicComputation(
        concreteDestExpr, pushAssertion, {{V, 0, destAssertion}}));     // 对约束进行符号计算，将计算结果存储为和具体值相对应的符号表达式
  }
}

// 计算域(结构体)中的成员的偏移量（这个结构体可能是嵌套的）
uint64_t Symbolizer::aggregateMemberOffset(Type *aggregateType,
                                           ArrayRef<unsigned> indices) const {
  uint64_t offset = 0;
  auto *indexedType = aggregateType;  // 获取偏移类型为域的类型
  for (auto index : indices) {        // 根据域内索引遍历域内元素（注意，index表示当前索引下元素的个数，因为元素可以是数组）
    // All indices in an extractvalue instruction are constant:
    // https://llvm.org/docs/LangRef.html#extractvalue-instruction

    if (auto *structType = dyn_cast<StructType>(indexedType)) { // 若当前域内元素为结构体(域)，即当前为嵌套
      offset += dataLayout.getStructLayout(structType)->getElementOffset(index);  // 偏移量加上该嵌套域的偏移量
      indexedType = structType->getElementType(index);  // 索引类型为嵌套域内元素的类型（即递归处理嵌套域）
    } else {  // 若不是域（即当前元素为单独元素，非嵌套域）
      auto *arrayType = cast<ArrayType>(indexedType); // 根据当前索引类型获得对应的数组类型
      unsigned elementSize =
          dataLayout.getTypeAllocSize(arrayType->getArrayElementType());  // 获得该数组类型对应的size
      offset += elementSize * index;                  // 偏移量为该size乘以索引
      indexedType = arrayType->getArrayElementType(); // 更新索引类型
    }
  }

  return offset;  // 返回偏移量
}
