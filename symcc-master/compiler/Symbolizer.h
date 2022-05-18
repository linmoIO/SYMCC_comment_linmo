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

#ifndef SYMBOLIZE_H
#define SYMBOLIZE_H

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/InstVisitor.h>
#include <llvm/IR/ValueMap.h>
#include <llvm/Support/raw_ostream.h>
#include <optional>

#include "Runtime.h"

class Symbolizer : public llvm::InstVisitor<Symbolizer> {
public:
  explicit Symbolizer(llvm::Module &M)
      : runtime(M), dataLayout(M.getDataLayout()),
        ptrBits(M.getDataLayout().getPointerSizeInBits()),
        intPtrType(M.getDataLayout().getIntPtrType(M.getContext())) {}

  /// Insert code to obtain the symbolic expressions for the function arguments.
  // 获取函数参数的符号表达式
  void symbolizeFunctionArguments(llvm::Function &F);

  /// Insert a call to the run-time library to notify it of the basic block
  /// entry.
  // 向运行库中插入基本块入口的相关通知
  void insertBasicBlockNotification(llvm::BasicBlock &B);

  /// Finish the processing of PHI nodes.
  ///
  /// This assumes that there is a dummy PHI node for each such instruction in
  /// the function, and that we have recorded all PHI nodes in the member
  /// phiNodes. In other words, the function has to be called after all
  /// instructions have been processed in order to fix up PHI nodes. See the
  /// documentation of member phiNodes for why we process PHI nodes in two
  /// steps.
  ///
  /// Important! Calling this function invalidates symbolicExpressions.
  // 完成PHI节点的处理。（调用此函数将使符号表达式无效）
  void finalizePHINodes();

  /// Rewrite symbolic computation to only occur if some operand is symbolic.
  ///
  /// We don't want to build up formulas for symbolic computation if all
  /// operands are concrete. Therefore, this function rewrites all places that
  /// build up formulas (as recorded during the main pass) to skip formula
  /// construction if all operands are concrete. Additionally, it inserts code
  /// that constructs formulas for concrete operands if necessary.
  ///
  /// The basic idea is to transform code like this...
  ///
  ///   res_expr = call _sym_some_computation(expr1, expr2, ...)
  ///   res      = some_computation(val1, val2, ...)
  ///
  /// ...into this:
  ///
  ///   start:
  ///   expr1_symbolic = icmp ne 0, expr1
  ///   ...
  ///   some_symbolic = or expr1_symbolic, ...
  ///   br some_symbolic, check_arg1, end
  ///
  ///   check_arg1:
  ///   need_expr1 = icmp eq 0, expr1
  ///   br need_expr1, create_expr1, check_arg2
  ///
  ///   create_expr1:
  ///   new_expr1 = ... (based on val1)
  ///   br check_arg2
  ///
  ///   check_arg2:
  ///   good_expr1 = phi [expr1, check_arg1], [new_expr1, create_expr1]
  ///   need_expr2 = ...
  ///   ...
  ///
  ///   sym_computation:
  ///   sym_expr = call _sym_some_computation(good_expr1, good_expr2, ...)
  ///   br end
  ///
  ///   end:
  ///   final_expr = phi [null, start], [sym_expr, sym_computation]
  ///
  /// The resulting code is much longer but avoids solver calls for all
  /// operations without symbolic data.
  // 重写符号计算，使其仅在存在操作数为符号的时候才发生；即对于纯具体的计算，不调用符号计算，跳过公式构造
  void shortCircuitExpressionUses();

  // 下面的所有指令都是IR指令，可以通过查询 https://llvm.org/docs/LangRef.html 进行查看

  void handleIntrinsicCall(llvm::CallBase &I);    // 处理内部调用
  void handleInlineAssembly(llvm::CallInst &I);   // 处理内联汇编
  void handleFunctionCall(llvm::CallBase &I, llvm::Instruction *returnPoint); // 处理函数调用

  ///
  /// Implementation of InstVisitor
  void visitBinaryOperator(llvm::BinaryOperator &I);  // 处理二元运算符操作
  void visitSelectInst(llvm::SelectInst &I);          // 处理选择指令操作（类似于 ?: ）
  void visitCmpInst(llvm::CmpInst &I);                // 处理整数的比较
  void visitReturnInst(llvm::ReturnInst &I);          // 处理return指令
  void visitBranchInst(llvm::BranchInst &I);          // 处理分支指令（即br跳转指令）
  void visitIndirectBrInst(llvm::IndirectBrInst &I);  // 处理可变跳转指令（indirectbr），这类跳转跳转的目的地往往是一个 集 中的一个label，是根据计算进行跳转
  void visitCallInst(llvm::CallInst &I);              // 处理函数调用指令（即call指令）
  void visitInvokeInst(llvm::InvokeInst &I);          // 处理函数调用指令（invoke指令）
  void visitAllocaInst(llvm::AllocaInst &);           // 处理分配内存指令（alloca指令）
  void visitLoadInst(llvm::LoadInst &I);              // 处理读取内存指令（load指令）
  void visitStoreInst(llvm::StoreInst &I);            // 处理写入内存指令（store指令）
  void visitGetElementPtrInst(llvm::GetElementPtrInst &I);  // 处理获取对象地址的指令（getelementptr指令）
  void visitBitCastInst(llvm::BitCastInst &I);        // 处理类型转换指令（每一位的值不做任何变化）
  void visitTruncInst(llvm::TruncInst &I);            // 处理截断指令（必须均为整数）
  void visitIntToPtrInst(llvm::IntToPtrInst &I);      // Int转指针
  void visitPtrToIntInst(llvm::PtrToIntInst &I);      // 指针转Int
  void visitSIToFPInst(llvm::SIToFPInst &I);          // 有符号整数转浮点数
  void visitUIToFPInst(llvm::UIToFPInst &I);          // 无符号整数转浮点数
  void visitFPExtInst(llvm::FPExtInst &I);            // 浮点数拓展
  void visitFPTruncInst(llvm::FPTruncInst &I);        // 浮点数截断
  void visitFPToSI(llvm::FPToSIInst &I);              // 浮点数转有符号整数
  void visitFPToUI(llvm::FPToUIInst &I);              // 浮点数转无符号整数
  void visitCastInst(llvm::CastInst &I);              // 处理转换指令（分为ZExt和SExt）
  void visitPHINode(llvm::PHINode &I);                // 处理phi结点
  void visitInsertValueInst(llvm::InsertValueInst &I);// 处理插入指令（该指令可针对地对结构体中的某一个值进行赋值）
  void visitExtractValueInst(llvm::ExtractValueInst &I);  // 和insertvalue对应，此处为取域中的某个值的指令
  void visitSwitchInst(llvm::SwitchInst &I);          // 处理switch指令
  void visitUnreachableInst(llvm::UnreachableInst &);
  void visitInstruction(llvm::Instruction &I);        // 处理未知指令

private:
  static constexpr unsigned kExpectedMaxPHINodesPerFunction = 16;
  static constexpr unsigned kExpectedSymbolicArgumentsPerComputation = 2;

  /// A symbolic input.
  // 符号输入
  struct Input {
    llvm::Value *concreteValue; // 具体值
    unsigned operandIndex;      // 操作数索引
    llvm::Instruction *user;    //

    llvm::Value *getSymbolicOperand() const {
      return user->getOperand(operandIndex);
    }

    void replaceOperand(llvm::Value *newOperand) {
      user->setOperand(operandIndex, newOperand);
    }
  };

  /// A symbolic computation with its inputs.
  // 符号计算
  struct SymbolicComputation {
    llvm::Instruction *firstInstruction = nullptr, *lastInstruction = nullptr;
    llvm::SmallVector<Input, kExpectedSymbolicArgumentsPerComputation> inputs;

    SymbolicComputation() = default;

    SymbolicComputation(llvm::Instruction *first, llvm::Instruction *last,
                        llvm::ArrayRef<Input> in)
        : firstInstruction(first), lastInstruction(last),
          inputs(in.begin(), in.end()) {} // 将第一个指令和最后一个指令加入，并加入input

    /// Append another symbolic computation to this one.
    ///
    /// The computation that is to be appended must occur after the one that
    /// this method is called on.
    void merge(const SymbolicComputation &other) {
      if (&other == this)
        return;

      if (firstInstruction == nullptr)
        firstInstruction = other.firstInstruction;
      lastInstruction = other.lastInstruction;

      for (const auto &input : other.inputs)
        inputs.push_back(input);
    }

    // 重载<<运算符，输出函数计算
    friend llvm::raw_ostream &
    operator<<(llvm::raw_ostream &out,
               const Symbolizer::SymbolicComputation &computation) {
      out << "\nComputation starting at " << *computation.firstInstruction
          << "\n...ending at " << *computation.lastInstruction
          << "\n...with inputs:\n";
      for (const auto &input : computation.inputs) {
        out << '\t' << *input.concreteValue << '\n';
      }
      return out;
    }
  };

  /// Create an expression that represents the concrete value.
  // 创建一个表达式代表具体值
  llvm::CallInst *createValueExpression(llvm::Value *V, llvm::IRBuilder<> &IRB);

  /// Get the (already created) symbolic expression for a value.
  // 获取具体值的符号表达式，若未创建则返回空
  llvm::Value *getSymbolicExpression(llvm::Value *V) {
    auto exprIt = symbolicExpressions.find(V);
    return (exprIt != symbolicExpressions.end()) ? exprIt->second : nullptr;
  }

  // 获取具体值的符号表达式，若未创建则返回空
  llvm::Value *getSymbolicExpressionOrNull(llvm::Value *V) {
    auto *expr = getSymbolicExpression(V);
    if (expr == nullptr)
      return llvm::ConstantPointerNull::get(
          llvm::IntegerType::getInt8PtrTy(V->getContext()));
    return expr;
  }

  // 判断是否为小端法
  bool isLittleEndian(llvm::Type *type) {
    return (!type->isAggregateType() && dataLayout.isLittleEndian());
  }

  /// Like buildRuntimeCall, but the call is always generated.
  // 强制进行buildRuntimeCall，在run-time库中创建一个对指定函数的调用
  SymbolicComputation
  forceBuildRuntimeCall(llvm::IRBuilder<> &IRB, SymFnT function,
                        llvm::ArrayRef<std::pair<llvm::Value *, bool>> args);

  /// Create a call to the specified function in the run-time library.
  ///
  /// Each argument is specified as a pair of Value and Boolean. The Boolean
  /// specifies whether the Value is a symbolic argument, in which case the
  /// corresponding symbolic expression will be passed to the run-time function.
  /// Moreover, the use of symbolic expressions will be recorded in the
  /// resulting SymbolicComputation. If all symbolic arguments are known to be
  /// concrete (e.g., because they are compile-time constants), no call
  /// instruction is emitted and the function returns null.
  // 在run-time库中创建一个对指定函数的调用
  // 若所有参数都是具体的，就不会发生调用，而是返回空
  std::optional<SymbolicComputation>
  buildRuntimeCall(llvm::IRBuilder<> &IRB, SymFnT function,
                   llvm::ArrayRef<std::pair<llvm::Value *, bool>> args) {
    if (std::all_of(args.begin(), args.end(),
                    [this](std::pair<llvm::Value *, bool> arg) {
                      return (getSymbolicExpression(arg.first) == nullptr);
                    })) { // 若其参数为符号参数，则获取对应的符号表达式
      return {};
    }

    return forceBuildRuntimeCall(IRB, function, args);
  }

  /// Convenience overload that treats all arguments as symbolic.
  // 在run-time库中创建一个对指定函数的调用(执行中将所有参数视为符号）
  std::optional<SymbolicComputation>
  buildRuntimeCall(llvm::IRBuilder<> &IRB, SymFnT function,
                   llvm::ArrayRef<llvm::Value *> symbolicArgs) {
    std::vector<std::pair<llvm::Value *, bool>> args;
    for (const auto &arg : symbolicArgs) {
      args.emplace_back(arg, true); // 设标志bool为true，表示所有参数均为符号
    }

    return buildRuntimeCall(IRB, function, args);
  }

  /// Register the result of the computation as the symbolic expression
  /// corresponding to the concrete value and record the computation for
  /// short-circuiting.
  // 将计算结果存储为与具体值相对应的符号表达式
  void registerSymbolicComputation(const SymbolicComputation &computation,
                                   llvm::Value *concrete = nullptr) {
    if (concrete != nullptr)
      symbolicExpressions[concrete] = computation.lastInstruction;
    expressionUses.push_back(computation);
  }

  /// Convenience overload for chaining with buildRuntimeCall.
  void registerSymbolicComputation(
      const std::optional<SymbolicComputation> &computation,
      llvm::Value *concrete = nullptr) {
    if (computation)
      registerSymbolicComputation(*computation, concrete);
  }

  /// Generate code that makes the solver try an alternative value for V.
  // 生成代码，使解算器尝试V的替代值。
  void tryAlternative(llvm::IRBuilder<> &IRB, llvm::Value *V);

  /// Helper to use a pointer to a host object as integer (truncating!).
  ///
  /// Note that the conversion will truncate the most significant bits of the
  /// pointer if the host uses larger addresses than the target. Therefore, use
  /// this function only when such loss is acceptable (e.g., when generating
  /// site identifiers to be passed to the backend, where collisions of the
  /// least significant bits are reasonably unlikely).
  ///
  /// Why not do a lossless conversion and make the backend accept 64-bit
  /// integers?
  ///
  /// 1. Performance: 32-bit architectures will process 32-bit values faster
  /// than 64-bit values.
  ///
  /// 2. Pragmatism: Changing the backend to accept and process 64-bit values
  /// would require modifying code that we don't control (in the case of Qsym).
  // （通过将其视为整数）对指针进行截断处理，使得不同位数的指针（64位转32位）在目标对象上也可使用
  llvm::ConstantInt *getTargetPreferredInt(void *pointer) {
    return llvm::ConstantInt::get(intPtrType,
                                  reinterpret_cast<uint64_t>(pointer));
  }

  /// Compute the offset of a member in a (possibly nested) aggregate.
  // 计算域(结构体)中的成员的偏移量（这个结构体可能是嵌套的）
  uint64_t aggregateMemberOffset(llvm::Type *aggregateType,
                                 llvm::ArrayRef<unsigned> indices) const;

  const Runtime runtime;

  /// The data layout of the currently processed module.
  //当前处理模块的数据布局
  const llvm::DataLayout &dataLayout;

  /// The width in bits of pointers in the module.
  // 模块中指针的长度
  unsigned ptrBits;

  /// An integer type at least as wide as a pointer.
  // 一个至少和指针宽度一致的整数原型
  llvm::IntegerType *intPtrType;

  /// Mapping from SSA values to symbolic expressions.
  ///
  /// For pointer values, the stored value is an expression describing the value
  /// of the pointer itself (i.e., the address, not the referenced value). For
  /// structure values, the expression is a single large bit vector.
  ///
  /// TODO This member adds a lot of complexity: various methods rely on it, and
  /// finalizePHINodes invalidates it. We may want to pass the map around
  /// explicitly.
  // 从SSA中间语言的值到符号表达式的映射（指针存储的是指向的地址，结构体存储的是大型位向量）
  llvm::ValueMap<llvm::Value *, llvm::Value *> symbolicExpressions;

  /// A record of all PHI nodes in this function.
  ///
  /// PHI nodes may refer to themselves, in which case we run into an infinite
  /// loop when trying to generate symbolic expressions recursively. Therefore,
  /// we only insert a dummy symbolic expression for each PHI node and fix it
  /// after all instructions have been processed.
  // 存储函数中的所有PHI结点
  llvm::SmallVector<llvm::PHINode *, kExpectedMaxPHINodesPerFunction> phiNodes;

  /// A record of expression uses that can be short-circuited.
  ///
  /// Most values in a program are concrete, even if they're not constant (in
  /// which case we would know that they're concrete at compile time already).
  /// There is no point in building up formulas if all values involved in a
  /// computation are concrete, so we short-circuit those cases. Since this
  /// process requires splitting basic blocks, we can't do it during the main
  /// analysis phase (because InstVisitor gets out of step if we try).
  /// Therefore, we keep a record of all the places that construct expressions
  /// and insert the fast path later.
  // 记录可以成为短路的表达式（短路指部分无需详细执行，例如a&&b，a若为0则b无需计算；此处的短路指若表达式内均为具体值，则无需进行符号计算）
  std::vector<SymbolicComputation> expressionUses;
};

#endif
