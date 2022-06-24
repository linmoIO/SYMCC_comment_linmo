// 主要做从域中获取符号表达式以及将符号表达式加入到域中的操作

#include "GarbageCollection.h"

#include <vector>

#include <Runtime.h>
#include <Shadow.h>

/// A list of memory regions that are known to contain symbolic expressions.
// 初始域集（一系列存储了可知的符号表达式的域）
std::vector<ExpressionRegion> expressionRegions;

// 注册表达式域（将当前域加入域集）
void registerExpressionRegion(ExpressionRegion r) {
  expressionRegions.push_back(std::move(r));  // std::move用于强制转换（具体STFW）
}

// 返回当前可找到的符号表达式集合
std::set<SymExpr> collectReachableExpressions() {
  std::set<SymExpr> reachableExpressions;
  auto collectReachableExpressions = [&](ExpressionRegion r) {  // 创建获取表达式的函数（这种编写方法为lambda表达式）
    auto *end = r.first + r.second;
    for (SymExpr *expr_ptr = r.first; expr_ptr < end; expr_ptr++) { // 遍历域
      if (*expr_ptr != nullptr) { // 若表达式不为空，则加入返回集合
        reachableExpressions.insert(*expr_ptr);
      }
    }
  };

  for (auto &r : expressionRegions) { // 遍历域集中的域，进行表达式获取
    collectReachableExpressions(r);
  }

  for (const auto &mapping : g_shadow_pages) {  // 根据映射遍历影子内存，获取表达式
    collectReachableExpressions({mapping.second, kPageSize});
  }

  return reachableExpressions;
}
