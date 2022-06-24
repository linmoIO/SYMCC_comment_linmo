// 主要做从域中获取符号表达式以及将符号表达式加入到域中的操作

#ifndef GARBAGECOLLECTION_H
#define GARBAGECOLLECTION_H

#include <utility>
#include <set>

#include <Runtime.h>

/// An imitation of std::span (which is not available before C++20) for symbolic
/// expressions.
// 针对std::span的符号表达式模拟
using ExpressionRegion = std::pair<SymExpr *, size_t>;

/// Add the specified region to the list of places to search for symbolic
/// expressions.
// 将具体的域加入到查找符号表达式的域集中
void registerExpressionRegion(ExpressionRegion r);

/// Return the set of currently reachable symbolic expressions.
// 返回当前可找到的符号表达式集合
std::set<SymExpr> collectReachableExpressions();

#endif
