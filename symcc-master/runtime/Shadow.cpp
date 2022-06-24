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

#include "Shadow.h"

// 影子内存的具体实现是通过map，键为地址指针，值存储对应的符号表达式
// 影子内存主要映射内存中的值对应的符号表达式（包括全局变量、文件流、内存存储的值）
std::map<uintptr_t, SymExpr *> g_shadow_pages;
