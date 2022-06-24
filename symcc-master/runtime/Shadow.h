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

#ifndef SHADOW_H
#define SHADOW_H

#include <algorithm>
#include <cassert>
#include <cstring>
#include <iterator>
#include <map>

#include <Runtime.h>

#include <z3.h>

//
// This file is dedicated to the management of shadow memory.
//
// We manage shadows at page granularity. Since the shadow for each page is
// malloc'ed and thus at an unpredictable location in memory, we need special
// handling for memory allocations that cross page boundaries. This header
// provides iterators over shadow memory that automatically handle jumps between
// memory pages (and thus shadow regions). They should work with the C++
// standard library.
//
// We represent shadowed memory as a sequence of 8-bit expressions. The
// iterators therefore expose the shadow in the form of byte expressions.
//

// 影子内存以页面为管理单位，有指向页面头部的迭代器（用于处理跨页面的操作）
// 用于影子内存的底层是用map实现的，因此迭代器也是基于STL标准库的
// 影子内存为8位表达式序列，因此迭代器是以字节表示

constexpr uintptr_t kPageSize = 4096;   // 页的大小为4k
// uintptr_t为64位，8个字节，unsigned long long类型
// 地址为64位，页的大小为4k，也就是低12位为页内offset，高24位为页号

/// Compute the corresponding page address.
// 根据地址，获取对应页起始的位置
constexpr uintptr_t pageStart(uintptr_t addr) {
  return (addr & ~(kPageSize - 1));
  // 即addr & ~0xfff，即将低位置0，获取地址对应的页的起始地址（offset=0）
}

/// Compute the corresponding offset into the page.
// 根据地址，获取页内偏移
constexpr uintptr_t pageOffset(uintptr_t addr) {
  return (addr & (kPageSize - 1));
  // 即addr & 0xfff，即截取低12位，获取地址对应的页内偏移
}

/// A mapping from page addresses to the corresponding shadow regions. Each
/// shadow is large enough to hold one expression per byte on the shadowed page.
// 影子内存，每一个地址对应一个符号表达式
extern std::map<uintptr_t, SymExpr *> g_shadow_pages;

/// An iterator that walks over the shadow bytes corresponding to a memory
/// region. If there is no shadow for any given memory address, it just returns
/// null.
// 影子内存的读迭代器（双向迭代器）
class ReadShadowIterator
    : public std::iterator<std::bidirectional_iterator_tag, SymExpr> {
public:
  // 构造函数（用地址对迭代器进行初始化）
  explicit ReadShadowIterator(uintptr_t address)
      : std::iterator<std::bidirectional_iterator_tag, SymExpr>(),
        address_(address), shadow_(getShadow(address)) {}

  // 重载自加操作符++
  ReadShadowIterator &operator++() {
    auto previousAddress = address_++;  // 地址++
    if (shadow_ != nullptr) // 若表达式指针不为空
      shadow_++;  // 指向下一个
    if (pageStart(address_) != pageStart(previousAddress))  // 若涉及跨页
      shadow_ = getShadow(address_);  // 则重新获取指向对应表达式的指针
    return *this;
  }

  // 重载自减操作符--，操作和上述类似
  ReadShadowIterator &operator--() {
    auto previousAddress = address_--;
    if (shadow_ != nullptr)
      shadow_--;
    if (pageStart(address_) != pageStart(previousAddress))
      shadow_ = getShadow(address_);
    return *this;
  }

  // 重载取值操作符*
  SymExpr operator*() {
    assert((shadow_ == nullptr || *shadow_ == nullptr ||
            _sym_bits_helper(*shadow_) == 8) &&
           "Shadow memory always represents bytes");  // 需要存在对应表达式，或者指针为空or对应表达式为空（格式验证）
    return shadow_ != nullptr ? *shadow_ : nullptr; // 返回对应表达式；若shadow内对应的表达式为空，则返回空指针
  }

  // 重载相等判断==
  bool operator==(const ReadShadowIterator &other) const {
    return (address_ == other.address_);  // 所对应的地址相等即为相等
  }

  // 重载不相等判断!=
  bool operator!=(const ReadShadowIterator &other) const {
    return !(*this == other); // 只要不是全部相等，即为不相等
  }

protected:
  // 获取指向对应表达式的指针的函数
  static SymExpr *getShadow(uintptr_t address) {
    if (auto shadowPageIt = g_shadow_pages.find(pageStart(address));  // 在影子内存中根据对应地址寻址
        shadowPageIt != g_shadow_pages.end()) // 若对应页面存在
      return shadowPageIt->second + pageOffset(address);  // 返回该对应位置的表达式

    return nullptr; // 不存在则返回空
  }

  uintptr_t address_; // 当前迭代器对应的地址
  SymExpr *shadow_;   // 当前迭代器存储的指向表达式的指针
};

/// Like ReadShadowIterator, but return an expression for the concrete memory
/// value if a region does not have a shadow.
// 扩展版读迭代器，继承于影子内存的读迭代器
// 对于影子内存中无对应符号表达式的情况（返回为nullptr的情况），会去实际内存中返回具体值
class NonNullReadShadowIterator : public ReadShadowIterator {
public:
  // 构造函数
  explicit NonNullReadShadowIterator(uintptr_t address)
      : ReadShadowIterator(address) {}

  // 重载取值操作符*
  SymExpr operator*() {
    if (auto *symbolicResult = ReadShadowIterator::operator*()) // 若获取到的不为nullptr
      return symbolicResult;  // 直接返回

    // 否则去实际内存中返回具体值
    return _sym_build_integer(*reinterpret_cast<const uint8_t *>(address_), 8);
  }
};

/// An iterator that walks over the shadow corresponding to a memory region and
/// exposes it for modification. If there is no shadow yet, it creates a new
/// one.
// 影子内存的写迭代器（继承自读迭代器）
class WriteShadowIterator : public ReadShadowIterator {
public:
  // 根据地址进行构造
  WriteShadowIterator(uintptr_t address) : ReadShadowIterator(address) {
    shadow_ = getOrCreateShadow(address); // 若之前不存在，则会创建
  }

  // 重载自加操作符++ （和之前ReadShadowIterator中的基本一致，感觉冗余了）
  WriteShadowIterator &operator++() {
    auto previousAddress = address_++;
    shadow_++;
    if (pageStart(address_) != pageStart(previousAddress))
      shadow_ = getOrCreateShadow(address_);
    return *this;
  }

  // 重载自减操作符--
  WriteShadowIterator &operator--() {
    auto previousAddress = address_--;
    shadow_--;
    if (pageStart(address_) != pageStart(previousAddress))
      shadow_ = getOrCreateShadow(address_);
    return *this;
  }

  // 重载取值操作符*
  SymExpr &operator*() { return *shadow_; }

protected:
  // 获取符号表达式或者创建符号表达式（若不存在）
  static SymExpr *getOrCreateShadow(uintptr_t address) {
    if (auto *shadow = getShadow(address))  // 获取符号表达式
      return shadow;  // 不为空直接返回

    // 创建新的影子内存页并初始化
    auto *newShadow =
        static_cast<SymExpr *>(malloc(kPageSize * sizeof(SymExpr)));
        // 申请内存空间，大小为 4k*符号表达式大小
    memset(newShadow, 0, kPageSize * sizeof(SymExpr));  // 对内存空间进行初始化
    g_shadow_pages[pageStart(address)] = newShadow;     // 装入影子内存
    return newShadow + pageOffset(address); // 返回符号表达式
  }
};

/// A view on shadow memory that exposes read-only functionality.
// 只读影子内存视图（作为一种视图）
struct ReadOnlyShadow {
  template <typename T> // 模板
  // 根据地址指针和长度进行初始化
  ReadOnlyShadow(T *addr, size_t len)
      : address_(reinterpret_cast<uintptr_t>(addr)), length_(len) {}

  // 起始位置和结束位置（返回迭代器）
  ReadShadowIterator begin() const { return ReadShadowIterator(address_); }
  ReadShadowIterator end() const {
    return ReadShadowIterator(address_ + length_);
  }

  // 起始位置和结束位置（针对NonNull的情况，返回可获取具体值的迭代器）
  NonNullReadShadowIterator begin_non_null() const {
    return NonNullReadShadowIterator(address_);
  }
  NonNullReadShadowIterator end_non_null() const {
    return NonNullReadShadowIterator(address_ + length_);
  }

  uintptr_t address_; // 地址
  size_t length_;     // 内存长度
};

/// A view on shadow memory that allows modifications.
// 可读写影子内存视图（作为一种视图）
template <typename T> struct ReadWriteShadow {
  // 根据地址指针和长度进行初始化
  ReadWriteShadow(T *addr, size_t len)
      : address_(reinterpret_cast<uintptr_t>(addr)), length_(len) {}

  // 起始位置和结束位置（返回迭代器）
  WriteShadowIterator begin() { return WriteShadowIterator(address_); }
  WriteShadowIterator end() { return WriteShadowIterator(address_ + length_); }

  uintptr_t address_; // 地址
  size_t length_;     // 内存长度
};

/// Check whether the indicated memory range is concrete, i.e., there is no
/// symbolic byte in the entire region.
// 检查对应内存区域是否均为具体值
template <typename T> bool isConcrete(T *addr, size_t nbytes) {
  // Fast path for allocations within one page.
  auto byteBuf = reinterpret_cast<uintptr_t>(addr); // 获取地址
  if (pageStart(byteBuf) == pageStart(byteBuf + nbytes) &&
      !g_shadow_pages.count(pageStart(byteBuf)))  // 若影子内存中找不到对应地址的页
    return true;  // 则表示均为具体值

  // 若内存涉及跨页操作
  ReadOnlyShadow shadow(addr, nbytes);  // 创建只读影子内存视图
  // 遍历，查找对应的符号表达式，均为空则返回true；否则返回false
  return std::all_of(shadow.begin(), shadow.end(),
                     [](SymExpr expr) { return (expr == nullptr); });
}

#endif
