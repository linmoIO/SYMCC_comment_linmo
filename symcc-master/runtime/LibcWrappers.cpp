// 主要做函数库封装操作，针对的是截取函数集中的函数（查看Runtime.cpp最后面的截取函数集），
// 注意和Rumtime.cpp中所针对的IR函数部分的插桩进行区分

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <string>
#include <vector>

#include <arpa/inet.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>

#include "Config.h"
#include "Shadow.h"
#include <Runtime.h>

#define SYM(x) x##_symbolized // 对函数重命名，加上_symbolized后缀

// 创建本地静态命名空间（仅本文件内使用）
namespace {

/// The file descriptor referring to the symbolic input.
// 指向符号输入的文件描述符（针对后面的open这样的文件操作）
int inputFileDescriptor = -1;

/// The current position in the (symbolic) input.
// 当前符号输入的位置
uint64_t inputOffset = 0;

/// Tell the solver to try an alternative value than the given one.
// 调用解算器，尝试生成Value的替代值（也就是调用Solver进行约束求解）
// 此处为将"符号表达式=具体值"这一约束压入路径约束，进行路径约束求解
template <typename V, typename F>
void tryAlternative(V value, SymExpr valueExpr, F caller) {
  if (valueExpr) {
    _sym_push_path_constraint(
        _sym_build_equal(valueExpr,
                         _sym_build_integer(value, sizeof(value) * 8)),
        true, reinterpret_cast<uintptr_t>(caller));
  }
}

// A partial specialization for pointer types for convenience.
// 针对指针类型，对tryAlternative函数进行重载
template <typename E, typename F>
void tryAlternative(E *value, SymExpr valueExpr, F caller) {
  tryAlternative(reinterpret_cast<intptr_t>(value), valueExpr, caller);
}
} // namespace

// 对函数库封装进行初始化
void initLibcWrappers() {
  if (g_config.fullyConcrete) // 如果不允许符号表达式（全都是具体值，则直接返回）
    return;

  if (g_config.inputFile.empty()) { // 若输入文件为空
    // Symbolic data comes from standard input.
    inputFileDescriptor = 0;
  }
}

// 用C编译器进行编译（而不使用C++），会使得函数仅被存储为名字，而不保存参数类型（不使用函数重载）
// 例如，C++中，对于void func(int a,int b)会存储为_func_int_int，而C则是会存储为_func
// （因为
extern "C" {

// SYM(malloc)表示为函数加上_symbolized后缀，当前也就是 void *malloc_symbolized(malloc)(size_t size)
// 内存分配，根据size的大小申请内存（不做初始化）
void *SYM(malloc)(size_t size) {
  auto *result = malloc(size);    // 获得具体值

  tryAlternative(size, _sym_get_parameter_expression(0), SYM(malloc));  // 把参数的符号约束压入

  _sym_set_return_expression(nullptr);  // 设置返回值的符号表达式
  return result;  // 返回实际具体执行的值 （当前函数的返回类型为void*，不是void，注意区分，void*表示指向任意类型的指针类型）
}

// calloc_symbolized
// 内存分配，根据元素数目nmemb和元素大小size分配内存，做初始化
void *SYM(calloc)(size_t nmemb, size_t size) {
  auto *result = calloc(nmemb, size);

  tryAlternative(nmemb, _sym_get_parameter_expression(0), SYM(calloc)); // 把参数的符号约束压入
  tryAlternative(size, _sym_get_parameter_expression(1), SYM(calloc));  // 把参数的符号约束压入

  _sym_set_return_expression(nullptr);
  return result;
}

// See comment on lseek and lseek64 below; the same applies to the "off"
// parameter of mmap.

// mmap64_symbolized
// 把文件映射到虚拟内存部分，用于修改文件（而无需通过read和write等）
void *SYM(mmap64)(void *addr, size_t len, int prot, int flags, int fildes,
                  uint64_t off) {
  auto *result = mmap64(addr, len, prot, flags, fildes, off);

  tryAlternative(len, _sym_get_parameter_expression(1), SYM(mmap64));

  _sym_set_return_expression(nullptr);
  return result;
}

// mmap_symbolized
// 把文件映射到虚拟内存部分，用于修改文件（而无需通过read和write等）
void *SYM(mmap)(void *addr, size_t len, int prot, int flags, int fildes,
                uint32_t off) {
  return SYM(mmap64)(addr, len, prot, flags, fildes, off);  // 可依赖的只有mmap64
}

// open_symbolized（实际上还没支持文件的符号化操作，只能做具体执行）
// 打开文件
int SYM(open)(const char *path, int oflag, mode_t mode) {
  auto result = open(path, oflag, mode);
  _sym_set_return_expression(nullptr);

  if (result >= 0 && !g_config.fullyConcrete && !g_config.inputFile.empty() &&
      strstr(path, g_config.inputFile.c_str()) != nullptr) {  // 若符合各类条件（open具体操作成功、不是全都为具体值、(文件)输入不为空、含路径）
    if (inputFileDescriptor != -1)  // 且文件描述符不为-1
      std::cerr << "Warning: input file opened multiple times; this is not yet "
                   "supported"
                << std::endl;
    inputFileDescriptor = result;
    inputOffset = 0;
  }

  return result;
}

// read_symbolized
// 从文件中读取数据
ssize_t SYM(read)(int fildes, void *buf, size_t nbyte) {
  tryAlternative(buf, _sym_get_parameter_expression(1), SYM(read));
  tryAlternative(nbyte, _sym_get_parameter_expression(2), SYM(read));

  auto result = read(fildes, buf, nbyte);
  _sym_set_return_expression(nullptr);

  if (result < 0)
    return result;

  // 后续操作是将具体的文件的输入字符装入影子内存中
  if (fildes == inputFileDescriptor) {
    // Reading symbolic input.
    ReadWriteShadow shadow(buf, result);  // 根据buf和result创建一个可读写影子内存视图
    std::generate(shadow.begin(), shadow.end(),
                  []() { return _sym_get_input_byte(inputOffset++); });
                  // 存入具体的值
  } else if (!isConcrete(buf, result)) {
    ReadWriteShadow shadow(buf, result);  // 创建可读写影子内存视图
    std::fill(shadow.begin(), shadow.end(), nullptr); // 全置为空（表示无input文件的输入）
  }

  return result;
}

// lseek is a bit tricky because, depending on preprocessor macros, glibc
// defines it to be a function operating on 32-bit values or aliases it to
// lseek64. Therefore, we cannot know in general whether calling lseek in our
// code takes a 32 or a 64-bit offset and whether it returns a 32 or a 64-bit
// result. In fact, since we compile this library against LLVM which requires us
// to compile with "-D_FILE_OFFSET_BITS=64", we happen to know that, for us,
// lseek is an alias to lseek64, but this may change any time. More importantly,
// client code may call one or the other, depending on its preprocessor
// definitions.
//
// Therefore, we define symbolic versions of both lseek and lseek64, but
// internally we only use lseek64 because it's the only one on whose
// availability we can rely.

// lseek64_symbolized
// 根据文件操作符fd，基于基准whence(文件开头、结尾等)和偏移量offset，设置文件读写指针的位置
// 返回指针距离文件起始位置的偏移量
uint64_t SYM(lseek64)(int fd, uint64_t offset, int whence) {
  auto result = lseek64(fd, offset, whence);
  _sym_set_return_expression(nullptr);
  if (result == (off_t)-1)  // 若失败
    return result;

  if (whence == SEEK_SET)   // SEEK_SET=0，也就是文件头（目前只支持SEEK_SET，不支持SEEK_CUR和SEEK_END）
    _sym_set_return_expression(_sym_get_parameter_expression(1)); // 设置返回值的符号表达式

  if (fd == inputFileDescriptor)
    inputOffset = result;   // 更新文件偏移量

  return result;
}

// lseek_symbolized
// 根据文件操作符fd，基于基准whence(文件开头、结尾等)和偏移量offset，设置文件读写指针的位置
// 返回指针距离文件起始位置的偏移量
uint32_t SYM(lseek)(int fd, uint32_t offset, int whence) {
  uint64_t result = SYM(lseek64)(fd, offset, whence); // 调用64位的

  // Perform the same overflow check as glibc in the 32-bit version of lseek.

  auto result32 = (uint32_t)result; // 对32位做额外处理
  if (result == result32)           // 若符合，则返回
    return result32;

  // 若溢出，则显示错误
  errno = EOVERFLOW;
  return (uint32_t)-1;
}

// fopen_symbolized
// 根据路径和打开模式打开文件，返回指向该文件的指针（是系统调用句柄open的封装函数，有用户态的缓存，避免用户态和内核态的反复切换）
FILE *SYM(fopen)(const char *pathname, const char *mode) {
  // 实现参考open，类似
  auto *result = fopen(pathname, mode);
  _sym_set_return_expression(nullptr);

  if (result != nullptr && !g_config.fullyConcrete &&
      !g_config.inputFile.empty() &&
      strstr(pathname, g_config.inputFile.c_str()) != nullptr) {
    if (inputFileDescriptor != -1)
      std::cerr << "Warning: input file opened multiple times; this is not yet "
                   "supported"
                << std::endl;
    inputFileDescriptor = fileno(result);
    inputOffset = 0;
  }

  return result;
}

// fopen64_symbolized
// 根据路径和打开模式打开文件，返回指向该文件的指针（是系统调用句柄open的封装函数，有用户态的缓存，避免用户态和内核态的反复切换）
FILE *SYM(fopen64)(const char *pathname, const char *mode) {
  auto *result = fopen64(pathname, mode);
  _sym_set_return_expression(nullptr);

  if (result != nullptr && !g_config.fullyConcrete &&
      !g_config.inputFile.empty() &&
      strstr(pathname, g_config.inputFile.c_str()) != nullptr) {
    if (inputFileDescriptor != -1)
      std::cerr << "Warning: input file opened multiple times; this is not yet "
                   "supported"
                << std::endl;
    inputFileDescriptor = fileno(result);
    inputOffset = 0;
  }

  return result;
}

// fread_symbolized
// 和fopen配套的，带缓冲区，从文件中读取字符
size_t SYM(fread)(void *ptr, size_t size, size_t nmemb, FILE *stream) {
  tryAlternative(ptr, _sym_get_parameter_expression(0), SYM(fread));
  tryAlternative(size, _sym_get_parameter_expression(1), SYM(fread));
  tryAlternative(nmemb, _sym_get_parameter_expression(2), SYM(fread));

  // 后续操作是将具体的文件的输入字符装入影子内存中，参考read

  auto result = fread(ptr, size, nmemb, stream);
  _sym_set_return_expression(nullptr);

  if (fileno(stream) == inputFileDescriptor) {
    // Reading symbolic input.
    ReadWriteShadow shadow(ptr, result * size);
    std::generate(shadow.begin(), shadow.end(),
                  []() { return _sym_get_input_byte(inputOffset++); });
  } else if (!isConcrete(ptr, result * size)) {
    ReadWriteShadow shadow(ptr, result * size);
    std::fill(shadow.begin(), shadow.end(), nullptr);
  }

  return result;
}

// fgets_symbolized
// 从指定文件输入流中读取字符，一次读取一行
char *SYM(fgets)(char *str, int n, FILE *stream) {
  tryAlternative(str, _sym_get_parameter_expression(0), SYM(fgets));
  tryAlternative(n, _sym_get_parameter_expression(1), SYM(fgets));

  auto result = fgets(str, n, stream);
  _sym_set_return_expression(_sym_get_parameter_expression(0)); // 设置返回值的表达式

  if (fileno(stream) == inputFileDescriptor) {
    // Reading symbolic input.
    ReadWriteShadow shadow(str, sizeof(char) * strlen(str));
    std::generate(shadow.begin(), shadow.end(),
                  []() { return _sym_get_input_byte(inputOffset++); });
  } else if (!isConcrete(str, sizeof(char) * strlen(str))) {
    ReadWriteShadow shadow(str, sizeof(char) * strlen(str));
    std::fill(shadow.begin(), shadow.end(), nullptr);
  }

  return result;
}

// rewind_symbolized
// 设置文件的指针重新指向一个流的开头
void SYM(rewind)(FILE *stream) {
  rewind(stream);
  _sym_set_return_expression(nullptr);

  if (fileno(stream) == inputFileDescriptor) {
    inputOffset = 0;
  }
}

// fseek_symbolized
// 移动文件指针位置（和lseek类似，只是这个是针对文件流的）
int SYM(fseek)(FILE *stream, long offset, int whence) {
  tryAlternative(offset, _sym_get_parameter_expression(1), SYM(fseek));

  auto result = fseek(stream, offset, whence);
  _sym_set_return_expression(nullptr);
  if (result == -1)
    return result;

  if (fileno(stream) == inputFileDescriptor) {
    auto pos = ftell(stream); // ftell能获得文件位置指针当前相对于文件首的偏移字节数
    if (pos == -1)
      return -1;
    inputOffset = pos;
  }

  return result;
}

// fseeko_symbolized
// 和fseek的区别仅仅是offset的数据类型不同
int SYM(fseeko)(FILE *stream, off_t offset, int whence) {
  tryAlternative(offset, _sym_get_parameter_expression(1), SYM(fseeko));

  auto result = fseeko(stream, offset, whence);
  _sym_set_return_expression(nullptr);
  if (result == -1)
    return result;

  if (fileno(stream) == inputFileDescriptor) {
    auto pos = ftello(stream);  // 获得文件位置指针当前相对于文件首的偏移字节数
    if (pos == -1)
      return -1;
    inputOffset = pos;
  }

  return result;
}

// fseeko64_symbolized
// 和fseek的区别仅仅是offset的数据类型不同
int SYM(fseeko64)(FILE *stream, uint64_t offset, int whence) {
  tryAlternative(offset, _sym_get_parameter_expression(1), SYM(fseeko64));

  auto result = fseeko64(stream, offset, whence);
  _sym_set_return_expression(nullptr);
  if (result == -1)
    return result;

  if (fileno(stream) == inputFileDescriptor) {
    auto pos = ftello64(stream);
    if (pos == -1)
      return -1;
    inputOffset = pos;
  }

  return result;
}

// getc_symbolized
// 从指定文件中读取字符
int SYM(getc)(FILE *stream) {
  auto result = getc(stream); // 读取字符，存储到result中
  if (result == EOF) {        // 若文件读完，则设置返回表达式为空，并返回结果
    _sym_set_return_expression(nullptr);
    return result;
  }

  // 否则
  if (fileno(stream) == inputFileDescriptor)
    _sym_set_return_expression(_sym_build_zext(
        _sym_get_input_byte(inputOffset++), sizeof(int) * 8 - 8));
        // 根据偏移在上下文中获取字符表达式，并将其返回
  else
    _sym_set_return_expression(nullptr);

  return result;
}

// fgetc_symbolized
// 和getc的区别是，getc是宏，fgetc是函数
int SYM(fgetc)(FILE *stream) {
  auto result = fgetc(stream);
  if (result == EOF) {
    _sym_set_return_expression(nullptr);
    return result;
  }

  if (fileno(stream) == inputFileDescriptor)
    _sym_set_return_expression(_sym_build_zext(
        _sym_get_input_byte(inputOffset++), sizeof(int) * 8 - 8));
  else
    _sym_set_return_expression(nullptr);

  return result;
}

// getchar_symbolized
// 获取字符（从控制台IO流）
int SYM(getchar)(void) {
  return SYM(getc)(stdin);  // 替换为基于stdin(流)调用getc
}

// ungetc_symbolized
// getc的逆操作，将一个字符回退到输入流中，返回被推入的字符
int SYM(ungetc)(int c, FILE *stream) {
  auto result = ungetc(c, stream);
  _sym_set_return_expression(_sym_get_parameter_expression(0));
  // 设置返回值的符号表达式为返回被推入的字符的符号表达式

  if (fileno(stream) == inputFileDescriptor && result != EOF)
    inputOffset--;  // 更新偏移量

  return result;
}

// memcpy_symbolized
// 内存拷贝
void *SYM(memcpy)(void *dest, const void *src, size_t n) {
  auto *result = memcpy(dest, src, n);

  tryAlternative(dest, _sym_get_parameter_expression(0), SYM(memcpy));
  tryAlternative(src, _sym_get_parameter_expression(1), SYM(memcpy));
  tryAlternative(n, _sym_get_parameter_expression(2), SYM(memcpy));

  _sym_memcpy(static_cast<uint8_t *>(dest), static_cast<const uint8_t *>(src),
              n); // 此处是将影子内存中对应区域的符号表达式也进行拷贝
  _sym_set_return_expression(_sym_get_parameter_expression(0));
  return result;
}

// memset_symbolized
// 内存初始化
void *SYM(memset)(void *s, int c, size_t n) {
  auto *result = memset(s, c, n);

  tryAlternative(s, _sym_get_parameter_expression(0), SYM(memset));
  tryAlternative(n, _sym_get_parameter_expression(2), SYM(memset));

  // 将影子内存中的对应区域初始化为c对应的符号表达式（也就是对应符号化操作）
  _sym_memset(static_cast<uint8_t *>(s), _sym_get_parameter_expression(1), n);
  _sym_set_return_expression(_sym_get_parameter_expression(0));
  return result;
}

// memmove_symbolized
// 将内存中src区域的值复制到dest中，n为需要复制的字节数
// 和memset的区别是，其允许源内存区和目的内存区有重叠（通过改变拷贝顺序）
void *SYM(memmove)(void *dest, const void *src, size_t n) {
  tryAlternative(dest, _sym_get_parameter_expression(0), SYM(memmove));
  tryAlternative(src, _sym_get_parameter_expression(1), SYM(memmove));
  tryAlternative(n, _sym_get_parameter_expression(2), SYM(memmove));

  auto *result = memmove(dest, src, n);
  _sym_memmove(static_cast<uint8_t *>(dest), static_cast<const uint8_t *>(src),
               n);  // 对影子内存中做相同操作

  _sym_set_return_expression(_sym_get_parameter_expression(0));
  return result;
}

// strncpy_symbolized
// 字符串拷贝函数（不足n个补0）
char *SYM(strncpy)(char *dest, const char *src, size_t n) {
  tryAlternative(dest, _sym_get_parameter_expression(0), SYM(strncpy));
  tryAlternative(src, _sym_get_parameter_expression(1), SYM(strncpy));
  tryAlternative(n, _sym_get_parameter_expression(2), SYM(strncpy));

  auto *result = strncpy(dest, src, n);
  _sym_set_return_expression(nullptr);

  size_t srcLen = strnlen(src, n);      // 获取源字符串长度
  size_t copied = std::min(n, srcLen);  // 获取需要copy的长度
  if (isConcrete(src, copied) && isConcrete(dest, n))
    return result;  // 若源和目的均是具体值，无符号表达式，则无需额外处理

  // 否则在影子内存里进行额外的针对符号表达式的处理
  auto srcShadow = ReadOnlyShadow(src, copied); // 根据源创建只读影子内存视图
  auto destShadow = ReadWriteShadow(dest, n);   // 根据目的地创建可读写影子内存视图

  std::copy(srcShadow.begin(), srcShadow.end(), destShadow.begin());  // 将源copy到目的中
  if (copied < n) { // 若不足n个
    ReadWriteShadow destRestShadow(dest + copied, n - copied);  // 创建可读写影子内存视图
    std::fill(destRestShadow.begin(), destRestShadow.end(), nullptr); // 补0（也就是空）
  }

  return result;
}

// strchr_symbolized
// 在字符串s中查找字符c，返回指向这个位置
const char *SYM(strchr)(const char *s, int c) {
  tryAlternative(s, _sym_get_parameter_expression(0), SYM(strchr));
  tryAlternative(c, _sym_get_parameter_expression(1), SYM(strchr));

  auto *result = strchr(s, c);
  _sym_set_return_expression(nullptr);

  auto *cExpr = _sym_get_parameter_expression(1);   // 获取字符c的符号表达式
  if (isConcrete(s, result != nullptr ? (result - s) : strlen(s)) &&
      cExpr == nullptr) // 若无需对符号表达式进行处理，则直接返回
    return result;

  if (cExpr == nullptr) // 若字符无符号表达式
    cExpr = _sym_build_integer(c, 8);   // 则赋符号表达式
  else
    cExpr = _sym_build_trunc(cExpr, 8); // 根据原有的创建符号表达式（截取为8位）

  size_t length = result != nullptr ? (result - s) : strlen(s); // 获得字符c位置的长度（若无则为s的长度）
  auto shadow = ReadOnlyShadow(s, length);  // 创建只读影子内存视图（存储字符s到c的截取片段）
  auto shadowIt = shadow.begin();           // 指针指向影子内存的起始
  for (size_t i = 0; i < length; i++) {     // 遍历源字符串s，从头开始到c的位置
    // 约束应为：c之前的位置，均和c的符号表达式不相等；c的位置，和c的符号表达式相等
    _sym_push_path_constraint(
        _sym_build_not_equal(
            (*shadowIt != nullptr) ? *shadowIt : _sym_build_integer(s[i], 8),
            cExpr),
        /*taken*/ 1, reinterpret_cast<uintptr_t>(SYM(strchr)));
    ++shadowIt;
  }

  return result;
}

// memcmp_symbolized
// 内存比较函数，比较n个字符
int SYM(memcmp)(const void *a, const void *b, size_t n) {
  tryAlternative(a, _sym_get_parameter_expression(0), SYM(memcmp));
  tryAlternative(b, _sym_get_parameter_expression(1), SYM(memcmp));
  tryAlternative(n, _sym_get_parameter_expression(2), SYM(memcmp));

  auto result = memcmp(a, b, n);
  _sym_set_return_expression(nullptr);

  if (isConcrete(a, n) && isConcrete(b, n))
    return result;

  auto aShadowIt = ReadOnlyShadow(a, n).begin_non_null(); // 为a设置只读影子内存视图，指向开头
  auto bShadowIt = ReadOnlyShadow(b, n).begin_non_null(); // 为b设置只读影子内存视图，指向开头
  auto *allEqual = _sym_build_equal(*aShadowIt, *bShadowIt);  // build相等约束
  for (size_t i = 1; i < n; i++) {  // 遍历
    ++aShadowIt;  // 指向下一个
    ++bShadowIt;  // 指向下一个
    allEqual =    // 对约束求合取（即每一个字符均要相等），得到合取约束
        _sym_build_bool_and(allEqual, _sym_build_equal(*aShadowIt, *bShadowIt));
  }

  // 将约束压入路径
  _sym_push_path_constraint(allEqual, result == 0,
                            reinterpret_cast<uintptr_t>(SYM(memcmp)));
  return result;
}

// ntohl_symbolized
// 将网络上传输的字节顺序转换为本机的（网络上和大端一致）
// 但其他常用的网络编程函数并没有符号化实现（例如htonl），感觉是未实现完
uint32_t SYM(ntohl)(uint32_t netlong) {
  auto netlongExpr = _sym_get_parameter_expression(0);
  auto result = ntohl(netlong);

  if (netlongExpr == nullptr) {
    _sym_set_return_expression(nullptr);
    return result;
  }

#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__ // 小端机器
  _sym_set_return_expression(_sym_build_bswap(netlongExpr));
#elif __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__  // 大端机器
  _sym_set_return_expression(netlongExpr);
#else
#error Unsupported __BYTE_ORDER__
#endif

  return result;
}
}
