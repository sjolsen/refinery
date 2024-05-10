#ifndef BORAX_MOCK_ERROR_HPP
#define BORAX_MOCK_ERROR_HPP

#include <iostream>

template <typename ... T>
static inline void
MockError (
  const char    *Function,
  const char    *File,
  int           Line,
  const T &...  Args
  )
{
  std::cerr << Function << ":" << File << ":" << Line << ": ";
  (std::cerr << ... << Args);
  std::cerr << std::endl;
}

#define MOCK_ERROR(_s)  (MockError (__PRETTY_FUNCTION__, __FILE__, __LINE__, (_s)))

#endif // BORAX_MOCK_ERROR_HPP
