#ifndef BORAX_WRAP_FN_H
#define BORAX_WRAP_FN_H

#include <memory>

template <typename T, typename ProtocolT>
struct ProtocolBlockT {
  T            *This;
  ProtocolT    Protocol;
};

template <typename T, typename ProtocolT>
class ProtocolClass {
protected:
  std::unique_ptr<ProtocolBlockT<T, ProtocolT> > ProtocolBlock;

  static T *
  ProtocolDownCast (
    ProtocolT  *Protocol
    )
  {
    using Block = ProtocolBlockT<T, ProtocolT>;
    return BASE_CR (Protocol, Block, Protocol)->This;
  }

  static constexpr const auto Unsupported =
    [] (ProtocolT *Protocol, auto...) [[gnu::ms_abi]]->EFI_STATUS {
    return EFI_UNSUPPORTED;
  };

public:
  ProtocolClass(
                )
    : ProtocolBlock (std::make_unique<ProtocolBlockT<T, ProtocolT> >())
  {
    ProtocolBlock->This = static_cast<T *>(this);
  }

  ProtocolClass(
                const ProtocolClass &
                ) = delete;
  ProtocolClass(
                ProtocolClass &&
                ) = default;
  ProtocolClass &
  operator= (
    const ProtocolClass &
    ) = delete;

  ProtocolClass &
  operator= (
    ProtocolClass &&
    ) = default;

  ~ProtocolClass(
                 ) = default;

  ProtocolT *
  GetProtocol (
    ) const
  {
    return &ProtocolBlock->Protocol;
  }
};

#define WRAP_FN(_memfn)                               \
[](auto This, auto... Args) [[gnu::ms_abi]] -> auto { \
  return ProtocolDownCast(This)->_memfn(Args...);     \
}

#endif // BORAX_WRAP_FN_H
