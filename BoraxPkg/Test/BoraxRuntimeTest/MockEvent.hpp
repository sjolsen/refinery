#ifndef BORAX_MOCK_EVENT_H
#define BORAX_MOCK_EVENT_H

#include <deque>
#include <memory>
#include <vector>

extern "C" {
  #include <Uefi.h>
  #include <Library/UefiBootServicesTableLib.h>
}

template <typename T>
class TemporaryBinding {
private:
  T *Location_;
  T OldValue_;

public:
  TemporaryBinding(
                   T  *Location,
                   T  NewValue
                   )
    : Location_ (Location), OldValue_ (NewValue)
  {
    if (Location_ != nullptr) {
      std::swap (*Location_, OldValue_);
    }
  }

  ~TemporaryBinding(
                    )
  {
    if (Location_ != nullptr) {
      *Location_ = OldValue_;
    }
  }

  TemporaryBinding(
                   const TemporaryBinding &
                   ) = delete;
  TemporaryBinding(
                   TemporaryBinding &&
                   ) = delete;
  TemporaryBinding &
  operator= (
    const TemporaryBinding &
    ) = delete;

  TemporaryBinding &
  operator= (
    TemporaryBinding &&
    ) = delete;
};

extern "C" {
  std::remove_pointer_t<EFI_RAISE_TPL>       MockRaiseTPL;
  std::remove_pointer_t<EFI_RESTORE_TPL>     MockRestoreTPL;
  std::remove_pointer_t<EFI_CREATE_EVENT>    MockCreateEvent;
  std::remove_pointer_t<EFI_WAIT_FOR_EVENT>  MockWaitForEvent;
  std::remove_pointer_t<EFI_SIGNAL_EVENT>    MockSignalEvent;
  std::remove_pointer_t<EFI_CLOSE_EVENT>     MockCloseEvent;
  std::remove_pointer_t<EFI_CHECK_EVENT>     MockCheckEvent;
}

class MockEventEngine {
private:
  enum MockEventState {
    ST_WAITING,
    ST_SIGNALED,
  };

  struct MockEvent {
    UINT32              Type;
    EFI_TPL             NotifyTpl;
    EFI_EVENT_NOTIFY    NotifyFunction;
    VOID                *NotifyContext;
    MockEventState      State;
  };

  TemporaryBinding<MockEventEngine *> BindSingleton;
  TemporaryBinding<EFI_RAISE_TPL> BindRaiseTPL;
  TemporaryBinding<EFI_RESTORE_TPL> BindRestoreTPL;
  TemporaryBinding<EFI_CREATE_EVENT> BindCreateEvent;
  TemporaryBinding<EFI_WAIT_FOR_EVENT> BindWaitForEvent;
  TemporaryBinding<EFI_SIGNAL_EVENT> BindSignalEvent;
  TemporaryBinding<EFI_CLOSE_EVENT> BindCloseEvent;
  TemporaryBinding<EFI_CHECK_EVENT> BindCheckEvent;

  EFI_TPL CurrentTpl;
  std::vector<std::unique_ptr<MockEvent> > Events;
  std::deque<EFI_EVENT> CallbackQueue;
  std::deque<EFI_EVENT> NotifyQueue;

  friend class MockEvent;
  friend std::remove_pointer_t<EFI_RAISE_TPL>       MockRaiseTPL;
  friend std::remove_pointer_t<EFI_RESTORE_TPL>     MockRestoreTPL;
  friend std::remove_pointer_t<EFI_CREATE_EVENT>    MockCreateEvent;
  friend std::remove_pointer_t<EFI_WAIT_FOR_EVENT>  MockWaitForEvent;
  friend std::remove_pointer_t<EFI_SIGNAL_EVENT>    MockSignalEvent;
  friend std::remove_pointer_t<EFI_CLOSE_EVENT>     MockCloseEvent;
  friend std::remove_pointer_t<EFI_CHECK_EVENT>     MockCheckEvent;

  static MockEventEngine *Singleton;

  EFI_STATUS
  GetEvent (
    EFI_EVENT  Event,
    MockEvent  **MockEvent
    );

  EFI_STATUS
  Enqueue (
    EFI_EVENT  Event
    );

  VOID
  ProcessOne (
    EFI_EVENT  Event
    );

  VOID
  ProcessQueues (
    );

public:
  MockEventEngine(
                  );
  ~MockEventEngine(
                   ) = default;
  MockEventEngine(
                  const MockEventEngine &
                  ) = delete;
  MockEventEngine(
                  MockEventEngine &&
                  ) = delete;
  MockEventEngine &
  operator= (
    const MockEventEngine &
    ) = delete;

  MockEventEngine &
  operator= (
    MockEventEngine &&
    ) = delete;
};

#endif // BORAX_MOCK_EVENT_H
