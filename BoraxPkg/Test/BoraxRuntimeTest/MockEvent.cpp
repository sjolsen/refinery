#include "MockEvent.hpp"

#include <cstdint>

#include "MockError.hpp"

MockEventEngine  *MockEventEngine::Singleton = nullptr;

static inline EFI_EVENT
EventFromIndex (
  std::size_t  Index
  )
{
  return reinterpret_cast<void *>(static_cast<std::uintptr_t>(Index));
}

static inline std::size_t
EventToIndex (
  EFI_EVENT  Event
  )
{
  return static_cast<std::size_t>(reinterpret_cast<std::uintptr_t>(Event));
}

EFI_STATUS
MockEventEngine::GetEvent (
  EFI_EVENT  Event,
  MockEvent  **MockEvent
  )
{
  std::size_t  Index = EventToIndex (Event);

  if (Index >= Events.size ()) {
    MOCK_ERROR ("event index out of range");
    return EFI_INVALID_PARAMETER;
  }

  *MockEvent = Events[Index].get ();
  return EFI_SUCCESS;
}

EFI_STATUS
MockEventEngine::Enqueue (
  EFI_EVENT  Event
  )
{
  EFI_STATUS  Status;
  MockEvent   *MockEvent;

  Status = GetEvent (Event, &MockEvent);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  switch (MockEvent->NotifyTpl) {
    case TPL_CALLBACK:
      CallbackQueue.push_back (Event);
      return EFI_SUCCESS;
    case TPL_NOTIFY:
      NotifyQueue.push_back (Event);
      return EFI_SUCCESS;
    default:
      MOCK_ERROR ("invalid NotifyTpl");
      return EFI_INVALID_PARAMETER;
  }
}

VOID
MockEventEngine::ProcessOne (
  EFI_EVENT  Event
  )
{
  EFI_STATUS  Status;
  MockEvent   *MockEvent;

  Status = GetEvent (Event, &MockEvent);
  if (EFI_ERROR (Status)) {
    return;
  }

  MockEvent->NotifyFunction (Event, MockEvent->NotifyContext);
}

VOID
MockEventEngine::ProcessQueues (
  )
{
  while (true) {
    if ((NotifyQueue.size () > 0) && (CurrentTpl < TPL_NOTIFY)) {
      EFI_TPL    OldTpl = MockRaiseTPL (TPL_NOTIFY);
      EFI_EVENT  Event  = NotifyQueue.front ();
      NotifyQueue.pop_front ();
      (VOID)ProcessOne (Event);
      MockRestoreTPL (OldTpl);
    } else if ((CallbackQueue.size () > 0) && (CurrentTpl < TPL_CALLBACK)) {
      EFI_TPL    OldTpl = MockRaiseTPL (TPL_CALLBACK);
      EFI_EVENT  Event  = CallbackQueue.front ();
      CallbackQueue.pop_front ();
      (VOID)ProcessOne (Event);
      MockRestoreTPL (OldTpl);
    } else {
      return;
    }
  }
}

EFI_TPL
EFIAPI
MockRaiseTPL (
  IN EFI_TPL  NewTpl
  )
{
  MockEventEngine  *This = MockEventEngine::Singleton;

  std::swap (This->CurrentTpl, NewTpl);
  return NewTpl;
}

VOID
EFIAPI
MockRestoreTPL (
  IN EFI_TPL  OldTpl
  )
{
  MockEventEngine  *This = MockEventEngine::Singleton;

  This->CurrentTpl = OldTpl;
}

EFI_STATUS
EFIAPI
MockCreateEvent (
  IN  UINT32            Type,
  IN  EFI_TPL           NotifyTpl,
  IN  EFI_EVENT_NOTIFY  NotifyFunction OPTIONAL,
  IN  VOID              *NotifyContext OPTIONAL,
  OUT EFI_EVENT         *Event
  )
{
  MockEventEngine  *This = MockEventEngine::Singleton;

  // Validate Type, NotifyFunction, and NotifyContext
  switch (Type) {
    case 0:
      NotifyFunction = nullptr;
      NotifyContext  = nullptr;
      break;
    case EVT_NOTIFY_WAIT:
    case EVT_NOTIFY_SIGNAL:
      if (NotifyFunction == nullptr) {
        MOCK_ERROR ("NotifyFunction is null");
        return EFI_INVALID_PARAMETER;
      }

      if (NotifyContext == nullptr) {
        MOCK_ERROR ("NotifyContext is null");
        return EFI_INVALID_PARAMETER;
      }

      // Validate NotifyTpl
      switch (NotifyTpl) {
        case TPL_CALLBACK:
        case TPL_NOTIFY:
          break;
        default:
          MOCK_ERROR ("invalid NotifyTpl");
          return EFI_INVALID_PARAMETER;
      }

      break;
    default:
      MOCK_ERROR ("invalid Type");
      return EFI_INVALID_PARAMETER;
  }

  // Validate Event
  if (Event == nullptr) {
    MOCK_ERROR ("Event is null");
    return EFI_INVALID_PARAMETER;
  }

  // Create the event
  *Event = EventFromIndex (This->Events.size ());
  MockEventEngine::MockEvent  MockEvent {
    Type,
    NotifyTpl,
    NotifyFunction,
    NotifyContext,
    MockEventEngine::ST_WAITING
  };

  This->Events.push_back (std::make_unique<MockEventEngine::MockEvent> (MockEvent));
  return EFI_SUCCESS;
}

EFI_STATUS
EFIAPI
MockWaitForEvent (
  IN  UINTN      NumberOfEvents,
  IN  EFI_EVENT  *Event,
  OUT UINTN      *Index
  )
{
  EFI_STATUS                  Status;
  MockEventEngine             *This = MockEventEngine::Singleton;
  MockEventEngine::MockEvent  *MockEvent;
  BOOLEAN                     WorkDone;

  // Validate parameters
  if (This->CurrentTpl != TPL_APPLICATION) {
    MOCK_ERROR ("WaitForEvent not called from TPL_APPLICATION");
    return EFI_UNSUPPORTED;
  }

  if (NumberOfEvents == 0) {
    MOCK_ERROR ("NumberOfEvents is null");
    return EFI_INVALID_PARAMETER;
  }

  if (Event == nullptr) {
    MOCK_ERROR ("Event is null");
    return EFI_INVALID_PARAMETER;
  }

  if (Index == nullptr) {
    MOCK_ERROR ("Index is null");
    return EFI_INVALID_PARAMETER;
  }

  for (UINTN I = 0; I < NumberOfEvents; ++I) {
    Status = This->GetEvent (Event[I], &MockEvent);
    if (EFI_ERROR (Status)) {
      return Status;
    }

    if (MockEvent->Type == EVT_NOTIFY_SIGNAL) {
      MOCK_ERROR ("WaitForEvent called on event of type EVT_NOTIFY_SIGNAL");
      *Index = I;
      return EFI_INVALID_PARAMETER;
    }
  }

  // Loop until an event is signaled
  do {
    WorkDone = FALSE;

    for (UINTN I = 0; I < NumberOfEvents; ++I) {
      Status = This->GetEvent (Event[I], &MockEvent);
      if (EFI_ERROR (Status)) {
        return Status;
      }

      if (MockEvent->State == MockEventEngine::ST_SIGNALED) {
        MockEvent->State = MockEventEngine::ST_WAITING;
        *Index           = I;
        return EFI_SUCCESS;
      }

      if (MockEvent->NotifyFunction == nullptr) {
        continue;
      }

      Status = This->Enqueue (Event);
      if (EFI_ERROR (Status)) {
        return Status;
      }

      WorkDone = TRUE;
      This->ProcessQueues ();

      if (MockEvent->State == MockEventEngine::ST_SIGNALED) {
        MockEvent->State = MockEventEngine::ST_WAITING;
        *Index           = I;
        return EFI_SUCCESS;
      }
    }
  } while (WorkDone);

  MOCK_ERROR ("WaitForEvent will never complete");
  return EFI_UNSUPPORTED;
}

EFI_STATUS
EFIAPI
MockSignalEvent (
  IN  EFI_EVENT  Event
  )
{
  EFI_STATUS                  Status;
  MockEventEngine             *This = MockEventEngine::Singleton;
  MockEventEngine::MockEvent  *MockEvent;

  Status = This->GetEvent (Event, &MockEvent);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  if (MockEvent->State == MockEventEngine::ST_SIGNALED) {
    return EFI_SUCCESS;
  }

  MockEvent->State = MockEventEngine::ST_SIGNALED;

  if (MockEvent->Type == EVT_NOTIFY_SIGNAL) {
    Status = This->Enqueue (Event);
    if (EFI_ERROR (Status)) {
      return Status;
    }
  }

  This->ProcessQueues ();
  return EFI_SUCCESS;
}

EFI_STATUS
EFIAPI
MockCloseEvent (
  IN EFI_EVENT  Event
  )
{
  MockEventEngine  *This = MockEventEngine::Singleton;
  std::size_t      Index = EventToIndex (Event);

  if (Index >= This->Events.size ()) {
    MOCK_ERROR ("Event index out of range");
    return EFI_INVALID_PARAMETER;
  }

  if (This->Events[Index] == nullptr) {
    MOCK_ERROR ("CloseEvent called on closed event");
    return EFI_INVALID_PARAMETER;
  }

  This->Events[Index].reset (nullptr);
  return EFI_SUCCESS;
}

EFI_STATUS
EFIAPI
MockCheckEvent (
  IN EFI_EVENT  Event
  )
{
  EFI_STATUS                  Status;
  MockEventEngine             *This = MockEventEngine::Singleton;
  MockEventEngine::MockEvent  *MockEvent;

  Status = This->GetEvent (Event, &MockEvent);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  if (MockEvent->Type == EVT_NOTIFY_SIGNAL) {
    MOCK_ERROR ("CheckEvent called on event of type EVT_NOTIFY_SIGNAL");
    return EFI_INVALID_PARAMETER;
  }

  if (MockEvent->State == MockEventEngine::ST_SIGNALED) {
    MockEvent->State = MockEventEngine::ST_WAITING;
    return EFI_SUCCESS;
  }

  if (MockEvent->NotifyFunction == nullptr) {
    return EFI_NOT_READY;
  }

  Status = This->Enqueue (Event);
  if (EFI_ERROR (Status)) {
    return Status;
  }

  This->ProcessQueues ();

  if (MockEvent->State == MockEventEngine::ST_SIGNALED) {
    MockEvent->State = MockEventEngine::ST_WAITING;
    return EFI_SUCCESS;
  }

  return EFI_NOT_READY;
}

MockEventEngine::MockEventEngine(
                                 )
  : BindSingleton {&Singleton, this},
  BindRaiseTPL {&gBS->RaiseTPL, MockRaiseTPL},
  BindRestoreTPL {&gBS->RestoreTPL, MockRestoreTPL},
  BindCreateEvent {&gBS->CreateEvent, MockCreateEvent},
  BindWaitForEvent {&gBS->WaitForEvent, MockWaitForEvent},
  BindSignalEvent {&gBS->SignalEvent, MockSignalEvent},
  BindCloseEvent {&gBS->CloseEvent, MockCloseEvent},
  BindCheckEvent {&gBS->CheckEvent, MockCheckEvent},
  CurrentTpl {TPL_APPLICATION}
{
}
