
!!
!!
!!
module ErrorMessages
  implicit none
  private

  ! RCS info
  character (len = 128), parameter :: &
    RCSid = "$Id: errormessages.f90,v 1.1.1.1 2002/09/17 18:23:54 afsdhmd Exp $"

  ! Private (internal) parameters. Underscores are used here so arguments
  !   to inquiry functions don't need them.
  integer, parameter :: max_num_messages        = 100, &
                        max_message_length      = 256

  ! -- Integer values that define the error state.
  !   The values are arbitrary and aren't available outside the module.
  integer, parameter :: UndefinedState   = 0, &
                        SuccessState     = 1, &
                        WarningState     = 2, &
                        FailureState     = 3
  character (len = 21), parameter :: &
                        UndefinedMessageText = "Status is undefined."

  ! Public type: at least one variable of this type is declared in the
  !   procedure that uses this module. The components of this type are
  !   private, and so not accessable outside the module.
  type, public :: ErrorMessage
    private

    ! lastMessage keeps track of the last message added to the pile
    ! currentMessage is used to iterate through a pile of messages
    integer                                :: lastMessage, currentMessage

    ! These members are allocated statically - this ensures that they'll
    !   be available at run time and simplifies book keeping, but
    !   means that we can potentially run out of room.
    ! messageTexts keeps any textual information passed along with the status
    character (len = max_message_length),     &
             dimension(0:max_num_messages) :: messageTexts

    ! states holds one of the states we've defined
    integer, dimension(0:max_num_messages) :: states
  end type ErrorMessage

  ! Here we declare which functions are visible outside the module.
  !   This defines the interface.
  ! -- Boolean functions for reporting the current error state
  public :: stateIsSuccess, &
            stateIsWarning, &
            stateIsFailure

  ! -- Routines for setting state and adding a new message
  public :: initializeState,   &
            setStateToSuccess, &
            setStateToWarning, &
            setStateToFailure, &
            setStateToCompleteSuccess, &
            get_next_io_unit

  ! -- Routines for iterating through the list of messages
  !  Pattern N ("Iterator") in the Gang of Four book.
  public :: firstMessage,      &
            nextMessage,       &
            getCurrentMessage, &
            moreMessagesExist

  ! -- Introspection: inquire about the maximum lengths of strings
  public :: getErrorMessageLimits

contains
  ! -----------
  !  Functions for initializing or setting state with optional messages
  ! -----------
  function stateIsSuccess(messageVariable)
    type (ErrorMessage), intent ( in) :: messageVariable
    logical                           :: stateIsSuccess

    ! The current state is reported as success only if has been explicitly
    !   set that way. No state may have ever been set (lastMessage = 0)
    !   or the state might be something else (Warning, Failure).
    stateIsSuccess =                        &
      messageVariable%lastMessage > 0 .and. &
      messageVariable%states(messageVariable%currentMessage) == SuccessState
  end function stateIsSuccess

  ! -----------
  function stateIsWarning(messageVariable)
    type (ErrorMessage), intent ( in) :: messageVariable
    logical                           :: stateIsWarning

    ! See notes in stateIsSuccess function
    stateIsWarning =                        &
      messageVariable%lastMessage > 0 .and. &
      messageVariable%states(messageVariable%currentMessage) == WarningState
  end function stateIsWarning

  ! -----------
  function stateIsFailure(messageVariable)
    type (ErrorMessage), intent ( in) :: messageVariable
    logical                           :: stateIsFailure

   ! See notes in stateIsSuccess function
    stateIsFailure =                        &
      messageVariable%lastMessage > 0 .and. &
      messageVariable%states(messageVariable%currentMessage) == FailureState
  end function stateIsFailure

  ! -----------
   function getCurrentMessage(messageVariable)
     type (ErrorMessage),    intent (in)  :: messageVariable
     character (len = max_message_length) :: getCurrentMessage

     ! If someone has never used one of the "set" functions they have
     !   no reason to be asking for the text. We give them some text
     !   that tells them so. The state is undefined
     !   (not Success, Warning, or Failure).
     if(messageVariable%currentMessage == 0) then
       getCurrentMessage = UndefinedMessageText
     else
       getCurrentMessage = trim(messageVariable%messageTexts(messageVariable%currentMessage))
     end if
   end function getCurrentMessage

  ! -----------
  !  Subroutines for initializing or setting state with optional messages
  ! -----------
  subroutine initializeState(messageVariable)
    type (ErrorMessage),           intent (out) :: messageVariable

    ! Starting with a clean slate. Everything is set to
    !   undefined values
    messageVariable%currentMessage  = 0
    messageVariable%lastMessage     = 0
    messageVariable%messageTexts(:) = ""
    messageVariable%messageTexts(0) = trim(UndefinedMessageText)
    messageVariable%states(:)       = UndefinedState
  end subroutine initializeState

  ! -----------
  subroutine setStateToSuccess(messageVariable, messageText)
    type (ErrorMessage),           intent (out) :: messageVariable
    character (len = *), optional, intent ( in) :: messageText

    ! Local variable
    integer :: thisMessage

    ! lastMessage keeps track of the end of the pile of messages;
    !   currentMessage helps us iterate through the list.
    ! We shouldn't change the state in the middle of iterating through
    !   the list.
    thisMessage = messageVariable%lastMessage
    ! We only add a new message if we have enough room. We're most interested
    !   in the first errors (presumably those lowest on the call stack) so we
    !   overwrite the most recent messages if we don't have enough room.
    if (thisMessage < max_num_messages) &
      thisMessage = thisMessage + 1
    messageVariable%currentMessage              = thisMessage
    ! When we set the state we go back to the top (most recent end)
    !   of the pile of messages.
    messageVariable%lastMessage                 = thisMessage

    messageVariable%states(thisMessage)         = SuccessState
    if(present(messageText)) &
      messageVariable%messageTexts(thisMessage) = trim(messageText)
  end subroutine setStateToSuccess

  ! -----------
  subroutine setStateToWarning(messageVariable, messageText)
    type (ErrorMessage),           intent (out) :: messageVariable
    character (len = *), optional, intent ( in) :: messageText

    ! Local variable
    integer :: thisMessage

    ! See comments in setStateToSuccess
    thisMessage = messageVariable%lastMessage
    if (thisMessage < max_num_messages) &
      thisMessage = thisMessage + 1
    messageVariable%lastMessage                 = thisMessage
    messageVariable%currentMessage              = thisMessage

    messageVariable%states(thisMessage)         = WarningState
    if(present(messageText)) &
      messageVariable%messageTexts(thisMessage) = trim(messageText)
  end subroutine setStateToWarning

  ! -----------
  subroutine setStateToFailure(messageVariable, messageText)
    type (ErrorMessage),           intent (out) :: messageVariable
    character (len = *), optional, intent ( in) :: messageText

    ! Local variable
    integer :: thisMessage

    ! See comments in setStateToSuccess
    thisMessage = messageVariable%lastMessage
    if (thisMessage < max_num_messages) &
      thisMessage = thisMessage + 1
    messageVariable%lastMessage                 = thisMessage
    messageVariable%currentMessage              = thisMessage

    messageVariable%states(thisMessage)         = FailureState
    if(present(messageText)) &
      messageVariable%messageTexts(thisMessage) = trim(messageText)
  end subroutine setStateToFailure

  ! -----------
  subroutine setStateToCompleteSuccess(messageVariable, messageText)
    type (ErrorMessage),           intent (out) :: messageVariable
    character (len = *), optional, intent ( in) :: messageText

    ! Everything we've done is successful. We clear any messages
    !   in the pile and tell everyone we're OK.
    call initializeState(messageVariable)
    if(present(messageText)) then
      call setStateToSuccess(messageVariable, messageText)
    else
      call setStateToSuccess(messageVariable)
    end if
  end subroutine setStateToCompleteSuccess

  ! -----------
  !  Functions for iterating over a collection of messages
  ! -----------
   subroutine firstMessage(messageVariable)
     type (ErrorMessage), intent (inout) :: messageVariable

     messageVariable%currentMessage = min(1, messageVariable%lastMessage)
   end subroutine firstMessage

  ! -----------
   subroutine nextMessage(messageVariable)
     type (ErrorMessage), intent (out) :: messageVariable

     ! Users should stop asking for the next message once we've reached the
     !   top of the pile, but they might not.
     if(messageVariable%currentMessage < messageVariable%lastMessage) &
       messageVariable%currentMessage = messageVariable%currentMessage + 1
   end subroutine nextMessage

  ! -----------
   function moreMessagesExist(messageVariable)
     type (ErrorMessage), intent (in) :: messageVariable
     logical                          :: moreMessagesExist

     ! Can we go on to the next message?
     moreMessagesExist = &
       messageVariable%currentMessage < messageVariable%lastMessage
   end function moreMessagesExist

  ! -----------
  !  Inquiry procedures
  ! -----------
  subroutine getErrorMessageLimits(messageVariable, maxNumberOfMessages, maxMessageLength)
     type (ErrorMessage), intent ( in) :: messageVariable
     integer, optional,   intent (out) :: maxNumberOfMessages, maxMessageLength

     ! Access to internal string lengths, maximum number of messages.
     !  This is in the spirit of introspection, but isn't that useful since
     !  everything is allocated at compile time.
     if(present(maxNumberOfMessages)) &
       maxNumberOfMessages = max_num_messages
     if(present(maxMessageLength))    &
       maxMessageLength = max_message_length
  end subroutine getErrorMessageLimits

function get_next_io_unit () result (next)
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
! find a unit number available for i/o action
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  implicit none
  integer :: next   ! the next available unit number

  integer, parameter :: min_unit = 10, max_unit = 999
  integer, save      :: last_unit = 0   ! initalize
  integer            :: count           ! number of failures
  logical            :: open            ! file status

  count = 0 ; next = min_unit - 1
  if ( last_unit > 0 ) then ! check next in line
    next = last_unit + 1
    inquire (unit=next, opened=open)
    if ( .not. open ) last_unit = next ! found it
    return
  else ! loop through allowed units
    do ! forever
      next = next + 1
      inquire (unit=next, opened=open)
      if ( .not. open ) then
        last_unit = next     ! found it
        exit ! the unit loop
      end if
      if ( next == max_unit ) then ! attempt reset 3 times
        last_unit = 0
        count     = count + 1
        if ( count <= 3 ) next = min_unit - 1
      end if ! reset try
      if ( next > max_unit ) then ! abort
        print *,'ERROR: max unit exceeded in get_next_io_unit'
        stop    'ERROR: max unit exceeded in get_next_io_unit'
      end if ! abort
    end do ! over unit numbers
  end if ! last_unit
end function get_next_io_unit
end module ErrorMessages

! -----------
! CVS/RCS modification history
!
! $Log: errormessages.f90,v $
! Revision 1.1.1.1  2002/09/17 18:23:54  afsdhmd
! import sources
!
! -----------


