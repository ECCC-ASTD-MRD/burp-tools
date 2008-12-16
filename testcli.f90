! F2KCLI : Fortran 200x Command Line Interface
! copyright Interactive Software Services Ltd. 2001
! For conditions of use see manual.txt
!
! Free-format Fortran 90 test program
!
PROGRAM TESTCLI
!
USE F2KCLI
IMPLICIT NONE ! for sake of Elf90
!
CHARACTER(LEN=256) :: LINE
CHARACTER(LEN=256) :: EXE
CHARACTER(LEN=40)  :: CMD
INTEGER            :: NARG,IARG
!
NARG = COMMAND_ARGUMENT_COUNT()
WRITE(*,*) 'Arg count=', NARG
!
CALL GET_COMMAND(LINE)
WRITE(*,*) 'Line=',TRIM(LINE)
!
CALL GET_COMMAND_ARGUMENT(0,EXE)
WRITE(*,*) 'Program=',TRIM(EXE)
!
DO IARG = 1,NARG
  CALL GET_COMMAND_ARGUMENT(IARG,CMD)
  WRITE(*,*) 'Arg ',IARG,'=',CMD
END DO
STOP  ! for sake of Elf90
END PROGRAM TESTCLI
