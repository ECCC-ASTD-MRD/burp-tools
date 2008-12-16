

!!
!!
!!
module librmn_declaration

     implicit none
     integer, external:: mrfget, mrfprm, mrbhdr, mrfvoi
     integer, external:: mrbloc, mrbprm, mrbxtr
     integer, external:: mrbcvt, mrbdcl
     integer, external:: mrfopr
     integer, external:: mrbini,mrbcol, mrbadd, mrbupd, mrfput
     integer, external:: mrfdel, mrbdel
     integer, external:: mrfgor,mrfgoc
     integer, external:: mrbtyp, mrblocx
     integer, external:: fnom,fclos

!! interfaces
     interface

!     integer function fnom(unit,fname,acces,mode)
!        integer, intent(in)        :: unit
!        character*(*), intent (in) :: fname
!        character*(*), intent (in) :: acces
!        integer, intent(in)        :: mode
!     end function fnom

     integer function mrfopn(unit,acces)
        integer, intent(in) :: unit
        character*(*), intent (in) :: acces
     end function mrfopn

     integer function mrfmxl(unit)
        integer, intent(in) :: unit
     end function mrfmxl

     integer function mrfcls(unit)
        integer, intent(in) :: unit
     end function mrfcls

!     subroutine fclos(unit)
!        integer, intent(in) :: unit
!     end subroutine fclos

     integer function mrfloc(unit,my_handle, my_stnid, my_idtyp,   &
                  my_lati, my_long, my_date, my_temps,my_sup, my_nsup)
        character*(*),      intent (in) ::             my_stnid
        integer,            intent (in) ::             my_handle,  &
                                                       my_idtyp,   &
                                                       unit,       &
                                                       my_lati,    &
                                                       my_long,    &
                                                       my_temps,   &
                                                       my_date,    &
                                                       my_nsup
        integer, dimension(my_nsup),intent(in)  ::     my_sup
     end function mrfloc

     integer function mrfopc(optnom,opvalc)
        character(*), intent (in) :: optnom
        character(*), intent (in) :: opvalc
     end function mrfopc

     end interface
end module
