      Subroutine PREFIXS(prefix)
      implicit none
      integer prefix(*)
#include "maxprms.cdk"
#include "char.cdk"
  
      EXTERNAL ARGDIMS, ARGDOPE
      INTEGER  ARGDIMS, ARGDOPE, LISTE(20), I,k,po,nc,c,npref
      character (len=4) bufc

      s_prefix_path =' '
      npref = argdope(1,liste,20)
      do k=1,npref
        PO = ISHFT(LISTE(K),-16)
        NC = IAND(255, ISHFT(LISTE(K),-8))
        c=iand(255,liste(k))
!        print *,'PO=',po,' nc=',nc,' c=',c
        do i=po,po+(nc+3)/4-1
          write(bufc,'(a4)') prefix(i)
!          print *,'bufc=',bufc
          s_prefix_path = trim(s_prefix_path) // trim(bufc)
        enddo
      enddo
!      print *,'s_prefix_path=','"',trim(s_prefix_path),'"'
      return
      end

      Subroutine PREFIXD(prefix)
      implicit none
      integer prefix(*)
#include "maxprms.cdk"
#include "char.cdk"
  
      EXTERNAL ARGDIMS, ARGDOPE
      INTEGER  ARGDIMS, ARGDOPE, LISTE(20), I,k,po,nc,c,npref
      character (len=4) bufc

      d_prefix_path =' '
      npref = argdope(1,liste,20)
      do k=1,npref
        PO = ISHFT(LISTE(K),-16)
        NC = IAND(255, ISHFT(LISTE(K),-8))
        c=iand(255,liste(k))
        do i=po,po+(nc+3)/4-1
          write(bufc,'(a4)') prefix(i)
          d_prefix_path = trim(d_prefix_path) // trim(bufc)
        enddo
      enddo
!      print *,'d_prefix_path=','"',trim(d_prefix_path),'"'
      return
      end
