       PROGRAM VOIRBRP

       IMPLICIT NONE
       EXTERNAL MRFVOI, FNOM, CCARD
       INTEGER FNOM, MRFVOI
       INTEGER IER,IPOS
       CHARACTER*256 LISTE(2), DEF1(2), DEF2(2)


       DATA LISTE /'L','IMENT.'/

       DATA DEF1  /'OUTPUT','NULL'/
       DATA DEF2  /'OUTPUT','NULL'/

       IPOS = -1
       CALL CCARD(LISTE, DEF1, DEF2,2,IPOS)

       IER = FNOM(6,DEF2(1),'SEQ+FTN+FMT',0)
       IER = FNOM(10,DEF2(2),'RND',0)

       IER = MRFVOI(10)

       STOP
       END
      
      character *128 function product_id_tag()
      product_id_tag='$Id$'
      return
      end
