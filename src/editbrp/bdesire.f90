!   S/P BDESIRE - EXTRACTION DES ARGUMENTS D'UNE DIRECTIVE "DESIRE BURP"
!
      SUBROUTINE BDESIRE(STATION,LAT,LON,DAT,TYPE,FLAG,DTX,DTY,LENG)
      use app
      IMPLICIT NONE 
  
      INTEGER  TYPE(10), LAT(10), LON(10), DAT(20), FLAG(10), &
              LENG(10), DTX(10), DTY(10), STATION(30)    
!
!AUTEUR
!VERSION ORIGINALE Y. BOURASSA FEV 91
!REVISION 001      "      "    DEC 91 QDFERR, BUG TRIAGE PAR DATE
!         002      "      "    AVR 92 FIX BUG ENTRY BEXCLUR
!     LANGUAGA FTN77
  
!ARGUMENTS
! ENTREE  STATION -  1 A 10 LISTE DE STATION
!   "     LAT     -    "    LATITUDES  OU INTERVALE AVEC SAUT
!   "     LON     -    "    LONGITUDES OU INTERVALE AVEC SAUT
!   "     DAT     -    "    DATES/TEMP OU INTERVALE AVEC SAUT
!   "     TYPE    -    "    TYPES DE DECORDS
!   "     FLAG    -    "    PATRONS DE 24 BYTES
!   "     DTX     -    "    DX
!   "     DTY     -    "    DY
!   "     LENG    -    "    DIMENSIONS DE RECORD EN UNITES DE 64 BITS
!
#include "maxprms.cdk"
#include "desrs.cdk"
#include "fiches.cdk"
#include "char.cdk"
  
      EXTERNAL ARGDIMS, ARGDOPE, BEXDES
      INTEGER  ARGDIMS, ARGDOPE, LISTE(10), I, J, K, DEX, NC, PO
      DEX = -1
    1 IF(NREQ .EQ. NMD) THEN
         call App_Log(APP_ERROR,'bdesire: Maximum number of requests reached')
         RETURN
      ENDIF
  
!     SI DEJA EN MODE EXPRESS CDETTE DIRECTIVE EXT INUTILE
      IF( EXPRESS ) THEN
         call App_Log(APP_WARNING,'bdesire: Useless directive')
         RETURN
      ENDIF

!     COMPTE LES DIRECTIVES 
      NREQ = NREQ+1 
      NARG(NREQ) = NP-1
      write(app_msg,*) 'bdesire: Request #',NREQ
      call App_Log(APP_INFO,app_msg)

!     INDICATEUR QUE LA REQUETE NREQ N'EST PAS SATISFAITE
      SATISF(NREQ) = 0
!     INDICATEUR QUE LA REQUETE NREQ EST DESIRE ou EXCLURE
      DESEXC(NREQ) = DEX
      STNS(NREQ)   = 0
      DO 10 J=1,8
         REQ(11,J,NREQ) = 0
  10     CONTINUE
  
      GO TO(120, 110, 100, 90, 80, 60, 50, 40, 20) NP
!     TRIAGE PAR LENG
   20 IF(LENG(1) .eq. -1) THEN
         NP = 8
      ELSE
         REQ(11,8,NREQ) = ARGDIMS(9)
            REQ(J,8,NREQ) = LENG(J) * WL
   30       CONTINUE
      write(app_msg,*) 'bdesire: LENG= ',(REQ(I,8,NREQ),I=1,ARGDIMS(9))
      call App_Log(APP_INFO,app_msg)
      ENDIF

!     TRIAGE PAR DTY
   40 IF(DTY(1) .eq. -1) THEN
         IF(NP .eq. 8) NP = 7
      ELSE
         CALL BEXDES(DTY, ARGDIMS(8), 7)
         write(app_msg,*) 'bdesire: DTY  = ',(REQ(I,7,NREQ),I=1,ARGDIMS(8))
         call App_Log(APP_INFO,app_msg)
         ENDIF

!     TRIAGE PAR DTX
   50 IF(DTX(1) .eq. -1) THEN
         IF(NP .eq. 7) NP = 6
      ELSE
         CALL BEXDES(DTX, ARGDIMS(7), 6)
         write(app_msg,*) 'bdesire: DTX  = ',(REQ(I,6,NREQ),I=1,ARGDIMS(7))
         call App_Log(APP_INFO,app_msg)
         ENDIF

!     TRIAGE PAR FLAG
   60 IF(FLAG(1) .eq. -1) THEN
         IF(NP .eq. 6) NP = 5
      ELSE
         REQ(11,5,NREQ) = ARGDIMS(6)
         DO 70 J=1,ARGDIMS(6)
            REQ(J,5,NREQ) = FLAG(J)
   70       CONTINUE
         write(app_msg,*) 'bdesire: FLAGS= ',(REQ(I,5,NREQ),I=1,ARGDIMS(6))
         call App_Log(APP_INFO,app_msg)
         ENDIF

!     TRIAGE PAR TYPE
   80 IF(TYPE(1) .eq. -1) THEN
         IF(NP .eq. 5) NP = 4
      ELSE
         CALL BEXDES(TYPE, ARGDIMS(5), 4)
         write(app_msg,*) 'bdesire: TYPE= ',(REQ(I,4,NREQ),I=1,ARGDIMS(5))
         call App_Log(APP_INFO,app_msg)
      ENDIF

!     TRIAGE PAR DATE
   90 IF(DAT(1) .eq.-1) THEN
         IF(NP .eq. 4) NP = 3
      ELSE
         CALL BEXDES(DAT, ARGDIMS(4), 3)
      ENDIF

!     TRIAGE PAR LON
  100 IF(LON(1) .eq. -1) THEN
         IF(NP .eq. 3) NP = 2
      ELSE
         CALL BEXDES(LON, ARGDIMS(3), 2)
         write(app_msg,*) 'bdesire: LON = ',(REQ(I,2,NREQ),I=1,ARGDIMS(3))
         call App_Log(APP_INFO,app_msg)
      ENDIF

!     TRIAGE PAR LAT 
  110 IF(LAT(1) .eq. -1) THEN
         IF(NP .eq. 2) NP = 1
      ELSE
         CALL BEXDES(LAT, ARGDIMS(2), 1)
         write(app_msg,*) 'bdesire: LAT = ',(REQ(I,1,NREQ),I=1,ARGDIMS(2))
         call App_Log(APP_INFO,app_msg)
      ENDIF

!     TRIAGE PAR STNID
  120 IF(STATION(1) .NE. -1) THEN
         STNS(NREQ) = ARGDOPE(1, LISTE, 10)
         DO 130 K=1, STNS(NREQ)
            PO = ISHFT(LISTE(K),-16)
            NC = IAND(255, ISHFT(LISTE(K),-8))
            IF(NC .GT. 9) THEN
               call App_Log(APP_ERROR,'bdesire: STNID limited to 9 characters')
               I = 48
            ENDIF
            IF(IAND(255, LISTE(K)) .NE. 3) then
               call App_Log(APP_ERROR,'bdesire: invalid STNID')
               I = 48
            endif
            I  = NC / NCW
            IF(I*NCW .LT. NC) I = I+1  
            WRITE(ID(K,NREQ), CAR9) (STATION(J),J=PO,PO+I-1)
  130       CONTINUE
         write(app_msg,*) 'bdesire: STNID=',(ID(I,NREQ),I=1,STNS(NREQ))
         call App_Log(APP_INFO,app_msg)       
      ELSEIF(NP .eq. 1) THEN
         IF( DEX.eq.0 .and. NREQ.eq.NEXC ) then
            call App_Log(APP_WARNING,'bdesire: Exclude all the file ?')
            I = 48
         endif
         IF(DEX.eq.-1 .and. NREQ.eq.1) THEN
            EXPRESS = .true. 
            IF( .not.STATS ) PRINT*,'DEMANSE DE COPIE INTEGRALE DE S'
         ENDIF
      ENDIF
      RETURN
  
!     POUR POINTER LES CHAMPS NON VOULUS
      ENTRY BEXCLUR(STATION, LAT, LON, DAT, TYPE, FLAG, DTX, DTY, LENG) 
!     COMPTE LES DIRECTIVES EXCLURE
      NEXC = NEXC + 1
      DEX  = 0
      GO TO 1
  
      END 








