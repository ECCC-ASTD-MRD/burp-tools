*** S/R BEXDES DECODE LA DATE, LES LAT, LON LA CARTE DESIRE/EXCLURE
*
      SUBROUTINE BEXDES(CLE, N, M)
  
      IMPLICIT NONE 
  
      INTEGER    CLE(10), N, M
  
**ARGUMENTS
* ENTRE   CLE  -  ARGUMENTS DE L'APPEL A DESIRE 
*   "     N    -  DIMENSION DE CLE
*   "     M    -  PARAMETRE A DECODER 1=LAT 2=LON 3=DATE 4=TYPE 5=FLAG
*                                     6=DX  7=DY  8=LENG
*AUTEURS
*VERSION ORIGINALE - Y. BOURASSA FEV  91
*REVISION      001   "      "    JUIN 92 FIX BUB DECODAGE DE DATE
*              002   "      "    JAN  93 FIX BUB DANS CALCUL DE LA FENETRE
*LANGUAGE FTN77
#include "maxprms.cdk"
#include "desrs.cdk"
#include "fiches.cdk"
  
      EXTERNAL JULM, JHMENM
      INTEGER  JULM, JHMENM, I, J, K, W
      LOGICAL  SAUT, WINDOW

*     saut = .true. quand cle = interval avec saut.
      SAUT   = (CLE(1).EQ.-2) .OR. ((N.GT.1).AND.(CLE(2).EQ.-2)) 
     X                        .OR. ((N.GT.2).AND.(CLE(3).EQ.-2))
*     window = .true. quand la fenetre est definie et cle=date sans delta
*     ie: AAMMJJ, [AAMMJJ,HHMM], [@,AAMMJJ,HHMM], [AAMMJJ,HHMM.@]
      WINDOW = (FENETRE.NE.0) .AND. (M.EQ.3) .AND. (N.LT.4)
      IF(SAUT .OR. WINDOW) THEN
         REQ( 1,M,NREQ) = 0
         IF(M .NE. 3) THEN
            REQ( 2,M,NREQ) = 999999999
         ELSE
*           TOP = (DEC 31 2049 - 23H - 59 MIN)
            REQ(2,M,NREQ) = TOP
         ENDIF
         REQ( 3,M,NREQ) = 1
         REQ(11,M,NREQ) = -1
      ENDIF  

*     DECODAGE DE LA DATE  
      IF(M .EQ. 3) THEN
         IF( WINDOW ) THEN
*           CALCULE LA EXTREMITES D'UNE FENETRE DANS LE TEMPS
            W = JHMENM( FENETRE )
            J = 0
*           CHANGE REQUETE DE TYPE [@,AAMMJJ,HHMM] OU [AAMMJJ,HHMM,@] OU
*                                  [AAMMJJ,HHMM]
            IF( SAUT ) THEN
               IF(CLE(1) .EQ. -2) THEN
*                 LA FENETRE SE FINIT A [@,AAMMJJ,HHMM]
                  IF(N .EQ. 3) J = CLE(3)
                  REQ(2,M,NREQ)  = JULM(CLE(2), J) 
                  REQ(1,M,NREQ)  = REQ(2,M,NREQ) - w + 1
                  IF(INFORM) WRITE(6,*)' LA FENETRE DE',W,' MINUTE(S)',
     X               ' FINIT A ',REQ(2,M,NREQ)
               ELSE
*                 LA FENETRE SE DEBUTE A [AAMMJJ,HHMM,@]
                  IF(N .EQ. 3) J = CLE(2)
                  REQ(1,M,NREQ)  = JULM(CLE(1), J) 
                  REQ(2,M,NREQ)  = REQ(1,M,NREQ) + w - 1
                  IF(INFORM) WRITE(6,*)' LA FENETRE DE',W,' MINUTE(S)',
     X               ' DEBUTE A',REQ(1,M,NREQ) 
               ENDIF
            ELSE
*              [AAMMJJ,HHMM]  = LE CENTRE DE LA FENETRE
               IF(N .EQ. 2) J = CLE(2)
               I              = JULM(CLE(1),J) 
               REQ(1,M,NREQ)  = I - INT(FLOAT(W)/2.+0.1)
               REQ(2,M,NREQ)  = REQ(1,M,NREQ) + W - 1
               IF(INFORM) WRITE(6,*)' LA FENETRE DE',W,' MINUTE(S)',
     X            ' A POUR CENTRE',I
            ENDIF  
         ELSEIF( SAUT ) THEN
*           REQUETE AVEC SAUT SANS FENETRE?
            IF(CLE(1) .EQ. -2) THEN
*              CHANGE REQUETE DE TYPE  [@,AAMMJJ,HHMM,DELTA,JJHHMM]
               IF(N .EQ. 2) THEN
                  J = 0
               ELSE
                  J = CLE(3)
                  IF(N .GT. 3) REQ(3,M,NREQ) = JHMENM(CLE(N))
               ENDIF
               REQ(2,M,NREQ) = JULM(CLE(2), J)
               IF(INFORM) WRITE(6,*)' LA PERIODE FINIT A',
     X            CLE(2),J,' DELTA=',REQ(3,M,NREQ),' MINUTE(S)'
            ELSE
*              CHANGE REQUETE TYPE  [AAMMJJ,HHMM,@...]
               REQ(1,M,NREQ) = JULM(CLE(1), CLE(2))
               IF(N .EQ. 3) THEN
*                 REQUETE TYPE  [AAMMJJ,HHMM,@]
*                 FENETRE DEBUTANT LE AAMMJJ HHMM
                  IF(INFORM) WRITE(6,*)' LA PERIODE DEBUTE A',
     X               (CLE(I),I=1,2),' DELTA=',REQ(3,M,NREQ),
     X               ' MINUTE(S)'
               ELSE
                  IF(N .EQ. 4) THEN
*                    REQUETE TYPE  [AAMMJJ,HHMM,@,AAMMJJ]
                     REQ(2,M,NREQ) = JULM(CLE(4), 0)
                  ELSE
*                    REQUETE TYPE  [AAMMJJ,HHMM,@,AAMMJJ,HHMM...]
                     REQ(2,M,NREQ) = JULM(CLE(4), CLE(5))
                  ENDIF
*                 REQUETE TYPE  [AAMMJJ,HHMM,@,AAMMJJ,HHMM,DELTA,JJHHMM]
                  IF(N .GT. 5) REQ(3,M,NREQ) = JHMENM(CLE(N))
                  IF(INFORM) WRITE(6,*)' LA PERIODE DEBUTE A ',(CLE(I),
     X               I=1,2),' FINIT A',CLE(3),J,' DELTA=',REQ(3,M,NREQ),
     X               ' MINUTE(S)'
               ENDIF
            ENDIF
         ELSEIF(CLE(1) .EQ. -4) THEN
*           PRENDRE LA DATE DE LA PERIODE?
            IF(JOURS(4) .EQ. 0) THEN
               PRINT*,'PAS RENCONTRE DE DIRECTIVE PERIODE'
            ELSE
               IF( INFORM ) WRITE(6,*)' DATE = DATE OU PERIODE COMMUNE'
               REQ(11,M,NREQ) = JOURS(4)
               DO 10 I=1,3
   10             REQ(I,M,NREQ) = JOURS(I) 
            ENDIF
         ELSE
*           LISTE DE 1 A 5 DATE BURP
            IF(N .EQ. 1) THEN
               REQ(1,M,NREQ) = JULM(CLE(1), 0)
               K = 1
            ELSE
               K = 0
               DO 20 I=1,N-1,2
                  K = K+1
   20             REQ(K,M,NREQ) = JULM(CLE(I), CLE(I+1))
            ENDIF
            REQ(11,M,NREQ) = K
            IF( INFORM ) WRITE(6,600) (CLE(J),J=1,N)
  600       FORMAT(' DATE(S)=',5(I6,',',I4,3X))
         ENDIF
      ELSE
*        DECODE LAT, LON, TYPE, LNG, FLAGS
         IF(CLE(1) .EQ. -2) THEN
*           REQUETE DE TYPE  [@,-----]
            I = 2
         ELSE
*           REQUETE DE TYPE  [-----,@] OU [-----,-----,-----] 
            I = 1
         ENDIF
         K = 0
         DO 30 J=I,N
            IF(CLE(J) .GE. 0) THEN
               K = K+1
               REQ(K,M,NREQ) = CLE(J)
            ENDIF
   30       CONTINUE
         IF( .NOT.SAUT ) REQ(11,M,NREQ) = K
      ENDIF

*     excepte dans le cas des longitudes va toujours du plus
*     petit au plus grand
      IF(SAUT .and. (M.ne.2)) THEN
         IF(REQ(2,M,NREQ) .lt. REQ(1,M,NREQ)) THEN
            J = REQ(1,M,NREQ)
            REQ(1,M,NREQ) = REQ(2,M,NREQ)
            REQ(2,M,NREQ) = J
         ENDIF
      ENDIF


      RETURN
      END 
