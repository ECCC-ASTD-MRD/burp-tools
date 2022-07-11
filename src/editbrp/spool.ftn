*** S/R SPOOL IMPRIME L'INTERPRETATION DES DIRECTIVES SI EN MODE INFORM
      SUBROUTINE SPOOL
  
      IMPLICIT NONE 
  
*     AUTEUR - Y. R. BOURASSA FEV 91
  
*     LANGUAGE FTN77
#include "maxprms.cdk"
#include "desrs.cdk"
#include "fiches.cdk"
#include "char.cdk"

      EXTERNAL QDFERR, BDESIRE, BARF
      INTEGER  QDFERR, I, J, K, L, M

*     si fichiers sources ou destination pas accessible  
      IF(NFS .eq. 0) THEN
         I = QDFERR('SPOOL','FICHIER SOURCE INCONNU', 6, 48)
         RETURN
      ENDIF

*     NOTE UNE TENTATIVE DE COPIE
      ESAIS = .TRUE.
  
*     SI PAS DE DIRECTIVES DESIRE OU EXCLURES 
      IF(NREQ .eq. 0) THEN
         IF(.NOT. STATS) PRINT*,'ON DEMANDE TOUT LE FICHIER'
         EXPRESS = .TRUE.
         GO TO 30
      ENDIF

*     SI DIRECTIVES EXCLURES SEULEMENT
      IF(NREQ. eq. NEXC) THEN
         NP = 1
         CALL BDESIRE(-1)
      ENDIF

*     ANALYSE DES REQUETES
      L = 0
      DO 20 K=1,NREQ
         J = STNS(K)
         IF(J .GT. 0) THEN
*           LISTE DE PARAMETRES ETIKETTES
            L = 1
            IF( INFORM ) WRITE(6,*) 'STNID=',(ID(I,K),I=1,J)
         ENDIF
         DO 10 J=1,8
            M = REQ(11,J,K)   
            IF(M .GT. 0) THEN 
               L = 1
*              LISTE DE PARAMETRES
               IF( INFORM ) THEN
                  IF(J.EQ.1) WRITE(6,*)'LAT  =',(REQ(I,1,K),I=1,M)
                  IF(J.EQ.2) WRITE(6,*)'LON  =',(REQ(I,2,K),I=1,M)
                  IF(J.EQ.3) WRITE(6,*)'JULM =',(REQ(I,3,K),I=1,M)
                  IF(J.EQ.4) WRITE(6,*)'TYP  =',(REQ(I,4,K),I=1,M)
                  IF(J.EQ.5) WRITE(6,*)'FLAG =',(REQ(I,5,K),I=1,M)
                  IF(J.EQ.6) WRITE(6,*)'DX   =',(REQ(I,6,K),I=1,M)
                  IF(J.EQ.7) WRITE(6,*)'DY   =',(REQ(I,7,K),I=1,M)
                  IF(J.EQ.8) WRITE(6,*)'LNG  =',(REQ(I,8,K),I=1,M)
               ENDIF
            ELSEIF(M .LT. 0) THEN
               L = 1
*              INTERVALE AVEC SAUT.
               IF( INFORM ) THEN
                  IF(J.EQ.1) WRITE(6,*)'LAT = ',REQ(1,1,K),' @ ',
     X                                 REQ(2,1,K),' DELTA ',REQ(3,1,K)
                  IF(J.EQ.2) WRITE(6,*)'LON = ',REQ(1,2,K),' @ ',
     X                                 REQ(2,2,K),' DELTA ',REQ(3,2,K)
                  IF(J.EQ.3) WRITE(6,*)'JULM= ',REQ(1,3,K),' @ ',
     X                                 REQ(2,3,K),' DELTA ',REQ(3,3,K)
                  IF(J.EQ.4) WRITE(6,*)'TYP = ',REQ(1,4,K),' @ ',
     X                                 REQ(2,4,K),' DELTA ',REQ(3,4,K)
                  IF(J.EQ.5) WRITE(6,*)'FLAG= ',REQ(1,5,K),' @ ',
     X                                 REQ(2,5,K),' DELTA ',REQ(3,5,K)
                  IF(J.EQ.6) WRITE(6,*)'DX  = ',REQ(1,6,K),' @ ',
     X                                 REQ(2,6,K),' DELTA ',REQ(3,6,K)
                  IF(J.EQ.7) WRITE(6,*)'DY  = ',REQ(1,7,K),' @ ',
     X                                 REQ(2,7,K),' DELTA ',REQ(3,7,K)
                  IF(J.EQ.8) WRITE(6,*)'LNG = ',REQ(1,8,K),' @ ',
     X                                 REQ(2,8,K),' DELTA ',REQ(3,8,K)
               ENDIF
            ENDIF
   10       CONTINUE
   20    CONTINUE

   30 CALL BARF   

      RETURN
      END 



















