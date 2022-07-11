*** S/R RESTDEZ CONTROLE LA PORTEE DES DIRECTIVES APRES COPIE
      SUBROUTINE RESTDEZ
  
      IMPLICIT NONE 
  
*
*AUTEUR
*        Y. BOURASSA FEV 91
*REV 001 Y. BOURASSA OCT 91 LEGERE MODIF
*
*LANGUAGE   - FTN77 
*
#include "maxprms.cdk"
#include "desrs.cdk"
      INTEGER  I, J, K
  
      DO 10 I=1,NREQ
         SATISF(I) = 0
   10    CONTINUE

*     EFFACE TOUTE TRACE DES DESIRE/EXCLURE INUTILES
      IF(SAUV .GE. 0) THEN
         DO 20 J=SAUV+1, NREQ
            NARG(J) = 0 
            STNS(J) = 0
            DO 20 K=1,6
               DO 20 I=1, 11
                  REQ(I,K,J) = 0
   20             CONTINUE

*        CONTE LES EXCLUSIONS QUI RESTENT EN FORCE
         NEXC = 0
         NREQ = SAUV

         DO 30 I=1,SAUV
            IF(DESEXC(I) .EQ. 0) NEXC = NEXC + 1
   30       CONTINUE
      ENDIF

      RETURN
      END
