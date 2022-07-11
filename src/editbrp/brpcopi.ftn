*** S/P BRPCOPI - APPELEE VIA DIRECTIVE BRPCOPI(S, D, F) 
*                 OUVRE LES FICHIERS AU BESOIN, APPEL A BARF
  
      SUBROUTINE BRPCOPI(INPT, OUPT, FUSN)
  
      IMPLICIT NONE 
      INTEGER  INPT(*), OUPT(*), FUSN(*)
*
*ARGUMENTS
*  ENTRE    - INPT  - DN  FICHIER SOURCE
*    "      - OUPT  - DN     "    DESTINATION
*    "      - FUSN  - DN     "    FUSION
*AUTEUR
*VERSION ORIGINALE - Y. BOURASSA   - JAN 91
*REVISION 001        "     "       - DEC 91 UTILISATION DE QDFERR
*         002        "     "       - DEC 91 FICHIER DE FUSION
*         003        "     "       - FEV 92 BUG APPELS A OUVREBS-D-F
*LANGAGE  - FTN77
*
#include "maxprms.cdk"
#include "fiches.cdk"
#include "char.cdk"
      EXTERNAL ARGDIMS, OUVREBS, OUVREBD, OUVREBF, RESTDEZ,
     X         SPOOL,   BARF,   FERMBS
      INTEGER  ARGDIMS, I

*     DIRECTION A PRENDRE SELON LE NOMBRE D'ARGUMENTS PASSES
      GO TO(30, 20, 10) NP

*     OUVERTURE DU FICHIER FUSION
   10 IF(FUSN(1) .NE. -1) THEN
         WRITE(NX, LIN128) (FUSN(I), I=1,ARGDIMS(3))
         CALL OUVREBF( NX ,.false. )
      ENDIF

*     OUVERTURE DU FICHIER DESTINATION
   20 IF(OUPT(1) .NE. -1) THEN
         WRITE(NX, LIN128) (OUPT(I), I=1,ARGDIMS(2))
         CALL OUVREBD( NX ,.false. )
      ENDIF

*     OUVERTURE DU FICHIER  SOURCE 
   30 IF(INPT(1) .NE. -1) THEN
         CALL FERMBS
         NFS = 1
         WRITE(NX, LIN128) (INPT(I), I=1,ARGDIMS(1))
         CALL OUVREBS( NX , .false. )
      ENDIF
 
*     INTERPRETATION DES DIRECTIVES & COPIE FICHIER(S) BURP
      CALL SPOOL

*     CONTROLE DE LA PORTEE DES DIRECTIVES
      CALL RESTDEZ
      RETURN

*     OUVERTURE DU FICHIER  SOURCE
      entry NEWSRC( inpt )
      NFS = 1
      WRITE(NX, LIN128) (INPT(I), I=1,ARGDIMS(1))
      CALL OUVREBS( NX , .false. )
      return

*     OUVERTURE DU FICHIER DESTINATION
      entry NEWDST( oupt )
      WRITE(NX, LIN128) (oupt(I), I=1,ARGDIMS(1))
      CALL OUVREBD( NX , .false. )
      return

*     OUVERTURE DU FICHIER FUSION
      entry NEWFUS( fusn )
      WRITE(NX, LIN128) (fusn(I), I=1,ARGDIMS(1))
      CALL OUVREBF( NX , .false. )
      return
      END 
