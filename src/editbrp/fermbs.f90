!   S/R FERMBS  LE FICHIER SOURCE
      SUBROUTINE FERMBS
  
      IMPLICIT      NONE
!
!AUTEURs
!VERSION ORIGINALE - Y. BOURASSA FEV 90
!REVISION   001    - "     "     DEC 91 ALLEGEMENT
!             2    - "     "     UTILISE NFO AU LIEU DE OUVREBS
!
!LANGUAGE FTN77
!
#include "maxprms.cdk"
#include "char.cdk"
#include "fiches.cdk"

      EXTERNAL MRFVOI, MRFCLS, XDFUSE, FCLOS
      INTEGER  MRFVOI, MRFCLS, XDFUSE, FCLOS, I, J

!     TRAITEMENT DU FICHIER SOURCE
      IF( NFO .GT. 0 ) THEN
         DO 10 I=1,NFO
            J     = MRFCLS( SOURCES(I) )
            J     = FCLOS(  SOURCES(I) )
            NS(I) = ' '
   10       CONTINUE
         NFO   = 0
      ENDIF
      RETURN
  
      ENTRY FERMBD
!     TRAITEMENT DU FICHIER DESTINATION 
      IF( OUVD ) THEN
         IF( OUVF ) I = XDFUSE(3, 2)
         IF(  VD  ) I = MRFVOI( 3 )
         I    = MRFCLS( 3 )
         I    = FCLOS(  3 )
         OUVD = .FALSE.
         ND   = ' '
      ENDIF
      RETURN

      ENTRY FERMBF
!     TRAITEMENT DU FICHIER DES FUSIONS 
      IF( OUVF ) THEN
         I    = MRFCLS( 2 )
         I    = FCLOS(  2 )
         OUVF = .FALSE.
         NF   = ' '
      ENDIF

      RETURN
      END 
