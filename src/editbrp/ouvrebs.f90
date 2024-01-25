!!! S/R OUVREBS OUVRE DES FICHIERS BURP
      SUBROUTINE OUVREBS( DN , SEQ) 
  
      use app
      IMPLICIT      NONE
      CHARACTER*(*) DN(*)
      LOGICAL SEQ
  
!ARGUMENTS
!ENTREE DN    -  NOMS DES FICHIERS 25@59 (SOURCE)
!
!AUTEUR - Y. BOURASSA SEP 90
!REV 001  "     "     OCT 90 VERSION QLXINS, STANDARD 90
! "  002  "     "      "  91 MRFNBR POUR NUMBLKS
! "  003  "     "     DEC 91 QDFERR
! "  004  "     "     DEC 91 OUVREBF POUR FUSION, 1@15 SOURCES
! "  005  "     "     MAI 92 CHANGE QDFERR POUT PRINT!.
!                            FCLOS SI SOURCE INEXISTANT
! "  006  M. Lepine   MAI 98 Ajout de l'option seq pour fichier destination
!LANGUAGE FTN77
#include "maxprms.cdk"
#include "char.cdk"
#include "fiches.cdk"
!!
      EXTERNAL FERMBD, MRFOPN, MRFNBR, FCLOS, NUMBLKS,      &
               FERMBF, FERMBS, MRFCLS, MRFVOI, FNOM, MRFAPP  
      INTEGER  MRFOPN, MRFNBR, MRFCLS, FNOM, NUMBLKS,       &
               MRFVOI, FCLOS,  I, J, K, MRFAPP

!     FERMER LES FICHIERS DEJA OUVERTS?  
      IF(NFO .GT. 0) THEN
!        SI "LE" FICHIER A OUVRIR DEJA OUVERT 
         IF( (NFO*NFS.eq.1) .and. (DN(1).eq.NS(1)) ) THEN
            call App_Log(APP_INFO,'ouvrebs: File arleady opened')
            RETURN
         ELSE
            CALL FERMBS
         ENDIF   
         DO 10 J=1,NFS
!           SI LE FICHIER A OUVRIR DEJA OUVERT COMME FICHIER DE
!           SORTIE OU FUSION 
            IF(DN(J) .eq. ND) CALL FERMBD
            IF(DN(J) .eq. NF) CALL FERMBF
 10         CONTINUE
      ENDIF

!     LES FICHIERS EXISTENT-ILS?
      I = 0
      DO 20 K=1,NFS
         IF( FNOM(SOURCES(NFO+1),trim(s_prefix_path)//trim(DN(K)),'RND+OLD',0) .ne. 0) THEN
            J = FCLOS( SOURCES(NFO+1) )
         ELSE
            IF( NUMBLKS(SOURCES(NFO+1)) .EQ. 0) THEN
               J = FCLOS( SOURCES(NFO+1) )
               write(app_msg,*) 'ouvrebs: File ',trim(DN(K)),'is empty'
               call App_Log(APP_INFO,app_msg)
            ELSE
               NFO     = NFO + 1
               NS(NFO) = DN(K)
               I       = I + MRFOPN( SOURCES(NFO), 'READ')
               IF( VS )  J = MRFVOI( SOURCES(NFO) )
            ENDIF
         ENDIF
  20    CONTINUE
!     SI LES FICHIERS SONT VIDES
      RETURN

      ENTRY OUVREBD( DN, SEQ )

!     OUVRE UN FICHIER DESTINATION 

!     SI DEJA OUVERT
      IF(DN(1).eq.ND .and. ND.ne.' ') THEN
         call App_Log(APP_INFO,'ouvrebs: File already opened')
         RETURN
      ELSEIF( OUVD ) THEN
!        SI UN AUTRE DESTINATION DEJA OUVERT
         CALL FERMBD
      ENDIF   
!     EN MODE STATS SI FICHIER DESTINATION PAS DONNE
      ND    = DN(1)

      STATS = ND .EQ. ' ' 
      IF( STATS ) THEN
         call App_Log(APP_INFO,'ouvrebs: EDITBRP is in STATS mode, no copy will occur')
      ELSE
!        SI FICHIER DES FUSUONS = DESTINATION
         IF(ND.EQ.NF .AND. OUVF) CALL FERMBF
!        SI SOURCE = DESTINATION ABORT
         DO 40 I=1,NFO
            IF(ND .EQ. NS(I)) call App_Log(APP_ERROR,'ouvrebs: Destination = Source')
   40       CONTINUE

!        OUVRE LE FICHIER DESTINATION
         IF (SEQ) THEN
            I = FNOM(3, trim(d_prefix_path)//trim(ND), 'BURP+SEQ', 0)
         ELSE
            I = FNOM(3, trim(d_prefix_path)//trim(ND), 'BURP+RND', 0)
         ENDIF
         I = NUMBLKS(3) 
         IF(I .LE. 0) THEN
            J = MRFOPN(3, 'CREATE')
         ELSE
            J = MRFOPN(3, 'APPEND')
            IF (SEQ) THEN
               I = MRFAPP(3)
            ENDIF
         ENDIF
         OUVD   = .TRUE.
         RENDUA = LIMITE
         COPIES = 0
      ENDIF
      RETURN

      ENTRY OUVREBF( DN , SEQ )
!     OUVRE UN FICHIER BURP POUR FUSIONNER LES SORTIES

!     SI DEJA OUVERT
      IF(DN(1).EQ.NF .AND. OUVF) RETURN

      IF( OUVF ) CALL FERMBF
      NF = DN(1)

!     SI UN DES SOURCES = FUSION ABORT
      DO 50 I=1,NFS
         IF(NF .EQ. NS(I)) call App_Log(APP_ERROR,'ouvrebs: Source = Fusion file')
   50    CONTINUE

!     SI FUSION = DESTINATION 
      IF(NF.EQ.ND .AND.OUVD) call App_Log(APP_WARNING,'ouvrebs: Destination = Fusion file')

!     OUVRE LE FICHIER DES FUSIONS
      I = FNOM(2, trim(d_prefix_path)//trim(NF), 'RND', 0)
      IF(MRFNBR(2) .LT. 0) THEN
         I = MRFOPN(2, 'CREATE')
      ELSE
         I = MRFOPN(2, 'APPEND')
      ENDIF
      OUVF = .TRUE.
 
      RETURN
      END 
