      ! - ouverture d'un fichier (1) burp en mode lecture
      ! - checher les repports de (1) repondant au cles de recherche
      ! - afficher le header de chacun  de ces rapports
      ! - pour chaque rapport trouve, chercher tous les blocks 
      ! - afficher vers le stdout  l'entete du block  et les donnees
      ! - fermeture de fichiers et liberation des ressources memoires
      !
      ! note :ce programme illustre comment faire
      ! une sortie comme l'application de JOSE GARCIA avec al commande 
      ! liburp "fichier_burp" -codtyp 32 -heure 2300
      !
      program LIKE_LIBURP
        use BURP_MODULE

        IMPLICIT NONE
        character(len=60)       :: cmd, path_in,prog
        type(BURP_FILE)         :: File_in
        type(BURP_RPT)          :: rpt_in
        type(BURP_BLOCK)        :: Block_in
        type(BURP_BLOCK)        :: hb_blk 
        integer(kind = int_def) :: narg,iarg, error
        integer(kind = int_def) :: ref_rpt, ref_blk


        Call BURP_init(rpt_in)
        Call BURP_init(Block_in)
        Call BURP_init(hb_blk)
        Call BURP_init(File_in)

        path_in = '/data/cmdax8/afsd/hmd/2003101500_' ! fichier a lire par defaut

        ! Getting Command Line arguments
        ! l'utilisation de BURP_MODULE vous donne acces a ces 
        ! procedures.- inclure module reference --
        ! 
        narg = Command_Argument_Count()        ! nbre d'arguments
        Call Get_Command_Argument(0,prog)      ! nom du programme
        do iarg = 1,narg
           Call Get_Command_Argument(iarg,cmd) ! chercher les arguments
           path_in = cmd
        end do
        write (*,*) "votre programme est : ", prog
        write (*,*) "votre fichier est : ", path_in

        ! ouverture de fichier en mode lecture
        ! en initialisant la variable File_in
        ! de type BURP_FILE
        !
        Call BURP_New(File_in,                              &
                FILENAME           = path_in,               &
                MODE               = FILE_ACC_READ,         &
                IOSTAT             = error)
        if (error /= burp_noerr)  call handle_error()

        ! scanner les rapports
        ! ici on cherche a partie du debut du fichier
        ! ref_rpt donne l'adresse du rapport trouve
        ! on trouve rien si ref_rpt < 0
        !
        ref_rpt = 0
        do
            ! les rapports cherches sont ceux dont le codtyp est
            ! 32 et d'heure de validation 2300 
            !
            ref_rpt = BURP_Find_Report(File_in,              &
                REPORT       = rpt_in,                       &
                SEARCH_FROM  = ref_rpt ,                     &
!                STNID        = "MIPAS" ,                     &
!                IDTYP        = 32  ,                         &
!                TEMPS        = 2300,                         &
                IOSTAT       = error)

                ! toujours verifier si pas d'ereur
                !
                if (error /= burp_noerr)  call handle_error()

                ! on sort de la boucle do si on trouve rien
                !
                if (ref_rpt < 0) Exit

                ! on t imprime vers le stdout tout le header du rapport
                ! le format utlise est comme celui  du programme liburp
                ! de J.GARCIA http://iweb/~afsqjmg
                ! man liburp sur pollux pour details.
                !
                ! write(*,*) "<<<<< BURP_TO_STDOUT (REPORT) OUPUT >>>>>"
                Call BURP_TO_STDOUT(rpt_in)
                write(*,*) " "

                ! trouver tous les blocks
                !
                ref_blk = 0
                do
                  !
                  ref_blk = BURP_Find_Block(rpt_in, &
                     BLOCK       = Block_in,        &
                     SEARCH_FROM = ref_blk,         &
                     CONVERT      = .FALSE.,         &
                     GET          =.FALSE.,          &
                     IOSTAT = error)
                  if (error /= burp_noerr)  call handle_error()


                  ! si on trouve pas de blk on sort de la boucle
                  !
                  if (ref_blk < 0) Exit
                  Call BURP_Get_block(rpt_in, &
                     BLOCK       = Block_in,        &
                     REF         = ref_blk,         &
                     IOSTAT = error)


                  ! on peut envoyer tout l'entete du block, vers
                  ! le standart output. et les donness 
                  !
                  if (IS_Burp_Btyp('MRQR',BLOCK = Block_in) ) then
                     Call BURP_TO_STDOUT(Block_in,convert=.false.)
                  else
                     call BURP_CONVERT_BLOCK(Block_in, MODE=BUFR_to_MKSA)
                     Call BURP_TO_STDOUT(Block_in,convert=.true.)
                  endif
                end do
        end do

        ! Clean-up
        !

        ! les ressources pour manipuler les rapports
        !
        Call BURP_Free(rpt_in,IOSTAT=error)
        if (error /= burp_noerr)  call handle_error()

        ! les ressources pour manipuler les blocks
        !
        Call BURP_Free(Block_in,IOSTAT=error)
        Call BURP_Free(hb_blk,IOSTAT=error)
        if (error /= burp_noerr)  call handle_error()

        ! les ressources pour manipuler les fichiers
        ! fermeture de fichier
        !
        Call BURP_Free(File_in,IOSTAT=error)
        if (error /= burp_noerr)  call handle_error()

        !
        CONTAINS

        ! routine pour gerer les erreurs
        ! pour montrer les la fonction: BURP_STR_ERROR()
        ! qui nous renseigne sur la trace de l'erreur
        ! La procedure BURP_STR_ERROR_HISTORY() donne l'historique
        ! des erreurs si dans le programme on les apas intercepte.
        !
        subroutine handle_error()
        !
        implicit none
          write(*,*) BURP_STR_ERROR()
          write(*,*) "history"
          Call BURP_STR_ERROR_HISTORY()
          stop
        end subroutine


      end program LIKE_LIBURP

