      ! - ouverture d'un fichier (1) burp en mode lecture
      ! - ouverture d'un fichier (2) pour ajout /ou creation 
      ! - checher les repports de (1) repondant au criters voulus
      ! - enregistrer les rapports trouves dasn le nouveua fichier

      ! A la fin d'execution du programme, faire un liburp file_write1
      ! pour voir le resultat
      !
      program WRITE_BURP
        use BURP_MODULE

        IMPLICIT NONE
        character(len=60)       :: cmd, path_in
        type(BURP_FILE)         :: File_in,File_out
        type(BURP_RPT)          :: Rpt_in
        type(BURP_BLOCK)        :: Block_in
        integer(kind = int_def) :: narg,iarg, error
        integer(kind = int_def) :: ref_rpt, ref_blk


        path_in = '../toto' ! fichier a lire par defaut

        ! Getting Command Line arguments
        !
        narg = Command_Argument_Count()
        do iarg = 1,narg
           Call Get_Command_Argument(iarg,cmd)
           path_in = cmd
        end do



        ! ouvrir le fichier path_in en lecture
        Call BURP_New(File_in,                              &
                FILENAME           = path_in,               &
                MODE               = FILE_ACC_READ,         &
                IOSTAT             = error)
        if (error /= burp_noerr)  call handle_error()

        ! creer un fichier de detination
        Call BURP_New(File_out,                             &
                 FILENAME     = 'file_write1f',                  &
                 MODE         = FILE_ACC_APPEND,            &
                 IOSTAT       = error)
        if (error /= burp_noerr)  call handle_error()


        ! scanning reports
        !
        ref_rpt = 0
        do
            ref_rpt = BURP_Find_Report(File_in,              &
                REPORT       = Rpt_in,                       &
                SEARCH_FROM  = ref_rpt ,                     &
                IOSTAT       = error)

                if (error /= burp_noerr)  call handle_error()
                if (ref_rpt < 0) Exit

                ! enregister chaque rapport trouve dans le nouceau fichier 
                !
                ! REMARQUE: NE PAS APPELER la procedure
                ! BURP_INIT_REPORT_WRITE- avant de l'enregistrer
                ! dans un fichier
                Call BURP_Write_Report(File_out,Rpt_in,IOSTAT = error)
                if (error /= burp_noerr)  call handle_error()
        end do

        ! Clean-up
        !
        Call BURP_Free(Rpt_in,IOSTAT=error)
        Call BURP_Free(Block_in,IOSTAT=error)
        Call BURP_Free(File_in,F2 = File_out,IOSTAT=error)

        Call BURP_STR_ERROR_HISTORY()

        ! internal procedures
        !
        CONTAINS

        subroutine handle_error()
        !
        implicit none
          write(*,*) BURP_STR_ERROR()
          write(*,*) "history"
          Call BURP_STR_ERROR_HISTORY()
          stop
        end subroutine


      end program WRITE_BURP
