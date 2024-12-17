       !-1- test de la procedure burp_reduce_block
       !-2- test de la procedure BURP_copy_header
       ! qui permet de copier le header d'in rapport vers un autre
       program PROG_TEST
          use BURP_MODULE

          IMPLICIT NONE
          character(len=60)                          :: cmd, path_in
          type(BURP_FILE)                            :: File_in
          type(BURP_RPT)                             :: Rpt_in,rapport
          type(BURP_BLOCK)                           :: Block_in,blk2
          integer                                    :: narg,iarg
          integer                                    :: error, ref_rpt,&
                                                         ref_blk
          path_in = '/data/cmdax8/afsd/hmd/2002031500_'

          ! Getting Command Line arguments
          !
          narg = Command_Argument_Count()
          do iarg = 1,narg
             Call Get_Command_Argument(iarg,cmd)
             path_in = cmd
          end do
          call BURP_INIT(rapport,Rpt_in)

          Call BURP_New(File_in,                               &
                    FILENAME     = path_in,                    &
                    MODE         = FILE_ACC_READ  ,            &
                    IOSTAT = error)
          Call BURP_New(rapport,                               &
                    alloc_space  = 1000,                       &
                    IOSTAT = error)
          Call BURP_TO_STDOUT(rapport)

          ! parsing reports
          !
          ref_rpt = 0
          do
              ref_rpt = BURP_Find_Report(File_in,              &
                  REPORT       = Rpt_in,                       &
                  SEARCH_FROM  = ref_rpt ,                     &
                  IDTYP        = 32 ,                          &
                  TEMPS        = 2300 ,                        &
                  STNID        = '74724' ,                     &
                  IOSTAT     = error)

               if (ref_rpt < 0) Exit
               Call BURP_TO_STDOUT(Rpt_in)
               Call BURP_copy_header(TO = rapport,FROM = rpt_in)
               Call BURP_TO_STDOUT(rapport)

               ! parsing blocks
               !
               ref_blk = 0
               do
                 ref_blk = BURP_Find_Block(Rpt_in,Block_in, &
                    SEARCH_FROM = ref_blk,                  &
                    IOSTAT = error)

                 if (ref_blk < 0) Exit
!                 Call BURP_TO_STDOUT(Block_in)
                 blk2 = block_in
               end do
          end do 
          Call BURP_TO_STDOUT(blk2)
          call burp_reduce_block(blk2,new_nele= 3,new_nval= 2,new_nt =1,IOSTAT=error) 
          if (error /= burp_noerr)  call handle_error()
          Call BURP_TO_STDOUT(blk2)
          

          ! Free-up
          !
          Call BURP_Free(Rpt_in)
          Call BURP_Free(Block_in)
          Call BURP_Free(File_in)
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
       end program PROG_TEST
