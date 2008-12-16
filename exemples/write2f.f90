      ! - ouverture d'un fichier (1) burp en mode ecriture 
      ! - creer un nouveau rapport
      ! - creer un nouveau block
      ! - le remplir et l'ajouter au nouveau rapport
      ! - ajouter le nouveau rapport au fichier  
      !
      program WRITE2
        use BURP_MODULE

        IMPLICIT NONE
        character(len=60)       :: cmd, path_out
        type(BURP_FILE)         :: File_out
        type(BURP_RPT)          :: Rpt_out
        type(BURP_BLOCK)        :: blk_out
        integer(kind = int_def) :: narg,iarg, error

        path_out = 'file_write2f' ! fichier a creer par defaut

        ! Getting Command Line arguments
        !
        narg = Command_Argument_Count()
        do iarg = 1,narg
           Call Get_Command_Argument(iarg,cmd)
           path_out = cmd
        end do

        ! new file for writing 
        !
        Call BURP_New(File_out,                             &
                 FILENAME     = path_out,                   &
                 MODE         = FILE_ACC_CREATE,            &
                 IOSTAT       = error)
        if (error /= burp_noerr)  call handle_error()

        ! create New Report params
        !
        Call BURP_New(Rpt_out,               &
                 Alloc_Space = 10000,        &
                 STNID = "74724",  dx = 100, &
                 IDTYP      = 32,            &
                 IOSTAT=error)
        if (error /= burp_noerr)  call handle_error()

        ! initialize the new report  for writing
        !
        call BURP_INIT_REPORT_WRITE(File_out,Rpt_out,  IOSTAT=error)
        if (error /= burp_noerr)  call handle_error()

        !
        ! create New block with params
        ! 2 elements with 2 values per element
        !
        Call BURP_New(blk_out,                    &
                 NELE       = 2, NVAL = 2, NT = 1,&
                 bfam = 12,  BDESC = 0,           & 
                 BKNAT= 2, BKTYP =70,BKSTP=0,DATYP=6,&
                 IOSTAT = error)
        if (error /= burp_noerr)  call handle_error()

        ! afficher info sur le btyp du block
        !
        call BURP_TO_STDOUT_Info_BTYP(BLOCK=blk_out)
        if (error /= burp_noerr)  call handle_error()

        ! setting block elements
        !
        call BURP_Set_Element(blk_out,NELE_IND = 1,ELEMENT =7004, &
                             IOSTAT=error)
        call BURP_Set_Element(blk_out,NELE_IND = 2,ELEMENT =11001,&
                             IOSTAT=error)

        ! setting  elements values
        !
        Call BURP_Set_Rval(blk_out,&
                NELE_IND= 1,NVAL_IND =1,NT_IND= 1,RVAL= 92500.0,IOSTAT=error)
        Call BURP_Set_Rval(blk_out,&
                NELE_IND= 1,NVAL_IND =2,NT_IND= 1,RVAL= 85000.0,IOSTAT=error)
        Call BURP_Set_Rval(blk_out,&
                NELE_IND= 2,NVAL_IND =1,NT_IND= 1,RVAL= 92600.,IOSTAT = error)
        Call BURP_Set_Rval(blk_out,&
                NELE_IND= 2,NVAL_IND =2,NT_IND= 1,rval= 0.000079,IOSTAT=error)

        ! writing block to report
        !
        Call BURP_Write_Block( Rpt_out, blk_out,&
                  ENCODE_BLOCK = .TRUE.,CONVERT_BLOCK = .true.,&
                  IOSTAT= error)
        if (error /= burp_noerr)  call handle_error()

        ! effacer les donnees du block, en gardant son enetete 
        !
        blk_out = .CLEAR.blk_out
        Call BURP_Write_Block( Rpt_out, blk_out,&
                  ENCODE_BLOCK = .FALSE.,CONVERT_BLOCK = .FALSE., IOSTAT= error)
        if (error /= burp_noerr)  call handle_error()
!!hb
!!hb        !  
!!hb        !
!!hb        blk_out = .MRQR.blk_out
!!hb        Call BURP_Set_tblval(blk_out,&
!!hb                NELE_IND= 2,NVAL_IND =2,NT_IND= 1,tblval= 4096,IOSTAT=error)
!!hb        Call BURP_Write_Block( Rpt_out, blk_out,&
!!hb                  ENCODE_BLOCK = .TRUE., IOSTAT= error)
!!hb        if (error /= burp_noerr)  call handle_error()
!!hb
!!hb        blk_out = .CLEAR.blk_out
!!hb        Call BURP_Write_Block( Rpt_out, blk_out,&
!!hb                  ENCODE_BLOCK = .TRUE., IOSTAT= error)
!!hb        if (error /= burp_noerr)  call handle_error()
        ! write the newreport to the new file
        !
        Call BURP_Write_Report(File_out,Rpt_out,IOSTAT = error)
        if (error /= burp_noerr)  call handle_error()

        ! Clean-up
        !
        Call BURP_Free(Rpt_out,IOSTAT=error)
        Call BURP_Free(blk_out,IOSTAT=error)
        Call BURP_Free(File_out,IOSTAT=error)
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


      end program WRITE2
