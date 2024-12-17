      ! - ouverture d'un fichier (1) burp en mode ecriture
      ! - creer un nouveau rapport
      ! - creer un nouveau block
      ! - le remplir et l'ajouter au nouveau rapport
      ! - creer une copie d'un block -montrer l'affectation de bloxk
      ! - l'ajouter au nouveau rapport
      ! - creer un block marqueur - operateur .MRQR.
      !   en fait il fait une copie d'un block, pour le quel
      !   il ajoute 200000 pour les valeurs des elemnts
      !   et flague le block - btyp- comme marqueur.
      !   si le block pour le quel on applique l'operateur est deja
      !   bloxk marqueu, aucune action n;est faite
      !   les valeurs entieres de TBLVAL du block sont remises a defaut a -1
      ! - le remplir et l'ajouter au nouveau rapport
      ! - ajouter le nouveau rapport au fichier
      !
      program WRITE3
        use BURP_MODULE

        IMPLICIT NONE
        character(len=60)       :: cmd, path_out
        type(BURP_FILE)         :: File_out
        type(BURP_RPT)          :: Rpt_out
        type(BURP_BLOCK)        :: blk_out,&
                                   copy_of_blk_out,&
                                   blk_mrq_of_blk_out

        integer(kind = int_def) :: narg,iarg, error
        integer(kind = int_def) :: rpt_size

        path_out = 'file_write3' ! fichier a creer par defaut

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
                 BKNAT= 2, BKTYP =70,BKSTP=0,     &
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
                NELE_IND= 2,NVAL_IND =1,NT_IND= 1,RVAL= 92600.0,IOSTAT = error)
        Call BURP_Set_Rval(blk_out,&
                NELE_IND= 2,NVAL_IND =2,NT_IND= 1,rval= 70000.0,IOSTAT=error)

        ! writing block to report
        !
        Call BURP_Write_Block( Rpt_out, blk_out,&
                  ENCODE_BLOCK = .TRUE., CONVERT_BLOCK = .TRUE.,&
                  IOSTAT= error)
        if (error /= burp_noerr)  call handle_error()

        ! copy de block
        copy_of_blk_out = blk_out

        ! l'ecrire dans le rapport
        Call BURP_Write_Block( Rpt_out, copy_of_blk_out,&
                  IOSTAT= error)
        if (error /= burp_noerr)  call handle_error()

        ! block_marqueur
        blk_mrq_of_blk_out = .MRQR.blk_out

        ! le remplir
        ! setting  elements values
        ! pour les block marquerurs SETTER directeemnt le tableau
        ! des entiers TBLVAL
        !
        Call BURP_Set_TBLVAL(blk_mrq_of_blk_out,&
                NELE_IND= 1,NVAL_IND =1,NT_IND= 1,TBLVAL= 512,IOSTAT=error)
        Call BURP_Set_TBLVAL(blk_mrq_of_blk_out,&
                NELE_IND= 1,NVAL_IND =2,NT_IND= 1,TBLVAL= 4096,IOSTAT=error)
        Call BURP_Set_TBLVAL(blk_mrq_of_blk_out,&
                NELE_IND= 2,NVAL_IND =1,NT_IND= 1,TBLVAL= 4096,IOSTAT = error)

        ! cette valeur n'est pas sette
        ! pour les valeurs par defaut qui est egale a -1

        ! Call BURP_Set_TBLVAL(blk_mrq_of_blk_out,&
        !        NELE_IND= 2,NVAL_IND =2,NT_IND= 1,TBLVAL= 4096,IOSTAT=error)


        ! ecrire le block dans le rapport
        Call BURP_Write_Block( Rpt_out, blk_mrq_of_blk_out,&
                  ENCODE_BLOCK = .TRUE., IOSTAT= error)
        if (error /= burp_noerr)  call handle_error()



!        call BURP_GET_PROPERTY(RPT_OUT,NSIZE=rpt_size,IOSTAT=error)
!        if (error /= burp_noerr)  call handle_error()
!        write(*,*) "rpt_size",rpt_size
!        Call BURP_RESIZE_REPORT(RPT_OUT,ADD_SIZE=2000,IOSTAT=error)
!        if (error /= burp_noerr)  call handle_error()
!        call BURP_GET_PROPERTY(RPT_OUT,NSIZE=rpt_size,IOSTAT=error)
!        if (error /= burp_noerr)  call handle_error()
!        write(*,*) "rpt_size",rpt_size


        ! write the newreport to the new file
        !
        Call BURP_Write_Report(File_out,Rpt_out,IOSTAT = error)
        if (error /= burp_noerr)  call handle_error()

        ! Clean-up
        !
        Call BURP_Free(Rpt_out,IOSTAT=error)
        Call BURP_Free(blk_out,               &
                       B2 =copy_of_blk_out,   &
                       B3= blk_mrq_of_blk_out, IOSTAT=error)
        Call BURP_Free(File_out,IOSTAT=error)

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


      end program WRITE3
