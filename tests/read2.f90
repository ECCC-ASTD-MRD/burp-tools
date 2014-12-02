      ! - ouverture d'un fichier (1) burp en mode lecture
      ! - checher les repports de (1) repondant au cles de recherche
      !   par exemple le code type 164 tovs et sta NOAA16 "STNID = ^NOAA16"
      ! - pour chaque rapport trouve, chercher le block 3-D
      ! - interroger le block pour  quelques unes de ses proprietes
      ! - trouver les elemnts 6002 (longitude) et 5002 (latitude)
      ! - imprimer le header de cahque enregistrement et les lat/lon 
      !   du block 3-D 
      ! - on cherche le total des observations
      !
      program READ2
        use BURP_MODULE

        IMPLICIT NONE
        character(len=60)       :: cmd, path_in
        type(BURP_FILE)         :: File_in
        type(BURP_RPT)          :: rpt_in
        type(BURP_BLOCK)        :: Block_in
        integer(kind = int_def) :: narg,iarg, error
        integer(kind = int_def) :: ref_rpt,ref_blk, &
                                   ind_lat,ind_lon,&
                                   i,j,k,tot_obs


        ! variales for pour l'objet block
        integer(kind = int_def) :: my_nele,&  ! nombre d'elemnts d'un block
                                   my_nt,  &  ! nbre valeurs pour chaque element
                                   my_nval    ! profondeur du bloc

        path_in = '/data/cmdax8/afsd/hmd/2003101500_' ! fichier a lire par defaut
        tot_obs = 0                                   ! total observations

        ! Getting Command Line arguments
        !
        narg = Command_Argument_Count()
        do iarg = 1,narg
           Call Get_Command_Argument(iarg,cmd)
           path_in = cmd
        end do

        Call BURP_New(File_in,                              &
                FILENAME           = path_in,               &
                MODE               = FILE_ACC_READ,         &
                IOSTAT             = error)
        if (error /= burp_noerr)  call handle_error()

         ! scanner les enregitrements
        ref_rpt = 0
        do
            ref_rpt = BURP_Find_Report(File_in,              &
                REPORT       = rpt_in,                       &
                SEARCH_FROM  = ref_rpt ,                     &
                IDTYP        = 164  ,                        &
                STNID        = "^NOAA16" ,                   &
                IOSTAT       = error)

                ! gerer les erreurs
                if (error /= burp_noerr)  call handle_error()

                ! on trouve ce qu'on cherche ou non
                if (ref_rpt < 0) Exit

                ! si station rejetee par AO on passe a l'enreg. suivant
                !
                if (IS_Burp_Flgs('STN_REJET_PAR_AO',REPORT=rpt_in)) cycle

                ! sortie vers le stdout du hesder du rapport
                !
                write(*,*) "<<<<< BURP_TO_STDOUT (REPORT) OUPUT >>>>>"
                Call BURP_TO_STDOUT(rpt_in)
                write(*,*) " "

                ! afficher les informations sur le flag du rapport
                call BURP_TO_STDOUT_Info_Flgs(REPORT=rpt_in)

                ! chercher les blocks dans le rapport
                ref_blk = 0
                do
                  ref_blk = BURP_Find_Block(rpt_in, &
                     BLOCK       = Block_in,        &
                     SEARCH_FROM = ref_blk,         &
                     IOSTAT = error)

                  ! gestion d'erreur
                  if (error /= burp_noerr)  call handle_error()

                  ! on trouve ou on ne trouve pas
                  if (ref_blk < 0) Exit

                  ! si ce n'est pas un block 3-D on passe au block suivant
                  if (.NOT.IS_Burp_btyp('3-D',BLOCK= Block_in)) cycle

                  ! afficher les info dur le btyp du block
                  call BURP_TO_STDOUT_Info_Btyp(BLOCK=Block_in)

                  ! chercher quelques proprietes du block
                  call BURP_Get_Property(Block_in, &
                            NELE = my_nele,        &
                            NT   = my_nt,          &
                            NVAL = my_nval, IOSTAT=error)
                  if (error /= burp_noerr)  call handle_error()

                  ! trouver les positions des elemnts
                  ! 
                  ind_lat= BURP_Find_Element(Block_in,5002,IOSTAT=error)
                  ind_lon= BURP_Find_Element(Block_in,6002,IOSTAT=error)

                  ! si on les trouve leurs indices sont ++
                  !
                  if ( (ind_lat > 0) .AND. (ind_lon>0)) then
                    !
                    ! on cherche leurs valeurs
                    do k =1,my_nt
                       do j =1, my_nval
                          write(*,FMT='(2(A7,F8.2))') &
                          "lat = ",BURP_Get_Rval(Block_in,ind_lat,j,k,error),&
                          " lon = ",BURP_Get_Rval(Block_in,ind_lon,j,k,error)
                       end do
                    end do
                    write(*,*) " "
                    write(*,*) "nombre obs dans le bloc = " // CVT(my_nt)
                    write(*,*) " "
                    
                    ! le module burp possde une fonction CVT, qui convertit un 
                    ! entier ou un reel en une chaine de caractere..utile pour
                    ! concatener pour les sorties...

                    ! mettre a jour le total des obsrvations
                    tot_obs = tot_obs + my_nt
                  endif
                end do
        end do
        write(*,*) " "
        write(*,*) "total des observations repondant a&
                    & IDTYP et STNID = " // CVT(tot_obs)
        write(*,*) " "

        Call BURP_Free(rpt_in)
        Call BURP_Free(Block_in)
        Call BURP_Free(File_in)


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
      end program READ2

