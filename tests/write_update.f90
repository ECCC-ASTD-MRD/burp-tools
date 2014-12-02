      ! - ouverture d'un fichier (1) burp en mode ajout 
      ! - chercher les repports de (1) repondant aux cles de recherches
      ! - pour chaque rapport trouve faire un resie pour ajouter des block 
      ! - creer un nouveau bloc de donnees,  l'ajouter au  rapport
      ! - mettre a jour le fichier avec le rapport modifie 
      ! - dans cet exemple est montre comment redimensionner un block
      !   selon les 3 diemnsions NELE, NVAL et NT
      ! - faire une copie d'un block -affectation de block -
      ! - l'ajouter au  rapport
      !
      ! Cet exemple montre aussi 
      ! l'utilsatio de la procedure BURP_RESIZE_REPORT 
      ! l'utilsation BURP_WRITE_REPORT() avec le mot cle UPDATE = .TRUE. pour
      ! ecraser l'encien rapport et s'ajouter au fichier burp - IL s'ajoute
      ! a la fin - UPDATE =. FALSE. ou s'il n'est pas rentre, il n'ecrase pas
      ! le rapport trouve , il se met a la fin .

      ! 2 CONSEQUENCES IMPORTANTES:
      !   - le fichiers ou il ya ecrasements de rapports subissent
      !   - generalemnt une CURE  avec REFLEX pour ne 
      !     recuperer que les enregistrements non effaces.
      !     - voir RPN REFLEX - ou man reflex.
      !   - dans l'exemple, etre tres prudent  quand il s'agit de la mise jour
      !     dans une boucle qui fait la recherche de rapports avec des cles.
      !     Si dans la mise a jour la cle de recherche n'est pas modifie pour
      !     le rapport que l'on modifie( dans cet exmple -j'ai garde l'entete-) 
      !     l'enregistrement risque d'etre retrouve et une autre operation lui
      !     serait appliqueea, car il aura une autre reference--
      !     et c'est une boucle INFINIE--
      !     C'EST POURQUOI dans CET EXEMPLE , il ya la partie A ET B 
      !     ou on on opere que sur les enregstrements dja refereences.
      !     Et on utilse  aussi la procedure BURP_GET_REPORT pour pointer
      !     exactemet un enregistremen connaissant sa refeence.t


      program WRITE_UPDATE 

        use BURP_MODULE

        IMPLICIT NONE
        character(len=60)       :: cmd, path_in
        type(BURP_FILE)         :: File_in
        type(BURP_RPT)          :: Rpt_in,cp_rpt
        type(BURP_BLOCK)        :: blk_out,cp_blk
        integer(kind = int_def) :: narg,iarg, error
        integer(kind = int_def) :: ref_rpt,rpt_size,handle,r_size

        integer,         dimension(:), allocatable :: addresses
        integer(kind = int_def)                    :: compteur,k
        integer(kind = int_def)                    :: nb_rpts

        path_in = 'file_to_update'

        ! Getting Command Line arguments
        !
        narg = Command_Argument_Count()
        do iarg = 1,narg
           Call Get_Command_Argument(iarg,cmd)
           path_in = cmd
        end do



        Call BURP_New(File_in,                              &
                FILENAME           = path_in,               &
                MODE               = FILE_ACC_APPEND,       &
                IOSTAT             = error)
        if (error /= burp_noerr)  call handle_error()

        ! chercher le nbre de rapport valides 
        !
        Call BURP_Get_Property(File_in,NRPTS = nb_rpts)
  
        ! allouer l'espace en consequence
        !
        Allocate(addresses(nb_rpts))
        addresses(:) = 0; compteur     = 0


        ! PARTIE A 

        ! scanning reports
        ! pour chercher les adresses
        ref_rpt = 0
        do
            ref_rpt = BURP_Find_Report(File_in,              &
                REPORT       = Rpt_in,                       &
                SEARCH_FROM  = ref_rpt ,                     &
                IDTYP        = 32  ,                         &
                TEMPS        = 2300 ,                        &
                STNID        = '74724' ,                     &
                IOSTAT       = error)
                if (error /= burp_noerr)  call handle_error()

                if (ref_rpt < 0) Exit
                ! incrementer le nbre de rapports trouves
                !
                compteur = compteur + 1
  
                 
                ! remplir le tableau d'adress 
                !
                Call BURP_Get_Property(Rpt_in,               &     
                          HANDLE = addresses(compteur), IOSTAT=error)

                if (error /= burp_noerr)  call handle_error()
        end do

        ! PARTIE B PARTIE B 

        ! aller  mettre a jour les rapports en question
        ! 
        if (compteur > 0 ) then
           do k = 1, compteur
               Call  BURP_Get_Report(File_in,              &
                     REPORT       = Rpt_in,                &
                     REF          = addresses(k),          &
                     IOSTAT       = error)
                   if (error /= burp_noerr)  call handle_error()
                   call BURP_Get_PROPERTY(RPT_IN,HANDLE=handle,&
                                NSIZE = r_size)
                   write(*,*) 'handle =',handle,'addresse=',addresses(k)
                   write(*,*) 'size =', r_size

                   Call BURP_RESIZE_REPORT(RPT_IN,ADD_SIZE=2000,IOSTAT=error)
                   if (error /= burp_noerr)  call handle_error()
                   call BURP_Get_PROPERTY(RPT_IN,HANDLE=handle,&
                                NSIZE = r_size)
                   write(*,*) 'handle =',handle,'addresse=',addresses(k)
                   write(*,*) 'size =', r_size
   
   
                   ! creer New block params
                   !
                   Call BURP_New(blk_out,                    &
                            NELE       = 2, NVAL = 3, NT = 2,&
                            bfam = 712,  BDESC = 0,  & 
                            BKNAT= 7, BKTYP =70,BKSTP=0,&
                            IOSTAT = error)
                   if (error /= burp_noerr)  call handle_error()
   
   
                   ! setting block elements
                   !
                   call BURP_Set_Element(blk_out,nele_ind = 1,Element =7004, &
                                        IOSTAT=error)
                   call BURP_Set_Element(blk_out,nele_ind = 2,Element =11001,&
                                        IOSTAT=error)
                   ! setting  elements values
                   !
                   Call BURP_Set_Rval(blk_out,&
                           nele_ind= 1,nval_ind =1,nt_ind= 1,rval= 92500.0,IOSTAT=error)
                   Call BURP_Set_Rval(blk_out,&
                           nele_ind= 1,nval_ind =2,nt_ind= 1,rval= 85000.0,IOSTAT=error)
                   Call BURP_Set_Rval(blk_out,&
                           nele_ind= 2,nval_ind =1,nt_ind= 1,rval= 92600.,IOSTAT = error)
                   Call BURP_Set_Rval(blk_out,&
                           nele_ind= 2,nval_ind =2,nt_ind= 1,rval= 70000.,IOSTAT=error)
                   Call BURP_Set_Rval(blk_out,&
                           nele_ind= 2,nval_ind =3,nt_ind= 1,rval= 25000.,IOSTAT=error)
                   Call BURP_Set_Rval(blk_out,&
                           nele_ind= 2,nval_ind =3,nt_ind= 2,rval= 3000.,IOSTAT=error)
                 

                   ! faire une copie du block precedent
                   cp_blk = blk_out

                   ! redimensionner pour ajouter un element
                   ! 1 val pour tous les elemnst et un groupe NELE X NVAL)


                   Call BURP_RESIZE_BLOCK(cp_blk,ADD_NELE = 1,&
                                                 ADD_NVAL = 1,&
                                                 ADD_NT   = 1,IOSTAT =error)
                   if (error /= burp_noerr)  call handle_error()

                   ! on peut affecter les dimensions separement
                   ! Surtout Checker error
                   !
                   ! Call BURP_RESIZE_BLOCK(cp_blk,ADD_NELE = 1,IOSTAT =error)
                   ! Call BURP_RESIZE_BLOCK(cp_blk,ADD_NT = 1,IOSTAT =error)
                   ! Call BURP_RESIZE_BLOCK(cp_blk,ADD_NVAL = 1,IOSTAT =error)
                   !

                   ! setter element ajoute
                   call BURP_Set_Element(cp_blk,nele_ind = 3,Element =11004,&
                                        IOSTAT=error)
                   if (error /= burp_noerr)  call handle_error()


                   ! writing blocks to reports
                   !
                   Call BURP_Write_Block( Rpt_in, blk_out,&
                             ENCODE_BLOCK = .TRUE., CONVERT_BLOCK = .FALSE.,&
                             IOSTAT= error)
                   if (error /= burp_noerr)  call handle_error()

                   Call BURP_Write_Block( Rpt_in, cp_blk,&
                             ENCODE_BLOCK = .TRUE., CONVERT_BLOCK = .FALSE.,&
                             IOSTAT= error)
                   if (error /= burp_noerr)  call handle_error()
   
   
                   ! write the new report the new file
                   !
                   call BURP_Set_Property(Rpt_in,DATE=20011201)
                   Call BURP_Write_Report(File_in,Rpt_in,UPDATE =.TRUE.,IOSTAT = error)
                   if (error /= burp_noerr)  call handle_error()
           end do
         endif

        ! Clean-up
        !

        Deallocate(addresses)
        Call BURP_Free(Rpt_in,IOSTAT=error)
        Call BURP_Free(blk_out,B2= cp_blk,IOSTAT=error)
        Call BURP_Free(File_in,IOSTAT=error)


        ! internal procedures
        !
        CONTAINS

        subroutine handle_error()
        !
        implicit none
          write(*,*) BURP_STR_ERROR()
          write(*,*) "history"
          Call BURP_STR_ERROR_HISTORY()
          Deallocate(addresses)
          Call BURP_Free(Rpt_in)
          Call BURP_Free(blk_out,B2= cp_blk)
          Call BURP_Free(File_in)
          stop
        end subroutine


      end program WRITE_UPDATE
