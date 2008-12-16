      ! - ouverture d'un fichier (1) burp en mode lecture
      ! - checher les repports de (1) repondant au cles de recherche
      ! - interroger le rapport pour  quelques unes de ses proprietes
      ! - afficher le header de chacun  de ces rapports
      ! - manipulation sur le parametre RUNN
      ! - manipulation sur le parametre FLGS
      ! - pour chaque rapport trouve, chercher les blocks repondant au cles
      !   de recherche
      ! - manipuler les proprietes du BTYP du block
      ! - interroger le block pour  quelques unes de ses proprietes
      ! - afficher vers le stdout  l'entete du block  et les donnees
      ! - ferner les fichiers et liberer les ressources..memoire
      program READ1
        use BURP_MODULE

        IMPLICIT NONE
        character(len=60)       :: cmd, path_in
        type(BURP_FILE)         :: File_in
        type(BURP_RPT)          :: rpt_in
        type(BURP_BLOCK)        :: Block_in
        integer(kind = int_def) :: narg,iarg, error
        integer(kind = int_def) :: ref_rpt, ref_blk
        integer(kind = int_def) :: my_bknat,my_bktyp,my_bkstp

        ! variales pour proprietes de l'objet fichier
        integer(kind = int_def) :: my_rpts ! nombre de rapports dans fichier
        integer(kind = int_def) :: my_unit ! unite associe au fichier
        character(len=128)      :: my_file ! nom di fichiler

        ! variales pour l'objet rapport
        integer(kind = int_def) :: my_date, & ! pour la date du rapport
                                   my_idtyp   ! pour le codtyp du rapport
        character(len = 9)      :: my_station ! pour la station du rapport

        ! variales for pour l'objet block
        integer(kind = int_def) :: my_nele,&  ! nombre d'elemnts d'un block
                                   my_nt,  &  ! nbre valeurs pour chaque element
                                   my_nval,&  ! profondeur du bloc
                                   my_btyp

       path_in = '/users/dor/afsd/hmd/tmp/2003101500_' ! fichier a lire par defaut

        ! Getting Command Line arguments
        !
        narg = Command_Argument_Count()
        do iarg = 1,narg
           Call Get_Command_Argument(iarg,cmd)
           path_in = cmd
        end do

        ! ouverture de fichier en mode lecture
        ! en initialisant la variable File_in
        ! de type BURP_FILE
        !
        Call BURP_New(File_in,                              &
                FILENAME           = path_in,               &
                MODE               = FILE_ACC_READ,         &
                REAL_OPTNAME       = "MISSING",             &
                REAL_OPTNAME_VALUE = -77.77,                &
                CHAR_OPTNAME       = "MSGLVL",              &
                CHAR_OPTNAME_VALUE = "FATAL",               &
                IOSTAT             = error)
        if (error /= burp_noerr)  call handle_error()

        ! On peut recuperer a tout moment
        ! les proprietes de la variable File_in
        ! exemple: NRPTS nombre de rapports actifs
        ! dans le fichier burp.(mot cle NRPTS)
        ! le nom di fichier associe a FILE_IN
        ! est recuperable avec le mot cle FILENAME
        ! unite associe au fichier IO_UNIT
        !
        Call BURP_Get_Property(File_in,                     &
                NRPTS        = my_rpts,                     &
                FILENAME     = my_file,                     &
                IO_UNIT      = my_unit,                       &
                IOSTAT       = error)
        if (error /= burp_noerr)  call handle_error()

        ! si pas d'erreur, on ecrit  vers le stdout
        ! les valeurs trouvees
        !
        write(*,*) "<<<<< nom fichier, unite etc >>>>>"
        write(*,*) "nom du fichier      = ", trim(my_file)
        write(*,*) "unite associe       = ", my_unit
        write(*,*) "nombre de rapports  = ", my_rpts
        write(*,*) " "

        ! sortie vers stdout des proprietes
        ! de l'objet file_in
        !
        write(*,*) "<<<<< BURP_TO_STDOUT (File_OBJ) OUPUT >>>>>"
        Call BURP_TO_STDOUT(File_in)
        write(*,*) " "


        ! scanner les rapports
        ! ici on cherche a partie du debut du fichier
        ! ref_rpt donne l'adresse du rapport trouve
        ! on trouve rien si ref_rpt < 0
        !
        ref_rpt = 0
        do
            !les rapports cherches sont ceux dont le codtyp et
            ! 32 , temps de vaidatio de 2300 et de station
            ! 74724
            !
            ref_rpt = BURP_Find_Report(File_in,              &
                REPORT       = rpt_in,                       &
                SEARCH_FROM  = ref_rpt ,                     &
                IDTYP        = 146  ,                        &
                TEMPS        = 0000 ,                        &
                STNID        = "71231" ,                     &
                IOSTAT       = error)

                ! toujours verifier si pas d'ereur
                !
                if (error /= burp_noerr)  call handle_error()

                ! on sort de la boucle do si on trouve rien
                !
                if (ref_rpt < 0) Exit

                ! on peut imprimer vers le stdout tout le header du rapport
                ! le format utlise est comme celui  du programme liburp
                ! de J.GARCIA http://iweb/~afsqjmg
                ! man liburp sur pollux pour details.
                !
                write(*,*) "<<<<< BURP_TO_STDOUT (REPORT) OUPUT >>>>>"
                Call BURP_TO_STDOUT(rpt_in)
                write(*,*) " "

                ! on imprime les informations sur le parametre
                ! RUNN
                !
                call Burp_TO_STDOUT_Info_Runn(REPORT = rpt_in)

                ! On peut interroger le rapport sur le proprietes
                ! de la RUNN
                ! voir documentation
                !
                write(*,*) "is runn REGULIERE =",   &
                               IS_Burp_Runn("REGUL",REPORT = rpt_in)
                write(*,*) "is runn GLOBALE =",     &
                               IS_Burp_Runn("GLO",REPORT = rpt_in)
                write(*,*) "is runn AO_ALT_FINAL =",&
                               IS_Burp_Runn("AO_ALT_FINAL",REPORT= rpt_in)
                write(*,*) "is runn 06Z =",         &
                               IS_Burp_Runn('06Z',REPORT=rpt_in)

                ! on peut changer les proprites de la runn
                ! Ces procedures sont utiles lors de la 
                ! MANIPULTION des RAPPORTS pour l'ecriture
                !
                call Set_Burp_Runn('PARAL',REPORT=rpt_in,IOSTAT =error)
                call Set_Burp_Runn('REG',REPORT=rpt_in)
                call Set_Burp_Runn('AO_SFC_COMPL',REPORT=rpt_in)
                call Set_Burp_Runn('21Z',REPORT=rpt_in)
                ! imprimer ce que cela donne
                call Burp_TO_STDOUT_Info_Runn(REPORT = rpt_in)
                write(*,*)

                ! Affcher les proprietes du parametre FLGS de
                ! l'enregistrement
                !
                call BURP_TO_STDOUT_Info_flgs(REPORT = rpt_in)
                write(*,*)

                ! On peut interroger le rapport sur le proprietes
                ! de du flgs 
                ! voir documentation
                !
                write(*,*) "Is ENREG_CONT_DON_RESIDUS =",&
                           IS_Burp_Flgs('ENREG_CONT_DON_RESIDUS',REPORT = rpt_in)
                write(*,*) "Is PART_C_TEMP_PILOT_SATEM =",&
                           IS_Burp_Flgs('PART_C_TEMP_PILOT_SATEM',REPORT = rpt_in)
                write(*,*) "Is PART_B_TEMP_PILOT_SATEM =",&
                           IS_Burp_Flgs('PART_B_TEMP_PILOT_SATEM',REPORT = rpt_in)
                write(*,*) "Is ENREG_CONT_DON_DERIVEES =",&
                           IS_Burp_Flgs('ENREG_CONT_DON_DERIVEES',REPORT=rpt_in)
                write(*,*) "Is ENREG_CONT_DON_VERIFIEES =",&
                           IS_Burp_Flgs('ENREG_CONT_DON_VERIFIEES',REPORT=rpt_in)

                ! acitiver le bit du flags correspondant
                ! aux CORRECTIONS_RADIATION
                !
                call Set_Burp_Flgs('CORRECTION_RADIATION',REPORT=rpt_in,IOSTAT =error)
                write(*,*) error
                if (error /= burp_noerr)  call handle_error()

                ! acitiver le bit du flags correspondant
                ! aux OBS_DESSUS_TERRE 
                !
                call Set_Burp_Flgs('OBS_DESSUS_TERRE',REPORT=rpt_in)

                ! faire une impression pour voir le resultat
                ! ces methodes sont autilser pour le manipulation des
                ! enregstrement lors de l'ecrture
                !
                call BURP_TO_STDOUT_Info_flgs(REPORT= rpt_in)

                ! L'enregistrement cherche etent trouve,
                ! tous ces parametres et donnees sont dans
                ! la variable objet rapport rpt_in

                ! on peut alors interroger le rapport rpt_in
                ! sur les valeurs des parametres de son entete
                ! ici par exemple on cherche la date
                ! utliser le mot cle DATE = variable qui va acceullir sa valeur
                ! IDTYP = variable ..
                ! il en est de meme de tous les parametes qu'on veut consulter
                ! ELEV, DRND, OARS, RUNN, LATI, LONG, DX, DY
                ! STNID, NBLK, FLGS
                !
                call BURP_Get_Property(rpt_in, &
                         DATE  = my_date,      &
                         STNID = my_station,   &
                         IDTYP = my_idtyp,IOSTAT=error)

                if (error /= burp_noerr)  call handle_error()

                ! si pas d'erreur on imprime ce qu'on a cherche
                !
                write(*,*) "<<<<< date, codtyp, station >>>>>"
                write(*,*) "date   = ",my_date
                write(*,*) "codtyp = ",my_idtyp
                write(*,*) "station= ",my_station
                write(*,*) " "


                ! scanner les blocks
                ! on commence a chercher les blocks a partir du debut du
                ! rapport. ref_blk est le numero du block dans le rapport.
                ! la recherche d'un block se fait avec une ou plusierus cles
                ! BFAM =, BDESC=,BTYP=
                !
                ref_blk = 0
                do
                  !
                  ! SEARCH_FROM --> specifie a partir de quel numero de bloc
                  ! on commence a chercher. rpt_in --> c'est le rapport
                  ! dans le quel on cherche. block_in --> variable de type block
                  ! qui contiendra toutes les donnees du block si trouve.
                  ! BFAM = --> specifie la cle de recherhe selon le bfam
                  ! BTYP = --> specifie la cle de recherhe selon le btyp
                  ! BDESC= --> specifie la cle de recherhe selon le bdesk
                  ! ERROR= --> variable pour la gestion d'erreur
                  !
                  ref_blk = BURP_Find_Block(rpt_in, &
                     BLOCK       = Block_in,        &
                     SEARCH_FROM = ref_blk,         &
                     BFAM        = 0,               &
                     BKNAT       = 4,               &
                     BKTYP       = 70,              &
                     BKSTP       = 0,               &
                     CONVERT     = .false.,         &
                     IOSTAT = error)
                  if (error /= burp_noerr)  call handle_error()


                  ! si on trouve pas de blk on sort de la boucle
                  !
                  if (ref_blk < 0) Exit

                  ! on peut envoyer tout l'entete du block, vers
                  ! le standart output. le mot cle CONVERT= .true.,
                  ! pour imprimer les donnes reelles, plutot que les valeurs
                  ! entieres codees dans le fichier burp. la conversion  int vers reel
                  ! est selon les facteurs d'echelle pour chaque element. (table burp)
                  !
                  write(*,*) "<<<<< BURP_TO_STDOUT (BLOCK) OUPUT >>>>>"
                  ! ici conversion des valeurs selon TABLE BURP
                  Call BURP_TO_STDOUT(Block_in,CONVERT=.TRUE.)
                  ! ici aucune conversion - donc des entiers -
                  Call BURP_TO_STDOUT(Block_in)
                  write(*,*) " "


                  ! imprimer les informations sur le BTYP
                  !
                  call Burp_TO_STDOUT_Info_Btyp(BLOCK = block_in)

                  ! inerroger le block sur les proprietes de BTYP
                  !
                  write(*,*) "is btyp MULTI        =",&
                              IS_Burp_Btyp("MULTI",BTYP = 9322)
                  write(*,*) "is btyp DATA         =",&
                              IS_Burp_Btyp("DATA",BLOCK = Block_in)
                  write(*,*) "is btyp ALTTUDE      =",&
                              IS_Burp_Btyp("ALT", BTYP  = 9322)
                  write(*,*) "is btyp POST_ALT_GLO =",&
                              IS_Burp_Btyp('POST_ALT_GLO',BLOCK=Block_in)

                  ! changer les proprietes du BTYP du BLOCK 
                  !
                  ! le setter comme un 3-D
                  call Set_Burp_Btyp('3-D', BLOCK = block_in,IOSTAT = error)
                  if (error /= burp_noerr)  call handle_error()

                  ! le setter comme un block UNI
                  call Set_Burp_Btyp('UNI', BLOCK = block_in)

                  ! afficher vers le standart output
                  call Burp_TO_STDOUT_Info_Btyp(BLOCK = block_in)

                  ! on peut chnager les proprietes bTYP du block
                  ! via les BKNAT, BKTYP, BKSTYP
                  call BURP_Set_Property(block_in,BKNAT=7,BKSTP=0,BKTYP=0,IOSTAT=error)
                  if (error /= burp_noerr) call handle_error()

                  ! voir
                  call Burp_TO_STDOUT_Info_Btyp(BLOCK = block_in)

                  ! changer juste le BKNAT
                  call BURP_Set_Property(block_in,BKNAT=2,IOSTAT=error)
                  if (error /= burp_noerr) call handle_error()
                  ! voir
                  call Burp_TO_STDOUT_Info_Btyp(BLOCK = block_in)

                  !
                  ! interroger les paramertes du blk
                  ! block_in --> le block trouve
                  ! NELE = var, pour acceuilir la val. du nbre. d'elemenst
                  ! NVAL = var, pour acceuilir la val du nbre de valeurs
                  !             pour chaque element.
                  ! NT   = var, pour acceulir la valeur de la profonduer du bloc
                  !             c'est le nombre de plans (NELE X NVAL)
                  !
                  ! BFAM = var, pour la valeur de bfam
                  ! BTYP = var, pour la valeur de btyp
                  ! BDESC= var, pour la valeur de bdesc
                  ! NBIT = var, pour la valeur de nbit
                  ! BIT0 = var, pour la valeur de bit0
                  ! OARS = var, pour la valeur de oars
                  ! DATYP= var, pour la valeur de datyp
                  ! BKNO = var, pour la valeur du numero du block
                  ! ERROR= var, pour la valeur de l'erreur
                  ! BKNAT= var, pour la valeur de bknat 
                  ! BKTYP= var, pour la valeur de bktyp
                  ! BKSTP= var, pour la valeur de bkstp
                  !
                  call BURP_Get_Property(Block_in, &
                            NELE = my_nele,        &
                            NT   = my_nt,          &
                            NVAL = my_nval,        &
                            BTYP = my_btyp,        &
                            BKNAT= my_bknat,          &
                            BKTYP= my_bktyp,          &
                            BKSTP= my_bkstp,          &
                            IOSTAT=error)

                  ! si ereur on la gere
                  !
                  if (error /= burp_noerr)  call handle_error()

                  ! sinon on imprime les valeurs trouves
                  !
                  write(*,*) "<<<<< nele, nval, nt >>>>>"
                  write(*,*) "NELE = ",My_nele
                  write(*,*) "NVAL = ",my_nval
                  write(*,*) "NT   = ",my_nt
                  write(*,*) "BKNAT= ",my_bknat
                  write(*,*) "BKTYP= ",my_bktyp
                  write(*,*) "BKSTP= ",my_bkstp
                  write(*,*) " "
                end do
        end do

        ! Clean-up
        ! on libere toutes les ressources
        ! dealolocation de la memoire dynamique
        ! fermeture de fichiers burp
        !

        ! les ressources pour manipuler les rapports
        !
        Call BURP_Free(rpt_in,IOSTAT=error)
        if (error /= burp_noerr)  call handle_error()

        ! les ressources pour manipuler les blocks
        !
        Call BURP_Free(Block_in,IOSTAT=error)
        if (error /= burp_noerr)  call handle_error()

        ! les ressources pour manipuler les fichiers
        !
        Call BURP_Free(File_in,IOSTAT=error)
        if (error /= burp_noerr)  call handle_error()

          Call BURP_STR_ERROR_HISTORY()

        ! donne l'historique des succes/warninngs/echecs
        ! d'utilisation des routines de la librairie.
        ! il n'est pas obligatoire de verifier si erreur ou pas.
        ! si le programme fonctionne , dans l'historique
        ! des erreurs. il ne devrait pas y avoir failure...
        !

        ! internal procedures
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


      end program READ1

