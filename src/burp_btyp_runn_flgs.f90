!
MODULE burp_btyp_runn_flgs
     use burp_rpt_class
     implicit none

     ! note
     ! ibits(m,k,len) = fcn qui extrait un ss-ens de
     !                  "len" bits de "m", en partant
     !                  de la position "k" et en allant
     !                  vers la gauche de "len" bits.
     !
contains

     function burp_block_type(block,pos,IOSTAT,BTYP) result(blk_type)
      implicit none
      type(Burp_block),optional,intent(in)       :: block
      integer(kind = int_def),optional,intent(in):: BTYP
      integer(kind = int_def),intent(in):: pos
      integer(kind = int_def) :: bknat1,bknat2
      integer(kind = int_def) :: bktyp1,bktyp2
      integer(kind = int_def) :: bkstp,my_btyp,error
      integer(kind = int_def), optional :: IOSTAT

      character(len = 20)     :: blk_type
      blk_type = "UNKNOWN"

      if (present(block)) then
          call BURP_Get_Property(block, &
                BTYP = my_btyp, IOSTAT=error)
          if (error /= burp_noerr) return
      endif
      if (present(BTYP)) my_btyp = btyp

     ! decomposition du btyp
     !   btyp    =      bknat       |      bktyp      |  bkstp
     !   15 bits =      4 bits      |      7 bits     |  4 bits
     !           =  bknat1 | bknat2 | bktyp1 | bktyp2 |  bkstp
     !           =  2 bits | 2 bits | 1 bit  | 6 bits |  4 bits
     !

      bknat1 = ibits ( my_btyp, 13, 2 )
      bknat2 = ibits ( my_btyp, 11, 2 )
      bktyp1 = ibits ( my_btyp, 10, 1 )
      bktyp2 = ibits ( my_btyp,  4, 6 )
      bkstp  = ibits ( my_btyp,  0, 4 )

     ! traitement de bknat1
     ! 00 = UNI
     ! 01 = MULTI
     !
     if (pos == 0) then
         select case (bknat1)
             case (0)
                  blk_type = "UNI"
             case (1)
                  blk_type = "MULTI"
             case default
                  blk_type = "RESERVED"
          end select
          return
     endif

         ! traitement de bknat2
         ! 00 = donnees
         ! 01 = info
         ! 10 = descripteur 3-D
         ! 11 = marqueurs
         !

     if (pos == 1) then
         select case (bknat2)

             case (0)
                  blk_type = "DATA"
             case (1)
                  blk_type = "INFO"
             case (2)
                  blk_type = "3-D"
             case (3)
                  blk_type = "MRQR"
             case default
                  blk_type = "RESERVED"
          end select
          return
     endif


         ! traitement de bktyp1
         ! 00 = SFC
         ! 01 = ALT
         !

     if (pos == 2) then
         select case (bktyp1)
             case (0)
                  blk_type = "SFC"
             case (1)
                  blk_type = "ALT"
             case default
                  blk_type = "RESERVED"
          end select
          return
     endif

         ! traitement de bktyp2
         ! 0 = observations (ADE)
         ! 1 = observations brutes (NON DECODEES)
         ! ...............
         ! ...............
         ! 22 = donnees SSMI
         ! 23 @ 63 = EN RESERVE
         !

     if (pos == 3) then
         select case (bktyp2)
             case (0)
                  blk_type = "OBS_ADE"
             case (1)
                  blk_type = "OBS_BRUTES"
             case (2)
                  blk_type = "DERI_ALT_GLO"
             case (3)
                  blk_type = "DERI_ALT_REG"
             case (4)
                  blk_type = "DERI_SFC_GLO"
             case (5)
                  blk_type = "DERI_SFC_REG"
             case (6)
                  blk_type = "POST_ALT_GLO"
             case (7)
                  blk_type = "POST_ALT_REG"
             case (8)
                  blk_type = "POST_SFC_GLO"
             case (9)
                  blk_type = "POST_SFC_REG"
             case (10)
                  blk_type = "PRFL_ALT_GLO"
             case (11)
                  blk_type = "PRFL_ALT_REG"
             case (12)
                  blk_type = "RESERVED"
             case (13)
                  blk_type = "RESERVED"
             case (14)
                  blk_type = "ANAL_ALT_GLO"
             case (15)
                  blk_type = "ANAL_ALT_REG"
             case (16)
                  blk_type = "ANAL_SFC_GLO"
             case (17)
                  blk_type = "ANAL_SFC_REG"
             case (18)
                  blk_type = "PREV_GLO"
             case (19)
                  blk_type = "PREV_REG"
             case (20)
                  blk_type = "STAT_PENSE"
             case (21)
                  blk_type = "STAT_KALMAN"
             case (22)
                  blk_type = "SSMI"
             case default
                  blk_type = "RESERVED"
          end select
          return
     endif
     end function burp_block_type

     function burp_runn_type(report,pos,IOSTAT,RUNN) result(runn_type)
     !
     implicit none
      type(Burp_rpt),optional,intent(in)            :: report
      integer(kind = int_def),optional,intent(in)   :: runn
      integer(kind = int_def),optional,intent(inout):: IOSTAT
      integer(kind = int_def),intent(in)            :: pos
      integer(kind = int_def) :: rnat1,rnat2
      integer(kind = int_def) :: rtyp1,rtyp2
      integer(kind = int_def) :: my_runn,error

      character(len = 20)     :: runn_type
      runn_type = "UNKNOWN"

      ! decomposition de la runn
      !
      !   runn    =      rnat        |       rtyp
      !   8  bits =      2 bits      |       6 bits
      !           =  rnat1  | rnat2  | rtyp1  | rtyp2
      !           =  1 bits | 1 bits | 3 bit  | 3 bits
      !


      if (present(report)) then
          call BURP_Get_Property(report, &
                RUNN = my_runn, IOSTAT=error)
          if (error /= burp_noerr) return
      endif
      if (present(RUNN)) my_runn = runn


      rnat1 = ibits ( my_runn, 7, 1 )
      rnat2 = ibits ( my_runn, 6, 1 )
      rtyp1 = ibits ( my_runn, 3, 3 )
      rtyp2 = ibits ( my_runn, 0, 3 )

      ! write(*,*) " "
      ! write(*,*) "rnat1",rnat1
      ! write(*,*) "rnat2",rnat2
      ! write(*,*) "rtyp1",rtyp1
      ! write(*,*) "rtyp2",rtyp2

      ! traitement de rnat1
      !  0 = REGUL
      !  1 = PARAL
      !

     if (pos == 0) then
         select case (rnat1)
             case (0)
                  runn_type = "REGUL"
             case (1)
                  runn_type = "PARAL"
             case default
                  runn_type = "RESERVED"
         end select
     endif


         ! traitement de rnat2
         ! 0 = GLO
         ! 1 = REG
         !

     if (pos == 1) then
         select case (rnat2)
             case (0)
                  runn_type = "GLO"
             case (1)
                  runn_type = "REG"
             case default
                  runn_type = "RESERVED"
         end select
     endif


         !traitement de rtyp1
         !000 = AO_ALT_PRELI
         !001 = AO_ALT_COMPL
         !010 = AO_ALT_FINAL
         !011 = AO_SFC_SPECI
         !100 = AO_SFC_PRELI
         !101 = AO_SFC_COMPL
         !110 = AO_SFC_FINAL
         !111 = RESERVED
         !/


     if (pos == 2) then
         select case (rtyp1)
             case (0)
                  runn_type = "AO_ALT_PRELI"
             case (1)
                  runn_type = "AO_ALT_COMPL"
             case (2)
                  runn_type = "AO_ALT_FINAL"
             case (3)
                  runn_type = "AO_SFC_SPECI"
             case (4)
                  runn_type = "AO_SFC_PRELI"
             case (5)
                  runn_type = "AO_SFC_COMPL"
             case (6)
                  runn_type = "AO_SFC_FINAL"
             case (7)
                  runn_type = "RESERVED"
             case default
                  runn_type = "RESERVED"
         end select
     endif


         !traitement de rtyp2
         !000 = 00Z
         !001 = 03Z
         !010 = 06Z
         !011 = 09Z
         !100 = 12Z
         !101 = 15Z
         !110 = 18Z
         !111 = 21Z
         !/


     if (pos == 3) then
         select case (rtyp2)
             case (0)
                  runn_type = "00Z"
             case (1)
                  runn_type = "03Z"
             case (2)
                  runn_type = "06Z"
             case (3)
                  runn_type = "09Z"
             case (4)
                  runn_type = "12Z"
             case (5)
                  runn_type = "15Z"
             case (6)
                  runn_type = "18Z"
             case (7)
                  runn_type = "21Z"
             case default
                  runn_type = "RESERVED"
         end select
     endif

    end function

    function burp_Flgs_type(report,pos,IOSTAT,flgs) result(flgs_type)
     !
     implicit none
      type(Burp_rpt),optional,intent(in)            :: report
      integer(kind = int_def),optional,intent(in)   :: flgs
      integer(kind = int_def),optional,intent(inout):: IOSTAT
      integer(kind = int_def),intent(in)            :: pos
      integer(kind = int_def) :: my_flgs,error

      character(len = 40)     :: flgs_type
      flgs_type = "BIT_OFF"
      my_flgs   = 0

      if (present(report)) then
          call BURP_Get_Property(report, &
                FLGS = my_flgs, IOSTAT=error)
          if (error /= burp_noerr) return
      endif
      if (present(flgs)) my_flgs = flgs

      ! decompostion du flag
      !
      ! bit 00  OBS_DESSUS_TERRE      >> observation au-dessus de la terre
      ! bit 01  VENT_SFC_UTILISE      >> vent de surface utilise
      ! bit 02  CORRECTION_RADIATION  >> correction de radiation
      ! bit 03  CORRECTION_POS_BATEAU >> correction position bateaux
      ! bit 04  RESERVED              >> reserve
      ! bit 05  STN_HORS_INTERET      >> station hors du domaine d'interet
      ! bit 06  STN_REJET_PAR_AO      >> station rejetee par control de
      !                                  qualite de l'AO
      ! bit 07  STN_LISTE_NOIRE       >> station sur liste noire
      ! bit 08  STN_A_EVALUER         >> station a evaluer mais non utilisee
      !                                  par l'AO
      ! bit 09  STN_DOUTEUSE          >> decodeur rapporte position de station
      !                                  douteuse
      ! bit 10  ENREG_CONT_DON_OBSERVEES     >> donnees observees
      ! bit 11  ENREG_CONT_DON_DERIVEES      >> donnees derivees
      ! bit 12  ENREG_CONT_DON_VUES_PAR_AO   >> donnees vues par l'AO
      ! bit 13  ENREG_CONT_DON_RESIDUS       >> residus
      ! bit 14  PART_RDT_STB_STM      >> partie radat / ts satob / ts satem
      ! bit 15  PART_A_TEMP           >> partie a   temp / pilot / satem / satob
      ! bit 16  PART_B_TEMP           >> partie b   temp / pilot / satem
      ! bit 17  PART_C_TEMP           >> partie c   temp / pilot / satem
      ! bit 18  PART_D_TEMP           >> partie d   temp / pilot / satem
      ! bit 19  ENREG_CONT_DON_ANALYSEES  >> enregistrement contient des donnees
      !                                      analysees
      ! bit 20  ENREG_CONT_DON_PREVUES    >> enregistrement contient des donnees
      !                                      prevues
      ! bit 21  ENREG_CONT_DON_VERIFIEES  >> enregistrement contient des donnees
      !                                      verifiees
      ! bit 22  RESERVED
      ! bit 23  RESERVED
      !
      select case (pos)
          case (0)
               if (btest(my_flgs,00)) flgs_type = 'OBS_DESSUS_TERRE'
          case (1)
               if (btest(my_flgs,01)) flgs_type = 'VENT_SFC_UTILISE'
          case (2)
               if (btest(my_flgs,02)) flgs_type = 'CORRECTION_RADIATION'
          case (3)
               if (btest(my_flgs,03)) flgs_type = 'CORRECTION_POS_BATEAU'
          case (4)
               if (btest(my_flgs,04)) flgs_type = 'RESERVED'
          case (5)
               if (btest(my_flgs,05)) flgs_type = 'STN_HORS_INTERET'
          case (6)
               if (btest(my_flgs,06)) flgs_type = 'STN_REJET_PAR_AO'
          case (7)
               if (btest(my_flgs,07)) flgs_type = 'STN_LISTE_NOIRE'
          case (8)
               if (btest(my_flgs,08)) flgs_type = 'STN_A_EVALUER'
          case (9)
               if (btest(my_flgs,09)) flgs_type = 'STN_DOUTEUSE'
          case (10)
               if (btest(my_flgs,10)) flgs_type = 'ENREG_CONT_DON_OBSERVEES'
          case (11)
               if (btest(my_flgs,11)) flgs_type = 'ENREG_CONT_DON_DERIVEES'
          case (12)
               if (btest(my_flgs,12)) flgs_type = 'ENREG_CONT_DON_VUES_PAR_AO'
          case (13)
               if (btest(my_flgs,13)) flgs_type = 'ENREG_CONT_DON_RESIDUS'
          case (14)
               if (btest(my_flgs,14)) flgs_type = 'PART_RADAT_SATEM_SATOB'
          case (15)
               if (btest(my_flgs,15)) flgs_type = 'PART_A_TEMP_PILOT_SATEM_SATOB'
          case (16)
               if (btest(my_flgs,16)) flgs_type = 'PART_B_TEMP_PILOT_SATEM'
          case (17)
               if (btest(my_flgs,17)) flgs_type = 'PART_C_TEMP_PILOT_SATEM'
          case (18)
               if (btest(my_flgs,18)) flgs_type = 'PART_D_TEMP_PILOT_SATEM'
          case (19)
               if (btest(my_flgs,19)) flgs_type = 'ENREG_CONT_DON_ANALYSEES'
          case (20)
               if (btest(my_flgs,20)) flgs_type = 'ENREG_CONT_DON_PREVUES'
          case (21)
               if (btest(my_flgs,21)) flgs_type = 'ENREG_CONT_DON_VERIFIEES'
          case (22)
               if (btest(my_flgs,22)) flgs_type = 'RESERVED'
          case (23)
               if (btest(my_flgs,23)) flgs_type = 'RESERVED'
          case default
               return
      end select
      return
    end function

    function IS_Burp_Flgs(Flgs_Key,REPORT,flgs) result(flag)
    !
    implicit none
    type(Burp_rpt),optional,intent(in)         :: REPORT
    integer(kind = int_def),optional,intent(in):: flgs
    character(len = *),intent(in)              :: Flgs_Key
    logical                                    :: flag
    integer(kind = int_def)                    :: k
    flag = .FALSE.
    if (present(report)) then
       do k=0,23
          if ( (BURP_Flgs_Type(pos = k, REPORT= report) == flgs_key)) then
              flag = .TRUE.
              return
          endif
        enddo
    endif
    if (present(flgs)) then
       do k=0,23
          if ( (BURP_Flgs_Type(pos = k, FLGS= flgs) == flgs_key) ) then
              flag = .TRUE.
              return
           endif
        enddo
    endif
    return
    end function

    function IS_Burp_Btyp(Btyp_Key,BLOCK,BTYP) result(flag)
    !
    implicit none
    type(Burp_block),optional,intent(in)       :: BLOCK
    integer(kind = int_def),optional,intent(in):: btyp
    character(len = *),intent(in) :: btyp_Key
    logical                       :: flag
    integer(kind = int_def)       :: k

    flag = .FALSE.
    if (present(BLOCK)) then
       do k=0,3
          if ( (BURP_Block_Type(pos = k, BLOCK= block) == btyp_key)) then
              flag = .TRUE.
              return
           endif
        enddo
    endif
    if (present(BTYP)) then
       do k=0,3
          if ( (BURP_Block_Type(pos = k, BTYP= btyp) == btyp_key)) then
             flag = .TRUE.
             return
           endif
        enddo
    endif
    return
    end function

    function IS_Burp_Runn(Runn_Key,REPORT,RUNN) result(flag)
    !
    implicit none
    type(Burp_rpt),optional,intent(in)         :: REPORT
    integer(kind = int_def),optional,intent(in):: runn
    character(len = *),intent(in)              :: Runn_Key
    logical                :: flag
    integer(kind = int_def):: k

    flag = .FALSE.
    if (present(report)) then
       do k=0,3
       if ( (BURP_Runn_Type(pos = k, REPORT= report) == runn_key)) then
            flag = .TRUE.
            return
           endif
        enddo
    endif
    if (present(runn)) then
       do k=0,3
          if ( (BURP_Runn_Type(pos = k, runn= runn) == runn_key)) then
            flag = .TRUE.
            return
           endif
        enddo
    endif
    return
    end function

    subroutine Burp_TO_STDOUT_Info_Btyp(BLOCK,BTYP)
    !
    implicit none
    type(Burp_block),optional,intent(in)       :: BLOCK
    integer(kind = int_def),optional,intent(in):: BTYP
    integer(kind = int_def)                    :: k,bknat,bktyp,bkstp,&
                                                  my_btyp,error
    if (present(BLOCK)) then
      Call BURP_Get_Property(BLOCK,BTYP=my_btyp,BKNAT=bknat,&
                             BKTYP=bktyp,BKSTP=bkstp,IOSTAT=error)
      if (error /= burp_noerr) return
      write(*,'(60a)')">> btyp =     bknat       |      bktyp      | bkstp"
      write(*,'(60a)')" 15 bits=     4 bits      |      7 bits     | 4 bits"
      write(*,'(60a)')"        = bknat1 | bknat2 | bktyp1 | bktyp2 | bkstp"
      write(*,'(60a)')"        = 2 bits | 2 bits | 1 bit  | 6 bits | 4 bits"
      write(*,*) " "
      write(*,'(a10,a6)',ADVANCE="no") " bknat1 = ",BURP_Block_Type(pos = 0, BLOCK= block)
      write(*,'(a10,a6)',ADVANCE="no") " bknat2 = ",BURP_Block_Type(pos = 1, BLOCK= block)
      write(*,'(a10,a6)',ADVANCE="no") " bktyp1 = ",BURP_Block_Type(pos = 2, BLOCK= block)
      write(*,'(a10,a20)') "bktyp2 = ",BURP_Block_Type(pos = 3, BLOCK= block)
      write(*,'(a10,i6)',ADVANCE="no") " btyp   = ",my_btyp
      write(*,'(a10,i6)',ADVANCE="no") " bknat  = ",bknat
      write(*,'(a10,i6)',ADVANCE="no") " bktyp  = ",bktyp
      write(*,'(a10,i6)') " bkstp  = ",bkstp
      write(*,*) "<< end btyp"
      return
    endif
    if (present(BTYP)) then
      error = MRBTYP(bknat,bktyp,bkstp,btyp)
      if (error /=burp_noerr) return
      write(*,'(60a)')">> btyp =     bknat       |      bktyp      | bkstp"
      write(*,'(60a)')" 15 bits=     4 bits      |      7 bits     | 4 bits"
      write(*,'(60a)')"        = bknat1 | bknat2 | bktyp1 | bktyp2 | bkstp"
      write(*,'(60a)')"        = 2 bits | 2 bits | 1 bit  | 6 bits | 4 bits"
      write(*,*) " "
      write(*,'(a10,a6)',ADVANCE="no") " bknat1 = ",BURP_Block_Type(pos = 0, BTYP= btyp)
      write(*,'(a10,a6)',ADVANCE="no") " bknat2 = ",BURP_Block_Type(pos = 1, BTYP= btyp)
      write(*,'(a10,a6)',ADVANCE="no") " bktyp1 = ",BURP_Block_Type(pos = 2, BTYP= btyp)
      write(*,'(a10,a20)') "bktyp2 = ",BURP_Block_Type(pos = 3, BTYP= btyp)
      write(*,'(a10,i6)',ADVANCE="no") " btyp   = ",btyp
      write(*,'(a10,i6)',ADVANCE="no") " bknat  = ",bknat
      write(*,'(a10,i6)',ADVANCE="no") " bktyp  = ",bktyp
      write(*,'(a10,i6)') " bkstp  = ",bkstp
      write(*,*) "<< end btyp"
    endif
    end subroutine

    subroutine Burp_TO_STDOUT_Info_Runn(REPORT,RUNN)
    !
    implicit none
    type(Burp_rpt),optional,intent(in)       :: report
    integer(kind = int_def),optional,intent(in):: runn
    integer(kind = int_def)                    :: rnat1,rnat2,rtyp1,&
                                                  rtyp2,error,my_runn
    if (present(REPORT)) then
      Call BURP_Get_Property(REPORT,RUNN=my_runn ,IOSTAT=error)
      if (error /= burp_noerr) return
      rnat1 = ibits ( my_runn, 7, 1 )
      rnat2 = ibits ( my_runn, 6, 1 )
      rtyp1 = ibits ( my_runn, 3, 3 )
      rtyp2 = ibits ( my_runn, 0, 3 )
      !
      write(*,'(60a)')">> runn =      rnat        |       rtyp"
      write(*,'(60a)')" 8  bits=      2 bits      |       6 bits"
      write(*,'(60a)')"        =  rnat1  | rnat2  | rtyp1  | rtyp2"
      write(*,'(60a)')"        =  1 bits | 1 bits | 3 bit  | 3 bits"
      write(*,*) " "
      write(*,'(a10,i6)',ADVANCE="no") " runn   = ",my_runn
      write(*,'(a10,a6)',ADVANCE="no") " rnat1  = ",BURP_Runn_Type(pos = 0, REPORT= report)
      write(*,'(a10,a6)',ADVANCE="no") " rnat2  = ",BURP_Runn_Type(pos = 1, REPORT= report)
      write(*,'(a10,a15)',ADVANCE="no") " rtyp1  = ",BURP_Runn_Type(pos = 2, REPORT= report)
      write(*,'(a10,a6)') "rtyp2 =  ",BURP_Runn_Type(pos = 3, REPORT= report)
      write(*,'(a10,i6)',ADVANCE="no") " runn   = ",my_runn
      write(*,'(a10,i6)',ADVANCE="no") " rnat1  = ",rnat1
      write(*,'(a10,i6)',ADVANCE="no") " rnat2  = ",rnat2
      write(*,'(a10,i15)',ADVANCE="no") " rtyp1  = ",rtyp1
      write(*,'(a10,i6)') " rtyp2  = ",rtyp2
      write(*,*) "<< end runn"
       return
    endif
    if (present(RUNN)) then
      rnat1 = ibits ( runn, 7, 1 )
      rnat2 = ibits ( runn, 6, 1 )
      rtyp1 = ibits ( runn, 3, 3 )
      rtyp2 = ibits ( runn, 0, 3 )

      write(*,'(60a)')">> runn =      rnat        |       rtyp"
      write(*,'(60a)')" 8  bits=      2 bits      |       6 bits"
      write(*,'(60a)')"        =  rnat1  | rnat2  | rtyp1  | rtyp2"
      write(*,'(60a)')"        =  1 bits | 1 bits | 3 bit  | 3 bits"
      write(*,*) " "
      write(*,'(a10,i6)',ADVANCE="no") " runn   = ",runn
      write(*,'(a10,a6)',ADVANCE="no") " rnat1  = ",BURP_Runn_Type(pos = 0, RUNN= runn)
      write(*,'(a10,a6)',ADVANCE="no") " rnat2  = ",BURP_Runn_Type(pos = 1, RUNN= runn)
      write(*,'(a10,a15)',ADVANCE="no") " rtyp1  = ",BURP_Runn_Type(pos = 2, RUNN= runn)
      write(*,'(a10,a6)') "rtyp2  = ",BURP_Runn_Type(pos = 3, RUNN= runn)
      write(*,'(a10,i6)',ADVANCE="no") " runn   = ",runn
      write(*,'(a10,i6)',ADVANCE="no") " rnat1  = ",rnat1
      write(*,'(a10,i6)',ADVANCE="no") " rnat2  = ",rnat2
      write(*,'(a10,i15)',ADVANCE="no") " rtyp1  = ",rtyp1
      write(*,'(a10,i6)') " rtyp2  = ",rtyp2
      write(*,*) "<< end runn"
    endif
    end subroutine

    subroutine Burp_TO_STDOUT_Info_flgs(REPORT,flgs)
    !
    implicit none
    type(Burp_rpt),optional,intent(in)       :: report
    integer(kind = int_def),optional,intent(in):: flgs
    integer(kind = int_def)                    :: k
    character(len = 40)                        :: flgs_key
    if (present(REPORT)) then
       write(*,*) ">> flgs"
       do k=0,23
         flgs_key = BURP_Flgs_Type(pos = k, REPORT= report )
         if (flgs_key == 'BIT_OFF') cycle
           if (k <= 9) then
              write(*,*) "BIT 0"//CVT(k)//" ON >> "//flgs_key
           else
              write(*,*) "BIT "//CVT(k)//" ON >> "//flgs_key
           endif
       enddo
       write(*,*) "<< end flgs"
       return
    endif
    if (present(flgs)) then
       write(*,*) ">> flgs"
       do k=0,23
         flgs_key = BURP_flgs_Type(POS = k, flgs= flgs)
         if (flgs_key == 'BIT_OFF') cycle
           if (k <= 9) then
              write(*,*) "BIT 0"//CVT(k)//" ON >> "//flgs_key
           else
              write(*,*) "BIT "//CVT(k)//" ON >> "//flgs_key
           endif
       enddo
       write(*,*) "<< end flgs"
    endif
    end subroutine


    subroutine Set_Burp_Btyp(Btyp_Key,BLOCK,BTYP,IOSTAT)
    ! Set_Burp_Btyp subroutine
    !
    ! affecter les bits correspondant du btyp
    ! en fonction de la cle
    !
    ! decomposition du btyp
    !   btyp    =      bknat       |      bktyp      |  bkstp
    !   15 bits =      4 bits      |      7 bits     |  4 bits
    !           =  bknat1 | bknat2 | bktyp1 | bktyp2 |  bkstp
    !           =  2 bits | 2 bits | 1 bit  | 6 bits |  4 bits
    !

    ! bknat1 = ibits ( my_btyp, 13, 2 )
    ! bknat2 = ibits ( my_btyp, 11, 2 )
    ! bktyp1 = ibits ( my_btyp, 10, 1 )
    ! bktyp2 = ibits ( my_btyp,  4, 6 )
    ! bkstp  = ibits ( my_btyp,  0, 4 )
    !
    implicit none
    type(Burp_block),optional,intent(inout)       :: BLOCK
    integer(kind = int_def),optional,intent(inout):: btyp
    integer(kind = int_def),optional,intent(inout):: IOSTAT
    character(len = *),intent(in) :: btyp_Key
    integer(kind = int_def)       :: k
    integer(kind = int_def) :: bknat1,bknat2
    integer(kind = int_def) :: bktyp1,bktyp2
    integer(kind = int_def) :: bkstp,my_btyp,error

    error = burp_noerr

    if (present(BLOCK)) then
          call BURP_Get_Property(block, &
                BTYP = my_btyp, IOSTAT=error)
          if (present(IOSTAT)) IOSTAT = error
          if (error /= burp_noerr) return

     select case (btyp_key)
         case ('UNI')
         ! bknat1 = ibits ( my_btyp, 13, 2 )
         ! traitement de bknat1
         ! 00 = UNI
         ! 01 = MULTI
         !
              my_btyp = IBCLR(my_btyp,13)
              my_btyp = IBCLR(my_btyp,14)
         case ('MULTI')
             write(*,*) 'MULTI my_btyp',my_btyp
              my_btyp = IBSET(my_btyp,13)
              my_btyp = IBCLR(my_btyp,14)

         case ('DATA')
         ! bknat2 = ibits ( my_btyp, 11, 2 )
         ! traitement de bknat2
         ! 00 = donnees
         ! 01 = info
         ! 10 = descripteur 3-D
         ! 11 = marqueurs
         !
              my_btyp = IBCLR(my_btyp,11)
              my_btyp = IBCLR(my_btyp,12)
         case ('INFO')
              my_btyp = IBSET(my_btyp,11)
              my_btyp = IBCLR(my_btyp,12)
         case ('3-D')
              my_btyp = IBCLR(my_btyp,11)
              my_btyp = IBSET(my_btyp,12)
         case ('MRQR')
              my_btyp = IBSET(my_btyp,11)
              my_btyp = IBSET(my_btyp,12)

         case ('SFC')
         ! bktyp1 = ibits ( my_btyp, 10, 1 )
         ! traitement de bktyp1
         ! 00 = SFC
         ! 01 = ALT
         !
              my_btyp = IBCLR(my_btyp,10)
         case ('ALT')
              my_btyp = IBSET(my_btyp,10)

         case ( 'OBS_ADE')
         ! bktyp2 = ibits ( my_btyp,  4, 6 )
         ! traitement de bktyp2
         ! 0 = observations (ADE)
         ! 1 = observations brutes (NON DECODEES)
         ! ...............
         ! ...............
         ! 22 = donnees SSMI
         ! 23 @ 63 = EN RESERVE
         !
            !>> 9 8 7 6 5 4
            !>> 0 0 0 0 0 0
            do k = 4,9
              my_btyp = IBCLR(my_btyp,k)
            enddo
         case ( 'OBS_BRUTES')
            !>> 9 8 7 6 5 4
            !>> 0 0 0 0 0 1
            do k = 4,9
              my_btyp = IBCLR(my_btyp,k)
            enddo
            my_btyp = IBSET(my_btyp,4)
         case ( 'DERI_ALT_GLO')
            !>> 9 8 7 6 5 4
            !>> 0 0 0 0 1 0
            do k = 4,9
              my_btyp = IBCLR(my_btyp,k)
            enddo
            my_btyp = IBSET(my_btyp,5)
         case ( 'DERI_ALT_REG')
            !>> 9 8 7 6 5 4
            !>> 0 0 0 0 1 1
            do k = 4,9
              my_btyp = IBCLR(my_btyp,k)
            enddo
            my_btyp = IBSET(my_btyp,4)
            my_btyp = IBSET(my_btyp,5)
         case ( 'DERI_SFC_GLO')
            !>> 9 8 7 6 5 4
            !>> 0 0 0 1 0 0
            do k = 4,9
              my_btyp = IBCLR(my_btyp,k)
            enddo
            my_btyp = IBSET(my_btyp,6)
         case ( 'DERI_SFC_REG')
            !>> 9 8 7 6 5 4
            !>> 0 0 0 1 0 1
            do k = 4,9
              my_btyp = IBCLR(my_btyp,k)
            enddo
            my_btyp = IBSET(my_btyp,6)
            my_btyp = IBSET(my_btyp,4)
         case ( 'POST_ALT_GLO')
            !>> 9 8 7 6 5 4
            !>> 0 0 0 1 1 0
            do k = 4,9
              my_btyp = IBCLR(my_btyp,k)
            enddo
            my_btyp = IBSET(my_btyp,6)
            my_btyp = IBSET(my_btyp,5)
         case ( 'POST_ALT_REG')
            !>> 9 8 7 6 5 4
            !>> 0 0 0 1 1 1
            do k = 4,9
              my_btyp = IBCLR(my_btyp,k)
            enddo
            my_btyp = IBSET(my_btyp,6)
            my_btyp = IBSET(my_btyp,5)
            my_btyp = IBSET(my_btyp,4)
         case ( 'POST_SFC_GLO')
            !>> 9 8 7 6 5 4
            !>> 0 0 1 0 0 0
            do k = 4,9
              my_btyp = IBCLR(my_btyp,k)
            enddo
            my_btyp = IBSET(my_btyp,7)
         case ( 'POST_SFC_REG')
            !>> 9 8 7 6 5 4
            !>> 0 0 1 0 0 1
            do k = 4,9
              my_btyp = IBCLR(my_btyp,k)
            enddo
            my_btyp = IBSET(my_btyp,7)
            my_btyp = IBSET(my_btyp,4)
         case ( 'PRFL_ALT_GLO')
            !>> 9 8 7 6 5 4
            !>> 0 0 1 0 1 0
            do k = 4,9
              my_btyp = IBCLR(my_btyp,k)
            enddo
            my_btyp = IBSET(my_btyp,7)
            my_btyp = IBSET(my_btyp,5)
         case ( 'PRFL_ALT_REG')
            !>> 9 8 7 6 5 4
            !>> 0 0 1 0 1 1
            do k = 4,9
              my_btyp = IBCLR(my_btyp,k)
            enddo
            my_btyp = IBSET(my_btyp,7)
            my_btyp = IBSET(my_btyp,5)
            my_btyp = IBSET(my_btyp,4)
         case ( 'RESERVED')
            !>> 9 8 7 6 5 4
            !>> 0 0 1 1 0 0
            do k = 4,9
              my_btyp = IBCLR(my_btyp,k)
            enddo
            my_btyp = IBSET(my_btyp,7)
            my_btyp = IBSET(my_btyp,6)
!         case ( 'RESERVED')
!         !>> 9 8 7 6 5 4
!         !>> 0 0 1 1 0 1
!            do k = 4,9
!              my_btyp = IBCLR(my_btyp,k)
!            enddo
!            my_btyp = IBSET(my_btyp,7)
!            my_btyp = IBSET(my_btyp,6)
!            my_btyp = IBSET(my_btyp,4)
         case ( 'ANAL_ALT_GLO')
            !>> 9 8 7 6 5 4
            !>> 0 0 1 1 1 0
            do k = 4,9
              my_btyp = IBCLR(my_btyp,k)
            enddo
            my_btyp = IBSET(my_btyp,7)
            my_btyp = IBSET(my_btyp,6)
            my_btyp = IBSET(my_btyp,5)
         case ( 'ANAL_ALT_REG')
            !>> 9 8 7 6 5 4
            !>> 0 0 1 1 1 1
            do k = 4,9
              my_btyp = IBCLR(my_btyp,k)
            enddo
            my_btyp = IBSET(my_btyp,7)
            my_btyp = IBSET(my_btyp,6)
            my_btyp = IBSET(my_btyp,5)
            my_btyp = IBSET(my_btyp,4)
         case ( 'ANAL_SFC_GLO')
            !>> 9 8 7 6 5 4
            !>> 0 1 0 0 0 0
            do k = 4,9
              my_btyp = IBCLR(my_btyp,k)
            enddo
            my_btyp = IBSET(my_btyp,8)
         case ( 'ANAL_SFC_REG')
            !>> 9 8 7 6 5 4
            !>> 0 1 0 0 0 1
            do k = 4,9
              my_btyp = IBCLR(my_btyp,k)
            enddo
            my_btyp = IBSET(my_btyp,8)
            my_btyp = IBSET(my_btyp,4)
         case ( 'PREV_GLO')
            !>> 9 8 7 6 5 4
            !>> 0 1 0 0 1 0
            do k = 4,9
              my_btyp = IBCLR(my_btyp,k)
            enddo
            my_btyp = IBSET(my_btyp,8)
            my_btyp = IBSET(my_btyp,5)
         case ( 'PREV_REG')
            !>> 9 8 7 6 5 4
            !>> 0 1 0 0 1 1
            do k = 4,9
              my_btyp = IBCLR(my_btyp,k)
            enddo
            my_btyp = IBSET(my_btyp,8)
            my_btyp = IBSET(my_btyp,5)
            my_btyp = IBSET(my_btyp,4)
         case ( 'STAT_PENSE')
            !>> 9 8 7 6 5 4
            !>> 0 1 0 1 0 0
            do k = 4,9
              my_btyp = IBCLR(my_btyp,k)
            enddo
            my_btyp = IBSET(my_btyp,8)
            my_btyp = IBSET(my_btyp,6)
         case ( 'STAT_KALMAN')
            !>> 9 8 7 6 5 4
            !>> 0 1 0 1 0 1
            do k = 4,9
              my_btyp = IBCLR(my_btyp,k)
            enddo
            my_btyp = IBSET(my_btyp,8)
            my_btyp = IBSET(my_btyp,6)
            my_btyp = IBSET(my_btyp,4)
         case ( 'SSMI')
            !>> 9 8 7 6 5 4
            !>> 0 1 0 1 1 0
            do k = 4,9
              my_btyp = IBCLR(my_btyp,k)
            enddo
            my_btyp = IBSET(my_btyp,8)
            my_btyp = IBSET(my_btyp,6)
            my_btyp = IBSET(my_btyp,5)
         case default
             Call setStateToFailure(BURP_ERROR%ERROR, &
             "Failure >> "//btyp_key//" :Unknown btyp_key : set_burp_btyp ")
              if (present(IOSTAT))  IOSTAT = -1
              return
      end select
      call BURP_Set_Property(block, &
           BTYP = my_btyp, IOSTAT=error)
      if (present(IOSTAT))  IOSTAT = error
      if (error /= burp_noerr) return

    endif


    ! si le parametre est l'entier btyp
    !
    if (present(BTYP)) then
     select case (btyp_key)
         case ('UNI')
         ! bknat1 = ibits ( btyp, 13, 2 )
         ! traitement de bknat1
         ! 00 = UNI
         ! 01 = MULTI
         !
              btyp = IBCLR(btyp,13)
              btyp = IBCLR(btyp,14)
         case ('MULTI')
              btyp = IBSET(btyp,13)
              btyp = IBCLR(btyp,14)

         case ('DATA')
         ! bknat2 = ibits ( btyp, 11, 2 )
         ! traitement de bknat2
         ! 00 = donnees
         ! 01 = info
         ! 10 = descripteur 3-D
         ! 11 = marqueurs
         !
              btyp = IBCLR(btyp,11)
              btyp = IBCLR(btyp,12)
         case ('INFO')
              btyp = IBSET(btyp,11)
              btyp = IBCLR(btyp,12)
         case ('3-D')
              btyp = IBCLR(btyp,11)
              btyp = IBSET(btyp,12)
         case ('MRQR')
              btyp = IBSET(btyp,11)
              btyp = IBSET(btyp,12)

         case ('SFC')
         ! bktyp1 = ibits ( btyp, 10, 1 )
         ! traitement de bktyp1
         ! 00 = SFC
         ! 01 = ALT
         !
              btyp = IBCLR(btyp,10)
         case ('ALT')
              btyp = IBSET(btyp,10)

         case ( 'OBS_ADE')
         ! bktyp2 = ibits ( btyp,  4, 6 )
         ! traitement de bktyp2
         ! 0 = observations (ADE)
         ! 1 = observations brutes (NON DECODEES)
         ! ...............
         ! ...............
         ! 22 = donnees SSMI
         ! 23 @ 63 = EN RESERVE
         !
            !>> 9 8 7 6 5 4
            !>> 0 0 0 0 0 0
            do k = 4,9
              btyp = IBCLR(btyp,k)
            enddo
         case ( 'OBS_BRUTES')
            !>> 9 8 7 6 5 4
            !>> 0 0 0 0 0 1
            do k = 4,9
              btyp = IBCLR(btyp,k)
            enddo
            btyp = IBSET(btyp,4)
         case ( 'DERI_ALT_GLO')
            !>> 9 8 7 6 5 4
            !>> 0 0 0 0 1 0
            do k = 4,9
               btyp = IBCLR(btyp,k)
            enddo
            btyp = IBSET(btyp,5)
         case ( 'DERI_ALT_REG')
            !>> 9 8 7 6 5 4
            !>> 0 0 0 0 1 1
            do k = 4,9
              btyp = IBCLR(btyp,k)
            enddo
            btyp = IBSET(btyp,4)
            btyp = IBSET(btyp,5)
         case ( 'DERI_SFC_GLO')
            !>> 9 8 7 6 5 4
            !>> 0 0 0 1 0 0
            do k = 4,9
              btyp = IBCLR(btyp,k)
            enddo
            btyp = IBSET(btyp,6)
         case ( 'DERI_SFC_REG')
            !>> 9 8 7 6 5 4
            !>> 0 0 0 1 0 1
            do k = 4,9
              btyp = IBCLR(btyp,k)
            enddo
            btyp = IBSET(btyp,6)
            btyp = IBSET(btyp,4)
         case ( 'POST_ALT_GLO')
            !>> 9 8 7 6 5 4
            !>> 0 0 0 1 1 0
            do k = 4,9
              btyp = IBCLR(btyp,k)
            enddo
            btyp = IBSET(btyp,6)
            btyp = IBSET(btyp,5)
         case ( 'POST_ALT_REG')
            !>> 9 8 7 6 5 4
            !>> 0 0 0 1 1 1
            do k = 4,9
              btyp = IBCLR(btyp,k)
            enddo
            btyp = IBSET(btyp,6)
            btyp = IBSET(btyp,5)
            btyp = IBSET(btyp,4)
         case ( 'POST_SFC_GLO')
            !>> 9 8 7 6 5 4
            !>> 0 0 1 0 0 0
            do k = 4,9
              btyp = IBCLR(btyp,k)
            enddo
            btyp = IBSET(btyp,7)
         case ( 'POST_SFC_REG')
            !>> 9 8 7 6 5 4
            !>> 0 0 1 0 0 1
            do k = 4,9
              btyp = IBCLR(btyp,k)
            enddo
            btyp = IBSET(btyp,7)
            btyp = IBSET(btyp,4)
         case ( 'PRFL_ALT_GLO')
            !>> 9 8 7 6 5 4
            !>> 0 0 1 0 1 0
            do k = 4,9
              btyp = IBCLR(btyp,k)
            enddo
            btyp = IBSET(btyp,7)
            btyp = IBSET(btyp,5)
         case ( 'PRFL_ALT_REG')
            !>> 9 8 7 6 5 4
            !>> 0 0 1 0 1 1
            do k = 4,9
              btyp = IBCLR(btyp,k)
            enddo
            btyp = IBSET(btyp,7)
            btyp = IBSET(btyp,5)
            btyp = IBSET(btyp,4)
         case ( 'RESERVED')
            !>> 9 8 7 6 5 4
            !>> 0 0 1 1 0 0
            do k = 4,9
              btyp = IBCLR(btyp,k)
            enddo
            btyp = IBSET(btyp,7)
            btyp = IBSET(btyp,6)
!         case ( 'RESERVED')
!         !>> 9 8 7 6 5 4
!         !>> 0 0 1 1 0 1
!            do k = 4,9
!              btyp = IBCLR(btyp,k)
!            enddo
!            btyp = IBSET(btyp,7)
!            btyp = IBSET(btyp,6)
!            btyp = IBSET(btyp,4)
         case ( 'ANAL_ALT_GLO')
            !>> 9 8 7 6 5 4
            !>> 0 0 1 1 1 0
            do k = 4,9
              btyp = IBCLR(btyp,k)
            enddo
            btyp = IBSET(btyp,7)
            btyp = IBSET(btyp,6)
            btyp = IBSET(btyp,5)
         case ( 'ANAL_ALT_REG')
            !>> 9 8 7 6 5 4
            !>> 0 0 1 1 1 1
            do k = 4,9
              btyp = IBCLR(btyp,k)
            enddo
            btyp = IBSET(btyp,7)
            btyp = IBSET(btyp,6)
            btyp = IBSET(btyp,5)
            btyp = IBSET(btyp,4)
         case ( 'ANAL_SFC_GLO')
            !>> 9 8 7 6 5 4
            !>> 0 1 0 0 0 0
            do k = 4,9
              btyp = IBCLR(btyp,k)
            enddo
            btyp = IBSET(btyp,8)
         case ( 'ANAL_SFC_REG')
            !>> 9 8 7 6 5 4
            !>> 0 1 0 0 0 1
            do k = 4,9
              btyp = IBCLR(btyp,k)
            enddo
            btyp = IBSET(btyp,8)
            btyp = IBSET(btyp,4)
         case ( 'PREV_GLO')
            !>> 9 8 7 6 5 4
            !>> 0 1 0 0 1 0
            do k = 4,9
              btyp = IBCLR(btyp,k)
            enddo
            btyp = IBSET(btyp,8)
            btyp = IBSET(btyp,5)
         case ( 'PREV_REG')
            !>> 9 8 7 6 5 4
            !>> 0 1 0 0 1 1
            do k = 4,9
              btyp = IBCLR(btyp,k)
            enddo
            btyp = IBSET(btyp,8)
            btyp = IBSET(btyp,5)
            btyp = IBSET(btyp,4)
         case ( 'STAT_PENSE')
            !>> 9 8 7 6 5 4
            !>> 0 1 0 1 0 0
            do k = 4,9
              btyp = IBCLR(btyp,k)
            enddo
            btyp = IBSET(btyp,8)
            btyp = IBSET(btyp,6)
         case ( 'STAT_KALMAN')
            !>> 9 8 7 6 5 4
            !>> 0 1 0 1 0 1
            do k = 4,9
              btyp = IBCLR(btyp,k)
            enddo
            btyp = IBSET(btyp,8)
            btyp = IBSET(btyp,6)
            btyp = IBSET(btyp,4)
         case ( 'SSMI')
            !>> 9 8 7 6 5 4
            !>> 0 1 0 1 1 0
            do k = 4,9
              btyp = IBCLR(btyp,k)
            enddo
            btyp = IBSET(btyp,8)
            btyp = IBSET(btyp,6)
            btyp = IBSET(btyp,5)
         case default
         Call setStateToFailure(BURP_ERROR%ERROR, &
             "Failure >> "//btyp_key//" :Unknown btyp_key : set_burp_btyp ")
         if (present(IOSTAT))  IOSTAT = -1
         return
      end select

    endif
    end subroutine

    ! changing flags
    !
    subroutine Set_Burp_Flgs(Flgs_Key,REPORT,FLGS,IOSTAT)
    !
    implicit none
    type(Burp_rpt),optional,intent(inout)         :: REPORT
    integer(kind = int_def),optional,intent(inout):: FLGS,IOSTAT
    character(len = *),intent(in)                 :: Flgs_Key
    integer(kind = int_def)::error, k,my_flgs


    error = burp_noerr
    if (present(report)) then
      call BURP_Get_Property(report, &
           FLGS = my_flgs, IOSTAT=error)
      if (present(IOSTAT))  IOSTAT = error
      if (error /= burp_noerr) return

      select case (flgs_key)
          case ( 'OBS_DESSUS_TERRE')
               my_flgs = IBSET(my_flgs,0)
          case ( 'VENT_SFC_UTILISE')
               my_flgs = IBSET(my_flgs,1)
          case ( 'CORRECTION_RADIATION')
               my_flgs = IBSET(my_flgs,2)
          case ( 'CORRECTION_POS_BATEAU')
               my_flgs = IBSET(my_flgs,3)
          case ( 'STN_HORS_INTERET')
               my_flgs = IBSET(my_flgs,5)
          case ( 'STN_REJET_PAR_AO')
               my_flgs = IBSET(my_flgs,6)
          case ( 'STN_LISTE_NOIRE')
               my_flgs = IBSET(my_flgs,7)
          case ( 'STN_A_EVALUER')
               my_flgs = IBSET(my_flgs,8)
          case ( 'STN_DOUTEUSE')
               my_flgs = IBSET(my_flgs,9)
          case ( 'ENREG_CONT_DON_OBSERVEES')
               my_flgs = IBSET(my_flgs,10)
          case ( 'ENREG_CONT_DON_DERIVEES')
               my_flgs = IBSET(my_flgs,11)
          case ( 'ENREG_CONT_DON_VUES_PAR_AO')
               my_flgs = IBSET(my_flgs,12)
          case ( 'ENREG_CONT_DON_RESIDUS')
               my_flgs = IBSET(my_flgs,13)
          case ( 'PART_RADAT_SATEM_SATOB')
               my_flgs = IBSET(my_flgs,14)
          case ( 'PART_A_TEMP_PILOT_SATEM_SATOB')
               my_flgs = IBSET(my_flgs,15)
          case ( 'PART_B_TEMP_PILOT_SATEM')
               my_flgs = IBSET(my_flgs,16)
          case ( 'PART_C_TEMP_PILOT_SATEM')
               my_flgs = IBSET(my_flgs,17)
          case ( 'PART_D_TEMP_PILOT_SATEM')
               my_flgs = IBSET(my_flgs,18)
          case ( 'ENREG_CONT_DON_ANALYSEES')
               my_flgs = IBSET(my_flgs,19)
          case ('ENREG_CONT_DON_PREVUES')
               my_flgs = IBSET(my_flgs,20)
          case ( 'ENREG_CONT_DON_VERIFIEES')
               my_flgs = IBSET(my_flgs,21)
          case default
          Call setStateToFailure(BURP_ERROR%ERROR, &
             "Failure >> "//flgs_key//" :Unknown flgs_key : set_burp_flgs ")
          if (present(IOSTAT))  IOSTAT = -1
          return
      end select
      call BURP_Set_Property(report, &
                FLGS = my_flgs, IOSTAT=error)
      if (present(IOSTAT))  IOSTAT = error
      if (error /= burp_noerr) return
    endif
    if (present(FLGS)) then
      select case (flgs_key)
          case ( 'OBS_DESSUS_TERRE')
               flgs = IBSET(flgs,0)
          case ( 'VENT_SFC_UTILISE')
               flgs = IBSET(flgs,1)
          case ( 'CORRECTION_RADIATION')
               flgs = IBSET(flgs,2)
          case ( 'CORRECTION_POS_BATEAU')
               flgs = IBSET(flgs,3)
          case ( 'STN_HORS_INTERET')
               flgs = IBSET(flgs,5)
          case ( 'STN_REJET_PAR_AO')
               flgs = IBSET(flgs,6)
          case ( 'STN_LISTE_NOIRE')
               flgs = IBSET(flgs,7)
          case ( 'STN_A_EVALUER')
               flgs = IBSET(flgs,8)
          case ( 'STN_DOUTEUSE')
               flgs = IBSET(flgs,9)
          case ( 'ENREG_CONT_DON_OBSERVEES')
               flgs = IBSET(flgs,10)
          case ( 'ENREG_CONT_DON_DERIVEES')
               flgs = IBSET(flgs,11)
          case ( 'ENREG_CONT_DON_VUES_PAR_AO')
               flgs = IBSET(flgs,12)
          case ( 'ENREG_CONT_DON_RESIDUS')
               flgs = IBSET(flgs,13)
          case ( 'PART_RADAT_SATEM_SATOB')
               flgs = IBSET(flgs,14)
          case ( 'PART_A_TEMP_PILOT_SATEM_SATOB')
               flgs = IBSET(flgs,15)
          case ( 'PART_B_TEMP_PILOT_SATEM')
               flgs = IBSET(flgs,16)
          case ( 'PART_C_TEMP_PILOT_SATEM')
               flgs = IBSET(flgs,17)
          case ( 'PART_D_TEMP_PILOT_SATEM')
               flgs = IBSET(flgs,18)
          case ( 'ENREG_CONT_DON_ANALYSEES')
               flgs = IBSET(flgs,19)
          case ('ENREG_CONT_DON_PREVUES')
               flgs = IBSET(flgs,20)
          case ( 'ENREG_CONT_DON_VERIFIEES')
               flgs = IBSET(flgs,21)
          case default
          Call setStateToFailure(BURP_ERROR%ERROR, &
             "Failure >> "//flgs_key//" :Unknown flgs_key : set_burp_flgs ")
          if (present(IOSTAT))  IOSTAT = -1
          return
      end select
    endif
    return
    end subroutine

    subroutine Set_Burp_Runn(Runn_Key,REPORT,RUNN,IOSTAT)
    ! Set_Burp_Runn subroutine
    !
    ! affecter les bits correspondant du runn
    ! en fonction de la cle
    !
    ! decomposition de la runn
    !
    !   runn    =      rnat        |       rtyp
    !   8  bits =      2 bits      |       6 bits
    !           =  rnat1  | rnat2  | rtyp1  | rtyp2
    !           =  1 bits | 1 bits | 3 bit  | 3 bits
    !
    !
    ! rnat1 = ibits ( my_runn, 7, 1 )
    ! rnat2 = ibits ( my_runn, 6, 1 )
    ! rtyp1 = ibits ( my_runn, 3, 3 )
    ! rtyp2 = ibits ( my_runn, 0, 3 )

    implicit none
    type(Burp_Rpt),optional,intent(inout)         :: REPORT
    integer(kind = int_def),optional,intent(inout):: runn,IOSTAT
    character(len = *),intent(in) :: runn_Key
    integer(kind = int_def)       :: k
    integer(kind = int_def) :: rnat1,rnat2
    integer(kind = int_def) :: rtyp1,rtyp2
    integer(kind = int_def) :: my_runn,error

    error = burp_noerr
    if (present(REPORT)) then
          call BURP_Get_Property(REPORT, &
                RUNN = my_runn, IOSTAT=error)
          if (present(IOSTAT))  IOSTAT = error
          if (error /= burp_noerr) return

     select case (runn_key)

         ! rnat1 = ibits ( my_runn, 7, 1 )
         ! traitement de rnat1
         !
         ! 0 = REGUL
         ! 1 = PARAL
         !
         case ('REGUL')
              my_runn = IBCLR(my_runn,7)
         case ('PARAL')
              my_runn = IBSET(my_runn,7)


         ! rnat2 = ibits ( my_runn, 6, 1 )
         ! traitement de rnat2
         ! 0 = GLO
         ! 1 = REG
         case ('GLO')
              my_runn = IBCLR(my_runn,6)
         case ('REG')
              my_runn = IBSET(my_runn,6)


         ! rtyp1 = ibits ( my_runn, 3, 3 )
         !traitement de rtyp1
         !
         !000 = AO_ALT_PRELI
         !001 = AO_ALT_COMPL
         !010 = AO_ALT_FINAL
         !011 = AO_SFC_SPECI
         !100 = AO_SFC_PRELI
         !101 = AO_SFC_COMPL
         !110 = AO_SFC_FINAL
         !111 = RESERVED
         !
         case ( 'AO_ALT_PRELI')
            ! bits :  5 4 3
            ! etat :  0 0 0
            do k = 3,5
              my_runn = IBCLR(my_runn,k)
            enddo
         case ( 'AO_ALT_COMPL')
            ! bits :  5 4 3
            ! etat :  0 0 1
            do k = 3,5
              my_runn = IBCLR(my_runn,k)
            enddo
            my_runn = IBSET(my_runn,3)
         case ( 'AO_ALT_FINAL')
            ! bits :  5 4 3
            ! etat :  0 1 0
            do k = 3,5
              my_runn = IBCLR(my_runn,k)
            enddo
            my_runn = IBSET(my_runn,4)
         case ( 'AO_SFC_SPECI')
            ! bits :  5 4 3
            ! etat :  0 1 1
            do k = 3,5
              my_runn = IBCLR(my_runn,k)
            enddo
            my_runn = IBSET(my_runn,3)
            my_runn = IBSET(my_runn,4)
         case ( 'AO_SFC_PRELI')
            ! bits :  5 4 3
            ! etat :  1 0 0
            do k = 3,5
              my_runn = IBCLR(my_runn,k)
            enddo
            my_runn = IBSET(my_runn,5)
         case ( 'AO_SFC_COMPL')
            ! bits :  5 4 3
            ! etat :  1 0 1
            do k = 3,5
              my_runn = IBCLR(my_runn,k)
            enddo
            my_runn = IBSET(my_runn,3)
            my_runn = IBSET(my_runn,5)
         case ( 'AO_SFC_FINAL')
            ! bits :  5 4 3
            ! etat :  1 1 0
            do k = 3,5
              my_runn = IBCLR(my_runn,k)
            enddo
            my_runn = IBSET(my_runn,4)
            my_runn = IBSET(my_runn,5)


         ! rtyp2 = ibits ( my_runn, 0, 3 )
         !traitement de rtyp2
         !000 = 00Z
         !001 = 03Z
         !010 = 06Z
         !011 = 09Z
         !100 = 12Z
         !101 = 15Z
         !110 = 18Z
         !111 = 21Z
         case ( '00Z')
            ! bits :  2 1 0
            ! etat :  0 0 0
            do k = 0,2
              my_runn = IBCLR(my_runn,k)
            enddo
         case ( '03Z')
            ! bits :  2 1 0
            ! etat :  0 0 1
            do k = 0,2
              my_runn = IBCLR(my_runn,k)
            enddo
            my_runn = IBSET(my_runn,0)
         case ( '06Z')
            ! bits :  2 1 0
            ! etat :  0 1 0
            do k = 0,2
              my_runn = IBCLR(my_runn,k)
            enddo
            my_runn = IBSET(my_runn,1)
         case ( '09Z')
            ! bits :  2 1 0
            ! etat :  0 1 1
            do k = 0,2
              my_runn = IBCLR(my_runn,k)
            enddo
            my_runn = IBSET(my_runn,0)
            my_runn = IBSET(my_runn,1)
         case ( '12Z')
            ! bits :  2 1 0
            ! etat :  1 0 0
            do k = 0,2
              my_runn = IBCLR(my_runn,k)
            enddo
            my_runn = IBSET(my_runn,2)
         case ( '15Z')
            ! bits :  2 1 0
            ! etat :  1 0 1
            do k = 0,2
              my_runn = IBCLR(my_runn,k)
            enddo
            my_runn = IBSET(my_runn,2)
            my_runn = IBSET(my_runn,0)
         case ( '18Z')
            ! bits :  2 1 0
            ! etat :  1 1 0
            do k = 0,2
              my_runn = IBCLR(my_runn,k)
            enddo
            my_runn = IBSET(my_runn,2)
            my_runn = IBSET(my_runn,1)
         case ( '21Z')
            ! bits :  2 1 0
            ! etat :  1 1 1
            do k = 0,2
              my_runn = IBCLR(my_runn,k)
            enddo
            my_runn = IBSET(my_runn,2)
            my_runn = IBSET(my_runn,1)
            my_runn = IBSET(my_runn,0)
         case default
         Call setStateToFailure(BURP_ERROR%ERROR, &
             "Failure >> "//Runn_Key//" :Unknown runn_key : set_burp_runn ")
         if (present(IOSTAT))  IOSTAT = -1
         return
      end select
      call BURP_Set_Property(REPORT, &
           RUNN = my_runn, IOSTAT=error)
      if (present(IOSTAT))  IOSTAT = error
      if (error /= burp_noerr) return
    endif
    if (present(RUNN)) then
     select case (runn_key)

         ! rnat1 = ibits ( runn, 7, 1 )
         ! traitement de rnat1
         !
         ! 0 = REGUL
         ! 1 = PARAL
         !
         case ('REGUL')
              runn = IBCLR(runn,7)
         case ('PARAL')
              runn = IBSET(runn,7)


         ! rnat2 = ibits ( runn, 6, 1 )
         ! traitement de rnat2
         ! 0 = GLO
         ! 1 = REG
         case ('GLO')
              runn = IBCLR(runn,6)
         case ('REG')
              runn = IBSET(runn,6)


         ! rtyp1 = ibits ( runn, 3, 3 )
         !traitement de rtyp1
         !
         !000 = AO_ALT_PRELI
         !001 = AO_ALT_COMPL
         !010 = AO_ALT_FINAL
         !011 = AO_SFC_SPECI
         !100 = AO_SFC_PRELI
         !101 = AO_SFC_COMPL
         !110 = AO_SFC_FINAL
         !111 = RESERVED
         !
         case ( 'AO_ALT_PRELI')
            ! bits :  5 4 3
            ! etat :  0 0 0
            do k = 3,5
              runn = IBCLR(runn,k)
            enddo
         case ( 'AO_ALT_COMPL')
            ! bits :  5 4 3
            ! etat :  0 0 1
            do k = 3,5
              runn = IBCLR(runn,k)
            enddo
            runn = IBSET(runn,3)
         case ( 'AO_ALT_FINAL')
            ! bits :  5 4 3
            ! etat :  0 1 0
            do k = 3,5
              runn = IBCLR(runn,k)
            enddo
            runn = IBSET(runn,4)
         case ( 'AO_SFC_SPECI')
            ! bits :  5 4 3
            ! etat :  0 1 1
            do k = 3,5
              runn = IBCLR(runn,k)
            enddo
            runn = IBSET(runn,3)
            runn = IBSET(runn,4)
         case ( 'AO_SFC_PRELI')
            ! bits :  5 4 3
            ! etat :  1 0 0
            do k = 3,5
              runn = IBCLR(runn,k)
            enddo
            runn = IBSET(runn,5)
         case ( 'AO_SFC_COMPL')
            ! bits :  5 4 3
            ! etat :  1 0 1
            do k = 3,5
              runn = IBCLR(runn,k)
            enddo
            runn = IBSET(runn,3)
            runn = IBSET(runn,5)
         case ( 'AO_SFC_FINAL')
            ! bits :  5 4 3
            ! etat :  1 1 0
            do k = 3,5
              runn = IBCLR(runn,k)
            enddo
            runn = IBSET(runn,4)
            runn = IBSET(runn,5)


         ! rtyp2 = ibits ( runn, 0, 3 )
         !traitement de rtyp2
         !000 = 00Z
         !001 = 03Z
         !010 = 06Z
         !011 = 09Z
         !100 = 12Z
         !101 = 15Z
         !110 = 18Z
         !111 = 21Z
         case ( '00Z')
            ! bits :  2 1 0
            ! etat :  0 0 0
            do k = 0,2
              runn = IBCLR(runn,k)
            enddo
         case ( '03Z')
            ! bits :  2 1 0
            ! etat :  0 0 1
            do k = 0,2
              runn = IBCLR(runn,k)
            enddo
            runn = IBSET(runn,0)
         case ( '06Z')
            ! bits :  2 1 0
            ! etat :  0 1 0
            do k = 0,2
              runn = IBCLR(runn,k)
            enddo
            runn = IBSET(runn,1)
         case ( '09Z')
            ! bits :  2 1 0
            ! etat :  0 1 1
            do k = 0,2
              runn = IBCLR(runn,k)
            enddo
            runn = IBSET(runn,0)
            runn = IBSET(runn,1)
         case ( '12Z')
            ! bits :  2 1 0
            ! etat :  1 0 0
            do k = 0,2
              runn = IBCLR(runn,k)
            enddo
            runn = IBSET(runn,2)
         case ( '15Z')
            ! bits :  2 1 0
            ! etat :  1 0 1
            do k = 0,2
              runn = IBCLR(runn,k)
            enddo
            runn = IBSET(runn,2)
            runn = IBSET(runn,0)
         case ( '18Z')
            ! bits :  2 1 0
            ! etat :  1 1 0
            do k = 0,2
              runn = IBCLR(runn,k)
            enddo
            runn = IBSET(runn,2)
            runn = IBSET(runn,1)
         case ( '21Z')
            ! bits :  2 1 0
            ! etat :  1 1 1
            do k = 0,2
              runn = IBCLR(runn,k)
            enddo
            runn = IBSET(runn,2)
            runn = IBSET(runn,1)
            runn = IBSET(runn,0)
         case default
         Call setStateToFailure(BURP_ERROR%ERROR, &
             "Failure >> "//Runn_Key//" :Unknown runn_key : set_burp_runn ")
         if (present(IOSTAT))  IOSTAT = -1
         return
      end select
    endif

    end subroutine Set_Burp_Runn

end module burp_btyp_runn_flgs
