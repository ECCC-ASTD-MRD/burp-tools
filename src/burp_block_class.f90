!
module burp_block_class
     use burp_constants
     use errormessages
     use librmn_declaration
     use object_initialization
     use conversion_to_string
     implicit none

     !!
     !! define burp_block type
     !!
     type burp_block
       integer(kind=int_def) :: bkno
       integer(kind=int_def) :: nele
       integer(kind=int_def) :: nval
       integer(kind=int_def) :: nt
       integer(kind=int_def) :: bfam
       integer(kind=int_def) :: bdesc
       integer(kind=int_def) :: btyp
       integer(kind=int_def) :: bknat
       integer(kind=int_def) :: bktyp
       integer(kind=int_def) :: bkstp
       integer(kind=int_def) :: nbit
       integer(kind=int_def) :: bit0
       integer(kind=int_def) :: oars
       integer(kind=int_def) :: datyp
       integer(kind=int_def) :: rpt_handle 
       character(len=4)      :: store_mode
       logical               :: convert
       integer(kind=int_def),dimension(:),    pointer :: lstele 
       integer(kind=int_def),dimension(:),    pointer :: dlstele
       integer(kind=int_def),dimension(:,:,:),pointer :: tblval 
       real,                 dimension(:,:,:),pointer :: rval 
       type(t_init)                                   :: init

     end type burp_block

     type burp_err_status
        type (errormessage)           :: error
        type(t_init)                  :: init
     end type
     type(burp_err_status), save      :: burp_error

     !!
     !! interfaces
     !!

     interface burp_new
       module procedure init_burp_block_to_write
     end interface

     interface burp_to_stdout
       module procedure print_burp_block
     end interface

     interface burp_get_property
       module procedure  block_get_property
     end interface

     interface burp_set_property
       module procedure  block_set_property
     end interface

     interface assignment(=)
       module procedure block_affectation
     end interface

     interface operator(.mrqr.)
       module procedure make_block_marqueur
     end interface

     interface operator(.clear.)
       module procedure make_block_clear
     end interface
     contains

       !!
       !! constructor
       !!
       subroutine init_burp_block(this,iostat)
        type (burp_block),    intent(inout)        :: this
        integer(kind=int_def),optional,intent(out) :: iostat
        integer(kind=int_def)                      :: error
        error = burp_noerr


        call initialize(this%init)
        nullify(this%lstele)
        nullify(this%dlstele)
        nullify(this%tblval)
        nullify(this%rval)

        this%bkno        = 0
        this%nele        = 0
        this%nval        = 0
        this%nt          = 1
        this%bfam        = 0
        this%bdesc       = 0
        this%btyp        = 0
        this%bknat       = 0
        this%bktyp       = 0
        this%bkstp       = 0
        this%nbit        = 1
        this%bit0        = 0
        this%oars        = 0
        this%datyp       = 4
        this%rpt_handle  = 0
        this%store_mode  =  'int'
        this%convert     = .TRUE.

       if (present(iostat))  iostat = error
       end subroutine init_burp_block

       !!
       !! constructor for anew block
       !!
       subroutine init_burp_block_to_write(this,           &
        nele        ,                                      &
        nval        ,                                      &
        nt          ,                                      &
        bfam        ,                                      &
        bdesc       ,                                      &
        btyp        ,                                      &
        bknat       ,                                      &
        bktyp       ,                                      &
        bkstp       ,                                      &
        nbit        ,                                      &
        bit0        ,                                      &
        oars        ,                                      &
        datyp       ,                                      &
        rpt_handle  ,                                      &
        store_mode  ,                                      &
        convert     ,                                      &
        iostat)

        type (burp_block),    intent(inout)        :: this
        integer(kind=int_def),optional,intent(out) :: iostat
        integer(kind=int_def)                      :: error
        integer(kind=int_def),          intent(in) :: nele
        integer(kind=int_def),          intent(in) :: nval
        integer(kind=int_def),          intent(in) :: nt
        integer(kind=int_def),optional, intent(in) :: bfam
        integer(kind=int_def),optional, intent(in) :: bdesc
        integer(kind=int_def),optional, intent(in) :: btyp
        integer(kind=int_def),optional, intent(in) :: bknat
        integer(kind=int_def),optional, intent(in) :: bktyp
        integer(kind=int_def),optional, intent(in) :: bkstp
        integer(kind=int_def),optional, intent(in) :: nbit
        integer(kind=int_def),optional, intent(in) :: bit0
        integer(kind=int_def),optional, intent(in) :: oars
        integer(kind=int_def),optional, intent(in) :: datyp
        integer(kind=int_def),optional, intent(in) :: rpt_handle
        character(len =4),optional,intent(in)      :: store_mode
        logical, optional,intent(in)               :: convert
        logical:: all_present,un_seul,valide

        error = burp_noerr

        all_present = .false.
        un_seul     = .false.
        valide    = .true.
        if (present(bknat) .and. present(bktyp) .and. present(bkstp))&
           all_present = .true.
        if (present(bknat) .or. present(bktyp) .or. present(bkstp))  &
           un_seul = .true.
        if (all_present) then
            if ((bknat< 0) .or. (bktyp<0) .or.(bkstp<0).or.  &
                (bknat> 15) .or. (bktyp>127) .or.(bkstp>15)) then
               valide=.false.
               error = -1
               call setstatetofailure(burp_error%error, &
               "failure >> check 0<=bknat<=15 0<=btyp <=127 0<=bkstp<=15 "//" &
               &trace - init_burp_to_write.")
               if (present(iostat))  iostat = error
               return
             endif
        endif


        if (nt*nele*nval <= 0 ) then
           error = -1
           call setstatetofailure(burp_error%error, &
           "failure >> une des valures nt|nval|nele est <=0:"//" &
           &trace - init_burp_to_write.")
           if (present(iostat))  iostat = error
           return
        endif


        if (.not.is_init(this%init)) then
           call init_burp_block(this,iostat=error)
        else
           call free_burp_block(this,iostat = error)
        end if
        if (error /= burp_noerr) return
        this%nele        =  nele
        this%nval        =  nval
        this%nt          =  nt
        if (present(convert))  this%convert = convert
        if (present(datyp))    this%datyp = datyp

        allocate(this%lstele(this%nele),stat  = error)
        allocate(this%dlstele(this%nele),stat = error )
        allocate(this%tblval(this%nele,this%nval,this%nt),stat=error)
        if (error /= 0) then
           if (present(iostat))  iostat = error
           call setstatetofailure(burp_error%error, &
           "failure >> to allocate memory"//" &
           &trace - init_burp_to_write.")
           return
        endif
        this%lstele(:)     = -1
        this%dlstele(:)    = -1
        this%tblval(:,:,:) = -1

        ! voir si on demande de travailler en mode conversion, ie l'usager
        ! veut rentrer des valeurs reelles, donc necessite de faire
        ! des conversions de reel a int suivant les tables burp.
        ! mettre a jour la propriete convert a la valeur entree s'il ya lieu
        ! Si le datyp == 6 ,ie on a affaire avec des burp ou ons tore
        ! directement des reels, alors allouer le tableau des reels


        if (this%convert .or. (this%datyp == 6)) then
           allocate(this%rval(this%nele,this%nval,this%nt),  stat=error)
           if (error /= 0) then
              if (present(iostat))  iostat = error
              call setstatetofailure(burp_error%error, &
              "failure >> to allocate memory"//" &
              &trace - init_burp_to_write.")
              return
           endif
           this%rval(:,:,:)   = -1.0
        end if


        !construire le btyp
        if (all_present ) then
            this%bknat       =  bknat
            this%bktyp       =  bktyp
            this%bkstp       =  bkstp
            this%btyp =0
            this%btyp = ior(ishft(this%bknat,11),ishft(this%bktyp,4))
            this%btyp = ior(this%btyp, this%bkstp)

        endif



        if (present(bfam       )) this%bfam        =  bfam
        if (present(bdesc      )) this%bdesc       =  bdesc
        if (present(btyp       )) this%btyp        =  btyp
        if (present(nbit       )) this%nbit        =  nbit
        if (present(bit0       )) this%bit0        =  bit0
        if (present(oars       )) this%oars        =  oars
        if (present(rpt_handle )) this%rpt_handle  =  rpt_handle
        if (present(store_mode )) this%store_mode  =  store_mode

        if (present(iostat))  iostat = error
       end subroutine init_burp_block_to_write

       !!
       !! printing properties -debug-
       !!
       subroutine print_burp_block(this,convert,iostat)
        integer(kind=int_def),optional,intent(inout):: iostat
        type (burp_block), intent (inout) :: this
        logical,optional,intent(in) :: convert
        integer(kind=int_def)       :: i ,j ,k,error
        logical                     :: conv

        error =burp_noerr
        if (.not.is_init(this%init)) then
            call init_burp_block(this,error)    ! initialize block
        endif
        conv = .false.

        if (present(convert)) conv = convert
        ! si cas des reels qui sont stores directement dans brp
        if (this%datyp == 6) then
           conv = .true.
        elseif (.not.this%convert) then
            if (conv) then
                write(*,*) 'your are working in mode convert&
                & false, conversion not done'
                conv = .false.
            end if
        end if


        !
        ! output like liburp (jose programm)
        !
        print *, "  "
        print 100, " blkno  = ",this%bkno,&
              " nele   = ", this%nele, " nval   = ", this%nval ,    &
              " nt     = ", this%nt,   " bfam   = ", this%bfam

        print 100," bdesc  = ",this%bdesc,&
              " btyp   = ", this%btyp, " nbit   = ", this%nbit,     &
              " bit0   = ", this%bit0, " datyp  = ", this%datyp
        100 format (a10,i10,4(a10,i7))
        print *, " "



        do k = 1, this%nt
           if ( this%nt /= 1) then
              print *, "observation  " // cvt(k) // "/" // cvt(this%nt)
           end if
           write(*,fmt = '(a)', advance = 'no')  " lstele = "
           do i = 1 , this%nele
                if ((modulo(i-1,7) == 0) .and. (i /= 1) .and.        &
                   (i < this%nele)) then
                   write(*,fmt='(/a,i10)', advance = 'no')           &
                               "          ",this%dlstele(i)
                else
                   write(*,fmt= '(i10)', advance = 'no') this%dlstele(i)
                end if
           end do
           print *, " "
           if (conv) then
              ! si datyp == 6 utilser le format G15.6
              if (this%datyp == 6) then
               do j = 1, this%nval
                        write(*,fmt = '(a)', advance = 'no')  " tblval = "
                        do i = 1 , this%nele
                           if ((modulo(i-1,7) == 0) .and. (i /= 1) .and.  &
                              (i < this%nele)) then
                                 write(*,fmt= '(/a,G15.6)', advance = 'no') &
                                    "          ", this%rval(i,j,k)
                           else
                                 write(*,fmt= '(G15.6)',  advance = 'no')   &
                                       this%rval(i,j,k)
                           end if
                        end do
               print *, " "
               end do
            else
               do j = 1, this%nval
                        write(*,fmt = '(a)', advance = 'no')  " tblval = "
                        do i = 1 , this%nele
                           if ((modulo(i-1,7) == 0) .and. (i /= 1) .and.  &
                              (i < this%nele)) then
                                 write(*,fmt= '(/a,f10.2)', advance = 'no') &
                                    "          ", this%rval(i,j,k)
                           else
                                 write(*,fmt= '(f10.2)',  advance = 'no')   &
                                       this%rval(i,j,k)
                           end if
                        end do
               print *, " "
               end do
            end if
           else
              do j = 1, this%nval
                      write(*,fmt = '(a)', advance = 'no')  " tblval = "
                      do i = 1 , this%nele
                          if ((modulo(i-1,7) == 0) .and. (i /= 1) .and.  &
                            (i < this%nele)) then
                              write(*,fmt= '(/a,i10)', advance = 'no')   &
                                   "          ", this%tblval(i,j,k)
                          else
                              write(*,fmt= '(i10)',  advance = 'no')     &
                                    this%tblval(i,j,k)
                          end if
                      end do
              print *, " "
              end do
           endif
           print *, " "
        end do
        print *, " "

        if (present(iostat))  iostat = error

       end subroutine print_burp_block

       !!
       !! destructor
       !!
       subroutine free_burp_block(this,iostat)
        type (burp_block),intent(inout)           :: this
        integer(kind=int_def),optional,intent(out):: iostat
        integer(kind=int_def)                     :: error
        error = burp_noerr


        if (.not.is_init(this%init)) then
               call init_burp_block(this,error)    ! initialize block
               if (present(iostat))  iostat = error
               if (error /= burp_noerr) return
        end if
        if (associated(this%lstele)) then
        deallocate(this%lstele,stat=error)
         nullify(this%lstele)
        endif
        if (associated(this%dlstele)) then
         deallocate(this%dlstele,stat=error)
         nullify(this%dlstele)
        end if
        if (associated(this%tblval)) then
         deallocate(this%tblval, stat=error)
         nullify(this%tblval)
        endif
        if (associated(this%rval)) then
         deallocate(this%rval,stat=error)
         nullify(this%rval)
        endif
        if (error /= burp_noerr) then
           call setstatetofailure(burp_error%error, &
           "failure >> deallocate memory -lstel/dlst/tblva - free_burp_block")
           if (present(iostat))  iostat = error
           return
        endif

        if (present(iostat))  iostat = error
        end subroutine free_burp_block

       !!
       !! multi elemental destructor
       !!
       subroutine free_all_burp_block(b1,b2,b3,b4,b5,&
                                      b6,b7,b8,b9,b10,iostat)
        type (burp_block),intent(inout)           :: b1
        type (burp_block),optional,intent(inout)  :: b2
        type (burp_block),optional,intent(inout)  :: b3
        type (burp_block),optional,intent(inout)  :: b4
        type (burp_block),optional,intent(inout)  :: b5
        type (burp_block),optional,intent(inout)  :: b6
        type (burp_block),optional,intent(inout)  :: b7
        type (burp_block),optional,intent(inout)  :: b8
        type (burp_block),optional,intent(inout)  :: b9
        type (burp_block),optional,intent(inout)  :: b10
        integer(kind=int_def),optional,intent(out):: iostat
        integer(kind=int_def)                     :: error

        error = burp_noerr
        call free_burp_block(b1, error)
        if (error /= burp_noerr) then
           if (present(iostat))  iostat = error
           return
        endif
        if (present(b2)) call free_burp_block(b2, error)
        if (error /= burp_noerr) then
           if (present(iostat))  iostat = error
           return
        endif
        if (present(b3)) call free_burp_block(b3, error)
        if (error /= burp_noerr) then
           if (present(iostat))  iostat = error
           return
        endif
        if (present(b4)) call free_burp_block(b4, error)
        if (error /= burp_noerr) then
           if (present(iostat))  iostat = error
           return
        endif
        if (present(b5)) call free_burp_block(b5, error)
        if (error /= burp_noerr) then
           if (present(iostat))  iostat = error
           return
        endif
        if (present(b6)) call free_burp_block(b6, error)
        if (error /= burp_noerr) then
           if (present(iostat))  iostat = error
           return
        endif
        if (present(b7)) call free_burp_block(b7, error)
        if (error /= burp_noerr) then
           if (present(iostat))  iostat = error
           return
        endif
        if (present(b8)) call free_burp_block(b8, error)
        if (error /= burp_noerr) then
           if (present(iostat))  iostat = error
           return
        endif
        if (present(b9)) call free_burp_block(b9, error)
        if (error /= burp_noerr) then
           if (present(iostat))  iostat = error
           return
        endif
        if (present(b10)) call free_burp_block(b10, error)
        if (error /= burp_noerr) then
           if (present(iostat))  iostat = error
           return
        endif
        if (present(iostat))  iostat = error

        end subroutine free_all_burp_block

       !!
       !! multi elemental initialiseur du type derive
       !!
       subroutine init_all_burp_block(b1,b2,b3,b4,b5,&
                                      b6,b7,b8,b9,b10, iostat)
        type (burp_block),intent(inout)           :: b1
        type (burp_block),optional,intent(inout)  :: b2
        type (burp_block),optional,intent(inout)  :: b3
        type (burp_block),optional,intent(inout)  :: b4
        type (burp_block),optional,intent(inout)  :: b5
        type (burp_block),optional,intent(inout)  :: b6
        type (burp_block),optional,intent(inout)  :: b7
        type (burp_block),optional,intent(inout)  :: b8
        type (burp_block),optional,intent(inout)  :: b9
        type (burp_block),optional,intent(inout)  :: b10
        integer(kind=int_def),optional,intent(out):: iostat
        integer(kind=int_def)                     :: error

        error = burp_noerr
        call init_burp_block(b1, error)
        if (error /= burp_noerr) then
           if (present(iostat))  iostat = error
           return
        endif
        if (present(b2)) call init_burp_block(b2, error)
        if (error /= burp_noerr) then
           if (present(iostat))  iostat = error
           return
        endif
        if (present(b3)) call init_burp_block(b3, error)
        if (error /= burp_noerr) then
           if (present(iostat))  iostat = error
           return
        endif
        if (present(b4)) call init_burp_block(b4, error)
        if (error /= burp_noerr) then
           if (present(iostat))  iostat = error
           return
        endif
        if (present(b5)) call init_burp_block(b5, error)
        if (error /= burp_noerr) then
           if (present(iostat))  iostat = error
           return
        endif
        if (present(b6)) call init_burp_block(b6, error)
        if (error /= burp_noerr) then
           if (present(iostat))  iostat = error
           return
        endif
        if (present(b7)) call init_burp_block(b7, error)
        if (error /= burp_noerr) then
           if (present(iostat))  iostat = error
           return
        endif
        if (present(b8)) call init_burp_block(b8, error)
        if (error /= burp_noerr) then
           if (present(iostat))  iostat = error
           return
        endif
        if (present(b9)) call init_burp_block(b9, error)
        if (error /= burp_noerr) then
           if (present(iostat))  iostat = error
           return
        endif
        if (present(b10)) call init_burp_block(b10, error)
        if (error /= burp_noerr) then
           if (present(iostat))  iostat = error
           return
        endif

        end subroutine 

       !!
       !! getting properties
       !!
       subroutine block_get_property(this,                   &
         bkno        ,                                      &
         nele        ,                                      &
         nval        ,                                      &
         nt          ,                                      &
         bfam        ,                                      &
         bdesc       ,                                      &
         btyp        ,                                      &
         bknat       ,                                      &
         bktyp       ,                                      &
         bkstp       ,                                      &
         nbit        ,                                      &
         bit0        ,                                      &
         oars        ,                                      &
         datyp       ,                                      &
         rpt_handle  ,                                      &
         store_mode  ,                                      &
         convert     ,                                      &
         iostat)

         type (burp_block),intent(in) :: this
         integer(kind=int_def)        :: error

         integer(kind=int_def),optional,intent(out)  :: bkno
         integer(kind=int_def),optional,intent(out)  :: nele
         integer(kind=int_def),optional,intent(out)  :: nval
         integer(kind=int_def),optional,intent(out)  :: nt
         integer(kind=int_def),optional,intent(out)  :: bfam
         integer(kind=int_def),optional,intent(out)  :: bdesc
         integer(kind=int_def),optional,intent(out)  :: btyp
         integer(kind=int_def),optional,intent(out)  :: bknat
         integer(kind=int_def),optional,intent(out)  :: bktyp
         integer(kind=int_def),optional,intent(out)  :: bkstp
         integer(kind=int_def),optional,intent(out)  :: nbit
         integer(kind=int_def),optional,intent(out)  :: bit0
         integer(kind=int_def),optional,intent(out)  :: oars
         integer(kind=int_def),optional,intent(out)  :: datyp
         integer(kind=int_def),optional,intent(out)  :: rpt_handle
         integer(kind=int_def),optional,intent(inout):: iostat
         character(len =4),optional,intent(out)      :: store_mode
         logical, optional,intent(out)               :: convert

         error = burp_noerr

         if (present(bkno ) .or. present(nele) .or. present(nval ) .or.&
             present(nt   ) .or. present(bfam) .or. present(bdesc) .or.&
             present(btyp ) .or. present(bknat).or. present(bktyp) .or.&
             present(bkstp) .or. present(nbit ).or. present(bit0 ) .or.&
             present(oars ) .or. present(rpt_handle) .or.              &
             present(datyp) .or. present(convert)) error = burp_noerr

         if (error /= burp_noerr) then
            if (present(iostat)) iostat = error
            call setstatetofailure(burp_error%error, &
            "failure >> block getting property: what property your looking for?? ")
             return
         endif

         if (present(bkno       )) bkno        =  this%bkno
         if (present(nele       )) nele        =  this%nele
         if (present(nval       )) nval        =  this%nval
         if (present(nt         )) nt          =  this%nt
         if (present(bfam       )) bfam        =  this%bfam
         if (present(bdesc      )) bdesc       =  this%bdesc
         if (present(btyp       )) btyp        =  this%btyp
         if (present(bknat      )) bknat       =  this%bknat
         if (present(bktyp      )) bktyp       =  this%bktyp
         if (present(bkstp      )) bkstp       =  this%bkstp
         if (present(nbit       )) nbit        =  this%nbit
         if (present(bit0       )) bit0        =  this%bit0
         if (present(oars       )) oars        =  this%oars
         if (present(datyp      )) datyp       =  this%datyp
         if (present(rpt_handle )) rpt_handle  =  this%rpt_handle
         if (present(store_mode )) store_mode  =  this%store_mode
         if (present(convert    )) convert     =  this%convert
         if (present(iostat)) iostat = error


       end subroutine block_get_property

       !!
       !! setting properties
       !!
       subroutine block_set_property(this,                   &
         bfam        ,                                      &
         bdesc       ,                                      &
         btyp        ,                                      &
         nbit        ,                                      &
         bit0        ,                                      &
         oars        ,                                      &
         datyp       ,                                      &
         bknat       ,                                      &
         bktyp       ,                                      &
         bkstp       ,                                      &
         rpt_handle  ,                                      &
         convert     ,                                      &
         iostat)

         type (burp_block),intent(inout) :: this
         integer(kind=int_def)           :: error,k

         integer(kind=int_def),optional,intent(in)  :: bfam
         integer(kind=int_def),optional,intent(in)  :: bdesc
         integer(kind=int_def),optional,intent(in)  :: btyp
         integer(kind=int_def),optional,intent(in)  :: nbit
         integer(kind=int_def),optional,intent(in)  :: bit0
         integer(kind=int_def),optional,intent(in)  :: oars
         integer(kind=int_def),optional,intent(in)  :: datyp
         integer(kind=int_def),optional,intent(in)  :: bknat
         integer(kind=int_def),optional,intent(in)  :: bktyp
         integer(kind=int_def),optional,intent(in)  :: bkstp
         integer(kind=int_def),optional,intent(in)  :: rpt_handle
         logical,optional,intent(in)                :: convert
         integer(kind=int_def),optional,intent(inout):: iostat

         logical:: un_seul,valide_bknat,valide_bktyp,valide_bkstp
 
         error = -1
 
         valide_bknat    = .true.
         valide_bktyp    = .true.
         valide_bkstp    = .true.
         un_seul         = .false.

         if (present(bknat) .or. present(bktyp) .or. present(bkstp))  &
            un_seul = .true.
         if (present(bknat)) then
             if ((bknat< 0) .or. (bknat> 15) ) then
                valide_bknat=.false.
                error = -1
                call setstatetofailure(burp_error%error, &
                "failure >> check 0<=bknat<=15  "//" &
                &trace - blk_set_property.")
                if (present(iostat))  iostat = error
                return
              endif
         endif
         if (present(bktyp)) then
             if ((bktyp< 0) .or. (bktyp> 127) ) then
                valide_bktyp=.false.
                error = -1
                call setstatetofailure(burp_error%error, &
                "failure >> check 0<=bktyp<=15  "//" &
                &trace - blk_set_property.")
                if (present(iostat))  iostat = error
                return
              endif
         endif
         if (present(bkstp)) then
             if ((bkstp< 0) .or. (bkstp> 15) ) then
                valide_bkstp=.false.
                error = -1
                call setstatetofailure(burp_error%error, &
                "failure >> check 0<=bkstp<=15  "//" &
                &trace - blk_set_property.")
                if (present(iostat))  iostat = error
                return
              endif
         endif
         if (un_seul .and. present(btyp)) then
                error =-1
                call setstatetofailure(burp_error%error, &
                "failure >> choose to set btyp or (bknat|bktyp,bkstp) &
                &trace - blk_set_property.")
                if (present(iostat))  iostat = error
                return
         endif
 
         if (present(bfam) .or. present(bdesc) .or. present(btyp ) .or.&
             present(nbit ).or. present(bit0 ) .or. present(oars ) .or.&
             present(bknat ).or. present(bktyp ) .or. present(bkstp ) .or.&
             present(rpt_handle) .or. present(datyp) .or. present(convert)) error=burp_noerr

         if (error /= burp_noerr) then
            if (present(iostat)) iostat = error
            call setstatetofailure(burp_error%error, &
            "failure >> block setting property: what property to set?? ")
             return
         endif

         if (present(btyp       )) then
            this%btyp  = btyp
            error = mrbtyp(this%bknat,this%bktyp,this%bkstp,this%btyp)
         endif
         if (present(bknat      )) then
            this%bknat = bknat
            do k=11,14
               this%btyp = ibclr(this%btyp,k)
            enddo
            this%btyp = ior(this%btyp, ishft(this%bknat,11))
         endif
         if (present(bktyp      )) then
            this%bktyp = bktyp
            do k=4,10
               this%btyp = ibclr(this%btyp,k)
            enddo
            this%btyp = ior(this%btyp, ishft(this%bktyp,4))
         endif
         if (present(bkstp      )) then
            this%bkstp = bkstp
            do k=0,3
               this%btyp = ibclr(this%btyp,k)
            enddo
            this%btyp = ior(this%btyp, this%bkstp)
         endif

!            !construire le btyp
!            if (all_present ) then
!                this%bknat       =  bknat
!                this%bktyp       =  bktyp
!                this%bkstp       =  bkstp
!                this%btyp =0
!                this%btyp = ior(ishft(this%bknat,11),ishft(this%bktyp,4))
!                this%btyp = ior(this%btyp, this%bkstp)
!            endif

         if (present(bfam       )) this%bfam  = bfam
         if (present(bdesc      )) this%bdesc = bdesc
         if (present(nbit       )) this%nbit  = nbit
         if (present(bit0       )) this%bit0  = bit0
         if (present(oars       )) this%oars  = oars
         if (present(datyp      )) this%datyp = datyp
         if (present(convert    )) this%convert=convert
         if (present(rpt_handle )) this%rpt_handle = rpt_handle
         if (present(iostat)) iostat = error

       end subroutine block_set_property

       !!
       !! getting block from buffer array 
       !!
       subroutine blk_get_block(this,buffer,bkno,convert, iostat)
         type (burp_block),    intent(inout)            :: this
         integer(kind=int_def),dimension(:),intent (in) :: buffer
         integer(kind=int_def),intent(in)               :: bkno
         logical,intent(in)                             :: convert
         integer(kind=int_def),optional,intent(inout)   :: iostat
         integer(kind=int_def)                          :: err
         integer(kind=int_def)                          :: error
         real                                           :: real_opt_value

         error = burp_noerr
         real_opt_value = 0.0
         error = mrfgor("MISSING",real_opt_value)
         if (error /= 0) then
            call setstatetofailure(burp_error%error, &
            "failure >> getting real option value - librmn_mrfgor")
            if (present(iostat))  iostat = error
            return
         endif

         if (.not.is_init(this%init)) then
             call init_burp_block(this,error)    ! initialize block
             if (present(iostat))  iostat = error
             if (error /= burp_noerr) return
         end if

         this%bkno = bkno

         !
         ! get block parameters
         !
         error = mrbprm(buffer,bkno,this%nele,this%nval,   &
                  this%nt, this%bfam, this%bdesc,this%btyp,&
                  this%nbit, this%bit0, this%datyp)
         if (error /= 0) then
            call setstatetofailure(burp_error%error, &
            "failure >> getting block parameters - librmn_mrbprm")
            if (present(iostat))  iostat = error
            return
         endif
         error = mrbtyp(this%bknat,this%bktyp,this%bkstp,this%btyp)
         if (error /= 0) then
            call setstatetofailure(burp_error%error, &
            "failure >> getting bknat,bktyp,bkstp- librmn_mrbtyp")
            if (present(iostat))  iostat = error
            return
         endif

         ! verifier que toutes les dimensions nt,nval et nele sont postives
         !
         if ( ((this%nele) * (this%nt) *(this%nval)) <= 0) then
            call setstatetofailure(burp_error%error, &
            "failure >> one of the dimensions nt, nval or nele is <= 0 !!")
            if (present(iostat))  iostat = -1
            return
         endif

         !
         ! extract block data
         !
         call free_burp_block(this,error)
         if (present(iostat))  iostat = error
         if (error /= burp_noerr) return

         ! fixer le parametre convert du block a la valeur entree par l'usager
         ! convert =.TRUE., veut dire que l'on demande faire appel a la conversion
         ! dea valeurs entieres vers les valeurs reellles selon al table burp

         this%convert = convert

         ! allouer eapace pour les elements
         allocate(this%lstele(this%nele), stat=error)
         if (present(iostat))  iostat = error
         if (error /= burp_noerr) then
            call setstatetofailure(burp_error%error, &
            "failure >> to allocate memory - lste- blk_get_blk")
             return
         endif
         allocate(this%dlstele(this%nele),stat=error)
         if (present(iostat))  iostat = error
         if (error /= burp_noerr) then
            call setstatetofailure(burp_error%error, &
            "failure >> to allocate memory - dlste- blk_get_blk")
             return
         endif
         ! si datyp ==6 valeur real4 qui sont storees dans le bloc
         ! alors allouer le tableau des reels
         if (this%datyp < 5) then
            allocate(this%tblval(this%nele,this%nval,this%nt),stat = error)
         elseif(this%datyp < 7) then 
         ! allouer pour tblval qqchose, peut-etre pour
         ! eviter un seg fault si on affecte tblval a aute tbval meme si pas utilse
         ! blk_affectation utilise tblval
            allocate(this%rval(this%nele,this%nval,this%nt),  stat = error)
            allocate(this%tblval(this%nele,this%nval,this%nt),  stat = error)
         else
            call setstatetofailure(burp_error%error, &
            "failure >> dtatyp > 6 not supported yet urp_get_blk")
            return
         end if
         if (error /= burp_noerr) then
            if (present(iostat))  iostat = error
            call setstatetofailure(burp_error%error, &
            "failure >> to allocate memory - tblval/rval- blk_get_blk")
            return
         endif
 

         ! extraire les donnees
         if (this%datyp < 5) then
            error = mrbxtr(buffer,bkno,this%lstele,this%tblval)
         elseif(this%datyp < 7) then 
            error = mrbxtr(buffer,bkno,this%lstele,this%rval)
         end if
         if (present(iostat))  iostat = error
         if (error /= burp_noerr) then
             call setstatetofailure(burp_error%error, &
             "failure >> to extract block- librmn_mrbxtr")
             return
         endif
         ! decompacter les elements du block
         error = mrbdcl(this%lstele,this%dlstele,this%nele)
         if (present(iostat))  iostat = error
         if (error /= burp_noerr) then
            call setstatetofailure(burp_error%error, &
            "failure >> to convert lstele to dlstel - librmn_mrbdcl")
             return
         endif

         ! si les conversions sont necesaires
         ! dans le cas ou dans burp on a store des entiers: datyp < 5
         ! Dans le cas ou datyp == 6 on a deja alloue le tableau rval
         if (this%convert .and. (this%datyp < 5)) then
            allocate(this%rval(this%nele,this%nval,this%nt),  stat = error)
            if (present(iostat))  iostat = error
            if (error /= burp_noerr) then
                call setstatetofailure(burp_error%error, &
                "failure >> to allocate memory - rval- blk_get_blk")
                return
            endif
            this%rval(:,:,:) = real_opt_value
            error = mrbcvt(this%lstele,this%tblval,this%rval,this%nele,&
                        this%nval, this%nt, 0)
            if (present(iostat))  iostat = error
            if (error /= burp_noerr) then
                call setstatetofailure(burp_error%error, &
                "failure >> to convert tblval <--> rval - librmn_mrbcvt")
                return
            endif
        end if

       end subroutine blk_get_block
       !!
       !! getting only block hdr from buffer array 
       !!
       subroutine blk_get_only_hdr(this,buffer,bkno,convert, iostat)
         type (burp_block),    intent(inout)            :: this
         integer(kind=int_def),dimension(:),intent (in) :: buffer
         integer(kind=int_def),intent(in)               :: bkno
         logical,intent(in)                             :: convert
         integer(kind=int_def),optional,intent(inout)   :: iostat
         integer(kind=int_def)                          :: err
         integer(kind=int_def)                          :: error
         real                                           :: real_opt_value

         error = burp_noerr
         real_opt_value = 0.0
         error = mrfgor("MISSING",real_opt_value)
         if (error /= 0) then
            call setstatetofailure(burp_error%error, &
            "failure >> getting real option value - librmn_mrfgor")
            if (present(iostat))  iostat = error
            return
         endif

         if (.not.is_init(this%init)) then
             call init_burp_block(this,error)    ! initialize block
             if (present(iostat))  iostat = error
             if (error /= burp_noerr) return
         end if

         this%bkno = bkno

         !
         ! get block parameters
         !
         error = mrbprm(buffer,bkno,this%nele,this%nval,   &
                  this%nt, this%bfam, this%bdesc,this%btyp,&
                  this%nbit, this%bit0, this%datyp)
         if (error /= 0) then
            call setstatetofailure(burp_error%error, &
            "failure >> getting block parameters - librmn_mrbprm")
            if (present(iostat))  iostat = error
            return
         endif
         error = mrbtyp(this%bknat,this%bktyp,this%bkstp,this%btyp)
         if (error /= 0) then
            call setstatetofailure(burp_error%error, &
            "failure >> getting bknat,bktyp,bkstp- librmn_mrbtyp")
            if (present(iostat))  iostat = error
            return
         endif

         !
         ! extract block data
         !
         call free_burp_block(this,error)
         if (present(iostat))  iostat = error
         if (error /= burp_noerr) return

         ! fixer le parametre convert du block a la valeur entree par l'usager
         ! convert =.TRUE., veut dire que l'on demande faire appel a la conversion
         ! dea valeurs entieres vers les valeurs reellles selon al table burp

         this%convert = convert

       end subroutine blk_get_only_hdr


       !!
       !!
       !!
       function burp_find_element(this,element, iostat) result(indice)
         implicit none
         type (burp_block),    intent (in)    :: this
         integer(kind=int_def),intent (in)    :: element
         integer(kind=int_def)                :: indice
         integer(kind=int_def)                :: i, error
         integer(kind=int_def),optional,intent(inout) :: iostat


         error = burp_noerr
         !
         ! search vector dliste
         !
         do i=1,this%nele
                if (this%dlstele(i) == element) then
                        indice = i
                        if (present(iostat))  iostat = error
                        exit
                end if
         end do
         if (i > this%nele)  indice = -1

         if (present(iostat))  iostat = error
       end function  burp_find_element

       !!
       !!
       !!
       function burp_get_element(this,index, octal,iostat) result(element)
         implicit none
         type (burp_block),    intent(in)    :: this
         integer(kind=int_def),intent(in)    :: index
         integer(kind=int_def)               :: element
         integer(kind=int_def)               :: i, error
         integer(kind=int_def),optional, intent (inout) :: iostat
         logical,              optional, intent (in)    ::octal

         logical :: compresse 
         compresse = .false.
         if (present(octal))  compresse = octal

         error = burp_noerr
         !
         ! return element name at position index in dliste
         !
         if (index > this%nele) then
                element = -1
                if (present(iostat))  iostat = -1
                return
         end if
         if (compresse) then 
            element = this%lstele(index)
         else
            element = this%dlstele(index)
         end if


         if (present(iostat))  iostat = error
       end function  burp_get_element
       !!
       !!
       !!
       function burp_get_rval(this,nele_ind,nval_ind,&
                     nt_ind, iostat) result(rval)
         implicit none
         type (burp_block),    intent(in)    :: this
         integer(kind=int_def),intent(in)    :: nele_ind
         integer(kind=int_def),intent(in)    :: nval_ind
         integer(kind=int_def),intent(in)    :: nt_ind
         integer(kind=int_def)               :: error
         real                                :: rval
         integer(kind=int_def),optional,intent(inout) :: iostat

         !
         ! dont forget error checking (bound)
         !
         error = burp_noerr
         if (nele_ind > this%nele) then
            if (present(iostat))  iostat = -1
            call setstatetofailure(burp_error%error, &
            "failure >> element index > nele block value - burp_get_rval")
             return
         endif
         if (nval_ind > this%nval) then
            if (present(iostat))  iostat = -1
            call setstatetofailure(burp_error%error, &
            "failure >> nval index > nval block value - burp_get_rval")
             return
         end if
         if (nt_ind > this%nt) then
            if (present(iostat))  iostat = -1
            call setstatetofailure(burp_error%error, &
            "failure >> nt index > nt block value - burp_get_rval")
             return
         end if
         if (this%convert .or. this%datyp == 6) then
            rval= this%rval(nele_ind, nval_ind, nt_ind)
         else
            if (present(iostat))  iostat = -1
            call setstatetofailure(burp_error%error, &
            "failure >> convert is false or datyp /= 6&
            & call burp_find_block with convert=.true. - burp_get_rval")
             return
         end if


         if (present(iostat))  iostat = error
       end function  burp_get_rval
       !!
       !!
       !!
       function burp_get_tblval(this,nele_ind,nval_ind,&
                     nt_ind, iostat) result(tblval)
         implicit none
         type (burp_block),    intent(in)    :: this
         integer(kind=int_def),intent(in)    :: nele_ind
         integer(kind=int_def),intent(in)    :: nval_ind
         integer(kind=int_def),intent(in)    :: nt_ind
         integer(kind=int_def)               :: error
         integer(kind=int_def)               :: tblval
         integer(kind=int_def),optional,intent(inout) :: iostat

         !
         ! dont forget error checking (bound)
         !
         error = burp_noerr
         if (nele_ind > this%nele) then
            if (present(iostat))  iostat = -1
            call setstatetofailure(burp_error%error, &
            "failure >> element index > nele block value - burp_get_tblval")
             return
         endif
         if (nval_ind > this%nval) then
            if (present(iostat))  iostat = -1
            call setstatetofailure(burp_error%error, &
            "failure >> nval index > nval block value - burp_get_tblval")
             return
         end if
         if (nt_ind > this%nt) then
            if (present(iostat))  iostat = -1
            call setstatetofailure(burp_error%error, &
            "failure >> nt index > nt block value - burp_get_tblval")
             return
         end if
         if (this%datyp == 6) return
         tblval= this%tblval(nele_ind, nval_ind, nt_ind)

         if (present(iostat))  iostat = error
       end function  burp_get_tblval

       !!
       !! setting elements
       !!
       subroutine burp_set_element(this,nele_ind, element, iostat)
         implicit none
         type (burp_block),    intent(inout) :: this
         integer(kind=int_def),intent(in)    :: element
         integer(kind=int_def),intent (in)   :: nele_ind
         integer(kind=int_def)               :: error
         integer(kind=int_def),optional, intent (inout) :: iostat


         error = burp_noerr
         if (nele_ind > this%nele) then
            if (present(iostat))  iostat = -1
            call setstatetofailure(burp_error%error, &
            "failure >> element index > nele block parameter - burp_set_element")
             return
         endif

         ! fill dliste
         !
         this%dlstele(nele_ind) = element

         ! si on veut enrgeistrer les lemnts sans les compacter
         !
         this%lstele(nele_ind)  = element
         !
         if (present(iostat))  iostat = error
       end subroutine  burp_set_element

       !!
       !! setting rvalues
       !!
       subroutine burp_set_rval(this,nele_ind, nval_ind, nt_ind, &
                                   rval, iostat)
         implicit none
         type (burp_block),    intent(inout) :: this
         integer(kind=int_def),intent(in)    :: nele_ind
         integer(kind=int_def),intent(in)    :: nval_ind
         integer(kind=int_def),intent(in)    :: nt_ind
         real,                 intent (in)   :: rval
         integer(kind=int_def)               :: error
         integer(kind=int_def),optional, intent (inout) :: iostat


         error = burp_noerr
         if ((nele_ind * nval_ind * nt_ind) <= 0) then
            if (present(iostat))  iostat = -1
            call setstatetofailure(burp_error%error, &
            "failure >> index must be >0  - burp_set_rval")
             return
         endif
         if (nele_ind > this%nele) then
            if (present(iostat))  iostat = -1
            call setstatetofailure(burp_error%error, &
            "failure >> element index > nele block value - burp_set_rval")
             return
         endif
         if (nval_ind > this%nval) then
            if (present(iostat))  iostat = -1
            call setstatetofailure(burp_error%error, &
            "failure >> nval index > nval block value - burp_set_rval")
             return
         end if
         if (nt_ind > this%nt) then
            if (present(iostat))  iostat = -1
            call setstatetofailure(burp_error%error, &
            "failure >> nt index > nt block value - burp_set_rval")
             return
         end if

         ! fill rval
         if (this%convert .or. this%datyp == 6) then
         !
         this%rval(nele_ind,nval_ind,nt_ind) = rval
         else
            if (present(iostat))  iostat = -1
            call setstatetofailure(burp_error%error, &
            "failure >> you are working with a convert false mode&
            & call burp_new(block..) with convert=.true. or datyp /=6 &
            &- burp_set_rval" )
             return
         end if

         !
         if (present(iostat))  iostat = error
       end subroutine  burp_set_rval
       !!
       !! setting tblvalues
       !!
       subroutine burp_set_tblval(this,nele_ind, nval_ind, nt_ind, &
                                   tblval, iostat)
         implicit none
         type (burp_block),    intent(inout) :: this
         integer(kind=int_def),intent(in)    :: nele_ind
         integer(kind=int_def),intent(in)    :: nval_ind
         integer(kind=int_def),intent(in)    :: nt_ind
         integer(kind=int_def),intent(in)    :: tblval
         integer(kind=int_def)               :: error
         integer(kind=int_def),optional, intent (inout) :: iostat


         error = burp_noerr
         if (nele_ind > this%nele) then
            if (present(iostat))  iostat = -1
            call setstatetofailure(burp_error%error, &
            "failure >> element index > nele block value - burp_set_tblval")
             return
         endif
         if (nval_ind > this%nval) then
            if (present(iostat))  iostat = -1
            call setstatetofailure(burp_error%error, &
            "failure >> nval index > nval block value - burp_set_tblval")
             return
         end if
         if (nt_ind > this%nt) then
            if (present(iostat))  iostat = -1
            call setstatetofailure(burp_error%error, &
            "failure >> nt index > nt block value - burp_set_tblval")
             return
         end if

         ! fill tblval
         !
         this%tblval(nele_ind,nval_ind,nt_ind) = tblval
         !
         if (present(iostat))  iostat = error
       end subroutine  burp_set_tblval

       !!
       !! encoding block : compacter la liste des elements
       !!
       subroutine burp_encode_block(this, iostat)
         implicit none
         type (burp_block),    intent(inout)          :: this
         integer(kind=int_def),optional,intent(inout) :: iostat
         integer(kind=int_def)                        :: error

         error = burp_noerr

         error = mrbcol(this%dlstele,this%lstele,this%nele)
         if (present(iostat))  iostat = error
         if (error /= 0) then
            call setstatetofailure(burp_error%error, &
            "failure >> encoding block elements -"//"&
            &burp_encode_block:librmn_mrbcol")
            return
         endif

       end subroutine  burp_encode_block

       subroutine burp_safe_convert_block(this, mode, iostat)
         implicit none
         type (burp_block),    intent(inout)          :: this
         integer(kind=int_def),optional,intent(inout) :: iostat
         integer(kind=int_def),optional,intent(in)    :: mode
         integer(kind=int_def)                        :: error
         integer                                      :: my_mode
         real                                         :: real_opt_value

         real_opt_value = 0.0
         error = mrfgor("MISSING",real_opt_value)

         my_mode = MKSA_to_BUFR
         if (present(mode)) then
            my_mode = mode
         end if

         ! initialize destination memory prior to conversion to avoid junk from memory
         if (my_mode == BUFR_to_MKSA) then
           this%rval(:,:,:) = real_opt_value 
         else if (my_mode == MKSA_to_BUFR) then
           this%tblval(:,:,:) = -1
         end if

         call burp_convert_block(this, mode, iostat)
       end subroutine burp_safe_convert_block

       subroutine burp_convert_block(this, mode,iostat)
         implicit none
         type (burp_block),    intent(inout)          :: this
         integer(kind=int_def),optional,intent(inout) :: iostat
         integer(kind=int_def),optional,intent(in)    :: mode
         integer(kind=int_def)                        :: error
         integer                                      :: my_mode
         real                                       :: real_opt_value

         error = burp_noerr
         real_opt_value = 0.0
         error = mrfgor("MISSING",real_opt_value)
         my_mode = MKSA_to_BUFR
         if (present(mode)) then
            my_mode = mode
         end if
         error = burp_noerr
         if (this%datyp == 6) then
            ! dans le cas ou datyp == 6, ie on store les floats
            ! mrbcvt ne s'applique pas.
            return
         end if
         ! cette routine est appellee avant de mettre
         ! le block das le rapport. Le call to burp_new(blk)
         ! par defaut le tag convert est true, ie on 
         ! s'attend que les valeurs entrees sont des reels
         ! et vant de les mettre dans burp, les encoder
         ! en entier.
         ! Cette routine peur etre appellee aussi en cherhant
         ! un block em n=mode convert false et plus tard
         ! on peut appeler burp_convert avec mode BUFR_to_MKSA
         ! pour fair un brbcvt pour avoir les valeurs rees
         ! du block

         ! Rem : convert block doit etre toujours precedee par
         ! encodeblock, a mois qu'on ait rentre directement
         ! les elements BUFR encode, directement.. (lstelem)
         if (this%convert .and. (my_mode == MKSA_to_BUFR)) then
            error = mrbcvt(this%lstele,this%tblval,this%rval,this%nele, &
                            this%nval, this%nt,MKSA_to_BUFR)
            if (present(iostat))  iostat = error
            if (error /= 0) then
                call setstatetofailure(burp_error%error, &
                "failure >> converting block values -"//"&
                &burp_convert_block:librmn_mrbcvt")
                return
            endif
         else if (my_mode == BUFR_to_MKSA) then
            if (associated(this%rval)) then
               deallocate(this%rval,stat=error)
               nullify(this%rval)
            end if
            allocate(this%rval(this%nele,this%nval,this%nt),  stat = error)
            if (present(iostat))  iostat = error
            if (error /= burp_noerr) then
                call setstatetofailure(burp_error%error, &
                "failure >> to allocate memory - rval- burp_convert_blk")
                return
            endif
            error = mrbcvt(this%lstele,this%tblval,this%rval,this%nele, &
                            this%nval, this%nt,BUFR_to_MKSA)
            
            if (present(iostat))  iostat = error
            if (error /= burp_noerr) then
                call setstatetofailure(burp_error%error, &
                "failure >> to convert tblval <--> rval - librmn_mrbcvt")
                return
             else
                this%convert = .true.
            endif
         else
            if (present(iostat))  iostat = error
            if (error /= 0) then
                call setstatetofailure(burp_error%error, &
                "failure >> converting block values -"//"&
                &burp_convert_bloc , youe are working in mode convert &
                & FALSE:burp_convert_block")
                return
            endif
         end if

       end subroutine  burp_convert_block

       subroutine burp_block_mrbadd(this,buffer, iostat)
         implicit none
         type (burp_block),    intent(inout)             :: this
         integer(kind=int_def),dimension(:),intent(inout):: buffer
         integer(kind=int_def),optional,intent(inout)    :: iostat
         integer(kind=int_def)                           :: error

         error = burp_noerr
         ! si datyp ==6 on store directement le tabelau des float dans
         ! burp
         if (this%datyp < 5) then
            error = mrbadd(buffer,this%bkno, this%nele, this%nval,       &
                     this%nt, this%bfam, this%bdesc,this%btyp, this%nbit,&
                     this%bit0,this%datyp,this%lstele, this%tblval)
         else if(this%datyp < 7) then
            error = mrbadd(buffer,this%bkno, this%nele, this%nval,       &
                     this%nt, this%bfam, this%bdesc,this%btyp, this%nbit,&
                     this%bit0,this%datyp,this%lstele, this%rval)
         else
            if (present(iostat))  iostat = -1
             call setstatetofailure(burp_error%error, &
             "failure >> to add block to rpt"//&
             &"- burp_block_mrbadd : datyp not supported")
             return

         end if

         if (present(iostat))  iostat = error
         if (error /= 0) then
             call setstatetofailure(burp_error%error, &
             "failure >> to add block to rpt"//&
             &"- burp_block_mrbadd : librmn_mrbadd")
             return
         endif

       end subroutine  burp_block_mrbadd

       !
       ! block_affectation
       !
       subroutine block_affectation(blk_out,blk_in)
         implicit none
         type (burp_block),  intent (inout) :: blk_out
         type (burp_block),  intent (in)    :: blk_in
         integer(kind=int_def)              :: error

         error = burp_noerr

         if (.not.is_init(blk_out%init)) then
             call init_burp_block(blk_out,error)
             if (error /= burp_noerr) then
                 write(*,*) burp_str_error()
                 stop
             endif
         else
             call free_burp_block(blk_out,error)
             if (error /= burp_noerr) then
                 write(*,*) burp_str_error()
                 stop
             endif
             call init_burp_block(blk_out,error)
             if (error /= burp_noerr) then
                 write(*,*) burp_str_error()
                 stop
             endif
         end if
         if (.not.is_init(blk_in%init)) then
            return
         else
            call burp_new(blk_out,nele =blk_in%nele,nval= blk_in%nval, &
                          nt = blk_in%nt,convert=blk_in%convert,       &
                          datyp = blk_in%datyp ,iostat=error)
            if (error /= burp_noerr) then
                 write(*,*) burp_str_error()
                 stop
            endif
            blk_out%bkno          = blk_in%bkno
            blk_out%nele          = blk_in%nele
            blk_out%nval          = blk_in%nval
            blk_out%nt            = blk_in%nt
            blk_out%bfam          = blk_in%bfam
            blk_out%bdesc         = blk_in%bdesc
            blk_out%btyp          = blk_in%btyp
            blk_out%bknat         = blk_in%bknat
            blk_out%bktyp         = blk_in%bktyp
            blk_out%bkstp         = blk_in%bkstp
            blk_out%nbit          = blk_in%nbit
            blk_out%bit0          = blk_in%bit0
            blk_out%oars          = blk_in%oars
            blk_out%datyp         = blk_in%datyp
            blk_out%rpt_handle    = blk_in%rpt_handle
            blk_out%store_mode    = blk_in%store_mode
            blk_out%convert       = blk_in%convert
            blk_out%lstele(:)     = blk_in%lstele(:)
            blk_out%dlstele(:)    = blk_in%dlstele(:)
            ! todo voir si tblval est toujours valide oe alloue
            blk_out%tblval(:,:,:) = blk_in%tblval(:,:,:)
            if (blk_in%convert .or. (blk_in%datyp == 6)) then 
                blk_out%rval(:,:,:)   = blk_in%rval(:,:,:)
            end if

         end if
       end subroutine block_affectation

       !
       !  block  resize
       !
       !
       subroutine burp_resize_block(block,add_nele,add_nval,add_nt,iostat)
         implicit none
         type (burp_block),  intent (inout)      :: block
         integer(kind = int_def),optional,intent (in)  :: add_nele,add_nt,add_nval
         integer(kind = int_def),optional,intent(inout)::iostat
         
         type(burp_block)        :: blk_tmp
         integer(kind=int_def)   :: error
         integer(kind=int_def)   :: my_a_nele,my_a_nval,my_a_nt
         logical                 :: add_size

         error = burp_noerr
         my_a_nt  = 0
         my_a_nval= 0
         my_a_nele= 0
         add_size = .false.

         !  si le block n'est pas initialise
         !  ne pas resizer, on fresize un block existant!!
         if (.not.is_init(block%init)) then
                error = -1
                call setstatetofailure(burp_error%error, &
                "failure >> can't resize block witch never used : burp_resize_blockt!!")
                if (present(iostat))  iostat = error
                return
         endif 

         if (present(add_nele)) then
            if (add_nele <=0 ) then
                error = -1
                call setstatetofailure(burp_error%error, &
                "failure >> add_nele must > 0 : burp_resize_blockt!!")
                if (present(iostat))  iostat = error
                return
            endif
            my_a_nele = add_nele
            add_size = .true.
         endif
         if (present(add_nval)) then
            if (add_nval <=0 ) then
                error = -1
                call setstatetofailure(burp_error%error, &
                "failure >> add_nval must > 0 : burp_resize_blockt!!")
                if (present(iostat))  iostat = error
                return
            endif
            my_a_nval = add_nval
            add_size = .true.
         endif
         if (present(add_nt)) then
            if (add_nt <=0 ) then
                error = -1
                call setstatetofailure(burp_error%error, &
                "failure >> add_nt must > 0 : burp_resize_blockt!!")
                if (present(iostat))  iostat = error
                return
            endif
            my_a_nt = add_nt
            add_size = .true.
         endif

         if (.not.add_size) then
                error = -1
                call setstatetofailure(burp_error%error, &
                "failure >>nele or naval or nt not present: burp_resize_blockt!!")
                if (present(iostat))  iostat = error
                return
         endif


         ! creer un block temporaire avec nouveau size
         !
         call burp_new(blk_tmp,nele = block%nele + my_a_nele, &
                               nval = block%nval + my_a_nval, &
                               nt   = block%nt   + my_a_nt  , &
                               convert = block%convert,       &
                               datyp = block%datyp,           &
                               iostat=error)
         if (error /= burp_noerr) return

            blk_tmp%bkno          = block%bkno

!!! copier toute les proprietes sauf les dimensions
!            blk_tmp%nele          = block%nele
!            blk_tmp%nval          = block%nval
!            blk_tmp%nt            = block%nt

            blk_tmp%bfam          = block%bfam
            blk_tmp%bdesc         = block%bdesc
            blk_tmp%btyp          = block%btyp
            blk_tmp%bknat         = block%bknat
            blk_tmp%bktyp         = block%bktyp
            blk_tmp%bkstp         = block%bkstp
            blk_tmp%nbit          = block%nbit
            blk_tmp%bit0          = block%bit0
            blk_tmp%oars          = block%oars
            blk_tmp%datyp         = block%datyp
            blk_tmp%rpt_handle    = block%rpt_handle
            blk_tmp%store_mode    = block%store_mode
            blk_tmp%convert       = block%convert

            blk_tmp%lstele(:)     = block%lstele(:)
            blk_tmp%dlstele(:)    = block%dlstele(:)
            blk_tmp%tblval(1:block%nele,1:block%nval,1:block%nt) = block%tblval(:,:,:)

            ! si on travaille en mode convert oubien que nous travaillons
            ! avec du burp ou store directement des float ie datyp=6
            if (block%convert .or. (block%datyp == 6)) then
                blk_tmp%rval(1:block%nele,1:block%nval,1:block%nt) = block%rval(:,:,:)
            end if


         !
         ! maintenant on affecte le blk_tmp au rapport entre
          block = blk_tmp
          call free_burp_block(blk_tmp,iostat=error)
         if (present(iostat))  iostat = error

       end subroutine burp_resize_block
       !
       ! block  reduce
       ! reduire les dimensions des parametres du block : nelem, nval et nt
       !
       subroutine burp_reduce_block(block,new_nele,new_nval,new_nt,iostat)
         implicit none
         type (burp_block),  intent (inout)             :: block
         integer(kind = int_def),optional,intent (in)   :: new_nele,new_nt,new_nval
         integer(kind = int_def),optional,intent(inout) :: iostat
         
         type(burp_block)        :: blk_tmp
         integer(kind=int_def)   :: error
         integer(kind=int_def)   :: my_nele,my_nval,my_nt ! taille du block source
         logical                 :: reduce_size

         error = burp_noerr
         ! si aucune autre dimension n 'est passe an argument
         ! le block temporaire aura la meme diemsion que le block en entree
         my_nt  = block%nt
         my_nval= block%nval
         my_nele= block%nele

         reduce_size = .false.

         !  si le block n'est pas initialise
         !  ne pas resizer, on resize un block existant!!
         if (.not.is_init(block%init)) then
                error = -1
                call setstatetofailure(burp_error%error, &
                "failure >> can't reduce block witch never used : burp_reduce_block!!")
                if (present(iostat))  iostat = error
                return
         endif 
         ! 

         if (present(new_nele)) then
            if (new_nele > block%nele ) then
                error = -1
                call setstatetofailure(burp_error%error, &
                "failure >> new_nele must < block nele param : burp_reduce_block!!")
                if (present(iostat))  iostat = error
                return
            endif
            my_nele = new_nele
            reduce_size = .true.
         endif
         if (present(new_nval)) then
            if (new_nval > block%nval ) then
                error = -1
                call setstatetofailure(burp_error%error, &
                "failure >> new_nval must < block nval param : burp_reduce_block!!")
                if (present(iostat))  iostat = error
                return
            endif
            my_nval = new_nval
            reduce_size = .true.
         endif
         if (present(new_nt)) then
            if (new_nt > block%nt ) then
                error = -1
                call setstatetofailure(burp_error%error, &
                "failure >> new_nt must < block nt param : burp_reduce_block!!")
                if (present(iostat))  iostat = error
                return
            endif
            my_nt = new_nt
            reduce_size = .true.
         endif
         if (my_nt*my_nval*my_nele <= 0) then
                error = -1
                call setstatetofailure(burp_error%error, &
                "failure >> un des parametre est <=0 : burp_reduce_block!!")
                if (present(iostat))  iostat = error
                return
         endif

         if (.not.reduce_size) then
                error = -1
                call setstatetofailure(burp_error%error, &
                "failure >>new_nele or new_nval or new_nt not present: burp_reduce_block!!")
                if (present(iostat))  iostat = error
                return
         endif


         ! creer un block temporaire avec nouveau size
         !
         call burp_new(blk_tmp,nele =  my_nele, &
                               nval =  my_nval, &
                               nt   =  my_nt  , &
                               convert=block%convert,&
                               datyp=block%datyp,&
                               iostat=error)
         if (error /= burp_noerr) return

            blk_tmp%bkno          = block%bkno

!!! copier toute les proprietes sauf les dimensions
!            blk_tmp%nele          = block%nele
!            blk_tmp%nval          = block%nval
!            blk_tmp%nt            = block%nt

            blk_tmp%bfam          = block%bfam
            blk_tmp%bdesc         = block%bdesc
            blk_tmp%btyp          = block%btyp
            blk_tmp%bknat         = block%bknat
            blk_tmp%bktyp         = block%bktyp
            blk_tmp%bkstp         = block%bkstp
            blk_tmp%nbit          = block%nbit
            blk_tmp%bit0          = block%bit0
            blk_tmp%oars          = block%oars
            blk_tmp%datyp         = block%datyp
            blk_tmp%rpt_handle    = block%rpt_handle
            blk_tmp%store_mode    = block%store_mode
            blk_tmp%convert       = block%convert

            blk_tmp%lstele(:)     = block%lstele(1:my_nele)
            blk_tmp%dlstele(:)    = block%dlstele(1:my_nele)
            blk_tmp%tblval(:,:,:) = block%tblval(1:my_nele,1:my_nval,1:my_nt)

            if (block%convert .or. (block%datyp == 6)) then
                blk_tmp%rval(:,:,:)   = block%rval(1:my_nele,1:my_nval,1:my_nt)
            end if


         !
         ! maintenant on affecte le blk_tmp au rapport entre
          block = blk_tmp
          call free_burp_block(blk_tmp,iostat=error)
         if (present(iostat))  iostat = error

       end subroutine 

       !
       ! make blockmarqueur_
       !
       function make_block_marqueur(blk_in) result(blk_out)
         implicit none
         type (burp_block),  intent (in)    :: blk_in
         type (burp_block)     :: blk_out
         integer(kind=int_def) :: error,bknat2

         ! since make_block_marqueur is declared as a function
         ! the intent of blk_out can only be out
         ! consequently we need to initialize it to something
         ! before trying to fill it
         ! (otherwise valgrind gives us all sorts of messages
         ! about conditional jumps and we end up with double
         ! frees due to working on uninitialied values)
         call init_burp_block(blk_out,error)
         if (error /= burp_noerr) then
             write(*,*) burp_str_error()
             stop
         endif


         ! verifier si blk_in est deja un block marqueur
         ! alors lui juste renvoyer une copie
         ! decomposition du btyp
         !   btyp    =      bknat       |      bktyp      |  bkstp
         !   15 bits =      4 bits      |      7 bits     |  4 bits
         !           =  bknat1 | bknat2 | bktyp1 | bktyp2 |  bkstp
         !           =  2 bits | 2 bits | 1 bit  | 6 bits |  4 bits
         !

         ! bknat1 = ibits ( my_btyp, 13, 2 )
         ! bktyp1 = ibits ( my_btyp, 10, 1 )
         ! bktyp2 = ibits ( my_btyp,  4, 6 )
         ! bkstp  = ibits ( my_btyp,  0, 4 )

         ! traitement de bknat2
         ! 00 = donnees
         ! 01 = info
         ! 10 = descripteur 3-d
         ! 11 = marqueurs
         !

         bknat2 = ibits ( blk_in%btyp, 11, 2 )
         if (bknat2 == 3)  then
              blk_out = blk_in 
              return
         endif


         ! sinon faire les operaatins suivantes
         error = burp_noerr

         blk_out = blk_in
         ! si on genere un blk marqeur d'un bloc de datyp 6
         ! alors mettre le datyp =4
         if (blk_out%datyp ==6) blk_out%datyp = 4

         blk_out%lstele(:)     = blk_out%lstele  + 200000
         blk_out%dlstele(:)    = blk_out%dlstele + 200000
         blk_out%tblval(:,:,:) = 0 
         if (blk_out%convert .or. (blk_out%datyp==6)) then
            blk_out%rval(:,:,:)   = 0.0 
         end if

         ! marquer le block marqueur
         blk_out%btyp = ibset(blk_out%btyp,11)
         blk_out%btyp = ibset(blk_out%btyp,12)

       end function
       !
       ! make block clear
       ! reset les valeurs du blocs a -1 si ce n'est
       ! pas un marqueur et a 0 si c'est un marqeur
       function make_block_clear(blk_in) result(blk_out)
         implicit none
         type (burp_block),  intent (in)    :: blk_in
         type (burp_block)     :: blk_out
         integer(kind=int_def) :: error,bknat2

         ! verifier si blk_in est deja un block marqueur
         ! alors lui juste renvoyer une copie
         ! decomposition du btyp
         !   btyp    =      bknat       |      bktyp      |  bkstp
         !   15 bits =      4 bits      |      7 bits     |  4 bits
         !           =  bknat1 | bknat2 | bktyp1 | bktyp2 |  bkstp
         !           =  2 bits | 2 bits | 1 bit  | 6 bits |  4 bits
         !

         ! bknat1 = ibits ( my_btyp, 13, 2 )
         ! bktyp1 = ibits ( my_btyp, 10, 1 )
         ! bktyp2 = ibits ( my_btyp,  4, 6 )
         ! bkstp  = ibits ( my_btyp,  0, 4 )

         ! traitement de bknat2
         ! 00 = donnees
         ! 01 = info
         ! 10 = descripteur 3-d
         ! 11 = marqueurs
         !

         bknat2 = ibits ( blk_in%btyp, 11, 2 )
         if (bknat2 == 3)  then
              blk_out = blk_in 
              blk_out%tblval(:,:,:) = 0 
              if (blk_in%convert .or. (blk_in%datyp == 6)) blk_out%rval(:,:,:)   = 0.0 
         else
              blk_out = blk_in 
              blk_out%tblval(:,:,:) = -1 
              if (blk_in%convert .or. (blk_in%datyp == 6)) blk_out%rval(:,:,:)   = 0.0 
         endif


         ! sinon faire les operaatins suivantes
         error = burp_noerr
       end function

       !
       ! convert a returned value integer(kind=int_def) error status
       ! into an error message string
       !
       function burp_str_error(brperr) result(strerr)
       !
       implicit none
       integer(kind=int_def),optional,intent(in) :: brperr
       character(len = 80)                       :: strerr

       strerr = trim(getcurrentmessage(burp_error%error))
       return
       end function burp_str_error

       ! interrogation de la varibale erreur
       ! je ne vais l'utilser dans ce code
       ! je me contente d'imprimer juste le message d'erreur
       !
       ! print *, "success? ", stateissuccess(burp_error%error), &
       !          "warning? ", stateiswarning(burp_error%error), &
       !          "failure? ", stateisfailure(burp_error%error), &
       !           trim(getcurrentmessage(burp_error%error))

       subroutine burp_str_error_history()
       !
       implicit none
       call firstmessage(burp_error%error)
       do
          print *,  trim(getcurrentmessage(burp_error%error))

          if (moremessagesexist(burp_error%error)) then
             call nextmessage(burp_error%error)
             cycle
          end if
          exit
       end do
       end subroutine


end module burp_block_class
