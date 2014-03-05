!
module burp_rpt_class
     use burp_constants
     use errormessages
     use librmn_declaration
     use object_initialization
     use burp_block_class
     implicit none

     !
     ! define burp_rpt type
     !
     type burp_rpt
       private
       integer(kind=int_def),dimension(:),pointer:: buffer ! buffer array
       character (len =9)                        :: stnid  ! station id
       integer(kind=int_def)                     :: handle ! rpt adress
       integer(kind=int_def)                     :: nsize  ! buffer size
       integer(kind=int_def)                     :: temps  !
       integer(kind=int_def)                     :: flgs   ! flags
       integer(kind=int_def)                     :: idtyp ! code type
       integer(kind=int_def)                     :: lati   ! latitude
       integer(kind=int_def)                     :: long   ! longitude
       integer(kind=int_def)                     :: dx     ! x box size
       integer(kind=int_def)                     :: dy     ! y box size
       integer(kind=int_def)                     :: elev   !
       integer(kind=int_def)                     :: drnd   !
       integer(kind=int_def)                     :: date   !
       integer(kind=int_def)                     :: oars   !
       integer(kind=int_def)                     :: runn   !
       integer(kind=int_def)                     :: lngr   !
       integer(kind=int_def)                     :: nblk   ! nbre blocks
       integer(kind=int_def)                     :: file_io! from file io unit 
       integer(kind=int_def),dimension(:),pointer:: sup    !
       integer(kind=int_def)                     :: nsup   !
       integer(kind=int_def),dimension(:),pointer:: xaux   !
       integer(kind=int_def)                     :: nxaux  !
       type(t_init)                              :: init   ! check initialisation

     end type burp_rpt

     !
     ! interfaces
     !

     interface burp_new
       module procedure init_burp_rpt_to_write
     end interface

     interface burp_to_stdout
       module procedure print_burp_rpt
     end interface


     interface burp_get_property
       module procedure rpt_get_property
     end interface

     interface burp_set_property
       module procedure rpt_set_property
     end interface

     interface assignment(=)
       module procedure rpt_affectation
     end interface

     contains

       !
       ! constructor pour initilser le type derivee
       !
       subroutine init_burp_rpt(this,iostat)
        type (burp_rpt),      intent(inout)       :: this
        integer(kind=int_def),optional,intent(out):: iostat
        integer(kind=int_def)                     :: error
        error = -1


        call initialize(this%init)
        nullify(this%buffer)
        nullify(this%sup)
        nullify(this%xaux)

        this%stnid  ='*********'
        this%handle = 0
        this%nsize  = 0
        this%temps  = 0
        this%flgs   = 0
        this%idtyp  = 0
        this%lati   = 0
        this%long   = 0
        this%dx     = 0
        this%dy     = 0
        this%elev   = 0
        this%drnd   = 0
        this%date   = 0
        this%oars   = 0
        this%runn   = 0
        this%lngr   = 0
        this%nblk   = 0
        this%nsup   = 0
        this%nxaux  = 0

       if (present(iostat))  iostat = burp_noerr
       end subroutine init_burp_rpt

       !
       ! constructor pour creer un nouveau block
       !
       subroutine init_burp_rpt_to_write(this,            &
         alloc_space,                                     &
         stnid,                                           &
         handle,                                          &
         temps,                                           &
         flgs,                                            &
         idtyp,                                           &
         lati,                                            &
         long,                                            &
         dx,                                              &
         dy,                                              &
         elev,                                            &
         drnd,                                            &
         date,                                            &
         oars,                                            &
         runn,                                            &
         lngr,                                            &
         nblk,                                            &
         nsup,                                            &
         nxaux,                                           &
         iostat)

         type (burp_rpt),      intent(inout)         :: this
         integer(kind=int_def),intent(in)            :: alloc_space
         integer(kind=int_def),optional,intent(inout):: iostat
         integer(kind=int_def)                       :: error

         character (len =*),    optional, intent(in) :: stnid
         integer(kind=int_def), optional, intent(in) :: handle
         integer(kind=int_def), optional, intent(in) :: temps
         integer(kind=int_def), optional, intent(in) :: flgs
         integer(kind=int_def), optional, intent(in) :: idtyp
         integer(kind=int_def), optional, intent(in) :: lati
         integer(kind=int_def), optional, intent(in) :: long
         integer(kind=int_def), optional, intent(in) :: dx
         integer(kind=int_def), optional, intent(in) :: dy
         integer(kind=int_def), optional, intent(in) :: elev
         integer(kind=int_def), optional, intent(in) :: drnd
         integer(kind=int_def), optional, intent(in) :: date
         integer(kind=int_def), optional, intent(in) :: oars
         integer(kind=int_def), optional, intent(in) :: runn
         integer(kind=int_def), optional, intent(in) :: lngr
         integer(kind=int_def), optional, intent(in) :: nblk
         integer(kind=int_def), optional, intent(in) :: nsup
         integer(kind=int_def), optional, intent(in) :: nxaux

         error = burp_noerr

         if (.not.is_init(this%init)) then
                call init_burp_rpt(this,error)
                if (error /= burp_noerr) then
                   if (present(iostat))  iostat = error
                   return
                endif
         else
                call free_burp_rpt(this,error)
                if (error /= burp_noerr) then
                   if (present(iostat))  iostat = error
                   return
                endif

         end if

         if (alloc_space <= 0) then
              error = -1
              call setstatetofailure(burp_error%error, &
              "failure >> allocate alloc_space must be >0 - init_burp_to_write")
              if (present(iostat))  iostat = error
              return
         endif
            
         allocate(this%buffer(alloc_space),stat=error)
         if (error /= 0) then
             call setstatetofailure(burp_error%error, &
             "failure >> allocate memory - buffer - init_burp_to_write")
             if (present(iostat))  iostat = error
             return
         endif
         allocate(this%sup(this%nsup),stat=error)
         if (error /= 0) then
             call setstatetofailure(burp_error%error, &
             "failure >> allocate memory - nsup - init_burp_to_write")
             if (present(iostat))  iostat = error
             return
         endif
         allocate(this%xaux(this%nxaux),stat=error)
         if (error /= 0) then
             call setstatetofailure(burp_error%error, &
             "failure >> allocate memory - xaux - init_burp_to_write")
             if (present(iostat))  iostat = error
             return
         endif
         this%nsize     = alloc_space
         this%buffer(:) = 0
         this%buffer(1) = alloc_space

         if (present(stnid )) this%stnid  = stnid
         if (present(handle)) this%handle = handle
         if (present(temps )) this%temps  = temps
         if (present(flgs  )) this%flgs   = flgs
         if (present(idtyp)) this%idtyp = idtyp
         if (present(lati  )) this%lati   = lati
         if (present(long  )) this%long   = long
         if (present(dx    )) this%dx     = dx
         if (present(dy    )) this%dy     = dy
         if (present(elev  )) this%elev   = elev
         if (present(drnd  )) this%drnd   = drnd
         if (present(date  )) this%date   = date
         if (present(oars  )) this%oars   = oars
         if (present(runn  )) this%runn   = runn
         if (present(lngr  )) this%lngr   = lngr
         if (present(nblk  )) this%nblk   = nblk
         if (present(nsup  )) this%nsup   = nsup
         if (present(nxaux )) this%nxaux  = nxaux
         if (present(iostat))  iostat = error

       end subroutine init_burp_rpt_to_write

       !
       ! printing properties -debug-
       !
       subroutine print_burp_rpt(this,iostat)
        integer(kind = int_def), optional, intent (inout) :: iostat
        type (burp_rpt), intent (inout) :: this
        integer(kind = int_def)     :: error

        error =burp_noerr
        if (.not.is_init(this%init)) then
           call init_burp_rpt(this,error)
        endif

!        write(*,*) ' handle = ', this%handle
!        write(*,*) ' nsize  = ', this%nsize
!        write(*,*) ' temps  = ', this%temps
!        write(*,*) ' lngr   = ', this%lngr

        !
        ! output like liburp (jose programm)
        !
        write(*,fmt='(a,i8)', advance = 'no') ' hhmm   = ', this%temps
        write(*,fmt='(a,i6)', advance = 'no') ' flgs   = ', this%flgs
        write(*,fmt='(a,i6)', advance = 'no') ' codtyp = ', this%idtyp
        write(*,fmt='(a,a9)') ' stnids = ',  this%stnid
        write(*,fmt='(a,i8)', advance = 'no') ' blat   = ', this%lati
        write(*,fmt='(a,i6)', advance = 'no') ' blon   = ', this%long
        write(*,fmt='(a,i6)', advance = 'no') ' dx     = ', this%dx
        write(*,fmt='(a,i6)', advance = 'no') ' dy     = ', this%dy
        write(*,fmt='(a,i6)') ' stnhgt = ', this%elev
        write(*,fmt='(a,i8)', advance = 'no') ' yymmdd = ', this%date
        write(*,fmt='(a,i6)', advance = 'no') ' oars   = ', this%oars
        write(*,fmt='(a,i6)', advance = 'no') ' runn   = ', this%runn
        write(*,fmt='(a,i6)', advance = 'no') ' nblk   = ', this%nblk
        write(*,fmt='(a,i6/)') ' dlay   = ', this%drnd

        if (present(iostat))  iostat = error

       end subroutine print_burp_rpt

       !
       ! elemental destructor
       !
       subroutine free_burp_rpt(this,iostat)
        type (burp_rpt),   intent (inout) :: this
        integer(kind=int_def), optional, intent (out)   :: iostat
        integer(kind=int_def)                           :: error
        error = burp_noerr

        if (.not.is_init(this%init)) then
                call init_burp_rpt(this,error)
                if (error /= burp_noerr) return
        end if
        if (associated(this%buffer)) then
         deallocate(this%buffer,stat=error)
         nullify(this%buffer)
        end if
        if (error /= 0) then
           call setstatetofailure(burp_error%error, &
           "failure >> deallocate memory - buffer - free_burp_rpt")
           if (present(iostat))  iostat = error
           return
        endif
        if (associated(this%sup))  then
         deallocate(this%sup, stat=error)
         nullify(this%sup)
        end if
        if (associated(this%xaux)) then
         deallocate(this%xaux,stat=error)
         nullify(this%sup)
        end if
        if (error /= 0) then
           call setstatetofailure(burp_error%error, &
           "failure >> deallocate memory - sup/xaux - free_burp_rpt")
           if (present(iostat))  iostat = error
           return
        endif

        if (present(iostat))  iostat = burp_noerr
       end subroutine free_burp_rpt

       !
       ! multi elemental destructor
       !
       subroutine free_all_burp_rpt(r1,r2,r3,r4,r5,&
                                    r6,r7,r8,r9,r10,iostat)
        type (burp_rpt), intent(inout)            :: r1
        type (burp_rpt), optional, intent (inout) :: r2
        type (burp_rpt), optional, intent (inout) :: r3
        type (burp_rpt), optional, intent (inout) :: r4
        type (burp_rpt), optional, intent (inout) :: r5
        type (burp_rpt), optional, intent (inout) :: r6
        type (burp_rpt), optional, intent (inout) :: r7
        type (burp_rpt), optional, intent (inout) :: r8
        type (burp_rpt), optional, intent (inout) :: r9
        type (burp_rpt), optional, intent (inout) :: r10
        integer(kind=int_def),optional,intent(out):: iostat
        integer(kind=int_def)                     :: error
        error = burp_noerr
        call free_burp_rpt(r1, error)
        if (error /= burp_noerr) then
                if (present(iostat))  iostat = error
                return
        endif
        if (present(r2)) then
                call free_burp_rpt(r2, error)
                if (present(iostat))  iostat = error
                if (error /= burp_noerr) return
        endif
        if (present(r3)) then
                call free_burp_rpt(r3, error)
                if (present(iostat))  iostat = error
                if (error /= burp_noerr) return
        endif
        if (present(r4)) then
                call free_burp_rpt(r4, error)
                if (present(iostat))  iostat = error
                if (error /= burp_noerr) return
        endif
        if (present(r5)) then
                call free_burp_rpt(r5, error)
                if (present(iostat))  iostat = error
                if (error /= burp_noerr) return
        endif
        if (present(r6)) then
                call free_burp_rpt(r6, error)
                if (present(iostat))  iostat = error
                if (error /= burp_noerr) return
        endif
        if (present(r7)) then
                call free_burp_rpt(r7, error)
                if (present(iostat))  iostat = error
                if (error /= burp_noerr) return
        endif
        if (present(r8)) then
                call free_burp_rpt(r8, error)
                if (present(iostat))  iostat = error
                if (error /= burp_noerr) return
        endif
        if (present(r9)) then
                call free_burp_rpt(r9, error)
                if (present(iostat))  iostat = error
                if (error /= burp_noerr) return
        endif
        if (present(r10)) then
                call free_burp_rpt(r10, error)
                if (present(iostat))  iostat = error
                if (error /= burp_noerr) return
        endif
        if (present(iostat))  iostat = error

        end subroutine free_all_burp_rpt

       !
       ! multi elemental initiaseur de type 
       !
       subroutine init_all_burp_rpt(r1,r2,r3,r4,r5,&
                                    r6,r7,r8,r9,r10,iostat)
        type (burp_rpt), intent(inout)            :: r1
        type (burp_rpt), optional, intent (inout) :: r2
        type (burp_rpt), optional, intent (inout) :: r3
        type (burp_rpt), optional, intent (inout) :: r4
        type (burp_rpt), optional, intent (inout) :: r5
        type (burp_rpt), optional, intent (inout) :: r6
        type (burp_rpt), optional, intent (inout) :: r7
        type (burp_rpt), optional, intent (inout) :: r8
        type (burp_rpt), optional, intent (inout) :: r9
        type (burp_rpt), optional, intent (inout) :: r10
        integer(kind=int_def),optional,intent(out):: iostat
        integer(kind=int_def)                     :: error
        error = burp_noerr
        call init_burp_rpt(r1, error)
        if (error /= burp_noerr) then
                if (present(iostat))  iostat = error
                return
        endif
        if (present(r2)) then
                call init_burp_rpt(r2, error)
                if (present(iostat))  iostat = error
                if (error /= burp_noerr) return
        endif
        if (present(r3)) then
                call init_burp_rpt(r3, error)
                if (present(iostat))  iostat = error
                if (error /= burp_noerr) return
        endif
        if (present(r4)) then
                call init_burp_rpt(r4, error)
                if (present(iostat))  iostat = error
                if (error /= burp_noerr) return
        endif
        if (present(r5)) then
                call init_burp_rpt(r5, error)
                if (present(iostat))  iostat = error
                if (error /= burp_noerr) return
        endif
        if (present(r6)) then
                call init_burp_rpt(r6, error)
                if (present(iostat))  iostat = error
                if (error /= burp_noerr) return
        endif
        if (present(r7)) then
                call init_burp_rpt(r7, error)
                if (present(iostat))  iostat = error
                if (error /= burp_noerr) return
        endif
        if (present(r8)) then
                call init_burp_rpt(r8, error)
                if (present(iostat))  iostat = error
                if (error /= burp_noerr) return
        endif
        if (present(r9)) then
                call init_burp_rpt(r9, error)
                if (present(iostat))  iostat = error
                if (error /= burp_noerr) return
        endif
        if (present(r10)) then
                call init_burp_rpt(r10, error)
                if (present(iostat))  iostat = error
                if (error /= burp_noerr) return
        endif
        if (present(iostat))  iostat = error

        end subroutine 
       !
       ! setting properties
       !
       subroutine rpt_set_property(this,                  &
         stnid,                                           &
         handle,                                          &
         nsize,                                           &
         temps,                                           &
         flgs,                                            &
         idtyp,                                          &
         lati,                                            &
         long,                                            &
         dx,                                              &
         dy,                                              &
         elev,                                            &
         drnd,                                            &
         date,                                            &
         oars,                                            &
         runn,                                            &
         lngr,                                            &
         nblk,                                            &
         nsup,                                            &
         nxaux,                                           &
         iostat)

         type (burp_rpt),      intent(inout)         :: this
         integer(kind=int_def),optional,intent(inout):: iostat
         integer(kind=int_def)                       :: error 

         character (len =*),optional, intent(in)     :: stnid
         integer(kind=int_def), optional, intent(in) :: handle
         integer(kind=int_def), optional, intent(in) :: nsize
         integer(kind=int_def), optional, intent(in) :: temps
         integer(kind=int_def), optional, intent(in) :: flgs
         integer(kind=int_def), optional, intent(in) :: idtyp
         integer(kind=int_def), optional, intent(in) :: lati
         integer(kind=int_def), optional, intent(in) :: long
         integer(kind=int_def), optional, intent(in) :: dx
         integer(kind=int_def), optional, intent(in) :: dy
         integer(kind=int_def), optional, intent(in) :: elev
         integer(kind=int_def), optional, intent(in) :: drnd
         integer(kind=int_def), optional, intent(in) :: date
         integer(kind=int_def), optional, intent(in) :: oars
         integer(kind=int_def), optional, intent(in) :: runn
         integer(kind=int_def), optional, intent(in) :: lngr
         integer(kind=int_def), optional, intent(in) :: nblk
         integer(kind=int_def), optional, intent(in) :: nsup
         integer(kind=int_def), optional, intent(in) :: nxaux

	 error = burp_noerr
         if (.not.is_init(this%init)) then
                call init_burp_rpt(this,error)
         end if


         if (present(stnid )) this%stnid  = stnid
         if (present(handle)) this%handle = handle
         if (present(nsize )) this%nsize  = nsize
         if (present(temps )) this%temps  = temps
         if (present(flgs  )) this%flgs   = flgs
         if (present(idtyp)) this%idtyp = idtyp
         if (present(lati  )) this%lati   = lati
         if (present(long  )) this%long   = long
         if (present(dx    )) this%dx     = dx
         if (present(dy    )) this%dy     = dy
         if (present(elev  )) this%elev   = elev
         if (present(drnd  )) this%drnd   = drnd
         if (present(date  )) this%date   = date
         if (present(oars  )) this%oars   = oars
         if (present(runn  )) this%runn   = runn
         if (present(lngr  )) this%lngr   = lngr
         if (present(nblk  )) this%nblk   = nblk
         if (present(nsup  )) this%nsup   = nsup
         if (present(nxaux )) this%nxaux  = nxaux
         if (present(iostat))  iostat = error
       end subroutine rpt_set_property

       !
       ! getting properties
       !
       subroutine rpt_get_property(this,                  &
         stnid,                                           &
         handle,                                          &
         nsize,                                           &
         temps,                                           &
         flgs,                                            &
         idtyp,                                          &
         lati,                                            &
         long,                                            &
         dx,                                              &
         dy,                                              &
         elev,                                            &
         drnd,                                            &
         date,                                            &
         oars,                                            &
         runn,                                            &
         lngr,                                            &
         nblk,                                            &
         file_io,                                       &
         nsup,                                            &
         nxaux,                                           &
         iostat)

         type (burp_rpt),      intent(in)             :: this
         integer(kind=int_def),optional,intent(inout) :: iostat
         integer(kind=int_def)                        :: error

         character (len =9),optional, intent(out)     :: stnid
         integer(kind=int_def), optional, intent(out) :: handle
         integer(kind=int_def), optional, intent(out) :: nsize
         integer(kind=int_def), optional, intent(out) :: temps
         integer(kind=int_def), optional, intent(out) :: flgs
         integer(kind=int_def), optional, intent(out) :: idtyp
         integer(kind=int_def), optional, intent(out) :: lati
         integer(kind=int_def), optional, intent(out) :: long
         integer(kind=int_def), optional, intent(out) :: dx
         integer(kind=int_def), optional, intent(out) :: dy
         integer(kind=int_def), optional, intent(out) :: elev
         integer(kind=int_def), optional, intent(out) :: drnd
         integer(kind=int_def), optional, intent(out) :: date
         integer(kind=int_def), optional, intent(out) :: oars
         integer(kind=int_def), optional, intent(out) :: runn
         integer(kind=int_def), optional, intent(out) :: lngr
         integer(kind=int_def), optional, intent(out) :: nblk
         integer(kind=int_def), optional, intent(out) :: file_io 
         integer(kind=int_def), optional, intent(out) :: nsup
         integer(kind=int_def), optional, intent(out) :: nxaux

         error = -1

         if (present(stnid) .or. present(handle) .or. present(nsize) .or.&
             present(temps) .or. present(flgs)   .or. present(idtyp).or.&
             present(lati)  .or. present(long)   .or. present(dx)    .or.&
             present(dy)    .or. present(elev)   .or. present(drnd)  .or.&
             present(date)  .or. present(oars)   .or. present(runn)  .or.&
             present(lngr)  .or. present(nblk)   .or. present(nsup)  .or.&
             present(file_io)  .or. present(nxaux)) error = burp_noerr

         if (error /= burp_noerr) then
            if (present(iostat))  iostat = error
            call setstatetofailure(burp_error%error, &
            "failure >> rpt getting property: what property your looking for?? ")
            return
         endif

         if (present(stnid )) stnid  = this%stnid
         if (present(handle)) handle = this%handle
         if (present(nsize )) nsize  = this%nsize
         if (present(temps )) temps  = this%temps
         if (present(flgs  )) flgs   = this%flgs
         if (present(idtyp)) idtyp = this%idtyp
         if (present(lati  )) lati   = this%lati
         if (present(long  )) long   = this%long
         if (present(dx    )) dx     = this%dx
         if (present(dy    )) dy     = this%dy
         if (present(elev  )) elev   = this%elev
         if (present(drnd  )) drnd   = this%drnd
         if (present(date  )) date   = this%date
         if (present(oars  )) oars   = this%oars
         if (present(runn  )) runn   = this%runn
         if (present(lngr  )) lngr   = this%lngr
         if (present(nblk  )) nblk   = this%nblk
         if (present(file_io)) file_io  = this%file_io
         if (present(nsup  )) nsup   = this%nsup
         if (present(nxaux )) nxaux  = this%nxaux
         if (present(iostat))  iostat = error

       end subroutine rpt_get_property

       !
       ! getting report and put it in array buffer
       !
       subroutine rpt_get_report(this,handle,max_len,file_io,iostat)
         type (burp_rpt),      intent(inout)         :: this
         integer(kind=int_def),intent(in)            :: max_len,file_io
         integer(kind=int_def),intent(in)            :: handle
         integer(kind=int_def),optional,intent(inout):: iostat
         integer(kind=int_def)                       :: err
         integer(kind=int_def)                       :: error

         error = burp_noerr
         if (max_len <= 0) then
         if (present(iostat))  iostat = -1
             call setstatetofailure(burp_error%error, &
             "failure >> max_len must be > 0 - brp_get_rpt")
              return
         endif


         ! si le rapport a une longueur egale a lalngueur du plus long
         ! rapport dans le fichier, on ne redimensionne pas le tableau buffer
         ! ici je ne verifie pas si l'objet rapport est initilise
         ! car un rapport non initilise a nsize =0 et comme max_len <=0 n'est
         ! pas accepte par ;e test ci-haut, je ne risque pas d'aller remplir
         ! un objet rapport non inialise
         if (this%nsize == max_len) then
             error  = mrfget(handle,this%buffer)
             if (present(iostat))  iostat = error
             if (error /= 0) then
                 call setstatetofailure(burp_error%error, &
                 "failure >> to get report - librmn_mrfget")
                 return
             endif
             this%handle = handle
             error  = mrbhdr(this%buffer, this%temps, this%flgs,this%stnid, &
                    this%idtyp, this%lati, this%long, this%dx, this%dy,  &
                    this%elev,this%drnd, this%date, this%oars, this%runn, &
                    this%nblk,this%sup, this%nsup,this%xaux, this%nxaux)
             this%file_io = file_io
             if (present(iostat))  iostat = error
             if (error /= 0) then
                 call setstatetofailure(burp_error%error, &
                 "failure >> to get report header - librmn_mrbhdr")
                 return
             endif
             return ! ontermine ici 
         endif

         ! dans tous les autres cas, il faut redimensionner
         if (.not.is_init(this%init)) then
                call init_burp_rpt(this, error)
                if (error /= burp_noerr) return
         else
                call free_burp_rpt(this, error)
                if (error /= burp_noerr) return
         endif

         allocate(this%buffer(max_len),stat=error)
         if (error /= 0) then
             call setstatetofailure(burp_error%error, &
             "failure >> allocate memory - buffer - brp_get_rpt")
             if (present(iostat))  iostat = error
             return
         endif
         allocate(this%sup(this%nsup), stat = error)
         allocate(this%xaux(this%nxaux), stat = error)
         if (error /= 0) then
             call setstatetofailure(burp_error%error, &
             "failure >> allocate memory - sup/xaux - brp_get_rpt")
             if (present(iostat))  iostat = error
             return
         endif
         this%nsize     = max_len
         this%buffer(:) = 0
         this%buffer(1) = max_len

         error  = mrfget(handle,this%buffer)
         if (present(iostat))  iostat = error
         if (error /= 0) then
             call setstatetofailure(burp_error%error, &
             "failure >> to get report - librmn_mrfget")
             return
         endif
         this%handle = handle
         error  = mrbhdr(this%buffer, this%temps, this%flgs,this%stnid, &
                this%idtyp, this%lati, this%long, this%dx, this%dy,  &
                this%elev,this%drnd, this%date, this%oars, this%runn, &
                this%nblk,this%sup, this%nsup,this%xaux, this%nxaux)
         this%file_io = file_io
         if (present(iostat))  iostat = error
         if (error /= 0) then
             call setstatetofailure(burp_error%error, &
             "failure >> to get report header - librmn_mrbhdr")
             return
         endif


       end subroutine rpt_get_report
       
       !
       ! getting only report principle params 
       !
       subroutine rpt_get_only_hdr(this,handle,max_len,file_io,iostat)
         type (burp_rpt),      intent(inout)         :: this
         integer(kind=int_def),intent(in)            :: max_len,file_io
         integer(kind=int_def),intent(in)            :: handle
         integer(kind=int_def),optional,intent(inout):: iostat
         integer(kind=int_def)                       :: err
         integer(kind=int_def)                       :: error

         error = burp_noerr
         this%nsize     = max_len
         this%handle    = handle
         if (.not.is_init(this%init)) then
                call init_burp_rpt(this, error)
                if (error /= burp_noerr) return
         endif

         this%nsup = 0
         allocate(this%sup(1))
         this%sup(:) = -1
         error = mrfprm ( handle, this%stnid, this%idtyp, this%lati, this%long,this%dx, &
                this%dy, this%date, this%temps, this%flgs, this%sup, this%nsup, this%lngr )

         this%file_io = file_io
         if (present(iostat))  iostat = error
         if (error /= 0) then
             call setstatetofailure(burp_error%error, &
             "failure >> to get report header - librmn_mrbhdr")
             return
         endif

        deallocate(this%sup)

       end subroutine 

       !
       ! finding block
       !
       function burp_find_block(this,block,search_from,             &
                bfam, bdesc, btyp,                                  &
                bknat, bktyp, bkstp,convert,get,iostat) result(ref)
         type (burp_rpt),  intent(in)   :: this
         type (burp_block),intent(inout):: block
         logical,optional,intent(in)    :: get
         logical,optional,intent(in)    :: convert
         integer(kind=int_def),optional,intent(inout):: iostat
         integer(kind=int_def),optional,intent(in)   :: search_from,&
                                                        bfam,       &
                                                        bdesc,      &
                                                        btyp,       &
                                                        bknat,      &
                                                        bktyp,      &
                                                        bkstp      

         logical              :: my_convert
         integer(kind=int_def):: my_blk0,    &
                                 my_bfam,    &
                                 my_bdesc,   &
                                 my_btyp,    &
                                 my_bkstp,   &
                                 my_bknat,   &
                                 my_bktyp,   &
                                 err,        &
                                 ref,        &
                                 error
         logical:: all_present,un_seul
         logical::get_it

         all_present = .false.
         un_seul     = .false.
         get_it = .TRUE.
         error = burp_noerr
         !
         ! defaults
         !
         my_blk0 = 0;  my_bfam = -1; my_bdesc = -1;my_btyp = -1
         my_bknat= 0;  my_bktyp= 0 ; my_bkstp = 0; my_convert =.TRUE.

         if (present(bknat) .and. present(bktyp) .and. present(bkstp))&
            all_present = .true.
         if (present(bknat) .or. present(bktyp) .or. present(bkstp))  &
            un_seul = .true.

         if (present(get))          get_it      = get
         if (present(search_from))  my_blk0     = search_from
         if (present(bfam))         my_bfam     = bfam
         if (present(bdesc))        my_bdesc    = bdesc
         if (present(btyp))         my_btyp     = btyp
         if (present(bknat))        my_bknat    = bknat
         if (present(bktyp))        my_bktyp    = bktyp
         if (present(bkstp))        my_bkstp    = bkstp
         if (present(convert))      my_convert  = convert

         if (all_present) then
             ref  = mrblocx(this%buffer,my_bfam, my_bdesc, my_bknat,   &
                            my_bktyp, my_bkstp,my_blk0 )
         elseif (un_seul) then
              error = -1
              if (present(iostat))  iostat = error
              call setstatetofailure(burp_error%error,        &
              "failure >> bknat and bktyp and bkstp must be present together!!")
              return
         else
             ref  = mrbloc(this%buffer,my_bfam, my_bdesc, my_btyp,   &
                           my_blk0 )
         endif
         if (ref > 0) then
            if (get_it) then
                call blk_get_block(block,this%buffer,ref,my_convert,error)
             else
                call blk_get_only_hdr(block,this%buffer,ref,my_convert,error)
             end if
         end if
         if (present(iostat))  iostat = error
         if (error /= burp_noerr) return
         call burp_set_property(block,rpt_handle=this%handle,iostat=error)
         if (present(iostat))  iostat = error

       end function burp_find_block

       !
       ! getting block if ref block known
       !
       subroutine burp_get_block(this,block,ref, convert,iostat) 
         type (burp_rpt),  intent(inout):: this
         type (burp_block),intent(inout):: block
         integer,intent(in)             :: ref
         logical,optional,intent(in)    :: convert
         integer(kind=int_def),optional,intent(inout):: iostat
         logical                                     :: my_convert
         integer                                     :: error
         error = burp_noerr
         !
         my_convert =.TRUE.
         if (present(convert))  then
            my_convert  = convert
         else
            ! sinon on regarde ce qui a mis dans la procedure
            ! burp_find_block
            ! les tags du type block sont publics 
            my_convert  = block%convert
         end if
         if (ref > 0) then
                call blk_get_block(block,this%buffer,ref,my_convert,error)
         end if
         if (present(iostat))  iostat = error
         if (error /= burp_noerr) return
         call burp_set_property(block,rpt_handle=this%handle,iostat=error)
         if (present(iostat))  iostat = error

       end subroutine 

       !
       ! called from burp_file_class
       ! putting report header
       !
       subroutine burp_report_mrbini(this,io_unit, iostat)
         type (burp_rpt),      intent(inout) :: this
         integer(kind=int_def),intent(in)    :: io_unit
         integer(kind=int_def)               :: error
         integer(kind=int_def),optional,intent (inout) :: iostat

         error = burp_noerr
         error  = mrbini(io_unit , this%buffer, this%temps, this%flgs, &
               this%stnid, this%idtyp, this%lati, this%long, this%dx, &
               this%dy, this%elev, this%drnd, this%date, this%oars,    &
               this%runn, this%sup, this%nsup, this%xaux, this%nxaux)
         if (error /= 0) then
            if (present(iostat))  iostat = error
            call setstatetofailure(burp_error%error,        &
            "failure >> to init rpt  header for writing:"//"&
            &burp_report_mrbini:rmn_mrbini")
            return
         endif
            if (present(iostat))  iostat = error

       end subroutine burp_report_mrbini


       ! called from burp_file_class
       ! writing report in file
       !
       subroutine burp_report_mrfput(this,io_unit,update, iostat )

         type (burp_rpt),      intent(inout)         :: this
         integer(kind=int_def),intent(in)            :: io_unit
         logical,              intent(in)            :: update
         integer(kind=int_def),optional,intent(inout):: iostat
         integer(kind=int_def)                       :: error,handle

         error = burp_noerr
         handle = 0
         if(update) handle=this%handle

         error  = mrbupd(io_unit , this%buffer, this%temps, this%flgs, &
                this%stnid, this%idtyp, this%lati, this%long, this%dx,&
                this%dy, this%elev, this%drnd, this%date, this%oars,   &
                this%runn, this%sup, this%nsup, this%xaux, this%nxaux)
         !
         ! error handling
         if (error /= 0) then
            if (present(iostat))  iostat = error
            call setstatetofailure(burp_error%error,         &
            "failure >> to update rpt header for writing:"//"&
            &burp_report_mrfput:rmn_mrbupd")
            return
         endif

         error  = mrfput(io_unit, handle,this%buffer)
         if (error /= 0) then
            if (present(iostat))  iostat = error
            call setstatetofailure(burp_error%error,           &
            "failure >> to erite report to file: rmn_mrfput")
            return
         endif
         ! recuperer l'entete du rapport apres l'enregistrement du rapport
         error  = mrbhdr(this%buffer, this%temps, this%flgs,this%stnid, &
               this%idtyp, this%lati, this%long, this%dx, this%dy,  &
               this%elev,this%drnd, this%date, this%oars, this%runn, &
               this%nblk,this%sup, this%nsup,this%xaux, this%nxaux)

         if (present(iostat))  iostat = error
       end subroutine burp_report_mrfput


       !
       ! delete  block
       ! associate this delete with a report object
       !
       subroutine burp_delete_block(this,block, iostat)
         implicit none
         type (burp_rpt),  intent(inout):: this
         type (burp_block),intent(in)   :: block
         type (burp_block)              :: myblock
         integer(kind=int_def),optional,intent(inout):: iostat
         integer(kind=int_def)                       :: error
         integer(kind=int_def)                       :: blkno
         integer(kind=int_def)                       :: bfam_in, &
                                                        bdesc_in,&
                                                        btyp_in, &
                                                        ref_blk, &
                                                        rpt_handle
         error   = burp_noerr
         ref_blk = 0
         call burp_get_property(block,iostat = error,    & 
         bfam = bfam_in,  btyp = btyp_in, bkno =ref_blk, & 
         bdesc = bdesc_in, rpt_handle = rpt_handle)
         if (error /= burp_noerr) then
             if (present(iostat))  iostat = error
             return
         endif
         if (rpt_handle /= this%handle) then
             if (present(iostat))  iostat = -1
                 call setstatetofailure(burp_error%error,           &
                 "failure >> to delete block : block is not from this report!")
             return
         endif
            
!              do
!                ref_blk = burp_find_block(this,myblock, &
!                   search_from = ref_blk,               &
!                   bfam        = bfam_in,               &
!                   btyp        = btyp_in,               &
!                   bdesc       = bdesc_in,              &
!                   iostat = error)
!                if (error /= burp_noerr) then
!                    if (present(iostat))  iostat = error
!                    return
!                endif

!                if (ref_blk < 0) exit
                if (ref_blk < 0) return 
                error = mrbdel(this%buffer,ref_blk)
                if (error /= burp_noerr) then
                    if (present(iostat))  iostat = error
                    call setstatetofailure(burp_error%error,           &
                    "failure >> to delete block : burp_delete_block-rmn_mrbdel")
                    return
                endif

!              end do


         if (present(iostat))  iostat = error
       end subroutine burp_delete_block

       !
       ! add block to report
       !
       !
       subroutine burp_write_block(this,block,encode_block,&
                      convert_block,iostat)
         implicit none
         type (burp_rpt),  intent(inout):: this
         type (burp_block),intent(inout):: block
         integer(kind=int_def),optional,intent(inout):: iostat
         integer(kind=int_def)                       :: error
         logical,optional :: encode_block, convert_block
         logical             :: my_encode, my_convert

         error = burp_noerr
         my_encode = .true.
         my_convert= .false.

         if (present(encode_block)) then
            my_encode = encode_block
         end if
         if (present(convert_block))then
            my_convert= convert_block
         end if
         if (my_encode) then
            call burp_encode_block(block,iostat = error)
         end if
         if (present(iostat))  iostat = error
         if (error /= burp_noerr) return 
         if (my_convert) then 
            call burp_convert_block(block,iostat = error)
         end if
         if (present(iostat))  iostat = error
         if (error /= burp_noerr) return 

         call burp_block_mrbadd(block,this%buffer,iostat = error)
         if (present(iostat))  iostat = error

       end subroutine burp_write_block

       !
       !  rpt types affectation
       !
       !
       subroutine rpt_affectation(rpt_out,rpt_in)
         implicit none
         type (burp_rpt),  intent (inout) :: rpt_out
         type (burp_rpt),  intent (in)    :: rpt_in
         integer(kind=int_def)            :: error
         error = burp_noerr

         if (.not.is_init(rpt_out%init)) then
             call init_burp_rpt(rpt_out,error)
             if (error /= burp_noerr) then
                 write(*,*) burp_str_error()
                 stop
             endif
         else
             call free_burp_rpt(rpt_out,error)
             if (error /= burp_noerr) then
                 write(*,*) burp_str_error()
                 stop
             endif
             call init_burp_rpt(rpt_out,error)
             if (error /= burp_noerr) then
                 write(*,*) burp_str_error()
                 stop
             endif
         end if

         if (.not.is_init(rpt_in%init)) then
            return
         else

            call burp_new(rpt_out,alloc_space = rpt_in%nsize, &
                          iostat=error)
            if (error /= burp_noerr) then
                write(*,*) burp_str_error()
                stop
            endif
            rpt_out%stnid  = rpt_in%stnid
            rpt_out%handle = rpt_in%handle
            rpt_out%nsize  = rpt_in%nsize
            rpt_out%temps  = rpt_in%temps
            rpt_out%flgs   = rpt_in%flgs
            rpt_out%idtyp = rpt_in%idtyp
            rpt_out%lati   = rpt_in%lati
            rpt_out%long   = rpt_in%long
            rpt_out%dx     = rpt_in%dx
            rpt_out%dy     = rpt_in%dy
            rpt_out%elev   = rpt_in%elev
            rpt_out%drnd   = rpt_in%drnd
            rpt_out%date   = rpt_in%date
            rpt_out%oars   = rpt_in%oars
            rpt_out%runn   = rpt_in%runn
            rpt_out%lngr   = rpt_in%lngr
            rpt_out%nblk   = rpt_in%nblk
            rpt_out%nsup   = rpt_in%nsup
            rpt_out%nxaux  = rpt_in%nxaux

            rpt_out%buffer(:) = rpt_in%buffer(:)
            rpt_out%sup(:)    = rpt_in%sup(:)
            rpt_out%xaux(:)   = rpt_in%xaux(:)

         end if

       end subroutine rpt_affectation
       !
       !  rpt types copy header
       !
       !
       subroutine burp_copy_header(to,from)
         implicit none
         type (burp_rpt),  intent (inout) :: to
         type (burp_rpt),  intent (in)    :: from
         integer(kind=int_def)            :: error
         error = burp_noerr

         if (.not.is_init(to%init)) then
             call init_burp_rpt(to,error)
             if (error /= burp_noerr) then
                 write(*,*) burp_str_error()
                 stop
             endif
         end if

         if (.not.is_init(from%init)) then
             call init_burp_rpt(to,error)
             if (error /= burp_noerr) then
                 write(*,*) burp_str_error()
                 stop
             endif
         else

            to%stnid  = from%stnid
            to%handle = from%handle
            to%nsize  = from%nsize
            to%temps  = from%temps
            to%flgs   = from%flgs
            to%idtyp = from%idtyp
            to%lati   = from%lati
            to%long   = from%long
            to%dx     = from%dx
            to%dy     = from%dy
            to%elev   = from%elev
            to%drnd   = from%drnd
            to%date   = from%date
            to%oars   = from%oars
            to%runn   = from%runn
            to%lngr   = from%lngr
            to%nblk   = from%nblk
         end if

       end subroutine burp_copy_header

       !
       !  rpt  resize
       !
       !
       subroutine burp_resize_report(report,add_size,iostat)
         implicit none
         type (burp_rpt),  intent (inout)       :: report
         integer(kind = int_def), intent (in)    :: add_size
         integer(kind = int_def),optional,intent(inout)::iostat
         
         type(burp_rpt)          :: rpt_tmp
         integer(kind=int_def)   :: error
         error = burp_noerr

         if (add_size <=0 ) then
             error = -1
             call setstatetofailure(burp_error%error, &
             "failure >> add_size must > 0 : burp_resize_report!!")
             if (present(iostat))  iostat = error
             return
         endif
         ! initialiser le rapport s'il nest pas initialiser
         if (.not.is_init(report%init)) then
             call init_burp_rpt(report,error)
             if (present(iostat))  iostat = error
             if (error /= burp_noerr) return
         endif 

         ! creer un rapport temporaire avec nouveau size
         !
         call burp_new(rpt_tmp,alloc_space = report%nsize + add_size, &
                       iostat=error)
         if (error /= burp_noerr) return

         rpt_tmp%stnid  = report%stnid
         rpt_tmp%handle = report%handle

       ! copier toutes les proprietes
       ! sauf la diemnsion du rapport
       ! rpt_tmp%nsize  = report%nsize

         rpt_tmp%temps  = report%temps
         rpt_tmp%flgs   = report%flgs
         rpt_tmp%idtyp = report%idtyp
         rpt_tmp%lati   = report%lati
         rpt_tmp%long   = report%long
         rpt_tmp%dx     = report%dx
         rpt_tmp%dy     = report%dy
         rpt_tmp%elev   = report%elev
         rpt_tmp%drnd   = report%drnd
         rpt_tmp%date   = report%date
         rpt_tmp%oars   = report%oars
         rpt_tmp%runn   = report%runn
         rpt_tmp%lngr   = report%lngr
         rpt_tmp%nblk   = report%nblk
         rpt_tmp%nsup   = report%nsup
         rpt_tmp%nxaux  = report%nxaux

         rpt_tmp%buffer(:) = report%buffer(:)
         rpt_tmp%buffer(1) = rpt_tmp%nsize 

         rpt_tmp%sup(:)    = report%sup(:)
         rpt_tmp%xaux(:)   = report%xaux(:)
         !
         ! maintenant on affecte le rpt_tmp au rapport entre
          report = rpt_tmp
          call free_burp_rpt(rpt_tmp,iostat=error)
         if (present(iostat))  iostat = error

       end subroutine burp_resize_report


end module burp_rpt_class
