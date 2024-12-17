!!
!!
module burp_file_class
     use burp_constants
     use errormessages
     use librmn_declaration
     use burp_rpt_class       !<url:/users/dor/afsd/hmd/fortran/burp/burp_rpt_class.f90>
     implicit none

     !!
     !! define burp_file type
     !!
     type burp_file
      private
       character (len =256)  :: filename
       character (len =20)   :: mode
       integer(kind=int_def) :: io_unit
       integer(kind=int_def) :: nrpts
       integer(kind=int_def) :: max_len
       logical               :: init=.false.
     end type burp_file

     type burp_options
       character(len=20)     :: real_optname
       character(len=20)     :: char_optname
       character(len=20)     :: char_optname_value
       real                  :: real_optname_value
       logical               :: init=.false.
     end type burp_options

     type(burp_options),save :: var_burp_opt


     !!
     !! interfaces
     !!
     interface burp_new
       module procedure open_burp_file
     end interface


     interface burp_free
       module procedure close_all_burp_file
       module procedure free_all_burp_rpt
       module procedure free_all_burp_block
     end interface
     interface burp_init
       module procedure init_all_burp_file
       module procedure init_all_burp_rpt
       module procedure init_all_burp_block
     end interface

     interface burp_to_stdout
       module procedure print_burp_file
     end interface

     interface burp_get_property
       module procedure file_get_property
     end interface

     interface burp_set_property
       module procedure file_set_property
     end interface

     contains

       !!
       !! constructor
       !!
       subroutine open_burp_file(this,io_unit,filename,  &
                  mode,real_optname, real_optname_value, &
                  char_optname, char_optname_value,iostat)
         type (burp_file),     intent(inout)        :: this
         integer(kind=int_def),optional, intent(in ):: io_unit
         character(len=*),optional,intent(in)       :: real_optname
         character(len=*),optional,intent(in)       :: char_optname
         character(len=*),optional,intent(in)       :: char_optname_value
         real,            optional,intent(in)       :: real_optname_value

         integer(kind=int_def),optional,intent(out) :: iostat
         character(len =*),optional,intent(in)      :: filename
         character(len =*),optional,intent(in)      :: mode
         integer(kind=int_def)                      :: error
         logical                                    :: file_exist


         error = burp_noerr
         !
         ! initialization de la pile des messages
         ! d"ereurs. l'objet global est defini dans la
         ! classe burp_block_class.f90

         if (.not.burp_error%init) then
              burp_error%init = .true.
              call initializestate(burp_error%error)
         end if

         if (.not.present(filename)) then
             this%filename = ""
             this%mode     = ""
             this%io_unit  = -1
             this%nrpts    = 0
             this%max_len  = 0
             this%init = .true.
             if (present(iostat)) iostat = burp_noerr
             return
         endif
         if (.not.this%init) then
             this%init = .true.
             this%filename = ""
             this%mode     = ""
         endif

         ! checking if reading burp options are set
         ! real and char options
         ! var_burp_opt is persistent (global)
         !
         if (.not.var_burp_opt%init) then
             var_burp_opt%init = .true.
             ! values are from burp_constants module
             var_burp_opt%real_optname       = burp_real_opt_name
             var_burp_opt%real_optname_value = burp_real_opt_value
             var_burp_opt%char_optname       = burp_char_opt_name
             var_burp_opt%char_optname_value = burp_char_opt_value
             call burp_set_options(                         &
                real_optname       = burp_real_opt_name,    &
                real_optname_value = burp_real_opt_value,   &
                char_optname       = burp_char_opt_name,    &
                char_optname_value = burp_char_opt_value,   &
                iostat             = error)
                if (present(iostat))  iostat = error
                if (error /= burp_noerr) return
         endif

         if (  present(io_unit)) then
              this%io_unit = io_unit
         else
              this%io_unit = get_next_io_unit()
         end if

         if ((present(real_optname_value) .and.  &
            .not.present(real_optname)) .or.     &
            (.not.present(real_optname_value) .and. present(real_optname))) then
            if (present(iostat))  iostat = -1
            call setstatetofailure(burp_error%error,        &
            "failure >>  real option name or value missing!! ")
            return
         endif
         if ((present(char_optname_value) .and.  &
            .not.present(char_optname)) .or.     &
            (.not.present(char_optname_value) .and. present(char_optname))) then
            if (present(iostat))  iostat = -1
            call setstatetofailure(burp_error%error,        &
            "failure >>  char option name or value missing!! ")
            return
         endif

         ! setting real option name name and value
         !
         if (present(real_optname)) then
            call burp_set_options(                     &
               real_optname       = real_optname,      &
               real_optname_value = real_optname_value,&
               iostat             = error)
               if (present(iostat))  iostat = error
               if (error /= burp_noerr) return
         endif

         ! setting char option name name and value
         !
         if (present(char_optname)) then
            call burp_set_options(                     &
               char_optname       = char_optname,      &
               char_optname_value = char_optname_value,&
               iostat             = error)
               if (present(iostat))  iostat = error
               if (error /= burp_noerr) return
         endif


         ! check file
         !
         call burp_check_file(filename,file_exist,error)

         if (.not.present(mode)) then
            if (present(iostat))  iostat = -1
            call setstatetofailure(burp_error%error,           &
            "failure >> mode (file_acc_..)for opening file missing!!")
            return
         end if
         this%filename = filename(1:)

         !
         ! check mode
         if      ( mode .eq. file_acc_create ) then
           error = fnom ( this%io_unit, this%filename, 'rnd', 0 )
         else if ( mode .eq. file_acc_read   ) then
           if (.not.file_exist) then
              if (present(iostat))  iostat = -1
              return 
           endif
           error = fnom ( this%io_unit, this%filename, 'rnd+old', 0 )
         else if ( mode .eq. file_acc_append ) then
           if (.not.file_exist) then
              error = fnom ( this%io_unit, this%filename, 'rnd', 0 )
           else
              error = fnom ( this%io_unit, this%filename(1:),'rnd+append',0)
           endif
         else
            !
            ! error handling
            if (present(iostat))  iostat = -1
            call setstatetofailure(burp_error%error,           &
            "failure >> unknown mode for opening file")
            return
         end if
         this%mode     = mode

         !
         ! error handling
         if (error /= 0) then
            if (present(iostat))  iostat = error
            call setstatetofailure(burp_error%error,           &
            "failure >> to associate unit to filename: rmn_fnom")
            return
         else
            if (present(iostat))  iostat = error
            call setstatetosuccess(burp_error%error,           &
            "success >> to associate unit to filename: rmn_fnom")
         end if
         if (.not.file_exist .and. mode == file_acc_append) then
            this%nrpts    = mrfopn ( this%io_unit, file_acc_create )
         else
            this%nrpts    = mrfopn ( this%io_unit, mode )
         endif 
         !
         ! error handling
         if (this%nrpts < 0) then
            if (present(iostat))  iostat = this%nrpts 
            call setstatetofailure(burp_error%error,           &
            "failure >> to open burp file: rmn_mrfopn")
            return
         else
            if (present(iostat))  iostat = burp_noerr
            call setstatetosuccess(burp_error%error,           &
            "success >> to open burp file: rmn_mrfopn")
         end if

         if (mode == file_acc_read .or. mode == file_acc_append) then
                this%max_len      = mrfmxl(this%io_unit)
                ! si on ne connait pas la longueur du plus
                ! long rapport fixer une valeur arbitraire
                if (this%max_len == 0) then
                    this%max_len = 10000
                    call setstatetowarning(burp_error%error,    &
                   "warning >> to get the lenght of the lgst rpt: rmn_mrfmxl&
                    &  prog continue with max_len = 10000 ")
                else
                    this%max_len = this%max_len +1000
                end if
                !
                ! error handling
                if (this%max_len < 0) then
                   if (present(iostat))  iostat = this%max_len
                   call setstatetofailure(burp_error%error,    &
                   "failure >> to get the lenght of the lgst rpt: rmn_mrfmxl")
                   return
                else
                   if (present(iostat))  iostat = burp_noerr
                   call setstatetosuccess(burp_error%error,    &
                   "success >> to get the lenght of the lgst rpt: rmn_mrfopn")
                end if
         end if

!         ierr          = mrfvoi(this%io_unit)

       end subroutine open_burp_file

       !!
       !! destructor
       !!
       subroutine close_burp_file(this,iostat)
         type (burp_file),intent (inout)            :: this
         integer(kind=int_def),optional,intent (out):: iostat
         integer(kind=int_def)                      :: error

         if (.not.this%init) call open_burp_file(this,error)
         if (len_trim(this%filename) == 0) then
            if (present(iostat))  iostat = error
            return
         endif


         error = mrfcls ( this%io_unit )
         if (error /= burp_noerr) then
            if (present(iostat))  iostat = -1
            call setstatetofailure(burp_error%error,           &
            "failure >> to close "//this%filename(1:len_trim(this%filename))&
            &//" burp file: rmn_mrfcls")
            return
         else
            if (present(iostat))  iostat = burp_noerr
            call setstatetosuccess(burp_error%error,           &
            "success >> to close "//this%filename(1:len_trim(this%filename))&
            &//" burp file: rmn_mrfcls")
         end if

         error = fclos  ( this%io_unit )
         if (present(iostat))  iostat = burp_noerr
!         write  (*,*) 'file closed'
       end subroutine close_burp_file


       !!
       !! multi elemental constructor
       !!
       subroutine init_all_burp_file(f1,f2,f3,f4,f5,&
                                     f6,f7,f8,f9,f10,iostat)
        type (burp_file),intent (inout)             :: f1
        type (burp_file),optional,intent (inout)    :: f2
        type (burp_file),optional,intent (inout)    :: f3
        type (burp_file),optional,intent (inout)    :: f4
        type (burp_file),optional,intent (inout)    :: f5
        type (burp_file),optional,intent (inout)    :: f6
        type (burp_file),optional,intent (inout)    :: f7
        type (burp_file),optional,intent (inout)    :: f8
        type (burp_file),optional,intent (inout)    :: f9
        type (burp_file),optional,intent (inout)    :: f10
        integer(kind=int_def),optional,intent (out) :: iostat
        integer(kind=int_def)                       :: error
        call open_burp_file(f1, iostat=error)
        if (error /= burp_noerr) then
           if (present(iostat))  iostat = error
           return
        end if
        if (present(f2)) then
           call open_burp_file(f2, iostat=error)
           if (present(iostat))  iostat = error
           if (error /= burp_noerr)  return
        end if
        if (present(f3)) then
           call open_burp_file(f3, iostat=error)
           if (present(iostat))  iostat = error
           if (error /= burp_noerr)  return
        end if
        if (present(f4)) then
           call open_burp_file(f4, iostat=error)
           if (present(iostat))  iostat = error
           if (error /= burp_noerr)  return
        end if
        if (present(f5)) then
           call open_burp_file(f5, iostat=error)
           if (present(iostat))  iostat = error
           if (error /= burp_noerr)  return
        end if
        if (present(f6)) then
           call open_burp_file(f6, iostat=error)
           if (present(iostat))  iostat = error
           if (error /= burp_noerr)  return
        end if
        if (present(f7)) then
           call open_burp_file(f7, iostat=error)
           if (present(iostat))  iostat = error
           if (error /= burp_noerr)  return
        end if
        if (present(f8)) then
           call open_burp_file(f8, iostat=error)
           if (present(iostat))  iostat = error
           if (error /= burp_noerr)  return
        end if
        if (present(f9)) then
           call open_burp_file(f9, iostat=error)
           if (present(iostat))  iostat = error
           if (error /= burp_noerr)  return
        end if
        if (present(f10)) then
           call open_burp_file(f10, iostat=error)
           if (present(iostat))  iostat = error
           if (error /= burp_noerr)  return
        end if
        if (present(iostat))  iostat = burp_noerr

        end subroutine init_all_burp_file

       !!
       !! multi elemental destructor
       !!
       subroutine close_all_burp_file(f1,f2,f3,f4,f5,&
                                       f6,f7,f8,f9,f10,iostat)
        type (burp_file),intent (inout)             :: f1
        type (burp_file),optional,intent (inout)    :: f2
        type (burp_file),optional,intent (inout)    :: f3
        type (burp_file),optional,intent (inout)    :: f4
        type (burp_file),optional,intent (inout)    :: f5
        type (burp_file),optional,intent (inout)    :: f6
        type (burp_file),optional,intent (inout)    :: f7
        type (burp_file),optional,intent (inout)    :: f8
        type (burp_file),optional,intent (inout)    :: f9
        type (burp_file),optional,intent (inout)    :: f10
        integer(kind=int_def),optional,intent (out) :: iostat
        integer(kind=int_def)                       :: error
        call close_burp_file(f1, error)
        if (error /= burp_noerr) then
           if (present(iostat))  iostat = error
           return
        end if
        if (present(f2)) then
           call close_burp_file(f2, error)
           if (present(iostat))  iostat = error
           if (error /= burp_noerr)  return
        end if
        if (present(f3)) then
           call close_burp_file(f3, error)
           if (present(iostat))  iostat = error
           if (error /= burp_noerr)  return
        end if
        if (present(f4)) then
           call close_burp_file(f4, error)
           if (present(iostat))  iostat = error
           if (error /= burp_noerr)  return
        end if
        if (present(f5)) then
           call close_burp_file(f5, error)
           if (present(iostat))  iostat = error
           if (error /= burp_noerr)  return
        end if
        if (present(f6)) then
           call close_burp_file(f6, error)
           if (present(iostat))  iostat = error
           if (error /= burp_noerr)  return
        end if
        if (present(f7)) then
           call close_burp_file(f7, error)
           if (present(iostat))  iostat = error
           if (error /= burp_noerr)  return
        end if
        if (present(f8)) then
           call close_burp_file(f8, error)
           if (present(iostat))  iostat = error
           if (error /= burp_noerr)  return
        end if
        if (present(f9)) then
           call close_burp_file(f9, error)
           if (present(iostat))  iostat = error
           if (error /= burp_noerr)  return
        end if
        if (present(f10)) then
           call close_burp_file(f10, error)
           if (present(iostat))  iostat = error
           if (error /= burp_noerr)  return
        end if
        if (present(iostat))  iostat = burp_noerr

        end subroutine close_all_burp_file

       !!
       !! printing properties -debug-
       !!
       subroutine print_burp_file(this,print_unit,iostat)
         integer(kind = int_def), optional, intent (inout) :: iostat
         type (burp_file), intent (in)  :: this
         logical, optional, intent (in) :: print_unit
         integer(kind = int_def)        :: error

         error =burp_noerr
         if (.not.this%init) then
            if (present(iostat))  iostat = -1
            call setstatetofailure(burp_error%error,           &
            "failure >> burp_to_stdout(file_obj) must be used for objects &
            &already initialized!! use burp_new(your object) first")
            return
         endif

         if (present(print_unit)) then
             if (print_unit) then
                write (*,'(i2,a2)',advance='no') this%io_unit,': '
             end if
         end if

         write (*,*) 'file_filename  = ', trim(this%filename)
         write (*,*) 'file_nbrpts    = ', this%nrpts
         if (present(iostat))  iostat = error
       end subroutine print_burp_file

       !!
       !! setting properties
       !!
       subroutine file_set_property(this,filename,mode,io_unit, iostat)
         type (burp_file),     intent (inout)       :: this
         character(len =*),    intent (in)          :: filename
         character(len =*),optional,intent(in)      :: mode
         integer(kind=int_def),optional,intent (out):: iostat
         integer(kind=int_def),optional,intent (in) :: io_unit
         integer(kind=int_def)                      :: error
         call close_burp_file(this,error)
         if (error /= burp_noerr) then
            if (present(iostat))  iostat = error
            return
         end if
         if (present(io_unit)) then
             this%io_unit = io_unit
         else
             this%io_unit = get_next_io_unit()
         end if
         this%filename = filename(1:)
         if (present(mode)) this%mode = mode 
            
         call open_burp_file(this,filename = filename,       &
                  io_unit                  = this%io_unit,   &
                  mode                     = this%mode,      &
                  iostat             = error)
        if (error /= burp_noerr) then
           if (present(iostat))  iostat = error
           return
        end if
        if (present(iostat))  iostat = burp_noerr
       end subroutine file_set_property

       !!
       !! getting properties
       !!
       subroutine file_get_property(this,nrpts,filename, io_unit,iostat)
         type (burp_file),     intent (in)             :: this
         integer(kind=int_def),optional,intent (out)   :: nrpts
         integer(kind=int_def),optional,intent (out)   :: io_unit
         character (len=*),    optional,intent (out)   :: filename
         integer(kind=int_def),optional, intent (inout):: iostat
         integer(kind=int_def)                         :: error

         error = burp_noerr
         if (.not.present(nrpts) .and. .not.present(filename)) then
             if (present(iostat))  iostat = -1
             call setstatetowarning(burp_error%error,         &
             "warning >> file_get_property must be called with&
             & nrpts and/or filename arguments ")
             return
         endif

         if (present(nrpts))    nrpts    = this%nrpts
         if (present(filename)) filename = this%filename
         if (present(io_unit))  io_unit  = this%io_unit
         if (present(iostat))  iostat = error

       end subroutine file_get_property

       !!
       !! finding report
       !!
       function burp_find_report(this,report,                    &
               search_from, stnid, idtyp, lati,long, date,       &
               temps, sup, nsup,get, iostat) result(ref)
         implicit none 
         type (burp_file),     intent (inout)         :: this
         type (burp_rpt),      intent (inout)         :: report
         integer(kind=int_def),optional,intent (inout):: iostat
         logical, optional, intent (in)               :: get

         character(len=*) ,        optional,intent(in):: stnid
         integer(kind=int_def),optional,intent(in):: sup(:)
         integer(kind=int_def),optional,intent(in):: search_from,&
                                                     idtyp,      &
                                                     lati,       &
                                                     long,       &
                                                     date,       &
                                                     temps,      &
                                                     nsup

         character (len =9)               :: my_stnid
         logical                          :: get_it
         integer(kind=int_def),allocatable:: my_sup(:)
         integer(kind=int_def)            :: my_handle,  &
                                             my_idtyp,   &
                                             my_lati,    &
                                             my_long,    &
                                             my_temps,   &
                                             my_date,    &
                                             my_nsup,    &
                                             ref,        &
                                             error
         error = burp_noerr

         !! defaults
         !!
         my_handle = 0;  my_stnid = '*********'; my_idtyp = -1;my_nsup = 0
         my_lati   = -1; my_long  = -1; my_temps = -1; my_date = -1
         get_it = .true.

         if (present(get))          get_it      = get
         if (present(search_from))  my_handle   = search_from
         if (present(stnid))        my_stnid    = stnid
         if (present(idtyp))        my_idtyp    = idtyp
         if (present(lati))         my_lati     = lati
         if (present(long))         my_long     = long 
         if (present(temps))        my_temps    = temps
         if (present(date))         my_date     = date
         if (present(nsup)) then
                                    my_nsup     = nsup
                                    my_sup      = sup
         end if
         !modif hb
         allocate(my_sup(1))
         ref  = mrfloc(this%io_unit,my_handle, my_stnid, my_idtyp,   &
                  my_lati, my_long, my_date, my_temps,my_sup, my_nsup)


         ! rpt_get_report in rpt_class module
         if (ref > 0) then
            if (get_it) then
               call rpt_get_report(report,ref,this%max_len, this%io_unit,error)
            else
               call rpt_get_only_hdr(report,ref,this%max_len, this%io_unit,error)
            endif
         end if
         deallocate(my_sup)
         if (present(iostat))  iostat = error

       end function burp_find_report

       !!
       !! finding report
       !!
        subroutine burp_get_report(this,report, ref,iostat)
         implicit none 
         type (burp_file),     intent (inout)         :: this
         type (burp_rpt),      intent (inout)         :: report
         integer(kind = int_def), intent (in)         :: ref
         integer(kind=int_def),optional,intent (inout):: iostat
         integer(kind = int_def)                      :: error

         error = burp_noerr

         ! rpt_get_report in rpt_class module
         if (ref > 0) call rpt_get_report(report,ref,this%max_len,&
                            this%io_unit,error)
         if (present(iostat))  iostat = error

       end subroutine burp_get_report

       !!
       !! putting report header
       !! cette methode de burp, demande avoir acces
       !! aux champs  de rapport qui sont prives
       !! pour proteger  l'abstraction, implementer
       !! a meme la classe  rapport
       subroutine burp_init_report_write(this,report, iostat)

         type (burp_file),     intent(inout)         :: this
         type (burp_rpt),      intent(inout)         :: report
         integer(kind=int_def),optional,intent(inout):: iostat
         integer(kind=int_def)                       :: error

         error = burp_noerr
         call burp_report_mrbini(report, this%io_unit, iostat=error)
         if (present(iostat))  iostat = error

       end subroutine burp_init_report_write

       !!
       !! writing report in file
       !! cette methode de burp, demande avoir acces
       !! au champ buffer de rapport qui est prive
       !! pour proteger le l'abstraction faire impelemtion
       !! a meme la classe du rapport
       !!
       subroutine burp_write_report(this,report,update, iostat )

         type (burp_file),     intent(inout)          :: this
         type (burp_rpt),      intent(inout)          :: report
         integer(kind=int_def),optional,intent(inout) :: iostat
         logical,optional,intent(in)                  :: update
         integer(kind=int_def)                        :: error
         logical                                      :: my_update


         error = burp_noerr
         my_update = .false.

         if (present(update)) my_update = update

         if (.not.this%init) then
            if (present(iostat))  iostat = -1
            call setstatetofailure(burp_error%error,           &
            "failure >> initilise first object file with a file an mode!!&
            & example: use burp_new(file_obj, mode=file_acc_create")
            return
         endif

         if ((this%mode .eq. file_acc_create) .or. (this%mode .eq. &
              file_acc_append)) then
                 call burp_report_mrfput(report,this%io_unit, my_update,iostat=error)
         else
            if (present(iostat))  iostat = -1
            call setstatetofailure(burp_error%error,         &
           "failure >>to write to "//trim(this%filename)//" : file_acc_mode  = read!")
            return
         endif
         if (present(iostat))  iostat = error

       end subroutine burp_write_report
       !
       ! delete  report
       ! associate this delete with afile object !!!!!!
       !
       subroutine burp_delete_report(this, report, iostat)
         type (burp_file),      intent(inout)        :: this
         type (burp_rpt),      intent(inout)         :: report 
         integer(kind=int_def),optional,intent(inout):: iostat
         integer(kind=int_def)                       :: error 
         integer(kind=int_def)  :: my_file_io,&
                                   my_handle
                                                      

         error = burp_noerr

         call burp_get_property(report,iostat = error,   &
                   file_io = my_file_io,handle = my_handle) 
         if (error /= burp_noerr) then
             if (present(iostat))  iostat = error
             return
         endif

         if (this%io_unit /= my_file_io) then
            if (present(iostat))  iostat = -1
            call setstatetofailure(burp_error%error,           &
            "failure >> to delete report : report not from this file!")
            return
          endif
             
         error  = mrfdel(my_handle)
         if (error /= 0) then
            if (present(iostat))  iostat = error
            call setstatetofailure(burp_error%error,           &
            "failure >> to delete report : burp_delete_rpt-rmn_mrfdel")
            return
         endif

       end subroutine burp_delete_report

       ! setting burp options
       ! real values for missing data
       ! error level 
       !
       subroutine burp_set_options(real_optname, real_optname_value ,   &
                        char_optname,char_optname_value, iostat)
       !
       implicit none
         character(len=*),optional,intent(in)       :: real_optname
         character(len=*),optional,intent(in)       :: char_optname
         character(len=*),optional,intent(in)       :: char_optname_value
         real,            optional,intent(in)       :: real_optname_value
         integer(kind=int_def),optional,intent(out) :: iostat
         integer(kind=int_def)                      :: error

         error = burp_noerr

         ! checking if reading burp options are set
         ! real and char options
         ! var_burp_opt is persistent (global)
         !
         if (.not.var_burp_opt%init) then
             var_burp_opt%init = .true.
             ! values are from burp_constants
             var_burp_opt%real_optname       = burp_real_opt_name
             var_burp_opt%real_optname_value = burp_real_opt_value
             var_burp_opt%char_optname       = burp_char_opt_name
             var_burp_opt%char_optname_value = burp_char_opt_value
         endif

         !
         ! initialization de la pile des messages
         ! d"ereurs. l'objet global est defini dans la
         ! classe burp_block_class.f90

         if (.not.burp_error%init) then
              burp_error%init = .true.
              call initializestate(burp_error%error)
         end if


         if ((present(real_optname_value) .and.  &
            .not.present(real_optname)) .or.     &
            (.not.present(real_optname_value) .and. present(real_optname))) then
            if (present(iostat))  iostat = -1
            call setstatetofailure(burp_error%error,        &
            "failure >>  real option name or value missing!! ")
            return
         endif
         if ((present(char_optname_value) .and.  &
            .not.present(char_optname)) .or.     &
            (.not.present(char_optname_value) .and. present(char_optname))) then
            if (present(iostat))  iostat = -1
            call setstatetofailure(burp_error%error,        &
            "failure >>  char option name or value missing!! ")
            return
         endif


         ! setting real option name name and value
         !
         if (present(real_optname)) then
              error         = mrfopr ( real_optname, real_optname_value )
              if (error /= 0) then
                 if (present(iostat))  iostat = error
                 call setstatetofailure(burp_error%error,      &
                 "failure >> to init real format option ")
                 return
              else
                 if (present(iostat))  iostat = error
                 var_burp_opt%real_optname       = real_optname
                 var_burp_opt%real_optname_value = real_optname_value
                 call setstatetosuccess(burp_error%error,      &
                 "success >> setting default real format option ")
              endif
         else
            error          = mrfopr ( var_burp_opt%real_optname,&
                                     var_burp_opt%real_optname_value )
            !
            ! error handling
            if (error /= 0) then
               if (present(iostat))  iostat = error
               call setstatetofailure(burp_error%error,        &
               "failure >> burp init real format option ")
               return
            else
               if (present(iostat))  iostat = burp_noerr
               call setstatetosuccess(burp_error%error,        &
               "success >> burp init default real format option ")
            endif
         endif

         ! setting char option name name and value
         !
         if (present(char_optname)) then
              error          = mrfopc ( char_optname, char_optname_value )
              if (error /= 0) then
                 if (present(iostat))  iostat = error
                 call setstatetofailure(burp_error%error,      &
                 "failure >> to init char format option ")
                 return
              else
                 if (present(iostat))  iostat = error
                 var_burp_opt%char_optname       = char_optname
                 var_burp_opt%char_optname_value = char_optname_value
                 call setstatetosuccess(burp_error%error,      &
                 "success >> setting default char format option ")
              endif
         else
            error          = mrfopc ( var_burp_opt%char_optname,&
                                     var_burp_opt%char_optname_value )
            !
            ! error handling
            if (error /= 0) then
               if (present(iostat))  iostat = error
               call setstatetofailure(burp_error%error,        &
               "failure >> burp init char format option ")
               return
            else
               if (present(iostat))  iostat = burp_noerr
               call setstatetosuccess(burp_error%error,        &
               "success >> burp init default char format option ")
            endif
         endif

       end subroutine burp_set_options

       subroutine burp_check_file(filename,file_exist,iostat)
       !
       implicit none
         character(len=*),intent(in)                :: filename
         integer(kind=int_def),optional,intent(out) :: iostat
         integer(kind=int_def)                      :: error
         logical,intent(inout)                      :: file_exist

         error = burp_noerr
         !
         ! check existence of the file
         inquire(file=filename(1:),exist=file_exist)
         if (.not.file_exist) then
            call setstatetofailure(burp_error%error,         &
           "warning|failure >>"//filename(1:len_trim(filename))//    &
           &" no such file check the path - will be cerated if mode create or append!!!")
            return
         endif
         if (present(iostat))  iostat = error

       end subroutine

end module burp_file_class
