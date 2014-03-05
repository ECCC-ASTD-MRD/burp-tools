program test_init
        use object_initialization
        implicit none
        type point
                integer :: x,y
                type(T_INIT)::INIT
        end type
        type cercle
                integer :: r
                type(T_INIT)::INIT
        end type


        type (point) :: a, b
        type (cercle) :: r
        logical TEST
        a%x=1 
        a%y=2 
        r%r = 34


!        write(*,*) 'a egal = ',a
!        write(*,*) 'a INIT = ',a%init
!        write(*,*) 'a init id = ',GET_ID(a%init)
        test = IS_INIT(a%init)
        write(*,*) 'a is_init = ',test
        call INITIALIZE(a%init)
        test = IS_INIT(a%init)
        write(*,*) 'a is_init = ',test
        call INITIALIZE(a%init)
        test = IS_INIT(a%init)
        write(*,*) 'a is_init = ',test

end program
