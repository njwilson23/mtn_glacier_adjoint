!   Forward model for the Kees example at
!       http://websrv.cs.umt.edu/isis/index.php/Kees%27_assignment
!
program kees_forward

    implicit none

    integer,parameter       :: f64 = kind(8), i32 = kind(4)
    real(f64),parameter     :: g = 9.8, A = 1e-16, rho = 920.0
    integer(i32),parameter  :: n = 3
    real(f64),parameter     :: C = (2*A / (n+2)) * (rho*g)**n
    integer(i32)            :: ii
    real(f64)               :: volume

    ! Run workshop example
    integer(i32), parameter :: len = 50
    real(f64)               :: dx = 1000.0, dt = 0.05, &
                               time = 0.0, end_time = 1000.0

    real(f64), dimension(len)       :: mb

    type KeesModel
        real(f64),dimension(len)    :: x    ! horizontal coordinate
        real(f64),dimension(len)    :: b    ! base elevation
        real(f64),dimension(len)    :: s    ! surface elevation
    end type KeesModel

    type(KeesModel)                 :: model

    do ii = 1,len
        model%x(ii) = (ii-1)*dx
        model%b(ii) = -2.5e-2 * model%x(ii)
        model%s(ii) = model%b(ii)
        mb(ii) = 4.0 - 0.2e-3 * model%x(ii)
    end do
    !$openad INDEPENDENT(mb)

    !call writemodel("init.dat", model)

    do while (time .lt. end_time)
        call timestep_explicit(model, dt, mb)
        time = time + dt
    end do

    volume = glacier_volume(model)
    print*, "Glacier volume:",volume/1e6, "km^2"
    !$openad DEPENDENT(volume)

    !call writemodel("finl.dat", model)

    ! --------------------------------------------------------------- !

    contains

    ! Compute diffusivity for a model instance for a particular glen
    ! exponent
    subroutine diffusivity(km, n, nu)
        implicit none
        type(KeesModel), intent(in)     :: km
        integer(i32), intent(in)        :: n
        real(f64), dimension(size(km%x)-1), intent(inout)   :: nu
        integer(i32)                    :: ii

        do ii = 1, len-1
            nu(ii) = C &
                * (0.5*(km%s(ii+1)+km%s(ii)-km%b(ii+1)-km%b(ii)))**(n+2) &
                * abs((km%s(ii+1)-km%s(ii)) / (km%x(ii+1)-km%x(ii)))**(n-1)
        end do
    end subroutine

    ! Return the glacier volume (trapezoid rule)
    real(f64) function glacier_volume(model) result(sigma)

        implicit none
        type(KeesModel), intent(in)     :: model
        integer(i32)                    :: ii

        do ii = 1, size(model%x)-1
            sigma = sigma + (model%x(ii+1) - model%x(ii)) * 0.5 &
                * (model%s(ii+1) +model%s(ii) - model%b(ii+1) - model%b(ii))
        end do
    end function

    ! Perform an explicit finite difference forward step for a model
    ! instance and return a new model instance
    subroutine timestep_explicit(model, dt, mb)

        implicit none

        type(KeesModel), intent(inout)          :: model
        real(f64), intent(in)                   :: dt
        real(f64), dimension(:), intent(in)     :: mb

        integer(i32)                            :: len, ii
        real(f64), dimension(:), allocatable    :: nu
        real(f64), dimension(:), allocatable    :: flux
        real(f64), dimension(:), allocatable    :: dHdt

        len = size(model%s)
        allocate(nu(len-1), flux(len-1), dHdt(len))

        call diffusivity(model, 3, nu)
        do ii = 1, len-1
            flux(ii) = -nu(ii) * &
                (model%s(ii+1) - model%s(ii)) / (model%x(ii+1) - model%x(ii))
        end do

        dHdt(1) = -mb(1)
        dHdt(len) = -mb(len)
        do ii = 2, len-1
            dHdt(ii) = -(flux(ii) - flux(ii-1)) / (model%x(ii) - model%x(ii-1))
        end do

        model%s = max(model%s + dt*(dHdt + mb), model%b)

        deallocate(nu, flux, dHdt)

    end subroutine

    subroutine writemodel(filename, model)

        character(len=8), intent(in)    :: filename
        type(KeesModel), intent(in)     :: model
        integer(i32)                    :: ios

        open(unit=20, file=filename, form="formatted", status="old", iostat=ios)
        if (ios /= 0) then
            print*, "Error writing file"
            stop
        end if
        write(20,*) model%x
        write(20,*) model%b
        write(20,*) model%s
        close(20)

    end subroutine

end program

