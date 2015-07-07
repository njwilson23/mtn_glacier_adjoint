!   Forward model for the Kees example at
!       http://websrv.cs.umt.edu/isis/index.php/Kees%27_assignment
!

module kees_model

    integer,parameter       :: f64 = kind(8), i32 = kind(4)
    real(f64), parameter    :: g = 9.8, A = 1e-16, rho = 920.0
    integer(i32), parameter :: n = 3
    real(f64), parameter    :: C = (2*A / (n+2)) * (rho*g)**n

contains

    ! WARNING: modifies s IN-PLACE
    subroutine integrate_model(x, b, s, mb, volume, dt, end_time)

        implicit none

        real(f64), dimension(:), intent(in)     :: mb
        real(f64), dimension(:), intent(in)     :: x    ! horizontal coordinate
        real(f64), dimension(:), intent(in)     :: b    ! base elevation
        real(f64), dimension(:), intent(inout)  :: s    ! surface elevation
        real(f64), intent(inout)                :: volume
        real(f64), intent(in)   :: dt, end_time
        real(f64)               :: time

        !$openad INDEPENDENT(mb)

        time = 0.0

        do while (time .lt. end_time)
            call timestep_explicit(x, b, s, dt, mb)
            time = time + dt
        end do

        volume = glacier_volume(x, b, s)

        !$openad DEPENDENT(volume)

    end subroutine

    ! Compute diffusivity for a model instance for a particular glen
    ! exponent
    subroutine diffusivity(x, b, s, n, nu)
        implicit none
        real(f64), dimension(:), intent(in) :: x, b, s
        integer(i32), intent(in)        :: n
        real(f64), dimension(size(x)-1), intent(inout)   :: nu
        integer(i32)                    :: i

        do i = 1, size(nu)
            nu(i) = C * (0.5*(s(i+1)+s(i)-b(i+1)-b(i)))**(n+2) &
                       * abs((s(i+1)-s(i)) / (x(i+1)-x(i)))**(n-1)
        end do
    end subroutine

    ! Return the glacier volume (trapezoid rule)
    real(f64) function glacier_volume(x, b, s) result(sigma)

        implicit none
        real(f64), dimension(:), intent(in) :: x, b, s
        integer(i32)                        :: i

        do i = 1, size(x)-1
            sigma = sigma + &
                (x(i+1) - x(i)) * 0.5 * (s(i+1) +s(i) - b(i+1) - b(i))
        end do
    end function

    ! Perform an explicit finite difference forward step for a model
    ! instance and return a new model instance
    subroutine timestep_explicit(x, b, s, dt, mb)

        implicit none

        real(f64), dimension(:), intent(in)     :: x, b, mb
        real(f64), dimension(:), intent(inout)  :: s
        real(f64), intent(in)                   :: dt

        integer(i32)                            :: len, i
        real(f64), dimension(:), allocatable    :: nu
        real(f64), dimension(:), allocatable    :: flux
        real(f64), dimension(:), allocatable    :: dHdt

        len = size(s)
        allocate(nu(len-1), flux(len-1), dHdt(len))

        call diffusivity(x, b, s, 3, nu)
        do i = 1, len-1
            flux(i) = -nu(i) * &
                (s(i+1) - s(i)) / (x(i+1) - x(i))
        end do

        dHdt(1) = -mb(1)
        dHdt(len) = -mb(len)
        do i = 2, len-1
            dHdt(i) = -(flux(i) - flux(i-1)) / (x(i) - x(i-1))
        end do

        s = max(s + dt*(dHdt + mb), b)

        deallocate(nu, flux, dHdt)

    end subroutine

    subroutine writemodel(filename, x, b, s)

        character(len=8), intent(in)    :: filename
        real(f64), dimension(:), intent(in) :: x, b, s
        integer(i32)    :: ios

        open(unit=20, file=filename, form="formatted", status="old", iostat=ios)
        if (ios /= 0) then
            print*, "Error writing file"
            stop
        end if
        write(20,*) x
        write(20,*) b
        write(20,*) s
        close(20)

    end subroutine

end module
