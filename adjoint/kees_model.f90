!   Forward model for the Kees example at
!       http://websrv.cs.umt.edu/isis/index.php/Kees%27_assignment
!

module kees_model

    double precision, parameter :: g = 9.8, A = 1e-16, rho = 920.0
    integer, parameter          :: n = 3
    double precision, parameter :: C = (2*A / (n+2)) * (rho*g)**n

contains

    ! WARNING: modifies s IN-PLACE
    subroutine integrate_model(x, b, s, mb, volume, dt, end_time)

        implicit none

        double precision, dimension(:), intent(inout)   :: mb
        double precision, dimension(:), intent(inout)   :: x    ! horizontal coordinate
        double precision, dimension(:), intent(inout)   :: b    ! base elevation
        double precision, dimension(:), intent(inout)   :: s    ! surface elevation
        double precision, intent(inout)                 :: volume
        double precision, intent(inout)                 :: dt, end_time
        double precision                    :: time
        integer                             :: i

        !$openad INDEPENDENT(mb)

        time = 0.0

        do while (time .lt. end_time)
            call timestep_explicit(x, b, s, dt, mb)
        end do

        volume = glacier_volume(x, b, s)

        !$openad DEPENDENT(volume)

    end subroutine

    ! Compute diffusivity for a model instance for a particular glen
    ! exponent
    subroutine diffusivity(x, b, s, n, nu)
        implicit none
        double precision, dimension(:), intent(inout)   :: x, b, s
        integer, intent(inout)                          :: n
        double precision, dimension(size(x)-1), intent(inout)   :: nu
        integer                                         :: i

        do i = 1, size(nu)
            nu(i) = C * (0.5*(s(i+1)+s(i)-b(i+1)-b(i)))**(n+2) &
                       * abs((s(i+1)-s(i)) / (x(i+1)-x(i)))**(n-1)
        end do
    end subroutine

    ! Return the glacier volume (trapezoid rule)
    double precision function glacier_volume(x, b, s) result(sigma)

        implicit none
        double precision, dimension(:), intent(inout)   :: x, b, s
        integer                                         :: i

        do i = 1, size(x)-1
            sigma = sigma + &
                (x(i+1) - x(i)) * 0.5 * (s(i+1) +s(i) - b(i+1) - b(i))
        end do
    end function

    ! Perform an explicit finite difference forward step for a model
    ! instance and return a new model instance
    subroutine timestep_explicit(x, b, s, dt, mb)

        implicit none

        double precision, dimension(:), intent(inout)   :: x, b, mb
        double precision, dimension(:), intent(inout)   :: s
        double precision, intent(inout)                 :: dt

        integer                                         :: len, i, n
        double precision, dimension(:), allocatable     :: nu
        double precision, dimension(:), allocatable     :: flux
        double precision, dimension(:), allocatable     :: dHdt

        n = 3
        len = size(s)
        allocate(nu(len-1), flux(len-1), dHdt(len))

        call diffusivity(x, b, s, n, nu)
        do i = 1, len-1
            flux(i) = -nu(i) * &
                (s(i+1) - s(i)) / (x(i+1) - x(i))
        end do

        dHdt(1) = -mb(1)
        dHdt(len) = -mb(len)
        do i = 2, len-1
            dHdt(i) = -(flux(i) - flux(i-1)) / (x(i) - x(i-1))
        end do

        ! Apply mass balance
        do i = 1, len
            if (s(i) + dt*(dHdt(i)+mb(i)) .lt. b(i)) then
                s(i) = b(i)
            else
                s(i) = s(i) + dt*(dHdt(i)+mb(i))
            end if
        end do

        deallocate(nu, flux, dHdt)

    end subroutine

    subroutine writemodel(filename, x, b, s)

        character(len=8), intent(inout)                 :: filename
        double precision, dimension(:), intent(inout)   :: x, b, s
        integer                                         :: ios

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
