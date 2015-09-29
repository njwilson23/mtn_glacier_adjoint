!   Forward model for the Kees example at
!       http://websrv.cs.umt.edu/isis/index.php/Kees%27_assignment
!

module kees_model

    double precision, parameter :: g = 9.8, A = 1e-16, rho = 920.0
    integer, parameter          :: n = 3
    double precision, parameter :: C = (2*A / (n+2)) * (rho*g)**n

contains

    ! WARNING: modifies s IN-PLACE
    subroutine integrate_model(sz, x, b, s, mb, volume, dt, end_time)

        implicit none

        integer                                         :: sz
        double precision, dimension(sz), intent(inout)  :: mb
        double precision, dimension(sz), intent(inout)  :: x    ! horizontal coordinate
        double precision, dimension(sz), intent(inout)  :: b    ! base elevation
        double precision, dimension(sz), intent(inout)  :: s    ! surface elevation
        double precision, intent(inout)                 :: volume
        double precision, intent(inout)                 :: dt, end_time

        double precision                    :: time
        integer                             :: i

        !$openad INDEPENDENT(mb)

        time = 0.0
        do while (time .lt. end_time)
            call timestep_explicit(sz, x, b, s, dt, mb)
            time = time + dt
        end do

        volume = glacier_volume(sz, x, b, s)

        !$openad DEPENDENT(volume)

    end subroutine

    ! Compute diffusivity for a model instance for a particular glen
    ! exponent
    subroutine diffusivity(sz, x, b, s, n, nu)
        implicit none
        integer, intent(inout)                          :: sz
        double precision, dimension(sz), intent(inout)  :: x, b, s
        integer, intent(inout)                          :: n
        double precision, dimension(sz-1), intent(inout):: nu
        integer                                         :: i

        do i = 1, size(nu)
            nu(i) = C * (0.5*(s(i+1)+s(i)-b(i+1)-b(i)))**(n+2) &
                       * abs((s(i+1)-s(i)) / (x(i+1)-x(i)))**(n-1)
        end do
    end subroutine

    ! Return the glacier volume (trapezoid rule)
    double precision function glacier_volume(sz, x, b, s) result(sigma)

        implicit none
        integer, intent(inout)                          :: sz
        double precision, dimension(sz), intent(inout)  :: x, b, s
        integer                                         :: i

        sigma = 0.0
        do i = 1, sz-1
            sigma = sigma + (x(i+1) - x(i)) * 0.5 * (s(i+1) +s(i) - b(i+1) - b(i))
        end do
    end function

    ! Perform an explicit finite difference forward step for a model
    ! instance and return a new model instance
    subroutine timestep_explicit(sz, x, b, s, dt, mb)

        implicit none

        integer, intent(inout)                          :: sz
        double precision, dimension(sz), intent(inout)  :: x, b, mb
        double precision, dimension(sz), intent(inout)  :: s
        double precision, intent(inout)                 :: dt

        integer                                         :: i, n
        double precision, dimension(sz-1)               :: nu, flux
        double precision, dimension(sz)                 :: dHdt

        n = 3
        call diffusivity(sz, x, b, s, n, nu)
        do i = 1, sz-1
            flux(i) = -nu(i) * (s(i+1) - s(i)) / (x(i+1) - x(i))
        end do

        dHdt(1) = -mb(1)
        dHdt(sz) = -mb(sz)
        do i = 2, sz-1
            dHdt(i) = -(flux(i) - flux(i-1)) / (x(i) - x(i-1))
        end do

        ! Apply mass balance
        do i = 1, sz
            s(i) = s(i) + dt*(dHdt(i)+mb(i))
            if (s(i) .lt. b(i)) then
                s(i) = b(i)
            endif
        end do

    end subroutine

    subroutine writemodel(filename, sz, x, b, s)

        character(len=8), intent(inout)                 :: filename
        integer                                         :: sz
        double precision, dimension(sz), intent(inout)  :: x, b, s
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
