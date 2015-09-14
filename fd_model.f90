program brute_force

    use kees_model
    implicit none

    integer(i32), parameter :: len = 50
    real(f64), parameter    :: dx = 1000.0, dt = 0.05, end_time = 1000.0

    real(f64), dimension(len)   :: mb
    real(f64), dimension(len)   :: x    ! horizontal coordinate
    real(f64), dimension(len)   :: b    ! base elevation
    real(f64), dimension(len)   :: s    ! surface elevation
    real(f64), dimension(len)   :: volume, gradient
    real(f64)                   :: reference_volume

    real(f64), parameter        :: delta_mb = 1e-2
    integer(i32)                :: i, j

    do i = 1,len
        x(i) = (i-1)*dx
        b(i) = -2.5e-2 * x(i)
        s(i) = b(i)
        mb(i) = 4.0 - 0.2e-3 * x(i)
    end do

    call integrate_model(x, b, s, mb, reference_volume, dt, end_time)

    do i = 1, len

        do j = 1,len
            s(j) = b(j)
        end do

        mb(i) = mb(i) + delta_mb
        call integrate_model(x, b, s, mb, volume(i), dt, end_time)
        mb(i) = mb(i) - delta_mb

    end do

    open(unit=20, file="grad_fd.dat", form="formatted", status="replace")
    do i = 1,len
        gradient(i) = (volume(i) - reference_volume) / delta_mb
        print*, i, gradient(i)
        write(20,*) i, gradient(i)
    end do
    close(20)

end program
