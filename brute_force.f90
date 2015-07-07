program brute_force

    use kees_model
    implicit none

    integer(i32), parameter :: len = 50
    real(f64), parameter    :: dx = 1000.0, dt = 0.05, end_time = 1000.0

    real(f64), dimension(len)   :: mb
    real(f64), dimension(len)   :: x    ! horizontal coordinate
    real(f64), dimension(len)   :: b    ! base elevation
    real(f64), dimension(len)   :: s    ! surface elevation
    real(f64), dimension(2*len) :: volume
    real(f64), dimension(len)   :: gradient

    real(f64), parameter        :: delta_mb = 0.1
    integer(i32)                :: i, j, perturbed_idx

    do i = 1,len
        x(i) = (i-1)*dx
        b(i) = -2.5e-2 * x(i)
        s(i) = b(i)
        mb(i) = 4.0 - 0.2e-3 * x(i)
    end do

    do j = 1, 2*len

        if (mod(j, 2) .eq. 1) then      ! odd
            perturbed_idx = (j-1)/2 + 1
            mb(perturbed_idx) = mb(perturbed_idx) - delta_mb
        else                            ! even
            perturbed_idx = j/2
            mb(perturbed_idx) = mb(perturbed_idx) + delta_mb
        endif

        print*,"filling ",j,"by modifying", perturbed_idx
        call integrate_model(x, b, s, mb, volume(j), dt, end_time)

        if (mod(j, 2) .eq. 1) then      ! odd
            mb(perturbed_idx) = mb(perturbed_idx) + delta_mb
        else                            ! even
            mb(perturbed_idx) = mb(perturbed_idx) - delta_mb
        endif

    end do

    do i = 1,len
        gradient(i) = (volume(2*(i-1)+2) - volume(2*(i-1)+1)) / (2*delta_mb)
        print*, i, gradient(i)
    end do

end program
