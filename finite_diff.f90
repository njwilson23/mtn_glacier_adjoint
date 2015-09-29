program finite_differences

    use kees_model
    implicit none

    integer, parameter :: sz = 50
    double precision, parameter     :: dx = 1000.0
    double precision                :: dt, end_time

    double precision, dimension(sz) :: mb
    double precision, dimension(sz) :: x    ! horizontal coordinate
    double precision, dimension(sz) :: b    ! base elevation
    double precision, dimension(sz) :: s    ! surface elevation
    double precision                :: volume, vol0

    double precision, parameter     :: dmb = 1e-7
    integer            :: i, j

    dt = 0.05
    end_time = 1000.0

    do j = 0,sz

        do i = 1,sz
            x(i) = (i-1)*dx
            b(i) = -2.5e-2 * x(i)
            s(i) = b(i)
            mb(i) = 4.0 - 0.2e-3 * x(i)
        end do
        volume = 0.0

        if (j.ne.0) then
            mb(j) = mb(j) + dmb
        end if

        call integrate_model(sz, x, b, s, mb, volume, dt, end_time)

        if (j.eq.0) then
            vol0 = volume
        else
            print*, (volume-vol0) / dmb
        end if

    end do

end program
