program driver

    use OAD_active
    use OAD_rev
    use kees_model
    implicit none

    ! Run workshop example
    integer, parameter              :: sz = 50
    double precision                :: dx, dt, end_time

    double precision, dimension(sz) :: x    ! horizontal coordinate
    double precision, dimension(sz) :: b    ! base elevation
    type(active), dimension(sz)     :: s, mb
    type(active)                    :: volume

    integer                         :: i, sz_

    dx = 1000.0
    dt = 0.05
    end_time = 1000.0
    sz_ = sz

    our_rev_mode%tape = .true.

    do i = 1, sz
        x(i) = (i-1)*dx
        b(i) = -2.5e-2 * x(i)
        s(i)%v = b(i)
        s(i)%d = 0.0
        mb(i)%v = 4.0 - 0.2e-3 * x(i)
    end do

    volume%d=1.0

    call integrate_model(sz_, x, b, s, mb, volume, dt, end_time)

    do i = 1, sz
        print*, mb(i)%d
    end do

end program
