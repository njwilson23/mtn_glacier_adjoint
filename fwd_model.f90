! Run workshop example

program fwd_model

    use kees_model
    implicit none

    integer, parameter :: sz = 50
    double precision, parameter     :: dx = 1000.0
    double precision                :: dt, end_time

    double precision, dimension(sz) :: mb
    double precision, dimension(sz) :: x    ! horizontal coordinate
    double precision, dimension(sz) :: b    ! base elevation
    double precision, dimension(sz) :: s    ! surface elevation
    double precision                :: volume

    integer            :: i

    dt = 0.05
    end_time = 1000.0

    do i = 1,sz
        x(i) = (i-1)*dx
        b(i) = -2.5e-2 * x(i)
        s(i) = b(i)
        mb(i) = 4.0 - 0.2e-3 * x(i)
    end do

    !call writemodel("init.dat", x, b, s)
    call integrate_model(sz, x, b, s, mb, volume, dt, end_time)
    !call writemodel("finl.dat", x, b, s)

    print*, "Glacier volume:",volume/1e6, "km^2"
    print*, "       (should be 14.3877459     km^2)"

end program

