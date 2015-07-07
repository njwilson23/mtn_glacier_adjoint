! Run workshop example

program fwd_model

    use kees_model
    implicit none

    integer(i32), parameter :: len = 50
    real(f64), parameter    :: dx = 1000.0, dt = 0.05, end_time = 1000.0

    real(f64), dimension(len)   :: mb
    real(f64), dimension(len)   :: x    ! horizontal coordinate
    real(f64), dimension(len)   :: b    ! base elevation
    real(f64), dimension(len)   :: s    ! surface elevation
    real(f64)                   :: volume

    integer(i32)            :: i

    do i = 1,len
        x(i) = (i-1)*dx
        b(i) = -2.5e-2 * x(i)
        s(i) = b(i)
        mb(i) = 4.0 - 0.2e-3 * x(i)
    end do

    !call writemodel("init.dat", x, b, s)
    call integrate_model(x, b, s, mb, volume, dt, end_time)
    !call writemodel("finl.dat", x, b, s)

    print*, "Glacier volume:",volume/1e6, "km^2"

end program

