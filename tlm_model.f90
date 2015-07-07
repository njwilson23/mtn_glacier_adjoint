program driver

    use OAD_active
    use kees_model
    implicit none

    ! Run workshop example
    integer(i32), parameter :: len = 50
    real(f64), parameter    :: dx = 1000.0, dt = 0.05, end_time = 1000.0

    real(f64), dimension(len)   :: x    ! horizontal coordinate
    real(f64), dimension(len)   :: b    ! base elevation
    !real(f64), dimension(len)   :: s    ! surface elevation
    type(active), dimension(len):: s, mb
    type(active)                :: volume

    integer(i32)            :: i

    do i = 1, len
        x(i) = (i-1)*dx
        b(i) = -2.5e-2 * x(i)
        s(i)%v = b(i)
        s(i)%d = 1.0
        mb(i)%v = 4.0 - 0.2e-3 * x(i)
        mb(i)%d = 1.0
    end do

    call integrate_model(x, b, s, mb, volume, dt, end_time)
    print*, "Glacier volume:", volume%v/1e6, "km^2"
    print*, "Glacier volume derivative (mass balance)", volume%d/1e6, "km^2 per m/yr"

end program
