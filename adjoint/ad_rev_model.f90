program driver

    use OAD_active
    use OAD_rev
    use kees_model
    implicit none

    ! Run workshop example
    integer, parameter              :: len = 50
    double precision, parameter     :: dx = 1000.0, dt = 0.05, end_time = 1000.0

    double precision, dimension(len):: x    ! horizontal coordinate
    double precision, dimension(len):: b    ! base elevation
    type(active), dimension(len)    :: s, mb
    type(active)                    :: volume

    integer                         :: i

    do i = 1, len
        x(i) = (i-1)*dx
        b(i) = -2.5e-2 * x(i)
        s(i)%v = b(i)
        s(i)%d = 0.0
        mb(i)%v = 4.0 - 0.2e-3 * x(i)
        mb(i)%d = 0.0
    end do

    volume%d=1.0

    !our_rev_mode%tape = .true.
    !our_rev_mode%store = .false.
    !call tape_init()

    ! Record the forward model
    !call OAD_revTape()
    call integrate_model(x, b, s, mb, volume, dt, end_time)

    ! Back out the adjoint (why do I call the same model function as above?)
    !call OAD_revAdjoint()
    !call integrate_model(x, b, s, mb, volume, dt, end_time)

    do i = 1, len
        print*, mb
    end do

end program
