program calcpi
    implicit none

    real :: t1, x, y, d, pi, t2
    integer, parameter :: num = 100000000
    integer :: i, sum_global

    t1 = time() * 1000.0
    print *, "Start time (ms): ", t1
    sum_global = 0

    do i = 1, num
        x = rand(0)
        y = rand(0)
        d = x**2 + y**2

        if (d <= 1) then
            sum_global = sum_global + 1
        end if
    end do

    pi = float(sum_global) / num * 4.0
    t2 = time() * 1000.0

    print *, "End time (ms): ", t2

    print *, "pi = ", pi, " time = ", t2 - t1, " ms"
end program calcpi
