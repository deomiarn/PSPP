program helloomp
    use omp_lib
    implicit none

    real*8 :: sum, sum_local, sum_global

    integer*8 :: tid,i, num
    integer*8, parameter :: numthreads = 8
    integer*4 :: count, count_rate, count_max, t1,t2

    num = 1000000000
    sum_global = 0

    ! get millis
    call system_clock(t1, count_rate, count_max)

    ! -----------------------------------------------Compute in parallel
    ! -------- specifiy thread local "private" variables (i.e. not shared)
    !$omp parallel private(sum_local,tid,i) num_threads(numthreads)
    tid = omp_get_thread_num()
    print '("hello from thread = ",i2)', tid
    sum_local = 0
    do i = 1, num/numthreads
        sum_local = sum_local + 1
    end do
    ! ----------------------------------------------- critical section start (mutex)
    !$omp critical
    sum_global = sum_global + sum_local
    !$omp end critical
    ! ----------------------------------------------- critical section end

    !$omp end parallel
    ! ----------------------------------------------- Compute in parallel end

    call system_clock(t2, count_rate, count_max)

    print '("sum_global = ",f12.0," in ", i6," ms")',sum_global, t2-t1
end
