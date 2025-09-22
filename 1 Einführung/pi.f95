program  montecarlo
    use omp_lib
    implicit none
    real*8 :: ran0

    integer*4 :: num, numthreads
    character*32 :: arg
    if (iargc() < 1) then
        num = 100000000
        numthreads = 2
    else
        call getarg(1, arg)
        read(arg, '(i20)') num
        if (iargc() == 2) then
            call getarg(2, arg)
            read(arg, '(i20)') numthreads
        endif
    endif
    call measurepi("calcpi", 1000, 1)
    call measurepi("calcpi", 10000, 1)
    call measurepi("calcpi", 100000, 1)
    call measurepi("calcpi", num, 1)
    call measurepi("calcpi_omp1", num, 1)
    call measurepi("calcpi_omp1", num, numthreads)
    call measurepi("calcpi_omp2", num, 1)
    call measurepi("calcpi_omp2", num, numthreads)
end

subroutine measurepi(algo, num, numthreads)
    implicit none
    real*8, parameter :: exactpi = 4.D0 * DATAN(1.D0)
    real*8 :: calcpi, calcpi_omp1, calcpi_omp2, error
    integer*4 :: count_rate, count_max, num, numthreads, t1, t2
    character(*) :: algo
    real*8 :: pi

    call system_clock(t1, count_rate, count_max)
    if (algo == "calcpi") then
        pi = calcpi(num)
    else if (algo == "calcpi_omp1") then
        pi = calcpi_omp1(num, numthreads)
    else if (algo == "calcpi_omp2") then
        pi = calcpi_omp2(num, numthreads)
    endif
    call system_clock(t2, count_rate, count_max)
    error = abs(pi - exactpi) * 100
    print '("",a11," ",i10," ",i2," thread pi = ",f12.10," error = ",f4.2,"% time =",i5," ms")',algo,num,numthreads,pi,error,t2-t1
end

function calcpi(num)
    implicit none
    real*8 :: calcpi, x, y, d
    integer*4 :: num, i, sum_global

    sum_global = 0

    do i = 1, num
        x = rand(0)
        y = rand(0)
        d = x**2 + y**2
        if (d <= 1) then
            sum_global = sum_global + 1
        end if
    end do

    calcpi = float(sum_global) / num * 4.0
end

function calcpi_omp1(num, numthreads)
    use omp_lib
    implicit none
    real*8 :: calcpi_omp1, x, y, d
    integer*4 :: num, numthreads, sum_global, i, sum_local

    sum_global = 0

    !$omp parallel private(sum_local,i) num_threads(numthreads)
    sum_local = 0

    do i = 1, num / numthreads
        x = rand(0)
        y = rand(0)
        d = x**2 + y**2
        if (d <= 1) then
            sum_local = sum_local + 1
        end if
    end do

    !$omp critical
    sum_global = sum_global + sum_local
    !$omp end critical

    !$omp end parallel
    calcpi_omp1 = float(sum_global) / num * 4.0
end

function calcpi_omp2(num, numthreads)
    use omp_lib
    implicit none
    real*8 :: calcpi_omp2, x, y, d, ran0
    integer*4 :: num, numthreads, sum_global, i, sum_local,tid

    sum_global = 0

    !$omp parallel private(sum_local,i,tid) num_threads(numthreads)
    tid = omp_get_thread_num()
    sum_local = 0

    do i = 1, num / numthreads
        x = ran0(tid)
        y = ran0(tid)
        d = x**2 + y**2
        if (d <= 1) then
            sum_local = sum_local + 1
        end if
    end do

    !$omp critical
    sum_global = sum_global + sum_local
    !$omp end critical

    !$omp end parallel
    calcpi_omp2 = float(sum_global) / num * 4.0
end

function ran0(seed)
    integer*4 seed,ia,im,iq,ir,mask,k
    real*8 ran0,am
    parameter (ia=16807,im=2147483647,am=1./im, iq=127773,ir=2836,mask=123459876)
    seed=ieor(seed,mask)
    k=seed/iq
    seed=ia*(seed-k*iq)-ir*k
    if (seed.lt.0) seed=seed+im
    ran0=am*seed
    seed=ieor(seed,mask)
    return
end
