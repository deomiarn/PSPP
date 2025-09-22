  program ConvertF2C
  implicit none
  ! ---------------------------------------------- Declare
  real*8 :: tempC, tempF, FACTOR
  character*32 :: arg
  integer*4 :: ZERO_SHIFT

  parameter (ZERO_SHIFT = 32, FACTOR = 5./9.)
  ! -----------------------------------------------Input
  if (iargc() < 1) then
    print*, "Enter in Fahrenheit ..."
    read*, tempF
  else
    !read command line argument
    call getarg(1, arg)
    !convert to number
    read (arg,*) tempF
  endif

  ! ----------------------------------------------- Compute
  tempC = FACTOR * (tempF - ZERO_SHIFT)
  ! ----------------------------------------------- Output
  write (*,9000) tempF,tempC
  ! ----------------------------------------------- Format
  9000 format(1x,f6.2,' in Fahrenheit is ', f6.2,' in Celsius',//)
  end 
