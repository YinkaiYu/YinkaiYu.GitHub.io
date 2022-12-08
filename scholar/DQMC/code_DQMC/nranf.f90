
    integer function nranf(iseed,N)

      implicit none
      integer :: iseed, N
      Real (Kind=8), external :: ranf

      nranf  = nint(ranf(iseed)*dble(N) + 0.5)

      if (nranf .lt. 1 ) nranf = 1
      if (nranf .gt. N ) nranf = N 

    end function nranf
      
