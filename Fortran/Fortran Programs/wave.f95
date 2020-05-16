     program Wave
     implicit none
!
! This program calculates the breaker type of a wave.
!     
     
     real :: t, h, m, b, num, den
     real :: g = 981
     character(len=32):: type = '.'

     write (*,*) 'Input: M, H, T'
     read (*,*) m, h, t
     write (*,*) 'Entered:', m, '', h, '', t

!
! First calculate the denomenator.
!

     den = g * m * (t ** 2)

!
! Next calculate the numerator.
!
     b = h/den

     if (b < 0.003) then
          type = 'surging'
     else if (b > 0.068) then
          type = 'spilling'
     else
          type = 'plunging'
     end if

     write(*,*) 'Output ', type, 'at b =', b

     end