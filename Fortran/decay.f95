     program Decay
!
! This program calculates radioactive decay.
! It calculates the initial amount of radioactive material given a half life (HL), an current amount (N), and a time (T).
!     
     
     real hl, n, t, ln, lambda, no

     write (*,*) 'Input: HL, N, T'
     read (*,*) hl, n, t
     write (*,*) 'Entered:', hl, '', n, '', t

!
! First calculate ln(2) to get lambda
! ln(2) = log e (2)
!
     ln = log(2.0)
     lambda = ln / HL

!
!	calculate current amount
!
     no = n*exp(lambda*t)

     write (*,*) 'Output:', no, ' grams'
     end