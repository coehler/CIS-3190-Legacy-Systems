     program Circle
!
! This program reads a real number r and prints
! the area of a circle with radius r.
!
     real r, area
     write (*,*) 'radius r: '
     read (*,*) r
     area = 3.14159*r*r
     write (*,*) 'Area = ', area

     end