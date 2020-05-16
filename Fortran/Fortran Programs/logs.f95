      program logs
      implicit none

!
! Christopher Oehler | 1/26/19 | coehler@uoguelph.ca
! 
! This program calculates the volume of saw logs based on the International Rule
! It is partly based on a USDA, Forest Service research note
! 

! Main part of the program is a loop allowing for continuous entry

      

      integer :: com, kerf
      logical :: exec
      real :: ds, dl, tl, v

      write (*,*) 'Calculate the volume of a saw log based on the international rule.'
      write (*,*) 'Original code by J. E. Brickell | 1973 | USDA, Forest Service.'
      write (*,*) 'Upgrade by Christopher Oehler | 1/26/19 | University of Guelph.'
      write (*,*) ''

      exec = .TRUE.

      do while (exec)

        write (*,*) 'Select command by entering a number.'
        write (*,*) '1. Calculate log volume.'
        write (*,*) '2. Exit program.'

        read (*,*) com

        if (com == 1) then

          v = 0.0
          call getLOGdata(ds, dl, tl, kerf)
          call calcLOGjclark(ds, dl, tl, kerf, v)

          write (*,*) 'The log contains', v, 'broad foot volumes of lumber.'

          call calcLOGvolume(ds, dl, tl, v)

          write (*,*) 'The log contains', v, 'cubic metres of lumber.'

        elseif (com == 2) then

          exec = .FALSE.

        else

          write (*,*) 'Error. Invalid Input.'

        end if

      end do




      end program logs
!
! Reads in log data required by calcLOGjclark()
!
      subroutine getLOGdata(ds, dl, tl, kerf)

      implicit none

      real, intent(out) :: ds, dl, tl
      integer, intent(out) :: kerf

      write (*,*) 'Input: DIBSmall, DIBLarge, Total Length, Kerf'
      read (*,*) ds, dl, tl, kerf

      end subroutine getLOGdata
!
! Calculate the volume of a log in cubic metres, using data obtained by getLOGdata()
!
      subroutine calcLOGvolume(ds, dl, tl, v)

      implicit none

      real, intent(in) :: ds, tl
      real, intent(inout) :: dl
      real :: mds, mdl, mtl, pi, a1, a2
      real, intent(out) :: v

      pi = 3.14159265359

! Smalian's Formula will be used to calculate the volume of a log in m3. V = ((A1 + A2) / 2)L
! This calculation requires A1, A2, and L to be in metric. They must be converted.

      if(dl == 0.0) then

        dl = ds + (5.0 * 0.5)

      end if

      mds = (ds / 39.37) / 2
      mdl = (dl / 39.37) / 2
      mtl = tl / 3.2808

      a1 = pi * mds * mds
      a2 = pi * mdl * mdl

      v = ((a1 + a2) / 2) * mtl

      end subroutine calcLOGvolume
!
! Calculate the volume of a log in board feet obtained by getLOGdata()
! Modernized version of algorithm written by USDA, Forest Service
!
      subroutine calcLOGjclark(ds, dl, tl, kerf, v)

      implicit none

! This subroutine was written by j.e.brickell of the u.s.forest service to calculate board foot volume of sawlogs by the international rule.
!
! Variables in the calling sequence are:
!
! ds = log’s scaling diameter (inches)
! dl = dib at log’s large end (inches) (0.0 if 1/2 inch taper)
! tl = total log length (feet)
! kerf >0 if kerf assumption is 1/4 inch
! kerf <0, or = 0, if kerf assumption is 1/8 inch
! v = log volume returned to the calling program

      real, intent(in) :: ds, dl, tl
      real, intent(out) :: v
      real :: d, dc, dex, sl, t, vadd, xi, xl
      integer :: i, j, k, l, kerf

      v = 0.0

! If total log length is less than four feet no board foot volume will be computed.

      if (tl >= 4.0) then

        ! If the log’s large end diameter is furnished to jclark a taper rate will be computed.
        ! If dl = 0 the standard assumption of 1/2 inch per 4 feet of log length will be used.

        if(dl <= 0.0) then

          t = 0.5

        else

          t = 4.0 * (dl - ds) / tl 

        end if

      else

        ! The log is less than four feet. Do nothing and return 0.
        return

      end if  

! The following loop finds out how many full 4 foot segments the log contains.
      
      do i = 1, 20

        if ((tl - float(4 * i)) < 0) then 

          exit

        end if

      end do

      l = i - 1
      sl = float(4 * l)

! the following statement moves the scaling diameter down to the end of the 4 foot segments and increases it according to taper.

      d = ds + (t / 4.0) * (tl - sl)

! The following loop (through statement 7) finds out how many full feet of length are in the segment less than 4 feet long.
      
      do j = 1, 4

        xi = float(j)

        if ((sl - tl + xi) > 0) then

          exit

        end if

      end do

! The next three statements calculate log volume in the 1, 2, or 3 foot segment at the small end of the log.

      xl = xi - 1.0
      dex = ds + (t / 4.0) * (tl - sl - xl)
      vadd = 0.055 * xl * dex * dex - 0.1775 * xl * dex

! The following loop (through 9) calculates volume in the portion of the log containing whole 4 foot segments.
      
      do k = 1, l

        dc = d + t * float(k - 1)
        v = v + 0.22 * dc * dc - 0.71 * dc

      end do
      
      v = v + vadd

! If ‘kerf’ is greater than zero, international 1/8 inch volume as computed above will be converted to international 1/4 inch volume.

      if (kerf > 0) then

        v = 0.905 * v

      end if

      return

      end subroutine calcLOGjclark