!
! These routines are for making FORTRAN style real number formatting
! available to Modula-2.
!
! A m2 foreign definition module exists which calls these routines
! and a higher level m2 implementation/definition module exists which
! uses these routines in a m2 like manner.
!
! These routine must be compiled and the object code added to the
! library mod$system:modula.olb to which all modula2 code is linked.
! No other special linking is required.
! Its probably best to compile these with the /NOOPTIMIZE option
!
!
! V1.0, J. Andrea, Aug.8/91
! This code may be freely used and distributed, it may not be sold.
!



!---------------------------------------------------------------
      subroutine For$FFormat( x, width, digits, out_string )

      implicit none
      real          x
      integer       width, digits
      character*(*) out_string
      integer       status

      write( out_string, 99, iostat=status ) x

      return
 99   format( F<width>.<digits> )
      end

!---------------------------------------------------------------
      subroutine For$EFormat( x, width, digits, out_string )

      implicit none
      real          x
      integer       width, digits
      character*(*) out_string
      integer       status

      write( out_string, 99, iostat=status ) x

      return
 99   format( E<width>.<digits> )
      end
