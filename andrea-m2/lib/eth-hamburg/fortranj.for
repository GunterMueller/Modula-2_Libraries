! This routine is provided bacause of a problem in the Modula-2 call
! sequence for the LIB$GETJPI definition

      subroutine for$libjpi( item, value, string, length, status )

      implicit none

      integer       item
      byte          value(*)
      character*(*) string
      integer       length
      integer       status

      integer*2     returned_len

      integer  lib$getjpi
      external lib$getjpi

c.....^.............................................................72.^

      status = lib$getjpi( item,,, value, string, returned_len )

      length = returned_len

      return
      end
