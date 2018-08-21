!
! These routines are for making direct access files available to
! Modula-2.
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
! J. Andrea, Aug.12/91- after a create, leave the file in an open state
! J. Andrea, Aug.8/91 - names changed to include the $
!                     - and this documentation added
!                     - add IMPLICIT NONEs
! J. Andrea, 1984
! This code may be freely used and distributed, it may not be sold.
!

!---------------------------------------------------------------
      subroutine For$DirClose( u, ios )

      implicit none
      integer  u, ios

c.....close a file

      close( u, iostat=ios )

      return
      end



!---------------------------------------------------------------
      subroutine For$DirCreate( file_unit,   file_name,
     &                          max_records, ios )

      implicit none
      integer       file_unit, max_records, ios 
      character*(*) file_name

c.....open a direct access file

      open(unit=FILE_UNIT, file=FILE_NAME, status='NEW',
     &     access='DIRECT', form='UNFORMATTED', maxrec=MAX_RECORDS,
     &     organization='RELATIVE', recl=128, recordtype='VARIABLE',
     &     iostat=IOS)

      return
      end



!---------------------------------------------------------------
      subroutine For$DirOpen( file_unit, file_name, ios )

      implicit none
      integer       file_unit, ios 
      character*(*) file_name

      character*80 file_access, file_direct 
      character*80 file_org, file_rec, file_form
      logical      file_exists, file_opened
      integer      file_recl

c.....open a direct access file

c....make sure the file exists, and has the proper characteristics

      inquire(file=file_name, access=file_access, 
     &        direct=file_direct, exist=file_exists,
     &        form=file_form, iostat = ios, 
     &        opened=file_opened, organization=file_org,
     &        recl=file_recl, recordtype=file_rec )

      if( file_exists )then
         if( file_direct(1:3) .eq. 'YES' )then
            if( file_org(1:8) .eq. 'RELATIVE' )then
               if( file_rec(1:8) .eq. 'VARIABLE' )then

                   open(unit=FILE_UNIT,
     &                  file=FILE_NAME, status='OLD',
     &                  access='DIRECT', form='UNFORMATTED',
     &                  organization='RELATIVE', 
     &                  recordtype='VARIABLE', iostat=IOS)
 
               else
                  ios = 4
               end if
            else
               ios = 3
            end if
         else
            ios = 2
         end if
      else
         ios = 1
      end if

      return
      end



!---------------------------------------------------------------
      subroutine For$DirRead( u, r, record, n, ios )

c.....perform a direct access read

      implicit none
      integer  u, r, n, ios, i
      byte     record(*)

      read(u,rec=r,iostat=ios)( record(i), i = 1, n )

      return
      end



!---------------------------------------------------------------
      subroutine For$DirWrite( u, r, record, n, ios )

c.....perform a direct access read

      implicit none
      integer u, r, n, ios, i
      byte    record(*)

      write(u,rec=r,iostat=ios)( record(i), i = 1, n )

      return
      end
