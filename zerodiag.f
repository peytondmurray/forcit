*********0*********0*********0********0*********0*********0*********0**
* Zerodiag.f: Creates a polygon that can be used with the GMT program
* grdmask to produce a grid with zeros along the diagaonal and ones
* everywhere else. For use in filtering the reversible ridge in
* FORC diagrams.
* Example:
* zerodiag -s 10 > zero.xy
* grdmask zero.xy -Gzero.grd -I10 -R-1000/1000/-1000/1000 -H1 -N1/0/0 
*
* Written by Gary Acton April 2004
*********0*********0*********0********0*********0*********0*********0**
      character*1 tab
      character*20 arg
      character*20 dum
      tab='\t' 

*********0*********0*********0********0*********0*********0*********0**
* Get 
      iarg=1
      ia=2
      narg=iargc()
      if(narg.ne.0)then
        call getarg(iarg,arg)
        if(arg.eq.'-h')then
  1       write(*,*)'Usage: zerodiag [-s] [N] ',
     &    'where N is the integer grid spacing.'
          write(*,*)' '
          write(*,*)'Zerodiag creates a polygon that can be used ',
     &    'with the GMT program grdmask to produce a grid with zeros'
          write(*,*)'along the diagaonal and ones everywhere else. ',
     &    'For use in filtering the reversible ridge in'
          write(*,*)'FORC diagrams.' 
          write(*,*)' '
          write(*,*)'Example of how to run zerodiag:'
          write(*,*)'zerodiag -s 10 > zero.xy'
          write(*,*)'To get the grid file, use grdmask, e.g., '
          write(*,*)'grdmask zero.xy -Gzero.grd -I10 ',
     &    '-R-1000/1000/-1000/1000 -H1 -N1/0/0'
          write(*,*)' '
          write(*,*)' Options:'
          write(*,*)'   -h  gives these help pages '
          write(*,*)'   -s  followed by an integer'
          write(*,*)'     i.e., '
          write(*,*)'   -s 10'
          write(*,*)' Defaults:'
          write(*,*)'   NONE other than that the diagonal ',
     &    'grid will not extend beyond 2000 mT.'
          stop
        elseif(arg .eq. '-s') then
          call getarg(ia,dum)
          read(dum,*) dx
          idx=nint(dx)
          if(idx.lt.1) idx=1
        endif
      endif
*********0*********0*********0*********0*********0*********0*********0**

      ixmax=2000
      iymax=2000
      ixmin=-2000
      iymin=-2000
      write(6,'(a30)') "> zero.grd along the diagonal."
      write(6,'(i6,1x,i6)') ix,iy
      do 30 i=1,40
       ix=ixmax-idx-(100*(i-1)) 
       iy=iymax-(100*(i-1))
       write(6,'(i6,1x,i6)') ix,iy
 30   continue
      write(6,'(i6,1x,i6)') ix,iy
      do 40 i=1,40
       ix=ixmin+idx+(100*(i-1)) 
       iy=iymin+(100*(i-1))
       write(6,'(i6,1x,i6)') ix,iy
 40   continue
      write(6,'(i6,1x,i6)') ixmax,iymax
      stop 
      end
