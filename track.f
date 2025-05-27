*********0*********0*********0*********0*********0*********0*********0**
* Track.f: Creates a 1d track along which data from a FORC 
* distribution, which resides in a GMT grd file, can be extracted
* using grdtrac
* Example:
* track -s 0 0 1200 0 10 > zero.xy
* grdtrack track.out -Grhodxdyrn.grd  > track.xy
*
* Written by Gary Acton April 2004
*********0*********0*********0*********0*********0*********0*********0**
      character*1 tab
      character*20 arg
      character*10 dum
      tab='\t' 

*********0*********0*********0*********0*********0*********0*********0**
      open(unit=12,file="track.out",status='UNKNOWN')
* Get 
      iarg=1
      ia=2
      narg=iargc()
      if(narg.ne.0)then
        call getarg(iarg,arg)
        if(arg.eq.'-h')then
  1       write(*,*)'Usage: track [-s] [x1 y1 x2 y2 dd] ',
     &    'where x1, y1 and x2, y2 are the end points of the track',
     &    'and dd is the interval spacing.'
          write(*,*)' '
          write(*,*)'Track creates a sequence of x,y points evenly ',
     &    'along a track line. This file can then be used with the ',
     &    'GMT program grdtrack, which will then extract data from ',
     &    'the grid file along the track.'
          write(*,*)' '
          write(*,*)'Example of how to run zerodiag:'
          write(*,*)'track -s 0.0 0.0 300.0 0.0 0.5 > track.out'
          write(*,*)'To get the data from the grid file, '
          write(*,*)'use grdtrack, e.g., '
          write(*,*)'grdtrack track.out -Grhodxdyrn.grd > out ',
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
          read(dum,*) x1
          call getarg(3,dum)
          read(dum,*) y1
          call getarg(4,dum)
          read(dum,*) x2
          call getarg(5,dum)
          read(dum,*) y2
          call getarg(6,dum)
          read(dum,*) dd
        endif
      endif
*********0*********0*********0*********0*********0*********0*********0**
      write(12,'(a20,f8.2,f8.2,a8,f8.2,f8.2,a4,f9.3)')
     &  "> Track line x1,y1 =", x1,y1," x2,y2 =",x2,y2," dd=",dd
* the slope is used only as a test for direction of the track
      if(abs(x2-x1) .lt. 1E-08) then
        slope = (y2-y1)/0.0001
      else
        slope = (y2-y1)/(x2-x1)
      endif
* check for case where track is along the x-axis but endpoints decrease
      if(abs(y2-y1) .lt. 1E-08) then
        if(x2 .lt. x1) slope = -1000000.0
      endif
      if(slope .lt. 0.0) dd = -dd
      dist=0.0
*********0*********0*********0*********0*********0*********0*********0**
      if(abs(x2-x1) .lt. 1E-08) then
        x=x1
        y=y1
        write(12,*) x,y,dist
  10    y=y+dd
        dist=dist+abs(dd) 
        if(slope .ge. 0.0) then
          if(y .lt. y2) then
            write(12,*) x,y,dist
            goto 10
          else
            y=y2
            dist=abs(y2-y1)
            write(12,*) x,y,dist
            goto 999
          endif
        else 
          if(y .gt. y2) then
            write(12,*) x,y,dist
            goto 10
          else
            y=y2
            dist=abs(y2-y1)
            write(12,*) x,y,dist
            goto 999
          endif
        endif
      endif
*********0*********0*********0*********0*********0*********0*********0**
      if(abs(y2-y1) .lt. 1E-08) then
        x=x1
        y=y1
        write(12,*) x,y,dist
  20    x=x+dd
        dist=dist+abs(dd)
        if(slope .ge. 0.0) then
          if(x .lt. x2) then
            write(12,*) x,y,dist
            goto 20
          else
            x=x2
            dist=abs(x2-x1)
            write(12,*) x,y,dist
            goto 999
          endif
        else
          if(x .gt. x2) then
            write(12,*) x,y,dist
            goto 20
          else
            x=x2
            dist=abs(x2-x1)
            write(12,*) x,y,dist
            goto 999
          endif
        endif
      endif

*********0*********0*********0*********0*********0*********0*********0**
      c = sqrt((x2-x1)**2 + (y2-y1)**2)
      n = int(c/abs(dd))
      dx=  (sqrt(c**2 - (y2-y1)**2))/n
      dy=  (sqrt(c**2 - (x2-x1)**2))/n
      if(x2 .lt. x1) dx = -dx  
      if(y2 .lt. y1) dy = -dy  
        x=x1
        y=y1
        write(12,*) x,y,dist
  30    x=x+dx
        y=y+dy
        dist=dist+abs(dd)
        if(dx .ge. 0.0) then
          if(x .lt. x2) then
            write(12,*) x,y,dist
            goto 30
          else
            dist=c
            write(12,*) x2,y2,dist
            goto 999
          endif
        else
          if(x .gt. x2) then
            write(12,*) x,y,dist
            goto 30
          else
            dist=c
            write(12,*) x2,y2,dist
            goto 999
          endif
        endif
 999  continue
*********0*********0*********0*********0*********0*********0*********0**
* Make a GMT script called gmt.track
      open(unit=15,file="gmt.track",status='UNKNOWN')
      write(15,'(a11)') "#! /bin/csh"
      write(15,'(a1)') " "
      if (abs(x2-x1) .gt. abs(y2-y1)) then
        dist = (x2-x1) 
        if(x2 .gt. 1) then
          write(15,'(a16,f10.4)') "set X1        = ", x1
          write(15,'(a16,f10.4)') "set X2        = ", x2
        else
          write(15,'(a16,f10.4)') "set X1        = ", x2
          write(15,'(a16,f10.4)') "set X2        = ", x1
        endif
        if (int(abs(y2-y1)) .eq. 0) then
          write(15,'(a32)')       "set Label     = 'Coercivity(mT)'"
        else
          write(15,'(a30)')       "set Label     = 'Distance(mT)'"
        endif
      else
        dist = (y2-y1) 
        if(y2 .gt. y1) then
          write(15,'(a16,f10.4)') "set X1        = ", y1
          write(15,'(a16,f10.4)') "set X2        = ", y2
        else
          write(15,'(a16,f10.4)') "set X1        = ", y2
          write(15,'(a16,f10.4)') "set X2        = ", y1
        endif
        if (int(abs(x2-x1)) .eq. 0) then
          write(15,'(a33)')       "set Label     = 'Interaction(mT)'"
        else
          write(15,'(a30)')       "set Label     = 'Distance(mT)'"
        endif
      endif
*********0*********0*********0*********0*********0*********0*********0**
      scaleX=5.0/dist
      write(15,'(a16,f10.4)') "set scaleX    = ", scaleX

* Default Y axes are superceded in track.gmt
      write(15,'(a16,f10.4)') "set Y1        = ", -0.1
      write(15,'(a16,f10.4)') "set Y2        = ", 1.0
      scaleY=6.0
      write(15,'(a16,f10.4)') "set scaleY    = ", scaleY
* GMT anotations and tic marks for plot axes 
      anotX= float(nint(abs(dist)/50)*10)
      ticX = anotX/5.0
      if(abs(dist) .lt. 25) anotX=5.0
      if(abs(dist) .lt. 25) ticX=5.0
      write(15,'(a17,1pe7.1,a1,1pe7.1)') "set ticX      = a",
     &     anotX,"f",ticX
      write(15,'(a26)')      "set ticY      = a0.10f0.02"

      write(15,'(a48)') 
     &   'grdtrack track.out -Grhodxdyrn.grd -H > track.xy'
      if (abs(x2-x1) .gt. abs(y2-y1)) then
        write(15,'(a44)') "sed 1d track.xy | awk '{ print $1, $4 }' > t" 
      else
        write(15,'(a44)') "sed 1d track.xy | awk '{ print $2, $4 }' > t" 
      endif

*     write(15,'(a50,a47,a40,a34)') 
*    & 'psxy track.xy -R$X1/$X2/$Y1/$Y2 -Jx$scaleX/$scaleY',
*    & '-B$ticX\ :"B (mT)"::,"":/$ticY\ :"Normalized Rho',
*    & ' "::,""::."FORC Distribution track":WeSn ',
*    & ' -W1 -M -P -K -X1.0 -Y1.0 > fig.ps '
*     write(15,'(a58)') 
*    &'pstext -R0/8.5/0/11 -Jx1 -O -X-1.0 -Y-1.0 << END >> fig.ps'
*     write(15,'(a3)') 'END'
*********0*********0*********0*********0*********0*********0*********0**
      stop 
      end
