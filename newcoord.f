*********0*********0*********0********0*********0*********0*********0**
* Newcoord.f: Reads a GMT xzy file and transforms it to the FORC
* coordinate system, which is a rotation by the original (Ha, Hr)
* coordinate system by 45 degrees clockwise and a multiplication 
* by sqrt(2)/2 = 1/sqrt(2) = 0.7071.  This equates to:
*     xnew = (x - y)/2
*     ynew = (x + y)/2
*
* Intended for use with processing FORC data, which is currently
* being done in program forcit..
*
* To execute, type
* newcoord < input > output
*
* Written by  Gary Acton March 2004
* Modified by Gary Acton September 2004 (comments added and code cleaned).
*********0*********0*********0********0*********0*********0*********0**
      real x, y, xnew, ynew
      character*70 title
      character*1 tab
      character*13 z
      read(5,'(a70)') title 
      write(6,'(a70)') title 
      tab='\t'
  1   read(5,'(e13.6,1x,e13.6,1x,a13)', END=999) x,y,z 
      xnew = (x - y)/2
      ynew = (x + y)/2
      write(6,'(sp1pe13.6,1a,sp1pe13.6,1a,a13)') xnew,
     &         tab,ynew,tab,z
      goto 1
 999  continue
      stop
      end
