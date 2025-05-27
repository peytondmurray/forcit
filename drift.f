*********0*********0*********0********0*********0*********0*********0**
* drift.f: Reads the raw drift data output in file drift.raw by
* program readforc.f. It then smooths the drift data and corrects
* the raw data in file Ha-vs-Hr-vs-M.raw for this smoothed drift. 
* It also outputs three other files:
*
*       drift.raw: contains the drift data that are used in subroutine
*               drift.f, which smooths the raw drift data. The smoothed 
*               data are output to file drift.out. Both files have
*               columns with FORC#, field drift, normalized
*               moment drift, raw field value at which the drift
*               was monitored during the measurement, and
*               the raw moment measured at this drift field point.
* 	H-vs-M.out: contains the field and moment data with the 
*		drift correction applied. The data are formatted
*		for plotting in gmt.
*       Ha-vs-Hr.out: contains the applied field and reversal field
*               data with the drift correction applied. 
*		The data are formatted for plotting in gmt.
*
* Written by Gary Acton 3 August 2006
*
*********0*********0*********0********0*********0*********0*********0**
      subroutine drift(maxdrift, mindrift)
      parameter (MM=80000)
      parameter (NN=5000)
      real h(MM), moment(MM), hr(MM)
      real hdrift(NN), mdrift(NN),smdrift(NN), hpt(NN), mpt(NN)
      real maxdrift, mindrift
      integer nforc
*********0*********0*********0********0*********0*********0*********0**
      open(unit=22,file="Ha-vs-Hr-vs-M.raw",status='OLD')
      open(unit=23,file="Ha-vs-Hr-vs-M.out",status='UNKNOWN')
      open(unit=24,file="drift.raw",status='OLD')
      open(unit=25,file="drift.out",status='UNKNOWN')
*********0*********0*********0********0*********0*********0*********0**
      maxdrift=1.0
      mindrift=1.0
*********0*********0*********0********0*********0*********0*********0**
* i is the same as nforc for file drift.raw
      i=1  
  10  read(24,'(i5,1x,4(sp1pe13.6,1x))', end=19)
     &  nforc, hdrift(i), mdrift(i), hpt(i), mpt(i)
      if(mdrift(i) .gt. maxdrift) maxdrift = mdrift(i)
      if(mdrift(i) .lt. mindrift) mindrift = mdrift(i)
      i=i+1    
      goto 10
  19  nforcs=i-1

*     do 36 i=1, nforcs
*       smdrift(i)=mdrift(i)
* 36  continue
*     goto 29

*********0*********0*********0********0*********0*********0*********0**
* Smoothing is just a 5 point running average, which is done twice.
* smdrift is the smoothed drift of the moment
      ismooth=0
  20  smdrift(1)=(mdrift(1) + mdrift(2) + mdrift(3))/3.0
      smdrift(2)=(mdrift(1) + mdrift(2) + mdrift(3) + mdrift(4))/4.0
      do 25 i=3, nforcs-2
        smdrift(i)=(mdrift(i-2) + mdrift(i-1) + mdrift(i) +
     &              mdrift(i+1) + mdrift(i+2) )/5
  25  continue
      smdrift(nforcs-1)=(mdrift(nforcs-3) + mdrift(nforcs-2) +
     &                   mdrift(nforcs-1) + mdrift(nforcs))/4.0
      smdrift(nforcs)=  (mdrift(nforcs-2) + mdrift(nforcs-1) +
     &                   mdrift(nforcs))/3.0
      ismooth=ismooth+1
      if(ismooth .gt. 1) goto 29 
      do 26 i=1, nforcs
        mdrift(i)=smdrift(i)
  26  continue
      goto 20

  29  continue
*********0*********0*********0********0*********0*********0*********0**
      do 30 j=1, nforcs
        write(25,'(i5,1x,4(sp1pe13.6,1x))') j, hdrift(j), smdrift(j),
     &                         hpt(j),mpt(j)
  30  continue
      i=1
  40  read(22,'(3(sp1pe13.6,1x),s i4,1x,i5,1x,i6)', end=99)
     &     h(i),hr(i), moment(i), j, ij, idum
      moment(i)=moment(i)/smdrift(j)
      write(23,'(3(sp1pe13.6,1x),s i4,1x,i5,1x,i6)')
     &   h(i),hr(i), moment(i), j, ij, i
      i=i+1
      goto 40
  99  continue

*********0*********0*********0********0*********0*********0*********0**
      close(unit=25)
      return
      end
*********0*********0*********0********0*********0*********0*********0**
