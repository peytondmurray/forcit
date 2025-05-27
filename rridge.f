*********0*********0*********0********0*********0*********0*********0**
* Rridge.f: Estimates the FORC distribution along the Hb axis (Hc=0)
* using only the data point at the start of the FORC (the reversal 
* point) and the next point along the FORC.
*
* This is the quasi-reversible ridge as the data are discrete.
*
* Written by Gary Acton       August 2006
*
*********0*********0*********0********0*********0*********0*********0**
      subroutine rridge(ridgeH1, ridgeH2, ddmin, ddmax)
      parameter (MM=80000)
      parameter (NN=5000)
      real h(MM), moment(MM), hr(MM)
      real dmdh(NN), ddmdh(NN), hb(NN)
      real u(MM),smooth(MM)
      real ridgeH1, ridgeH2, ddmin, ddmax
*********0*********0*********0********0*********0*********0*********0**
      open(unit=10,file="Ha-vs-Hr-vs-M.out", status='OLD')
      open(unit=11,file="rridge.out",status='UNKNOWN')
*********0*********0*********0********0*********0*********0*********0**
      i=1
      k=0
  10  read(10,'(3(sp1pe13.6,1x),s i4,1x,i5,1x,i6)',end=15)
     &     h(i),hr(i), moment(i), j, ij, idum
      if(i .eq. 1) then
        i=i+1
        goto 10
      endif
      if(ij .eq. 1) then
        i=i+1
        read(10,'(3(sp1pe13.6,1x),s i4,1x,i5,1x,i6)')
     &     h(i),hr(i), moment(i), j, ij, idum
        k=k+1
        dmdh(k)=(moment(i)-moment(i-1))/(h(i)-h(i-1))
*       note that Hb=Ha=Hr along the ridge
        hb(k)=hr(i)        
      endif
      goto 10
  15  continue
      npts=k
*********0*********0*********0********0*********0*********0*********0**
* Smoothing is just a 5 point running average, which is done twice.
      do 20 i=1,npts
        u(i)=dmdh(i)
  20  continue
      call smooth5(u,smooth,npts)
      do 30 i=1,npts
        u(i)=smooth(i)
  30  continue
      call smooth5(u,smooth,npts)
*********0*********0*********0********0*********0*********0*********0**
      do 50 i=1,npts
        dmdh(i)=smooth(i)
  50  continue
        
*********0*********0*********0********0*********0*********0*********0**
* Take the second derivative (See Equation 1 of Pike, Physical Rev. B,
* 68, 2003).
      do 60 k=2, npts
* Note, the equation below assumes that the FORCs are extended with
* constant moment for Ba < Br using the moment value measured at Ba=Br.
* This would give dm/dh = 0 for all grid points where Ba < Br. Thus, the
* second derivative is just equal to dmdh(k)/(hb(k)-hb(k-1)) because 
* [dmdh(k)-dmdh(k-1)] = dmdh(k) - O = dmdh(k)
        ddmdh(k)=(-0.5)*dmdh(k)/(hb(k)-hb(k-1))
* Note, the equation below (Commented OUT) takes the derivative along the diagonal in
* Ba-vs-Br space. This would be equivalent to extending the FORCs 
* along a constant slope for Ba < Br.
*       ddmdh(k)=(-0.5)*(dmdh(k)-dmdh(k-1))/(hb(k)-hb(k-1))
  60  continue
      ddmdh(1)=ddmdh(2)
*********0*********0*********0********0*********0*********0*********0**
* Smoothing is just a 5 point running average, which is done twice.
      do 70 i=1,npts
        u(i)=ddmdh(i)
  70  continue
      call smooth5(u,smooth,npts)
      do 80 i=1,npts
        u(i)=smooth(i)
  80  continue
      call smooth5(u,smooth,npts)

      ddmax=0.0
      ddmin=1.0
      do 90 i=1,npts
        ddmdh(i)=smooth(i)
        if(ddmdh(i) .gt.  ddmax) ddmax = ddmdh(i)
        if(ddmdh(i) .lt.  ddmin) ddmin = ddmdh(i)
  90  continue
       
* The 2% reduction in the normalizer is introduced for plotting purposes. 
* Given all the smoothing that is done, the 2% is negligible.
      dnormalizer=ddmax*1.02
      if(abs(ddmin) .gt. ddmax) dnormalizer=1.02*abs(ddmin)
      do 100 i=1,npts
        ddnorm=ddmdh(i)/dnormalizer
        write(11,'(4(sp1pe13.6,1x))') 
     &      hb(i),dmdh(i),ddmdh(i),ddnorm
 100  continue

*********0*********0*********0********0*********0*********0*********0**
      ridgeH1=float(nint(hb(npts)-1.0))
      ridgeH2=float(nint(hb(1)  + 1.0))
      close (unit=10)
      close (unit=11)
      return
      end
*********0*********0*********0********0*********0*********0*********0**
      subroutine smooth5(u,smooth,npts)
      parameter (MM=80000)
      real u(MM),smooth(MM)
      integer npts
      smooth(1)=(u(1) + u(2) + u(3))/3.0
      smooth(2)=(u(1) + u(2) + u(3) + u(4))/4.0
      do 10 i=3, npts-2
        smooth(i)=(u(i-2) + u(i-1) + u(i) +
     &             u(i+1) + u(i+2) )/5
  10  continue
      smooth(npts-1)=(u(npts-3) + u(npts-2) +
     &                u(npts-1) + u(npts))/4.0
      smooth(npts)=  (u(npts-2) + u(npts-1) + u(npts))/3.0
      return
      end
