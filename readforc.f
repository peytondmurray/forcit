*********0*********0*********0********0*********0*********0*********0**
* Readforc.f: Reads a standard file output from making a FORC 
* measurement on the Princeton Corporation MicroMag AGM or VSM.
* It then:
*  1) extracts and reformats the data for use in other programs
*  2) corrects for paramagnetic slope and drift
*  3) creates a GMT script for plotting FORC diagrams, the drift curve,
*        and other related diagrams.
*
* Typically readforc is executed from program FORCIT, which is a
* Unix shell script. The execution command is 
* 	readforc < input
* where the input file is a the MicroMag output file that is formatted
* to be a Unix text file (not as a PC text file).
*
*       drift.raw: contains the drift data that are used in subroutine
*               drift.f, which smooths the raw drift data. The smoothed 
*               data are output to file drift.out. Both files have
*               columns with FORC#, field drift, normalized
*               moment drift, raw field value at which the drift
*               was monitored during the measurement, and
*               the raw moment measured at this drift field point.
*       Ha-vs-Hr-vs-M.raw: contains the applied field, reversal field,
*               and moment data along with the FORC#, the datum # along 
*               the particular FORC, and the datum # relative to the
*               total number of observations. 
*       Ha-vs-Hr-vs-M.out: same as above but with the drift correction
*               applied.
*       Ha-vs-Hr.out: contains the applied field and reversal field
*               data with the drift correction applied.
*               The data are formatted for plotting in gmt.
*       H-vs-M.out: contains the field and moment data with the
*               drift correction applied. The data are formatted
*               for plotting in gmt.
*
* Written by Gary Acton 2004, 2005, 2006
*     Last change 11 August 2006
*
*********0*********0*********0********0*********0*********0*********0**
      parameter (MM=800000)
      parameter (NN=50000)
      character*70 header(35)
      character*300 fline
      character*70 sampid
      character*27 data
      character*17 ptitle
      character*3 ftype
      character*3 aunits
      character*1 atest,title(70),quote,DEBUG
      real hpt(MM),mpt(MM), h(MM), moment(MM), hr(MM)
      real old(14)
      real hsat, hcal, hdrift,mdrift, maxdrift, mindrift
      real hb1, hb2, hc1, hc2, hncr, slope
      real mhigh,  Mnorm
      real hrnow
      integer hrtest
*********0*********0*********0********0*********0*********0*********0**
      open(unit=2, file='/dev/tty')
      open(unit=12,file="Ha-vs-Hr-vs-M.raw",status='UNKNOWN')
      open(unit=14,file="drift.raw",status='UNKNOWN')
      open(unit=15,file="gmt.forc",status='UNKNOWN')
      DEBUG='N'
      if(DEBUG .eq. 'Y')open(unit=16,file="debug.out",status='UNKNOWN')
*********0*********0*********0********0*********0*********0*********0**
      quote='\"'
      sqrtwo = SQRT(2.0)/2.0
* Set default values in case the data file does not have them
      aunits="cgs"
      hb1 = -1000.0
      hb2 = +1000.0
      hc1 = +0000.0
      hc2 = +2000.0
*     hncr is the step size in mT between measurements
      hncr = 5
      ismooth = 2
      slope = 1.0E-20
      maxdrift=1.0
      mindrift=1.0
      ftype='new'
      sampid ="\"No name given"
      hrhigh=0.0
      hrlow=0.0
      hahigh=0.0
      halow=0.0

*********0*********0*********0********0*********0*********0*********0**
* Read Micromag data, starting with the header lines
* The first test determines if the data is in the old
* format, which has a single header line with 14 parameters.
* In the new format, the first line begins with the word Micromag,
* hence the test determines if the first character is an M. 
      read(5,'(a300)') fline
      read(fline, '(a1)') atest
      if(atest .ne. 'M') then
*      Read in header information for data files with old format (units are likely cgs)
       aunits="cgs"
       read(fline,*) (old(i), i=1,14)
       hsat=old(12)
       hcal=1.0
       hncr=1.0
       read(5,'(a70)') header(2)
       read(5,'(a27)') data
       if(DEBUG .eq. 'Y') write(16,*) 'Here 1'
       if(DEBUG .eq. 'Y') write(16,'(a27)') data
       read(data, '(a1)') atest
       ftype='old'
       goto 6
      endif
* Read in header information for data files with new format
      do 5 i=1,35
         read(5,'(a70)') header(i)
         read(header(i), '(a17)') ptitle
*********0*********0*********0********0*********0*********0*********0**
         read(header(i), '(a1)') atest
         if(DEBUG .eq. 'Y') write(16, '(a17)') ptitle
         if(atest .eq. '+' .or. atest .eq. '-') then
           if(DEBUG .eq. 'Y') write(16,*)'going to get data', i
           read(header(i),'(a27)') data 
           goto 6
         endif
         if(atest .eq. quote) then
           read(header(i), '(a70)') sampid
         endif
* Note, within the data file, the Units are given as either "cgs" or "Hybrid SI", e.g.,
* Units of measure:  cgs
* Units of measure:  Hybrid SI
         if(ptitle .eq. "Units of measure:") then
           read(header(i), '(19x,a1)') atest
           if(atest  .eq. "H") then
             aunits="SI "
             write(2,*) "Measurements are determined to be in SI Units"
           else
             aunits="cgs"
             write(2,*) "Measurements are determined to be in cgs Units"
           endif
         elseif(ptitle .eq. "Hb1            = ") then
           read(header(i), '(17x,e13.6)') hb1
           if(DEBUG .eq. 'Y') write(16,*) ptitle, hb1
         elseif(ptitle .eq. "Hb2            = ") then
           read(header(i), '(17x,e13.6)') hb2
           if(DEBUG .eq. 'Y') write(16,*) ptitle, hb2
         elseif(ptitle .eq. "Hc1            = ") then
           read(header(i), '(17x,e13.6)') hc1
           if(DEBUG .eq. 'Y') write(16,*) ptitle, hc1
         elseif(ptitle .eq. "Hc2            = ") then
           read(header(i), '(17x,e13.6)') hc2
           if(DEBUG .eq. 'Y') write(16,*) ptitle, hc2
         elseif(ptitle .eq. "HCal           = ") then
           read(header(i), '(17x,e13.6)') hcal
           if(DEBUG .eq. 'Y') write(16,*) ptitle, hcal
         elseif(ptitle .eq. "HNcr           = ") then
           read(header(i), '(17x,e13.6)') hncr 
           if(DEBUG .eq. 'Y') write(16,*) ptitle, hncr
         elseif(ptitle .eq. "HSat           = ") then
           read(header(i), '(17x,e13.6)') hsat
           if(DEBUG .eq. 'Y') write(16,*) ptitle, hsat
         elseif(ptitle .eq. "Smoothing      = ") then
           read(header(i), '(17x,i1)') ismooth
           if(DEBUG .eq. 'Y') write(16,*) ptitle, ismooth
         elseif(ptitle .eq. "Slope corr.    = ") then
           read(header(i), '(17x,a1)') atest
           if(atest .eq. "N") goto 5
           read(header(i), '(17x,e13.6)') slope
           if(DEBUG .eq. 'Y') write(16,*) ptitle, slope
         elseif(ptitle .eq. "Moment range   = ") then
           read(header(i), '(17x,e13.6)') mhigh
           if(DEBUG .eq. 'Y') write(16,*) ptitle, mhigh
*          set mhigh=0 and then search the data for the true extreme values
*          mhigh=0
         endif 
         write(6,'(a70)') header(i)
   5  continue
   6  continue
* Note the conversions are from SI Teslas to SI mT with moment remaining in A m^2
* or are from cgs (Oe and emu) to SI (mT and A m^2).
      if(aunits .eq. "SI ") then
        hb1=hb1*1000.0
        hb2=hb2*1000.0
        hc1=hc1*1000.0
        hc2=hc2*1000.0
        hcal=hcal*1000.0
        hncr=hncr*1000.0
        hsat=hsat*1000.0
        slope=slope*1000.0
       else
        hb1=hb1/10.0
        hb2=hb2/10.0
        hc1=hc1/10.0
        hc2=hc2/10.0
        hcal=hcal/10.0
        hncr=hncr/10.0
        hsat=hsat/10.0
        mhigh=mhigh/1000.0
        slope=slope*(10.0/1000.0)
      endif
      if(DEBUG .eq. 'Y') write(2,*)  "slope =", slope
*********0*********0*********0********0*********0*********0*********0**
* i  is the total number of data (observations)
* j  is the FORC number (as denoted by the number of times the sample
*       is saturated before starting a new FORC)
* ij is the number of data (observations) along each FORC
      i=0
      j=1
      ij=0
      if(DEBUG .eq. 'Y') write(16,*) 'Here 2'
      if(DEBUG .eq. 'Y') write(16,'(a27)') data
      if(atest .eq. '+' .or. atest .eq. '-') goto 10
   9  read(5,'(a27)') data 
      read(data,'(a1)') atest
      if(atest .eq. '+' .or. atest .eq. '-') goto 10
      goto 9
* The drift is determined by monitoring the moment change for a
* nearly constant field (hpt = a constant field point)
* The drift or variation in the applied field = hdrift is negligible
* and so hdrift could be set to 1.0, but I go ahead and correct for
* the slight variation.
*
* The drift of the moment relative to the first moment
* measured at a point (mpt) = mdrift is what is removed.
* Also the applied field varition at a point (hpt) relative
* to that applied at the first drift point is removed. 
* 
  10  read(data,'(e13.6,1x,e13.6)') hpt(j),mpt(j)
      if(aunits .eq. "SI ") then
        hpt(j)=hpt(j)*1000.0
      else
        hpt(j)=hpt(j)/10.0
        mpt(j)=mpt(j)/1000.0
      endif
      hdrift= hpt(j)/hpt(1)
      mdrift= mpt(j)/mpt(1)
* To turn off drift correction, remove the comments from the next three lines
*     write(2,*) "DRIFT CORRECTION TURNED OFF"
*     hdrift=1.0
*     mdrift=1.0
      if(mdrift .gt. maxdrift) maxdrift = mdrift
      if(mdrift .lt. mindrift) mindrift = mdrift
      ij=0
      if(j .eq. 1) then
*       This sets the maximum moment to 2% larger than the first drift point
*       All other moments are normalized by this value.
        Mnorm =  mpt(1) + slope*hpt(1)
        Mnorm =  Mnorm + Mnorm*0.02
      endif
      write(14,'(i5,1x,4(sp1pe13.6,1x))') j, hdrift, mdrift,
     &                         hpt(j),mpt(j)
      hrtest=1
      if(ftype .eq. 'old') then
*        read a second calibration point that is not used
         read(5,'(a27)') data 
*        read a third calibration point that is not used
         read(5,'(a27)') data 
         goto 13
      endif
      if(j.eq.1) then
        i=i+1
        ij=ij+1
*       First read statement is for a blank line
        read(5,'(a27)') data 
        read(5,'(a27)') data 
        read(data,'(e13.6,1x,e13.6)') h(i),moment(i)
        if(aunits .eq. "SI ") then
          h(i)=h(i)*1000.0
        else
          h(i)=h(i)/10.0
          moment(i)=moment(i)/1000.0
        endif
        hr(i)=h(i)
        moment(i)= moment(i) + slope*h(i)
        if(DEBUG .eq. 'Y') write(16,*) moment(i), slope, h(i)
        moment(i)= moment(i)/Mnorm
        write(12,'(3(sp1pe13.6,1x),s i4,1x,i5,1x,i6)') 
     &   h(i),hr(i), moment(i), j, ij, i
 12     read(5,'(a27)') data 
*       This is a check to make sure that the first FORC is a single point.
*       If it is not (which should never happen, but it has happened probably
*       as a result of a user manually editing a file ... or maybe yet another
*       version of the PMC software exist), then just ignore a few data until
*       the next FORC is found.
        read(data,'(1x,a1)') atest
        if(atest.ne." ") goto 12
        read(5,'(a27)') data 
        j=j+1
        goto 10
      endif
         
*********0*********0*********0********0*********0*********0*********0**
  13  data='                           '
      read(5,'(a27)', END=99) data 
      if(DEBUG .eq. 'Y') write(16,'(a27)') data
      read(data,'(1x,a1)') atest
      if(atest.eq." ") then
  15    i=i+1
        ij=ij+1
        data='                           '
        read(5,'(a27)', END=99) data 
        read(data,'(1x,a1)') atest
        if(atest.eq." ") then
          i=i-1
          ij=ij-1
          j=j+1
          read(5,'(a27)', END=99) data 
          read(data,'(1x,a1)') atest
          if(atest.eq."i" .or. atest .eq. "M") goto 99
          goto 10
        endif
        read(data,'(e13.6,1x,e13.6)') h(i),moment(i)
        if(aunits .eq. "SI ") then
          h(i)=h(i)*1000.0
        else
          h(i)=h(i)/10.0
          moment(i)=moment(i)/1000.0
        endif
        if(j.eq. 3 .and. ftype .eq. 'old') then
          if(ij .eq. 3) hncr=abs(h(3)-h(2))
        endif
        moment(i)= moment(i) + slope*h(i)
        moment(i)= moment(i)/Mnorm

*       The following test (hrtest) is to check if the field value is the reversal point
*       which is the first point along a FORC path. 
        if(hrtest .eq. 1) then
          hrtest = 0
          hrnow = h(i)
        endif
        hr(i)=hrnow
        write(12,'(3(sp1pe13.6,1x),s i4,1x,i5,1x,i6)') 
     &       h(i),hr(i), moment(i), j, ij, i
        goto 15
      endif
*********0*********0*********0********0*********0*********0*********0**
  99  continue
      npts=i
      nforcs=j

*********0*********0*********0********0*********0*********0*********0**
      close (unit=12)
      close (unit=14)
      call drift(maxdrift, mindrift)
*********0*********0*********0********0*********0*********0*********0**
* Output three GMT formatted files
      open(unit=10,file="H-vs-M.out",status='UNKNOWN')
      open(unit=11,file="Ha-vs-Hr.out",status='UNKNOWN')
      open(unit=12,file="Ha-vs-Hr-vs-M.out",status='UNKNOWN')
      i=1
      idum=1
      jtest=1
 100  if(idum .ge. npts) goto 150 
      read(12,'(3(sp1pe13.6,1x),s i4,1x,i5,1x,i6)',end=150)
     &     h(i),hr(i), moment(i), j, ij, idum
      if(i .eq. 1) then
        write(10,'("> First FORC (a single point)")')
        write(11,'("> First FORC (a single point)")')
      endif
      if(j .ne. jtest) then
        write(10,'("> New FORC")')
        write(11,'("> New FORC")')
      endif
      if(hr(i) .gt. hrhigh) hrhigh=hr(i)
      if(hr(i) .lt. hrlow) hrlow=hr(i)
      if(h(i) .gt. hahigh) then
        hahigh=h(i)
        hredge=hr(i)
      endif
      if(h(i) .lt. halow) halow=h(i)
      if(DEBUG .eq. 'Y') then
        write(16,*) "High and low values"
        write(16,'(3(sp1pe13.6,1x))') h(i),hahigh,hrlow
      endif
      write(10,'(sp1pe13.6,1x,sp1pe13.6)') h(i),moment(i)
      write(11,'(sp1pe13.6,1x,sp1pe13.6)') h(i),hr(i)
      i=i+1
      jtest=j
      goto 100
 150  continue
      if(DEBUG .eq. 'Y') write(2,*) "read out file", idum, npts
      close (unit=10)
      close (unit=11)
      close (unit=12)
*********0*********0*********0********0*********0*********0*********0**
* get the quasi-reversible ridge, which is the FORC distribution along 
* the Bb or Hb axis for Bc or Hc ~ 0.

      call rridge(ridgeH1, ridgeH2, ddmin, ddmax)

*********0*********0*********0********0*********0*********0*********0**
* Determine plot variables and start creating the GMT script
*********0*********0*********0********0*********0*********0*********0**
      read(sampid,'(70a1)') (title(i),i=1,70) 
      nt=1
* Determine if a end quote occurs in the sampid
* If not, add one
      do 200 i=2,70
        nt=nt+1
        if(title(i).eq. quote) goto 210 
 200  continue
      title(70)=quote
      if(DEBUG .eq. 'Y') write(16,'(70a1)') (title(i),i=1,70) 
 210  continue

*********0*********0*********0********0*********0*********0*********0**
      if(ftype .eq. 'old') then
        hc1=0.0
*       reduce area a bit (55%) from highest field, which is
*       hpt(1) or very near it.
* Note, for cgs conversion to mT that hpt(1) was converted above
        hc2=hpt(1)*0.55
        hb2=hc2*(old(9)/10.0)/2.0 + old(13)
*       hc2=old(12)
*       hb2=old(13)

        hc2=(hahigh+abs(hredge)-5.0*hncr)/2.0
        hb2=hrhigh

        hb1=-hb2
        if(DEBUG .eq. 'Y')write(2,*) "old(9) =", old(9)
        if(DEBUG .eq. 'Y')write(2,*) "old(13) =", old(13)
        if(DEBUG .eq. 'Y') write(2,*) "hc2 =", hc2
        if(DEBUG .eq. 'Y') write(2,*) "hb1 =", hb1
        if(DEBUG .eq. 'Y') write(2,*) "hb2 =", hb2
      endif
       
* The following code determines the grid spacing (step1).
* For high coercivity samples (e.g., hematite), which typically
* span a large range of Hc values, a small step size will result
* in a huge number of data in the grid. As a result, producing
* a plot will take a long time. Thus, I use a larger step size
* for samples that have coercivity axes that extend beyond 300 mT.
* For lower coercivity samples, the small step size (~1/4 hncr = the 
* original step sized used in collecting the data) can be important
* for creating the FORC plot in hysteresis space. 
* For all other plots, a larger value (e.g., ~1/2 hncr) works well.
* I also round step1 down to the nearest factor of 10 just to
* keep from having odd numbers in many of the parameters
*
* Variables stepx and ifilt are filters. 
* Because stepx is used to filter the reversible ridge and edge
* effects that occur along that grid boundary, it should 
* be several times the field step size in order to do what it was intended.
* Experimentation shows that ifilt should be about 10 to 30 times 
* the measurment field step size.
      if(hc2 .gt. 300) then
*       if(hncr .ge. 30) step1 = 10 
*       if(hncr .lt. 25 .and. hncr .ge. 10) step1 = 5 
*       if(hncr .lt. 10) step1 = float(nint(hncr/2))
* After experimentation, I am using the factor of 2.7 below
        step1 = float(nint(hncr/2.7))
*       Miniumum step allowed for this type of FORC is 0.5 mT (5 Oe) just because
        if(step1 .lt. 1 ) step1 = 0.5
        ifilt  = nint(18*step1)
      else
        if(hncr .ge. 8) step1 = 2
        if(hncr .lt. 8) step1 = 1
        ifilt  = nint(18*step1)
        if(hncr .lt. 3) then
          step1 = 0.5
          ifilt = 10
        endif
        if(hncr .lt. 2) then
          step1 = 0.4
          ifilt = 8
        endif
*       Overall miniumum step allowed is 0.2 mT (2 Oe) because this
*            roughly the uncertain in the field for the MicroMag.
*            Gridding at finer resolution, just resolves the noise better.
        if(hncr .lt. 1) then
          step1 = 0.2
          ifilt = 5
        endif
      endif
      stepx = 5*step1

      if(DEBUG .eq. 'Y') write(2,*) "step1= ", step1
      if(DEBUG .eq. 'Y') write(2,*) "hc2= ", hc2
      if(DEBUG .eq. 'Y') write(2,*) "hncr = ", hncr
      if(DEBUG .eq. 'Y') write(2,*) "hrhigh = ", hrhigh
      if(DEBUG .eq. 'Y') write(2,*) "hrlow = ", hrlow
      if(DEBUG .eq. 'Y') write(2,*) "hahigh = ", hahigh
      if(DEBUG .eq. 'Y') write(2,*) "halow = ", halow

*********0*********0*********0********0*********0*********0*********0**
* determine ibdrHa1
      if(abs(halow) .gt. 1000) then 
        ibdrHa1= 100* nint(halow/100 - 1.0)
      elseif (abs(halow) .gt. 100 .and. abs(halow) .le. 1000) then
        ibdrHa1= 10* nint(halow/10 - 1.0)
      else
        ibdrHa1= 10* nint(halow/10)
      endif
* determine ibdrHa2
      if(hahigh .gt. 1000) then 
        ibdrHa2= 100* nint(1.0 + hahigh/100)
      elseif (hahigh .gt. 100 .and. hahigh .le. 1000) then
        ibdrHa2= 10* nint(1.0 + hahigh/10)
      else
        ibdrHa2= 10* nint(hahigh/10)
      endif
* determine ibdrHr1
      if(abs(hrlow) .gt. 1000) then 
        ibdrHr1=  100 * nint(hrlow/100 - 1.0)
      elseif (abs(hrlow) .gt. 100 .and. abs(hrlow) .le. 1000) then
        ibdrHr1=  10 * nint(hrlow/10 - 1.0)
      else
        ibdrHr1=  10 * nint(hrlow/10)
      endif
* determine ibdrHr2
      if(hrhigh .gt. 1000) then 
        ibdrHr2=  100 * nint(1.0 + hrhigh/100)
      elseif (hrhigh .gt. 100 .and. hrhigh .le. 1000) then
        ibdrHr2=  10 * nint(1.0 + hrhigh/10)
      else
        if((10*nint(hrhigh/10)-hrhigh) .gt. 0) then
          ibdrHr2=  10 * nint(hrhigh/10)
        else
          ibdrHr2=  10 * nint(1 + hrhigh/10)
        endif
      endif
*********0*********0*********0********0*********0*********0*********0**
* Note, I force the hb axes to to be symmetric because that is typical of our measurement setup
      ihb1=nint(-1.0*hb2)
      ihb2=nint(hb2)
      ihc1=nint(hc1)
      ihc2=nint(hc2)
* Make sure there are an integer number of grid points between the grid boundary
      if(DEBUG .eq. 'Y')
     &   write(2,*) "ibdrHr1 = ", ibdrHr1,"  ibdrHr2 =",ibdrHr2
      igrid=10*nint((ibdrHr2-ibdrHr1)/(10*step1))
      ibdrHr1= nint(-1 * igrid * step1) + ibdrHr2
      if(DEBUG .eq. 'Y')
     &   write(2,*) "igrid = ", igrid,"  Hr1 =",ibdrHr1
      igrid=10*nint((ibdrHa2-ibdrHa1)/(10*step1))
      ibdrHa2= nint(igrid * step1) + ibdrHa1
      if(DEBUG .eq. 'Y') write(2,*) ibdrHa1, ibdrHa2, igrid
      if(DEBUG .eq. 'Y')
     &   write(2,*) "igrid = ", igrid,"  Ha2 =",ibdrHa2

      if(DEBUG .eq. 'Y')
     &   write(2,*) "hc1 = ", hc1,"  hc2 =",ihc2
      if(DEBUG .eq. 'Y') write(2,*) "step1 = ", step1
      if(step1 .ge. 2) then
        igrid=nint((ihc2+abs(ihc1)-5*step1)/(step1))
      else
        igrid=10*int((ihc2-abs(ihc1))/(10*step1))
*       igrid= 10*nint((igrid - 5*step1)/10)
      endif
      ihc2= (igrid * step1) + ihc1
      if(DEBUG .eq. 'Y')
     &   write(2,*) "igrid = ", igrid,"  hc2 =",ihc2

      if(step1 .ge. 2) then
        igrid=int((abs(ihb2)-5*step1)/(step1))
      else
*       igrid=10*nint(abs(ihb2)/(10*step1))
        igrid=nint(abs(ihb2)/step1)
      endif
      if(DEBUG .eq. 'Y')
     &   write(2,*) "hb1 = ", hb1,"  hb2 =",ihb2
      ihb2= nint(igrid * step1)
      ihb1=-1*ihb2
      if(DEBUG .eq. 'Y')
     &   write(2,*) "igrid = ", igrid,"  hb2 =",ihb2
      
* Scaling for the plots is for a 5 inch by 5 inch plot
*   although I require equal scaling in the x & y 
*   directions when the units are the same, as in mT/inch (formerly Oe/inch)
*   for the FORC plots
      scaleHa = 5.0/float(ibdrHa2-ibdrHa1)
      scaleHr = 5.0/float(ibdrHr2-ibdrHr1)
      if(scaleHr .lt. scaleHa) then 
        scaleHa = scaleHr
      else
        scaleHr = scaleHa
      endif
*     scaleM = 5.0/(2.0*mhigh)
      scaleM = 5.0/2.0
      scaleHc = 5.0/(hc2+abs(hc1))
      scaleHb = 5.0/float(ihb2+abs(ihb1))
      if(DEBUG .eq. 'Y')  then
	write(16,*) scaleHb,scaleHc, hc1, hc2,hb1,hb2
      endif
      if(scaleHb .lt. scaleHc) then
        scaleHc = scaleHb
      else
        scaleHb = scaleHc
      endif
      if(DEBUG .eq. 'Y') write(16,*) scaleHb,scaleHc

      ibdryF2=j
      ibdryF1=0
      scaleF=5.0/ibdryF2
      if(ibdryF2 .lt. 80)  then
        ianotF=10
        iticF=5
      endif
      if(ibdryF2 .ge. 80)  then
        ianotF=20
        iticF=10
      endif
      if(ibdryF2 .ge. 150)  then
        ianotF=30
        iticF=10
      endif

      bdryD2= nint(1.0  + 100.0*maxdrift)/100.0
      bdryD1= nint(-1.0 + 100.0*mindrift)/100.0
      scaleD=5.0/(bdryD2-bdryD1)

      scaleRb=5.0/(ridgeH2-ridgeH1)
      scaleRrho=5.0/((ddmax-ddmin)/ddmax)
      Rrho1=ddmin/ddmax
      Rrho1=Rrho1-Rrho1*0.05
      Rrho2=1.0

* Determine annotation and tic
      bdry=(ibdrHa2+abs(ibdrHa1))/6.0
      if(bdry .le. 10.0 ) then
        anotH=1.0*nint((bdry/1.0))
      elseif(bdry .gt. 10.0 .and. bdry .le. 100.0) then
        anotH=10.0*nint((bdry/10.0))
      elseif(bdry .gt. 100.0) then
        anotH=100.0*nint((bdry/100.0))
      endif
      ticH=anotH/5.0

      bdry=(ihc2+abs(ihc1))/6.0
      if(bdry .le. 10.0 ) then
        anotFORC=1.0*nint((bdry/1.0))
      elseif(bdry .gt. 10.0 .and. bdry .le. 100.0) then
        anotFORC=10.0*nint((bdry/10.0))
      elseif(bdry .gt. 100.0) then
        anotFORC=100.0*nint((bdry/100.0))
      endif
      ticFORC=anotFORC/5.0

      bdry=(ridgeH2 - ridgeH1)/6.0
      if(bdry .le. 10.0 ) then
        anotRb=1.0*nint((bdry/1.0))
      elseif(bdry .gt. 10.0 .and. bdry .le. 100 ) then
        anotRb=10.0*nint((bdry/10.0))
      elseif(bdry .gt. 100.0) then
        anotRb=100.0*nint((bdry/100.0))
      endif
      ticRb=anotRb/5.0

      stepM = 0.010 
*********0*********0*********0********0*********0*********0*********0**
      write(15,'(a11)') "#! /bin/csh"
      write(15,'(a16,70a1)') 'set sampleid  = ', (title(i),i=1,70)
      write(15,'(a1)') " "
      write(15,'(a45)') "# Plot variables for Br and Ba               "
      write(15,'(a45)') "# step1 is the grid spacing in mT            "
      write(15,'(a16,f8.2)') "set step1     = ", step1
      write(15,'(a45)') "# stepx is the width of the reversible       "
      write(15,'(a45)') "#   ridge set to 0.                          "
      write(15,'(a16,f8.2)') "set stepx     = ", stepx
      write(15,'(a45)') "# stepM is used for gridding the FORC        "
      write(15,'(a45)') "#   distribution inside the hysteresis loop. "
      write(15,'(a16,3x,f5.3)') "set stepM     = ", stepM
      write(15,'(a1)') " "

* The following are filtering parameters. 
      write(15,'(a45)') "# filt is the width of the Gaussian filter   "
      write(15,'(a45)') "#   used for smoothing the data (in mT)      "
      write(15,'(a16,i8)') "set filt      = ", ifilt
      write(15,'(a1)') " "

*********0*********0*********0********0*********0*********0*********0**
      write(15,'(a45)') "# Plot variables for Br and Ba               "
      write(15,'(a16,i8)') "set bdryHr2   = ", ibdrHr2
      write(15,'(a16,i8)') "set bdryHr1   = ", ibdrHr1
      write(15,'(a16,i8)') "set bdryHa2   = ", ibdrHa2
      write(15,'(a16,i8)') "set bdryHa1   = ", ibdrHa1
      write(15,'(a16,sp1pe8.1)') "set scaleHa   = ", scaleHa
      write(15,'(a16,sp1pe8.1)') "set scaleHr   = ", scaleHr
      write(15,'(a17,1pe7.1,a1,1pe7.1,a7)') "set ticH      = a",
     &     anotH,"f",ticH,"g200000"
      write(15,'(a1)') " "
*********0*********0*********0********0*********0*********0*********0**
      write(15,'(a45)') "# Plot variables for Y-axis of drift curve   "
      write(15,'(a16,f8.2)') "set bdryD2    = ", bdryD2
      write(15,'(a16,f8.2)') "set bdryD1    = ", bdryD1
      write(15,'(a16,f8.2)') "set scaleD    = ", scaleD
      write(15,'(a26)')      "set ticD      = a0.01f0.01"
      write(15,'(a1)') " "
*********0*********0*********0********0*********0*********0*********0**
      write(15,'(a45)') "# Plot variables for X-axis of drift curve   "
      write(15,'(a16,i8)') "set bdryF2    = ", ibdryF2
      write(15,'(a16,i8)') "set bdryF1    = ", ibdryF1
      write(15,'(a16,f8.2)') "set scaleF    = ", scaleF
      if(iticF .ge. 10) then
       write(15,'(a17,i2,a1,i2)') "set ticF      = a",
     &     ianotF,"f",iticF
      else
       write(15,'(a17,i2,a1,i1)') "set ticF      = a",
     &     ianotF,"f",iticF
      endif
      write(15,'(a1)') " "
*********0*********0*********0********0*********0*********0*********0**
      write(15,'(a45)') "# Plot variables for Quasi-reversible ridge  "
      write(15,'(a16,f8.1)') "set ridgeH1   = ", ridgeH1
      write(15,'(a16,f8.1)') "set ridgeH2   = ", ridgeH2
      write(15,'(a16,f8.2)') "set Rrho1     = ", Rrho1
      write(15,'(a16,f8.2)') "set Rrho2     = ", +1.00
      write(15,'(a16,1pe8.1)') "set scaleRb   = ", scaleRb
*     write(15,'(a16,f8.1)') "set scaleRrho = ", 2.5
      write(15,'(a16,sp1pe8.1)') "set scaleRrho = ", scaleRrho
      write(15,'(a17,1pe7.1,a1,1pe7.1,a7)') "set ticRb     = a",
     &     anotRb,"f",ticRb,"g200000"
      write(15,'(a37)') "set ticRrho   = a0.25f0.05g20000     "
      write(15,'(a16,sp1pe8.1)') "set Ridgemin  = ", ddmin
      write(15,'(a16,sp1pe8.1)') "set Ridgemax  = ", ddmax
      write(15,'(a1)') " "
*********0*********0*********0********0*********0*********0*********0**
      write(15,'(a45)') "# Plot variables for moment                  "
      write(15,'(a16,i8)') "set bdryM2    = ",  1
      write(15,'(a16,i8)') "set bdryM1    = ", -1
      write(15,'(a16,sp1pe8.1)') "set scaleM    = ", scaleM
      write(15,'(a37)') "set ticM      = a0.5f0.1g2000        "
      write(15,'(a16,sp1pe8.1)') "set Mnorm     = ", Mnorm
      write(15,'(a1)') " "
*********0*********0*********0********0*********0*********0*********0**
      write(15,'(a45)') "# Plot variables for normalized FORC         "
      write(15,'(a45)') "#   distribution (rho).                      "
      write(15,'(a16,i8)') "set Rho2      = ", 1
      write(15,'(a16,f8.1)') "set Rho1      = ",-0.1
      write(15,'(a16,f8.1)') "set scaleRho  = ", 2.5
      write(15,'(a37)') "set ticRho    =   a1f0.5             "
      write(15,'(a1)') " "
*********0*********0*********0********0*********0*********0*********0**
      write(15,'(a45)') "# Plot variables for bias (Hb)               "
      write(15,'(a45)') "#   and for coercivity (Hc).                 "
      write(15,'(a16,i8)') "set Hb1       = ", ihb1
      write(15,'(a16,i8)') "set Hb2       = ", ihb2
      write(15,'(a16,i8)') "set Hc1       = ", ihc1
      write(15,'(a16,i8)') "set Hc2       = ", ihc2
      write(15,'(a16,sp1pe8.1)') "set scaleHc   = ", scaleHc
      write(15,'(a16,sp1pe8.1)') "set scaleHb   = ", scaleHb
      write(15,'(a17,1pe7.1,a1,1pe7.1,a7)') "set ticFORC   = a",
     &     anotFORC,"f",ticFORC,"g200000"
      write(15,'(a1)') " "
*********0*********0*********0********0*********0*********0*********0**
      write(15,'(a45)') "# Plot variables for view angles of 3d plot  "
      write(15,'(a37)') "set ViewAng1  = 130/20               "
      write(15,'(a37)') "set ViewAng2  =  50/20               "
      write(15,'(a1)') " "
*********0*********0*********0********0*********0*********0*********0**
      write(15,'(3a40)') "# Ndecimate is used in determining how ",
     &                   "many FORCs to show for the B-vs-M and  ",
     &                   "the B-vs-M_forc2d plots.               "
      write(15,'(a45)') "#    Every Nth=Ndecimate FORC is plotted.    "
      write(15,'(a37)') "set Ndecimate =      5               "
      write(15,'(a1)') " "
*********0*********0*********0********0*********0*********0*********0**
      stop
      end
