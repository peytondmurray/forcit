#!   Example Makefile        Compiles software          7 August 2003
BIN=	/Applications/Local/bin
#BIN=	/Users/acton/Applications/bin

COMPILE=	g77  
COMPILE=	fort77  

ALL = permissions1 forc forcdecimate newcoord readforc track zerodiag permissions2
	
all:	${ALL} 

permissions1:
	chmod a+rwx ../forc
	chmod a+rx forcit pctounix .gmtdefaults forcall.gmt forc2d.gmt track.gmt  Makefile 
	chmod a+r *.f cont_int

permissions2:
	chmod a+x ${BIN}/forcit ${BIN}/forcdecimate ${BIN}/newcoord ${BIN}/readforc ${BIN}/track ${BIN}/trackit ${BIN}/zerodiag 

forc:
	chmod a+x forcit pctounix
	cp forcit ${BIN}
	cp pctounix ${BIN}

#drift:
#	${COMPILE} drift.f -o drift
#	mv drift ${BIN}

forcdecimate:
	${COMPILE} forcdecimate.f -o forcdecimate
	mv forcdecimate ${BIN}

newcoord:
	${COMPILE} newcoord.f -o newcoord
	mv newcoord ${BIN}

readforc:
	${COMPILE} readforc.f drift.f rridge.f -o readforc
	mv readforc ${BIN}

readhys:
	${COMPILE} readhys.f -o readhys
	mv readhys ${BIN}
	chmod a+x hysplot
	cp hysplot ${BIN}

track:
	${COMPILE} track.f -o track
	cp trackit ${BIN}
	mv track ${BIN}

zerodiag:
	${COMPILE} zerodiag.f -o zerodiag
	mv zerodiag ${BIN}

clean:
	rm -f readforc zerodiag readhys *.o
