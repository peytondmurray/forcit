# FORCIT & TRACKIT README

FORCIT is a program that produces a variety of diagrams 
related to first-order reversal curves (FORCs).
TRACKIT uses the FORC distribution grid file generated
by FORCIT to create a 1-D profile of the FORC distribution.

FORCIT, TRACKIT, their subroutines, and a Makefile are
available in a gzip/tar and stuffit formats in files:

	forcit.tar.gz
	forcit.sit

The files are available at the Web site 

	http://paleomag.ucdavis.edu/Software/forcit

Simply download one of the files from the web page (Option click for Macs). 
The file will likely automatically uncompress if you are using a Mac. 
If it does not automatically uncompress, then uncompress it using Stuffit Expander
or the gunzip command in a terminal window by typing:

	% gunzip forcit.tar
	% tar -xvf forcit.tar

To make (compile) the code, type
	make all

For details, see the paper:

Acton, G., Roth, A., and Verosub, K. L, Analyzing Micromagnetic
Properties With FORCIT Software. Eos, 2007 (in press). 

and for many more details see the manual: 
     FORCIT-Users-Guide.doc 
which is in the Manual directory.

This directory contains: 

drwxr-xr-x   17 acton  staff    578 Oct 22 08:51 Examples
-rwxr-xr-x    1 acton  staff   1193 Aug 10 15:18 Makefile
drwxr-xr-x    5 acton  staff    170 Oct 22 13:49 Manual
-rw-r--r--    1 acton  staff     42 Oct 22 13:50 Readme.txt
drwxr-xr-x   38 acton  staff   1292 Oct 20 14:44 Test-FORCs
-rw-r--r--    1 acton  staff     77 May  6  2004 cont_int
-rw-r--r--    1 acton  staff   3887 Aug 14 10:39 drift.f
-rwxr-xr-x    1 acton  staff   5813 Aug 16 16:02 forc2d.gmt
-rwxr-xr-x    1 acton  staff  18954 Aug 16 16:08 forcall.gmt
-rw-r--r--    1 acton  staff   2497 Aug 13 17:41 forcdecimate.f
-rwxr-xr-x    1 acton  staff   5645 Aug 16 16:18 forcit
-rw-r--r--    1 acton  staff   2379 Apr 26 17:46 forcit-cshrc
-rwxr-xr-x    1 acton  staff  10106 Aug 16 16:34 forcnow.gmt
-rw-r--r--    1 acton  staff   1110 Sep 27  2004 newcoord.f
-rwxr-xr-x    1 acton  staff    789 Feb 18  2005 pctounix
-rw-r--r--    1 acton  staff  29565 Oct 22 08:52 readforc.f
-rw-r--r--    1 acton  staff   4485 Sep 19 13:23 rridge.f
-rw-r--r--    1 acton  staff   7921 Oct 22 09:58 track.f
-rwxr-xr-x    1 acton  staff    605 Apr 26 16:44 track.gmt
-rw-r--r--    1 acton  staff   3075 Oct  9 15:26 trackit
-rw-r--r--    1 acton  staff   2658 Aug 13 21:43 zerodiag.f
