'Program to test correlation of VDC fraction coordinates.
'Compiler: PBWIN 10.04.0108 (www.powerbasic.com).

'Last modification: 03-15-2014 ~1832 HRS EST

'(c) Richard A. Formato.  All rights reserved worldwide.
'This source code is FREEWARE.  It may be freely copied,
'distributed and used as long as this copyright notice
'is included without modification and as long as no cost
'or fee is charged to the user, including "bundling" or
'similar fees.  Comments/questions: rf2@ieee.org

#COMPILE EXE
#DIM ALL
%USEMACROS = 1
#INCLUDE "Win32API.inc"
DEFEXT A-Z

GLOBAL VDCfracIndex&&, IndexIncrement&&, NumVDCfractions&&
GLOBAL ScreenWidth&, ScreenHeight&, Quote$

DECLARE FUNCTION Halton(Index&&,NBase&&)
DECLARE FUNCTION DecimalToVanDerCorputBaseN(N&&,Nbase&&)
DECLARE SUB CheckHalton
DECLARE SUB SieveOfEratosthenes(N&&,Primes&&(),NumPrimes&&)
DECLARE SUB CreateGNUplotINIfile(PlotWindowULC_X&&,PlotWindowULC_Y&&,PlotWindowWidth&&,PlotWindowHeight&&)
DECLARE SUB TwoDplot(PlotFileName$,PlotTitle$,xCoord$,yCoord$,XaxisLabel$,YaxisLabel$,_
                     LogXaxis$,LogYaxis$,xMin$,xMax$,yMin$,yMax$,xTics$,yTics$,GnuPlotEXE$,LineType$,Annotation$)

'----------------------------------------------------------------------------------------------------------------

FUNCTION PBMAIN () AS LONG
LOCAL Np&&, Nd&&, p&&, i&&, FirstDimNum&&, SecondDimNum&&, N&&, s1$, s2$, s1s2$, DefaultS1S2$, YorN&
LOCAL A$, B$, PlotTitle$, xMin$, xMax$, yMin$, yMax$, NumDims$, SequenceType$
LOCAL R(), VDCfractions() AS EXT
LOCAL Primes&&(), NumPrimes&&, MaxNum&&, Nbase&&, StartingIndex&&

'   call CheckHalton : end

    Quote$ = CHR$(34)

    SequenceType$ = "HALTON" '"VDC" '"HALTON" '"VDC"

    DESKTOP GET SIZE TO ScreenWidth&, ScreenHeight&  'get screen size (global variables)
    IF DIR$("wgnuplot.exe") = "" THEN
        MSGBOX("WARNING!  'wgnuplot.exe' not found.  Run terminated.") : EXIT FUNCTION
    END IF

    REDIM Primes&&(1 TO 1)
    MaxNum&& = 1000000 'max # for primes
    CALL SieveOfEratosthenes(MaxNum&&,Primes&&(),NumPrimes&&) 'fills Primes&&() array up to greatest prime =< MaxNum&&

    s1$ = "-999" : DefaultS1S2$ = "21"
    DO UNTIL VAL(s1$) >= 1 AND VAL(s1$) =< NumPrimes&&
         s1 = INPUTBOX$("Index of Starting Prime #"+$CRLF+$CRLF+_
                        "[1-50]","Nd & Np",DefaultS1S2$)
    LOOP
    StartingIndex&& = VAL(s1$)

    s1$ = "-999" : s2$ = "-999" : DefaultS1S2$ = "30 1000"
    DO UNTIL VAL(s1$) >= 1 AND VAL(s1$) =< 100 AND VAL(s2$) >= 1 AND VAL(s2$) =< 20000
         s1s2$ = INPUTBOX$("# Dimensions and # Data"+$CRLF+"Points to Plot"+$CRLF+_
                           "(separated by a space) "+$CRLF+$CRLF+_
                           "[1-100, 1-20000]","Nd & Np",DefaultS1S2$)
         s1$   = PARSE$(s1s2$,ANY" ./:-_",1)
         s2$   = PARSE$(s1s2$,ANY" ./:-_",2)
    LOOP
    Nd&&     = VAL(s1$) : Np&& = VAL(s2$)
    NumDims$ = REMOVE$(STR$(Nd&&),ANY" ")

    REDIM R(1 TO Np&&, 1 TO Nd&&)
    FOR p&& = 1 TO Np&&
        FOR i&& = 1 TO Nd&&
            Nbase&&    = Primes&&(StartingIndex&& + i&& - 1) 'each dimension has a different sequential prime # base
            IF SequenceType$ = "HALTON" THEN R(p&&,i&&) = Halton(p&&,NBase&&)
            IF SequenceType$ = "VDC"    THEN R(p&&,i&&) = DecimalToVanDerCorputBaseN(p&&,Nbase&&)
        NEXT i&&
    NEXT p&&

    s1$ = "-999" : s2$ = "-999" : DefaultS1S2$ = REMOVE$(STR$(MAX(1,Nd&&-1)),ANY" ")+" "+REMOVE$(STR$(Nd&&),ANY" ")
    DO UNTIL VAL(s1$) >= 1 AND VAL(s1$) =< Nd&& AND VAL(s2$) >= 1 AND VAL(s2$) =< Nd&&
         s1s2$ = INPUTBOX$("1st and 2nd Dimensions to"+$CRLF+_
                           "Plot (separated by a space)"+$CRLF+$CRLF+_
                           "[1-"+NumDims$+", 1-"+NumDims$+"]","PLOTTED COORDINATES",DefaultS1S2$)
         s1$   = PARSE$(s1s2$,ANY" ./:-_",1)
         s2$   = PARSE$(s1s2$,ANY" ./:-_",2)
    LOOP
    FirstDimNum&& = VAL(s1$) : SecondDimNum&& = VAL(s2$)

    PlotTitle$    = SequenceType$+" POINTS IN "+REMOVE$(STR$(Nd&&),ANY" ")+" DIMENSIONS, "+_
                    REMOVE$(STR$(Np&&),ANY" ")+" POINTS.\n"+_
                    "Plot of dimensions "+REMOVE$(STR$(FirstDimNum&&),ANY" ")+_
                    " and "+REMOVE$(STR$(SecondDimNum&&),ANY" ")+".\n"

    N&& = FREEFILE
    OPEN "VDCfracTst.DAT" FOR OUTPUT AS #N&&
        FOR p&& = 1 TO Np&&
            PRINT #N&&, USING$("#.#####  #.#####",R(p&&,FirstDimNum&&),R(p&&,SecondDimNum&&))
        NEXT p&&
    CLOSE #N&&

    MSGBOX("Plot data file 'VDCfracTst.DAT' has been created.")

    CALL CreateGNUplotINIfile(350,20,1000,1000)
    CALL TwoDplot("VDCfracTst.DAT",PlotTitle$,"0.6","0.7","x"+REMOVE$(STR$(FirstDimNum&&),ANY" ")+_
                  "\n\n","\nx"+REMOVE$(STR$(SecondDimNum&&),ANY" ")+"","NO","NO",_
                  xMin$,xMax$,yMin$,yMax$,"5","5","wgnuplot.exe"," pointsize 1 linewidth 2","")
END FUNCTION 'PBMAIN()

'---------------------

FUNCTION Halton(Index&&,NBase&&)
'Written 06-10-2012, raf
'See Wiki reference below.

'Returns a Halton sequence of numbers on [0,1] computed from NBase& (inetger base).
'Index& is which number in tehy sequence is returned.
LOCAL Z, ratio AS EXT
LOCAL D&&, I&&
    I&&   = Index&&
    Z     = 0##
    ratio = 1##/NBase&&
    DO WHILE I&& <> 0
        D&&   = I&& MOD NBase&&
        Z     = Z + D&&*ratio
        I&&   = (I&&-D&&)/NBase&&
        ratio = ratio/NBase&&
    LOOP
    Halton = Z
END FUNCTION

'REF: Wikipedia: http://en.wikipedia.org/wiki/Halton_sequence (reproduced below).
'HALTON SEQUENCE
'===============
'In statistics, Halton sequences are sequences used to generate points in space for numerical methods such as
'Monte Carlo simulations. Although these sequences are deterministic they are of low discrepancy, that is,
'appear to be random for many purposes. They were first introduced in 1960 and are an example of a quasirandom
'number sequence. They generalise the one-dimensional van der Corput sequences.

'Example of Halton sequence used to generate points in (0, 1) × (0, 1) in R2
'---------------------------------------------------------------------------
'The Halton sequence is constructed according to a deterministic method that uses a prime number as its base.
'As a simple example, let's take one dimension of the Halton sequence to be based on 2 and the other on 3.  To
'generate the sequence for 2, we start by dividing the interval (0,1) in half, then in fourths, eighths, etc, which
'generates: 1/2, 1/4, 3/4, 1/8, 5/8, 3/8, 7/8, 1/16, 9/16,...

'And to generate the sequence for 3, we divide the interval (0,1) in thirds, then ninths, twenty-sevenths, etc,
'which generates: 1/3, 2/3, 1/9, 4/9, 7/9, 2/9, 5/9, 8/9, 1/27,...

'When we pair them up, we get a sequence of points in a unit square:
'(1/2, 1/3), (1/4, 2/3), (3/4, 1/9), (1/8, 4/9), (5/8, 7/9), (3/8, 2/9), (7/8, 5/9), (1/16, 8/9), (9/16, 1/27).

'Even though standard Halton sequences perform very well in low dimensions, correlation problems have been
'noted between sequences generated from higher primes.  For example if we started with the primes 17 and 19,
'the first 17 pairs of points would have perfect linear correlation. To avoid this, it is common to drop the first 20
'entries, or some other predetermined number depending on the primes chosen. In order to deal with this
'problem, various other methods have been proposed; one of the most prominent solutions is the scrambled
'Halton sequence, which uses permutations of the coefficients used in the construction of the standard sequence.

'Implementation in Pseudo Code
'-----------------------------
'FUNCTION (index, base)
'BEGIN
'   result = 0;
'   f = 1 / base;
'   i = index;
'       WHILE (i > 0)
'       BEGIN
'           result = result + f * (i % base);
'           i = FLOOR(i / base);
'           f = f / base;
'       END
'       RETURN result;
'END

'---------------------

SUB CheckHalton
LOCAL Idx&&, NBase&&, A$
LOCAL Denom AS EXT
    NBase&& = 2 : A$ = ""
    Denom = 1##/64 'for checking only
    FOR Idx&& = 100 TO 125 '1 TO 70
        A$ = A$ + STR$(Halton(Idx&&,NBase&&)) + "         " + STR$(Halton(Idx&&,NBase&&)/Denom) + CHR$(13)
    NEXT Idx&&
    MSGBOX(A$)
END SUB 'CheckHalton

'-------------------

FUNCTION DecimalToVanDerCorputBaseN(N&&,Nbase&&)

'Returns the decimal value of the VDC base 2 'radical inverse function' to create a Low Discrepancy sequence of decimal values.
'Maps integer argument into a quasirandom real number on the interval [0,1).

'Refs: (1) "Improved Particle Swarm Optimization with Low-Discrepancy Sequences,"
'           Pant, Thangaraj, Grosan & Abraham, 2008 IEEE Congress on Evolutionary Computation (CEC2008), p. 3011.

'      (2) "Quasirandom Ramblings," Computing Science column in American Scientist Magazine, July-Augist 2011, p. 282.
'          (www.american scientist.org)

'CHECK VALUES FOR BASE 2
'-----------------------
'Integer(N&&)  Base 2 Representation   Digits Reversed    VDC Decimal Value
'------------  ---------------------   ---------------    -----------------
'      0                    0                     0          0.0
'      2                   10                    01          0.25
'      3                   11                    11          0.75
'      4                  100                   001          0.125
'      6                  110                   011          0.375
'      7                  111                   111          0.875
'     15                 1111                  1111          0.9375
'    120              1111000               0001111          0.1171875
'    121              1111001               1001111          0.6171875
'    532           1000010100            0010100001          0.1572265625

'CHECK VALUES FOR BASE 3
'-----------------------
'Integer(N&&)  Base 3 Representation   Digits Reversed    VDC Decimal Value
'------------  ---------------------   ---------------    -----------------
'      0                    0                     0          0.0
'      2                    2                     2          0.666...
'      3                   10                    01          0.111...
'      7                   21                    12          0.555...
'    120                11110                 01111          0.164609053497942
'    532               201201                102102          0.422496570544719

'CHECK VALUES FOR BASE 5
'-----------------------
'Integer(N&&)  Base 5 Representation   Digits Reversed    VDC Decimal Value
'------------  ---------------------   ---------------    -----------------
'      0                    0                     0          0.0
'      1                    1                     1          0.2
'      2                    2                     2          0.4
'      3                    3                     3          0.6
'      4                    4                     4          0.8
'      5                   10                    01          0.04
'      7                   12                    21          0.44
'     17                   32                    23          0.52
'    121                  441                   144          0.392
'    532                 4112                  2114          0.4544

LOCAL S AS EXT, R AS EXT, I&&, D&&

    S = 0##

    IF N&& > 2^63-2 THEN
        MSGBOX("WARNING! VDC arg > 2^63-2.  Zero returned.") : GOTO ExitVDC
    END IF

    I&& = N&& : R = 1##/Nbase&&

    DO WHILE I&& <> 0 : D&& = I&& MOD Nbase&& : S = S + D&&*R : I&& = (I&&-D&&)/Nbase&& : R = R/Nbase&& : LOOP

ExitVDC:

DecimalToVanDerCorputBaseN = S

END FUNCTION

'-----------

SUB SieveOfEratosthenes(N&&,Primes&&(),NumPrimes&&)

'Returns primes between 2 and N&& and their number.

'NOTE: There are 303 primes between 2 and 2000.

LOCAL i&&, iStart&&, p&&, j&&, TempArray&&()

    REDIM TempArray&&(1 TO N&&)

    TempArray&&(1) = 0 : FOR i&& = 2 TO N&& : TempArray&&(i&&) = 1 : NEXT i&&

    p&& = 2 : WHILE p&&^2 =< N&& : j&& = p&&^2 : WHILE j&& =< N&& : TempArray&&(j&&) = 0 : j&& = j&& + p&& : WEND : p&& = p&& + 1 : WEND 'inserts 0 or 1 depending on whether or not # is a prime

    NumPrimes&& = 0

    FOR i&& = 1 TO N&&  'converts the 1's into their corresponding prime numbers & retrieves # primes between 2 and N&&
        IF TempArray&&(i&&) = 1 THEN INCR NumPrimes&&
        TempArray&&(i&&) = i&&*TempArray&&(i&&)
    NEXT i&&

    REDIM Primes&&(1 TO NumPrimes&&)

    iStart&& = 1
    FOR j&& = 1 TO NumPrimes&&
        FOR i&& = iStart&& TO N&&
            IF TempArray&&(i&&) <> 0 THEN
                Primes&&(j&&) = TempArray&&(i&&) : iStart&& = i&&+1 : EXIT FOR
            END IF
        NEXT i&&
    NEXT j&&

END SUB 'SieveOfEratosthenes()

'-----------------------------

SUB TwoDplot(PlotFileName$,PlotTitle$,xCoord$,yCoord$,XaxisLabel$,YaxisLabel$, _
             LogXaxis$,LogYaxis$,xMin$,xMax$,yMin$,yMax$,xTics$,yTics$,GnuPlotEXE$,LineType$,Annotation$)
LOCAL N&&, ProcessID???
    N&& = FREEFILE
    OPEN "cmd2d.gp" FOR OUTPUT AS #N&&
        IF LogXaxis$ = "YES" AND LogYaxis$ = "NO"  THEN PRINT #N&&, "set logscale x"
        IF LogXaxis$ = "NO"  AND LogYaxis$ = "YES" THEN PRINT #N&&, "set logscale y"
        IF LogXaxis$ = "YES" AND LogYaxis$ = "YES" THEN PRINT #N&&, "set logscale xy"
        IF xMin$ <> "" AND xMax$ <> "" THEN  PRINT #N&&, "set xrange ["+xMin$+":"+xMax$+"]"
        IF yMin$ <> "" AND yMax$ <> "" THEN  PRINT #N&&, "set yrange ["+yMin$+":"+yMax$+"]"
        PRINT #N&&, "set label "      + Quote$ + Annotation$ + Quote$ + " at graph " + xCoord$ + "," + yCoord$
        PRINT #N&&, "set grid xtics " + XTics$
        PRINT #N&&, "set grid ytics " + yTics$
        PRINT #N&&, "set grid mxtics"
        PRINT #N&&, "set grid mytics"
        PRINT #N&&, "show grid"
        PRINT #N&&, "set title "  + Quote$+PlotTitle$+Quote$
        PRINT #N&&, "set xlabel " + Quote$+XaxisLabel$+Quote$
        PRINT #N&&, "set ylabel " + Quote$+YaxisLabel$+Quote$
        PRINT #N&&, "plot "+Quote$+PlotFileName$+Quote$+" notitle"+LineType$
    CLOSE #N&&
    ProcessID??? = SHELL(GnuPlotEXE$+" cmd2d.gp -")' : CALL Delay(0.5##)
END SUB 'TwoDplot()

'---------------------

SUB CreateGNUplotINIfile(PlotWindowULC_X&&,PlotWindowULC_Y&&,PlotWindowWidth&&,PlotWindowHeight&&)
LOCAL N&&, WinPath$, A$, B$, WindowsDirectory$
    WinPath$ = UCASE$(ENVIRON$("Path"))'DIR$("C:\WINDOWS",23)
    DO
        B$ = A$
        A$ = EXTRACT$(WinPath$,";")
        WinPath$ = REMOVE$(WinPath$,A$+";")
        IF RIGHT$(A$,7) = "WINDOWS" OR A$ = B$ THEN EXIT LOOP
        IF RIGHT$(A$,5) = "WINNT"   OR A$ = B$ THEN EXIT LOOP
    LOOP
    WindowsDirectory$ = A$
    N&& = FREEFILE
'   ----------- WGNUPLOT.INPUT FILE -----------
    OPEN WindowsDirectory$+"\wgnuplot.ini" FOR OUTPUT AS #N&&
        PRINT #N&&,"[WGNUPLOT]"
        PRINT #N&&,"TextOrigin=0 0"
        PRINT #N&&,"TextSize=640 150"
        PRINT #N&&,"TextFont=Terminal,9"
        PRINT #N&&,"GraphOrigin="+REMOVE$(STR$(PlotWindowULC_X&&),ANY" ")+" "+REMOVE$(STR$(PlotWindowULC_Y&&),ANY" ")
        PRINT #N&&,"GraphSize="  +REMOVE$(STR$(PlotWindowWidth&&),ANY" ")+" "+REMOVE$(STR$(PlotWindowHeight&&),ANY" ")
        PRINT #N&&,"GraphFont=Arial,10"
        PRINT #N&&,"GraphColor=1"
        PRINT #N&&,"GraphToTop=1"
        PRINT #N&&,"GraphBackground=255 255 255"
        PRINT #N&&,"Border=0 0 0 0 0"
        PRINT #N&&,"Axis=192 192 192 2 2"
        PRINT #N&&,"Line1=0 0 255 0 0"
        PRINT #N&&,"Line2=0 255 0 0 1"
        PRINT #N&&,"Line3=255 0 0 0 2"
        PRINT #N&&,"Line4=255 0 255 0 3"
        PRINT #N&&,"Line5=0 0 128 0 4"
    CLOSE #N&&
END SUB 'CreateGNUplotINIfile()

'------------------------------------------------ END PROGRAM --------------------------------------------
