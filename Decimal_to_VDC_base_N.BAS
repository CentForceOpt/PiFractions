#COMPILE EXE
#DIM ALL
DEFEXT A-Z

DECLARE FUNCTION DecimalToVanDerCorputBaseN(N&&,Nbase%)

FUNCTION PBMAIN () AS LONG

LOCAL N&&, A$, Nbase%

    A$ = "" : Nbase% = 5

    FOR N&& =  532 TO 532

'N&& = 3 : Nbase% = 3

        A$ = A$ + "N="+STR$(N&&)+"   VDC base "+STR$(Nbase%)+" = "+STR$(DecimalToVanDerCorputBaseN(N&&,Nbase%)) + CHR$(13)

    NEXT N&&

    MSGBOX(A$)

END FUNCTION

'-----------

FUNCTION DecimalToVanDerCorputBaseN(N&&,Nbase%)

'Returns the decimal value of the VDC base Nbase% 'radical inverse function' to create a Low Discrepancy sequence of decimal values.
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

    I&& = N&& : R = 1##/Nbase%

    DO WHILE I&& <> 0 : D&& = I&& MOD Nbase% : S = S + D&&*R : I&& = (I&&-D&&)/Nbase% : R = R/Nbase% : LOOP

ExitVDC:

DecimalToVanDerCorputBaseN = S

END FUNCTION
