'THIS PROGRAM GENERATED THE DATA FOR TABLE 2 IN THE FERMAT PI FRACTION PAPER
'===========================================================================
'Program GASR_PI_FRAC_01-13-2014.BAS.
'Stand-alone Genetic Algorithm with Sibling Rivalry for use in Hybrid
'CFO-GASR Algorithm utilizing Pi Fractions as pseudorandom variables.
'Compiled with Power Basic/Windows Compiler 10.04.0108 (www.PowerBasic.com).
'LAST MOD 01-13-2014 ~0831 HRS EST
'-----------------------------------------------------------------------------
'This program was used to generate data for the arXiv paper posted 01-13-2014.

#COMPILE EXE
#DIM ALL
%USEMACROS = 1
#INCLUDE "Win32API.inc"
DEFEXT A-Z

'----------------------

GLOBAL NumPiFractions&&

GLOBAL PiFractions() AS EXT

GLOBAL RunDataFile$, CheckEarlyTerm$

GLOBAL EulerConst, Pi, Pi2, Pi4, TwoPi, FourPi, FivePi, e, Root2 AS EXT

GLOBAL Alphabet$, Digits$, RunID$, Quote$, SpecialCharacters$

GLOBAL Mu0, Eps0, c, eta0 AS EXT

GLOBAL RadToDeg, DegToRad, Feet2Meters, Meters2Feet, Inches2Meters, Meters2Inches AS EXT

GLOBAL Miles2Meters, Meters2Miles, NautMi2Meters, Meters2NautMi AS EXT

'------------------------------------------------------------------------=--------------------------------------

DECLARE SUB Get_Pi_Fractions

DECLARE FUNCTION RandomIntegerPiFrac%(n%,m%,k&&)

DECLARE FUNCTION RandomNumPiFrac(a,b,k&&)

DECLARE SUB RunGASR(BestFitnessAllRuns,GASRseedChromo(),FunctionName$,Nd%,XiMin(),XiMax(),_
            TotalEvalsThisGroup&&,GAgroupNum%,NumGAgroups%,LastGenNum&)

DECLARE FUNCTION ObjectiveFunction(R(),Nd%,p%,j&,FunctionName$)

DECLARE FUNCTION Colville(R(),Nd%,p%,j&)    'Colville (4-D)

DECLARE FUNCTION Griewank(R(),Nd%,p%,j&)    'Griewank (n-D)

DECLARE FUNCTION Ackley(R(),Nd%,p%,j&)      'Ackley

DECLARE FUNCTION Exponential(R(),Nd%,p%,j&) 'Exponential

DECLARE FUNCTION CosineMix(R(),Nd%,p%,j&)   'Cosine Mix

DECLARE FUNCTION ParrottF4(R(),Nd%,p%,j&)   'Parrott F4 (1D)

DECLARE FUNCTION Schwefel(R(),Nd%,p%,j&)    'Schwefel Problem 2.26 (nD)

DECLARE FUNCTION Rastrigin(R(),Nd%,p%,j&)   'Rastrigin (nD)

DECLARE FUNCTION SGO(R(),Nd%,p%,j&)         'SGO (Space Gravitational Optimization)

DECLARE FUNCTION GP(R(),Nd%,p%,j&)          'Goldstein-Price

DECLARE SUB GA(FunctionName$,Np%,Nd%,NumGenerations&,XiMin(),XiMax(),Chromos(),Fitness(),BestChromoThisRun%,_
               BestGenerationThisRun&,BestFitnessThisRun,NevalThisRun&&,BestFitnessVsGenThisRun(),LastGenNum&)

DECLARE SUB MutateChromo(XiMin(),XiMax(),Chromos(),ChromoNum%,Nd%,GenNum&,NumGenerations&,Alpha,w)

DECLARE SUB MathematicalConstants

DECLARE SUB AlphabetAndDigits

DECLARE SUB SpecialSymbols

DECLARE SUB EMconstants

DECLARE SUB ConversionFactors

'=======================================================================================================

FUNCTION PBMAIN () AS LONG

LOCAL i%, Nd%, m%, TotalEvalsThisGroup&&, TotalEvalsOverAllGroups&&, NumGAgroups%

LOCAL BestFitnessAllRuns, GASRseedChromo(), XiMin(), XiMax() AS EXT

LOCAL FunctionName$, A$, Coords$, LastGenNum&

'---------- Globals ------------
CALL AlphabetAndDigits
CALL MathematicalConstants
CALL SpecialSymbols
CALL EMconstants
CALL ConversionFactors
CALL Get_Pi_Fractions 'fills GLOBAL Pi fraction array

'---------------------------- Test Function ---------------------------------

    CheckEarlyTerm$ = "YES" '"NO"

    FunctionName$   = "Schwefel" '"Rastrigin" '"Griewank" '"Expon" '"CosMix" '"Ackley"

    SELECT CASE FunctionName$
        CASE "SGO"       : Nd% = 2
        CASE "GP"        : Nd% = 2
        CASE "Rastrigin" : Nd% = 30
        CASE "Schwefel"  : Nd% = 30
        CASE "ParrottF4" : Nd% = 1
        CASE "Expon"     : Nd% = 30
        CASE "Ackley"    : Nd% = 30
        CASE "CosMix"    : Nd% = 30
        CASE "Griewank"  : Nd% = 30
        CASE "Colville"  : Nd% = 4
        CASE ELSE : MSGBOX("ERROR in FunctionName$ !!") : EXIT FUNCTION
    END SELECT

    REDIM XiMin(1 TO Nd%), XiMax(1 TO Nd%), GASRseedChromo(1 TO Nd%)

    SELECT CASE FunctionName$
        CASE "SGO"       : FOR i% = 1 TO Nd% : XiMin(i%) =  -50## : XiMax(i%) =  50## : NEXT i% 'DS for SGO
        CASE "GP"        : FOR i% = 1 TO Nd% : XiMin(i%) = -100## : XiMax(i%) = 100## : NEXT i% 'DS for Goldstein-Price
        CASE "Rastrigin" : FOR i% = 1 TO Nd% : XiMin(i%) =  -10## : XiMax(i%) =  10## : NEXT i% 'Rastrigin boundaries from Li A&P paper (note: larger than the usual +/-5.12)
        CASE "Schwefel"  : FOR i% = 1 TO Nd% : XiMin(i%) = -500## : XiMax(i%) = 500## : NEXT i% 'DS for Schwefel 2.26
        CASE "ParrottF4" : FOR i% = 1 TO Nd% : XiMin(i%) =    0## : XiMax(i%) =   1## : NEXT i% 'DS for ParrottF4
        CASE "Expon"     : FOR i% = 1 TO Nd% : XiMin(i%) =   -1## : XiMax(i%) =   1## : NEXT i% 'DS for Exponential
        CASE "Ackley"    : FOR i% = 1 TO Nd% : XiMin(i%) =  -30## : XiMax(i%) =  30## : NEXT i% 'DS for Ackley
        CASE "CosMix"    : FOR i% = 1 TO Nd% : XiMin(i%) =   -1## : XiMax(i%) =   1## : NEXT i% 'DS for Cosine Mix
        CASE "Griewank"  : FOR i% = 1 TO Nd% : XiMin(i%) = -600## : XiMax(i%) = 600## : NEXT i% 'DS for Griewank
        CASE "Colville"  : FOR i% = 1 TO Nd% : XiMin(i%) =  -10## : XiMax(i%) =  10## : NEXT i% 'DS for Colville
    END SELECT

'-------------
    A$                        = ""
    NumGAgroups%              = 1
    TotalEvalsOverAllGroups&& = 0
    FOR m% = 1 TO NumGAgroups% 'NOTE: Option of executing NumGAgroups%>1 groups of independent GA runs as consistency check
                               '      if compiler built-in RND function is used for randomization instead of Pi fractions.
        CALL RunGASR(BestFitnessAllRuns,GASRseedChromo(),FunctionName$,Nd%,XiMin(),XiMax(),TotalEvalsThisGroup&&,m%,_
                     NumGAgroups%,LastGenNum&)
        TotalEvalsOverAllGroups&& = TotalEvalsOverAllGroups&& + TotalEvalsThisGroup&&
        Coords$ = "" : FOR i% = 1 TO Nd% : Coords$ = Coords$+SPACE$(30)+"i ="+STR$(i%)+_
                  ":   "+STR$(ROUND(GASRseedChromo(i%),6)) + $CRLF : NEXT i%
        A$ = A$+"GA group #"+STR$(m%)+": Best Fitness = "+STR$(BestFitnessAllRuns,12)+"  @  coords:"+$CRLF+$CRLF+Coords$
    NEXT m%

    MSGBOX("in PBMAIN, "+REMOVE$(STR$(Nd%),ANY" ")+"D "+FunctionName$+_
                       ": Tot Evals = "+STR$(TotalEvalsOverAllGroups&&)+" / "+STR$(LastGenNum&+1)+" Gens"+$CRLF+$CRLF+_
                       A$+$CRLF+"Output File(s):  "+FunctionName$+"*.DAT")
END FUNCTION 'PBMAIN()

'---------------------

SUB Get_Pi_Fractions

LOCAL p&&, i&&, K&&, m&&, N&&, LineCount&&, idx&&, Lyne$, PiDigits$(), DataFileName$, A$

LOCAL LastDigit&&, ThisDigit&&, NumBins&&, BinNum&&, TotalCount&&, PDF&&(), PDFtotalPoints&&

LOCAL BinMin, BinMax, CDF(), AvgPtsPerBin AS EXT

LOCAL PiFracAvg AS EXT

'NOTE: The user must replace this code to read an external Pi fraction data file that
'      probably has a different name and format than file "BBP_Pi_160K.DAT".

    DataFileName$ = "BBP_Pi_160K.DAT"
    IF DIR$(DataFileName$) = "" THEN
        MSGBOX("ERROR! Data file "+DataFileName$+" not found.  Run terminated.") : END
    END IF

    LineCount&& = 0 : NumPiFractions&& = 0
    N&& = FREEFILE  : OPEN DataFileName$ FOR INPUT AS #N&&
    K&& = FREEFILE  : OPEN "TEMP" FOR OUTPUT AS #K&&
        WHILE NOT EOF(N&&)
            INPUT #N&&, Lyne$ : Linecount&& = Linecount&& + 1
            IF INSTR(Lyne$,"fraction") > 0 THEN
                PRINT #K&&, Lyne$ : NumPiFractions&& = NumPiFractions&& + 1
            END IF
        WEND
    CLOSE K&& : CLOSE #N&&

    REDIM PiDigits$(1 TO NumPiFractions&&), PiFractions(1 TO NumPiFractions&&)

    N&& = FREEFILE : OPEN "TEMP" FOR INPUT AS #N&&
        FOR m&& = 1 TO NumPiFractions&& : INPUT #N&&, Lyne$ : PiDigits$(m&&) = Lyne$ : NEXT m&&
    CLOSE #N&&
    KILL "TEMP"

    PiFracAvg = 0##
    FOR m&& = 1 TO NumPiFractions&&
        PiFractions(m&&) = VAL(REMOVE$(PiDigits$(m&&),ANY Alphabet$+" ="))
        PiFracAvg        = PiFracAvg + PiFractions(m&&)
    NEXT m&&
    PiFracAvg = PiFracAvg/NumPiFractions&&

'   ---------- PDF/CDF ----------

    NumBins&& = 1000 'Compute PDF
    REDIM PDF&&(1 TO NumBins&&),CDF(1 TO NumBins&&)
    FOR BinNum&& = 1 TO NumBins&&
        BinMin = (BinNum&&-1)/NumBins&& : BinMax = BinNum&&/NumBins&& 'Pi fractions are in [0,1]
        FOR m&& = 1 TO NumPiFractions&&
            IF PiFractions(m&&) >= BinMin AND PiFractions(m&&) < BinMax THEN PDF&&(BinNum&&) = PDF&&(BinNum&&) + 1
        NEXT m&&
    NEXT BinNum&&

    PDFtotalPoints&& = 0
    FOR BinNum&& = 1 TO NumBins&& : PDFtotalPoints&& = PDFtotalPoints&& + PDF&&(BinNum&&) : NEXT BinNum&&
    IF PDFtotalPoints&& <> NumPiFractions&& THEN
        MSGBOX("ERROR! Tot PDF points ="+STR$(PDFtotalPoints&&)+" <> # Pi fractions ("+REMOVE$(STR$(NumPiFractions&&),ANY" ")+").")
        END
    END IF
    AvgPtsPerBin = PDFtotalPoints&&/NumBins&&

    CDF(1) = PDF&&(1)/NumPiFractions&&
    FOR BinNum&& = 2 TO NumBins&&  'Compute CDF
        CDF(BinNum&&) = CDF(BinNum&&-1) + PDF&&(BinNum&&)/NumPiFractions&&
    NEXT BinNum&&

    TotalCount&& = 0
    N&& = FREEFILE : OPEN "Pi_Frac_PDF&CDF.DAT" FOR OUTPUT AS #N&&
        PRINT #N&&,"Pi Fraction Statistical Data"+$CRLF+"Created "+DATE$+" "+TIME$+$CRLF
        PRINT #N&&,"#fractions:"+STR$(NumPiFractions&&)+$CRLF+"Mean value: 0"+REMOVE$(STR$(PiFracAvg),ANY" ")+$CRLF
        PRINT #N&&," Norm Bin#    PDF       CDF"
        A$ =       "#.########  #.####   #.########"
        PRINT #N&&, USING$(A$,0,1,0)
        FOR BinNum&& = 1 TO NumBins&&
            TotalCount&& = TotalCount&& + PDF&&(BinNum&&)
            PRINT #N&&, USING$(A$,BinNum&&/NumBins&&,PDF&&(BinNum&&)/AvgPtsPerBin,CDF(BinNum&&))
        NEXT BinNum&&
    CLOSE #N&&
    MSGBOX("File 'Pi_Frac_PDF&CDF.DAT' created."   +$CRLF+_
           "Total Data Points ="+STR$(TotalCount&&)+$CRLF+_
           "Average Value = "   +STR$(PiFracAvg))

END SUB 'Get_Pi_Fractions

'------------------------

SUB RunGASR(BestFitnessAllRuns,GASRseedChromo(),FunctionName$,Nd%,XiMin(),XiMax(),TotalEvalsThisGroup&&,GAgroupNum%,NumGAgroups%,LastGenNum&)

'Creates a Group of Independent GA runs and returns Best Fitness
'and corresponding Chromosome across all runs in all groups.

LOCAL Chromos(), Fitness(), BestFitnessThisRun, BestFitnessVsGenThisRun() AS EXT

LOCAL i%, N%, BestChromoThisRun%, BestGenerationThisRun&, NevalThisRun&&, A$

LOCAL RunNumber%, GenNum&, NumGenerations&, NumChromos%, NumIndependentGArunsPerGroup%

'---------------------------------------------------
NumIndependentGArunsPerGroup% =    1
NumGenerations&               =  100
NumChromos%                   = 2500 'MUST BE EVEN #
'---------------------------------------------------

BestFitnessAllRuns = -1E4200 : TotalEvalsThisGroup&& = 0

REDIM Chromos(1 TO NumChromos%, 1 TO Nd%, 0 TO NumGenerations&),_
      Fitness(1 TO NumChromos%, 0 TO NumGenerations&),_
      BestFitnessVsGenThisRun(0 TO NumGenerations&)

'NOTE: GA generations are numbered starting with ZERO for consistency with the iteration
'      numbering in CFO (anticpating that GA might be more tightly integrated into CFO).

REDIM GASRseedChromo(1 TO Nd%)

FOR RunNumber% = 1 TO NumIndependentGArunsPerGroup%

    CALL GA(FunctionName$,NumChromos%,Nd%,NumGenerations&,XiMin(),XiMax(),Chromos(),Fitness(),_
            BestChromoThisRun%,BestGenerationThisRun&,BestFitnessThisRun,NevalThisRun&&,_
            BestFitnessVsGenThisRun(),LastGenNum&)

    TotalEvalsThisGroup&& = TotalEvalsThisGroup&& + NevalThisRun&&

    IF BestFitnessThisRun >= BestFitnessAllRuns THEN
        BestFitnessAllRuns = BestFitnessThisRun
        FOR i% = 1 TO Nd% : GASRseedChromo(i%) = Chromos(BestChromoThisRun%,i%,BestGenerationThisRun&) : NEXT i%
    END IF

    RunDataFile$ = FunctionName$+"_"+REMOVE$(STR$(Nd%),ANY" ")+"D_GR#"+REMOVE$(STR$(GAgroupNum%),ANY" ")+_
                                 "_Run#"+REMOVE$(STR$(RunNumber%),ANY" ")+".DAT"
    N% = FREEFILE
    OPEN RunDataFile$ FOR OUTPUT AS #N%
        A$ = REMOVE$(STR$(Nd%),ANY" ")+"D "+FunctionName$
        PRINT# N%, A$+$CRLF+STRING$(LEN(A$),"-")                           +$CRLF+_
                  "Run ID: " + RunID$                                      +$CRLF+_
                  "# Groups: "+STR$(NumGAgroups%)                          +$CRLF+_
                  "# GA runs/group: " +STR$(NumIndependentGArunsPerGroup%) +$CRLF+_
                  "# Generations:   " +STR$(NumGenerations&)               +$CRLF+_
                  "# Chromos:       " +STR$(NumChromos%)                   +$CRLF+_
                  "# Eval this run: " +STR$(NevalThisRun&&)                +$CRLF+_
                  "# Gens req'd:    " +STR$(LastGenNum&+1)                 +$CRLF+$CRLF+_
                  "       Gen #         Best Fitness"                      +$CRLF+_
                  "     ---------       ------------"
            A$=   "     ######          ######.#####"
        FOR GenNum& = 0 TO LastGenNum& : PRINT #N%, USING$(A$,GenNum&,BestFitnessVsGenThisRun(GenNum&)) : NEXT GenNum&
    CLOSE #N%

NEXT RunNumber%

END SUB 'RunGASR()

'---------------------------

SUB GA(FunctionName$,NumChromos%,Nd%,NumGenerations&,XiMin(),XiMax(),Chromos(),Fitness(),_
       BestChromoThisRun%,BestGenerationThisRun&,BestFitnessThisRun,NevalThisRun&&,BestFitnessVsGenThisRun(),LastGenNum&)

'GENETIC ALGORITHM WITH SIBLING RIVALRY (Li et al.)
'--------------------------------------------------
'GASR Reference: Li W. T., Shi X. W., Hei Y. Q., Liu S. F., and Zhu J., "A Hybrid Optimization Algorithm and Its Application for
'Conformal Array Pattern Synthesis," IEEE Trans. Ant. & Prop., vol.58, no.10, October 2010, pp.3401-3406.

LOCAL ChromoNum%, i%, GenNum& 'chromo # (a/k/a probe,individual,agent); gene # (a/k/a decision variable, dimension);
                              'generation # (a/k/a time step, iteration) [terminology in other metaheuristics].

LOCAL CrossOverProbability, MutationProbability, w, Alpha, MAXI, MINI, SUM2, BestPreviousFitness, Tol AS EXT

LOCAL b1(), b2(), b3(), b4(), ChildChromoFitness() AS EXT 'child chromos

LOCAL ChildChromoNumbersOrderedByFitness%(), NumberOfBestPreviousChromo%, NumberOfSecondBestPreviousChromo%, ElitistChromoNumber1%

LOCAL L%, M%, s%, t%, k&& 'k&& is Pi Fraction index

LOCAL A$

'---------------------------------------------- Initialize Matrices --------------------------------------------

REDIM Chromos(1 TO NumChromos%, 1 TO Nd%, 0 TO NumGenerations&), Fitness(1 TO NumChromos%, 0 TO NumGenerations&)

REDIM b1(1 TO NumChromos%, 1 TO Nd%, 0 TO NumGenerations&), b2(1 TO NumChromos%, 1 TO Nd%, 0 TO NumGenerations&),_
      b3(1 TO NumChromos%, 1 TO Nd%, 0 TO NumGenerations&), b4(1 TO NumChromos%, 1 TO Nd%, 0 TO NumGenerations&)

REDIM BestFitnessVsGenThisRun(0 TO NumGenerations&)

REDIM ChildChromoNumbersOrderedByFitness%(1 TO 4)

LastGenNum& = NumGenerations&

FOR L% = 1 TO 4 : ChildChromoNumbersOrderedByFitness%(L%) = L% : NEXT L%

'-------------------------------------- Run GA ---------------------------------------

'NOTE: THE FOLLOWING PARAMETERS ARE SET EMPIRICALLY BASED ON GETTING 'GOOD' RESULTS...
CrossOverProbability =  0.8##
MutationProbability  = 0.02##
w                    =  0.5##
Alpha                =    2##
Tol                  = 0.000001## 'for early termination

BestPreviousFitness = -1E4200 : BestFitnessThisRun = -1E4200 : NevalThisRun&& = 0

'STEP (A): CREATE RANDOM INITIAL POPULATION (GENERATION #0) AND COMPUTE INITIAL FITNESSES

    FOR ChromoNum% = 1 TO NumChromos%
        FOR i% = 1 TO Nd%
'IMPORTANT NOTE: If more than one run/group is made and/or more than one group is used, then the run and
'                group numbers should be included in the Pi Fraction index to avoid using the same fractions
'                in successive runs.  Note too that any scheme for varying the index should work well because
'                the Pi Fractions are uniformly distributed in (0,1), but that the scheme should avoid over-
'                lapping too many fractions (if any) in order to avoid creating multiple copies of a single
'                point in the decision space.
            k&& = ChromoNum%*i%
            Chromos(ChromoNum%,i%,0) = RandomNumPiFrac(XiMin(i%),XiMax(i%),k&&)
        NEXT i%
    NEXT ChromoNum%

    FOR ChromoNum% = 1 TO NumChromos%
        Fitness(ChromoNum%,0)     = ObjectiveFunction(Chromos(),Nd%,ChromoNum%,0,FunctionName$) : INCR NevalThisRun&&
        IF Fitness(ChromoNum%,0) >= BestPreviousFitness THEN
               NumberOfBestPreviousChromo% = ChromoNum%
               BestPreviousFitness         = Fitness(ChromoNum%,0)
               BestFitnessVsGenThisRun(0)  = Fitness(ChromoNum%,0)
        END IF
        IF Fitness(ChromoNum%,0)     >= BestFitnessThisRun THEN
               BestChromoThisRun%     = ChromoNum%
               BestGenerationThisRun& = 0
               BestFitnessThisRun     = Fitness(ChromoNum%,0)
        END IF
    NEXT ChromoNum%

FOR GenNum& = 1 TO NumGenerations& 'Loop over generations

    FOR ChromoNum% = 1 TO NumChromos%-1 STEP 2 'Loop over chromos pairwise in each generation

    k&& = ChromoNum%+GenNum&
    IF RandomNumPiFrac(0##,1##,k&&) =< CrossOverProbability THEN 'Create 2 new chromos this gen from children
                                                                 'whose parents are from previous generation.

'STEP (B): SELECT TWO DIFFERENT PARENT CHROMOS FROM _PREVIOUS_ GENERATION
    k&& = ChromoNum%
    DO
        s%  = RandomIntegerPiFrac%(1,NumChromos%,k&&) : t% = RandomIntegerPiFrac%(1,NumChromos%,k&&+2) 's, t are parent chromo #s
        IF s% <> t% THEN EXIT LOOP
        k&& = k&& + 1
    LOOP

'STEP (C): CROSSOVER (MATE) PARENTS TO CREATE FOUR CHILD CHROMOS
    FOR i% = 1 TO Nd%
        MAXI = MAX(Chromos(s%,i%,GenNum&-1),Chromos(t%,i%,GenNum&-1))
        MINI = MIN(Chromos(s%,i%,GenNum&-1),Chromos(t%,i%,GenNum&-1))
        SUM2 = (Chromos(s%,i%,GenNum&-1)+Chromos(t%,i%,GenNum&-1))/2##

        b1(ChromoNum%,i%,GenNum&) = (1##-w) * MAXI      + w*SUM2
        b2(ChromoNum%,i%,GenNum&) = (1##-w) * MINI      + w*SUM2
        b3(ChromoNum%,i%,GenNum&) = (1##-w) * XiMax(i%) + w*MAXI
        b4(ChromoNum%,i%,GenNum&) = (1##-w) * XiMin(i%) + w*MINI
    NEXT i%

'   ------------------------------------ Choose Two Best Child Chromos ---------------------------------------

    REDIM ChildChromoFitness(1 TO 4)
    ChildChromoFitness(1) = ObjectiveFunction(b1(),Nd%,ChromoNum%,GenNum&,FunctionName$) : INCR NevalThisRun&&
    ChildChromoFitness(2) = ObjectiveFunction(b2(),Nd%,ChromoNum%,GenNum&,FunctionName$) : INCR NevalThisRun&&
    ChildChromoFitness(3) = ObjectiveFunction(b3(),Nd%,ChromoNum%,GenNum&,FunctionName$) : INCR NevalThisRun&&
    ChildChromoFitness(4) = ObjectiveFunction(b4(),Nd%,ChromoNum%,GenNum&,FunctionName$) : INCR NevalThisRun&&

    ARRAY SORT ChildChromoFitness(), TAGARRAY ChildChromoNumbersOrderedByFitness%(), DESCEND
   'Array ChildChromoNumbersOrderedByFitness%() contains chromo numbers, best to worst.

'   ----------------------------- Copy Best Child Chromos Into New Chromos in This Generation ---------------------------------

    SELECT CASE ChildChromoNumbersOrderedByFitness%(1) '1st new chromo this gen, #ChromoNum%
        CASE 1 : FOR i% = 1 TO Nd% :  Chromos(ChromoNum%,i%,GenNum&) = b1(ChromoNum%,i%,GenNum&)   : NEXT i% 'best child is b1()
        CASE 2 : FOR i% = 1 TO Nd% :  Chromos(ChromoNum%,i%,GenNum&) = b2(ChromoNum%,i%,GenNum&)   : NEXT i% 'best child is b2()
        CASE 3 : FOR i% = 1 TO Nd% :  Chromos(ChromoNum%,i%,GenNum&) = b3(ChromoNum%,i%,GenNum&)   : NEXT i% 'best child is b3()
        CASE 4 : FOR i% = 1 TO Nd% :  Chromos(ChromoNum%,i%,GenNum&) = b4(ChromoNum%,i%,GenNum&)   : NEXT i% 'best child is b4()
    END SELECT

    SELECT CASE ChildChromoNumbersOrderedByFitness%(2) '2nd new chromo this gen, #ChromoNum%+1
        CASE 1 : FOR i% = 1 TO Nd% :  Chromos(ChromoNum%+1,i%,GenNum&) = b1(ChromoNum%,i%,GenNum&) : NEXT i% '2nd best child is b1()
        CASE 2 : FOR i% = 1 TO Nd% :  Chromos(ChromoNum%+1,i%,GenNum&) = b2(ChromoNum%,i%,GenNum&) : NEXT i% '2nd best child is b2()
        CASE 3 : FOR i% = 1 TO Nd% :  Chromos(ChromoNum%+1,i%,GenNum&) = b3(ChromoNum%,i%,GenNum&) : NEXT i% '2nd best child is b3()
        CASE 4 : FOR i% = 1 TO Nd% :  Chromos(ChromoNum%+1,i%,GenNum&) = b4(ChromoNum%,i%,GenNum&) : NEXT i% '2nd best child is b4()
    END SELECT

ELSE 'If RandomNumPiFrac(0##,1##,k&&)>CrossOverProbability, then copy into this
     'gen with no change the two corresponding chromos from previous generation.

    FOR i% = 1 TO Nd%:  Chromos(ChromoNum%,i%,GenNum&)   = Chromos(ChromoNum%,i%,GenNum&-1)   : NEXT i%
    FOR i% = 1 TO Nd%:  Chromos(ChromoNum%+1,i%,GenNum&) = Chromos(ChromoNum%+1,i%,GenNum&-1) : NEXT i%

END IF 'RandomNumPiFrac(0##,1##,k&&) =< CrossOverProbability

'STEP (D): MUTATE EACH OF THE TWO NEW CHROMOS

    k&& = ChromoNum%+GenNum&
    IF RandomNumPiFrac(0##,1##,k&&) =< MutationProbability THEN 'mutate chromos, otherwise don't.
        CALL MutateChromo(XiMin(),XiMax(),Chromos(),ChromoNum%,Nd%,GenNum&,NumGenerations&,Alpha,w)
        CALL MutateChromo(XiMin(),XiMax(),Chromos(),ChromoNum%+1,Nd%,GenNum&,NumGenerations&,Alpha,w)
    END IF

    NEXT ChromoNum% 'Chromo Loop

'STEP (E): ELITISM - INSERT BEST PREVIOUS CHROMO RANDOMLY INTO THIS GENERATION (MAYBE INSERT TWO BEST??)
    k&&                   = GenNum&
    ElitistChromoNumber1% = RandomIntegerPiFrac%(1,NumChromos%,k&&)
    FOR i% = 1 TO Nd% : Chromos(ElitistChromoNumber1%,i%,GenNum&) = Chromos(NumberOfBestPreviousChromo%,i%,GenNum&-1) : NEXT i%

'STEP (F): COMPUTE FITNESS FOR ALL CHROMOS IN THIS GENERATION

    FOR ChromoNum% = 1 TO NumChromos%

        Fitness(ChromoNum%,GenNum&) = ObjectiveFunction(Chromos(),Nd%,ChromoNum%,GenNum&,FunctionName$) : INCR NevalThisRun&&

        IF Fitness(ChromoNum%,GenNum&) >= BestPreviousFitness THEN
                NumberOfBestPreviousChromo% = ChromoNum% : BestPreviousFitness = Fitness(ChromoNum%,GenNum&)
                BestFitnessVsGenThisRun(GenNum&) = Fitness(ChromoNum%,GenNum&)
                IF CheckEarlyTerm$ = "YES" AND GenNum& > 20 AND GenNum& MOD 5 = 0 AND _
                   BestFitnessVsGenThisRun(GenNum&)-BestFitnessVsGenThisRun(GenNum&-19) =< Tol THEN 'Early termination test.
                   LastGenNum& = GenNum& : EXIT SUB
                END IF
        END IF

        IF Fitness(ChromoNum%,GenNum&) >= BestFitnessThisRun THEN
                BestChromoThisRun%     = ChromoNum%
                BestGenerationThisRun& = GenNum&
                BestFitnessThisRun     = Fitness(ChromoNum%,GenNum&)
        END IF

    NEXT ChromoNum%

NEXT GenNum& 'Generation Loop

END SUB 'GA()
'------------

SUB MutateChromo(XiMin(),XiMax(),Chromos(),ChromoNum%,Nd%,GenNum&,NumGenerations&,Alpha,w)

LOCAL K%, m&&

LOCAL Xk, XkU, XkL, XkStar, Mu AS EXT

    m&& = ChromoNum%+GenNum&
    K%  = RandomIntegerPiFrac%(1,Nd%,m&&) 'randomly select element index K for mutation

    Xk  = Chromos(ChromoNum%,K%,GenNum&) 'Kth element of chromo BEFORE mutation

    Mu  = 1## -(GenNum&/NumGenerations&)^((1##-GenNum&/NumGenerations&)^Alpha) 'Alpha is the "shape parameter"

    XkU = MIN(Xk + Mu*(XiMax(K%)-XiMin(K%))/2##,XiMax(K%))

    XkL = MAX(Xk - Mu*(XiMax(K%)-XiMin(K%))/2##,XiMin(K%))

    XkStar = XkL + w*(XkU-XkL) 'Kth element of chromo AFTER mutation

    Chromos(ChromoNum%,K%,GenNum&) = XkStar 'insert Kth element after mutating it

END SUB 'MutateChromo()

'----------------------

FUNCTION ObjectiveFunction(R(),Nd%,p%,j&,FunctionName$) 'Objective function to be MAXIMIZED is defined here

    SELECT CASE FunctionName$

        CASE "GP"        : ObjectiveFunction = GP(R(),Nd%,p%,j&)          'Goldstein-Price (2D)

        CASE "SGO"       : ObjectiveFunction = SGO(R(),Nd%,p%,j&)         'SGO Function (2D)

        CASE "Rastrigin" : ObjectiveFunction = Rastrigin(R(),Nd%,p%,j&)   'Rastrigin (nD)

        CASE "Schwefel"  : ObjectiveFunction = Schwefel(R(),Nd%,p%,j&)    'Schwefel 2.26 (nD)

        CASE "ParrottF4" : ObjectiveFunction = ParrottF4(R(),Nd%,p%,j&)   'Parrott F4 (1D)

        CASE "Expon"     : ObjectiveFunction = Exponential(R(),Nd%,p%,j&) 'Exponential (30D)

        CASE "Ackley"    : ObjectiveFunction = Ackley(R(),Nd%,p%,j&)      'Ackley (30D)

        CASE "CosMix"    : ObjectiveFunction = CosineMix(R(),Nd%,p%,j&)   'Cosine Mix (30D)

        CASE "Griewank"  : ObjectiveFunction = Griewank(R(),Nd%,p%,j&)    'Griewank (30D)

        CASE "Colville"  : ObjectiveFunction = Colville(R(),Nd%,p%,j&)    'Colville (4D)

    END SELECT

END FUNCTION 'ObjectiveFunction()

'--------------------------------

FUNCTION ParrottF4(R(),Nd%,p%,j&) 'Parrott F4 (1D)

'MAXIMUM = 1 AT ~0.0796875... WITH ZERO OFFSET (SEEMS TO WORK BEST WITH JUST 3 PROBES, BUT NOT ALLOWED IN THIS VERSION...)

'References: (1) Beasley, D., D. R. Bull, and R. R. Martin, “A Sequential Niche Technique for Multimodal
'Function Optimization,” Evol. Comp. (MIT Press), vol. 1, no. 2, 1993, pp. 101-125
'(online at http://citeseer.ist.psu.edu/beasley93sequential.html).

'(2) Parrott, D., and X. Li, “Locating and Tracking Multiple Dynamic Optima by a Particle Swarm
'Model Using Speciation,” IEEE Trans. Evol. Computation, vol. 10, no. 4, Aug. 2006, pp. 440-458.


LOCAL Z, x, offset AS EXT

    offset = 0##

    x = R(p%,1,j&)

    Z = EXP(-2##*LOG(2##)*((x-0.08##-offset)/0.854##)^2)*(SIN(5##*Pi*((x-offset)^0.75##-0.05##)))^6 'WARNING! This is a NATURAL LOG, NOT Log10!!!

    ParrottF4 = Z

END FUNCTION 'ParrottF4()

'------------------------

FUNCTION Schwefel(R(),Nd%,p%,j&) 'Schwefel Problem 2.26 (n-D)

'MAXIMUM = 12,569.5 @ [420.8687]^30 (30-D CASE).

'Reference: Yao, X., Liu, Y., and Lin, G., “Evolutionary Programming Made Faster,”
'IEEE Trans. Evolutionary Computation, Vol. 3, No. 2, 82-102, Jul. 1999.

    LOCAL Z, Xi AS EXT

    LOCAL i%

    Z = 0##

    FOR i% = 1 TO Nd%

        Xi = R(p%,i%,j&)

        Z = Z + Xi*SIN(SQR(ABS(Xi)))

    NEXT i%

    Schwefel = Z - 418.9829##*Nd%

END FUNCTION 'SCHWEFEL226()

'--------------------------

FUNCTION Rastrigin(R(),Nd%,p%,j&) 'Rastrigin (nD)

'MAXIMUM = ZERO (nD CASE).

'Reference: Yao, X., Liu, Y., and Lin, G., “Evolutionary Programming Made Faster,”
'IEEE Trans. Evolutionary Computation, Vol. 3, No. 2, 82-102, Jul. 1999.

    LOCAL Z, Xi AS EXT

    LOCAL i%

    Z = 0##

    FOR i% = 1 TO Nd%

        Xi = R(p%,i%,j&)

        Z  = Z + Xi^2 - 10##*COS(TwoPi*Xi) + 10##

    NEXT i%

    Rastrigin = -Z

END FUNCTION 'Rastrigin

'----------------------

FUNCTION SGO(R(),Nd%,p%,j&)

'SGO Function (2-D)

'MAXIMUM = ~130.8323226... @ ~(-2.8362075...,-2.8362075...) WITH ZERO OFFSET.

'Reference: Hsiao, Y., Chuang, C., Jiang, J., and Chien, C., "A Novel Optimization Algorithm: Space
'Gravitational Optimization," Proc. of 2005 IEEE International Conference on Systems, Man,
'and Cybernetics, 3, 2323-2328. (2005)

LOCAL x1, x2, Z, t1, t2, SGOx1offset, SGOx2offset AS EXT

    SGOx1offset = 0## : SGOx2offset = 0##

    x1 = R(p%,1,j&) - SGOx1offset : x2 = R(p%,2,j&) - SGOx2offset

    t1 = x1^4 - 16##*x1^2 + 0.5##*x1 : t2 = x2^4 - 16##*x2^2 + 0.5##*x2

    Z = t1 + t2

    SGO = -Z 'for maximization

END FUNCTION 'SGO()

'------------------

FUNCTION GP(R(),Nd%,p%,j&)

'MAXIMUM = -3 @ (0,-1) WITH ZERO OFFSET.

'Reference: Cui, Z., Zeng, J., and Sun, G. (2006) ‘A Fast Particle Swarm Optimization,’ Int’l. J.
'Innovative Computing, Information and Control, vol. 2, no. 6, December, pp. 1365-1380.

    LOCAL Z, x1, x2, offset1, offset2, t1, t2 AS EXT

    offset1 = 0## : offset2 = 0##

'    offset1 = 20## : offset2 = -10##

    x1 = R(p%,1,j&)-offset1 : x2 = R(p%,2,j&)-offset2

    t1 = 1##+(x1+x2+1##)^2*(19##-14##*x1+3##*x1^2-14##*x2+6##*x1*x2+3##*x2^2)

    t2 = 30##+(2##*x1-3##*x2)^2*(18##-32##*x1+12##*x1^2+48##*x2-36##*x1*x2+27##*x2^2)

    Z  = t1*t2

    GP = -Z

END FUNCTION 'GP()

'------------------

FUNCTION CosineMix(R(),Nd%,p%,j&) '(n-D)

'MAXIMUM = 0.1*Nd @ [0]^Nd (n-D CASE).

'Reference: Pehlivanoglu, Y. V., "N New Particle Swarm Optimization Method Enhanced With a
'Periodic Mutation Strategy and Neural Networks," IEEE Trans. Evol. Computation,
'Vol. 17, No. 3, June 2013, pp. 436-452.

    LOCAL Z, Xi, Sum1, Sum2 AS EXT

    LOCAL i%

    Z = 0## : Sum1 = 0## : Sum2 = 0##

    FOR i% = 1 TO Nd%

        Xi   = R(p%,i%,j&)

        Sum1 = Sum1 + Xi^2

        Sum2 = Sum2 + COS(FivePi*Xi)

    NEXT i%

    Z = Sum1 - 0.1##*Sum2

    COSINEMIX = -Z

END FUNCTION 'COSINEMIX

'----------------------

FUNCTION Exponential(R(),Nd%,p%,j&) '(n-D)
'MAXIMUM = 1 @ [0]^Nd (n-D CASE).

'Reference: Pehlivanoglu, Y. V., "N New Particel Swarm Optimization Method Enhanced With a
'Periodic Mutation Strategy and Neural Networks," IEEE Trans. Evol. Computation,
'Vol. 17, No. 3, June 2013, pp. 436-452.

    LOCAL Z, Xi, Sum AS EXT

    LOCAL i%

    Z = 0## : Sum = 0##

    FOR i% = 1 TO Nd%

        Xi   = R(p%,i%,j&)

        Sum = Sum + Xi^2

    NEXT i%

    Z = -EXP(-0.5##*Sum)

    EXPONENTIAL = -Z

END FUNCTION 'EXPONENTIAL

'------------------------

FUNCTION Ackley(R(),Nd%,p%,j&) '(n-D)

'MAXIMUM = ZERO (n-D CASE).

'References: (1) Yao, X., Liu, Y., and Lin, G., “Evolutionary Programming Made Faster,”
'IEEE Trans. Evolutionary Computation, Vol. 3, No. 2, 82-102, Jul. 1999.

'(2) Pehlivanoglu, Y. V., "N New Particel Swarm Optimization Method Enhanced With a
'Periodic Mutation Strategy and Neural Networks," IEEE Trans. Evol. Computation,
'Vol. 17, No. 3, June 2013, pp. 436-452.

    LOCAL Z, Xi, Sum1, Sum2 AS EXT

    LOCAL i%

    Z = 0## : Sum1 = 0## : Sum2 = 0##

    FOR i% = 1 TO Nd%

        Xi   = R(p%,i%,j&)

        Sum1 = Sum1 + Xi^2

        Sum2 = Sum2 + COS(TwoPi*Xi)

    NEXT i%

    Z = -20##*EXP(-0.2##*SQR(Sum1/Nd%)) - EXP(Sum2/Nd%) + 20## + e

    ACKLEY = -Z

END FUNCTION 'ACKLEY

'-------------------

FUNCTION Griewank(R(),Nd%,p%,j&) 'Griewank (n-D)

'Max of zero at (0,...,0)

'References: (1) Yao, X., Liu, Y., and Lin, G., “Evolutionary Programming Made Faster,”
'IEEE Trans. Evolutionary Computation, Vol. 3, No. 2, 82-102, Jul. 1999.
'(2) Pehlivanoglu, Y. V., "N New Particel Swarm Optimization Method Enhanced With a
'Periodic Mutation Strategy and Neural Networks," IEEE Trans. Evol. Computation,
'Vol. 17, No. 3, June 2013, pp. 436-452.

    LOCAL Offset, Sum, Prod, Z, Xi AS EXT

    LOCAL i%

    Sum = 0## : Prod = 1##

    Offset = 0## '75.123##

    FOR i% = 1 TO Nd%

        Xi = R(p%,i%,j&) - Offset

        Sum = Sum + Xi^2

        Prod = Prod*COS(Xi/SQR(i%))

    NEXT i%

    Z = Sum/4000## - Prod + 1##

    Griewank = -Z

END FUNCTION 'Griewank()

'--------------------------

FUNCTION Colville(R(),Nd%,p%,j&) 'Colville Function (4-D)

'MAXIMUM = 0 @ (1,1,1,1) WITH ZERO OFFSET.

'Reference: Doo-Hyun, and Se-Young, O., “A New Mutation Rule for Evolutionary Programming Motivated
'from Backpropagation Learning,” IEEE Trans. Evolutionary Computation, Vol. 4, No. 2, pp. 188-190,
'July 2000.

    LOCAL Z, x1, x2, x3, x4, offset AS EXT

    offset = 0## '7.123##

    x1 = R(p%,1,j&)-offset : x2 = R(p%,2,j&)-offset : x3 = R(p%,3,j&)-offset : x4 = R(p%,4,j&)-offset

    Z =  100##*(x2-x1^2)^2 + (1##-x1)^2  + _
          90##*(x4-x3^2)^2 + (1##-x3)^2  + _
        10.1##*((x2-1##)^2 + (x4-1##)^2) + _
        19.8##*(x2-1##)*(x4-1##)

    Colville = -Z

END FUNCTION 'Colville()

'-----------------------

'Note: NumPiFractions&&, PiFractions() are GLOBAL
FUNCTION RandomNumPiFrac(a,b,k&&) 'Returns random number X, a=< X < b.
    RandomNumPiFrac = a + (b-a)*PiFractions(k&&)
END FUNCTION 'RandomNumPiFrac()

'------------------------------

FUNCTION RandomIntegerPiFrac%(n%,m%,k&&) 'Returns random integer I, n=< I =< m.
    RandomIntegerPiFrac% = n% + (m%-n%)*PiFractions(k&&)
END FUNCTION 'RandomIntegerPiFrac%()

'-----------------------------------

SUB MathematicalConstants
    EulerConst = 0.577215664901532860606512##
    Pi         = 3.141592653589793238462643##
    Pi2        = Pi/2##
    Pi4        = Pi/4##
    TwoPi      = 2##*Pi
    FourPi     = 4##*Pi
    FivePi     = 5##*Pi
    e          = 2.718281828459045235360287##
    Root2      = 1.4142135623730950488##
END SUB

'------

SUB AlphabetAndDigits
    Alphabet$ = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
    Digits$   = "0123456789"
    RunID$    = REMOVE$(DATE$+TIME$,ANY" -:/")
END SUB

'------

SUB SpecialSymbols
    Quote$             = CHR$(34) 'Quotation mark "
    SpecialCharacters$ = "'(),#:;/_"
END SUB

'-----

SUB EMconstants
    Mu0  = 4E-7##*Pi     'hy/meter
    Eps0 = 8.854##*1E-12 'fd/meter
    c    = 2.998E8##     'velocity of light, 1##/SQR(Mu0*Eps0) 'meters/sec
    eta0 = SQR(Mu0/Eps0) 'impedance of free space, ohms
END SUB

'------

SUB ConversionFactors
    RadToDeg      = 180##/Pi
    DegToRad      = 1##/RadToDeg
    Feet2Meters   = 0.3048##
    Meters2Feet   = 1##/Feet2Meters
    Inches2Meters = 0.0254##
    Meters2Inches = 1##/Inches2Meters
    Miles2Meters  = 1609.344##
    Meters2Miles  = 1##/Miles2Meters
    NautMi2Meters = 1852##
    Meters2NautMi = 1##/NautMi2Meters
END SUB

'----------------------------- END PROGRAM GASR_PI_FRAC_01-13-2014.BAS -----------------------------
