#COMPILE EXE
#DIM ALL

DEFEXT A-Z

'------------------------------------

DECLARE FUNCTION Halton(Index&,NBase&)

FUNCTION PBMAIN () AS LONG

LOCAL Idx&, NBase&, A$

LOCAL Denom AS EXT

    NBase& = 2 : A$ = ""

    Denom = 1##/64 'for checking only

    FOR Idx& = 1 TO 70

        A$ = A$ + STR$(Halton(Idx&,NBase&)) + "         " + STR$(Halton(Idx&,NBase&)/Denom) + CHR$(13)

    NEXT Idx&

    MSGBOX(A$)


END FUNCTION

'-----------

FUNCTION Halton(Index&,NBase&)

'Written 06-10-2012, raf
'See Wiki reference below.

'Returns a Halton sequence of numbers on [0,1] computed from NBase& (inetger base).
'Index& is which number in the sequence is returned.

LOCAL Z, ratio AS EXT

LOCAL D&, I&

    I& = Index&

    Z = 0##
    ratio = 1##/NBase&

    DO WHILE I& <> 0
        D&    = I& MOD NBase&
        Z     = Z + D&*ratio
        I&    = (I&-D&)/NBase&
        ratio = ratio/NBase&
    LOOP

    Halton = Z

END FUNCTION

'REF: Wikipedia, the free encyclopedia: http://en.wikipedia.org/wiki/Halton_sequence

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
