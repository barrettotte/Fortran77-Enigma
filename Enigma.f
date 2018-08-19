      PROGRAM ENIGMA
      IMPLICIT NONE
C
C ====================================================================
C                        HEADER
C
C  PROGRAM:     M3 / M4 ENGIMA MACHINE EMULATOR
C  AUTHOR:      BARRETT OTTE
C  DATE:        08-12-2018
C  PURPOSE:     SIMULATE ENGIMA MACHINE ENCRYPTION.
C
C  NOTES:  
C    - ATTEMPTED TO STICK AS CLOSE TO F77 RULES AS POSSBILE.
C    - INTRINSIC FUNCTIONS USED:  LEN, MOD
C    - COMPILED ON UBUNTU USING GFORTRAN AND DEFAULT FLAGS.
C    - INPUT IS READ FROM FILE NAMED @INPUT
C    - ENCRYPTION IS OUTPUT TO FILE NAMED @OUTPUT
C
C  LIMITATIONS:
C    - FILE SIZES ONLY UP TO 999 LINES LONG AND X BYTES
C
C ====================================================================
C                       VARIABLES
C 
C  GENERAL
      CHARACTER LBUFF*50, ABCU*26, ABCL*26, CC*1
      CHARACTER RWS(8)*26, REFS(3)*26
      CHARACTER NOTS(8, 2), TRNVS(8, 2)
      INTEGER ROTCP
      INTEGER STAT, NLS, LBL, IC, IDXL
      INTEGER ROFFS(3), SRWS(3)
C  FUNCTIONS
      INTEGER LENGTH, ISLTTR, CHIDX
      CHARACTER TOUPR
C     
C ====================================================================                
C                        VARIABLE DEFINITIONS        
C
C  GENERAL    
C       LBUFF  - LINE BUFFER   
C       CC     - CURRENT CHARACTER           
C       ABCU   - ARRAY OF THE UPPERCASE ALPHABET
C       ABCL   - ARRAY OF THE LOWERCASE ALPHABET
C       RWS    - ARRAY OF ROTOR WIRINGS
C       NOTS   - ARRAY OF ROTOR NOTCHES
C       TRNVS  - ARRAY OF ROTOR TURNOVERS
C       SRWS   - ARRAY OF ROTOR WIRINGS SET
C       REFS   - ARRAY OF REFLECTOR WIRINGS
C       STAT   - FLAG TO CHECK IO STATUS
C       NLS    - NUMBER OF LINES READ IN FILE
C       LBL    - CURRENT LINE BUFFER LENGTH
C       IC     - CHARACTER ITERATOR
C       IDXL   - LETTER INDEX IN ALPHABET
C       ROFFS  - ARRAY OF ROTOR OFFSETS
C       ROTCP  - ROTATION CAP FOR ROTORS
C  FUNCTIONS
C       LENGTH - RETURNS STRING LENGTH IGNORING TRAILING BLANKS
C       ISLTTR - RETURNS 1 IF CHARACTER IS A LETTER, -1 IF NOT
C       CHIDX  - RETURNS IDX OF CHAR IN STRING, -1 IF NOT FOUND
C       TOUPR  - RETURN CHARACTER AS UPPERCASE
C
C ====================================================================
C                       INTIALIZATION
C
C
      ROTCP = 26
      ABCU = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
      ABCL = 'abcdefghijklmnopqrstuvwxyz'
C  ROTOR WIRINGS
      RWS(1) = 'EKMFLGDQVZNTOWYHXUSPAIBRCJ'
      RWS(2) = 'AJDKSIRUXBLHWTMCQGZNPYFVOE'
      RWS(3) = 'BDFHJLCPRTXVZNYEIWGAKMUSQO'
      RWS(4) = 'ESOVPZJAYQUIRHXLNFTGKDCMWB'
      RWS(5) = 'VZBRGITYUPSDNHLXAWMJQOFECK'
      RWS(6) = 'JPGVOUMFYQBENHZRDKASXLICTW'
      RWS(7) = 'NZJHGRCXMYSWBOUFAIVLPEKQDT'
      RWS(8) = 'FKQHTLXOCBJSPDZRAMEWNIUYGV'
C  REFLECTOR WIRINGS
      REFS(1) = 'EJMZALYXVBWFCRQUONTSPIKHGD'
      REFS(2) = 'YRUHQSLDPXNGOKMIEBFZCWVJAT'
      REFS(3) = 'FVPJIAOYEDRZXWGCTKUQSBNMHL'
C  ROTOR NOTCHES
      NOTS(1,1) = 'Q'
      NOTS(2,1) = 'E'
      NOTS(3,1) = 'V'
      NOTS(4,1) = 'J'
      NOTS(5,1) = 'Z'
      NOTS(6,1) = 'Z'
      NOTS(6,2) = 'M'
      NOTS(7,1) = 'Z'
      NOTS(7,2) = 'M'
      NOTS(8,1) = 'Z'
      NOTS(8,2) = 'M'
C  ROTOR TURNOVERS
      TRNVS(1,1) = 'R'
      TRNVS(2,1) = 'F'
      TRNVS(3,1) = 'W'
      TRNVS(4,1) = 'K'
      TRNVS(5,1) = 'A'
      TRNVS(6,1) = 'A'
      TRNVS(6,2) = 'N'
      TRNVS(7,1) = 'A'
      TRNVS(7,2) = 'N'
      TRNVS(8,1) = 'A'
      TRNVS(8,2) = 'N' 
C
C ====================================================================
C                        ENIGMA MACHINE CONFIGURATION
C
C
C  SET ROTORS TO USE IN MACHINE LEFT=1, MID=2, RIGHT=3
      SRWS(1) = 1
      SRWS(2) = 2
      SRWS(3) = 3
C  SET ROTOR'S POSITION OFFSET WHEN PLACED IN MACHINE
      ROFFS(1) = 1
      ROFFS(2) = 1
      ROFFS(3) = 1

C ====================================================================
C                       STARTUP SCREEN
C
C
      PRINT *, '---------------------------------------------------'
      PRINT *, '*                  ENIGMA MACHINE                 *'
      PRINT *, '*                                                 *'
      PRINT *, '*               BARRETT OTTE   2018               *'
      PRINT *, '---------------------------------------------------'
      PRINT *,

      OPEN(UNIT=12, FILE='@INPUT', STATUS='OLD', IOSTAT=STAT)
      IF(STAT.NE.0) THEN
        PRINT *, '>  @INPUT DOES NOT EXIST OR COULD NOT BE OPENED.'
        GOTO 90
      ENDIF

      OPEN(UNIT=13, FILE='@OUTPUT', STATUS='UNKNOWN', IOSTAT=STAT)
      IF(STAT.NE.0) THEN
        PRINT *, '> @OUTPUT DOES NOT EXIST OR COULD NOT BE OPENED.'
        GOTO 90
      ENDIF

      PRINT *, ' '
      PRINT *, '>  INITIALIZATION SUCCESSFUL.'
      PRINT *, ' '
C
C ====================================================================
C                       MAIN
C
      PRINT *,'>  ENCRYPTING [MESSAGE].'
      PRINT *,

      DO
        READ(12, '(A)', END=10) LBUFF
        NLS = NLS + 1
        LBL = LENGTH(LBUFF)

        WRITE(*, '(A50)') LBUFF
C 1     FORMAT(A, I3, A)
C       WRITE(*, 1)' > ', LBL, ' CHARACTERS LONG.'
        
        DO IC = 1,LBL

          CC = LBUFF(IC:IC)
        
          IF(ISLTTR(CC, ABCU, ABCL).EQ.1) THEN
            CC = TOUPR(CC, ABCU, ABCL)
            IDXL = CHIDX(CC, ABCU)
            WRITE(*, '(A,A,A,I2)')' > ', CC, '  ', IDXL

C           CYCLE FIRST ROTOR BEFORE PUSHING THROUGH
            
            ROFFS(1) = ROFFS(1) + 1
            ROFFS(1) = MOD(ROFFS(1), ROTCP)
            



          ELSE
C           WRITE IT TO FILE PLAIN - NO ENCRYPTION          
          ENDIF


        ENDDO
      ENDDO

10    CONTINUE
      PRINT *,
      WRITE(*, 11)' >  ', NLS, ' LINES READ.'
11    FORMAT(A, I0.3, A)

      PRINT *,
      PRINT *, '>  ENCRYPTION WRITTEN TO [ENCRYPTED].'
      GOTO 90
C
C ====================================================================
C                        END
C
C
90    PRINT *, '>  PROGRAM TERMINATED.'
      CLOSE(12)
      CLOSE(13)
      READ(*,*)
      STOP
      END PROGRAM ENIGMA
C
C ====================================================================
C                        LENGTH                    
C     
C  RETURNS LENGTH OF STRING IGNORING TRAILING BLANKSPACES
C
      INTEGER FUNCTION LENGTH(STR)
      CHARACTER*(*) STR
      INTEGER I
      DO 100, I = LEN(STR), 1, -1
        IF(STR(I:I).NE.' ') GOTO 110
100   CONTINUE
110   LENGTH = I
      RETURN
      END FUNCTION LENGTH
C
C ====================================================================
C                        ISLTTR                    
C     
C  RETURNS 1 IF CHARACTER IS A LETTER, -1 IF NOT
C
      INTEGER FUNCTION ISLTTR(CH, ABCU, ABCL)
        CHARACTER*1  CH
        CHARACTER*26 ABCU, ABCL

        DO I = 1, 26
          IF(CH.EQ.ABCU(I:I).OR.CH.EQ.ABCL(I:I)) THEN
            ISLTTR = 1
            RETURN
          ENDIF
        ENDDO
        ISLTTR = -1
        RETURN
      END FUNCTION ISLTTR
C
C ====================================================================
C                        TOUPR
C
C  RETURNS LETTER AS UPPERCASE
C
      CHARACTER FUNCTION TOUPR(L, ABCU, ABCL)
        CHARACTER*(*) L
        CHARACTER*26 ABCU, ABCL

        DO I = 1, 26
          IF(L.EQ.ABCL(I:I)) THEN
            TOUPR = ABCU(I:I)
            RETURN
          ENDIF
        ENDDO
        TOUPR = L
        RETURN
      END FUNCTION TOUPR
C
C ====================================================================
C                        CHIDX
C
C  RETURNS INDEX OF CHARACTER IN STRING, -1 IF NOT FOUND
C
      INTEGER FUNCTION CHIDX(CH, STR)
        CHARACTER*(*) CH
        CHARACTER*(*) STR
        CHIDX = -1
        DO 200, I = LEN(STR), 1, -1
          IF(CH.EQ.STR(I:I)) GOTO 210
200     CONTINUE
        RETURN
210     CHIDX = I
        RETURN
      END FUNCTION CHIDX
C
C ====================================================================