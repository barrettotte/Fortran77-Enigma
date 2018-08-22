      PROGRAM ENIGMA
      IMPLICIT NONE
C
C ====================================================================
C                       HEADER
C
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
C    - A LOT OF THE CODE LOOKS BAD, BUT HEY I DONT REALLY CARE.
C
C  LIMITATIONS:
C    - USER IS EXPECTED TO CORRECTLY POPULATE PLUGBOARD (NO DUPS).
C    - THE MAX FILE SIZE HAS NOT BEEN TESTED, BUT ITS PROBABLY SMALL.
C
C ====================================================================
C                       VARIABLES
C 
C
C  GENERAL
      CHARACTER    LBUFF*75, CC, TMP
      CHARACTER*26 ABCU, ABCL, PLGBRD
      CHARACTER    RWS(8)*26, REFS(3)*26
      CHARACTER    NOTS(8, 2), TRNVS(8, 2), CHKVLS(2)
      INTEGER      SREF, STAT, LBL, IC, IR, CI
      INTEGER      ROFFS(3), SRWS(3), NDSTRN(3)
C  FUNCTIONS
      INTEGER      LENGTH, CHIDX, ISLTTR
      CHARACTER    TOUPR
C     
C ====================================================================                
C                       VARIABLE DEFINITIONS        
C
C
C  GENERAL    
C       LBUFF  - LINE BUFFER
C       CC     - CURRENT CHARACTER
C       TMP    - TEMP VARIABLE FOR CALCULATION
C       PLGBRD - WIRING OF KEYBOARD TO PLUGBOARD
C       ABCU   - ALPHABET IN UPPERCASE
C       ABCL   - ALPHABET IN LOWERCASE
C       RWS    - WIRING OF ROTORS
C       REFS   - WIRING OF REFLECTORS
C       NOTS   - NOTCH POSITIONS
C       TRNVS  - TURNOVER POSITIONS
C       CHKVLS - TEMPS FOR CHECKING TURNOVER AND NOTCH POSITIONS
C       SREF   - REFLECTOR SET IN CONFIGURATION
C       STAT   - CHECK IF I/O ERROR OCCURRED OPENING FILES
C       LBL    - LINE BUFFER LENGTH
C       IC     - CHARACTER ITERATOR
C       IR     - ROTOR ITERATOR
C       CI     - CHARACTER INDEX
C       ROFFS  - ROTOR OFFSETS
C       SRWS   - ROTOR WIRINGS SET IN CONFIGURATION
C       NDSTRN - TRACK IF ROTOR NEEDS TO TURNOVER
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
      NOTS(1,2) = '!'
      NOTS(2,1) = 'E'
      NOTS(2,2) = '!'
      NOTS(3,1) = 'V'
      NOTS(3,2) = '!'
      NOTS(4,1) = 'J'
      NOTS(4,2) = '!'
      NOTS(5,1) = 'Z'
      NOTS(5,2) = '!'
      NOTS(6,1) = 'Z'
      NOTS(6,2) = 'M'
      NOTS(7,1) = 'Z'
      NOTS(7,2) = 'M'
      NOTS(8,1) = 'Z'
      NOTS(8,2) = 'M'
C  ROTOR TURNOVERS
      TRNVS(1,1) = 'R'
      TRNVS(1,2) = '!'
      TRNVS(2,1) = 'F'
      TRNVS(2,2) = '!'
      TRNVS(3,1) = 'W'
      TRNVS(3,2) = '!'
      TRNVS(4,1) = 'K'
      TRNVS(4,2) = '!'
      TRNVS(5,1) = 'A'
      TRNVS(5,2) = '!'
      TRNVS(6,1) = 'A'
      TRNVS(6,2) = 'N'
      TRNVS(7,1) = 'A'
      TRNVS(7,2) = 'N'
      TRNVS(8,1) = 'A'
      TRNVS(8,2) = 'N' 
C
C ====================================================================
C                       SET MACHINE CONFIGURATION
C
C
C  SET PLUGBOARD, DO NOT SET DUPLICATE KEY-PAIRS
      PLGBRD = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
C  SET ROTORS TO USE IN MACHINE LEFT=1, MID=2, RIGHT=3
      SRWS(1) = 1
      SRWS(2) = 2
      SRWS(3) = 3
C  SET ROTOR'S POSITION OFFSET WHEN PLACED IN MACHINE
      ROFFS(1) = 3
      ROFFS(2) = 2
      ROFFS(3) = 1
C  SET REFLECTOR TO BE USED IN MACHINE
      SREF = 1
C
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
C
      PRINT *,'>  ENCRYPTING [MESSAGE].'
      PRINT *,
      PRINT *, '---------------------------------------------------'
      DO
C  GET CHARACTER FROM KEYBOARD
        READ(12,'(A)',END=10) LBUFF
        LBL = LENGTH(LBUFF)
        DO IC=1,LBL,1
          PRINT *,
          CC = LBUFF(IC:IC)
          WRITE(*,*) '>  ORIGINAL  - ', CC
          IF(ISLTTR(CC, ABCU, ABCL).EQ.1) THEN
            CC = TOUPR(CC, ABCU, ABCL)
            CI = CHIDX(CC, ABCU)
C  CYCLE FIRST ROTOR, CHECKING IF TURNOVER NEEDED
            ROFFS(1) = ROFFS(1) + 1
            ROFFS(1) = MOD(ROFFS(1), 26) 
            CHKVLS(1) = TRNVS(SRWS(1),1)
            CHKVLS(2) = TRNVS(SRWS(1),2)
            TMP = RWS(SRWS(1))(ROFFS(1):ROFFS(1))
            IF(TMP.EQ.CHKVLS(1).OR.TMP.EQ.CHKVLS(2)) THEN
              NDSTRN(1) = 1
            ENDIF
C  DOUBLE STEP MIDDLE ROTOR, CHECKING IF NOTCH ACTIVATED
            CHKVLS(1) = NOTS(SRWS(2),1)
            CHKVLS(2) = NOTS(SRWS(2),2)
            TMP = RWS(SRWS(2))(ROFFS(2):ROFFS(2))
            IF(TMP.EQ.CHKVLS(1).OR.TMP.EQ.CHKVLS(2)) THEN
              ROFFS(2) = ROFFS(2) + 1
              ROFFS(2) = MOD(ROFFS(2), 26)
              CHKVLS(1) = TRNVS(SRWS(2),1)
              CHKVLS(2) = TRNVS(SRWS(2),2)
              TMP = RWS(SRWS(2))(ROFFS(2):ROFFS(2))
              IF(TMP.EQ.CHKVLS(1).OR.TMP.EQ.CHKVLS(2)) THEN
                NDSTRN(2) = 1
              ENDIF
            ENDIF
C  STEP ROTOR 1 AND 2, CHECKING IF ANY TURNOVERS NEEDED
            DO IR=1,2,1
              CC = PLGBRD(ROFFS(IR):ROFFS(IR))
              IF(NDSTRN(IR).EQ.1) THEN
                NDSTRN(IR) = 0
                ROFFS(IR+1) = ROFFS(IR+1) + 1
                ROFFS(IR+1) = MOD(ROFFS(IR+1),26)
                CHKVLS(1) = NOTS(SRWS(IR+1),1)
                CHKVLS(2) = NOTS(SRWS(IR+1),2)
                TMP = RWS(SRWS(IR+1))(ROFFS(IR+1):ROFFS(IR+1))
                IF(TMP.EQ.CHKVLS(IR+1).OR.TMP.EQ.CHKVLS(IR+1)) THEN
                  NDSTRN(IR+1) = 1
                ENDIF
              ENDIF
            ENDDO
C  PASS THROUGH ALL ROTORS FORWARD
            DO IR=1,3,1
              CI = MOD(CI + ROFFS(IR), 26)
              CI = CHIDX(RWS(IR)(CI:CI), PLGBRD)
              CI = MOD(26 + CI - ROFFS(IR), 26)
            ENDDO
C  PASS THROUGH REFLECTOR
            CC = REFS(SREF)(CI:CI)
            CI = CHIDX(CC,PLGBRD)
C  PASS THROUGH ALL ROTORS BACKWARD
            DO IR=3,1,-1
              CI = MOD((CI + ROFFS(IR)), 26)
              CI = CHIDX(ABCU(CI:CI), RWS(SRWS(IR)))
              CI = MOD(26 + CI - ROFFS(IR), 26)
            ENDDO
            CC = ABCU(CI:CI)
          ENDIF
          WRITE(*,*)'>  ENCRYPTED - ', CC
          WRITE(13, '(A)', ADVANCE='NO') CC
        ENDDO
        WRITE(13,*)
      ENDDO
10    CONTINUE
      PRINT *,
11    FORMAT(A, I0.3, A)
      PRINT *, '---------------------------------------------------'
      PRINT *,
      PRINT *, '>  ENCRYPTION WRITTEN TO [OUTPUT].'
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
C
C  RETURNS 1 IF CHARACTER IS A LETTER, -1 IF NOT
C
      INTEGER FUNCTION ISLTTR(CH, ABCU, ABCL)
        CHARACTER*1  CH
        CHARACTER*26 ABCU, ABCL
        INTEGER I
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
C
C  RETURNS LETTER AS UPPERCASE
C
      CHARACTER FUNCTION TOUPR(L, ABCU, ABCL)
        CHARACTER*(*) L
        CHARACTER*26 ABCU, ABCL
        INTEGER I
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
C
C  RETURNS INDEX OF CHARACTER IN STRING, -1 IF NOT FOUND
C
      INTEGER FUNCTION CHIDX(CH, STR)
        CHARACTER*(*) CH
        CHARACTER*(*) STR
        INTEGER I
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