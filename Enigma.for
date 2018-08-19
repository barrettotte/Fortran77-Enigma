        PROGRAM ENIGMA
C ======================================================
C       PROGRAM:  M3 / M4 ENGIMA MACHINE EMULATOR
C       AUTHOR: 			BARRETT OTTE
C       DATE:				08-12-2018
C       DESCRIPTION:  
C           - SIMULATE  ENGIMA MACHINE ENCRYPTION
C           - SUPPORTS FILES UP TO 5242880 BYTES (5MB)
C

C ======================================================
C                                            VARIABLES
C 
C
        CHARACTER*25 OBUFF, NBUFF, OBUFF2
        CHARACTER CC*1, ABCSU*26, ABCSL*26
        CHARACTER ROTS(8)*26
        INTEGER NOTS(8, 2), TRNS(8, 2), SROTS(3)
        CHARACTER REFS(3)*26
		
C      OBUFF - ORIGINAL BUFFER         
C      NBUFF - NEW BUFFER
C      CC - CURRENT CHARACTER           
C      ABCSL - LOWERCASE ALPHABET
C      ABCSU - UPPERCASE ALPHABET
C      SROTS - SET ROTORS
C      ROTS - ROTOR WIRINGS
C      NOTS - ROTOR NOTCHES            
C      TRNS - ROTOR TURNOVERS
C      REFS - REFLECTOR WIRINGS

C ======================================================
C                                         INTIALIZATION
C
C
        ITER = 0
        ABCSL = 'abcdefghijklmnopqrstuvwxyz'
        ABCSU = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
C                                    ROTOR WIRINGS
        ROTS(1) = 'EKMFLGDQVZNTOWYHXUSPAIBRCJ'
        ROTS(2) = 'AJDKSIRUXBLHWTMCQGZNPYFVOE'
        ROTS(3) = 'BDFHJLCPRTXVZNYEIWGAKMUSQO'
        ROTS(4) = 'ESOVPZJAYQUIRHXLNFTGKDCMWB'
        ROTS(5) = 'VZBRGITYUPSDNHLXAWMJQOFECK'
        ROTS(6) = 'JPGVOUMFYQBENHZRDKASXLICTW'
        ROTS(7) = 'NZJHGRCXMYSWBOUFAIVLPEKQDT'
        ROTS(8) = 'FKQHTLXOCBJSPDZRAMEWNIUYGV'
C                                  REFLECTOR WIRINGS
        REFS(1) = 'EJMZALYXVBWFCRQUONTSPIKHGD'
        REFS(2) = 'YRUHQSLDPXNGOKMIEBFZCWVJAT'
        REFS(3) = 'FVPJIAOYEDRZXWGCTKUQSBNMHL'
		
C ======================================================
C                                             SETTINGS
C
C
        SROTS(1) = 1
        SROTS(2) = 2
        SROTS(3) = 3
		
C ======================================================
C                                         STARTUP SCREEN
C
C
        WRITE(*,*) '---------------------------------------------------'
        WRITE(*,*) '*                  ENIGMA MACHINE                 *'
        WRITE(*,*) '*                                                 *'
        WRITE(*,*) '*               BARRETT OTTE   2018               *'
        WRITE(*,*) '---------------------------------------------------'
        WRITE(*,*)
        OPEN(UNIT=12, FILE='@MESSAGE', ACCESS='SEQUENTIAL')
        OPEN(UNIT=13, FILE='@ENCRYPTED')
        WRITE(*,*) ' '
        WRITE(*,*) '>  INITIALIZATION SUCCESSFUL.'
        WRITE(*,*) ' '
        
        GOTO 10
		
C ======================================================
C                                                    MAIN
C
C
10    WRITE(*,*) '>  ENCRYPTING [MESSAGE]'
        READ(12, 11) OBUFF
        READ(12, *) OBUFF2
        WRITE(*,*) OBUFF
        WRITE(*,*) OBUFF2
        WRITE(*,*)

11      FORMAT(A80)
		
        DO IC = 1,25
		    CC = OBUFF(IC:IC)
            DO IA = 0, 26
			    IF(CC .EQ. ABCSL(IA:IA)) THEN
				    WRITE(*,*) IC, '  :  ', CC,  '  IS A LOWERCASE LETTER.'
                ELSEIF(CC .EQ. ABCSU(IA:IA)) THEN
				    WRITE(*,*) IC, '  :  ', CC,  '  IS AN UPPERCASE LETTER.'
                ENDIF
            END DO
        END DO
		
		
        WRITE(*,*)
        WRITE(*,*) '>  ENCRYPTION WRITTEN TO [ENCRYPTED]'
        CLOSE(12)
        CLOSE(13)
        GOTO 99
C ======================================================
C                                                     END
C
C
99    WRITE(*,*) '>  PROGRAM TERMINATED.'
        READ(*,*)
        STOP
        END
		
		
		
		
		
		
		
		

