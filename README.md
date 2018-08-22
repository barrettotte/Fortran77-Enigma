# Fortran77-Enigma
Enigma machine emulator in Fortran77. With this project I tried to abide by the rules of Fortran77 the 
best that I could. Such as column number limit and variable naming up to 6 characters.

## Notes
 * Intrinsic Functions used:  LEN, MOD
 *  Compiled on Ubuntu usinng gfortran compiler with default flags.
 * Input is read from file named @INPUT
 * Encryption is output to file named @OUTPUT
 * The code doesn't look great, but Im not going to fix it. This is just a small weekend project.

## Limitations
 * User is expected to correctly populate plugboard (no duplicate keys).
 * The max file size has not been tested, but its probably pretty small.


## Diagram of Enigma Machine
![enigma](http://enigma.louisedade.co.uk/wiringdiagram.png)

From http://enigma.louisedade.co.uk/howitworks.html


## Sources Used
* Enigma emulator with mechanics explanation http://enigma.louisedade.co.uk/howitworks.html
* Details of enigma machine rotors https://en.wikipedia.org/wiki/Enigma_rotor_details
* Detailed information on engima machine https://en.wikipedia.org/wiki/Enigma_machine
* Oracle docs on Fortran77 https://docs.oracle.com/cd/E19957-01/805-4939/index.html
* Old Fortran77 book that was an amazing reference [amazon](https://www.amazon.com/Fortran-77-Numerical-Methods-Engineers/dp/0534925626/ref=sr_1_sc_3?)
