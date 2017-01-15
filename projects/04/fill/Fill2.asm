// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input.
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel;
// the screen should remain fully black as long as the key is pressed. 
// When no key is pressed, the program clears the screen, i.e. writes
// "white" in every pixel;
// the screen should remain fully clear as long as no key is pressed.

// Put your code here.

    @isFilled
    M=0

(MAINLOOP)

    @KBD
    D=M
    

    @CLEAR
    D;JEQ   // If (KBD == 0) { goto CLEAR }

    @8192
    D=A
    @i
    M=D  // i = 256 rows of 32 bytes
    @SCREEN 
    D=A     
    @pixel  
    M=D     // pixel = SCREEN

(DRAWLOOP)

    @i
    D=M
    @LOOP
    D;JEQ  // If(i == 0) goto LOOP

    @pixel
    A=M
    M=-1   // *pixel = -1
    
    @pixel
    M=M+1
    @i
    M=M-1
    @DRAWLOOP
    0;JMP

(CLEAR)

    @8192
    D=A
    @i
    M=D  // i = 256 rows of 32 bytes
    @SCREEN 
    D=A     
    @pixel  
    M=D     // pixel = SCREEN
    

(CLEARLOOP)

    @i
    D=M
    @LOOP
    D;JEQ  // If(i == 0) goto LOOP

    @pixel
    A=M
    M=0   // *pixel = -1
    
    @pixel
    M=M+1
    @i
    M=M-1
    @CLEARLOOP
    0;JMP
