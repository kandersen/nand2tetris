@256
D=A
@SP
M=D
@1
D=A
@ARG
A=M+D
D=M
@SP
M=M+1
A=M-1
M=D
@SP
M=M-1
A=M
D=M
@R4
M=D
@0
D=A
@SP
M=M+1
A=M-1
M=D
@0
D=A
@THAT
D=D+M
@R13
M=D
@SP
M=M-1
A=M
D=M
@R13
A=M
M=D
@1
D=A
@SP
M=M+1
A=M-1
M=D
@1
D=A
@THAT
D=D+M
@R13
M=D
@SP
M=M-1
A=M
D=M
@R13
A=M
M=D
@0
D=A
@ARG
A=M+D
D=M
@SP
M=M+1
A=M-1
M=D
@2
D=A
@SP
M=M+1
A=M-1
M=D
@SP
M=M-1
A=M
D=M
A=A-1
M=M-D
@0
D=A
@ARG
D=D+M
@R13
M=D
@SP
M=M-1
A=M
D=M
@R13
A=M
M=D
(.MAIN_LOOP_START)
@0
D=A
@ARG
A=M+D
D=M
@SP
M=M+1
A=M-1
M=D
@SP
M=M-1
A=M
D=M
@.COMPUTE_ELEMENT
D;JNE
@.END_PROGRAM
0;JMP
(.COMPUTE_ELEMENT)
@0
D=A
@THAT
A=M+D
D=M
@SP
M=M+1
A=M-1
M=D
@1
D=A
@THAT
A=M+D
D=M
@SP
M=M+1
A=M-1
M=D
@SP
M=M-1
A=M
D=M
A=A-1
M=M+D
@2
D=A
@THAT
D=D+M
@R13
M=D
@SP
M=M-1
A=M
D=M
@R13
A=M
M=D
@R4
D=M
@SP
M=M+1
A=M-1
M=D
@1
D=A
@SP
M=M+1
A=M-1
M=D
@SP
M=M-1
A=M
D=M
A=A-1
M=M+D
@SP
M=M-1
A=M
D=M
@R4
M=D
@0
D=A
@ARG
A=M+D
D=M
@SP
M=M+1
A=M-1
M=D
@1
D=A
@SP
M=M+1
A=M-1
M=D
@SP
M=M-1
A=M
D=M
A=A-1
M=M-D
@0
D=A
@ARG
D=D+M
@R13
M=D
@SP
M=M-1
A=M
D=M
@R13
A=M
M=D
@.MAIN_LOOP_START
0;JMP
(.END_PROGRAM)
@_INTERNAL.1
(_INTERNAL.1)
0;JMP
