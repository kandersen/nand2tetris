//function SimpleFunction.test 2
(SimpleFunction.test)
@0
D=A
@SP
M=M+1
A=M-1
M=D
@SP
M=M+1
A=M-1
M=D
@0
D=A
@LCL
A=M+D
D=M
@SP
M=M+1
A=M-1
M=D
@1
D=A
@LCL
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
@SP
A=M-1
M=!M
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
A=A-1
M=M+D
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
A=A-1
M=M-D
//return
@LCL
D=M
@R13
M=D
@5
D=A
@R13
A=M-D
D=M
@R14
M=D
@SP
M=M-1
A=M
D=M
@ARG
A=M
M=D
@ARG
D=M+1
@SP
M=D
@R13
D=M
@1
A=D-A
D=M
@THAT
M=D
@R13
D=M
@2
A=D-A
D=M
@THIS
M=D
@R13
D=M
@3
A=D-A
D=M
@ARG
M=D
@R13
D=M
@4
A=D-A
D=M
@LCL
M=D
@R14
A=M
0;JMP
