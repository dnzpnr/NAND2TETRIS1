
@n
M=255
@i
M=0
@SCREEN
D=A
@address
M=D

(LOOP)
@i
D=M
@n
D=D-M
@END
D;JGT

@address
A=M
M=-1
@i
M=M+1
@32
D=A
@address
M=D+M
@LOOP
0;JMP

@END
0;JMP
