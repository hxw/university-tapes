
;
DECIN:BEGIN
MOV R2,-(R6)
CLR R2
IGNORE:JSR R7,INSYM
CMP R0,#10;NL
BEQ IGNORE
CMP R0,#' ;R6
BEQ IGNORE
CMP R0,#'+
BEQ RDCH
CMP R0,#'-
BNE NRDCH
DEC R2
RDCH:JSR R7,INSYM
NRDCH:CMP R0,#'0
BLT ERROR
CMP R0,#'9
BGT ERROR
MOV R0,R1
REP:SUB #'0,R1
JSR R7,INSYM
CMP R0,#'0
BLT FINISH
CMP R0,#'9
BGT FINISH
ASL R1
MOV R1,-(R6)
ASL R1
ASL R1
ADD (R6)+,R1
ADD R0,R1
BR REP
FINISH:TST R2
BEQ PLUS
NEG R1
PLUS:MOV (R6)+,R2
RTS R7
ERROR:MOV #CON,R1
JSR R7,WTEXT
JMP STOP
CON:="DE
="CI
="N ;R6
="FA
="UL
="T.
END
;
OPRINT:BEGIN
MOV R1,-(R6)
MOV #'0,R0
ROL R1
ADC R0
JSR R7,OUTSYM
MOV #5,-(R6)
REP:ROL R1
ROL R1
ROL R1
ROL R1
MOV R1,R0
ROR R1
BIC #*177770,R0
ADD #'0,R0
JSR R7,OUTSYM
DEC @R6
BNE REP
CLR (R6)+
MOV (R6)+,R1
RTS R7
END
;
DECPTR:BEGIN
MOV R1,-(R6)
MOV #' ,MINUS
CLR F
CLR ZER
MOV R2,-(R6)
TST R1
BPL SKIP
NEG R1
MOV #'-,MINUS
SKIP:MOV #8,R2
LOOP:MOV #'0-1,R0
REP:INC R0
SUB TABLE(R2),R1
BPL REP
ADD TABLE(R2),R1
CMP #'0,R0
BEQ ZROSP
PZRO:TST F
BNE PASS
MOV R0,-(R6)
MOV MINUS,R0
JSR R7,OUTSYM
MOV (R6)+,R0
INC F
PASS:INC ZER
ZSPRI:JSR R7,OUTSYM
SUB #2,R2
BPL LOOP
MOV(R6)+,R2
MOV(R6)+,R1
RTS R7
ZROSP:TST ZER
BNE ZSPRI
PRI:TST R2
BEQ PZRO
MOV #' ,R0
BR ZSPRI
TABLE:=1
= 10
=100
= 1000
=10000
ZER:=0
F:=0
MINUS:=0
END
;
MULT:BEGIN
CLR R0
LB:ROR R2
BCC LA
ADD R1,R0
LA:ASL R1
TST R2
BNE LB
RTS R7
END
;
DIV:BEGIN
MOV R3,-(R6)
MOV R4,-(R6)
MOV R5,-(R6)
MOV #1,R5
MOV #1,-(R6)
TST R1
BPL LA
NEG R1
NEG (R6)
NEG R5
LA:TST R2
BEQ ZERO
BPL LB
NEG R2
NEG R5
LB:CLR R0
MOV #16,R3
CLR R4
LD:ASL R0
ASL R1
ROL R4
CMP R4,R2
BMI LC
SUB R2,R4
INC R0
LC:DEC R3
BNE LD
TST R5
BPL LE
NEG R0
LE:MOV R4,R1
TST (R6)+
BPL LF
NEG R4
LF: MOV (R6)+,R5
MOV (R6)+,R4
MOV (R6)+,R3
RTS R7
ZERO:MOV #ERROR,R1
JSR R7,WTEXT
JMP STOP
ERROR:="DI
="VI
="SI
="ON
=" B
="Y 
="ZE
="RO
=0
END
;
WTEXT:BEGIN
LA:MOVB (R1)+,R0
BEQ LB
JSR R7,OUTSYM
BR LA
LB:RTS R7
END
;
SQRT:BEGIN
MOV R4,-(R6)
MOV R3,-(R6)
MOV R2,-(R6)
MOV R1,-(R6)
MOV R1,R3
BMI NEGA
BEQ ZERO
MOV R1,R2
MOV R1,R4
LOOP:MOV R4,R1
MOV R3,R2
JSR R7,DIV
ADD R3,R0
ASR R0
CMP R3,R0
BLE FINISH
MOV R0,R3
BR LOOP
ZERO:CLR R0
FINISH:MOV R3,R0
MOV (R6)+,R1
MOV (R6)+,R2
MOV (R6)+,R3
MOV (R6)+,R4
RTS R7
NEGA:MOV #ERROR,R1
JSR R7,WTEXT
JMP STOP
ERROR:="SQ
="RT
=" E
="RR
="OR
=0
END
;
