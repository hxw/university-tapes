
IN SHED
?***A JOB DX36 AAA #46
       SAL
       LPON       NUMPRG=4               ;DATA INITIALISATION
       EVENTS=16
       PRGSTP=4
       STALEN=NUMPRG+NUMPRG+NUMPRG+NUMPRG
       FIN=2                  ;)RUN CONDITIONS IN STATUS WORD
       WAI=1                  ;)0=GO 1=STOPED 2=WAITING
       LEN=256
   ST0:.=.+LEN                ;STACKS ST1..ST4 FOR PROGRAMS
   ST1:.=.+LEN                ;       ST0      FOR SHEDULER
   ST2:.=.+LEN
   ST3:.=.+LEN
   ST4:=0
                              ;
 SIGNL:.=.+EVENTS             ;BYTES HOLDING SIGNALED CONDITIONS
STATUS:.=.+STALEN             ;STATUS 2 WORDS FOR EACH PROG

                              ;SP=2 BYTES, WAIT EVENT & RUN 1BYTE EACH
 PRGNO:=0
                              ;
 TABLE:=ST1                   ;TABLE OF STACK AND PROGRAM ADDRESSES
       =PROG1
       =ST2
       =PROG2
       =ST3
       =PROG3
       =ST4
       =PROG4
                              ;
  STA2=STATUS+2
  STA3=STATUS+3
                              ;
  INIT:MOV  #TABLE,R0         ;INITIALISE START ADDRESS,SP,STATUS & REGS
       MOV  #STATUS,R1        ;FOR EACH PROGRAM
       MOV  #SIGNL,R2
       MOV  #NUMPRG,R3
    L1:MOV  #6,R4
       MOV  (R0)+,R5
       MOV  (R0)+,-(R5)       ;SET START ADDRESS
    L2:CLR  -(R5)             ;)SET 6 ZEROS - FOR REGS
       SOB  R4,L2             ;)
       MOV  R5,(R1)+          ;SET SP
       CLR  (R1)+             ;CLEAR STATUS
       SOB  R3,L1
       MOV  #EVENTS,R0        ;CLEAR SIGNALS
    L3:CLRB (R2)+
       SOB  R0,L3
       CLR  PRGNO
       BR   EXIT
                              ;
  EXIT:MOV  PRGNO,R0          ;RESTORE REGISTERS & RETURN TO PROGRAM
       MOV  STATUS(R0),R6     ;#PRGNO
       MOV  (R6)+,R5
       MOV  (R6)+,R4
       MOV  (R6)+,R3
       MOV  (R6)+,R2
       MOV  (R6)+,R1
       MOV  (R6)+,R0
       RTS  R7
                              ;
  WAIT:BEGIN
       BIC  #-EVENTS,R0
       TSTB SIGNL(R0)          ;)IF SIGNAL HAS ALREADY OCCURED
       BEQ  STK                ;)DO NOT SUSPEND PROGRAM
       CLRB SIGNL(R0)          ;)
       RTS  R7                 ;)
   STK:MOV  R0,-(R6)           ;SAVE REGISTERS
       MOV  R1,-(R6)
       MOV  R2,-(R6)
       MOV  R3,-(R6)
       MOV  R4,-(R6)
       MOV  R5,-(R6)
       MOV  PRGNO,R1           ;)SAVE PC
       MOV  R6,STATUS(R1)      ;)     SP
       INCB STA3(R1)           ;)SET PROGRAM AS WAITING
       MOVB R0,STA2(R1)        ;)SET EVENT NUMBER
  LOOP:ADD  #PRGSTP,PRGNO      ;FIND A PROGRAM WHICH IS READY
       CMP  PRGNO,#STALEN
       BNE  NXPRG
       CLR  PRGNO
 NXPRG:MOV  PRGNO,R1           ;PROGRAM FOUND READY
       TSTB STA3(R1)
       BNE  LOOP
       BR   EXIT
       END
                               ;
SIGNAL:BEGIN
       BIC  #-EVENTS,R0
       MOV  R0,-(R6)          ;SAVE REGISTERS
       MOV  R1,-(R6)
       MOV  R2,-(R6)
       MOV  R3,-(R6)
       MOV  R4,-(R6)
       MOV  R5,-(R6)
       INCB SIGNL(R0)         ;SET SIGNAL
       MOV  PRGNO,R1
       MOV  R6,STATUS(R1)
       MOV  #NUMPRG,R2
  LOOP:CMPB R0,STA2(R1)       ;SEARCH FOR A PROGRAM WAITING FOR EVENT
       BNE  NOCH
       CMPB #FIN,STA3(R1)
       BNE  CHANGE
  NOCH:ADD  #PRGSTP,R1
       CMP  R1,#STALEN
       BNE  NXPROG
       CLR  R1
NXPROG:SOB  R2,LOOP
       BR   EXIT
CHANGE:CLRB SIGNL(R0)         ;IF A PROGRAM IS WAITING CLEAR SIGNAL
       CLR  STA2(R1)          ;& SET IT TO A READY STATE
       BR   EXIT              ;RETURN TO ORIGINAL PROGRAM
       END                    ;          ========
                              ;
FINISH:BEGIN
       MOV  PRGNO,R1          ;)
       MOVB #FIN,STA3(R1)     ;)SET PROGRAM AS FINISHED
       CLRB STA2(R1)          ;)
       MOV  #NUMPRG,R0
  LOOP:TSTB STA3(R1)          ;FIND A PROGRAM WHICH IS READY
       BEQ  PRGO
       ADD  #PRGSTP,R1
       CMP  R1,#STALEN
       BNE  NXPROG
       CLR  R1
NXPROG:SOB  R0,LOOP           ;)ALLPROGRAMS FINISHED - STOP
       JMP  STOP              ;)
  PRGO:MOV  R1,PRGNO          ; )RETURN TO READY PROGRAM
       BR   EXIT              ; )
       END
                              ;
                              ;
?