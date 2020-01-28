IN POST
!***A JOB DX36 TZD #41
       SAL
       LPOFF
                               ;
                               ;
       ARROW='^
       TIMES='*
       DIVIDE='/
       PLUS='+
       MINUS='-
       UNPLUS='&
       UNMIN='$
       OPENB='(
       CLOSEB=')
       SPACE=*40
       LF=10
                               ;
                               ;
       .=1000
                               ;
                               ;
 STACK:=0
                               ;
                               ;
PRIORI:BEGIN                   ;PRIORITY OF CHAR IN R0 TO R0
       MOV R1,-(R6)
       MOV #TABLE,R1
       BIC #*177600,R0
    L1:CMP R0,(R1)+
       BEQ L2
       ADD #2,R1
       BR L1
    L2:MOV (R1),R0
       MOV (R6)+,R1
       RTS R7
 TABLE:=UNPLUS
       =4
       =UNMIN
       =4
       =ARROW
       =3
       =TIMES
       =2
       =DIVIDE
       =2
       =PLUS
       =1
       =MINUS
       =1
       =OPENB
       =0
       =LF
       =-1
       END
                               ;
                               ;
OPTEST:BEGIN                   ;TEST IF OPERATOR VALID , IF NOT SET CY
       MOV R1,-(R6)
       MOV #TABLE,R1
       CLC
    L1:CMP R0,(R1)+
       BEQ L2
       TST (R1)
       BNE L1
       SEC
    L2:MOV (R6)+,R1
       RTS R7
 TABLE:=ARROW
       =TIMES
       =DIVIDE
       =PLUS
       =MINUS
       =0
       END
                               ;
                               ;
POLISH:BEGIN                   ;R2 ^ INPUT BUFFER; R3 ^ OUTPUT BUFFER
       MOV R1,-(R6)            ;R1 IS THE UNARY OP FLAG
       MOV R4,-(R6)            ;R4 IS THE STACK POINTER
       MOV R5,-(R6)            ;R5 IS THE LETTER FLAG
       MOV #STACK,R4
       MOV #LF,-(R4)
       CLR R5
       CLR R1
       CLR OPFLAG
   ONE:MOVB (R2)+,R0
       BIC #*177600,R0
       CMP R0,#'A
       BLT NOTLET
       CMP R0,#'Z
       BLE LETTER
NOTLET:CMP R0,#OPENB
       BEQ OPBR
       CMP R0,#CLOSEB
       BEQ CLBR
       CMP R0,#LF
       BEQ FINISH
       JSR R7,OPTEST
       BCS ERROR1
       INC OPFLAG
       TST R5                  ;DEAL WITH OPERATOR
       BNE NOTST
       CMP R0,#PLUS            ; SECTION TO CONVERT UNARY OPS
       BEQ PL
       CMP R0,#MINUS
       BNE ERROR2
       MOV #UNMIN,R0
       BR UNTST
    PL:MOV #UNPLUS,R0
 UNTST:TST R1
       BNE ERROR8
       INC R1
 NOTST:CLR R5
       MOV R0,-(R6)
       JSR R7,PRIORI
       MOV R0,-(R6)
PRLOOP:MOV (R4),R0
       JSR R7,PRIORI
       CMP R0,(R6)
       BLT PRFIN
       MOV (R4)+,R0
       MOVB R0,(R3)+
       BR PRLOOP
 PRFIN:CLR (R6)+
       MOV (R6)+,-(R4)
       BR ONE
LETTER:TST R5                  ;OPERANDS
       BNE ERROR3
       MOVB R0,(R3)+
       INC R5
       CLR R1
       CLR OPFLAG
       BR ONE
  OPBR:TST R5                  ;OPEN BRACKETS
       BNE ERROR4
       CLR R1
       MOV R0,-(R4)
       BR ONE
  CLBR:TST R5                  ;CLOSE BRACKETS
       BEQ ERROR5
BRLOOP:CMP (R4),#OPENB
       BEQ BREND
       CMP R4,#STACK
       BEQ ERROR6
       MOV (R4)+,R0
       MOVB R0,(R3)+
       BR BRLOOP
 BREND:CLR(R4)+
       BR ONE
FINISH:TST OPFLAG              ;LINE FEED
       BNE ERROR9
       CMP R4,#STACK
       BEQ EXIT
       CMP (R4),#OPENB
       BEQ ERROR7
       MOV (R4)+,R0
       MOVB R0,(R3)+
       BR FINISH
  EXIT:MOV (R6)+,R5
       MOV (R6)+,R4
       MOV (R6)+,R1
       CLC
       RTS R7
ERROR1:MOV #'1,R0              ;ERROR SECTION
       BR ERROR
ERROR2:MOV #'2,R0
       BR ERROR
ERROR3:MOV #'3,R0
       BR ERROR
ERROR4:MOV #'4,R0
       BR ERROR
ERROR5:MOV #'5,R0
       BR ERROR
ERROR6:MOV #'6,R0
       BR ERROR
ERROR7:MOV #'7,R0
       BR ERROR
ERROR8:MOV #'8,R0
       BR ERROR
ERROR9:MOV #'9,R0
       BR ERROR
 ERROR:MOV R0,-(R6)
       MOV R1,-(R6)
       MOV #DATA,R1
  LOOP:MOVB (R1)+,R0
       CMP R0,#ARROW
       BEQ EREXIT
       JSR R7,OUTSYM
       BR LOOP
EREXIT:MOV (R6)+,R1
       MOV (R6)+,R0
       JSR R7,OUTSYM
       MOV #LF,R0
       JSR R7,OUTSYM
       MOV (R6)+,R5
       MOV (R6)+,R4
       MOV (R6)+,R1
       SEC
       RTS R7
  DATA:="ER
       ="RO
       ="R 
       ="#^
OPFLAG:=0
       END
                               ;
                               ;
       EOF=3
       PAGE=12
                               ;
                               ;
  TEST:MOV #BUF1,R2
       MOV #BUF2,R3
  SKIP:JSR R7,INSYM
       CMP R0,#EOF
       BEQ FINISH
       CMP R0,#SPACE
       BEQ SKIP
       MOVB R0,(R2)+
       CMP R0,#LF
       BNE SKIP
       MOV #BUF1,R2
       MOV #BUF1,R1
       JSR R7,PRINT
       JSR R7,POLISH
       BCS TEST
       MOV #BUF2,R1
       JSR R7,PRINT
       MOV #LF,R0
       JSR R7,OUTSYM
       BR TEST
FINISH:JMP STOP
                                ;
                                ;
 PRINT:BEGIN
  LOOP:MOVB (R1)+,R0
       CMP R0,#LF
       BEQ SPS
       JSR R7,OUTSYM
       BR LOOP
   SPS:MOV #30,R1
       MOV #13,R0
       JSR R7,OUTSYM
       MOV #SPACE,R0
    L1:JSR R7,OUTSYM
       SOB R1,L1
       RTS R7
       END
                                ;
                                ;
  BUF1:.=.+512
  BUF2:=0
                                ;
                                ;
       ENTER TEST
!
