***A JOB K8       X        CMP.10  
NEW
PROGRAM PL0(IP1,OP1,OUTPUT);
CONST NORW=14;
     TXMAX=100;
     NMAX=5;
     AL=8;
     AMAX=65535;
     LEVMAX=3;
     CXMAX=200;
TYPE SYMBOL=
  (NUL,IDENT,NUMBER,PLUS,MINUS,TIMES,SLASH,ODDSYM,
   EQL,NEQ,LSS,LEQ,GTR,GEQ,LPAREN,RPAREN,COMMA,SEMICOLON,
   PERIOD,BECOMES,BEGINSYM,ENDSYM,IFSYM,THENSYM,ELSESYM,
   WHILESYM,DOSYM,CALLSYM,CONSTSYM,VARSYM,PROCSYM,
   READSYM,WRITESYM,QUOTE);
    ALFA=PACKED ARRAY[1..AL] OF CHAR;
    OBJECT=(CONSTANT,VARIABLE,PROC);
    SYMSET=SET OF SYMBOL;
    FCT=(LIT,OPR,LOD,STO,CAL,INT,JMP,JPC);
    INSTRUCTION=PACKED RECORD
      F:FCT;
    L:0..LEVMAX;
      A:0..AMAX;
      LABELED:BOOLEAN
     END;
     FTYP=0..CXMAX;

VAR   CH:CHAR;
     SYM:SYMBOL;
     ID:ALFA;
     NUM:INTEGER;
     CC:INTEGER;
     LL:INTEGER;
     KK,ERR:INTEGER;
     CX:INTEGER;
     LINE:ARRAY[1..81]OF CHAR;
     A:ALFA;
     CODE:ARRAY[0..CXMAX]OF INSTRUCTION;
     WORD:ARRAY[1..NORW]OF ALFA;
     WSYM:ARRAY[1..NORW]OF SYMBOL;
     SSYM:ARRAY[CHAR]OF SYMBOL;
     MNEMONIC:ARRAY[FCT]OF 
          PACKED ARRAY[1..5]OF CHAR;
     DECLBEGSYS,STATBEGSYS,FACBEGSYS:SYMSET;
     TABLE:ARRAY[0..TXMAX]OF
       RECORD NAME:ALFA;
        CASE KIND:OBJECT OF
         CONSTANT:(VAL:INTEGER);
         VARIABLE,PROC:(LEVEL,ADR:INTEGER)
       END;

FUNCTION IPNAME:REAL;
VAR  ID:PACKED ARRAY[1..8] OF CHAR;
      I:2..8;
BEGIN ID:='        ';
     IF EOLN(INPUT) THEN READLN(INPUT);
     WHILE NOT EOLN(INPUT) DO
     BEGIN
          FOR I:=2 TO 8 DO ID[I-1]:=ID[I];
          READ(INPUT,ID[8])
     END;
     IPNAME:=U64(ID)
END;

PROCEDURE ERROR(N:INTEGER);
BEGIN WRITELN('@@@@',' ':CC-1,'^',N:2);ERR:=ERR+1
END (*ERROR*);

PROCEDURE GETSYMZ;
VAR   I,J,K:INTEGER;
     CHA:CHAR;

  PROCEDURE GETCH;
  BEGIN IF CC=LL THEN
         BEGIN IF EOF(IP1) THEN
                BEGIN WRITE('PROGRAM INCOMPLETE'); HALT
                END;
       LL:=0;
       CC:=0; WRITE(CX:5,' ');
        
        WHILE NOT EOLN(IP1) DO
         BEGIN LL:=LL+1; READ(IP1,CH);WRITE(CH); LINE[LL]:=CH
         END;
       WRITELN; LL:=LL+1; READ(IP1,LINE[LL])
     END;
       CC:=CC+1; CH:=LINE[CC]
  END(*GETCH*);

BEGIN (*GETSYM*)
     WHILE CH=' ' DO GETCH;
     IF (CH>='A') AND (CH<='Z') THEN
     BEGIN (*IDENTIFIER OR RESERVED WORD*)
          K:=0;
          REPEAT IF K<AL THEN
               BEGIN K:=K+1; A[K]:=CH
               END;
          GETCH
          UNTIL NOT( (CH>='A') AND (CH<='Z') OR (CH>='0') AND (CH<='9'));
          IF K>=KK THEN KK:=K ELSE
           REPEAT A[KK]:=' '; KK:=KK-1
           UNTIL K=KK;
          ID:=A; I:=1; J:=NORW;
          REPEAT K:=(I+J) DIV 2;
               IF ID<=WORD[K] THEN J:=K-1;
               IF ID>=WORD[K] THEN I:=K+1
          UNTIL I>J;
          IF I-1>J THEN SYM:=WSYM[K] ELSE SYM:=IDENT
     END ELSE
     IF (CH>='0') AND (CH<='9') THEN
     BEGIN  (*NUMBER*)K:=0; NUM:=0; SYM:=NUMBER;
          REPEAT NUM:=10*NUM+(ORD(CH)-ORD('0'));
               K:=K+1; GETCH
          UNTIL NOT ( (CH>='0') AND (CH<='9') );
          IF K>NMAX THEN ERROR(30)
     END ELSE
     IF CH=':' THEN
     BEGIN GETCH;
          IF CH='=' THEN
          BEGIN SYM:=BECOMES; GETCH
          END ELSE SYM:=NUL;
     END ELSE
     IF CH IN ['<','>','='] THEN
     BEGIN CHA:=CH; GETCH;
          IF CH IN ['<','>','='] THEN
          IF 	(CHA='<') AND 	(CH='>') THEN BEGIN SYM:=NEQ; GETCH END
          ELSE IF (CHA='<') AND (CH='=') THEN BEGIN SYM:=LEQ; GETCH END
          ELSE IF (CHA='>') AND (CH='<') THEN BEGIN SYM:=NEQ; GETCH END
          ELSE IF (CHA='>') AND (CH='=') THEN BEGIN SYM:=GEQ; GETCH END
          ELSE IF (CHA='=') AND (CH='>') THEN BEGIN SYM:=GEQ; GETCH END
          ELSE IF (CHA='=') AND (CH='<') THEN BEGIN SYM:=LEQ; GETCH END
          ELSE SYM:=SSYM[CHA]
          ELSE SYM:=SSYM[CHA]
     END ELSE
     BEGIN SYM:=SSYM[CH]; GETCH
     END;
END (*GEYSYM*);

PROCEDURE GETSYM;
BEGIN REPEAT GETSYMZ UNTIL ORD(SYM)<>0
END;

PROCEDURE GEN(X:FCT; Y,Z:INTEGER);
BEGIN IF CX>CXMAX THEN
     BEGIN WRITE('PROGRAM TOO LONG'); HALT
     END;
     WITH CODE[CX] DO
     BEGIN F:=X; L:=Y; A:=Z; LABELED:=FALSE
     END;
     CX:=CX+1
END (*GEN*);

PROCEDURE TEST(S1,S2:SYMSET; N:INTEGER);
BEGIN IF NOT(SYM IN S1) THEN
     BEGIN ERROR(N); S1:=S1+S2;
          WHILE NOT(SYM IN S1) DO GETSYM
     END
END (*TEST*);


PROCEDURE BLOCK(LEV,TX:INTEGER; FSYS:SYMSET);
VAR DX:INTEGER; (*DATA ALLOCATION INDEX*)
   TX0:INTEGER; (*INITIAL TABLE INDEX*)
   CX0:INTEGER; (*INITIAL CODE INDEX*)
 PROCEDURE ENTER(K:OBJECT);
 BEGIN (*ENTER OBJECT IN TABLE*)
      TX:=TX+1;
      WITH TABLE[TX] DO
      BEGIN NAME:=ID; KIND:=K;
           CASE K OF
            CONSTANT:BEGIN IF NUM>AMAX THEN
                           BEGIN ERROR(30);NUM:=0; END;
                           VAL:=NUM
                     END;
            VARIABLE:BEGIN LEVEL:=LEV; ADR:=DX; DX:=DX+1
                     END;
            PROC    :LEVEL:=LEV
           END
      END
 END (*ENTER*);

 FUNCTION POSITION(ID:ALFA):INTEGER;
 VAR I:INTEGER;
 BEGIN (*FIND IDENTIFIER ID IN TABLE*)
      TABLE[0].NAME:=ID; I:=TX;
      WHILE TABLE[I].NAME<>ID DO I:=I-1;
      POSITION:=I
 END (*POSITION*);

 PROCEDURE CONSTDECLARATION;
 BEGIN IF SYM=IDENT THEN
      BEGIN GETSYM;
           IF SYM IN [EQL,BECOMES] THEN
           BEGIN IF SYM=BECOMES THEN ERROR(1);
                GETSYM;
                IF SYM=NUMBER THEN
                BEGIN ENTER(CONSTANT); GETSYM
                END ELSE ERROR(2)
           END ELSE ERROR(3)
      END ELSE ERROR(4)
 END (*CONSTDECLARATION*);

 PROCEDURE VARDECLARATION;
 BEGIN IF SYM=IDENT THEN
      BEGIN ENTER(VARIABLE); GETSYM
      END ELSE ERROR(4)
 END (*VARDECLARATION*);

 PROCEDURE LISTCODE;
 VAR I:INTEGER;
 BEGIN (*LIST CODE GENERATED BY THIS BLOCK*)
      FOR I:=CX0 TO CX-1 DO
       WITH CODE[I] DO
        CASE F OF
          INT,CAL,LOD,STO:WRITELN(I,' : ',MNEMONIC[F]:5,L:3,A:5);
          LIT,JMP,JPC:WRITELN(I,' : ',MNEMONIC[F]:5,A:5);
          OPR:CASE A OF
               0:WRITELN(I,' : ','RET',L:3);
               1:WRITELN(I,' : ','NEG');
               2:WRITELN(I,' : ','ADD');
               3:WRITELN(I,' : ','SUB');
               4:WRITELN(I,' : ','MPY');
               5:WRITELN(I,' : ','DIV');
               6:WRITELN(I,' : ','ODD');
               8:WRITELN(I,' : ','EQL');
               9:WRITELN(I,' : ','NEQ');
              10:WRITELN(I,' : ','LSS');
              11:WRITELN(I,' : ','GEQ');
              12:WRITELN(I,' : ','GTR');
              13:WRITELN(I,' : ','LEQ');
              50:WRITELN(I,' : ','RDN');
              51:WRITELN(I,' : ','WRN');
              END
        END;
 END (*LISTCODE*);

 PROCEDURE STATEMENT(FSYS:SYMSET);
 VAR I,CX1,CX2:INTEGER;

  PROCEDURE EXPRESSION(FSYS:SYMSET);
  VAR ADDOP:SYMBOL;

   PROCEDURE TERM(FSYS:SYMSET);
   VAR MULOP:SYMBOL;

    PROCEDURE FACTOR(FSYS:SYMSET);
    VAR I:INTEGER;
    BEGIN TEST(FACBEGSYS,FSYS,24);
         WHILE SYM IN FACBEGSYS DO
         BEGIN IF SYM=IDENT THEN
              BEGIN I:=POSITION(ID);
                   IF I=0 THEN ERROR(11) ELSE
                   WITH TABLE[I] DO
                   CASE KIND OF
                    CONSTANT:GEN(LIT,0,VAL);
                    VARIABLE:GEN(LOD,LEVEL,ADR);
                    PROC    :ERROR(21)
                   END;
                   GETSYM
              END ELSE
              IF SYM=NUMBER THEN
              BEGIN IF NUM>AMAX THEN
                   BEGIN ERROR(30); NUM:=0
                   END;
                   GEN(LIT,0,NUM); GETSYM
              END ELSE
              IF SYM=LPAREN THEN
              BEGIN GETSYM; EXPRESSION([RPAREN]+FSYS);
                   IF SYM=RPAREN THEN GETSYM ELSE ERROR(22)
              END;
              TEST(FSYS,[LPAREN],23)
         END
    END (*FACTOR*);

   BEGIN (*TERM*)
         FACTOR(FSYS+[TIMES,SLASH]);
        WHILE SYM IN [TIMES,SLASH] DO
        BEGIN MULOP:=SYM; GETSYM; FACTOR(FSYS+[TIMES,SLASH]);
             IF MULOP=TIMES THEN GEN(OPR,0,4) ELSE GEN(OPR,0,5)
        END
   END (*TERM*);

  BEGIN (*EXPRESSION*)
       IF SYM IN [PLUS,MINUS] THEN
       BEGIN ADDOP:=SYM; GETSYM; TERM(FSYS+[PLUS,MINUS]);
            IF ADDOP=MINUS THEN GEN(OPR,0,1)
       END ELSE TERM(FSYS+[PLUS,MINUS]);
       WHILE SYM IN [PLUS,MINUS] DO
       BEGIN ADDOP:=SYM; GETSYM; TERM(FSYS+[PLUS,MINUS]);
            IF ADDOP=PLUS THEN GEN(OPR,0,2) ELSE GEN(OPR,0,3)
       END
  END (*EXPRESSION*);

 PROCEDURE CONDITION(FSYS:SYMSET);
 VAR RELOP:SYMBOL;
 BEGIN
      IF SYM=ODDSYM THEN
      BEGIN GETSYM; EXPRESSION(FSYS); GEN(OPR,0,6);
      END ELSE
      BEGIN EXPRESSION(FSYS+[EQL,NEQ,LSS,GTR,GEQ,LEQ]);
           IF NOT (SYM IN [EQL,NEQ,LSS,GTR,GEQ,LEQ]) THEN
            ERROR(20) ELSE
           BEGIN RELOP:=SYM; GETSYM; EXPRESSION(FSYS);
                CASE RELOP OF
                 EQL:GEN(OPR,0,8);
                 NEQ:GEN(OPR,0,9);
                 LSS:GEN(OPR,0,10);
                 GEQ:GEN(OPR,0,11);
                 GTR:GEN(OPR,0,12);
                 LEQ:GEN(OPR,0,13);
                END
           END
      END
 END (*CONDITION*);

 BEGIN (*STATEMENT*)
      IF SYM=IDENT THEN
      BEGIN I:=POSITION(ID);
           IF I=0 THEN ERROR(11) ELSE
           IF TABLE[I].KIND<>VARIABLE THEN
           BEGIN (*ASSIGNMENT TO NON-VAVIABLE*)ERROR(12); I:=0
           END;
           GETSYM;
           IF SYM=BECOMES THEN GETSYM ELSE ERROR(13);
           EXPRESSION(FSYS);
           IF I<>0 THEN
            WITH TABLE[I] DO GEN(STO,LEVEL,ADR)
      END ELSE
      IF SYM=CALLSYM THEN
      BEGIN GETSYM;
           IF SYM<>IDENT THEN ERROR(14) ELSE
           BEGIN I:=POSITION(ID);
                IF I=0 THEN ERROR(11) ELSE
                WITH TABLE[I] DO
                IF KIND=PROC THEN GEN(CAL,LEVEL,ADR)
           ELSE ERROR(15);
           GETSYM
           END
      END ELSE

      IF SYM=IFSYM THEN
      BEGIN GETSYM; CONDITION([THENSYM,DOSYM]+FSYS);
           IF SYM=THENSYM THEN GETSYM ELSE ERROR(16);
           CX1:=CX; GEN(JPC,0,0);
           STATEMENT(FSYS+[ELSESYM]);
           IF SYM=ELSESYM THEN
           BEGIN GETSYM; CX2:=CX;
                GEN(JMP,0,0);
                CODE[CX1].A:=CX;
                STATEMENT(FSYS);
                CODE[CX2].A:=CX
           END ELSE CODE[CX1].A:=CX
      END ELSE
      IF SYM=READSYM THEN
      REPEAT GETSYM;
          IF SYM<>IDENT THEN
          BEGIN ERROR(40); I:=0
          END ELSE
          BEGIN I:=POSITION(ID);
               IF I=0 THEN ERROR(11)
          END;
          IF TABLE[I].KIND	<>VARIABLE THEN
          BEGIN ERROR(12);
               I:=0
          END ELSE
          BEGIN GEN(OPR,0,50);
               WITH TABLE[I] DO GEN(STO,LEVEL,ADR)
          END;
          GETSYM
      UNTIL SYM<>COMMA
      ELSE
     IF SYM=WRITESYM THEN
     REPEAT GETSYM;
          IF SYM<>QUOTE THEN
          BEGIN EXPRESSION(FSYS+[COMMA]);
               GEN(OPR,0,51)
          END ELSE
          BEGIN (*STRING*) HALT
          END
     UNTIL SYM<>COMMA
     ELSE
      IF SYM=BEGINSYM THEN
      BEGIN GETSYM; STATEMENT([SEMICOLON,ENDSYM]+FSYS);
           WHILE SYM IN [SEMICOLON]+STATBEGSYS DO
           BEGIN IF SYM=SEMICOLON THEN GETSYM ELSE ERROR(10);
                STATEMENT([SEMICOLON,ENDSYM]+FSYS)
           END;
           IF SYM=ENDSYM THEN GETSYM ELSE ERROR(17)
      END ELSE
      IF SYM=WHILESYM THEN
      BEGIN CX1:=CX; GETSYM; CONDITION([DOSYM]+FSYS);
           CX2:=CX; GEN(JPC,0,0);
           IF SYM=DOSYM THEN GETSYM ELSE ERROR(18);
           STATEMENT(FSYS); GEN(JMP,0,CX1); CODE[CX2].A:=CX
      END;
      TEST(FSYS,[],19)
 END (*STATEMENT*);

BEGIN (*BLOCK*) DX:=0; TX0:=TX; TABLE[TX].ADR:=CX; GEN(JMP,0,0);
     IF LEV>LEVMAX THEN ERROR(32);
      REPEAT
          IF SYM=CONSTSYM THEN
          BEGIN GETSYM;
              REPEAT CONSTDECLARATION;
                    WHILE SYM=COMMA  DO
                    BEGIN GETSYM; CONSTDECLARATION
                    END;
                   IF SYM=SEMICOLON THEN GETSYM ELSE ERROR(5)
               UNTIL SYM<>IDENT
          END;
          IF SYM=VARSYM THEN
          BEGIN GETSYM;
               REPEAT VARDECLARATION;
                    WHILE SYM=COMMA DO
                    BEGIN GETSYM; VARDECLARATION
                    END;
                    IF SYM=SEMICOLON  THEN GETSYM ELSE ERROR(5)
               UNTIL SYM<>IDENT
          END;
          WHILE SYM=PROCSYM DO
          BEGIN GETSYM;
               IF SYM=IDENT THEN
               BEGIN ENTER(PROC); GETSYM
               END
               ELSE ERROR(4);
               IF SYM=SEMICOLON THEN GETSYM ELSE ERROR(5);
               BLOCK(LEV+1,TX,[SEMICOLON]+FSYS);
               IF SYM=SEMICOLON THEN
               BEGIN GETSYM; TEST(STATBEGSYS+[IDENT,PROCSYM],FSYS,6)
               END ELSE ERROR(5)
          END;
          TEST(STATBEGSYS+[IDENT],DECLBEGSYS,7)
     UNTIL NOT (SYM IN DECLBEGSYS);
     CODE[TABLE[TX0].ADR].A:=CX;
     WITH TABLE[TX0] DO
     BEGIN ADR:=CX; (*START ADR OF CODE*)
     END;
     CX0:=CX; GEN(INT,LEV,DX); (*S-O-PROC POINT*)
     STATEMENT([SEMICOLON,ENDSYM]+FSYS);
     GEN(OPR,LEV,0); (*RETURN*)
     TEST(FSYS,[],8);
     LISTCODE
END (*BLOCK*);

PROCEDURE Z80(FINISH:FTYP);
TYPE
     LABTYP=PACKED ARRAY[1..4] OF CHAR;
VAR  I:0..CXMAX;
     CU,CL:0..255;
     LABS:LABTYP;
     HL:(FULL,EMPTY);
     IX,CURRENTLEVEL:-1..LEVMAX;
     VARS:(SOME,NONE);
     START:0..AMAX;

  PROCEDURE LABELER(FINISH:FTYP);
  VAR  I:0..CXMAX;
  BEGIN
       FOR I:=0 TO FINISH DO
        WITH CODE[I] DO
         IF F IN [JMP,JPC,CAL] THEN CODE[A].LABELED:=TRUE
  END;

  PROCEDURE LABGEN(LAB:FTYP; VAR LABS:LABTYP);
  BEGIN
       LABS[1]:='L';
       LABS[2]:=CHR(LAB DIV 100 + ORD('0'));
       LABS[3]:=CHR((LAB - LAB DIV 100 * 100) DIV 10 + ORD('0'));
       LABS[4]:=CHR(LAB MOD 10 +ORD('0'))
  END;

  PROCEDURE CONTROL(F:FCT; A:INTEGER);
  VAR  LABS:LABTYP;
  BEGIN
       LABGEN(A,LABS);
       CASE F OF
        JMP:WRITELN('    JP   ',LABS);
        JPC:WRITELN('     JP   NC,',LABS);
        CAL:WRITELN('    CALL ',LABS)
       END
  END;

  PROCEDURE PROCHEADER(L,V:INTEGER);
  BEGIN
       WRITELN('    LD   HL,(DISPLAY',L:1,')');
       WRITELN('    PUSH HL');
       WRITELN('    LD   IY,0');
       WRITELN('    ADD  IY,SP');
       WRITELN('    LD   HL,',-V*2:3);
       WRITELN('    ADD  HL,SP');
       WRITELN('    LD   SP,HL');
       WRITELN('    LD   (DISPLAY',L:1,'),IY')
  END;
BEGIN (*Z80*)
     SELECTOUTPUT(1);
     LABELER(FINISH);
     FOR I:=0 TO FINISH DO
     WITH CODE[I] DO
     BEGIN
          IF I=0 THEN
          BEGIN
               WRITELN('START:');
               WRITELN('    LD   SP,???X');
               START:=A; IX:=-1
          END;
          IF LABELED THEN
          BEGIN
               LABGEN(I,LABS);
               WRITELN(LABS,':')
          END;
          CASE F OF
          INT:BEGIN (*ALLOCATE A VARIABLES AT LEVEL L*)
                   HL:=EMPTY; CURRENTLEVEL:=L;
                   VARS:=NONE;
                   IF A<>0 THEN BEGIN PROCHEADER(L,A); VARS:=SOME END
              END;
          JMP,JPC,CAL:CONTROL(F,A);
          LIT:BEGIN
                   IF HL=FULL THEN WRITELN('    PUSH HL');
                   HL:=FULL;
                   WRITELN('    LD   HL,',A:6)
              END;
          LOD:BEGIN
                   IF HL=FULL THEN WRITELN('    PUSH HL');
                   HL:=FULL;
                   IF L=CURRENTLEVEL THEN
                   BEGIN
                        WRITELN('    LD   L,(IY',-2*A-1:3,')');
                        IF A=0 THEN
                        WRITELN('    LD   H,(IY+0)')
                        ELSE
                        WRITELN('    LD   H,(IY',-2*A:3,')')
                   END ELSE
                   BEGIN
                        IF IX<>L THEN
                        BEGIN
                             WRITELN('    LD   IX,(DISPLAY',L:1,')');
                             IX:=L
                        END;
                        WRITELN('    LD   L,(IX',-2*A-1:3,')');
                        IF A=0 THEN
                        WRITELN('    LD   H,(IX+0)') ELSE
                        WRITELN('    LD   H,(IX',-2*A:3,')')
                   END;
              END;
          STO:BEGIN
                   HL:=EMPTY;
                   IF L=CURRENTLEVEL THEN
                   BEGIN
                        WRITELN('    LD   (IY',-2*A-1:3,'),L');
                        IF A=0 THEN
                        WRITELN('    LD   (IY+0),H') ELSE
                        WRITELN('    LD   (IY',-2*A:3,'),H')
                   END ELSE
                   BEGIN
                        IF IX<>L THEN
                        BEGIN
                             WRITELN('    LD   IX,(DISPLAY',L:1,')');
                             IX:=L
                        END;
                        WRITELN('    LD   (IX',-2*A-1:3,'),L');
                        IF A=0 THEN
                        WRITELN('    LD   (IX+0),H') ELSE
                        WRITELN('    LD   (IX',-2*A:3,'),H')
                   END;
              END;
          OPR:CASE A OF
               0:BEGIN (*RET*)
                      IF VARS=SOME THEN
                      BEGIN
                           WRITELN('    LD   SP,IY');
                           WRITELN('    POP  HL');
                           WRITELN('    LD   (DISPLAY',L:1,'),HL');
                      END;
                      WRITELN('    RET');
              END;
               1:BEGIN (*NEG*)
                      WRITELN('    EX   HL,DE');
                      WRITELN('    LD   HL,0');
                      WRITELN('    OR   A');
                      WRITELN('    SBC  HL,DE');
                 END;
               2,3,4,5:BEGIN (*ADD,SUB,MPY,DIV*)
                            WRITELN('    POP  DE');
                            CASE A OF
                               2:WRITELN('    ADD  HL,DE');
                               3:BEGIN
                                      WRITELN('    EX   HL,DE');
                                      WRITELN('    OR   A');
                                      WRITELN('    SBC  HL,DE')
                                 END;
                               4:WRITELN('    CALL MPY');
                               5:WRITELN('    CALL DIV')
                           END;
                      END;
               6:BEGIN (*ODD*)
                      HL:=EMPTY;
                      WRITELN('    SRA  L');
                 END;
               8,9,10,11,12,13:
                 BEGIN (*RELATIONAL OPERATORS*)
                      WRITELN('    POP  DE');
                      CASE A OF
                        8:WRITELN('    LD   A,1');  (* = *)
                        9:WRITELN('    LD   A,6');  (* <> *)
                       10:WRITELN('    LD   A,2');  (* < *)
                       11:WRITELN('    LD   A,5');  (* >= *)
                       12:WRITELN('    LD   A,4');  (* > *)
                       13:WRITELN('    LD   A,3');  (* <= *)
                      END;
                      HL:=EMPTY;
                      WRITELN('    CALL RELOP')
                 END;
              END (*OPR CASE*);
          END (*F CASE*)
     END (*WITH*);
     SELECTOUTPUT(0);
     BREAKOUTPUT(1)
END (*Z80*);


BEGIN (*MAIN PROGRAM*)
     WRITE(OUTPUT,'INPUT FILE NAME');
     DEFINEINPUT(1,IPNAME);
     WRITE(OUTPUT,'OUTPUT FILE NAME');
     DEFINEOUTPUT(1,IPNAME,0,1000,1);
     SELECTOUTPUT(1);
     FOR CH:='A' TO ';' DO SSYM[CH]:=NUL;
     WORD[1]:='BEGIN   ';  WORD[2]:='CALL    ';
     WORD[3]:='CONST   ';  WORD[4]:='DO      ';
     WORD[5]:='ELSE    ';  WORD[6]:='END     ';
     WORD[7]:='IF      ';  WORD[8]:='ODD     ';
     WORD[9]:='PROC    ';  WORD[10]:='READ    ';
     WORD[11]:='THEN    ';  WORD[12]:='VAR     ';
     WORD[13]:='WHILE   ';  WORD[14]:='WRITE   ';
     WSYM[1]:=BEGINSYM; WSYM[2]:=CALLSYM;
     WSYM[3]:=CONSTSYM; WSYM[4]:=DOSYM;
     WSYM[5]:=ELSESYM; WSYM[6]:=ENDSYM;
     WSYM[7]:=IFSYM; WSYM[8]:=ODDSYM;
     WSYM[9]:=PROCSYM; WSYM[10]:=READSYM;
     WSYM[11]:=THENSYM; WSYM[12]:=VARSYM;
     WSYM[13]:=WHILESYM; WSYM[14]:=WRITESYM;
     SSYM['+']:=PLUS; SSYM['-']:=MINUS;
     SSYM['*']:=TIMES; SSYM['/']:=SLASH;
     SSYM['(']:=LPAREN; SSYM[')']:=RPAREN;
     SSYM['=']:=EQL; SSYM[',']:=COMMA;
     SSYM['.']:=PERIOD;
     SSYM['<']:=LSS; SSYM['>']:=GTR;
     SSYM[';']:=SEMICOLON;
     MNEMONIC[LIT]:='LIT  '; MNEMONIC[OPR]:='OPR  ';
     MNEMONIC[LOD]:='LOD  '; MNEMONIC[STO]:='STO  ';
     MNEMONIC[CAL]:='CAL  '; MNEMONIC[INT]:='INT  ';
     MNEMONIC[JMP]:='JMP  '; MNEMONIC[JPC]:='JPC  ';
      DECLBEGSYS:=[CONSTSYM,VARSYM,PROCSYM];
     STATBEGSYS:=[BEGINSYM,CALLSYM,IFSYM,WHILESYM,READSYM,WRITESYM];
     FACBEGSYS:=[IDENT,NUMBER,LPAREN];
     ERR:=0;
     CC:=0; CX:=0; LL:=0; CH:=' '; KK:=AL; GETSYM;
     BLOCK(0,0,[PERIOD]+DECLBEGSYS+STATBEGSYS);
     IF SYM<>PERIOD THEN ERROR(9);
     IF ERR<>0 THEN WRITE('ERRORS IN PL0 PROGRAM') ELSE Z80(CX-1)
END.





/
SAVE CMP.10  
LD
STOP
***Z

