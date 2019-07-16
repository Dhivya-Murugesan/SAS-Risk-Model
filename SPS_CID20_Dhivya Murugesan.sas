libname tt "D:\SAS";
/* Step One: Understand your data */
proc contents data=tt.Hmeq;
run;

/**********
              #    Variable    Type    Len    Label

              1    bad         Num       8    Default or seriously delinquent
             10    clage       Num       8    Age of oldest trade line in months
             12    clno        Num       8    Number of trade (credit) lines
             13    debtinc     Num       8    Debt to income ratio
              9    delinq      Num       8    Number of delinquent trade lines
              8    derog       Num       8    Number of major derogatory reports
              6    job         Char      6    Prof/exec sales mngr office self other
              2    loan        Num       8    Amount of current loan request
              3    mortdue     Num       8    Amount due on existing mortgage
             11    ninq        Num       8    Number of recent credit inquiries
              5    reason      Char      7    Home improvement or debt consolidation
              4    value       Num       8    Value of current property
              7    yoj         Num       8    Years on current job

bad
clage
clno
debtinc
delinq
derog
job
loan
mortdue
ninq
reason
value
yoj

***********/
proc print data=tt.hmeq;
run;
/* First of all, focus on the Char variables */
proc freq data=tt.hmeq;
   tables bad reason job;
run;

/* Step Two: Split data into two parts (Dev and Val) */
data MODEL_DEV MODEL_VAL;
  set tt.Hmeq;;
  if ranuni(1234567)<=0.6 THEN OUTPUT MODEL_DEV;
  ELSE                         OUTPUT MODEL_VAL;
run;

proc print data=Model_Dev;
run;


proc print data=Model_Val;
run;
/* Based on the step one, convert Char var to NUM var on the Dev dataset*/

data MODEL_DEV1(drop=job reason);
  set MODEL_DEV;
  JOB_Mgr=(JOB='Mgr');
  JOB_Office=(JOB='Office');
  JOB_Other=(JOB='Other');
  JOB_ProfExe=(JOB='ProfExe');
  JOB_Sales=(JOB='Sales');
  JOB_Self=(JOB='Self');
  JOB_miss=(JOB=' ');
  REASON_DebtCon=(REASON='DebtCon');
  REASON_HomeImp=(REASON='HomeImp');
  REASON_Miss=(REASON=' ');

  if CLAGE=. then CLAGE=94.325;
  if CLNO=. then CLNO=31.3848;
  if DELINQ=. then DELINQ=0;
  if DEROG=. then DEROG=0;
  if MORTDUE=. then MORTDUE=54175.95;
  if NINQ=. then NINQ= 0;
  if YOJ=. then YOJ= 25.1988;
  
run;

proc print data= Model_Dev1;
run;

proc contents data=MODEL_DEV1;
run;
/* Step Three : data preparation such as i). replace missing value; ii). data transformation */
'''
%let inter_var=

clage
clno
debtinc
delinq
derog
loan
mortdue
ninq
value
yoj
;

%LET DSN=MODEL_DEV1;
%LET RESP=BAD;
%LET GROUPS=10;

%MACRO LOGTCONT                         ;
      OPTIONS CENTER PAGENO=1 DATE;
	  data test;
	    set &DSN;
	  run;
	  %do i=1 %to 10;
	  %LET VBLE=%scan(&inter_var, &i);  
       PROC RANK DATA =TEST (KEEP=&RESP &VBLE)
               GROUPS = &GROUPS
                  OUT = JUNK1     ;
            RANKS NEWVBLE         ;
            VAR &VBLE             ;
       RUN                        ;

       PROC SUMMARY DATA = JUNK1 NWAY ;
            CLASS NEWVBLE             ;
            VAR &RESP &VBLE           ;
            OUTPUT OUT = JUNK2
                  MEAN =
                  MIN(&VBLE)=MIN
                  MAX(&VBLE)=MAX
                     N = NOBS         ;
       RUN                            ;

       DATA JUNK2                     ;
            SET JUNK2                 ;
            IF &RESP NE 0 THEN
               LOGIT = LOG ( &RESP / (1- &RESP) ) ;
            ELSE IF &RESP = 0 THEN LOGIT = .       ;
       RUN                            ;

       PROC SQL NOPRINT;
        CREATE TABLE JUNK3 AS
        SELECT 99 AS NEWVBLE, COUNT(*) AS NOBS, MEAN(&RESP) AS &RESP
        FROM test
        WHERE &VBLE=.
       ;

       DATA JUNK3;
        SET JUNK3;
        LOGIT=LOG(&RESP/(1-&RESP));
       RUN;

       DATA JUNK4;
        SET JUNK2 JUNK3;
       RUN;

       PROC PLOT DATA = JUNK4         ;
            TITLE1 "Plot of Logit(Response) by &&VBLE" ;
            PLOT  LOGIT* &VBLE        ;
       RUN                            ;

        proc plot data=junk4;
        plot &resp*&vble;
        plot _freq_*&vble;
        TITLE2 "Plot of Response by &&VBLE" ;
        run;

       PROC PRINT DATA = JUNK4 LABEL SPLIT = '*' NOOBS ;
            TITLE3 "Table of Response by Grouped &&VBLE" ;
            VAR NEWVBLE NOBS &VBLE MIN MAX &RESP ;
            LABEL NEWVBLE = "&&VBLE Grouping"
                     NOBS = '# of*Records'
                     LOGIT = "Logit of Response"
                     MIN   ='MIN'
                     MAX   ='MAX'                       ;
       RUN                                             ;

	   %end;


%MEND LOGTCONT      ;
%LOGTCONT      ;
'''

proc print data=Model_Dev1;
run;

/* Ranking the customers based on the Bad Rate*/


/*  clage - Ranking and grouping the customers based on bad rate*/

proc sort data=MODEL_DEV1;
 by decending BAD;
run;

  
       PROC RANK DATA =MODEL_DEV1(KEEP=BAD clage)
               GROUPS = 10
                  OUT = JUNK1     ;
            RANKS NEWVBLE         ;
            VAR clage             ;
       RUN                        ;

       PROC SUMMARY DATA = JUNK1 NWAY ;
            CLASS NEWVBLE             ;
            VAR BAD clage         ;
            OUTPUT OUT = JUNK2
                  MEAN =
                  MIN(clage)=MIN
                  MAX(clage)=MAX
                     N = NOBS         ;
       RUN                            ;

       DATA JUNK2                     ;
            SET JUNK2                 ;
            IF BAD NE 0 THEN
               LOGIT = LOG ( BAD / (1- BAD) ) ;
            ELSE IF BAD = 0 THEN LOGIT = .       ;
       RUN                            ;

       PROC SQL NOPRINT;
        CREATE TABLE JUNK3 AS
        SELECT 99 AS NEWVBLE, COUNT(*) AS NOBS, MEAN(BAD) AS BAD
        FROM MODEL_DEV1
        WHERE clage=.
       ;

       DATA JUNK3;
        SET JUNK3;
        LOGIT=LOG(BAD/(1-BAD));
       RUN;

       DATA JUNK4;
        SET JUNK2 JUNK3;
       RUN;

       PROC PLOT DATA = JUNK4         ;
            TITLE1 "Plot of Logit(Response) by clage" ;
            PLOT  LOGIT* clage        ;
       RUN                            ;

        proc plot data=junk4;
        plot BAD*clage;
        plot _freq_*clage;
        TITLE2 "Plot of Response by clage" ;
        run;

       PROC PRINT DATA = JUNK4 LABEL SPLIT = '*' NOOBS ;
            TITLE3 "Table of Response by Grouped clage" ;
            VAR NEWVBLE NOBS clage MIN MAX BAD ;
            LABEL NEWVBLE = "clage Grouping"
                     NOBS = '# of*Records'
                     LOGIT = "Logit of Response"
                     MIN   ='MIN'
                     MAX   ='MAX'                       ;
       RUN                                             ;


/*  clno - Ranking and grouping the customers based on bad rate*/


proc sort data=MODEL_DEV1 ;
by decending BAD;
run;

  
       PROC RANK DATA =MODEL_DEV1(KEEP=BAD clno)
               GROUPS = 10
                  OUT = JUNK5     ;
            RANKS NEWVBLE         ;
            VAR clno            ;
       RUN                        ;

       PROC SUMMARY DATA = JUNK5 NWAY ;
            CLASS NEWVBLE             ;
            VAR BAD clno         ;
            OUTPUT OUT = JUNK6
                  MEAN =
                  MIN(clno)=MIN
                  MAX(clno)=MAX
                     N = NOBS         ;
       RUN                            ;

       DATA JUNK6                     ;
            SET JUNK6                ;
            IF BAD NE 0 THEN
               LOGIT = LOG ( BAD / (1- BAD) ) ;
            ELSE IF BAD = 0 THEN LOGIT = .       ;
       RUN                            ;

       PROC SQL NOPRINT;
        CREATE TABLE JUNK7 AS
        SELECT 99 AS NEWVBLE, COUNT(*) AS NOBS, MEAN(BAD) AS BAD
        FROM MODEL_DEV1
        WHERE clno=.
       ;

       DATA JUNK7;
        SET JUNK7;
        LOGIT=LOG(BAD/(1-BAD));
       RUN;

       DATA JUNK8;
        SET JUNK6 JUNK7;
       RUN;

       PROC PLOT DATA = JUNK8         ;
            TITLE1 "Plot of Logit(Response) by clno" ;
            PLOT  LOGIT* clno        ;
       RUN                            ;

        proc plot data=junk8;
        plot BAD*clno;
        plot _freq_*clno;
        TITLE2 "Plot of Response by clage" ;
        run;

       PROC PRINT DATA = JUNK8 LABEL SPLIT = '*' NOOBS ;
            TITLE3 "Table of Response by Grouped clno" ;
            VAR NEWVBLE NOBS clno MIN MAX BAD ;
            LABEL NEWVBLE = "clno Grouping"
                     NOBS = '# of*Records'
                     LOGIT = "Logit of Response"
                     MIN   ='MIN'
                     MAX   ='MAX'                       ;
       RUN                                             ;


/*  debtinc - Ranking and grouping the customers based on bad rate*/

proc sort data=MODEL_DEV1 ;
by decending BAD;
run;

  
       PROC RANK DATA =MODEL_DEV1(KEEP=BAD debtinc)
               GROUPS = 10
                  OUT = JUNK9     ;
            RANKS NEWVBLE         ;
            VAR debtinc          ;
       RUN                        ;

       PROC SUMMARY DATA = JUNK9 NWAY ;
            CLASS NEWVBLE             ;
            VAR BAD debtinc        ;
            OUTPUT OUT = JUNK10
                  MEAN =
                  MIN(debtinc)=MIN
                  MAX(debtinc)=MAX
                     N = NOBS         ;
       RUN                            ;

       DATA JUNK10                     ;
            SET JUNK10                 ;
            IF BAD NE 0 THEN
               LOGIT = LOG ( BAD / (1- BAD) ) ;
            ELSE IF BAD = 0 THEN LOGIT = .       ;
       RUN                            ;

       PROC SQL NOPRINT;
        CREATE TABLE JUNK11 AS
        SELECT 99 AS NEWVBLE, COUNT(*) AS NOBS, MEAN(BAD) AS BAD
        FROM MODEL_DEV1
        WHERE debtinc=.
       ;

       DATA JUNK11;
        SET JUNK11;
        LOGIT=LOG(BAD/(1-BAD));
       RUN;

       DATA JUNK12;
        SET JUNK10 JUNK11;
       RUN;

       PROC PLOT DATA = JUNK12         ;
            TITLE1 "Plot of Logit(Response) by debtinc" ;
            PLOT  LOGIT*debtinc;
       RUN                            ;

        proc plot data=junk12;
        plot BAD*debtinc;
        plot _freq_*debtinc;
        TITLE2 "Plot of Response by debtinc" ;
        run;

       PROC PRINT DATA = JUNK12 LABEL SPLIT = '*' NOOBS ;
            TITLE3 "Table of Response by Grouped debtinc" ;
            VAR NEWVBLE NOBS debtinc MIN MAX BAD ;
            LABEL NEWVBLE = "debtinc Grouping"
                     NOBS = '# of*Records'
                     LOGIT = "Logit of Response"
                     MIN   ='MIN'
                     MAX   ='MAX'                       ;
       RUN                                             ;


/*  Delinq - Ranking and grouping the customers based on bad rate*/

proc sort data=MODEL_DEV1 ;
by decending BAD;
run;

  
       PROC RANK DATA =MODEL_DEV1(KEEP=BAD DELINQ)
               GROUPS = 10
                  OUT = JUNK13     ;
            RANKS NEWVBLE         ;
            VAR DELINQ          ;
       RUN                        ;

       PROC SUMMARY DATA = JUNK13 NWAY ;
            CLASS NEWVBLE             ;
            VAR BAD DELINQ         ;
            OUTPUT OUT = JUNK14
                  MEAN =
                  MIN(DELINQ)=MIN
                  MAX(DELINQ)=MAX
                     N = NOBS         ;
       RUN                            ;

       DATA JUNK14                     ;
            SET JUNK14                 ;
            IF BAD NE 0 THEN
               LOGIT = LOG ( BAD / (1- BAD) ) ;
            ELSE IF BAD = 0 THEN LOGIT = .       ;
       RUN                            ;

       PROC SQL NOPRINT;
        CREATE TABLE JUNK15 AS
        SELECT 99 AS NEWVBLE, COUNT(*) AS NOBS, MEAN(BAD) AS BAD
        FROM MODEL_DEV1
        WHERE DELINQ=.
       ;

       DATA JUNK15;
        SET JUNK15;
        LOGIT=LOG(BAD/(1-BAD));
       RUN;

       DATA JUNK16;
        SET JUNK14 JUNK15;
       RUN;

       PROC PLOT DATA = JUNK16         ;
            TITLE1 "Plot of Logit(Response) by DELINQ" ;
            PLOT  LOGIT* DELINQ        ;
       RUN                            ;

        proc plot data=junk16;
        plot BAD*DELINQ;
        plot _freq_*DELINQ;
        TITLE2 "Plot of Response by DELINQ" ;
        run;

       PROC PRINT DATA = JUNK16 LABEL SPLIT = '*' NOOBS ;
            TITLE3 "Table of Response by Grouped DELINQ" ;
            VAR NEWVBLE NOBS DELINQ MIN MAX BAD ;
            LABEL NEWVBLE = "DELINQ Grouping"
                     NOBS = '# of*Records'
                     LOGIT = "Logit of Response"
                     MIN   ='MIN'
                     MAX   ='MAX'                       ;
       RUN                                             ;



/*  Derog - Ranking and grouping the customers based on bad rate*/

 proc sort data=MODEL_DEV1 ;
by decending BAD;
run;

  
       PROC RANK DATA =MODEL_DEV1(KEEP=BAD DEROG)
               GROUPS = 10
                  OUT = JUNK17     ;
            RANKS NEWVBLE         ;
            VAR DEROG          ;
       RUN                        ;

       PROC SUMMARY DATA = JUNK17 NWAY ;
            CLASS NEWVBLE             ;
            VAR BAD DEROG         ;
            OUTPUT OUT = JUNK18
                  MEAN =
                  MIN(DEROG)=MIN
                  MAX(DEROG)=MAX
                     N = NOBS         ;
       RUN                            ;

       DATA JUNK18                     ;
            SET JUNK18                ;
            IF BAD NE 0 THEN
               LOGIT = LOG ( BAD / (1- BAD) ) ;
            ELSE IF BAD = 0 THEN LOGIT = .       ;
       RUN                            ;

       PROC SQL NOPRINT;
        CREATE TABLE JUNK19 AS
        SELECT 99 AS NEWVBLE, COUNT(*) AS NOBS, MEAN(BAD) AS BAD
        FROM MODEL_DEV1
        WHERE DEROG=.
       ;

       DATA JUNK19;
        SET JUNK19;
        LOGIT=LOG(BAD/(1-BAD));
       RUN;

       DATA JUNK20;
        SET JUNK18 JUNK19;
       RUN;

       PROC PLOT DATA = JUNK20        ;
            TITLE1 "Plot of Logit(Response) by DEROG" ;
            PLOT  LOGIT* DEROG ;
       RUN                            ;

        proc plot data=junk20;
        plot BAD*DEROG;
        plot _freq_*DEROG;
        TITLE2 "Plot of Response by DEROG" ;
        run;

       PROC PRINT DATA = JUNK20 LABEL SPLIT = '*' NOOBS ;
            TITLE3 "Table of Response by Grouped DEROG" ;
            VAR NEWVBLE NOBS DEROG MIN MAX BAD ;
            LABEL NEWVBLE = "DEROG Grouping"
                     NOBS = '# of*Records'
                     LOGIT = "Logit of Response"
                     MIN   ='MIN'
                     MAX   ='MAX'                       ;
       RUN                                             ;


/*  Loan - Ranking and grouping the customers based on bad rate*/


proc sort data=MODEL_DEV1 ;
by decending BAD;
run;

  
       PROC RANK DATA =MODEL_DEV1(KEEP=BAD LOAN)
               GROUPS = 10
                  OUT = JUNK21     ;
            RANKS NEWVBLE         ;
            VAR LOAN         ;
       RUN                        ;

       PROC SUMMARY DATA = JUNK21 NWAY ;
            CLASS NEWVBLE             ;
            VAR BAD LOAN         ;
            OUTPUT OUT = JUNK22
                  MEAN =
                  MIN(LOAN)=MIN
                  MAX(LOAN)=MAX
                     N = NOBS         ;
       RUN                            ;

       DATA JUNK22                     ;
            SET JUNK22                 ;
            IF BAD NE 0 THEN
               LOGIT = LOG ( BAD / (1- BAD) ) ;
            ELSE IF BAD = 0 THEN LOGIT = .       ;
       RUN                            ;

       PROC SQL NOPRINT;
        CREATE TABLE JUNK23 AS
        SELECT 99 AS NEWVBLE, COUNT(*) AS NOBS, MEAN(BAD) AS BAD
        FROM MODEL_DEV1
        WHERE LOAN=.
       ;

       DATA JUNK23;
        SET JUNK23;
        LOGIT=LOG(BAD/(1-BAD));
       RUN;

       DATA JUNK24;
        SET JUNK22 JUNK23;
       RUN;

       PROC PLOT DATA = JUNK24         ;
            TITLE1 "Plot of Logit(Response) by LOAN" ;
            PLOT  LOGIT* LOAN       ;
       RUN                            ;

        proc plot data=junk24;
        plot BAD*LOAN;
        plot _freq_*LOAN;
        TITLE2 "Plot of Response by LOAN" ;
        run;

       PROC PRINT DATA = JUNK24 LABEL SPLIT = '*' NOOBS ;
            TITLE3 "Table of Response by Grouped LOAN" ;
            VAR NEWVBLE NOBS LOAN MIN MAX BAD ;
            LABEL NEWVBLE = "LOAN Grouping"
                     NOBS = '# of*Records'
                     LOGIT = "Logit of Response"
                     MIN   ='MIN'
                     MAX   ='MAX'                       ;
       RUN                                             ;


/*  MortDue - Ranking and grouping the customers based on bad rate*/
  
proc sort data=MODEL_DEV1 ;
by decending BAD;
run;

  
       PROC RANK DATA =MODEL_DEV1(KEEP=BAD MORTDUE)
               GROUPS = 10
                  OUT = JUNK25     ;
            RANKS NEWVBLE         ;
            VAR MORTDUE        ;
       RUN                        ;

       PROC SUMMARY DATA = JUNK25 NWAY ;
            CLASS NEWVBLE             ;
            VAR BAD MORTDUE         ;
            OUTPUT OUT = JUNK26
                  MEAN =
                  MIN(MORTDUE)=MIN
                  MAX(MORTDUE)=MAX
                     N = NOBS         ;
       RUN                            ;

       DATA JUNK26                    ;
            SET JUNK26                 ;
            IF BAD NE 0 THEN
               LOGIT = LOG ( BAD / (1- BAD) ) ;
            ELSE IF BAD = 0 THEN LOGIT = .       ;
       RUN                            ;

       PROC SQL NOPRINT;
        CREATE TABLE JUNK27 AS
        SELECT 99 AS NEWVBLE, COUNT(*) AS NOBS, MEAN(BAD) AS BAD
        FROM MODEL_DEV1
        WHERE MORTDUE=.
       ;

       DATA JUNK27;
        SET JUNK27;
        LOGIT=LOG(BAD/(1-BAD));
       RUN;

       DATA JUNK28;
        SET JUNK26 JUNK27;
       RUN;

       PROC PLOT DATA = JUNK28        ;
            TITLE1 "Plot of Logit(Response) by MORTDUE" ;
            PLOT  LOGIT* MORTDUE      ;
       RUN                            ;

        proc plot data=junk28;
        plot BAD*MORTDUE;
        plot _freq_*MORTDUE;
        TITLE2 "Plot of Response by MORTDUE" ;
        run;

       PROC PRINT DATA = JUNK28 LABEL SPLIT = '*' NOOBS ;
            TITLE3 "Table of Response by Grouped MORTDUE" ;
            VAR NEWVBLE NOBS MORTDUE MIN MAX BAD ;
            LABEL NEWVBLE = "MORTDUE Grouping"
                     NOBS = '# of*Records'
                     LOGIT = "Logit of Response"
                     MIN   ='MIN'
                     MAX   ='MAX'                       ;
       RUN                                             ;


/*  ninq - Ranking and grouping the customers based on bad rate*/
  
proc sort data=MODEL_DEV1 ;
by decending BAD;
run;

  
       PROC RANK DATA =MODEL_DEV1(KEEP=BAD NINQ)
               GROUPS = 10
                  OUT = JUNK29     ;
            RANKS NEWVBLE         ;
            VAR NINQ        ;
       RUN                        ;

       PROC SUMMARY DATA = JUNK29 NWAY ;
            CLASS NEWVBLE             ;
            VAR BAD NINQ         ;
            OUTPUT OUT = JUNK30
                  MEAN =
                  MIN(NINQ)=MIN
                  MAX(NINQ)=MAX
                     N = NOBS         ;
       RUN                            ;

       DATA JUNK30                     ;
            SET JUNK30                 ;
            IF BAD NE 0 THEN
               LOGIT = LOG ( BAD / (1- BAD) ) ;
            ELSE IF BAD = 0 THEN LOGIT = .       ;
       RUN                            ;

       PROC SQL NOPRINT;
        CREATE TABLE JUNK31 AS
        SELECT 99 AS NEWVBLE, COUNT(*) AS NOBS, MEAN(BAD) AS BAD
        FROM MODEL_DEV1
        WHERE NINQ=.
       ;

       DATA JUNK31;
        SET JUNK31;
        LOGIT=LOG(BAD/(1-BAD));
       RUN;

       DATA JUNK32;
        SET JUNK30 JUNK31;
       RUN;

       PROC PLOT DATA = JUNK32         ;
            TITLE1 "Plot of Logit(Response) by NINQ" ;
            PLOT  LOGIT* NINQ      ;
       RUN                            ;

        proc plot data=junk32;
        plot BAD*NINQ;
        plot _freq_*NINQ;
        TITLE2 "Plot of Response by NINQ" ;
        run;

       PROC PRINT DATA = JUNK32 LABEL SPLIT = '*' NOOBS ;
            TITLE3 "Table of Response by Grouped NINQ" ;
            VAR NEWVBLE NOBS NINQ MIN MAX BAD ;
            LABEL NEWVBLE = "NINQ Grouping"
                     NOBS = '# of*Records'
                     LOGIT = "Logit of Response"
                     MIN   ='MIN'
                     MAX   ='MAX'                       ;
       RUN                                             ;



/* value - Ranking and grouping the customers based on bad rate*/

proc sort data=MODEL_DEV1 ;
by decending BAD;
run;

  
       PROC RANK DATA =MODEL_DEV1(KEEP=BAD VALUE)
               GROUPS = 10
                  OUT = JUNK33    ;
            RANKS NEWVBLE         ;
            VAR VALUE       ;
       RUN                        ;

       PROC SUMMARY DATA = JUNK33 NWAY ;
            CLASS NEWVBLE             ;
            VAR BAD VALUE        ;
            OUTPUT OUT = JUNK34
                  MEAN =
                  MIN(VALUE)=MIN
                  MAX(VALUE)=MAX
                     N = NOBS         ;
       RUN                            ;

       DATA JUNK34                     ;
            SET JUNK34                ;
            IF BAD NE 0 THEN
               LOGIT = LOG ( BAD / (1- BAD) ) ;
            ELSE IF BAD = 0 THEN LOGIT = .       ;
       RUN                            ;

       PROC SQL NOPRINT;
        CREATE TABLE JUNK35 AS
        SELECT 99 AS NEWVBLE, COUNT(*) AS NOBS, MEAN(BAD) AS BAD
        FROM MODEL_DEV1
        WHERE VALUE=.
       ;

       DATA JUNK35;
        SET JUNK35;
        LOGIT=LOG(BAD/(1-BAD));
       RUN;

       DATA JUNK36;
        SET JUNK34 JUNK35;
       RUN;

       PROC PLOT DATA = JUNK36         ;
            TITLE1 "Plot of Logit(Response) by VALUE" ;
            PLOT  LOGIT* VALUE      ;
       RUN                            ;

        proc plot data=junk36;
        plot BAD*VALUE;
        plot _freq_*VALUE;
        TITLE2 "Plot of Response by VALUE" ;
        run;

       PROC PRINT DATA = JUNK36 LABEL SPLIT = '*' NOOBS ;
            TITLE3 "Table of Response by Grouped VALUE" ;
            VAR NEWVBLE NOBS VALUE MIN MAX BAD ;
            LABEL NEWVBLE = "VALUE Grouping"
                     NOBS = '# of*Records'
                     LOGIT = "Logit of Response"
                     MIN   ='MIN'
                     MAX   ='MAX'                       ;
       RUN                                             ;


/* yoj - Ranking and grouping the customers based on bad rate*/

proc sort data=MODEL_DEV1 ;
by decending BAD;
run;

  
       PROC RANK DATA =MODEL_DEV1(KEEP=BAD yoj)
               GROUPS = 10
                  OUT = JUNK37     ;
            RANKS NEWVBLE         ;
            VAR yoj     ;
       RUN                        ;

       PROC SUMMARY DATA = JUNK37 NWAY ;
            CLASS NEWVBLE             ;
            VAR BAD yoj      ;
            OUTPUT OUT = JUNK38
                  MEAN =
                  MIN(yoj)=MIN
                  MAX(yoj)=MAX
                     N = NOBS         ;
       RUN                            ;

       DATA JUNK38                     ;
            SET JUNK38                 ;
            IF BAD NE 0 THEN
               LOGIT = LOG ( BAD / (1- BAD) ) ;
            ELSE IF BAD = 0 THEN LOGIT = .       ;
       RUN                            ;

       PROC SQL NOPRINT;
        CREATE TABLE JUNK39 AS
        SELECT 99 AS NEWVBLE, COUNT(*) AS NOBS, MEAN(BAD) AS BAD
        FROM MODEL_DEV1
        WHERE yoj=.
       ;

       DATA JUNK39;
        SET JUNK39;
        LOGIT=LOG(BAD/(1-BAD));
       RUN;

       DATA JUNK40;
        SET JUNK38 JUNK39;
       RUN;

       PROC PLOT DATA = JUNK40         ;
            TITLE1 "Plot of Logit(Response) by yoj" ;
            PLOT  LOGIT*yoj     ;
       RUN                            ;

        proc plot data=junk40;
        plot BAD*yoj;
        plot _freq_*yoj;
        TITLE2 "Plot of Response byyoj" ;
        run;

       PROC PRINT DATA = JUNK40 LABEL SPLIT = '*' NOOBS ;
            TITLE3 "Table of Response by Grouped yoj" ;
            VAR NEWVBLE NOBS yoj MIN MAX BAD ;
            LABEL NEWVBLE = "yoj Grouping"
                     NOBS = '# of*Records'
                     LOGIT = "Logit of Response"
                     MIN   ='MIN'
                     MAX   ='MAX'                       ;
       RUN                                             ;


/* Step Four: create dummy variables and do data transformation on the data of Dev */

data MODEL_DEV11(drop=job reason);
  set model_Dev1;
  JOB_Mgr=(JOB='Mgr');
  JOB_Office=(JOB='Office');
  JOB_Other=(JOB='Other');
  JOB_ProfExe=(JOB='ProfExe');
  JOB_Sales=(JOB='Sales');
  JOB_Self=(JOB='Self');
  JOB_miss=(JOB=' ');
  REASON_DebtCon=(REASON='DebtCon');
  REASON_HomeImp=(REASON='HomeImp');
  REASON_Miss=(REASON=' ');
 
  if CLAGE=. then CLAGE=94.325;
  if CLAGE>295 then CLAGE=295;
  if CLNO<10 then CLNO=0;
  if CLNO=. then CLNO= 31.3848;
  /*if CLNO<15 then CLNO= 15;*/
     DEBTINC_MISS=(DEBTINC=.);
  if DELINQ=. then DELINQ=0;
  if DEROG=. then DEROG=0;
  if LOAN>30500 then LOAN=29000;
  if MORTDUE=. then MORTDUE= 54175.95;
  if MORTDUE>140000 then MORTDUE=130000;
  if NINQ=. then NINQ=0;
     VALUE_MISS=(VALUE=.);
  if YOJ=. then YOJ=25.1988;
run;

PROC CONTENTS DATA=MODEL_DEV11;
RUN;
/* Step Five: Run procedure of Logistic to finalize the variables which are included in the final model */

%LET INPUT2=
DEBTINC_MISS
JOB_Mgr
JOB_Office
JOB_Other
JOB_ProfExe
JOB_Sales
JOB_Self
JOB_miss
REASON_DebtCon
REASON_HomeImp
REASON_Miss
VALUE_MISS
clage
clno
delinq
derog
loan
mortdue
ninq
yoj
;

proc logistic data=MODEL_DEV11 descending;
model bad=&input2
  /selection=stepwise fast lackfit rsquare corrb stb;
run;

proc print data=tt.hmeq;
run;

%LET INPUT3=
DEBTINC_Miss 
VALUE_Miss 
CLAGE 
DELINQ 
DEROG 
NINQ
YOJ 
;


proc logistic data=MODEL_DEV11 descending;
model bad=&input3
  /selection=stepwise fast lackfit rsquare corrb stb;
run;


/*
%LET INPUT4=
DEBTINC_MISS
JOB_Office
JOB_Sales
JOB_miss
REASON_HomeImp
VALUE_MISS
clage
clno
delinq
derog
ninq
yoj
;

proc logistic data=MODEL_DEV11 descending;
model bad=&input4
  /selection=stepwise fast lackfit rsquare corrb stb;
run;
*/


/* Step Six: Apply model equation to the data of Val */


data val;
  set MODEL_Val;
  JOB_Mgr=(JOB='Mgr');
  JOB_Office=(JOB='Office');
  JOB_Other=(JOB='Other');
  JOB_ProfExe=(JOB='ProfExe');
  JOB_Sales=(JOB='Sales');
  JOB_Self=(JOB='Self');
  JOB_miss=(JOB=' ');
  REASON_DebtCon=(REASON='DebtCon');
  REASON_HomeImp=(REASON='HomeImp');
  REASON_Miss=(REASON=' ');
 
  if CLAGE=. then CLAGE=94.325;
  if CLAGE>295 then CLAGE=295;
  if CLNO<10 then CLNO=0;
  if CLNO=. then CLNO= 31.3848;
  /*if CLNO<15 then CLNO= 15;*/
     DEBTINC_MISS=(DEBTINC=.);
  if DELINQ=. then DELINQ=0;
  if DEROG=. then DEROG=0;
  if LOAN>30500 then LOAN=29000;
  if MORTDUE=. then MORTDUE= 54175.95;
  if MORTDUE>140000 then MORTDUE=130000;
  if NINQ=. then NINQ=0;
     VALUE_MISS=(VALUE=.);
  if YOJ=. then YOJ=25.1988;

Logit=
-1.6681		
+2.6993		*	DEBTINC_MISS	/*Debt to income ratio IS MISSING */
+3.6737		*	Value_Miss		/* Value of current property is missing */
-0.00703	*	clage			/* Age of the oldest tradeline */
+0.6793	*	delinq				/* Number of delinquent tradeline */
+0.6063	*	derog				/*Number of major derogatory reports */
+0.1243	*	ninq				/*Number of recent credit enquiries */
-0.0279		*	yoj				/*Number of years in current job*/
;
prob=1/(1+exp(-logit)); 
run;

proc sort data=val out=val1;
   by descending prob;
run;

proc rank		data = val1
				out = val_ranked
				groups = 20
				descending;
		var		prob;
		ranks	rank;
run;

data val_ranked(drop=rank prob);
set val_ranked;
	model_rank=rank + 1;
	model_score=prob;
	
run;
ods csv body='rank.csv';
PROC TABULATE DATA = val_ranked MISSING NOSEPS;
            CLASS model_rank        ;
            VAR   model_score     bad  ;
	TABLES model_rank=' ' ALL, model_score*MEAN*F=5.3 
           bad='BAD'*(sum='# of Bad' n='# of Acct' mean*F=5.3)/box='Rank';
RUN;
ods csv close;

proc sort data=val out=val1;
   by descending prob;
run;
%let ds=val1;                   /*output dataset from proc logistic*/
%let response=bad;           /*response variable */

options mprint;
%macro charts(role=);

proc rank data = &ds out = gar;
 where prob^=.;;
 var prob;
 ranks rp;
run;

proc sql;
  select count(*) as tot_obs,
         sum(&response=1) as resp1,
         sum(&response=0) as resp0,
         mean(&response) as resprate
  into :tot_obs, :resp1, :resp0, :resprate
  from gar;
quit;

proc sort data = &ds out=preds1 (keep=prob &response);
 where prob^=.;
 by descending prob;
run;

                     /*** Lift chart and Moving Avg(Gains Chart)***/

data lft (keep=c_resp c_perf c_obs &response prob t_resp m_avg c_prob avg_resp);
 set preds1;

 if _n_ le &resp1 then c_perf = _n_ / &resp1;
 else                  c_perf = 1;

 if &response = 1 then do;
   t_resp+1;
   c_resp = t_resp/&resp1;
 end;
 c_obs = _n_ / &tot_obs;

 c_prob + prob;
 m_avg=c_prob/_n_;

 avg_resp = &resprate;

 attrib
        c_resp label = 'Cumulative Response'
        m_avg  label = 'Predicted Prob'
        c_prob label = 'Cumulative Predicted Prob'
        c_obs  label = 'Cumulative Population';
run;


proc plot data = lft ;                 /*** Lift Chart ***/
 plot (c_resp  c_obs)*c_obs='*' /overlay;
                              
 label c_obs='Cumulative Population'
       c_resp='Cumulative Response';
 
*title "Lift Chart - &role";
run;
title;

proc plot data = lft ;                 /*** Moving Avg.***/
 plot (m_avg avg_resp )*c_obs='*' / overlay ;
 format m_avg c_obs avg_resp percent6.;
*title "Gains Chart - &role";
run;
title;

proc datasets library=work;
  delete preds1 gar lft;
quit;

%mend;

%charts(role=model)

/*
data hmeq;
   input bad           1
         loan        3-7
         mortdue    9-17
         value     19-27
         reason  $ 29-35
         job     $ 37-42
         yoj       44-49
         derog     51-52
         delinq    54-55
         clage     57-63
         ninq      65-66
         clno      68-69
         debtinc   71-77;
		 /*******
label bad    ="Default or seriously delinquent"
      reason ="Home improvement or debt consolidation"
      job    ="Prof/exec sales mngr office self other"
      loan   ="Amount of current loan request"
      mortdue="Amount due on existing mortgage"
      value  ="Value of current property"
      debtinc="Debt to income ratio"
      yoj    ="Years on current job"
      derog  ="Number of major derogatory reports"
      clno   ="Number of trade (credit) lines"
      delinq ="Number of delinquent trade lines"
      clage  ="Age of oldest trade line in months"
      ninq   ="Number of recent credit inquiries"
		 *********/
      ;


