* Encoding: UTF-8.

********STEP 1 CURVE analsysis*********
************************************************
* First code Curve OK = 0 and Bad curve (bad melting curve or insufficient quality) = 1.
IF (Curve1 = 1 AND Curve2 = 0 AND Curve3 = 0) qPC = 1.
IF (Curve1 = 0 AND Curve2 = 1 AND Curve3 = 0) qPC = 1.
IF (Curve1 = 0 AND Curve2 = 0 AND Curve3 = 1) qPC = 1.
IF (Curve1 = 0 AND Curve2 = 0 AND Curve3 = 0) qPC = 2.
RECODE qPC (SYSMIS=0).
EXECUTE.

VALUE LABELS
qPC
0 '<2 good curves'
1 '2 good curves'
2 '3 good curves'.
EXECUTE.

* After step 1, there are 3 options, if there are either 3 good curves, 2 good curves or <2 good curves. 
* All have a different set of codes. However, all codes must always be runned. 

*********************************************************************************************
Option 1: IF THREE GOOD CURVES
*********************************************************************************************
* First rank Lowest, middle and highest Cq.
COMPUTE LowestCq=MIN(Cq1,Cq2,Cq3).
COMPUTE highestCq=MAX(Cq1,Cq2,Cq3).
COMPUTE middleCq= ((Cq1 + Cq2 + Cq3)-(highestCq+ LowestCq)).
EXECUTE.

*****Compute delta's AB, BC and ABC****
* Compute delta A(lowest-middle) and delta B(middle-highest).
COMPUTE Delta_a=SQRT((LowestCq-middleCq)**2).
COMPUTE Delta_b=SQRT((highestCq-middleCq)**2).
COMPUTE Delta_abc=SQRT((highestCq-LowestCq)**2).
EXECUTE.

* Compute the mean of delta AB and delta BC.
IF  (LowestCq  ~= 0 AND middleCq ~= 0) Mean_delta_ab =((LowestCq + middleCq)/2).
IF  (LowestCq  = 0) Mean_delta_ab =(middleCq).
IF  (middleCq  = 0) Mean_delta_ab =(LowestCq).
IF  (highestCq  ~= 0 AND middleCq ~= 0) Mean_delta_bc =((highestCq + middleCq)/2).
IF  (highestCq  = 0) Mean_delta_bc =(middleCq).
IF  (middleCq  = 0) Mean_delta_bc =(highestCq).
IF  (LowestCq  ~= 0 AND middleCq ~= 0  AND highestCq ~= 0) Mean_delta_abc= MEAN(LowestCq, middleCq, highestCq).
IF  (LowestCq  = 0 AND middleCq = 0) Mean_delta_abc =(highestCq).
IF  (LowestCq  = 0 AND middleCq ~= 0) Mean_delta_abc =((middleCq+highestCq)/2).
EXECUTE.

*************STEP 2 DELTA Cq IN RANGE***************
***************************************************************

****For Delta-ABC****

*Delta Cq is dependent on the PCR efficiency of the experiment. 
* This one is always <= 0.5, for every efficiency.  
IF ((Mean_delta_abc < 26.5) AND (Delta_abc <= 0.5))  abc = 1.
* Efficiency is between 1.45 and 1.55, differing Cq values. 
IF (((E >= 1.45) AND (E < 1.55)) AND (Mean_delta_abc >= 26.5 AND Mean_delta_abc < 27.5) AND (Delta_abc <= 0.6)) abc = 1. 
IF (((E >= 1.45) AND (E < 1.55)) AND (Mean_delta_abc >= 27.5 AND Mean_delta_abc < 28.5) AND (Delta_abc <= 0.8)) abc = 1. 
IF (((E >= 1.45) AND (E < 1.55)) AND (Mean_delta_abc >= 28.5 AND Mean_delta_abc < 29.5) AND (Delta_abc <= 0.9)) abc = 1. 
IF (((E >= 1.45) AND (E < 1.55)) AND (Mean_delta_abc >= 29.5 AND Mean_delta_abc < 30.5) AND (Delta_abc <= 1.1)) abc = 1. 
IF (((E >= 1.45) AND (E < 1.55)) AND (Mean_delta_abc >= 30.5 AND Mean_delta_abc < 31.5) AND (Delta_abc <= 1.4)) abc = 1. 
IF (((E >= 1.45) AND (E < 1.55)) AND (Mean_delta_abc >= 31.5 AND Mean_delta_abc < 32.5) AND (Delta_abc <= 1.7)) abc = 1. 
IF (((E >= 1.45) AND (E < 1.55)) AND (Mean_delta_abc >= 32.5 AND Mean_delta_abc < 33.5) AND (Delta_abc <= 2.1)) abc = 1. 
IF (((E >= 1.45) AND (E < 1.55)) AND (Mean_delta_abc >= 33.5 AND Mean_delta_abc < 34.5) AND (Delta_abc <= 2.7)) abc = 1. 
IF (((E >= 1.45) AND (E < 1.55)) AND (Mean_delta_abc >= 34.5 AND Mean_delta_abc < 35.5) AND (Delta_abc <= 3.3)) abc = 1. 
* Efficiency is between 1.55 and 1.65, differing Cq values. 
IF (((E >= 1.55) AND (E < 1.65)) AND (Mean_delta_abc < 28.5) AND (Delta_abc <= 0.5)) abc = 1. 
IF (((E >= 1.55) AND (E < 1.65)) AND (Mean_delta_abc >= 28.5 AND Mean_delta_abc < 29.5) AND (Delta_abc <= 0.7)) abc = 1. 
IF (((E >= 1.55) AND (E < 1.65)) AND (Mean_delta_abc >= 29.5 AND Mean_delta_abc < 30.5) AND (Delta_abc <= 0.8)) abc = 1. 
IF (((E >= 1.55) AND (E < 1.65)) AND (Mean_delta_abc >= 30.5 AND Mean_delta_abc < 31.5) AND (Delta_abc <= 1.1)) abc = 1. 
IF (((E >= 1.55) AND (E < 1.65)) AND (Mean_delta_abc >= 31.5 AND Mean_delta_abc < 32.5) AND (Delta_abc <= 1.4)) abc = 1. 
IF (((E >= 1.55) AND (E < 1.65)) AND (Mean_delta_abc >= 32.5 AND Mean_delta_abc < 33.5) AND (Delta_abc <= 1.7)) abc = 1. 
IF (((E >= 1.55) AND (E < 1.65)) AND (Mean_delta_abc >= 33.5 AND Mean_delta_abc < 34.5) AND (Delta_abc <= 2.2)) abc = 1. 
IF (((E >= 1.55) AND (E < 1.65)) AND (Mean_delta_abc >= 34.5 AND Mean_delta_abc < 35.5) AND (Delta_abc <= 2.9)) abc = 1. 
* Efficiency is between 1.65 and 1.75, differing Cq values. 
IF (((E >= 1.65) AND (E < 1.75)) AND (Mean_delta_abc < 29.5) AND (Delta_abc <= 0.5)) abc = 1. 
IF (((E >= 1.65) AND (E < 1.75)) AND (Mean_delta_abc >= 29.5 AND Mean_delta_abc < 30.5) AND (Delta_abc <= 0.6)) abc = 1. 
IF (((E >= 1.65) AND (E < 1.75)) AND (Mean_delta_abc >= 30.5 AND Mean_delta_abc < 31.5) AND (Delta_abc <= 0.8)) abc = 1. 
IF (((E >= 1.65) AND (E < 1.75)) AND (Mean_delta_abc >= 31.5 AND Mean_delta_abc < 32.5) AND (Delta_abc <= 1.1)) abc = 1. 
IF (((E >= 1.65) AND (E < 1.75)) AND (Mean_delta_abc >= 32.5 AND Mean_delta_abc < 33.5) AND (Delta_abc <= 1.5)) abc = 1. 
IF (((E >= 1.65) AND (E < 1.75)) AND (Mean_delta_abc >= 33.5 AND Mean_delta_abc < 34.5) AND (Delta_abc <= 1.9)) abc = 1. 
IF (((E >= 1.65) AND (E < 1.75)) AND (Mean_delta_abc >= 34.5 AND Mean_delta_abc < 35.5) AND (Delta_abc <= 2.5)) abc = 1.
* Efficiency is between 1.75 and 1.85, differing Cq values. 
IF (((E >= 1.75) AND (E < 1.85)) AND (Mean_delta_abc < 30.5) AND (Delta_abc <= 0.5)) abc = 1. 
IF (((E >= 1.75) AND (E < 1.85)) AND (Mean_delta_abc >= 30.5 AND Mean_delta_abc < 31.5) AND (Delta_abc <= 0.7)) abc = 1. 
IF (((E >= 1.75) AND (E < 1.85)) AND (Mean_delta_abc >= 31.5 AND Mean_delta_abc < 32.5) AND (Delta_abc <= 0.9)) abc = 1. 
IF (((E >= 1.75) AND (E < 1.85)) AND (Mean_delta_abc >= 32.5 AND Mean_delta_abc < 33.5) AND (Delta_abc <= 1.2)) abc = 1. 
IF (((E >= 1.75) AND (E < 1.85)) AND (Mean_delta_abc >= 33.5 AND Mean_delta_abc < 34.5) AND (Delta_abc <= 1.7)) abc = 1. 
IF (((E >= 1.75) AND (E < 1.85)) AND (Mean_delta_abc >= 34.5 AND Mean_delta_abc < 35.5) AND (Delta_abc <= 2.3)) abc = 1. 
* Efficiency is between 1.85 and 1.95, differing Cq values. 
IF (((E >= 1.85) AND (E < 1.95)) AND (Mean_delta_abc < 31.5) AND (Delta_abc <= 0.5)) abc = 1. 
IF (((E >= 1.85) AND (E < 1.95)) AND (Mean_delta_abc >= 31.5 AND Mean_delta_abc < 32.5) AND (Delta_abc <= 0.8)) abc = 1.
IF (((E >= 1.85) AND (E < 1.95)) AND (Mean_delta_abc >= 32.5 AND Mean_delta_abc < 33.5) AND (Delta_abc <= 1.1)) abc = 1. 
IF (((E >= 1.85) AND (E < 1.95)) AND (Mean_delta_abc >= 33.5 AND Mean_delta_abc < 34.5) AND (Delta_abc <= 1.5)) abc = 1. 
IF (((E >= 1.85) AND (E < 1.95)) AND (Mean_delta_abc >= 34.5 AND Mean_delta_abc < 35.5) AND (Delta_abc <= 2.1)) abc = 1.
* Efficiency is between 1.95 and 2.05, differing Cq values.
IF (((E >= 1.95) AND (E < 2.05)) AND (Mean_delta_abc < 31.5) AND (Delta_abc <= 0.5)) abc = 1. 
IF (((E >= 1.95) AND (E < 2.05)) AND (Mean_delta_abc >= 31.5 AND Mean_delta_abc < 32.5) AND (Delta_abc <= 0.7)) abc = 1.
IF (((E >= 1.95) AND (E < 2.05)) AND (Mean_delta_abc >= 32.5 AND Mean_delta_abc < 33.5) AND (Delta_abc <= 0.9)) abc = 1.
IF (((E >= 1.95) AND (E < 2.05)) AND (Mean_delta_abc >= 33.5 AND Mean_delta_abc < 34.5) AND (Delta_abc <= 1.3)) abc = 1. 
IF (((E >= 1.95) AND (E < 2.05)) AND (Mean_delta_abc >= 34.5 AND Mean_delta_abc < 35.5) AND (Delta_abc <= 1.9)) abc = 1.
RECODE abc (SYSMIS=0).
EXECUTE.

VALUE LABELS
abc
1  'acceptable', 
0 'not-acceptable'.
EXECUTE.


****For Delta-AB****

*Delta Cq is dependent on the PCR efficiency of the experiment. 
* This one is always <= 0.5, for every efficiency.  
IF ((Mean_delta_ab < 26.5) AND (Delta_a <= 0.5))  a = 1.
* Efficiency is between 1.45 and 1.55, differing Cq values. 
IF (((E >= 1.45) AND (E < 1.55)) AND (Mean_delta_ab >= 26.5 AND Mean_delta_ab < 27.5) AND (Delta_a <= 0.6)) a = 1. 
IF (((E >= 1.45) AND (E < 1.55)) AND (Mean_delta_ab >= 27.5 AND Mean_delta_ab < 28.5) AND (Delta_a <= 0.8)) a = 1. 
IF (((E >= 1.45) AND (E < 1.55)) AND (Mean_delta_ab >= 28.5 AND Mean_delta_ab < 29.5) AND (Delta_a <= 0.9)) a = 1. 
IF (((E >= 1.45) AND (E < 1.55)) AND (Mean_delta_ab >= 29.5 AND Mean_delta_ab < 30.5) AND (Delta_a <= 1.1)) a = 1. 
IF (((E >= 1.45) AND (E < 1.55)) AND (Mean_delta_ab >= 30.5 AND Mean_delta_ab < 31.5) AND (Delta_a <= 1.4)) a = 1. 
IF (((E >= 1.45) AND (E < 1.55)) AND (Mean_delta_ab >= 31.5 AND Mean_delta_ab < 32.5) AND (Delta_a <= 1.7)) a = 1. 
IF (((E >= 1.45) AND (E < 1.55)) AND (Mean_delta_ab >= 32.5 AND Mean_delta_ab < 33.5) AND (Delta_a <= 2.1)) a = 1. 
IF (((E >= 1.45) AND (E < 1.55)) AND (Mean_delta_ab >= 33.5 AND Mean_delta_ab < 34.5) AND (Delta_a <= 2.7)) a = 1. 
IF (((E >= 1.45) AND (E < 1.55)) AND (Mean_delta_ab >= 34.5 AND Mean_delta_ab < 35.5) AND (Delta_a <= 3.3)) a = 1. 
* Efficiency is between 1.55 and 1.65, differing Cq values. 
IF (((E >= 1.55) AND (E < 1.65)) AND (Mean_delta_ab < 28.5) AND (Delta_a <= 0.5)) a = 1. 
IF (((E >= 1.55) AND (E < 1.65)) AND (Mean_delta_ab >= 28.5 AND Mean_delta_ab < 29.5) AND (Delta_a <= 0.7)) a = 1. 
IF (((E >= 1.55) AND (E < 1.65)) AND (Mean_delta_ab >= 29.5 AND Mean_delta_ab < 30.5) AND (Delta_a <= 0.8)) a = 1. 
IF (((E >= 1.55) AND (E < 1.65)) AND (Mean_delta_ab >= 30.5 AND Mean_delta_ab < 31.5) AND (Delta_a <= 1.1)) a = 1. 
IF (((E >= 1.55) AND (E < 1.65)) AND (Mean_delta_ab >= 31.5 AND Mean_delta_ab < 32.5) AND (Delta_a <= 1.4)) a = 1. 
IF (((E >= 1.55) AND (E < 1.65)) AND (Mean_delta_ab >= 32.5 AND Mean_delta_ab < 33.5) AND (Delta_a <= 1.7)) a = 1. 
IF (((E >= 1.55) AND (E < 1.65)) AND (Mean_delta_ab >= 33.5 AND Mean_delta_ab < 34.5) AND (Delta_a <= 2.2)) a = 1. 
IF (((E >= 1.55) AND (E < 1.65)) AND (Mean_delta_ab >= 34.5 AND Mean_delta_ab < 35.5) AND (Delta_a <= 2.9)) a = 1. 
* Efficiency is between 1.65 and 1.75, differing Cq values. 
IF (((E >= 1.65) AND (E < 1.75)) AND (Mean_delta_ab < 29.5) AND (Delta_a <= 0.5)) a = 1. 
IF (((E >= 1.65) AND (E < 1.75)) AND (Mean_delta_ab >= 29.5 AND Mean_delta_ab < 30.5) AND (Delta_a <= 0.6)) a = 1. 
IF (((E >= 1.65) AND (E < 1.75)) AND (Mean_delta_ab >= 30.5 AND Mean_delta_ab < 31.5) AND (Delta_a <= 0.8)) a = 1. 
IF (((E >= 1.65) AND (E < 1.75)) AND (Mean_delta_ab >= 31.5 AND Mean_delta_ab < 32.5) AND (Delta_a <= 1.1)) a = 1. 
IF (((E >= 1.65) AND (E < 1.75)) AND (Mean_delta_ab >= 32.5 AND Mean_delta_ab < 33.5) AND (Delta_a <= 1.5)) a = 1. 
IF (((E >= 1.65) AND (E < 1.75)) AND (Mean_delta_ab >= 33.5 AND Mean_delta_ab < 34.5) AND (Delta_a <= 1.9)) a = 1. 
IF (((E >= 1.65) AND (E < 1.75)) AND (Mean_delta_ab >= 34.5 AND Mean_delta_ab < 35.5) AND (Delta_a <= 2.5)) a = 1.
* Efficiency is between 1.75 and 1.85, differing Cq values. 
IF (((E >= 1.75) AND (E < 1.85)) AND (Mean_delta_ab < 30.5) AND (Delta_a <= 0.5)) a = 1. 
IF (((E >= 1.75) AND (E < 1.85)) AND (Mean_delta_ab >= 30.5 AND Mean_delta_ab < 31.5) AND (Delta_a <= 0.7)) a = 1. 
IF (((E >= 1.75) AND (E < 1.85)) AND (Mean_delta_ab >= 31.5 AND Mean_delta_ab < 32.5) AND (Delta_a <= 0.9)) a = 1. 
IF (((E >= 1.75) AND (E < 1.85)) AND (Mean_delta_ab >= 32.5 AND Mean_delta_ab < 33.5) AND (Delta_a <= 1.2)) a = 1. 
IF (((E >= 1.75) AND (E < 1.85)) AND (Mean_delta_ab >= 33.5 AND Mean_delta_ab < 34.5) AND (Delta_a <= 1.7)) a = 1. 
IF (((E >= 1.75) AND (E < 1.85)) AND (Mean_delta_ab >= 34.5 AND Mean_delta_ab < 35.5) AND (Delta_a <= 2.3)) a = 1. 
* Efficiency is between 1.85 and 1.95, differing Cq values. 
IF (((E >= 1.85) AND (E < 1.95)) AND (Mean_delta_ab < 31.5) AND (Delta_a <= 0.5)) a = 1. 
IF (((E >= 1.85) AND (E < 1.95)) AND (Mean_delta_ab >= 31.5 AND Mean_delta_ab < 32.5) AND (Delta_a <= 0.8)) a = 1.
IF (((E >= 1.85) AND (E < 1.95)) AND (Mean_delta_ab >= 32.5 AND Mean_delta_ab < 33.5) AND (Delta_a <= 1.1)) a = 1. 
IF (((E >= 1.85) AND (E < 1.95)) AND (Mean_delta_ab >= 33.5 AND Mean_delta_ab < 34.5) AND (Delta_a <= 1.5)) a = 1. 
IF (((E >= 1.85) AND (E < 1.95)) AND (Mean_delta_ab >= 34.5 AND Mean_delta_ab < 35.5) AND (Delta_a <= 2.1)) a = 1.
* Efficiency is between 1.95 and 2.05, differing Cq values.
IF (((E >= 1.95) AND (E < 2.05)) AND (Mean_delta_ab < 31.5) AND (Delta_a <= 0.5)) a = 1. 
IF (((E >= 1.95) AND (E < 2.05)) AND (Mean_delta_ab >= 31.5 AND Mean_delta_ab < 32.5) AND (Delta_a <= 0.7)) a = 1.
IF (((E >= 1.95) AND (E < 2.05)) AND (Mean_delta_ab >= 32.5 AND Mean_delta_ab < 33.5) AND (Delta_a <= 0.9)) a = 1.
IF (((E >= 1.95) AND (E < 2.05)) AND (Mean_delta_ab >= 33.5 AND Mean_delta_ab < 34.5) AND (Delta_a <= 1.3)) a = 1. 
IF (((E >= 1.95) AND (E < 2.05)) AND (Mean_delta_ab >= 34.5 AND Mean_delta_ab < 35.5) AND (Delta_a <= 1.9)) a = 1.
RECODE a (SYSMIS=0).
EXECUTE.

VALUE LABELS
a
1  'acceptable', 
0 'not-acceptable'.
EXECUTE.

****For Delta-BC****

*Delta Cq is dependent on the PCR efficiency of the experiment. 
* This one is always <= 0.5, for every efficiency.  
IF ((Mean_delta_bc < 26.5) AND (Delta_b <= 0.5))  b = 1.
* Efficiency is between 1.45 and 1.55, differing Cq values. 
IF (((E >= 1.45) AND (E < 1.55)) AND (Mean_delta_bc >= 26.5 AND Mean_delta_bc < 27.5) AND (Delta_b <= 0.6)) b = 1. 
IF (((E >= 1.45) AND (E < 1.55)) AND (Mean_delta_bc >= 27.5 AND Mean_delta_bc < 28.5) AND (Delta_b <= 0.8)) b = 1. 
IF (((E >= 1.45) AND (E < 1.55)) AND (Mean_delta_bc >= 28.5 AND Mean_delta_bc < 29.5) AND (Delta_b <= 0.9)) b = 1. 
IF (((E >= 1.45) AND (E < 1.55)) AND (Mean_delta_bc >= 29.5 AND Mean_delta_bc < 30.5) AND (Delta_b <= 1.1)) b = 1. 
IF (((E >= 1.45) AND (E < 1.55)) AND (Mean_delta_bc >= 30.5 AND Mean_delta_bc < 31.5) AND (Delta_b <= 1.4)) b = 1. 
IF (((E >= 1.45) AND (E < 1.55)) AND (Mean_delta_bc >= 31.5 AND Mean_delta_bc < 32.5) AND (Delta_b <= 1.7)) b = 1. 
IF (((E >= 1.45) AND (E < 1.55)) AND (Mean_delta_bc >= 32.5 AND Mean_delta_bc < 33.5) AND (Delta_b <= 2.1)) b = 1. 
IF (((E >= 1.45) AND (E < 1.55)) AND (Mean_delta_bc >= 33.5 AND Mean_delta_bc < 34.5) AND (Delta_b <= 2.7)) b = 1. 
IF (((E >= 1.45) AND (E < 1.55)) AND (Mean_delta_bc >= 34.5 AND Mean_delta_bc < 35.5) AND (Delta_b <= 3.3)) b = 1. 
* Efficiency is between 1.55 and 1.65, differing Cq values. 
IF (((E >= 1.55) AND (E < 1.65)) AND (Mean_delta_bc < 28.5) AND (Delta_b <= 0.5)) b = 1. 
IF (((E >= 1.55) AND (E < 1.65)) AND (Mean_delta_bc >= 28.5 AND Mean_delta_bc < 29.5) AND (Delta_b <= 0.7)) b = 1. 
IF (((E >= 1.55) AND (E < 1.65)) AND (Mean_delta_bc >= 29.5 AND Mean_delta_bc < 30.5) AND (Delta_b <= 0.8)) b = 1. 
IF (((E >= 1.55) AND (E < 1.65)) AND (Mean_delta_bc >= 30.5 AND Mean_delta_bc < 31.5) AND (Delta_b <= 1.1)) b = 1. 
IF (((E >= 1.55) AND (E < 1.65)) AND (Mean_delta_bc >= 31.5 AND Mean_delta_bc < 32.5) AND (Delta_b <= 1.4)) b = 1. 
IF (((E >= 1.55) AND (E < 1.65)) AND (Mean_delta_bc >= 32.5 AND Mean_delta_bc < 33.5) AND (Delta_b <= 1.7)) b = 1. 
IF (((E >= 1.55) AND (E < 1.65)) AND (Mean_delta_bc >= 33.5 AND Mean_delta_bc < 34.5) AND (Delta_b <= 2.2)) b = 1. 
IF (((E >= 1.55) AND (E < 1.65)) AND (Mean_delta_bc >= 34.5 AND Mean_delta_bc < 35.5) AND (Delta_b <= 2.9)) b = 1. 
* Efficiency is between 1.65 and 1.75, differing Cq values. 
IF (((E >= 1.65) AND (E < 1.75)) AND (Mean_delta_bc < 29.5) AND (Delta_b <= 0.5)) b = 1. 
IF (((E >= 1.65) AND (E < 1.75)) AND (Mean_delta_bc >= 29.5 AND Mean_delta_bc < 30.5) AND (Delta_b <= 0.6)) b = 1. 
IF (((E >= 1.65) AND (E < 1.75)) AND (Mean_delta_bc >= 30.5 AND Mean_delta_bc < 31.5) AND (Delta_b <= 0.8)) b = 1. 
IF (((E >= 1.65) AND (E < 1.75)) AND (Mean_delta_bc >= 31.5 AND Mean_delta_bc < 32.5) AND (Delta_b <= 1.1)) b = 1. 
IF (((E >= 1.65) AND (E < 1.75)) AND (Mean_delta_bc >= 32.5 AND Mean_delta_bc < 33.5) AND (Delta_b <= 1.5)) b = 1. 
IF (((E >= 1.65) AND (E < 1.75)) AND (Mean_delta_bc >= 33.5 AND Mean_delta_bc < 34.5) AND (Delta_b <= 1.9)) b = 1. 
IF (((E >= 1.65) AND (E < 1.75)) AND (Mean_delta_bc >= 34.5 AND Mean_delta_bc < 35.5) AND (Delta_b <= 2.5)) b = 1.
* Efficiency is between 1.75 and 1.85, differing Cq values. 
IF (((E >= 1.75) AND (E < 1.85)) AND (Mean_delta_bc < 30.5) AND (Delta_b <= 0.5)) b = 1. 
IF (((E >= 1.75) AND (E < 1.85)) AND (Mean_delta_bc >= 30.5 AND Mean_delta_bc < 31.5) AND (Delta_b <= 0.7)) b = 1. 
IF (((E >= 1.75) AND (E < 1.85)) AND (Mean_delta_bc >= 31.5 AND Mean_delta_bc < 32.5) AND (Delta_b <= 0.9)) b = 1. 
IF (((E >= 1.75) AND (E < 1.85)) AND (Mean_delta_bc >= 32.5 AND Mean_delta_bc < 33.5) AND (Delta_b <= 1.2)) b = 1. 
IF (((E >= 1.75) AND (E < 1.85)) AND (Mean_delta_bc >= 33.5 AND Mean_delta_bc < 34.5) AND (Delta_b <= 1.7)) b = 1. 
IF (((E >= 1.75) AND (E < 1.85)) AND (Mean_delta_bc >= 34.5 AND Mean_delta_bc < 35.5) AND (Delta_b <= 2.3)) b = 1. 
* Efficiency is between 1.85 and 1.95, differing Cq values. 
IF (((E >= 1.85) AND (E < 1.95)) AND (Mean_delta_bc < 31.5) AND (Delta_b <= 0.5)) b = 1. 
IF (((E >= 1.85) AND (E < 1.95)) AND (Mean_delta_bc >= 31.5 AND Mean_delta_bc < 32.5) AND (Delta_b <= 0.8)) b = 1.
IF (((E >= 1.85) AND (E < 1.95)) AND (Mean_delta_bc >= 32.5 AND Mean_delta_bc < 33.5) AND (Delta_b <= 1.1)) b = 1. 
IF (((E >= 1.85) AND (E < 1.95)) AND (Mean_delta_bc >= 33.5 AND Mean_delta_bc < 34.5) AND (Delta_b <= 1.5)) b = 1. 
IF (((E >= 1.85) AND (E < 1.95)) AND (Mean_delta_bc >= 34.5 AND Mean_delta_bc < 35.5) AND (Delta_b <= 2.1)) b = 1.
* Efficiency is between 1.95 and 2.05, differing Cq values.
IF (((E >= 1.95) AND (E < 2.05)) AND (Mean_delta_bc < 31.5) AND (Delta_b <= 0.5)) b = 1. 
IF (((E >= 1.95) AND (E < 2.05)) AND (Mean_delta_bc >= 31.5 AND Mean_delta_bc < 32.5) AND (Delta_b <= 0.7)) b = 1.
IF (((E >= 1.95) AND (E < 2.05)) AND (Mean_delta_bc >= 32.5 AND Mean_delta_bc < 33.5) AND (Delta_b <= 0.9)) b = 1.
IF (((E >= 1.95) AND (E < 2.05)) AND (Mean_delta_bc >= 33.5 AND Mean_delta_bc < 34.5) AND (Delta_b <= 1.3)) b = 1. 
IF (((E >= 1.95) AND (E < 2.05)) AND (Mean_delta_bc >= 34.5 AND Mean_delta_bc < 35.5) AND (Delta_b <= 1.9)) b = 1.
RECODE b (SYSMIS=0).
EXECUTE.

VALUE LABELS
b
1  'acceptable', 
0 'not-acceptable'.
EXECUTE.

* Calculates which deltas are in range.
IF (a = 1 AND b =0) range = 1.
IF (a = 0 AND b =1) range = 2.
IF (abc = 1) range = 3.
IF (a = 0 AND b =0) range = 0.
EXECUTE.

VALUE LABELS
range
1  'delta AB in range', 
2  'delta BC in range', 
3  'both deltas in range' 
0 'none of deltas in range'.
EXECUTE.

*************STEP 3 Cq > critical value (35)***************
************************************************************
* Makes 3 variables: Cq of AB between 15-35, Cq of BC between 15-35 and Cq of all between 15-35 (mean of AB and BC are necessary to proceed if one is out of range but the other are in range).
IF ((RANGE(mean_delta_abc,15,35))) CQ35ABC = 1.
IF ((RANGE(Mean_delta_ab,15,35))) CQ35AB = 1.
IF ((RANGE(Mean_delta_bc,15,35))) CQ35BC = 1.
RECODE CQ35ABC (SYSMIS=0).
RECODE CQ35AB (SYSMIS=0).
RECODE CQ35BC (SYSMIS=0).
EXECUTE.

VALUE LABELS
CQ35AB
0 'Cq >35'
1  'Cq 15-35'.
VALUE LABELS
CQ35BC
0 'Cq >35'
1  'Cq 15-35'.
VALUE LABELS
CQ35ABC
0 'Cq >35'
1  'Cq 15-35'.
EXECUTE.

***********FINAL RESULT FOR 3 GOOD CURVES*************
**********************************************************************
* For 3 good curves and 3 delta's in range. 
IF (qPC = 2 AND range = 3 AND CQ35ABC = 1) QC = 3.
IF (qPC = 2 AND range = 3 AND CQ35ABC = 0) QC = 0.
* For 3 good curves and delta AB in range. 
IF (qPC = 2 AND range = 1 AND CQ35AB = 1) QC = 1.
IF (qPC = 2 AND range = 1 AND CQ35AB = 0) QC = 0.
* For 3 good curves and delta BC in range. 
IF (qPC = 2 AND range = 2 AND CQ35BC = 1) QC = 2.
IF (qPC = 2 AND range = 2 AND CQ35BC = 0) QC = 0.
* For 3 good curves and delta's not in range. 
IF (qPC = 2 AND range = 0 AND CQ35ABC = 1) QC = 5.
IF (qPC = 2 AND range = 0 AND CQ35ABC = 0) QC = 0.
EXECUTE.

VALUE LABELS
QC
1 'mean AB valid'
2 'mean BC valid'
3 'mean all valid'
4 'mean duplos valid'
5 'invalid'
0 'undetectable'.
EXECUTE.

*********************************************************************************************
Option 2: IF TWO GOOD CURVES
*********************************************************************************************
* If there are 2 good melting curves, one can proceed using these 2 curves and condider them duplos.
* To calculate these duplos.
IF (qPC =1 AND Curve1 = 0 AND Curve2 = 0) Duplo_Cq1 = Cq1. 
IF (qPC =1 AND Curve1 = 0 AND Curve3 = 0) Duplo_Cq1 = Cq1. 
IF (qPC =1 AND Curve2 = 0 AND Curve3 = 0) Duplo_Cq1 = Cq2.
IF (qPC =1 AND Curve1 = 0 AND Curve2 = 0) Duplo_Cq2 = Cq2. 
IF (qPC =1 AND Curve1 = 0 AND Curve3 = 0) Duplo_Cq2 = Cq3.  
IF (qPC =1 AND Curve2 = 0 AND Curve3 = 0) Duplo_Cq2 = Cq3. 
EXECUTE.

***************************** 
Calculate mean Cq of the duplos
-Condition: If one of the Cq's is 0, then these will not be calculated with and the mean is shown as the Cq of the working duplo.
IF  (Duplo_Cq1 ~= 0 AND Duplo_Cq2 ~= 0) Duplo_mean_Cq=((Duplo_Cq1 + Duplo_Cq2)/2).
IF  (Duplo_Cq1 = 0) Duplo_mean_Cq =(Duplo_Cq2).
IF  (Duplo_Cq2 = 0) Duplo_mean_Cq =(Duplo_Cq1).
EXECUTE.

*compute the difference between the 2 duplos.
COMPUTE Duplo_Cq_diff= SQRT((Duplo_Cq1 - Duplo_Cq2)**2).
EXECUTE.

********STEP 2: DELTA IN RANGE***************
***************************************************

*Delta Cq is dependent on the PCR efficiency of the experiment. Please fill in the appropriate max Cq range based on Table 1 in the paper. 
* This one is always <= 0.5, for every efficiency.  
IF ((Duplo_mean_Cq < 26.5) AND (Duplo_Cq_diff <= 0.5))  Duplo_Cq_diff_acceptable = 1.
* Efficiency is between 1.45 and 1.55, differing Cq values. 
IF (((E >= 1.45) AND (E < 1.55)) AND (Duplo_mean_Cq >= 26.5 AND Duplo_mean_Cq < 27.5) AND (Duplo_Cq_diff <= 0.6)) Duplo_Cq_diff_acceptable = 1. 
IF (((E >= 1.45) AND (E < 1.55)) AND (Duplo_mean_Cq >= 27.5 AND Duplo_mean_Cq < 28.5) AND (Duplo_Cq_diff <= 0.8)) Duplo_Cq_diff_acceptable = 1. 
IF (((E >= 1.45) AND (E < 1.55)) AND (Duplo_mean_Cq >= 28.5 AND Duplo_mean_Cq < 29.5) AND (Duplo_Cq_diff <= 0.9)) Duplo_Cq_diff_acceptable = 1. 
IF (((E >= 1.45) AND (E < 1.55)) AND (Duplo_mean_Cq >= 29.5 AND Duplo_mean_Cq < 30.5) AND (Duplo_Cq_diff <= 1.1)) Duplo_Cq_diff_acceptable = 1. 
IF (((E >= 1.45) AND (E < 1.55)) AND (Duplo_mean_Cq >= 30.5 AND Duplo_mean_Cq < 31.5) AND (Duplo_Cq_diff <= 1.4)) Duplo_Cq_diff_acceptable = 1. 
IF (((E >= 1.45) AND (E < 1.55)) AND (Duplo_mean_Cq >= 31.5 AND Duplo_mean_Cq < 32.5) AND (Duplo_Cq_diff <= 1.7)) Duplo_Cq_diff_acceptable = 1. 
IF (((E >= 1.45) AND (E < 1.55)) AND (Duplo_mean_Cq >= 32.5 AND Duplo_mean_Cq < 33.5) AND (Duplo_Cq_diff <= 2.1)) Duplo_Cq_diff_acceptable = 1. 
IF (((E >= 1.45) AND (E < 1.55)) AND (Duplo_mean_Cq >= 33.5 AND Duplo_mean_Cq < 34.5) AND (Duplo_Cq_diff <= 2.7)) Duplo_Cq_diff_acceptable = 1. 
IF (((E >= 1.45) AND (E < 1.55)) AND (Duplo_mean_Cq >= 34.5 AND Duplo_mean_Cq < 35.5) AND (Duplo_Cq_diff <= 3.3)) Duplo_Cq_diff_acceptable = 1. 
* Efficiency is between 1.55 and 1.65, differing Cq values. 
IF (((E >= 1.55) AND (E < 1.65)) AND (Duplo_mean_Cq < 28.5) AND (Duplo_Cq_diff <= 0.5)) Duplo_Cq_diff_acceptable = 1. 
IF (((E >= 1.55) AND (E < 1.65)) AND (Duplo_mean_Cq >= 28.5 AND Duplo_mean_Cq < 29.5) AND (Duplo_Cq_diff <= 0.7)) Duplo_Cq_diff_acceptable = 1. 
IF (((E >= 1.55) AND (E < 1.65)) AND (Duplo_mean_Cq >= 29.5 AND Duplo_mean_Cq < 30.5) AND (Duplo_Cq_diff <= 0.8)) Duplo_Cq_diff_acceptable = 1. 
IF (((E >= 1.55) AND (E < 1.65)) AND (Duplo_mean_Cq >= 30.5 AND Duplo_mean_Cq < 31.5) AND (Duplo_Cq_diff <= 1.1)) Duplo_Cq_diff_acceptable = 1. 
IF (((E >= 1.55) AND (E < 1.65)) AND (Duplo_mean_Cq >= 31.5 AND Duplo_mean_Cq < 32.5) AND (Duplo_Cq_diff <= 1.4)) Duplo_Cq_diff_acceptable = 1. 
IF (((E >= 1.55) AND (E < 1.65)) AND (Duplo_mean_Cq >= 32.5 AND Duplo_mean_Cq < 33.5) AND (Duplo_Cq_diff <= 1.7)) Duplo_Cq_diff_acceptable = 1. 
IF (((E >= 1.55) AND (E < 1.65)) AND (Duplo_mean_Cq >= 33.5 AND Duplo_mean_Cq < 34.5) AND (Duplo_Cq_diff <= 2.2)) Duplo_Cq_diff_acceptable = 1. 
IF (((E >= 1.55) AND (E < 1.65)) AND (Duplo_mean_Cq >= 34.5 AND Duplo_mean_Cq < 35.5) AND (Duplo_Cq_diff <= 2.9)) Duplo_Cq_diff_acceptable = 1. 
* Efficiency is between 1.65 and 1.75, differing Cq values. 
IF (((E >= 1.65) AND (E < 1.75)) AND (Duplo_mean_Cq < 29.5) AND (Duplo_Cq_diff <= 0.5)) Duplo_Cq_diff_acceptable = 1. 
IF (((E >= 1.65) AND (E < 1.75)) AND (Duplo_mean_Cq >= 29.5 AND Duplo_mean_Cq < 30.5) AND (Duplo_Cq_diff <= 0.6)) Duplo_Cq_diff_acceptable = 1. 
IF (((E >= 1.65) AND (E < 1.75)) AND (Duplo_mean_Cq >= 30.5 AND Duplo_mean_Cq < 31.5) AND (Duplo_Cq_diff <= 0.8)) Duplo_Cq_diff_acceptable = 1. 
IF (((E >= 1.65) AND (E < 1.75)) AND (Duplo_mean_Cq >= 31.5 AND Duplo_mean_Cq < 32.5) AND (Duplo_Cq_diff <= 1.1)) Duplo_Cq_diff_acceptable = 1. 
IF (((E >= 1.65) AND (E < 1.75)) AND (Duplo_mean_Cq >= 32.5 AND Duplo_mean_Cq < 33.5) AND (Duplo_Cq_diff <= 1.5)) Duplo_Cq_diff_acceptable = 1. 
IF (((E >= 1.65) AND (E < 1.75)) AND (Duplo_mean_Cq >= 33.5 AND Duplo_mean_Cq < 34.5) AND (Duplo_Cq_diff <= 1.9)) Duplo_Cq_diff_acceptable = 1. 
IF (((E >= 1.65) AND (E < 1.75)) AND (Duplo_mean_Cq >= 34.5 AND Duplo_mean_Cq < 35.5) AND (Duplo_Cq_diff <= 2.5)) Duplo_Cq_diff_acceptable = 1.
* Efficiency is between 1.75 and 1.85, differing Cq values. 
IF (((E >= 1.75) AND (E < 1.85)) AND (Duplo_mean_Cq < 30.5) AND (Duplo_Cq_diff <= 0.5)) Duplo_Cq_diff_acceptable = 1. 
IF (((E >= 1.75) AND (E < 1.85)) AND (Duplo_mean_Cq >= 30.5 AND Duplo_mean_Cq < 31.5) AND (Duplo_Cq_diff <= 0.7)) Duplo_Cq_diff_acceptable = 1. 
IF (((E >= 1.75) AND (E < 1.85)) AND (Duplo_mean_Cq >= 31.5 AND Duplo_mean_Cq < 32.5) AND (Duplo_Cq_diff <= 0.9)) Duplo_Cq_diff_acceptable = 1. 
IF (((E >= 1.75) AND (E < 1.85)) AND (Duplo_mean_Cq >= 32.5 AND Duplo_mean_Cq < 33.5) AND (Duplo_Cq_diff <= 1.2)) Duplo_Cq_diff_acceptable = 1. 
IF (((E >= 1.75) AND (E < 1.85)) AND (Duplo_mean_Cq >= 33.5 AND Duplo_mean_Cq < 34.5) AND (Duplo_Cq_diff <= 1.7)) Duplo_Cq_diff_acceptable = 1. 
IF (((E >= 1.75) AND (E < 1.85)) AND (Duplo_mean_Cq >= 34.5 AND Duplo_mean_Cq < 35.5) AND (Duplo_Cq_diff <= 2.3)) Duplo_Cq_diff_acceptable = 1. 
* Efficiency is between 1.85 and 1.95, differing Cq values. 
IF (((E >= 1.85) AND (E < 1.95)) AND (Duplo_mean_Cq < 31.5) AND (Duplo_Cq_diff <= 0.5)) Duplo_Cq_diff_acceptable = 1. 
IF (((E >= 1.85) AND (E < 1.95)) AND (Duplo_mean_Cq >= 31.5 AND Duplo_mean_Cq < 32.5) AND (Duplo_Cq_diff <= 0.8)) Duplo_Cq_diff_acceptable = 1. 
IF (((E >= 1.85) AND (E < 1.95)) AND (Duplo_mean_Cq >= 32.5 AND Duplo_mean_Cq < 33.5) AND (Duplo_Cq_diff <= 1.1)) Duplo_Cq_diff_acceptable = 1. 
IF (((E >= 1.85) AND (E < 1.95)) AND (Duplo_mean_Cq >= 33.5 AND Duplo_mean_Cq < 34.5) AND (Duplo_Cq_diff <= 1.5)) Duplo_Cq_diff_acceptable = 1. 
IF (((E >= 1.85) AND (E < 1.95)) AND (Duplo_mean_Cq >= 34.5 AND Duplo_mean_Cq < 35.5) AND (Duplo_Cq_diff <= 2.1)) Duplo_Cq_diff_acceptable = 1.
* Efficiency is between 1.95 and 2.05, differing Cq values.
IF (((E >= 1.95) AND (E < 2.05)) AND (Duplo_mean_Cq < 31.5) AND (Duplo_Cq_diff <= 0.5)) Duplo_Cq_diff_acceptable = 1. 
IF (((E >= 1.95) AND (E < 2.05)) AND (Duplo_mean_Cq >= 31.5 AND Duplo_mean_Cq < 32.5) AND (Duplo_Cq_diff <= 0.7)) Duplo_Cq_diff_acceptable = 1. 
IF (((E >= 1.95) AND (E < 2.05)) AND (Duplo_mean_Cq >= 32.5 AND Duplo_mean_Cq < 33.5) AND (Duplo_Cq_diff <= 0.9)) Duplo_Cq_diff_acceptable = 1. 
IF (((E >= 1.95) AND (E < 2.05)) AND (Duplo_mean_Cq >= 33.5 AND Duplo_mean_Cq < 34.5) AND (Duplo_Cq_diff <= 1.3)) Duplo_Cq_diff_acceptable = 1. 
IF (((E >= 1.95) AND (E < 2.05)) AND (Duplo_mean_Cq >= 34.5 AND Duplo_mean_Cq < 35.5) AND (Duplo_Cq_diff <= 1.9)) Duplo_Cq_diff_acceptable = 1.
RECODE Duplo_Cq_diff_acceptable (SYSMIS=0).
EXECUTE.

VALUE LABELS
Duplo_Cq_diff_acceptable
1  'acceptable', 
0 'not-acceptable'.
EXECUTE.

***********************************************************
STEP 3: If Mean Cq > 35, recode as nondetect 
***********************************************************
* If Cq >= 35 or Cq=0 THEN recode as nondetect (0).
IF (Duplo_mean_Cq >= 35 OR Duplo_mean_Cq <=15) Duplo_Cq_bigger35=0.
IF (RANGE(Duplo_mean_Cq,15,35)) Duplo_Cq_bigger35=1.
EXECUTE. 

*********************
Make final variable
********************** 
* 1 = mean AB valid 
* 2 = mean BC valid
* 3 = mean all valid
* 4 = mean duplos valid
* 5 = invalid 
* 0= undetectable.
IF (Duplo_Cq_diff_acceptable = 1 AND Duplo_Cq_bigger35 =1) QC = 4.
IF (Duplo_Cq_diff_acceptable = 1 AND Duplo_Cq_bigger35 =0) QC = 0.
IF (Duplo_Cq_diff_acceptable = 0 AND Duplo_Cq_bigger35 =0) QC = 0.
IF (Duplo_Cq_diff_acceptable = 0 AND Duplo_Cq_bigger35 =1) QC = 5.
EXECUTE. 

*********************************************************************************************
Option 3: IF ONE OR NO GOOD MELTING CURVES
*********************************************************************************************

IF (qPC = 0 AND Mean_delta_ABC =0) QC = 0.
IF (qPC = 0 AND Mean_delta_ABC >=35) QC = 0.
IF (qPC = 0 AND RANGE(Mean_delta_ABC,0.1,35) ) QC = 5.
EXECUTE. 

* move QC to end.
RECODE QC 
(0 = 0)
(1 = 1)
(2 = 2)
(3 = 3)
(4 = 4)
(5 = 5)
INTO QC_final.
Execute.

VALUE LABELS
QC_final
1 'mean AB valid'
2 'mean BC valid'
3 'mean all valid'
4 'mean duplos valid'
5 'invalid'
0 'undetectable'.
EXECUTE.

*note that QC_final is the same variable as QC, but is not positioned at the end of the file.


*************************
OPTIONAL: Calculate starting concentrations: 'N0 values'
*************************
* first calculate N0s .
COMPUTE N0_lowest=Nq/(E**LowestCq).
COMPUTE N0_middle=Nq/(E**middleCq).
COMPUTE N0_highest=Nq/(E**highestCq).
COMPUTE Duplo_N01=Nq/(E**Duplo_Cq1).
COMPUTE Duplo_N02=Nq/(E**Duplo_Cq2).
EXECUTE.

* compute final N0 variable.
IF (QC = 1) Final = Mean(N0_lowest,N0_middle).
IF (QC = 2) Final = Mean(N0_highest,N0_middle).
IF (QC = 3) Final = Mean(N0_highest,N0_middle,N0_lowest).
IF (QC = 4) Final = Mean(Duplo_N01,Duplo_N02).
EXECUTE.

*********
Final Step: Calculate the final N0 variable for the undetectables. 
*********
* Calculate the final N0 variable for  the undetectable QC, separate for the different MiRNA with undetectables. 
IF (QC = 0) Final = Nq/(E**38.0).
EXECUTE.

* Calculate the final N0 variable for  the undetectable QC, separate for the different MiRNA with undetectables. 
IF (MiRNA = 'xxxxxx' AND QC = 0) Final_N0 = Nq/(E**(Cq+1)). 
EXECUTE. 
