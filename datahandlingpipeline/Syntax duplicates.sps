* Encoding: UTF-8.
DATASET ACTIVATE DataSet1.
********STEP 1 CURVE analsysis*********
********************************************

* Curve OK = 0
* Bad curve (bad melting curve or insufficient quality) = 1.
*** This step must be done manually ***

IF (Curve1 = 1 AND Curve2 = 1) qPC = 0.
IF (Curve1 = 1 AND Curve2 = 0) qPC = 0.
IF (Curve1 = 0 AND Curve2 = 1) qPC = 0.
IF (Curve1 = 0 AND Curve2 = 0) qPC = 1.
EXECUTE.

VALUE LABELS
qPC
0 '<2 good curves'
1 '2 good curves'.
EXECUTE.


********STEP 2: DELTA IN RANGE***************
***************************************************

*Calculate mean Cq of the duplos
- condition: If one of the Cq's is 0, then these will not be calculated with and the mean is shown as the Cq of the working duplo.
IF  (Cq1 ~= 0 AND Cq2 ~= 0) mean_Cq=((Cq1 + Cq2)/2).
IF  (Cq1 = 0) mean_Cq =(Cq2).
IF  (Cq2 = 0) mean_Cq =(Cq1).
EXECUTE.

*Compute the difference between the 2 duplos.
COMPUTE Cq_diff= SQRT((Cq1 - Cq2)**2).
EXECUTE.

*Delta Cq is dependent on the PCR efficiency of the experiment. Please fill in the appropriate max Cq range based on Table 1 in the paper. 
* This one is always <= 0.5, for every efficiency.  
IF ((mean_Cq < 26.5) AND (Cq_diff <= 0.5))  Cq_diff_acceptable = 1.
* Efficiency is between 1.45 and 1.55, differing Cq values. 
IF (((E >= 1.45) AND (E < 1.55)) AND (mean_Cq >= 26.5 AND mean_Cq < 27.5) AND (Cq_diff <= 0.6)) Cq_diff_acceptable = 1. 
IF (((E >= 1.45) AND (E < 1.55)) AND (mean_Cq >= 27.5 AND mean_Cq < 28.5) AND (Cq_diff <= 0.8)) Cq_diff_acceptable = 1. 
IF (((E >= 1.45) AND (E < 1.55)) AND (mean_Cq >= 28.5 AND mean_Cq < 29.5) AND (Cq_diff <= 0.9)) Cq_diff_acceptable = 1. 
IF (((E >= 1.45) AND (E < 1.55)) AND (mean_Cq >= 29.5 AND mean_Cq < 30.5) AND (Cq_diff <= 1.1)) Cq_diff_acceptable = 1. 
IF (((E >= 1.45) AND (E < 1.55)) AND (mean_Cq >= 30.5 AND mean_Cq < 31.5) AND (Cq_diff <= 1.4)) Cq_diff_acceptable = 1. 
IF (((E >= 1.45) AND (E < 1.55)) AND (mean_Cq >= 31.5 AND mean_Cq < 32.5) AND (Cq_diff <= 1.7)) Cq_diff_acceptable = 1. 
IF (((E >= 1.45) AND (E < 1.55)) AND (mean_Cq >= 32.5 AND mean_Cq < 33.5) AND (Cq_diff <= 2.1)) Cq_diff_acceptable = 1. 
IF (((E >= 1.45) AND (E < 1.55)) AND (mean_Cq >= 33.5 AND mean_Cq < 34.5) AND (Cq_diff <= 2.7)) Cq_diff_acceptable = 1. 
IF (((E >= 1.45) AND (E < 1.55)) AND (mean_Cq >= 34.5 AND mean_Cq < 35.5) AND (Cq_diff <= 3.3)) Cq_diff_acceptable = 1. 
* Efficiency is between 1.55 and 1.65, differing Cq values. 
IF (((E >= 1.55) AND (E < 1.65)) AND (mean_Cq < 28.5) AND (Cq_diff <= 0.5)) Cq_diff_acceptable = 1. 
IF (((E >= 1.55) AND (E < 1.65)) AND (mean_Cq >= 28.5 AND mean_Cq < 29.5) AND (Cq_diff <= 0.7)) Cq_diff_acceptable = 1. 
IF (((E >= 1.55) AND (E < 1.65)) AND (mean_Cq >= 29.5 AND mean_Cq < 30.5) AND (Cq_diff <= 0.8)) Cq_diff_acceptable = 1. 
IF (((E >= 1.55) AND (E < 1.65)) AND (mean_Cq >= 30.5 AND mean_Cq < 31.5) AND (Cq_diff <= 1.1)) Cq_diff_acceptable = 1. 
IF (((E >= 1.55) AND (E < 1.65)) AND (mean_Cq >= 31.5 AND mean_Cq < 32.5) AND (Cq_diff <= 1.4)) Cq_diff_acceptable = 1. 
IF (((E >= 1.55) AND (E < 1.65)) AND (mean_Cq >= 32.5 AND mean_Cq < 33.5) AND (Cq_diff <= 1.7)) Cq_diff_acceptable = 1. 
IF (((E >= 1.55) AND (E < 1.65)) AND (mean_Cq >= 33.5 AND mean_Cq < 34.5) AND (Cq_diff <= 2.2)) Cq_diff_acceptable = 1. 
IF (((E >= 1.55) AND (E < 1.65)) AND (mean_Cq >= 34.5 AND mean_Cq < 35.5) AND (Cq_diff <= 2.9)) Cq_diff_acceptable = 1. 
* Efficiency is between 1.65 and 1.75, differing Cq values. 
IF (((E >= 1.65) AND (E < 1.75)) AND (mean_Cq < 29.5) AND (Cq_diff <= 0.5)) Cq_diff_acceptable = 1. 
IF (((E >= 1.65) AND (E < 1.75)) AND (mean_Cq >= 29.5 AND mean_Cq < 30.5) AND (Cq_diff <= 0.6)) Cq_diff_acceptable = 1. 
IF (((E >= 1.65) AND (E < 1.75)) AND (mean_Cq >= 30.5 AND mean_Cq < 31.5) AND (Cq_diff <= 0.8)) Cq_diff_acceptable = 1. 
IF (((E >= 1.65) AND (E < 1.75)) AND (mean_Cq >= 31.5 AND mean_Cq < 32.5) AND (Cq_diff <= 1.1)) Cq_diff_acceptable = 1. 
IF (((E >= 1.65) AND (E < 1.75)) AND (mean_Cq >= 32.5 AND mean_Cq < 33.5) AND (Cq_diff <= 1.5)) Cq_diff_acceptable = 1. 
IF (((E >= 1.65) AND (E < 1.75)) AND (mean_Cq >= 33.5 AND mean_Cq < 34.5) AND (Cq_diff <= 1.9)) Cq_diff_acceptable = 1. 
IF (((E >= 1.65) AND (E < 1.75)) AND (mean_Cq >= 34.5 AND mean_Cq < 35.5) AND (Cq_diff <= 2.5)) Cq_diff_acceptable = 1.
* Efficiency is between 1.75 and 1.85, differing Cq values. 
IF (((E >= 1.75) AND (E < 1.85)) AND (mean_Cq < 30.5) AND (Cq_diff <= 0.5)) Cq_diff_acceptable = 1. 
IF (((E >= 1.75) AND (E < 1.85)) AND (mean_Cq >= 30.5 AND mean_Cq < 31.5) AND (Cq_diff <= 0.7)) Cq_diff_acceptable = 1. 
IF (((E >= 1.75) AND (E < 1.85)) AND (mean_Cq >= 31.5 AND mean_Cq < 32.5) AND (Cq_diff <= 0.9)) Cq_diff_acceptable = 1. 
IF (((E >= 1.75) AND (E < 1.85)) AND (mean_Cq >= 32.5 AND mean_Cq < 33.5) AND (Cq_diff <= 1.2)) Cq_diff_acceptable = 1. 
IF (((E >= 1.75) AND (E < 1.85)) AND (mean_Cq >= 33.5 AND mean_Cq < 34.5) AND (Cq_diff <= 1.7)) Cq_diff_acceptable = 1. 
IF (((E >= 1.75) AND (E < 1.85)) AND (mean_Cq >= 34.5 AND mean_Cq < 35.5) AND (Cq_diff <= 2.3)) Cq_diff_acceptable = 1. 
* Efficiency is between 1.85 and 1.95, differing Cq values. 
IF (((E >= 1.85) AND (E < 1.95)) AND (mean_Cq < 31.5) AND (Cq_diff <= 0.5)) Cq_diff_acceptable = 1. 
IF (((E >= 1.85) AND (E < 1.95)) AND (mean_Cq >= 31.5 AND mean_Cq < 32.5) AND (Cq_diff <= 0.8)) Cq_diff_acceptable = 1. 
IF (((E >= 1.85) AND (E < 1.95)) AND (mean_Cq >= 32.5 AND mean_Cq < 33.5) AND (Cq_diff <= 1.1)) Cq_diff_acceptable = 1. 
IF (((E >= 1.85) AND (E < 1.95)) AND (mean_Cq >= 33.5 AND mean_Cq < 34.5) AND (Cq_diff <= 1.5)) Cq_diff_acceptable = 1. 
IF (((E >= 1.85) AND (E < 1.95)) AND (mean_Cq >= 34.5 AND mean_Cq < 35.5) AND (Cq_diff <= 2.1)) Cq_diff_acceptable = 1.
* Efficiency is between 1.95 and 2.05, differing Cq values.
IF (((E >= 1.95) AND (E < 2.05)) AND (mean_Cq < 31.5) AND (Cq_diff <= 0.5)) Cq_diff_acceptable = 1. 
IF (((E >= 1.95) AND (E < 2.05)) AND (mean_Cq >= 31.5 AND mean_Cq < 32.5) AND (Cq_diff <= 0.7)) Cq_diff_acceptable = 1. 
IF (((E >= 1.95) AND (E < 2.05)) AND (mean_Cq >= 32.5 AND mean_Cq < 33.5) AND (Cq_diff <= 0.9)) Cq_diff_acceptable = 1. 
IF (((E >= 1.95) AND (E < 2.05)) AND (mean_Cq >= 33.5 AND mean_Cq < 34.5) AND (Cq_diff <= 1.3)) Cq_diff_acceptable = 1. 
IF (((E >= 1.95) AND (E < 2.05)) AND (mean_Cq >= 34.5 AND mean_Cq < 35.5) AND (Cq_diff <= 1.9)) Cq_diff_acceptable = 1.
RECODE Cq_diff_acceptable (SYSMIS=0).
EXECUTE.


**********************************************************
STEP 3: If Mean Cq > 35, recode as nondetect 
**********************************************************
* If Cq >= 35 THEN recode as nondetect (0)
* If  Cq =0 in both duplo's, also recode as nondetect (0).
IF (mean_Cq >= 35 OR mean_Cq <1) Cq_bigger35=0.
IF (RANGE(mean_Cq,1,35)) Cq_bigger35=1.
EXECUTE. 

VALUE LABELS
Cq_bigger35
0 'beyond'
1 'within'.
EXECUTE.


*******************************************
STEP 4: Make final variable
*******************************************
* 0= undetectable
* 1 = mean valid 
* 2 = invalid 

IF (qPC = 1 AND Cq_diff_acceptable = 1 AND Cq_bigger35 =1) QC = 1.
IF (qPC = 1 AND Cq_diff_acceptable = 1 AND Cq_bigger35 =0) QC = 0.
IF (qPC = 1 AND Cq_diff_acceptable = 0 AND Cq_bigger35 =0) QC = 0.
IF (qPC = 1 AND Cq_diff_acceptable = 0 AND Cq_bigger35 =1) QC = 2.
IF (qPC = 0 AND Cq_bigger35 = 0) QC = 0.
IF (qPC = 0 AND Cq_bigger35 =1 ) QC = 2.
EXECUTE. 

VALUE LABELS
QC
0 'undetectable'
1 'valid'
2 'invalid'.
EXECUTE.

***************
* Calculate final N0 variable, for valid QC.(Only necessary when working with N0 values).
IF (QC = 1) Final_N0 = Mean(N01,N02). 
IF (QC = 1) Final_Cq = Mean(Cq1,Cq2).
EXECUTE. 








