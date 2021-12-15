###########
# Evelien Stevenson & Maurice WJ de Ronde
# Handling pipeline PCR data duplicates
###########


# open SPSS database 

library(foreign)
PCR <- read.spss("Example duplicates.sav", to.data.frame=TRUE, use.value.labels=FALSE)


#########Step 1: Curve Analysis##########
#########################################

PCR$qPC <- ifelse(PCR$Curve1==0 & PCR$Curve2==0,1,0)
#qPC = 0, <2 good curves
#qPC = 1, 2 good curves

#########Step 2: Delta in Range##########
#########################################
#Mean Cq of duplo's is calculated first, then this variable is altered if:
#If one of the Cq's is 0, then these will not be calculated with 
#and the mean is shown as the Cq of the working duplo.
PCR$MeanCq <- ((PCR$Cq1 + PCR$Cq2)/2)
PCR$MeanCq <- ifelse(PCR$Cq1==0,PCR$Cq2,PCR$MeanCq)
PCR$MeanCq <- ifelse(PCR$Cq2==0, PCR$Cq1, PCR$MeanCq)

#To compute the difference between the 2 duplo's
PCR$CqDiff <- (sqrt((PCR$Cq1-PCR$Cq2)^2))

#Delta Cq is dependent on the PCR efficiency of the experiment.
#Calculations for Efficiency from 1.45 - 2.05 are given below.
#First give the new variable the value 0.
PCR$CqDiffAcc <- 0
#This one is always <= 0.5, for every efficiency.
PCR$CqDiffAcc <- ifelse((PCR$MeanCq<26.5 & PCR$CqDiff<=0.5),1,PCR$CqDiffAcc)
#Efficiency is between 1.45 and 1.55, differing Cq values.
PCR$CqDiffAcc <- ifelse(((PCR$E>=1.45 & PCR$E <1.55)&(PCR$MeanCq>=26.5 & PCR$MeanCq<27.5)&(PCR$CqDiff<=0.6)),1,PCR$CqDiffAcc)
PCR$CqDiffAcc <- ifelse(((PCR$E>=1.45 & PCR$E <1.55)&(PCR$MeanCq>=27.5 & PCR$MeanCq<28.5)&(PCR$CqDiff<=0.8)),1,PCR$CqDiffAcc)
PCR$CqDiffAcc <- ifelse(((PCR$E>=1.45 & PCR$E <1.55)&(PCR$MeanCq>=28.5 & PCR$MeanCq<29.5)&(PCR$CqDiff<=0.9)),1,PCR$CqDiffAcc)
PCR$CqDiffAcc <- ifelse(((PCR$E>=1.45 & PCR$E <1.55)&(PCR$MeanCq>=29.5 & PCR$MeanCq<30.5)&(PCR$CqDiff<=1.1)),1,PCR$CqDiffAcc)
PCR$CqDiffAcc <- ifelse(((PCR$E>=1.45 & PCR$E <1.55)&(PCR$MeanCq>=30.5 & PCR$MeanCq<31.5)&(PCR$CqDiff<=1.4)),1,PCR$CqDiffAcc)
PCR$CqDiffAcc <- ifelse(((PCR$E>=1.45 & PCR$E <1.55)&(PCR$MeanCq>=31.5 & PCR$MeanCq<32.5)&(PCR$CqDiff<=1.7)),1,PCR$CqDiffAcc)
PCR$CqDiffAcc <- ifelse(((PCR$E>=1.45 & PCR$E <1.55)&(PCR$MeanCq>=32.5 & PCR$MeanCq<33.5)&(PCR$CqDiff<=2.1)),1,PCR$CqDiffAcc)
PCR$CqDiffAcc <- ifelse(((PCR$E>=1.45 & PCR$E <1.55)&(PCR$MeanCq>=33.5 & PCR$MeanCq<34.5)&(PCR$CqDiff<=2.7)),1,PCR$CqDiffAcc)
PCR$CqDiffAcc <- ifelse(((PCR$E>=1.45 & PCR$E <1.55)&(PCR$MeanCq>=34.5 & PCR$MeanCq<35.5)&(PCR$CqDiff<=3.3)),1,PCR$CqDiffAcc)
#Efficency is between 1.55 and 1.65, differing Cq values.
PCR$CqDiffAcc <- ifelse(((PCR$E>=1.55 & PCR$E <1.65)&(PCR$MeanCq<28.5)&(PCR$CqDiff<=0.5)),1,PCR$CqDiffAcc)
PCR$CqDiffAcc <- ifelse(((PCR$E>=1.55 & PCR$E <1.65)&(PCR$MeanCq>=28.5 & PCR$MeanCq<29.5)&(PCR$CqDiff<=0.7)),1,PCR$CqDiffAcc)
PCR$CqDiffAcc <- ifelse(((PCR$E>=1.55 & PCR$E <1.65)&(PCR$MeanCq>=29.5 & PCR$MeanCq<30.5)&(PCR$CqDiff<=0.8)),1,PCR$CqDiffAcc)
PCR$CqDiffAcc <- ifelse(((PCR$E>=1.55 & PCR$E <1.65)&(PCR$MeanCq>=30.5 & PCR$MeanCq<31.5)&(PCR$CqDiff<=1.1)),1,PCR$CqDiffAcc)
PCR$CqDiffAcc <- ifelse(((PCR$E>=1.55 & PCR$E <1.65)&(PCR$MeanCq>=31.5 & PCR$MeanCq<32.5)&(PCR$CqDiff<=1.4)),1,PCR$CqDiffAcc)
PCR$CqDiffAcc <- ifelse(((PCR$E>=1.55 & PCR$E <1.65)&(PCR$MeanCq>=32.5 & PCR$MeanCq<33.5)&(PCR$CqDiff<=1.7)),1,PCR$CqDiffAcc)
PCR$CqDiffAcc <- ifelse(((PCR$E>=1.55 & PCR$E <1.65)&(PCR$MeanCq>=33.5 & PCR$MeanCq<34.5)&(PCR$CqDiff<=2.2)),1,PCR$CqDiffAcc)
PCR$CqDiffAcc <- ifelse(((PCR$E>=1.55 & PCR$E <1.65)&(PCR$MeanCq>=34.5 & PCR$MeanCq<35.5)&(PCR$CqDiff<=2.9)),1,PCR$CqDiffAcc)
#Efficiency is between 1.65 and 1.75, differing Cq values.
PCR$CqDiffAcc <- ifelse(((PCR$E>=1.65 & PCR$E <1.75)&(PCR$MeanCq<29.5)&(PCR$CqDiff<=0.5)),1,PCR$CqDiffAcc)
PCR$CqDiffAcc <- ifelse(((PCR$E>=1.65 & PCR$E <1.75)&(PCR$MeanCq>=29.5 & PCR$MeanCq<30.5)&(PCR$CqDiff<=0.6)),1,PCR$CqDiffAcc)
PCR$CqDiffAcc <- ifelse(((PCR$E>=1.65 & PCR$E <1.75)&(PCR$MeanCq>=30.5 & PCR$MeanCq<31.5)&(PCR$CqDiff<=0.8)),1,PCR$CqDiffAcc)
PCR$CqDiffAcc <- ifelse(((PCR$E>=1.65 & PCR$E <1.75)&(PCR$MeanCq>=31.5 & PCR$MeanCq<32.5)&(PCR$CqDiff<=1.1)),1,PCR$CqDiffAcc)
PCR$CqDiffAcc <- ifelse(((PCR$E>=1.65 & PCR$E <1.75)&(PCR$MeanCq>=32.5 & PCR$MeanCq<33.5)&(PCR$CqDiff<=1.5)),1,PCR$CqDiffAcc)
PCR$CqDiffAcc <- ifelse(((PCR$E>=1.65 & PCR$E <1.75)&(PCR$MeanCq>=33.5 & PCR$MeanCq<34.5)&(PCR$CqDiff<=1.9)),1,PCR$CqDiffAcc)
PCR$CqDiffAcc <- ifelse(((PCR$E>=1.65 & PCR$E <1.75)&(PCR$MeanCq>=34.5 & PCR$MeanCq<35.5)&(PCR$CqDiff<=2.5)),1,PCR$CqDiffAcc)
#Efficiency is between 1.75 and 1.85, differing Cq values. 
PCR$CqDiffAcc <- ifelse(((PCR$E>=1.75 & PCR$E <1.85)&(PCR$MeanCq<30.5)&(PCR$CqDiff<=0.5)),1,PCR$CqDiffAcc)
PCR$CqDiffAcc <- ifelse(((PCR$E>=1.75 & PCR$E <1.85)&(PCR$MeanCq>=30.5 & PCR$MeanCq<31.5)&(PCR$CqDiff<=0.7)),1,PCR$CqDiffAcc)
PCR$CqDiffAcc <- ifelse(((PCR$E>=1.75 & PCR$E <1.85)&(PCR$MeanCq>=31.5 & PCR$MeanCq<32.5)&(PCR$CqDiff<=0.9)),1,PCR$CqDiffAcc)
PCR$CqDiffAcc <- ifelse(((PCR$E>=1.75 & PCR$E <1.85)&(PCR$MeanCq>=32.5 & PCR$MeanCq<33.5)&(PCR$CqDiff<=1.2)),1,PCR$CqDiffAcc)
PCR$CqDiffAcc <- ifelse(((PCR$E>=1.75 & PCR$E <1.85)&(PCR$MeanCq>=33.5 & PCR$MeanCq<34.5)&(PCR$CqDiff<=1.7)),1,PCR$CqDiffAcc)
PCR$CqDiffAcc <- ifelse(((PCR$E>=1.75 & PCR$E <1.85)&(PCR$MeanCq>=34.5 & PCR$MeanCq<35.5)&(PCR$CqDiff<=2.3)),1,PCR$CqDiffAcc)
#Efficiency is between 1.85 and 1.95, differing Cq values. 
PCR$CqDiffAcc <- ifelse(((PCR$E>=1.85 & PCR$E <1.95)&(PCR$MeanCq<31.5)&(PCR$CqDiff<=0.5)),1,PCR$CqDiffAcc)
PCR$CqDiffAcc <- ifelse(((PCR$E>=1.85 & PCR$E <1.95)&(PCR$MeanCq>=31.5 & PCR$MeanCq<32.5)&(PCR$CqDiff<=0.8)),1,PCR$CqDiffAcc)
PCR$CqDiffAcc <- ifelse(((PCR$E>=1.85 & PCR$E <1.95)&(PCR$MeanCq>=32.5 & PCR$MeanCq<33.5)&(PCR$CqDiff<=1.1)),1,PCR$CqDiffAcc)
PCR$CqDiffAcc <- ifelse(((PCR$E>=1.85 & PCR$E <1.95)&(PCR$MeanCq>=33.5 & PCR$MeanCq<34.5)&(PCR$CqDiff<=1.5)),1,PCR$CqDiffAcc)
PCR$CqDiffAcc <- ifelse(((PCR$E>=1.85 & PCR$E <1.95)&(PCR$MeanCq>=34.5 & PCR$MeanCq<35.5)&(PCR$CqDiff<=2.1)),1,PCR$CqDiffAcc)
#Efficiency is between 1.95 and 2.05, differing Cq values.
PCR$CqDiffAcc <- ifelse(((PCR$E>=1.95 & PCR$E <2.05)&(PCR$MeanCq<31.5)&(PCR$CqDiff<=0.5)),1,PCR$CqDiffAcc)
PCR$CqDiffAcc <- ifelse(((PCR$E>=1.95 & PCR$E <2.05)&(PCR$MeanCq>=31.5 & PCR$MeanCq<32.5)&(PCR$CqDiff<=0.7)),1,PCR$CqDiffAcc)
PCR$CqDiffAcc <- ifelse(((PCR$E>=1.95 & PCR$E <2.05)&(PCR$MeanCq>=32.5 & PCR$MeanCq<33.5)&(PCR$CqDiff<=0.9)),1,PCR$CqDiffAcc)
PCR$CqDiffAcc <- ifelse(((PCR$E>=1.95 & PCR$E <2.05)&(PCR$MeanCq>=33.5 & PCR$MeanCq<34.5)&(PCR$CqDiff<=1.3)),1,PCR$CqDiffAcc)
PCR$CqDiffAcc <- ifelse(((PCR$E>=1.95 & PCR$E <2.05)&(PCR$MeanCq>=34.5 & PCR$MeanCq<35.5)&(PCR$CqDiff<=1.9)),1,PCR$CqDiffAcc)

# note that PCR$CqDiffAcc = 0 for all mean Cqs > 35 (undetectables).

#########Step 3: If mean Cq>critical value (35), recode as nondetect##########
##############################################################################
#If Cq>=35 THEN recode as nondetect (0)
#If Cq=0 in both duplo's, also recode as nondetect (0).
PCR$CqBigger35 <- ifelse((PCR$MeanCq>=35 | PCR$MeanCq <1),0,1) 

#########Step 4: Make final variable##########
##############################################
#0=undetectable
#1=valid
#2=invalid
PCR$QC <- 0
PCR$QC <- ifelse((PCR$qPC==1 & PCR$CqDiffAcc==1 & PCR$CqBigger35==1),1,PCR$QC)
PCR$QC <- ifelse((PCR$qPC==1 & PCR$CqDiffAcc==0 & PCR$CqBigger35==1),2,PCR$QC)
PCR$QC <- ifelse((PCR$qPC==0 & PCR$CqBigger35==1),2,PCR$QC)

#Calculate final N0 variable, for valid QC(only necessary when working with N0 values).

PCR$N01 <- PCR$Nq/(PCR$E^PCR$Cq1)
PCR$N02 <- PCR$Nq/(PCR$E^PCR$Cq2)
PCR$MeanN0 <- ((PCR$N01 + PCR$N02)/2)

Datafinal <- cbind(PCR$Patient_nr, PCR$miRNA, PCR$QC, PCR$MeanN0)
colnames(Datafinal) <- c("Sample nr","target","QC","Final N0")

Datafinal




