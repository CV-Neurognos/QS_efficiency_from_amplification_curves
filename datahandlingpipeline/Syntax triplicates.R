###########
# Maurice WJ de Ronde and Evelien Stevenson
# Handling pipeline PCR data duplo's
###########


# open SPSS database 

library(foreign)
PCR <- read.spss("Triplicates_uitgewerkt.sav", to.data.frame=TRUE, use.value.labels=FALSE)

#########Step 1: Curve Analysis##########
#########################################

#PCR$Curve = 1 when bad curve occurred

PCR$qPC <- 0
PCR$qPC <- ifelse(PCR$Curve1==1 & PCR$Curve2 ==0 & PCR$Curve3 ==0,1,PCR$qPC)
PCR$qPC <- ifelse(PCR$Curve1==0 & PCR$Curve2 ==1 & PCR$Curve3 ==0,1,PCR$qPC)
PCR$qPC <- ifelse(PCR$Curve1==0 & PCR$Curve2 ==0 & PCR$Curve3 ==1,1,PCR$qPC)
PCR$qPC <- ifelse(PCR$Curve1==0 & PCR$Curve2 ==0 & PCR$Curve3 ==0,2,PCR$qPC)

#qPC = 0, <2 good curves
#qPC = 1, 2 good curves
#qPC = 2, 3 good curves

##########################################
# Step 2 and 3 contain different codes for 3 good curves, 2 good curves or <2 good curves. 
# All have a different set of codes. All codes must always be runned. 



##########################################
##############3 GOOD CURVES###############
##########################################


#Step 2: Discordance between replicates####
###########################################

# First, rank Cq variable

PCR$minCq <- pmin(PCR$Cq1,PCR$Cq2,PCR$Cq3)
PCR$maxCq <- pmax(PCR$Cq1,PCR$Cq2,PCR$Cq3)
PCR$middleCq <- ((PCR$Cq1+PCR$Cq2+PCR$Cq3)-(PCR$minCq+PCR$maxCq))


# compute deltas of AB, BC and ABC

PCR$deltaAB <- sqrt((PCR$minCq-PCR$middleCq)^2)
PCR$deltaBC <- sqrt((PCR$maxCq-PCR$middleCq)^2)
PCR$deltaABC <- sqrt((PCR$maxCq-PCR$minCq)^2)

# compute means of AB, BC and ABC
# mean AB
PCR$MeanAB <- ((PCR$minCq + PCR$middleCq)/2)
PCR$MeanAB <- ifelse(PCR$minCq==0,PCR$middleCq,PCR$MeanAB)
PCR$MeanAB <- ifelse(PCR$middleCq==0, PCR$minCq, PCR$MeanAB)
# mean BC
PCR$MeanBC <- ((PCR$maxCq + PCR$middleCq)/2)
PCR$MeanBC <- ifelse(PCR$maxCq==0,PCR$middleCq,PCR$MeanBC)
PCR$MeanBC <- ifelse(PCR$middleCq==0, PCR$maxCq, PCR$MeanBC)
# mean ABC
PCR$MeanABC <- ((PCR$minCq + PCR$middleCq + PCR$maxCq)/3)
PCR$MeanABC <- ifelse(PCR$minCq==0 & PCR$middleCq==0, PCR$maxCq,PCR$MeanABC)
PCR$MeanABC <- ifelse(PCR$minCq==0 & PCR$middleCq !=0, ((PCR$maxCq+PCR$middleCq)/2),PCR$MeanABC)

############################
#####Check delta ABC########
############################


#Delta Cq is dependent on the PCR efficiency of the experiment.
#Calculations for Efficiency from 1.45 - 2.05 are given below.
#First give the new variable the value 0.
PCR$concorABC <- 0
#This one is always <= 0.5, for every efficiency.
PCR$concorABC <- ifelse((PCR$MeanABC<26.5 & PCR$deltaABC<=0.5),1,PCR$concorABC)
#Efficiency is between 1.45 and 1.55, differing Cq values.
PCR$concorABC <- ifelse(((PCR$E>=1.45 & PCR$E <1.55)&(PCR$MeanABC>=26.5 & PCR$MeanABC<27.5)&(PCR$deltaABC<=0.6)),1,PCR$concorABC)
PCR$concorABC <- ifelse(((PCR$E>=1.45 & PCR$E <1.55)&(PCR$MeanABC>=27.5 & PCR$MeanABC<28.5)&(PCR$deltaABC<=0.8)),1,PCR$concorABC)
PCR$concorABC <- ifelse(((PCR$E>=1.45 & PCR$E <1.55)&(PCR$MeanABC>=28.5 & PCR$MeanABC<29.5)&(PCR$deltaABC<=0.9)),1,PCR$concorABC)
PCR$concorABC <- ifelse(((PCR$E>=1.45 & PCR$E <1.55)&(PCR$MeanABC>=29.5 & PCR$MeanABC<30.5)&(PCR$deltaABC<=1.1)),1,PCR$concorABC)
PCR$concorABC <- ifelse(((PCR$E>=1.45 & PCR$E <1.55)&(PCR$MeanABC>=30.5 & PCR$MeanABC<31.5)&(PCR$deltaABC<=1.4)),1,PCR$concorABC)
PCR$concorABC <- ifelse(((PCR$E>=1.45 & PCR$E <1.55)&(PCR$MeanABC>=31.5 & PCR$MeanABC<32.5)&(PCR$deltaABC<=1.7)),1,PCR$concorABC)
PCR$concorABC <- ifelse(((PCR$E>=1.45 & PCR$E <1.55)&(PCR$MeanABC>=32.5 & PCR$MeanABC<33.5)&(PCR$deltaABC<=2.1)),1,PCR$concorABC)
PCR$concorABC <- ifelse(((PCR$E>=1.45 & PCR$E <1.55)&(PCR$MeanABC>=33.5 & PCR$MeanABC<34.5)&(PCR$deltaABC<=2.7)),1,PCR$concorABC)
PCR$concorABC <- ifelse(((PCR$E>=1.45 & PCR$E <1.55)&(PCR$MeanABC>=34.5 & PCR$MeanABC<35.5)&(PCR$deltaABC<=3.3)),1,PCR$concorABC)
#Efficency is between 1.55 and 1.65, differing Cq values.
PCR$concorABC <- ifelse(((PCR$E>=1.55 & PCR$E <1.65)&(PCR$MeanABC<28.5)&(PCR$deltaABC<=0.5)),1,PCR$concorABC)
PCR$concorABC <- ifelse(((PCR$E>=1.55 & PCR$E <1.65)&(PCR$MeanABC>=28.5 & PCR$MeanABC<29.5)&(PCR$deltaABC<=0.7)),1,PCR$concorABC)
PCR$concorABC <- ifelse(((PCR$E>=1.55 & PCR$E <1.65)&(PCR$MeanABC>=29.5 & PCR$MeanABC<30.5)&(PCR$deltaABC<=0.8)),1,PCR$concorABC)
PCR$concorABC <- ifelse(((PCR$E>=1.55 & PCR$E <1.65)&(PCR$MeanABC>=30.5 & PCR$MeanABC<31.5)&(PCR$deltaABC<=1.1)),1,PCR$concorABC)
PCR$concorABC <- ifelse(((PCR$E>=1.55 & PCR$E <1.65)&(PCR$MeanABC>=31.5 & PCR$MeanABC<32.5)&(PCR$deltaABC<=1.4)),1,PCR$concorABC)
PCR$concorABC <- ifelse(((PCR$E>=1.55 & PCR$E <1.65)&(PCR$MeanABC>=32.5 & PCR$MeanABC<33.5)&(PCR$deltaABC<=1.7)),1,PCR$concorABC)
PCR$concorABC <- ifelse(((PCR$E>=1.55 & PCR$E <1.65)&(PCR$MeanABC>=33.5 & PCR$MeanABC<34.5)&(PCR$deltaABC<=2.2)),1,PCR$concorABC)
PCR$concorABC <- ifelse(((PCR$E>=1.55 & PCR$E <1.65)&(PCR$MeanABC>=34.5 & PCR$MeanABC<35.5)&(PCR$deltaABC<=2.9)),1,PCR$concorABC)
#Efficiency is between 1.65 and 1.75, differing Cq values.
PCR$concorABC <- ifelse(((PCR$E>=1.65 & PCR$E <1.75)&(PCR$MeanABC<29.5)&(PCR$deltaABC<=0.5)),1,PCR$concorABC)
PCR$concorABC <- ifelse(((PCR$E>=1.65 & PCR$E <1.75)&(PCR$MeanABC>=29.5 & PCR$MeanABC<30.5)&(PCR$deltaABC<=0.6)),1,PCR$concorABC)
PCR$concorABC <- ifelse(((PCR$E>=1.65 & PCR$E <1.75)&(PCR$MeanABC>=30.5 & PCR$MeanABC<31.5)&(PCR$deltaABC<=0.8)),1,PCR$concorABC)
PCR$concorABC <- ifelse(((PCR$E>=1.65 & PCR$E <1.75)&(PCR$MeanABC>=31.5 & PCR$MeanABC<32.5)&(PCR$deltaABC<=1.1)),1,PCR$concorABC)
PCR$concorABC <- ifelse(((PCR$E>=1.65 & PCR$E <1.75)&(PCR$MeanABC>=32.5 & PCR$MeanABC<33.5)&(PCR$deltaABC<=1.5)),1,PCR$concorABC)
PCR$concorABC <- ifelse(((PCR$E>=1.65 & PCR$E <1.75)&(PCR$MeanABC>=33.5 & PCR$MeanABC<34.5)&(PCR$deltaABC<=1.9)),1,PCR$concorABC)
PCR$concorABC <- ifelse(((PCR$E>=1.65 & PCR$E <1.75)&(PCR$MeanABC>=34.5 & PCR$MeanABC<35.5)&(PCR$deltaABC<=2.5)),1,PCR$concorABC)
#Efficiency is between 1.75 and 1.85, differing Cq values. 
PCR$concorABC <- ifelse(((PCR$E>=1.75 & PCR$E <1.85)&(PCR$MeanABC<30.5)&(PCR$deltaABC<=0.5)),1,PCR$concorABC)
PCR$concorABC <- ifelse(((PCR$E>=1.75 & PCR$E <1.85)&(PCR$MeanABC>=30.5 & PCR$MeanABC<31.5)&(PCR$deltaABC<=0.7)),1,PCR$concorABC)
PCR$concorABC <- ifelse(((PCR$E>=1.75 & PCR$E <1.85)&(PCR$MeanABC>=31.5 & PCR$MeanABC<32.5)&(PCR$deltaABC<=0.9)),1,PCR$concorABC)
PCR$concorABC <- ifelse(((PCR$E>=1.75 & PCR$E <1.85)&(PCR$MeanABC>=32.5 & PCR$MeanABC<33.5)&(PCR$deltaABC<=1.2)),1,PCR$concorABC)
PCR$concorABC <- ifelse(((PCR$E>=1.75 & PCR$E <1.85)&(PCR$MeanABC>=33.5 & PCR$MeanABC<34.5)&(PCR$deltaABC<=1.7)),1,PCR$concorABC)
PCR$concorABC <- ifelse(((PCR$E>=1.75 & PCR$E <1.85)&(PCR$MeanABC>=34.5 & PCR$MeanABC<35.5)&(PCR$deltaABC<=2.3)),1,PCR$concorABC)
#Efficiency is between 1.85 and 1.95, differing Cq values. 
PCR$concorABC <- ifelse(((PCR$E>=1.85 & PCR$E <1.95)&(PCR$MeanABC<31.5)&(PCR$deltaABC<=0.5)),1,PCR$concorABC)
PCR$concorABC <- ifelse(((PCR$E>=1.85 & PCR$E <1.95)&(PCR$MeanABC>=31.5 & PCR$MeanABC<32.5)&(PCR$deltaABC<=0.8)),1,PCR$concorABC)
PCR$concorABC <- ifelse(((PCR$E>=1.85 & PCR$E <1.95)&(PCR$MeanABC>=32.5 & PCR$MeanABC<33.5)&(PCR$deltaABC<=1.1)),1,PCR$concorABC)
PCR$concorABC <- ifelse(((PCR$E>=1.85 & PCR$E <1.95)&(PCR$MeanABC>=33.5 & PCR$MeanABC<34.5)&(PCR$deltaABC<=1.5)),1,PCR$concorABC)
PCR$concorABC <- ifelse(((PCR$E>=1.85 & PCR$E <1.95)&(PCR$MeanABC>=34.5 & PCR$MeanABC<35.5)&(PCR$deltaABC<=2.1)),1,PCR$concorABC)
#Efficiency is between 1.95 and 2.05, differing Cq values.
PCR$concorABC <- ifelse(((PCR$E>=1.95 & PCR$E <2.05)&(PCR$MeanABC<31.5)&(PCR$deltaABC<=0.5)),1,PCR$concorABC)
PCR$concorABC <- ifelse(((PCR$E>=1.95 & PCR$E <2.05)&(PCR$MeanABC>=31.5 & PCR$MeanABC<32.5)&(PCR$deltaABC<=0.7)),1,PCR$concorABC)
PCR$concorABC <- ifelse(((PCR$E>=1.95 & PCR$E <2.05)&(PCR$MeanABC>=32.5 & PCR$MeanABC<33.5)&(PCR$deltaABC<=0.9)),1,PCR$concorABC)
PCR$concorABC <- ifelse(((PCR$E>=1.95 & PCR$E <2.05)&(PCR$MeanABC>=33.5 & PCR$MeanABC<34.5)&(PCR$deltaABC<=1.3)),1,PCR$concorABC)
PCR$concorABC <- ifelse(((PCR$E>=1.95 & PCR$E <2.05)&(PCR$MeanABC>=34.5 & PCR$MeanABC<35.5)&(PCR$deltaABC<=1.9)),1,PCR$concorABC)

# note that PCR$concorABC = 0 for all mean Cqs > 35 (undetectables).
# concorACB = 1 (concordance between replicates)
# concorACB = 0 (discordance between replicates)


###########################
#####Check delta AB########
###########################


#Delta Cq is dependent on the PCR efficiency of the experiment.
#Calculations for Efficiency from 1.45 - 2.05 are given below.
#First give the new variable the value 0.
PCR$concorAB <- 0
#This one is always <= 0.5, for every efficiency.
PCR$concorAB <- ifelse((PCR$MeanAB<26.5 & PCR$deltaAB<=0.5),1,PCR$concorAB)
#Efficiency is between 1.45 and 1.55, differing Cq values.
PCR$concorAB <- ifelse(((PCR$E>=1.45 & PCR$E <1.55)&(PCR$MeanAB>=26.5 & PCR$MeanAB<27.5)&(PCR$deltaAB<=0.6)),1,PCR$concorAB)
PCR$concorAB <- ifelse(((PCR$E>=1.45 & PCR$E <1.55)&(PCR$MeanAB>=27.5 & PCR$MeanAB<28.5)&(PCR$deltaAB<=0.8)),1,PCR$concorAB)
PCR$concorAB <- ifelse(((PCR$E>=1.45 & PCR$E <1.55)&(PCR$MeanAB>=28.5 & PCR$MeanAB<29.5)&(PCR$deltaAB<=0.9)),1,PCR$concorAB)
PCR$concorAB <- ifelse(((PCR$E>=1.45 & PCR$E <1.55)&(PCR$MeanAB>=29.5 & PCR$MeanAB<30.5)&(PCR$deltaAB<=1.1)),1,PCR$concorAB)
PCR$concorAB <- ifelse(((PCR$E>=1.45 & PCR$E <1.55)&(PCR$MeanAB>=30.5 & PCR$MeanAB<31.5)&(PCR$deltaAB<=1.4)),1,PCR$concorAB)
PCR$concorAB <- ifelse(((PCR$E>=1.45 & PCR$E <1.55)&(PCR$MeanAB>=31.5 & PCR$MeanAB<32.5)&(PCR$deltaAB<=1.7)),1,PCR$concorAB)
PCR$concorAB <- ifelse(((PCR$E>=1.45 & PCR$E <1.55)&(PCR$MeanAB>=32.5 & PCR$MeanAB<33.5)&(PCR$deltaAB<=2.1)),1,PCR$concorAB)
PCR$concorAB <- ifelse(((PCR$E>=1.45 & PCR$E <1.55)&(PCR$MeanAB>=33.5 & PCR$MeanAB<34.5)&(PCR$deltaAB<=2.7)),1,PCR$concorAB)
PCR$concorAB <- ifelse(((PCR$E>=1.45 & PCR$E <1.55)&(PCR$MeanAB>=34.5 & PCR$MeanAB<35.5)&(PCR$deltaAB<=3.3)),1,PCR$concorAB)
#Efficency is between 1.55 and 1.65, differing Cq values.
PCR$concorAB <- ifelse(((PCR$E>=1.55 & PCR$E <1.65)&(PCR$MeanAB<28.5)&(PCR$deltaAB<=0.5)),1,PCR$concorAB)
PCR$concorAB <- ifelse(((PCR$E>=1.55 & PCR$E <1.65)&(PCR$MeanAB>=28.5 & PCR$MeanAB<29.5)&(PCR$deltaAB<=0.7)),1,PCR$concorAB)
PCR$concorAB <- ifelse(((PCR$E>=1.55 & PCR$E <1.65)&(PCR$MeanAB>=29.5 & PCR$MeanAB<30.5)&(PCR$deltaAB<=0.8)),1,PCR$concorAB)
PCR$concorAB <- ifelse(((PCR$E>=1.55 & PCR$E <1.65)&(PCR$MeanAB>=30.5 & PCR$MeanAB<31.5)&(PCR$deltaAB<=1.1)),1,PCR$concorAB)
PCR$concorAB <- ifelse(((PCR$E>=1.55 & PCR$E <1.65)&(PCR$MeanAB>=31.5 & PCR$MeanAB<32.5)&(PCR$deltaAB<=1.4)),1,PCR$concorAB)
PCR$concorAB <- ifelse(((PCR$E>=1.55 & PCR$E <1.65)&(PCR$MeanAB>=32.5 & PCR$MeanAB<33.5)&(PCR$deltaAB<=1.7)),1,PCR$concorAB)
PCR$concorAB <- ifelse(((PCR$E>=1.55 & PCR$E <1.65)&(PCR$MeanAB>=33.5 & PCR$MeanAB<34.5)&(PCR$deltaAB<=2.2)),1,PCR$concorAB)
PCR$concorAB <- ifelse(((PCR$E>=1.55 & PCR$E <1.65)&(PCR$MeanAB>=34.5 & PCR$MeanAB<35.5)&(PCR$deltaAB<=2.9)),1,PCR$concorAB)
#Efficiency is between 1.65 and 1.75, differing Cq values.
PCR$concorAB <- ifelse(((PCR$E>=1.65 & PCR$E <1.75)&(PCR$MeanAB<29.5)&(PCR$deltaAB<=0.5)),1,PCR$concorAB)
PCR$concorAB <- ifelse(((PCR$E>=1.65 & PCR$E <1.75)&(PCR$MeanAB>=29.5 & PCR$MeanAB<30.5)&(PCR$deltaAB<=0.6)),1,PCR$concorAB)
PCR$concorAB <- ifelse(((PCR$E>=1.65 & PCR$E <1.75)&(PCR$MeanAB>=30.5 & PCR$MeanAB<31.5)&(PCR$deltaAB<=0.8)),1,PCR$concorAB)
PCR$concorAB <- ifelse(((PCR$E>=1.65 & PCR$E <1.75)&(PCR$MeanAB>=31.5 & PCR$MeanAB<32.5)&(PCR$deltaAB<=1.1)),1,PCR$concorAB)
PCR$concorAB <- ifelse(((PCR$E>=1.65 & PCR$E <1.75)&(PCR$MeanAB>=32.5 & PCR$MeanAB<33.5)&(PCR$deltaAB<=1.5)),1,PCR$concorAB)
PCR$concorAB <- ifelse(((PCR$E>=1.65 & PCR$E <1.75)&(PCR$MeanAB>=33.5 & PCR$MeanAB<34.5)&(PCR$deltaAB<=1.9)),1,PCR$concorAB)
PCR$concorAB <- ifelse(((PCR$E>=1.65 & PCR$E <1.75)&(PCR$MeanAB>=34.5 & PCR$MeanAB<35.5)&(PCR$deltaAB<=2.5)),1,PCR$concorAB)
#Efficiency is between 1.75 and 1.85, differing Cq values. 
PCR$concorAB <- ifelse(((PCR$E>=1.75 & PCR$E <1.85)&(PCR$MeanAB<30.5)&(PCR$deltaAB<=0.5)),1,PCR$concorAB)
PCR$concorAB <- ifelse(((PCR$E>=1.75 & PCR$E <1.85)&(PCR$MeanAB>=30.5 & PCR$MeanAB<31.5)&(PCR$deltaAB<=0.7)),1,PCR$concorAB)
PCR$concorAB <- ifelse(((PCR$E>=1.75 & PCR$E <1.85)&(PCR$MeanAB>=31.5 & PCR$MeanAB<32.5)&(PCR$deltaAB<=0.9)),1,PCR$concorAB)
PCR$concorAB <- ifelse(((PCR$E>=1.75 & PCR$E <1.85)&(PCR$MeanAB>=32.5 & PCR$MeanAB<33.5)&(PCR$deltaAB<=1.2)),1,PCR$concorAB)
PCR$concorAB <- ifelse(((PCR$E>=1.75 & PCR$E <1.85)&(PCR$MeanAB>=33.5 & PCR$MeanAB<34.5)&(PCR$deltaAB<=1.7)),1,PCR$concorAB)
PCR$concorAB <- ifelse(((PCR$E>=1.75 & PCR$E <1.85)&(PCR$MeanAB>=34.5 & PCR$MeanAB<35.5)&(PCR$deltaAB<=2.3)),1,PCR$concorAB)
#Efficiency is between 1.85 and 1.95, differing Cq values. 
PCR$concorAB <- ifelse(((PCR$E>=1.85 & PCR$E <1.95)&(PCR$MeanAB<31.5)&(PCR$deltaAB<=0.5)),1,PCR$concorAB)
PCR$concorAB <- ifelse(((PCR$E>=1.85 & PCR$E <1.95)&(PCR$MeanAB>=31.5 & PCR$MeanAB<32.5)&(PCR$deltaAB<=0.8)),1,PCR$concorAB)
PCR$concorAB <- ifelse(((PCR$E>=1.85 & PCR$E <1.95)&(PCR$MeanAB>=32.5 & PCR$MeanAB<33.5)&(PCR$deltaAB<=1.1)),1,PCR$concorAB)
PCR$concorAB <- ifelse(((PCR$E>=1.85 & PCR$E <1.95)&(PCR$MeanAB>=33.5 & PCR$MeanAB<34.5)&(PCR$deltaAB<=1.5)),1,PCR$concorAB)
PCR$concorAB <- ifelse(((PCR$E>=1.85 & PCR$E <1.95)&(PCR$MeanAB>=34.5 & PCR$MeanAB<35.5)&(PCR$deltaAB<=2.1)),1,PCR$concorAB)
#Efficiency is between 1.95 and 2.05, differing Cq values.
PCR$concorAB <- ifelse(((PCR$E>=1.95 & PCR$E <2.05)&(PCR$MeanAB<31.5)&(PCR$deltaAB<=0.5)),1,PCR$concorAB)
PCR$concorAB <- ifelse(((PCR$E>=1.95 & PCR$E <2.05)&(PCR$MeanAB>=31.5 & PCR$MeanAB<32.5)&(PCR$deltaAB<=0.7)),1,PCR$concorAB)
PCR$concorAB <- ifelse(((PCR$E>=1.95 & PCR$E <2.05)&(PCR$MeanAB>=32.5 & PCR$MeanAB<33.5)&(PCR$deltaAB<=0.9)),1,PCR$concorAB)
PCR$concorAB <- ifelse(((PCR$E>=1.95 & PCR$E <2.05)&(PCR$MeanAB>=33.5 & PCR$MeanAB<34.5)&(PCR$deltaAB<=1.3)),1,PCR$concorAB)
PCR$concorAB <- ifelse(((PCR$E>=1.95 & PCR$E <2.05)&(PCR$MeanAB>=34.5 & PCR$MeanAB<35.5)&(PCR$deltaAB<=1.9)),1,PCR$concorAB)

# note that PCR$concorAB = 0 for all mean Cqs > 35 (undetectables).
# concorACB = 1 (concordance between replicates)
# concorACB = 0 (discordance between replicates)



###########################
#####Check delta BC########
###########################


#Delta Cq is dependent on the PCR efficiency of the experiment.
#Calculations for Efficiency from 1.45 - 2.05 are given below.
#First give the new variable the value 0.
PCR$concorBC <- 0
#This one is always <= 0.5, for every efficiency.
PCR$concorBC <- ifelse((PCR$MeanBC<26.5 & PCR$deltaBC<=0.5),1,PCR$concorBC)
#Efficiency is between 1.45 and 1.55, differing Cq values.
PCR$concorBC <- ifelse(((PCR$E>=1.45 & PCR$E <1.55)&(PCR$MeanBC>=26.5 & PCR$MeanBC<27.5)&(PCR$deltaBC<=0.6)),1,PCR$concorBC)
PCR$concorBC <- ifelse(((PCR$E>=1.45 & PCR$E <1.55)&(PCR$MeanBC>=27.5 & PCR$MeanBC<28.5)&(PCR$deltaBC<=0.8)),1,PCR$concorBC)
PCR$concorBC <- ifelse(((PCR$E>=1.45 & PCR$E <1.55)&(PCR$MeanBC>=28.5 & PCR$MeanBC<29.5)&(PCR$deltaBC<=0.9)),1,PCR$concorBC)
PCR$concorBC <- ifelse(((PCR$E>=1.45 & PCR$E <1.55)&(PCR$MeanBC>=29.5 & PCR$MeanBC<30.5)&(PCR$deltaBC<=1.1)),1,PCR$concorBC)
PCR$concorBC <- ifelse(((PCR$E>=1.45 & PCR$E <1.55)&(PCR$MeanBC>=30.5 & PCR$MeanBC<31.5)&(PCR$deltaBC<=1.4)),1,PCR$concorBC)
PCR$concorBC <- ifelse(((PCR$E>=1.45 & PCR$E <1.55)&(PCR$MeanBC>=31.5 & PCR$MeanBC<32.5)&(PCR$deltaBC<=1.7)),1,PCR$concorBC)
PCR$concorBC <- ifelse(((PCR$E>=1.45 & PCR$E <1.55)&(PCR$MeanBC>=32.5 & PCR$MeanBC<33.5)&(PCR$deltaBC<=2.1)),1,PCR$concorBC)
PCR$concorBC <- ifelse(((PCR$E>=1.45 & PCR$E <1.55)&(PCR$MeanBC>=33.5 & PCR$MeanBC<34.5)&(PCR$deltaBC<=2.7)),1,PCR$concorBC)
PCR$concorBC <- ifelse(((PCR$E>=1.45 & PCR$E <1.55)&(PCR$MeanBC>=34.5 & PCR$MeanBC<35.5)&(PCR$deltaBC<=3.3)),1,PCR$concorBC)
#Efficency is between 1.55 and 1.65, differing Cq values.
PCR$concorBC <- ifelse(((PCR$E>=1.55 & PCR$E <1.65)&(PCR$MeanBC<28.5)&(PCR$deltaBC<=0.5)),1,PCR$concorBC)
PCR$concorBC <- ifelse(((PCR$E>=1.55 & PCR$E <1.65)&(PCR$MeanBC>=28.5 & PCR$MeanBC<29.5)&(PCR$deltaBC<=0.7)),1,PCR$concorBC)
PCR$concorBC <- ifelse(((PCR$E>=1.55 & PCR$E <1.65)&(PCR$MeanBC>=29.5 & PCR$MeanBC<30.5)&(PCR$deltaBC<=0.8)),1,PCR$concorBC)
PCR$concorBC <- ifelse(((PCR$E>=1.55 & PCR$E <1.65)&(PCR$MeanBC>=30.5 & PCR$MeanBC<31.5)&(PCR$deltaBC<=1.1)),1,PCR$concorBC)
PCR$concorBC <- ifelse(((PCR$E>=1.55 & PCR$E <1.65)&(PCR$MeanBC>=31.5 & PCR$MeanBC<32.5)&(PCR$deltaBC<=1.4)),1,PCR$concorBC)
PCR$concorBC <- ifelse(((PCR$E>=1.55 & PCR$E <1.65)&(PCR$MeanBC>=32.5 & PCR$MeanBC<33.5)&(PCR$deltaBC<=1.7)),1,PCR$concorBC)
PCR$concorBC <- ifelse(((PCR$E>=1.55 & PCR$E <1.65)&(PCR$MeanBC>=33.5 & PCR$MeanBC<34.5)&(PCR$deltaBC<=2.2)),1,PCR$concorBC)
PCR$concorBC <- ifelse(((PCR$E>=1.55 & PCR$E <1.65)&(PCR$MeanBC>=34.5 & PCR$MeanBC<35.5)&(PCR$deltaBC<=2.9)),1,PCR$concorBC)
#Efficiency is between 1.65 and 1.75, differing Cq values.
PCR$concorBC <- ifelse(((PCR$E>=1.65 & PCR$E <1.75)&(PCR$MeanBC<29.5)&(PCR$deltaBC<=0.5)),1,PCR$concorBC)
PCR$concorBC <- ifelse(((PCR$E>=1.65 & PCR$E <1.75)&(PCR$MeanBC>=29.5 & PCR$MeanBC<30.5)&(PCR$deltaBC<=0.6)),1,PCR$concorBC)
PCR$concorBC <- ifelse(((PCR$E>=1.65 & PCR$E <1.75)&(PCR$MeanBC>=30.5 & PCR$MeanBC<31.5)&(PCR$deltaBC<=0.8)),1,PCR$concorBC)
PCR$concorBC <- ifelse(((PCR$E>=1.65 & PCR$E <1.75)&(PCR$MeanBC>=31.5 & PCR$MeanBC<32.5)&(PCR$deltaBC<=1.1)),1,PCR$concorBC)
PCR$concorBC <- ifelse(((PCR$E>=1.65 & PCR$E <1.75)&(PCR$MeanBC>=32.5 & PCR$MeanBC<33.5)&(PCR$deltaBC<=1.5)),1,PCR$concorBC)
PCR$concorBC <- ifelse(((PCR$E>=1.65 & PCR$E <1.75)&(PCR$MeanBC>=33.5 & PCR$MeanBC<34.5)&(PCR$deltaBC<=1.9)),1,PCR$concorBC)
PCR$concorBC <- ifelse(((PCR$E>=1.65 & PCR$E <1.75)&(PCR$MeanBC>=34.5 & PCR$MeanBC<35.5)&(PCR$deltaBC<=2.5)),1,PCR$concorBC)
#Efficiency is between 1.75 and 1.85, differing Cq values. 
PCR$concorBC <- ifelse(((PCR$E>=1.75 & PCR$E <1.85)&(PCR$MeanBC<30.5)&(PCR$deltaBC<=0.5)),1,PCR$concorBC)
PCR$concorBC <- ifelse(((PCR$E>=1.75 & PCR$E <1.85)&(PCR$MeanBC>=30.5 & PCR$MeanBC<31.5)&(PCR$deltaBC<=0.7)),1,PCR$concorBC)
PCR$concorBC <- ifelse(((PCR$E>=1.75 & PCR$E <1.85)&(PCR$MeanBC>=31.5 & PCR$MeanBC<32.5)&(PCR$deltaBC<=0.9)),1,PCR$concorBC)
PCR$concorBC <- ifelse(((PCR$E>=1.75 & PCR$E <1.85)&(PCR$MeanBC>=32.5 & PCR$MeanBC<33.5)&(PCR$deltaBC<=1.2)),1,PCR$concorBC)
PCR$concorBC <- ifelse(((PCR$E>=1.75 & PCR$E <1.85)&(PCR$MeanBC>=33.5 & PCR$MeanBC<34.5)&(PCR$deltaBC<=1.7)),1,PCR$concorBC)
PCR$concorBC <- ifelse(((PCR$E>=1.75 & PCR$E <1.85)&(PCR$MeanBC>=34.5 & PCR$MeanBC<35.5)&(PCR$deltaBC<=2.3)),1,PCR$concorBC)
#Efficiency is between 1.85 and 1.95, differing Cq values. 
PCR$concorBC <- ifelse(((PCR$E>=1.85 & PCR$E <1.95)&(PCR$MeanBC<31.5)&(PCR$deltaBC<=0.5)),1,PCR$concorBC)
PCR$concorBC <- ifelse(((PCR$E>=1.85 & PCR$E <1.95)&(PCR$MeanBC>=31.5 & PCR$MeanBC<32.5)&(PCR$deltaBC<=0.8)),1,PCR$concorBC)
PCR$concorBC <- ifelse(((PCR$E>=1.85 & PCR$E <1.95)&(PCR$MeanBC>=32.5 & PCR$MeanBC<33.5)&(PCR$deltaBC<=1.1)),1,PCR$concorBC)
PCR$concorBC <- ifelse(((PCR$E>=1.85 & PCR$E <1.95)&(PCR$MeanBC>=33.5 & PCR$MeanBC<34.5)&(PCR$deltaBC<=1.5)),1,PCR$concorBC)
PCR$concorBC <- ifelse(((PCR$E>=1.85 & PCR$E <1.95)&(PCR$MeanBC>=34.5 & PCR$MeanBC<35.5)&(PCR$deltaBC<=2.1)),1,PCR$concorBC)
#Efficiency is between 1.95 and 2.05, differing Cq values.
PCR$concorBC <- ifelse(((PCR$E>=1.95 & PCR$E <2.05)&(PCR$MeanBC<31.5)&(PCR$deltaBC<=0.5)),1,PCR$concorBC)
PCR$concorBC <- ifelse(((PCR$E>=1.95 & PCR$E <2.05)&(PCR$MeanBC>=31.5 & PCR$MeanBC<32.5)&(PCR$deltaBC<=0.7)),1,PCR$concorBC)
PCR$concorBC <- ifelse(((PCR$E>=1.95 & PCR$E <2.05)&(PCR$MeanBC>=32.5 & PCR$MeanBC<33.5)&(PCR$deltaBC<=0.9)),1,PCR$concorBC)
PCR$concorBC <- ifelse(((PCR$E>=1.95 & PCR$E <2.05)&(PCR$MeanBC>=33.5 & PCR$MeanBC<34.5)&(PCR$deltaBC<=1.3)),1,PCR$concorBC)
PCR$concorBC <- ifelse(((PCR$E>=1.95 & PCR$E <2.05)&(PCR$MeanBC>=34.5 & PCR$MeanBC<35.5)&(PCR$deltaBC<=1.9)),1,PCR$concorBC)

# note that PCR$concorBC = 0 for all mean Cqs > 35 (undetectables).
# concorACB = 1 (concordance between replicates)
# concorACB = 0 (discordance between replicates)


####Make final variable concordance#####
PCR$concor <- 0
PCR$concor <- ifelse(PCR$concorAB==1 & PCR$concorBC == 0,1,PCR$concor)
PCR$concor <- ifelse(PCR$concorAB==0 & PCR$concorBC == 1,2,PCR$concor)
PCR$concor <- ifelse(PCR$concorABC==1,3,PCR$concor)

# 0 = 'none of deltas in range'
# 1 = 'delta AB in range', 
# 2 = 'delta BC in range', 
# 3 = 'both deltas in range

#########Step 3: If mean Cq>critical value, recode as nondetect##########
#########################################################################
#If Cq>=35 THEN recode as nondetect (0)
#If Cq=0 in both duplicates, also recode as nondetect (0)
PCR$critABC <- 0
PCR$critAB <- 0
PCR$critBC <- 0
PCR$critABC <- ifelse((PCR$MeanABC<=35 & PCR$MeanABC >15),1,PCR$critABC) 
PCR$critAB <- ifelse((PCR$MeanAB<=35 & PCR$MeanAB >15),1,PCR$critAB) 
PCR$critBC <- ifelse((PCR$MeanBC<=35 & PCR$MeanBC >15),1,PCR$critBC) 


##########Final result 3 good curves#######
###########################################


# IF 3 good curves and all deltas in range. 
PCR$finalQC <- ifelse(PCR$qPC == 2 & PCR$concor ==3 & PCR$critABC ==1,3,NA) 
PCR$finalQC <- ifelse(PCR$qPC == 2 & PCR$concor ==3 & PCR$critABC ==0,0,PCR$finalQC) 
# IF 3 good curves and delta AB in range. 
PCR$finalQC <- ifelse(PCR$qPC == 2 & PCR$concor ==1 & PCR$critAB ==1,1,PCR$finalQC) 
PCR$finalQC <- ifelse(PCR$qPC == 2 & PCR$concor ==1 & PCR$critAB ==0,0,PCR$finalQC) 
# IF 3 good curves and delta AB in range. 
PCR$finalQC <- ifelse(PCR$qPC == 2 & PCR$concor ==2 & PCR$critBC ==1,2,PCR$finalQC) 
PCR$finalQC <- ifelse(PCR$qPC == 2 & PCR$concor ==2 & PCR$critBC ==0,0,PCR$finalQC) 
# IF 3 good curves and deltas all not in range. 
PCR$finalQC <- ifelse(PCR$qPC == 2 & PCR$concor ==0 & PCR$critABC ==1,5,PCR$finalQC) 
PCR$finalQC <- ifelse(PCR$qPC == 2 & PCR$concor ==0 & PCR$critABC ==0,0,PCR$finalQC) 


# 0 = 'undetectable'.
# 1 = 'mean AB valid'
# 2 = 'mean BC valid'
# 3 = 'mean all valid'
# 4 = 'mean duplos valid'
# 5 = 'invalid'

##########################################
##############2 GOOD CURVES###############
##########################################

#########Step 2: Delta in Range##########
#########################################

# consider the 2 good curves duplos 
# select these duplos

PCR$duploCq1 <- ifelse(PCR$qPC == 1 & PCR$Curve1 == 0 & PCR$Curve2 == 0, PCR$Cq1, NA)
PCR$duploCq2 <- ifelse(PCR$qPC == 1 & PCR$Curve1 == 0 & PCR$Curve2 == 0, PCR$Cq2, NA)
PCR$duploCq1 <- ifelse(PCR$qPC == 1 & PCR$Curve2 == 0 & PCR$Curve3 == 0, PCR$Cq2, PCR$duploCq1)
PCR$duploCq2 <- ifelse(PCR$qPC == 1 & PCR$Curve2 == 0 & PCR$Curve3 == 0, PCR$Cq3, PCR$duploCq2)
PCR$duploCq1 <- ifelse(PCR$qPC == 1 & PCR$Curve1 == 0 & PCR$Curve3 == 0, PCR$Cq1, PCR$duploCq1)
PCR$duploCq2 <- ifelse(PCR$qPC == 1 & PCR$Curve1 == 0 & PCR$Curve3 == 0, PCR$Cq3, PCR$duploCq2)


#Mean Cq of duplo's is calculated first, then this variable is altered if:
#If one of the Cq's is 0, then these will not be calculated with 
#and the mean is shown as the Cq of the working duplo.
PCR$MeanCqduplo <- ((PCR$duploCq1 + PCR$duploCq2)/2)
PCR$MeanCqduplo <- ifelse(PCR$duploCq1==0,PCR$duploCq2,PCR$MeanCqduplo)
PCR$MeanCqduplo <- ifelse(PCR$duploCq2==0, PCR$duploCq1, PCR$MeanCqduplo)

#To compute the difference between the 2 duplo's
PCR$CqDiffduplo <- (sqrt((PCR$duploCq1-PCR$duploCq2)^2))

#Delta Cq is dependent on the PCR efficiency of the experiment.
#Calculations for Efficiency from 1.45 - 2.05 are given below.
#First give the new variable the value 0.
PCR$concorduplo <- 0
#This one is always <= 0.5, for every efficiency.
PCR$concorduplo <- ifelse((PCR$MeanCqduplo<26.5 & PCR$CqDiffduplo<=0.5),1,PCR$concorduplo)
#Efficiency is between 1.45 and 1.55, differing Cq values.
PCR$concorduplo <- ifelse(((PCR$E>=1.45 & PCR$E <1.55)&(PCR$MeanCqduplo>=26.5 & PCR$MeanCqduplo<27.5)&(PCR$CqDiffduplo<=0.6)),1,PCR$concorduplo)
PCR$concorduplo <- ifelse(((PCR$E>=1.45 & PCR$E <1.55)&(PCR$MeanCqduplo>=27.5 & PCR$MeanCqduplo<28.5)&(PCR$CqDiffduplo<=0.8)),1,PCR$concorduplo)
PCR$concorduplo <- ifelse(((PCR$E>=1.45 & PCR$E <1.55)&(PCR$MeanCqduplo>=28.5 & PCR$MeanCqduplo<29.5)&(PCR$CqDiffduplo<=0.9)),1,PCR$concorduplo)
PCR$concorduplo <- ifelse(((PCR$E>=1.45 & PCR$E <1.55)&(PCR$MeanCqduplo>=29.5 & PCR$MeanCqduplo<30.5)&(PCR$CqDiffduplo<=1.1)),1,PCR$concorduplo)
PCR$concorduplo <- ifelse(((PCR$E>=1.45 & PCR$E <1.55)&(PCR$MeanCqduplo>=30.5 & PCR$MeanCqduplo<31.5)&(PCR$CqDiffduplo<=1.4)),1,PCR$concorduplo)
PCR$concorduplo <- ifelse(((PCR$E>=1.45 & PCR$E <1.55)&(PCR$MeanCqduplo>=31.5 & PCR$MeanCqduplo<32.5)&(PCR$CqDiffduplo<=1.7)),1,PCR$concorduplo)
PCR$concorduplo <- ifelse(((PCR$E>=1.45 & PCR$E <1.55)&(PCR$MeanCqduplo>=32.5 & PCR$MeanCqduplo<33.5)&(PCR$CqDiffduplo<=2.1)),1,PCR$concorduplo)
PCR$concorduplo <- ifelse(((PCR$E>=1.45 & PCR$E <1.55)&(PCR$MeanCqduplo>=33.5 & PCR$MeanCqduplo<34.5)&(PCR$CqDiffduplo<=2.7)),1,PCR$concorduplo)
PCR$concorduplo <- ifelse(((PCR$E>=1.45 & PCR$E <1.55)&(PCR$MeanCqduplo>=34.5 & PCR$MeanCqduplo<35.5)&(PCR$CqDiffduplo<=3.3)),1,PCR$concorduplo)
#Efficency is between 1.55 and 1.65, differing Cq values.
PCR$concorduplo <- ifelse(((PCR$E>=1.55 & PCR$E <1.65)&(PCR$MeanCqduplo<28.5)&(PCR$CqDiffduplo<=0.5)),1,PCR$concorduplo)
PCR$concorduplo <- ifelse(((PCR$E>=1.55 & PCR$E <1.65)&(PCR$MeanCqduplo>=28.5 & PCR$MeanCqduplo<29.5)&(PCR$CqDiffduplo<=0.7)),1,PCR$concorduplo)
PCR$concorduplo <- ifelse(((PCR$E>=1.55 & PCR$E <1.65)&(PCR$MeanCqduplo>=29.5 & PCR$MeanCqduplo<30.5)&(PCR$CqDiffduplo<=0.8)),1,PCR$concorduplo)
PCR$concorduplo <- ifelse(((PCR$E>=1.55 & PCR$E <1.65)&(PCR$MeanCqduplo>=30.5 & PCR$MeanCqduplo<31.5)&(PCR$CqDiffduplo<=1.1)),1,PCR$concorduplo)
PCR$concorduplo <- ifelse(((PCR$E>=1.55 & PCR$E <1.65)&(PCR$MeanCqduplo>=31.5 & PCR$MeanCqduplo<32.5)&(PCR$CqDiffduplo<=1.4)),1,PCR$concorduplo)
PCR$concorduplo <- ifelse(((PCR$E>=1.55 & PCR$E <1.65)&(PCR$MeanCqduplo>=32.5 & PCR$MeanCqduplo<33.5)&(PCR$CqDiffduplo<=1.7)),1,PCR$concorduplo)
PCR$concorduplo <- ifelse(((PCR$E>=1.55 & PCR$E <1.65)&(PCR$MeanCqduplo>=33.5 & PCR$MeanCqduplo<34.5)&(PCR$CqDiffduplo<=2.2)),1,PCR$concorduplo)
PCR$concorduplo <- ifelse(((PCR$E>=1.55 & PCR$E <1.65)&(PCR$MeanCqduplo>=34.5 & PCR$MeanCqduplo<35.5)&(PCR$CqDiffduplo<=2.9)),1,PCR$concorduplo)
#Efficiency is between 1.65 and 1.75, differing Cq values.
PCR$concorduplo <- ifelse(((PCR$E>=1.65 & PCR$E <1.75)&(PCR$MeanCqduplo<29.5)&(PCR$CqDiffduplo<=0.5)),1,PCR$concorduplo)
PCR$concorduplo <- ifelse(((PCR$E>=1.65 & PCR$E <1.75)&(PCR$MeanCqduplo>=29.5 & PCR$MeanCqduplo<30.5)&(PCR$CqDiffduplo<=0.6)),1,PCR$concorduplo)
PCR$concorduplo <- ifelse(((PCR$E>=1.65 & PCR$E <1.75)&(PCR$MeanCqduplo>=30.5 & PCR$MeanCqduplo<31.5)&(PCR$CqDiffduplo<=0.8)),1,PCR$concorduplo)
PCR$concorduplo <- ifelse(((PCR$E>=1.65 & PCR$E <1.75)&(PCR$MeanCqduplo>=31.5 & PCR$MeanCqduplo<32.5)&(PCR$CqDiffduplo<=1.1)),1,PCR$concorduplo)
PCR$concorduplo <- ifelse(((PCR$E>=1.65 & PCR$E <1.75)&(PCR$MeanCqduplo>=32.5 & PCR$MeanCqduplo<33.5)&(PCR$CqDiffduplo<=1.5)),1,PCR$concorduplo)
PCR$concorduplo <- ifelse(((PCR$E>=1.65 & PCR$E <1.75)&(PCR$MeanCqduplo>=33.5 & PCR$MeanCqduplo<34.5)&(PCR$CqDiffduplo<=1.9)),1,PCR$concorduplo)
PCR$concorduplo <- ifelse(((PCR$E>=1.65 & PCR$E <1.75)&(PCR$MeanCqduplo>=34.5 & PCR$MeanCqduplo<35.5)&(PCR$CqDiffduplo<=2.5)),1,PCR$concorduplo)
#Efficiency is between 1.75 and 1.85, differing Cq values. 
PCR$concorduplo <- ifelse(((PCR$E>=1.75 & PCR$E <1.85)&(PCR$MeanCqduplo<30.5)&(PCR$CqDiffduplo<=0.5)),1,PCR$concorduplo)
PCR$concorduplo <- ifelse(((PCR$E>=1.75 & PCR$E <1.85)&(PCR$MeanCqduplo>=30.5 & PCR$MeanCqduplo<31.5)&(PCR$CqDiffduplo<=0.7)),1,PCR$concorduplo)
PCR$concorduplo <- ifelse(((PCR$E>=1.75 & PCR$E <1.85)&(PCR$MeanCqduplo>=31.5 & PCR$MeanCqduplo<32.5)&(PCR$CqDiffduplo<=0.9)),1,PCR$concorduplo)
PCR$concorduplo <- ifelse(((PCR$E>=1.75 & PCR$E <1.85)&(PCR$MeanCqduplo>=32.5 & PCR$MeanCqduplo<33.5)&(PCR$CqDiffduplo<=1.2)),1,PCR$concorduplo)
PCR$concorduplo <- ifelse(((PCR$E>=1.75 & PCR$E <1.85)&(PCR$MeanCqduplo>=33.5 & PCR$MeanCqduplo<34.5)&(PCR$CqDiffduplo<=1.7)),1,PCR$concorduplo)
PCR$concorduplo <- ifelse(((PCR$E>=1.75 & PCR$E <1.85)&(PCR$MeanCqduplo>=34.5 & PCR$MeanCqduplo<35.5)&(PCR$CqDiffduplo<=2.3)),1,PCR$concorduplo)
#Efficiency is between 1.85 and 1.95, differing Cq values. 
PCR$concorduplo <- ifelse(((PCR$E>=1.85 & PCR$E <1.95)&(PCR$MeanCqduplo<31.5)&(PCR$CqDiffduplo<=0.5)),1,PCR$concorduplo)
PCR$concorduplo <- ifelse(((PCR$E>=1.85 & PCR$E <1.95)&(PCR$MeanCqduplo>=31.5 & PCR$MeanCqduplo<32.5)&(PCR$CqDiffduplo<=0.8)),1,PCR$concorduplo)
PCR$concorduplo <- ifelse(((PCR$E>=1.85 & PCR$E <1.95)&(PCR$MeanCqduplo>=32.5 & PCR$MeanCqduplo<33.5)&(PCR$CqDiffduplo<=1.1)),1,PCR$concorduplo)
PCR$concorduplo <- ifelse(((PCR$E>=1.85 & PCR$E <1.95)&(PCR$MeanCqduplo>=33.5 & PCR$MeanCqduplo<34.5)&(PCR$CqDiffduplo<=1.5)),1,PCR$concorduplo)
PCR$concorduplo <- ifelse(((PCR$E>=1.85 & PCR$E <1.95)&(PCR$MeanCqduplo>=34.5 & PCR$MeanCqduplo<35.5)&(PCR$CqDiffduplo<=2.1)),1,PCR$concorduplo)
#Efficiency is between 1.95 and 2.05, differing Cq values.
PCR$concorduplo <- ifelse(((PCR$E>=1.95 & PCR$E <2.05)&(PCR$MeanCqduplo<31.5)&(PCR$CqDiffduplo<=0.5)),1,PCR$concorduplo)
PCR$concorduplo <- ifelse(((PCR$E>=1.95 & PCR$E <2.05)&(PCR$MeanCqduplo>=31.5 & PCR$MeanCqduplo<32.5)&(PCR$CqDiffduplo<=0.7)),1,PCR$concorduplo)
PCR$concorduplo <- ifelse(((PCR$E>=1.95 & PCR$E <2.05)&(PCR$MeanCqduplo>=32.5 & PCR$MeanCqduplo<33.5)&(PCR$CqDiffduplo<=0.9)),1,PCR$concorduplo)
PCR$concorduplo <- ifelse(((PCR$E>=1.95 & PCR$E <2.05)&(PCR$MeanCqduplo>=33.5 & PCR$MeanCqduplo<34.5)&(PCR$CqDiffduplo<=1.3)),1,PCR$concorduplo)
PCR$concorduplo <- ifelse(((PCR$E>=1.95 & PCR$E <2.05)&(PCR$MeanCqduplo>=34.5 & PCR$MeanCqduplo<35.5)&(PCR$CqDiffduplo<=1.9)),1,PCR$concorduplo)

# note that PCR$concorduplo = 0 for all mean Cqs > 35 (undetectables).
# concorACB = 1 (concordance between replicates)
# concorACB = 0 (discordance between replicates)


#########Step 3: If mean Cq>critical value (35), recode as nondetect##########
##############################################################################
#If Cq>=35 THEN recode as nondetect (0)
#If Cq=0 in both duplo's, also recode as nondetect (0).
PCR$critduplo <- 0
PCR$critduplo <- ifelse((PCR$MeanCqduplo<=35 & PCR$MeanCqduplo >15),1,PCR$critduplo) 



##########Final result 2 good curves#######
###########################################


# IF 3 good curves and all deltas in range. 
PCR$finalQC <- ifelse(PCR$qPC == 1 & PCR$concorduplo ==1 & PCR$critduplo ==1,4,PCR$finalQC)
PCR$finalQC <- ifelse(PCR$qPC == 1 & PCR$concorduplo ==1 & PCR$critduplo ==0,0,PCR$finalQC) 
PCR$finalQC <- ifelse(PCR$qPC == 1 & PCR$concorduplo ==0 & PCR$critduplo ==0,0,PCR$finalQC) 
PCR$finalQC <- ifelse(PCR$qPC == 1 & PCR$concorduplo ==0 & PCR$critduplo ==1,5,PCR$finalQC) 


# 0 = 'undetectable'.
# 1 = 'mean AB valid'
# 2 = 'mean BC valid'
# 3 = 'mean all valid'
# 4 = 'mean duplos valid'
# 5 = 'invalid'




##########################################
##############1 or 0 GOOD CURVES##########
##########################################

PCR$finalQC <- ifelse(PCR$qPC == 0 & PCR$MeanABC == 0,0,PCR$finalQC)
PCR$finalQC <- ifelse(PCR$qPC == 0 & PCR$MeanABC > 35,0,PCR$finalQC)
PCR$finalQC <- ifelse(PCR$qPC == 0 & (PCR$MeanABC <= 35 & PCR$MeanABC >0.1),5,PCR$finalQC)

PCR$finalQC



#####################################################
##############OPTIONAL: Calculate N0 values##########
#####################################################


#Calculate final N0 variable, for valid QC(only necessary when working with N0 values).

PCR$N0min <-    PCR$Nq/(PCR$E^PCR$minCq)
PCR$N0middle <- PCR$Nq/(PCR$E^PCR$middleCq)
PCR$N0max <-    PCR$Nq/(PCR$E^PCR$maxCq)
PCR$N0Duplo1 <- PCR$Nq/(PCR$E^PCR$duploCq1)
PCR$N0Duplo2 <- PCR$Nq/(PCR$E^PCR$duploCq2)


PCR$finalN0 <- ifelse(PCR$finalQC == 1,((PCR$N0min+PCR$N0middle)/2),NA)
PCR$finalN0 <- ifelse(PCR$finalQC == 2,((PCR$N0max+PCR$N0middle)/2),PCR$finalN0)
PCR$finalN0 <- ifelse(PCR$finalQC == 3,((PCR$N0max+PCR$N0middle+PCR$N0min)/3),PCR$finalN0)
PCR$finalN0 <- ifelse(PCR$finalQC == 4,((PCR$N0Duplo1+PCR$N0Duplo2)/2),PCR$finalN0)

Datafinal <- cbind(PCR$Patient_nr, PCR$miRNA, PCR$finalQC, PCR$finalN0)
colnames(Datafinal) <- c("Sample nr","target","QC","Final N0")

Datafinal



