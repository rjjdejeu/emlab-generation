setwd("~/emlab-generation/rscripts")
source("rConfig.R")
source("batchRunAnalysis.R")

#Melt dataframe
# moltenDF <- melt(bigDF, id.vars = c("runId","tick"))
# moltenDF

#Initiate
technologyOrder=c("Nuclear","Lignite","CoalPSC","IGCC","CCGT","OCGT","Biomass","Biogas","Wind","WindOffshore","PV")
technologyPalette=c("CoalPSC" = "black", "Biomass" = "darkgreen", "Biogas"="darkolivegreen3", "Nuclear" = "purple", "Lignite" = "saddlebrown",
                    "OCGT" = "darkred", "CCGT" = "blue", "PV" = "yellow", "Wind" = "chartreuse4",
                    "CoalPscCCS" = "darkgray", "IGCC" = "orange", "IgccCCS"="orangered", "CcgtCCS" = "red",
                    "WindOffshore" = "navyblue", "HydroPower" = "skyblue3")
producerPalette=c("A"="black","B"="darkgreen","C"="purple","D"="darkred","E"="blue","F"="yellow","G"="orange","H"="navyblue","I"="darkgrey")


library(gridExtra)
library(TeachingDemos)
library(grid)
library(ggplot2)
library(reshape2)
library(plotrix)

#File and folder initiation
nameFile <- "SameTarget"
analysisFolder <- "~/Desktop/emlabGen/output/"
analysisFolder <- paste(analysisFolder, nameFile, "/", sep="")
analysisFolder
setwd(analysisFolder)
analysisFile <- paste(nameFile, ".csv", sep="")
analysisFile
scaleFactor <- 0.75
filePrefix <- nameFile

#Read csv-file
bigDF <- read.csv(analysisFile)
tickDF <- read.csv("~/Desktop/emlabGen/analysis/39ticks.csv")
# segmentDF <- read.csv("~/Desktop/emlabGen/output/nameFile/nameFile-SegmentClearingPoints.csv")
targetDF <- read.csv("~/Desktop/emlabGen/analysis/R2_policyGoalNREAP_NL_DE_2050.csv")

tick <- bigDF$tick
# tick

#Clean csv-file
library(stringr)
#bigDF$runId <- str_replace(bigDF$runId,"\\..*","")
#bigDF$runNumber <- bigDF$runId
# Some colnames start with "X.", get rid of this 
colnames(bigDF) = gsub("X\\.", "", colnames(bigDF))
# Get rid of periods at the start and end of the names
colnames(bigDF) = gsub("^\\.|\\.$", "", colnames(bigDF))
# Convert all periods into underscores
colnames(bigDF) = gsub("\\.", "_", colnames(bigDF))
#summary(bigDF)



pSA_1 = 
  ggplot(data=bigDF, aes(x=tick)) + 
  geom_smooth(aes(y=bigDF$PriceInEURperMWh_Segment_Country_A_1), method= "loess") + #(aes(colour = runId)) +
  xlab("Tick") +  
  ylab("Price (EUR/MWh)") + 
  ggtitle("Segment 1 - Country A") #give the plot a title
plot(pSA_1)
ggsave(filename = paste(filePrefix, "PriceSegmentA1.png", sep=""))


pSA_2 = 
  ggplot(data=bigDF, aes(x=tick)) + 
  geom_smooth(aes(y=bigDF$PriceInEURperMWh_Segment_Country_A_2), method= "loess") + #(aes(colour = runId)) +
  xlab("Tick") +  
  ylab("Price (EUR/MWh)") + 
  ggtitle("Segment 2 - Country A") #give the plot a title
plot(pSA_2)
ggsave(filename = paste(filePrefix, "PriceSegmentA2.png", sep=""))


pSA_3 = 
  ggplot(data=bigDF, aes(x=tick)) + 
  geom_smooth(aes(y=bigDF$PriceInEURperMWh_Segment_Country_A_3), method= "loess") + #(aes(colour = runId)) +
  xlab("Tick") +  
  ylab("Price (EUR/MWh)") + 
  ggtitle("Segment 3 - Country A") #give the plot a title
plot(pSA_3)
ggsave(filename = paste(filePrefix, "PriceSegmentA3.png", sep=""))


pSA_4 = 
  ggplot(data=bigDF, aes(x=tick)) + 
  geom_smooth(aes(y=bigDF$PriceInEURperMWh_Segment_Country_A_4), method= "loess") + #(aes(colour = runId)) +
  xlab("Tick") +  
  ylab("Price (EUR/MWh)") + 
  ggtitle("Segment 4 - Country A") #give the plot a title
plot(pSA_4)
ggsave(filename = paste(filePrefix, "PriceSegmentA4.png", sep=""))


pSB_1 = 
  ggplot(data=bigDF, aes(x=tick)) + 
  geom_smooth(aes(y=bigDF$PriceInEURperMWh_Segment_Country_B_1), method= "loess") + #(aes(colour = runId)) +
  xlab("Tick") +  
  ylab("Price (EUR/MWh)") + 
  ggtitle("Segment 1 - Country B") #give the plot a title
plot(pSB_1)
ggsave(filename = paste(filePrefix, "PriceSegmentB1.png", sep=""))


pSB_2 = 
  ggplot(data=bigDF, aes(x=tick)) + 
  geom_smooth(aes(y=bigDF$PriceInEURperMWh_Segment_Country_B_2), method= "loess") + #(aes(colour = runId)) +
  xlab("Tick") +  
  ylab("Price (EUR/MWh)") + 
  ggtitle("Segment 2 - Country B") #give the plot a title
plot(pSB_2)
ggsave(filename = paste(filePrefix, "PriceSegmentB2.png", sep=""))


pSB_3 = 
  ggplot(data=bigDF, aes(x=tick)) + 
  geom_smooth(aes(y=bigDF$PriceInEURperMWh_Segment_Country_B_3), method= "loess") + #(aes(colour = runId)) +
  xlab("Tick") +  
  ylab("Price (EUR/MWh)") + 
  ggtitle("Segment 3 - Country B") #give the plot a title
plot(pSB_3)
ggsave(filename = paste(filePrefix, "PriceSegmentB3.png", sep=""))


pSB_4 = 
  ggplot(data=bigDF, aes(x=tick)) + 
  geom_smooth(aes(y=bigDF$PriceInEURperMWh_Segment_Country_B_4), method= "loess") + #(aes(colour = runId)) +
  xlab("Tick") +  
  ylab("Price (EUR/MWh)") + 
  ggtitle("Segment 4 - Country B") #give the plot a title
plot(pSB_4)
ggsave(filename = paste(filePrefix, "PriceSegmentB4.png", sep=""))

segmentPricesA <-multiplot(pSB_4, cols=2)


meanRealizedTargetA=0
meanRealizedTargetB=0
seRealizedTargetA=0
seRealizedTargetB=0

for(j in 0:39) {
  meanRealizedTargetA[j] <- mean(subset( 
    ((bigDF$GenerationinMWhCountryA_Photovoltaic + bigDF$GenerationinMWhCountryA_Wind + bigDF$GenerationinMWhCountryA_Biomass + 
        bigDF$GenerationinMWhCountryA_HydroPower + bigDF$GenerationinMWhCountryA_Biogas + bigDF$GenerationinMWhCountryA_WindOffshore)/ bigDF$Total_DemandinMWh_Country_A) 
    , tick == j))
  
  meanRealizedTargetB[j] <- mean(subset( 
    ((bigDF$GenerationinMWhCountryB_Photovoltaic + bigDF$GenerationinMWhCountryB_Wind + bigDF$GenerationinMWhCountryB_Biomass + 
        bigDF$GenerationinMWhCountryB_HydroPower + bigDF$GenerationinMWhCountryB_Biogas + bigDF$GenerationinMWhCountryB_WindOffshore) / bigDF$Total_DemandinMWh_Country_B) 
    , tick == j))
  
  seRealizedTargetA[j] <- std.error(subset( 
    ((bigDF$GenerationinMWhCountryA_Photovoltaic + bigDF$GenerationinMWhCountryA_Wind + bigDF$GenerationinMWhCountryA_Biomass + 
        bigDF$GenerationinMWhCountryA_HydroPower + bigDF$GenerationinMWhCountryA_Biogas + bigDF$GenerationinMWhCountryA_WindOffshore) / bigDF$Total_DemandinMWh_Country_A) 
    , tick == j))
  
  seRealizedTargetB[j] <- std.error(subset( 
    ((bigDF$GenerationinMWhCountryB_Photovoltaic + bigDF$GenerationinMWhCountryB_Wind + bigDF$GenerationinMWhCountryB_Biomass + 
        bigDF$GenerationinMWhCountryB_HydroPower + bigDF$GenerationinMWhCountryB_Biogas + bigDF$GenerationinMWhCountryB_WindOffshore) / bigDF$Total_DemandinMWh_Country_B) 
    , tick == j))
}

meanRealizedTargetA
meanRealizedTargetB
seRealizedTargetA
seRealizedTargetB




DataTable <- c(mean((meanRealizedTargetA-targetDF$nl_target_slow)*100))
DataTable <- rbind(DataTable, c(mean((meanRealizedTargetB-targetDF$de_target)*100)))
rownames(DataTable) <- c("Target deviation A", "Target deviation B")
write.csv(DataTable, "DataTableTargetDeviation.csv")

renewableTargetDeviationAplot = ggplot(data=targetDF, aes(x=tick, y=(meanRealizedTargetA-nl_target_slow)*100)) + 
  geom_point() + 
  xlab("Year") +  
  ylab("[%]") + 
  ggtitle("Difference Realized Renewable Target and \n NREAP target (Country A)") #give the plot a title
plot(renewableTargetDeviationAplot)
ggsave(filename = paste(filePrefix, "renewableTargetDeviationA.png", sep=""),scale=1)

renewableTargetDeviationBplot = ggplot(data=targetDF, aes(x=tick, y=(meanRealizedTargetB-de_target)*100)) + 
  geom_point() + 
  xlab("Year") +  
  ylab("[%]") + 
  ggtitle("Difference Realized Renewable Target and \n NREAP target (Country B)") #give the plot a title
plot(renewableTargetDeviationBplot)
ggsave(filename = paste(filePrefix, "renewableTargetDeviationB.png", sep=""),scale=1)



# Average electricity wholesale price in country
meanElectricityPriceA=0
meanElectricityPriceB=0

for(j in 0:39) {
  meanElectricityPriceA[j] <- mean(subset(bigDF$Avg_El_PricesinEURpMWh_Country_A, tick == j))
  meanElectricityPriceB[j] <- mean(subset(bigDF$Avg_El_PricesinEURpMWh_Country_B, tick == j))
}
meanElectricityPriceA
meanElectricityPriceB


#Tender Clearing Prices, subsidies
meanTenderClearingPriceA=0
meanTenderClearingPriceB=0

for(j in 0:39) {
  meanTenderClearingPriceA[j] <- mean(subset(bigDF$tenderClearingPrice_Country_A, tick == j))
  meanTenderClearingPriceB[j] <- mean(subset(bigDF$tenderClearingPrice_Country_B, tick == j))
}
meanTenderClearingPriceA
meanTenderClearingPriceB

## Supply ratios
meanSupplyRatioA=0
meanSupplyRatioB=0

for(j in 0:39) {
  meanSupplyRatioA[j] <- mean(subset(bigDF$TotalOperationalCapacityPerZoneInMW_Country_A/bigDF$PeakDemandPerZoneInMW_Country_A, tick == j))
  meanSupplyRatioB[j] <- mean(subset(bigDF$TotalOperationalCapacityPerZoneInMW_Country_B/bigDF$PeakDemandPerZoneInMW_Country_B, tick == j))
}
meanSupplyRatioA
meanSupplyRatioB

DataTable <- c(mean(meanTenderClearingPriceA))
DataTable <- rbind(DataTable, c(mean(meanTenderClearingPriceB)))
DataTable <- rbind(DataTable, c(sd(meanTenderClearingPriceA)))
DataTable <- rbind(DataTable, c(sd(meanTenderClearingPriceB)))
DataTable <- rbind(DataTable, c(mean(meanElectricityPriceA)))
DataTable <- rbind(DataTable, c(mean(meanElectricityPriceB)))
DataTable <- rbind(DataTable, c(sd(meanElectricityPriceA)))
DataTable <- rbind(DataTable, c(sd(meanElectricityPriceB)))
DataTable <- rbind(DataTable, c(mean(meanSupplyRatioA)))
DataTable <- rbind(DataTable, c(mean(meanSupplyRatioB)))
DataTable <- rbind(DataTable, c(sd(meanSupplyRatioA)))
DataTable <- rbind(DataTable, c(sd(meanSupplyRatioB)))
rownames(DataTable) <- c("Mean Tender Price A", "Mean Tender Price B","SD Tender Price A", "SD Tender Price B","Mean El Price A", "Mean El Price B","SD El Price A", "SD El Price B","Mean SR A", "Mean SR B", "SD SR A", "SD SR B")
write.csv(DataTable, "DataTablePricesSR.csv")

## Costs breakdown
meanFixedOMCostsA=0
meanLoanCostsA=0
meanCommodityCostsA=0
meanDownpaymentsCostsA=0
seFixedOMCostsA=0
seLoanCostsA=0
seCommodityCostsA=0
seDownpaymentsCostsA=0
meanFixedOMCostsB=0
meanLoanCostsB=0
meanCommodityCostsB=0
meanDownpaymentsCostsB=0
seFixedOMCostsB=0
seLoanCostsB=0
seCommodityCostsB=0
seDownpaymentsCostsB=0

for(j in 0:39) {
  meanFixedOMCostsA[j] <- mean(subset(bigDF$CountryAProdCosts_Fixed_O_M, tick == j)) 
  meanLoanCostsA[j] <- mean(subset(bigDF$CountryAProdCosts_Loan, tick == j))
  meanCommodityCostsA[j] <- mean(subset(bigDF$CountryAProdCosts_Commodity, tick == j)) 
  meanDownpaymentsCostsA[j] <- mean(subset(bigDF$CountryAProdCosts_Downpayment, tick == j))
  seFixedOMCostsA[j] <- std.error(subset(bigDF$CountryAProdCosts_Fixed_O_M, tick == j))  
  seLoanCostsA[j] <- std.error(subset(bigDF$CountryAProdCosts_Loan, tick == j)) 
  seCommodityCostsA[j] <- std.error(subset(bigDF$CountryAProdCosts_Commodity, tick == j)) 
  seDownpaymentsCostsA[j] <- std.error(subset(bigDF$CountryAProdCosts_Downpayment, tick == j))
  meanFixedOMCostsB[j] <- mean(subset(bigDF$CountryBProdCosts_Fixed_O_M, tick == j)) 
  meanLoanCostsB[j] <- mean(subset(bigDF$CountryBProdCosts_Loan, tick == j))
  meanCommodityCostsB[j] <- mean(subset(bigDF$CountryBProdCosts_Commodity, tick == j)) 
  meanDownpaymentsCostsB[j] <- mean(subset(bigDF$CountryBProdCosts_Downpayment, tick == j))
  seFixedOMCostsB[j] <- std.error(subset(bigDF$CountryBProdCosts_Fixed_O_M, tick == j))  
  seLoanCostsB[j] <- std.error(subset(bigDF$CountryBProdCosts_Loan, tick == j)) 
  seCommodityCostsB[j] <- std.error(subset(bigDF$CountryBProdCosts_Commodity, tick == j)) 
  seDownpaymentsCostsB[j] <- std.error(subset(bigDF$CountryBProdCosts_Downpayment, tick == j))
}

fixedOMcostsAplot = ggplot(data=tickDF, aes(x=X0, y=meanFixedOMCostsA)) + 
  geom_errorbar(aes(ymin=meanFixedOMCostsA-seFixedOMCostsA, ymax=meanFixedOMCostsA+seFixedOMCostsA, width=1)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Fixed O&M Costs \n Country A") #give the plot a title
plot(fixedOMcostsAplot)
ggsave(filename = paste(filePrefix, "fixedOMcostsAplot.png", sep=""),scale=1)

loanCostsAplot = ggplot(data=tickDF, aes(x=X0, y=meanLoanCostsA)) + 
  geom_errorbar(aes(ymin=meanLoanCostsA-seLoanCostsA, ymax=meanLoanCostsA+seLoanCostsA, width=1)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Loan Costs \n Country A") #give the plot a title
plot(loanCostsAplot)
ggsave(filename = paste(filePrefix, "loanCostsAplot.png", sep=""),scale=1)

commodityCostsAplot = ggplot(data=tickDF, aes(x=X0, y= meanCommodityCostsA)) + 
  geom_errorbar(aes(ymin=meanCommodityCostsA-seCommodityCostsA, ymax= meanCommodityCostsA+seCommodityCostsA, width=1)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Commodity Costs \n Country A") #give the plot a title
plot(commodityCostsAplot)
ggsave(filename = paste(filePrefix, "commodityCostsAplot.png", sep=""),scale=1)

downpaymentsCostsAplot = ggplot(data=tickDF, aes(x=X0, y=meanDownpaymentsCostsA)) + 
  geom_errorbar(aes(ymin=meanDownpaymentsCostsA-seDownpaymentsCostsA, ymax=meanDownpaymentsCostsA+seDownpaymentsCostsA, width=1)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Downpayment Costs \n Country A") #give the plot a title
plot(downpaymentsCostsAplot)
ggsave(filename = paste(filePrefix, "downpaymentsCostsAplot.png", sep=""),scale=1)

fixedOMcostsBplot = ggplot(data=tickDF, aes(x=X0, y=meanFixedOMCostsB)) + 
  geom_errorbar(aes(ymin=meanFixedOMCostsB-seFixedOMCostsB, ymax=meanFixedOMCostsB+seFixedOMCostsB, width=1)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Fixed O&M Costs \n Country B") #give the plot a title
plot(fixedOMcostsBplot)
ggsave(filename = paste(filePrefix, "fixedOMcostsBplot.png", sep=""),scale=1)

loanCostsBplot = ggplot(data=tickDF, aes(x=X0, y=meanLoanCostsB)) + 
  geom_errorbar(aes(ymin=meanLoanCostsB-seLoanCostsB, ymax=meanLoanCostsB+seLoanCostsB, width=1)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Loan Costs \n Country B") #give the plot a title
plot(loanCostsBplot)
ggsave(filename = paste(filePrefix, "loanCostsBplot.png", sep=""),scale=1)

commodityCostsBplot = ggplot(data=tickDF, aes(x=X0, y= meanCommodityCostsB)) + 
  geom_errorbar(aes(ymin=meanCommodityCostsB-seCommodityCostsB, ymax= meanCommodityCostsB+seCommodityCostsB, width=1)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Commodity Costs \n Country B") #give the plot a title
plot(commodityCostsBplot)
ggsave(filename = paste(filePrefix, "commodityCostsBplot.png", sep=""),scale=1)

downpaymentsCostsBplot = ggplot(data=tickDF, aes(x=X0, y=meanDownpaymentsCostsB)) + 
  geom_errorbar(aes(ymin=meanDownpaymentsCostsB-seDownpaymentsCostsB, ymax=meanDownpaymentsCostsB+seDownpaymentsCostsB, width=1)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Downpayment Costs \n Country B") #give the plot a title
plot(downpaymentsCostsBplot)
ggsave(filename = paste(filePrefix, "downpaymentsCostsBplot.png", sep=""),scale=1)


meanAggProfitA=0
meanAggProfitB=0
seAggProfitA=0
seAggProfitB=0


for(j in 0:39) {
  meanAggProfitA[j] <- mean(subset(bigDF$CountryAProdFinances_Profit, tick == j))
  meanAggProfitB[j] <- mean(subset(bigDF$CountryBProdFinances_Profit, tick == j))
  seAggProfitA[j] <- std.error(subset(bigDF$CountryAProdFinances_Profit, tick == j))
  seAggProfitB[j] <- std.error(subset(bigDF$CountryBProdFinances_Profit, tick == j))
}

meanAggProfitA
meanAggProfitB
seAggProfitA
seAggProfitB



profitsAplot = ggplot(data=tickDF, aes(x=X0, y=meanAggProfitA)) + 
  geom_errorbar(aes(ymin=meanAggProfitA-seAggProfitA, ymax=meanAggProfitA+seAggProfitA, width=1)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Profits \n Country A") #give the plot a title
plot(profitsAplot)
ggsave(filename = paste(filePrefix, "profitsAplot.png", sep=""),scale=1)

profitsBplot = ggplot(data=tickDF, aes(x=X0, y=meanAggProfitB)) + 
  geom_errorbar(aes(ymin=meanAggProfitB-seAggProfitB, ymax=meanAggProfitB+seAggProfitB, width=1)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Profits \n Country B") #give the plot a title
plot(profitsBplot)
ggsave(filename = paste(filePrefix, "profitsBplot.png", sep=""),scale=1)

## Realized targets minus NREAP target
# Renewable generation in terms of demand (compare this with NREAP targets)
meanRealizedTargetA=0
meanRealizedTargetB=0
seRealizedTargetA=0
seRealizedTargetB=0

for(j in 0:39) {
  meanRealizedTargetA[j] <- mean(subset( 
    ((bigDF$GenerationinMWhCountryA_Photovoltaic + bigDF$GenerationinMWhCountryA_Wind + bigDF$GenerationinMWhCountryA_Biomass + 
        bigDF$GenerationinMWhCountryA_HydroPower + bigDF$GenerationinMWhCountryA_Biogas + bigDF$GenerationinMWhCountryA_WindOffshore)/ bigDF$Total_DemandinMWh_Country_A) 
    , tick == j))
  
  meanRealizedTargetB[j] <- mean(subset( 
    ((bigDF$GenerationinMWhCountryB_Photovoltaic + bigDF$GenerationinMWhCountryB_Wind + bigDF$GenerationinMWhCountryB_Biomass + 
        bigDF$GenerationinMWhCountryB_HydroPower + bigDF$GenerationinMWhCountryB_Biogas + bigDF$GenerationinMWhCountryB_WindOffshore) / bigDF$Total_DemandinMWh_Country_B) 
    , tick == j))
  
  seRealizedTargetA[j] <- std.error(subset( 
    ((bigDF$GenerationinMWhCountryA_Photovoltaic + bigDF$GenerationinMWhCountryA_Wind + bigDF$GenerationinMWhCountryA_Biomass + 
        bigDF$GenerationinMWhCountryA_HydroPower + bigDF$GenerationinMWhCountryA_Biogas + bigDF$GenerationinMWhCountryA_WindOffshore) / bigDF$Total_DemandinMWh_Country_A) 
    , tick == j))
  
  seRealizedTargetB[j] <- std.error(subset( 
    ((bigDF$GenerationinMWhCountryB_Photovoltaic + bigDF$GenerationinMWhCountryB_Wind + bigDF$GenerationinMWhCountryB_Biomass + 
        bigDF$GenerationinMWhCountryB_HydroPower + bigDF$GenerationinMWhCountryB_Biogas + bigDF$GenerationinMWhCountryB_WindOffshore) / bigDF$Total_DemandinMWh_Country_B) 
    , tick == j))
}

meanRealizedTargetA
meanRealizedTargetB
seRealizedTargetA
seRealizedTargetB

realizedTargetAplot = ggplot(data=tickDF, aes(x=X0, y=meanRealizedTargetA*100)) + 
  geom_errorbar(aes(ymin=(meanRealizedTargetA-seRealizedTargetA)*100, ymax=(meanRealizedTargetA+seRealizedTargetA)*100), width=1) + 
  geom_point() +
  xlab("Year") +  
  ylab("[%]") + 
  ggtitle("RES-E generation in terms of demand \n Country A") #give the plot a title
plot(realizedTargetAplot)
ggsave(filename = paste(filePrefix, "TESTrealizedTargetAplot.png", sep=""),scale=scaleFactor)

realizedTargetBplot = ggplot(data=tickDF, aes(x=X0, y=meanRealizedTargetB*100)) + 
  geom_errorbar(aes(ymin=(meanRealizedTargetB-seRealizedTargetB)*100, ymax=(meanRealizedTargetB+seRealizedTargetB)*100), width=1) + 
  geom_point() +
  xlab("Year") +  
  ylab("[%]") + 
  ggtitle("RES-E generation in terms of demand \n Country B") #give the plot a title
plot(realizedTargetBplot)
ggsave(filename = paste(filePrefix, "realizedTargetBplot.png", sep=""),scale=1)

renewableTargetDeviationBplot = ggplot(data=targetDF, aes(x=tick, y=(meanRealizedTargetB-de_target)*100)) + 
  geom_point() + 
  xlab("Year") +  
  ylab("[%]") + 
  ggtitle("Difference Realized Renewable Target and \n NREAP target (Country B)") #give the plot a title
plot(renewableTargetDeviationBplot)
ggsave(filename = paste(filePrefix, "renewableTargetDeviationA.png", sep=""),scale=1)

## Profits check
meanConsumerCostsA=0
meanProducerCostsA=0
meanGovernmentCostsA=0
meanProducerProfitsA=0
meanConsumerCostsB=0
meanProducerCostsB=0
meanGovernmentCostsB=0
meanAggProfit=0


for(j in 0:39) {
  meanAggProfit[j] <- mean(subset(bigDF$CountryAProdFinances_Profit + bigDF$CountryBProdFinances_Profit, tick == j))
  meanConsumerCostsA[j] <- mean(subset(bigDF$ConsumerExpenditure_Country_A_electricity_spot_market, tick == j))
  meanProducerCostsA[j] <- mean(subset(bigDF$CountryAProdCosts_Fixed_O_M + 
                                         bigDF$CountryAProdCosts_Loan + 
                                         bigDF$CountryAProdCosts_Commodity + 
                                         bigDF$CountryAProdCosts_Downpayment, tick == j))
  meanGovernmentCostsA[j] <- mean(subset(bigDF$CountryAProdFinances_Tender_Subsidy, tick == j))
  meanProducerProfitsA[j] <- mean(subset(bigDF$CountryAProdFinances_Profit, tick == j))
  
  meanConsumerCostsB[j] <- mean(subset(bigDF$ConsumerExpenditure_Country_B_electricity_spot_market, tick == j))
  meanProducerCostsB[j] <- mean(subset(bigDF$CountryBProdCosts_Fixed_O_M + 
                                         bigDF$CountryBProdCosts_Loan + 
                                         bigDF$CountryBProdCosts_Commodity + 
                                         bigDF$CountryBProdCosts_Downpayment, tick == j))
  meanGovernmentCostsB[j] <- mean(subset(bigDF$CountryBProdFinances_Tender_Subsidy, tick == j))
}

meanConsumerCostsA
meanProducerCostsA
meanGovernmentCostsA
meanConsumerCostsB
meanProducerCostsB
meanGovernmentCostsB
meanAggProfit

sum(bigDF$CountryAProdFinances_Profit)
sum(bigDF$CountryBProdFinances_Profit)

bigDF$TotalCostsA <- bigDF$CountryAProdCosts_Commodity + bigDF$CountryAProdCosts_Loan + bigDF$CountryAProdCosts_Downpayment + bigDF$CountryAProdCosts_Fixed_O_M
bigDF$TotalCostsB <- bigDF$CountryBProdCosts_Commodity + bigDF$CountryBProdCosts_Loan + bigDF$CountryBProdCosts_Downpayment + bigDF$CountryBProdCosts_Fixed_O_M

(bigDF$TotalCostsA / bigDF$CountryAProdFinances_Profit)
(bigDF$TotalCostsB / bigDF$CountryBProdFinances_Profit)

(sum(bigDF$TotalCostsA) / sum(bigDF$CountryAProdFinances_Total_Revenue))
(sum(bigDF$TotalCostsB) / sum(bigDF$CountryBProdFinances_Total_Revenue))

profitsAplot = ggplot(data=bigDF, aes(x=tick, y=CountryAProdFinances_Profit)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Profits \n Country A") #give the plot a title
plot(profitsAplot)

revenuesAplot = ggplot(data=bigDF, aes(x=tick, y=CountryAProdFinances_Total_Revenue)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Revenues \n Country A") #give the plot a title
plot(revenuesAplot)

# plotTimeSeriesWithConfidenceIntervalByFacettedGroup <- function(bigDF, CountryAProdFinances_Total_Revenue, Revenue, fun.data="median_hilow", conf.int=0.5, conf.int2=0.90, nrow=NULL)

totalAcosts = ggplot(data=bigDF, aes(x=tick, y=TotalCostsA)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Total costs \n Country A") #give the plot a title
plot(totalAcosts)

profitsBplot = ggplot(data=bigDF, aes(x=tick, y=CountryBProdFinances_Profit)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Profits \n Country B") #give the plot a title
plot(profitsBplot)

revenuesBplot = ggplot(data=bigDF, aes(x=tick, y=CountryBProdFinances_Total_Revenue)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Revenues \n Country B") #give the plot a title
plot(revenuesBplot)

totalBcosts = ggplot(data=bigDF, aes(x=tick, y=TotalCostsB)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Total costs \n Country B") #give the plot a title
plot(totalBcosts)


for(j in 0:39) {
  seGenerationPVA[j] <- std.error(subset(bigDF$GenerationinMWhCountryA_Photovoltaic, tick == j))
  seGenerationWindA[j] <- std.error(subset(bigDF$GenerationinMWhCountryA_Wind, tick == j))
  seGenerationWindOffshoreA[j] <- std.error(subset(bigDF$GenerationinMWhCountryA_WindOffshore, tick == j))
  seGenerationBiomassA[j] <- std.error(subset(bigDF$GenerationinMWhCountryA_Biomass, tick == j))
  seGenerationBiogasA[j] <- std.error(subset(bigDF$GenerationinMWhCountryA_Biogas, tick == j))
  seGenerationPVB[j] <- std.error(subset(bigDF$GenerationinMWhCountryB_Photovoltaic, tick == j))
  seGenerationWindB[j] <- std.error(subset(bigDF$GenerationinMWhCountryB_Wind, tick == j))
  seGenerationWindOffshoreB[j] <- std.error(subset(bigDF$GenerationinMWhCountryB_WindOffshore, tick == j))
  seGenerationBiomassB[j] <- std.error(subset(bigDF$GenerationinMWhCountryB_Biomass, tick == j))
  seGenerationBiogasB[j] <- std.error(subset(bigDF$GenerationinMWhCountryB_Biogas, tick == j))
  seExpectedRenewableGenerationA[j] <- std.error(subset(bigDF$ExpectedRenewableGeneration_RenewableTenderNL, tick == j-1))
  seExpectedRenewableGenerationB[j] <- std.error(subset(bigDF$ExpectedRenewableGeneration_RenewableTenderDE, tick == j-1))
}

renewableGenerationA <-generationPVA + generationWindA + generationWindOffshoreA + generationBiogasA + generationBiomassA
renewableGenerationB <-generationPVB + generationWindB + generationWindOffshoreB + generationBiogasB + generationBiomassB
seRenewableGenerationA <-seGenerationPVA + seGenerationWindA + seGenerationWindOffshoreA + seGenerationBiogasA + seGenerationBiomassA
seRenewableGenerationB <-seGenerationPVB + seGenerationWindB + seGenerationWindOffshoreB + seGenerationBiogasB + seGenerationBiomassB

expectedRenewableGenerationA
expectedRenewableGenerationB
seExpectedRenewableGenerationA
seExpectedRenewableGenerationB

estimationErrorExpectedGenerationA <- (expectedRenewableGenerationA - renewableGenerationA)*100/expectedRenewableGenerationA 
estimationErrorExpectedGenerationB <- (expectedRenewableGenerationB - renewableGenerationB)*100/expectedRenewableGenerationB 
seEstimationErrorExpectedGenerationA <- (seExpectedRenewableGenerationA - seRenewableGenerationA)/seExpectedRenewableGenerationA 
seEstimationErrorExpectedGenerationB <- (seExpectedRenewableGenerationB - seRenewableGenerationB)/seExpectedRenewableGenerationB 

estimationErrorExpectedGenerationAplot = ggplot(data=tickDF, aes(x=X0, y=estimationErrorExpectedGenerationA )) + 
  geom_errorbar(aes(ymin=estimationErrorExpectedGenerationA-seEstimationErrorExpectedGenerationA, ymax=estimationErrorExpectedGenerationA+seEstimationErrorExpectedGenerationA), width=1) + 
  geom_point() +
  xlab("Year") +  
  ylab("[%]") + 
  ggtitle("Estimation Error \n Expected Generation Country A") #give the plot a title
plot(estimationErrorExpectedGenerationAplot)
ggsave(filename = paste(filePrefix, "ForecastingErrorExpectedGenA.png", sep=""))

estimationErrorExpectedGenerationBplot = ggplot(data=tickDF, aes(x=X0, y=estimationErrorExpectedGenerationB)) + 
  geom_errorbar(aes(ymin=estimationErrorExpectedGenerationB-seEstimationErrorExpectedGenerationB, ymax=estimationErrorExpectedGenerationB+seEstimationErrorExpectedGenerationB), width=1) + 
  geom_point() +
  xlab("Year") +  
  ylab("[%]") + 
  ggtitle("Estimation Error \n Expected Generation Country B") #give the plot a title
plot(estimationErrorExpectedGenerationBplot)
ggsave(filename = paste(filePrefix, "ForecastingErrorExpectedGenB.png", sep=""))


# Expected Renewable generation check
## means ##
generationPVA=0
generationWindA=0
generationWindOffshoreA=0
generationBiogasA=0
generationBiomassA=0
generationPVB=0
generationWindB=0
generationWindOffshoreB=0
generationBiogasB=0
generationBiomassB=0
expectedRenewableGenerationA=0
expectedRenewableGenerationB=0

for(j in 0:39) {
  generationPVA[j] <- mean(subset(bigDF$GenerationinMWhCountryA_Photovoltaic, tick == j))
  generationWindA[j] <- mean(subset(bigDF$GenerationinMWhCountryA_Wind, tick == j))
  generationWindOffshoreA[j] <- mean(subset(bigDF$GenerationinMWhCountryA_WindOffshore, tick == j))
  generationBiomassA[j] <- mean(subset(bigDF$GenerationinMWhCountryA_Biomass, tick == j))
  generationBiogasA[j] <- mean(subset(bigDF$GenerationinMWhCountryA_Biogas, tick == j))
  generationPVB[j] <- mean(subset(bigDF$GenerationinMWhCountryB_Photovoltaic, tick == j))
  generationWindB[j] <- mean(subset(bigDF$GenerationinMWhCountryB_Wind, tick == j))
  generationWindOffshoreB[j] <- mean(subset(bigDF$GenerationinMWhCountryB_WindOffshore, tick == j))
  generationBiomassB[j] <- mean(subset(bigDF$GenerationinMWhCountryB_Biomass, tick == j))
  generationBiogasB[j] <- mean(subset(bigDF$GenerationinMWhCountryB_Biogas, tick == j))
  expectedRenewableGenerationA[j] <- mean(subset(bigDF$ExpectedRenewableGeneration_RenewableTenderNL, tick == j-1))
  expectedRenewableGenerationB[j] <- mean(subset(bigDF$ExpectedRenewableGeneration_RenewableTenderDE, tick == j-1))
}

seGenerationPVA=0
seGenerationWindA=0
seGenerationWindOffshoreA=0
seGenerationBiogasA=0
seGenerationBiomassA=0
seGenerationPVB=0
seGenerationWindB=0
seGenerationWindOffshoreB=0
seGenerationBiogasB=0
seGenerationBiomassB=0
seExpectedRenewableGenerationA=0
seExpectedRenewableGenerationB=0

for(j in 0:39) {
  seGenerationPVA[j] <- std.error(subset(bigDF$GenerationinMWhCountryA_Photovoltaic, tick == j))
  seGenerationWindA[j] <- std.error(subset(bigDF$GenerationinMWhCountryA_Wind, tick == j))
  seGenerationWindOffshoreA[j] <- std.error(subset(bigDF$GenerationinMWhCountryA_WindOffshore, tick == j))
  seGenerationBiomassA[j] <- std.error(subset(bigDF$GenerationinMWhCountryA_Biomass, tick == j))
  seGenerationBiogasA[j] <- std.error(subset(bigDF$GenerationinMWhCountryA_Biogas, tick == j))
  seGenerationPVB[j] <- std.error(subset(bigDF$GenerationinMWhCountryB_Photovoltaic, tick == j))
  seGenerationWindB[j] <- std.error(subset(bigDF$GenerationinMWhCountryB_Wind, tick == j))
  seGenerationWindOffshoreB[j] <- std.error(subset(bigDF$GenerationinMWhCountryB_WindOffshore, tick == j))
  seGenerationBiomassB[j] <- std.error(subset(bigDF$GenerationinMWhCountryB_Biomass, tick == j))
  seGenerationBiogasB[j] <- std.error(subset(bigDF$GenerationinMWhCountryB_Biogas, tick == j))
  seExpectedRenewableGenerationA[j] <- std.error(subset(bigDF$ExpectedRenewableGeneration_RenewableTenderNL, tick == j-1))
  seExpectedRenewableGenerationB[j] <- std.error(subset(bigDF$ExpectedRenewableGeneration_RenewableTenderDE, tick == j-1))
}

renewableGenerationA <-generationPVA + generationWindA + generationWindOffshoreA + generationBiogasA + generationBiomassA
renewableGenerationB <-generationPVB + generationWindB + generationWindOffshoreB + generationBiogasB + generationBiomassB
seRenewableGenerationA <-seGenerationPVA + seGenerationWindA + seGenerationWindOffshoreA + seGenerationBiogasA + seGenerationBiomassA
seRenewableGenerationB <-seGenerationPVB + seGenerationWindB + seGenerationWindOffshoreB + seGenerationBiogasB + seGenerationBiomassB

expectedRenewableGenerationA
expectedRenewableGenerationB
seExpectedRenewableGenerationA
seExpectedRenewableGenerationB

estimationErrorExpectedGenerationA <- (expectedRenewableGenerationA - renewableGenerationA)*100/expectedRenewableGenerationA 
estimationErrorExpectedGenerationB <- (expectedRenewableGenerationB - renewableGenerationB)*100/expectedRenewableGenerationB 
seEstimationErrorExpectedGenerationA <- (seExpectedRenewableGenerationA - seRenewableGenerationA)/seExpectedRenewableGenerationA 
seEstimationErrorExpectedGenerationB <- (seExpectedRenewableGenerationB - seRenewableGenerationB)/seExpectedRenewableGenerationB 

estimationErrorExpectedGenerationAplot = ggplot(data=tickDF, aes(x=X0, y=estimationErrorExpectedGenerationA )) + 
  geom_errorbar(aes(ymin=estimationErrorExpectedGenerationA-seEstimationErrorExpectedGenerationA, ymax=estimationErrorExpectedGenerationA+seEstimationErrorExpectedGenerationA), width=1) + 
  geom_point() +
  xlab("Year") +  
  ylab("[%]") + 
  ggtitle("Estimation Error \n Expected Generation Country A") #give the plot a title
plot(estimationErrorExpectedGenerationAplot)
ggsave(filename = paste(filePrefix, "ForecastingErrorExpectedGenA.png", sep=""))

estimationErrorExpectedGenerationBplot = ggplot(data=tickDF, aes(x=X0, y=estimationErrorExpectedGenerationB)) + 
  geom_errorbar(aes(ymin=estimationErrorExpectedGenerationB-seEstimationErrorExpectedGenerationB, ymax=estimationErrorExpectedGenerationB+seEstimationErrorExpectedGenerationB), width=1) + 
  geom_point() +
  xlab("Year") +  
  ylab("[%]") + 
  ggtitle("Estimation Error \n Expected Generation Country B") #give the plot a title
plot(estimationErrorExpectedGenerationBplot)
ggsave(filename = paste(filePrefix, "ForecastingErrorExpectedGenB.png", sep=""))