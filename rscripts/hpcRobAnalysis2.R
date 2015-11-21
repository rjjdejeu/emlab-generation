setwd("~/emlab-generation/rscripts")
source("rConfig.R")
source("batchRunAnalysis.R")

#File and folder initiation
nameFile <- "AfterTest"
analysisFolder <- "~/Desktop/emlabGen/output/"
analysisFolder <- paste(analysisFolder, nameFile, "/", sep="")
analysisFolder
setwd(analysisFolder)
analysisFile <- paste(nameFile, ".csv", sep="")
analysisFile
scaleFactor <- 1
filePrefix <- nameFile

#Read csv-file
bigDF <- read.csv(analysisFile)
tickDF <- read.csv("~/Desktop/emlabGen/analysis/39ticks.csv")
# segmentDF <- read.csv("~/Desktop/emlabGen/output/nameFile/nameFile-SegmentClearingPoints.csv")
targetDF <- read.csv("~/Desktop/emlabGen/analysis/R_policyGoalNREAP_NL_DE_2050.csv")

#Initiate
technologyOrder=c("Nuclear","Lignite","CoalPSC","IGCC","CCGT","OCGT","Biomass","Biogas","Wind","WindOffshore","PV")
technologyPalette=c("CoalPSC" = "black", "Biomass" = "darkgreen", "Biogas"="darkolivegreen3", "Nuclear" = "purple", "Lignite" = "saddlebrown",
                    "OCGT" = "darkred", "CCGT" = "blue", "PV" = "yellow", "Wind" = "chartreuse4",
                    "CoalPscCCS" = "darkgray", "IGCC" = "orange", "IgccCCS"="orangered", "CcgtCCS" = "red",
                    "WindOffshore" = "navyblue", "HydroPower" = "skyblue3")
producerPalette=c("A"="black","B"="darkgreen","C"="purple","D"="darkred","E"="blue","F"="yellow","G"="orange","H"="navyblue","I"="darkgrey")

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

#Melt dataframe
# moltenDF <- melt(bigDF, id.vars = c("runId","tick"))
# moltenDF

library(gridExtra)
library(TeachingDemos)
library(grid)
library(ggplot2)
library(reshape2)

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

bigDF$TotalCostsA <- bigDF$CountryAProdCosts_Commodity + bigDF$CountryAProdCosts_Loan + bigDF$CountryAProdCosts_Downpayment + bigDF$CountryAProdCosts_Fixed_O_M
bigDF$TotalCostsB <- bigDF$CountryBProdCosts_Commodity + bigDF$CountryBProdCosts_Loan + bigDF$CountryBProdCosts_Downpayment + bigDF$CountryBProdCosts_Fixed_O_M

(bigDF$TotalCostsA / bigDF$CountryAProdFinances_Profit)
(bigDF$TotalCostsB / bigDF$CountryBProdFinances_Profit)

mean(bigDF$TotalCostsA / bigDF$CountryAProdFinances_Total_Revenue)
mean(bigDF$TotalCostsB / bigDF$CountryBProdFinances_Total_Revenue)

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


cashProdAAplot = ggplot(data=bigDF, aes(x=tick, y=ProducerCash_Energy_Producer_A)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Cash Prod A \n Country A") #give the plot a title
plot(cashProdAAplot)

cashProdBaplot = ggplot(data=bigDF, aes(x=tick, y=ProducerCash_Energy_Producer_B)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Cash Prod B \n Country A") #give the plot a title
plot(cashProdBaplot)

cashProdCaplot = ggplot(data=bigDF, aes(x=tick, y=ProducerCash_Energy_Producer_C)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Cash Prod C \n Country A") #give the plot a title
plot(cashProdCaplot)

cashProdDaplot = ggplot(data=bigDF, aes(x=tick, y=ProducerCash_Energy_Producer_D)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Cash Prod D \n Country A") #give the plot a title
plot(cashProdDaplot)

commodityCostsAplot = ggplot(data=bigDF, aes(x=tick, y=CountryAProdCosts_Commodity)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Commodity costs \n Country A") #give the plot a title
plot(commodityCostsAplot)

loanCostsAplot = ggplot(data=bigDF, aes(x=tick, y=CountryAProdCosts_Loan)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Loan Costs \n Country A") #give the plot a title
plot(loanCostsAplot)

downpaymentCostsAplot = ggplot(data=bigDF, aes(x=tick, y=CountryAProdCosts_Downpayment)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Downpayment Costs \n Country A") #give the plot a title
plot(downpaymentCostsAplot)

fixedOMAcosts = ggplot(data=bigDF, aes(x=tick, y=CountryAProdCosts_Fixed_O_M)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Fixed OM costs \n Country A") #give the plot a title
plot(fixedOMAcosts)

cashProdBAplot = ggplot(data=bigDF, aes(x=tick, y=ProducerCash_Energy_Producer_E)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Cash Prod A \n Country B") #give the plot a title
plot(cashProdBAplot)

cashProdBaplot = ggplot(data=bigDF, aes(x=tick, y=ProducerCash_Energy_Producer_F)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Cash Prod B \n Country B") #give the plot a title
plot(cashProdBaplot)

cashProdCaplot = ggplot(data=bigDF, aes(x=tick, y=ProducerCash_Energy_Producer_G)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Cash Prod C \n Country B") #give the plot a title
plot(cashProdCaplot)

cashProdDaplot = ggplot(data=bigDF, aes(x=tick, y=ProducerCash_Energy_Producer_H)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Cash Prod D \n Country B") #give the plot a title
plot(cashProdDaplot)

commodityCostsBplot = ggplot(data=bigDF, aes(x=tick, y=CountryBProdCosts_Commodity)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Commodity costs \n Country B") #give the plot a title
plot(commodityCostsBplot)

loanCostsBplot = ggplot(data=bigDF, aes(x=tick, y=CountryBProdCosts_Loan)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Loan Costs \n Country B") #give the plot a title
plot(loanCostsBplot)

downpaymentCostsBplot = ggplot(data=bigDF, aes(x=tick, y=CountryBProdCosts_Downpayment)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Downpayment Costs \n Country B") #give the plot a title
plot(downpaymentCostsBplot)

fixedOMBcosts = ggplot(data=bigDF, aes(x=tick, y=CountryBProdCosts_Fixed_O_M)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Fixed OM costs \n Country B") #give the plot a title
plot(fixedOMBcosts)

# bigDF$CountryAProdCosts_Unclassified
# bigDF$CountryAProdCosts_Electricity_ltc
# bigDF$CountryAProdCosts_CO2_auction
# bigDF$CountryAProdCosts_Capacity_Market
# bigDF$CountryAProdCosts_Strategic_Reserve
# bigDF$CountryAProdCosts_CO2_Hedging
# bigDF$CountryAProdCosts_Electricity_spot
# bigDF$CountryAProdCosts_CO2_tax
# bigDF$CountryAProdCosts_National_CO2_MinPrice


# ggsave(filename = paste(filePrefix, "realizedTargetAplot.png", sep=""),scale=1)


s1 <- meanConsumerCostsA[39]
s2 <- meanConsumerCostsA[1]
changeMeanConsumerCostsA <- s1 - s2
pcChangeMeanConsumerCostsA <- changeMeanConsumerCostsA *100/s1

s1 <- meanProducerCostsA[39]
s2 <- meanProducerCostsA[1]
changeMeanProducerCostsA <- s1 - s2
pcChangeMeanProducerCostsA <- changeMeanProducerCostsA *100/s1

s1 <- meanConsumerCostsB[39]
s2 <- meanConsumerCostsB[1]
changeMeanConsumerCostsB <- s1 - s2
pcChangeMeanConsumerCostsB <- changeMeanConsumerCostsB *100/s1

s1 <- meanProducerCostsB[39]
s2 <- meanProducerCostsB[1]
changeMeanProducerCostsB <- s1 - s2
pcChangeMeanProducerCostsB <- changeMeanProducerCostsB *100/s1
meanGovernmentCostsInBillionsA <- sum(meanGovernmentCostsA)/1000000000
meanGovernmentCostsInBillionsB <- sum(meanGovernmentCostsB)/1000000000

DataTable <- c(pcChangeMeanConsumerCostsA)
DataTable <- rbind(DataTable, c(pcChangeMeanProducerCostsA))
DataTable <- rbind(DataTable, c(pcChangeMeanConsumerCostsB))
DataTable <- rbind(DataTable, c(pcChangeMeanProducerCostsB))
DataTable <- rbind(DataTable, c(meanGovernmentCostsInBillionsA))
DataTable <- rbind(DataTable, c(meanGovernmentCostsInBillionsB))
colnames(DataTable) <- c("Value")
rownames(DataTable) <- c("Consumer welfare A d%","Producer costs A d%", "Consumer welfare B d%","Producer costs B d%", "Subsidy in Billions Cum A","Subsidy in Billions Cum B")
write.csv(DataTable, "DataTableWelfareCostsSubsidies.csv")


# Generation shares per technology
meanSharePVA=0
meanShareWindA=0
meanShareWindOffshoreA=0
meanShareBiogasA=0
meanShareBiomassA=0
meanShareCoalA=0
meanShareIGCCA=0
meanShareLigniteA=0
meanShareOCGTA=0
meanShareCCGTA=0
meanShareNuclearA=0
meanSharePVB=0
meanShareWindB=0
meanShareWindOffshoreB=0
meanShareBiogasB=0
meanShareBiomassB=0
meanShareCoalB=0
meanShareIGCCB=0
meanShareLigniteB=0
meanShareOCGTB=0
meanShareCCGTB=0
meanShareNuclearB=0

for(j in 0:39) {
  meanSharePVA[j] <- mean(subset(bigDF$GenerationinMWhCountryA_Photovoltaic / bigDF$NationalTotalProductioninMWh_Country_A, tick == j))
  meanShareWindA[j] <- mean(subset(bigDF$GenerationinMWhCountryA_Wind / bigDF$NationalTotalProductioninMWh_Country_A, tick == j))
  meanShareWindOffshoreA[j] <- mean(subset(bigDF$GenerationinMWhCountryA_WindOffshore / bigDF$NationalTotalProductioninMWh_Country_A, tick == j))
  meanShareBiomassA[j] <- mean(subset(bigDF$GenerationinMWhCountryA_Biomass / bigDF$NationalTotalProductioninMWh_Country_A, tick == j))
  meanShareBiogasA[j] <- mean(subset(bigDF$GenerationinMWhCountryA_Biogas / bigDF$NationalTotalProductioninMWh_Country_A, tick == j))
  meanShareCoalA[j] <- mean(subset(bigDF$GenerationinMWhCountryA_CoalPSC / bigDF$NationalTotalProductioninMWh_Country_A, tick == j))
  meanShareIGCCA[j] <- mean(subset(bigDF$GenerationinMWhCountryA_IGCC / bigDF$NationalTotalProductioninMWh_Country_A, tick == j))
  meanShareLigniteA[j] <- mean(subset(bigDF$GenerationinMWhCountryA_Lignite / bigDF$NationalTotalProductioninMWh_Country_A, tick == j))
  meanShareOCGTA[j] <- mean(subset(bigDF$GenerationinMWhCountryA_OCGT / bigDF$NationalTotalProductioninMWh_Country_A, tick == j))
  meanShareCCGTA[j] <- mean(subset(bigDF$GenerationinMWhCountryA_CCGT / bigDF$NationalTotalProductioninMWh_Country_A, tick == j))
  meanShareNuclearA[j] <- mean(subset(bigDF$GenerationinMWhCountryA_Nuclear / bigDF$NationalTotalProductioninMWh_Country_A, tick == j))
  meanSharePVB[j] <- mean(subset(bigDF$GenerationinMWhCountryB_Photovoltaic / bigDF$NationalTotalProductioninMWh_Country_B, tick == j))
  meanShareWindB[j] <- mean(subset(bigDF$GenerationinMWhCountryB_Wind / bigDF$NationalTotalProductioninMWh_Country_B, tick == j))
  meanShareWindOffshoreB[j] <- mean(subset(bigDF$GenerationinMWhCountryB_WindOffshore / bigDF$NationalTotalProductioninMWh_Country_B, tick == j))
  meanShareBiomassB[j] <- mean(subset(bigDF$GenerationinMWhCountryB_Biomass / bigDF$NationalTotalProductioninMWh_Country_B, tick == j))
  meanShareBiogasB[j] <- mean(subset(bigDF$GenerationinMWhCountryB_Biogas / bigDF$NationalTotalProductioninMWh_Country_B, tick == j))
  meanShareIGCCB[j] <- mean(subset(bigDF$GenerationinMWhCountryB_IGCC / bigDF$NationalTotalProductioninMWh_Country_B, tick == j))
  meanShareCoalB[j] <- mean(subset(bigDF$GenerationinMWhCountryB_CoalPSC / bigDF$NationalTotalProductioninMWh_Country_B, tick == j))
  meanShareLigniteB[j] <- mean(subset(bigDF$GenerationinMWhCountryB_Lignite / bigDF$NationalTotalProductioninMWh_Country_B, tick == j))
  meanShareOCGTB[j] <- mean(subset(bigDF$GenerationinMWhCountryB_OCGT / bigDF$NationalTotalProductioninMWh_Country_B, tick == j))
  meanShareCCGTB[j] <- mean(subset(bigDF$GenerationinMWhCountryB_CCGT / bigDF$NationalTotalProductioninMWh_Country_B, tick == j))
  meanShareNuclearB[j] <- mean(subset(bigDF$GenerationinMWhCountryB_Nuclear / bigDF$NationalTotalProductioninMWh_Country_B, tick == j))
}

meanSharePVA
meanShareWindA
meanShareWindOffshoreA
meanShareBiogasA
meanShareBiomassA
meanShareCoalA
meanShareIGCCA
meanShareLigniteA
meanShareOCGTA
meanShareCCGTA
meanShareNuclearA
meanSharePVB
meanShareWindB
meanShareWindOffshoreB
meanShareBiogasB
meanShareBiomassB
meanShareCoalB
meanShareIGCCB
meanShareLigniteB
meanShareOCGTB
meanShareCCGTB
meanShareNuclearB

DataTable <- c(meanShareNuclearA)
DataTable <- rbind(DataTable, c(meanShareLigniteA))
DataTable <- rbind(DataTable, c(meanShareCoalA))
DataTable <- rbind(DataTable, c(meanShareIGCCA))
DataTable <- rbind(DataTable, c(meanShareCCGTA))
DataTable <- rbind(DataTable, c(meanShareOCGTA))
DataTable <- rbind(DataTable, c(meanShareBiomassA))
DataTable <- rbind(DataTable, c(meanShareBiogasA))
DataTable <- rbind(DataTable, c(meanShareWindA))
DataTable <- rbind(DataTable, c(meanShareWindOffshoreA))
DataTable <- rbind(DataTable, c(meanSharePVA))
colnames(DataTable) <- c("share","fraction")
rownames(DataTable) <- c("Nuclear","Lignite","CoalPSC","IGCC", "CCGT","OCGT","Biomass","Biogas","Wind","WindOffshore","PV")
write.csv(DataTable, "DataTableGenerationShareA.csv")
GenerationShareDataA <- melt(DataTable)

ggplot(GenerationShareDataA, aes(X2, value, fill = X1)) + 
  guides(fill=guide_legend(title=NULL)) +
  scale_fill_manual(values=technologyPalette) +
  geom_area(stat = "identity") + 
  xlab("Year") + 
  ylab("Fraction") +
  ggtitle("Relative generation shares Country A")
ggsave(filename = paste(filePrefix, "relativeGenerationShareAplot.png", sep=""))

DataTable <- c(meanShareNuclearB)
DataTable <- rbind(DataTable, c(meanShareLigniteB))
DataTable <- rbind(DataTable, c(meanShareCoalB))
DataTable <- rbind(DataTable, c(meanShareIGCCB))
DataTable <- rbind(DataTable, c(meanShareCCGTB))
DataTable <- rbind(DataTable, c(meanShareOCGTB))
DataTable <- rbind(DataTable, c(meanShareBiomassB))
DataTable <- rbind(DataTable, c(meanShareBiogasB))
DataTable <- rbind(DataTable, c(meanShareWindB))
DataTable <- rbind(DataTable, c(meanShareWindOffshoreB))
DataTable <- rbind(DataTable, c(meanSharePVB))
colnames(DataTable) <- c("share","fraction")
rownames(DataTable) <- c("Nuclear","Lignite","CoalPSC","IGCC", "CCGT","OCGT","Biomass","Biogas","Wind","WindOffshore","PV")
write.csv(DataTable, "DataTableGenerationShareB.csv")
GenerationShareDataB <- melt(DataTable)

ggplot(GenerationShareDataB, aes(X2, value, fill = X1)) + 
  guides(fill=guide_legend(title=NULL)) +
  scale_fill_manual(values=technologyPalette) +
  geom_area(stat = "identity") + 
  xlab("Year") + 
  ylab("Fraction") +
  ggtitle("Relative generation shares Country B")
ggsave(filename = paste(filePrefix, "relativeGenerationShareBplot.png", sep=""))


# Capacity shares per technology
meanCapSharePVA=0
meanCapShareWindA=0
meanCapShareWindOffshoreA=0
meanCapShareBiogasA=0
meanCapShareBiomassA=0
meanCapShareCoalA=0
meanCapShareIGCCA=0
meanCapShareLigniteA=0
meanCapShareOCGTA=0
meanCapShareCCGTA=0
meanCapShareNuclearA=0
meanCapSharePVB=0
meanCapShareWindB=0
meanCapShareWindOffshoreB=0
meanCapShareBiogasB=0
meanCapShareBiomassB=0
meanCapShareCoalB=0
meanCapShareIGCCB=0
meanCapShareLigniteB=0
meanCapShareOCGTB=0
meanCapShareCCGTB=0
meanCapShareNuclearB=0

for(j in 0:39) {
  meanCapSharePVA[j] <- mean(subset(bigDF$CapacityinMWinA_Photovoltaic / bigDF$TotalOperationalCapacityPerZoneInMW_Country_A, tick == j))
  meanCapShareWindA[j] <- mean(subset(bigDF$CapacityinMWinA_Wind / bigDF$TotalOperationalCapacityPerZoneInMW_Country_A, tick == j))
  meanCapShareWindOffshoreA[j] <- mean(subset(bigDF$CapacityinMWinA_WindOffshore / bigDF$TotalOperationalCapacityPerZoneInMW_Country_A, tick == j))
  meanCapShareBiomassA[j] <- mean(subset(bigDF$CapacityinMWinA_Biomass / bigDF$TotalOperationalCapacityPerZoneInMW_Country_A, tick == j))
  meanCapShareBiogasA[j] <- mean(subset(bigDF$CapacityinMWinA_Biogas / bigDF$TotalOperationalCapacityPerZoneInMW_Country_A, tick == j))
  meanCapShareCoalA[j] <- mean(subset(bigDF$CapacityinMWinA_CoalPSC / bigDF$TotalOperationalCapacityPerZoneInMW_Country_A, tick == j))
  meanCapShareIGCCA[j] <- mean(subset(bigDF$CapacityinMWinA_IGCC / bigDF$TotalOperationalCapacityPerZoneInMW_Country_A, tick == j))
  meanCapShareLigniteA[j] <- mean(subset(bigDF$CapacityinMWinA_Lignite / bigDF$TotalOperationalCapacityPerZoneInMW_Country_A, tick == j))
  meanCapShareOCGTA[j] <- mean(subset(bigDF$CapacityinMWinA_OCGT / bigDF$TotalOperationalCapacityPerZoneInMW_Country_A, tick == j))
  meanCapShareCCGTA[j] <- mean(subset(bigDF$CapacityinMWinA_CCGT / bigDF$TotalOperationalCapacityPerZoneInMW_Country_A, tick == j))
  meanCapShareNuclearA[j] <- mean(subset(bigDF$CapacityinMWinA_Nuclear / bigDF$TotalOperationalCapacityPerZoneInMW_Country_A, tick == j))
  meanCapSharePVB[j] <- mean(subset(bigDF$CapacityinMWinB_Photovoltaic / bigDF$TotalOperationalCapacityPerZoneInMW_Country_B, tick == j))
  meanCapShareWindB[j] <- mean(subset(bigDF$CapacityinMWinB_Wind / bigDF$TotalOperationalCapacityPerZoneInMW_Country_B, tick == j))
  meanCapShareWindOffshoreB[j] <- mean(subset(bigDF$CapacityinMWinB_WindOffshore / bigDF$TotalOperationalCapacityPerZoneInMW_Country_B, tick == j))
  meanCapShareBiomassB[j] <- mean(subset(bigDF$CapacityinMWinB_Biomass / bigDF$TotalOperationalCapacityPerZoneInMW_Country_B, tick == j))
  meanCapShareBiogasB[j] <- mean(subset(bigDF$CapacityinMWinB_Biogas / bigDF$TotalOperationalCapacityPerZoneInMW_Country_B, tick == j))
  meanCapShareIGCCB[j] <- mean(subset(bigDF$CapacityinMWinB_IGCC / bigDF$TotalOperationalCapacityPerZoneInMW_Country_B, tick == j))
  meanCapShareCoalB[j] <- mean(subset(bigDF$CapacityinMWinB_CoalPSC / bigDF$TotalOperationalCapacityPerZoneInMW_Country_B, tick == j))
  meanCapShareLigniteB[j] <- mean(subset(bigDF$CapacityinMWinB_Lignite / bigDF$TotalOperationalCapacityPerZoneInMW_Country_B, tick == j))
  meanCapShareOCGTB[j] <- mean(subset(bigDF$CapacityinMWinB_OCGT / bigDF$TotalOperationalCapacityPerZoneInMW_Country_B, tick == j))
  meanCapShareCCGTB[j] <- mean(subset(bigDF$CapacityinMWinB_CCGT / bigDF$TotalOperationalCapacityPerZoneInMW_Country_B, tick == j))
  meanCapShareNuclearB[j] <- mean(subset(bigDF$CapacityinMWinB_Nuclear / bigDF$TotalOperationalCapacityPerZoneInMW_Country_B, tick == j))
}

meanCapSharePVA
meanCapShareWindA
meanCapShareWindOffshoreA
meanCapShareBiogasA
meanCapShareBiomassA
meanCapShareCoalA
meanCapShareIGCCA
meanCapShareLigniteA
meanCapShareOCGTA
meanCapShareCCGTA
meanCapShareNuclearA
meanCapSharePVB
meanCapShareWindB
meanCapShareWindOffshoreB
meanCapShareBiogasB
meanCapShareBiomassB
meanCapShareCoalB
meanCapShareIGCCB
meanCapShareLigniteB
meanCapShareOCGTB
meanCapShareCCGTB
meanCapShareNuclearB

DataTable <- c(meanCapShareNuclearA)
DataTable <- rbind(DataTable, c(meanCapShareLigniteA))
DataTable <- rbind(DataTable, c(meanCapShareCoalA))
DataTable <- rbind(DataTable, c(meanCapShareIGCCA))
DataTable <- rbind(DataTable, c(meanCapShareCCGTA))
DataTable <- rbind(DataTable, c(meanCapShareOCGTA))
DataTable <- rbind(DataTable, c(meanCapShareBiomassA))
DataTable <- rbind(DataTable, c(meanCapShareBiogasA))
DataTable <- rbind(DataTable, c(meanCapShareWindA))
DataTable <- rbind(DataTable, c(meanCapShareWindOffshoreA))
DataTable <- rbind(DataTable, c(meanCapSharePVA))
colnames(DataTable) <- c("CapShare","fraction")
rownames(DataTable) <- c("Nuclear","Lignite","CoalPSC","IGCC", "CCGT","OCGT","Biomass","Biogas","Wind","WindOffshore","PV")
write.csv(DataTable, "DataTableGenerationCapShareA.csv")
CapShareDataA <- melt(DataTable)

ggplot(CapShareDataA, aes(X2, value, fill = X1)) + 
  guides(fill=guide_legend(title=NULL)) +
  scale_fill_manual(values=technologyPalette) +
  geom_area(stat = "identity") + 
  xlab("Year") + 
  ylab("Fraction") +
  ggtitle("Relative capacity shares Country A")
ggsave(filename = paste(filePrefix, "relativeGenerationCapShareAplot.png", sep=""))

DataTable <- c(meanCapShareNuclearB)
DataTable <- rbind(DataTable, c(meanCapShareLigniteB))
DataTable <- rbind(DataTable, c(meanCapShareCoalB))
DataTable <- rbind(DataTable, c(meanCapShareIGCCB))
DataTable <- rbind(DataTable, c(meanCapShareCCGTB))
DataTable <- rbind(DataTable, c(meanCapShareOCGTB))
DataTable <- rbind(DataTable, c(meanCapShareBiomassB))
DataTable <- rbind(DataTable, c(meanCapShareBiogasB))
DataTable <- rbind(DataTable, c(meanCapShareWindB))
DataTable <- rbind(DataTable, c(meanCapShareWindOffshoreB))
DataTable <- rbind(DataTable, c(meanCapSharePVB))
colnames(DataTable) <- c("CapShare","fraction")
rownames(DataTable) <- c("Nuclear","Lignite","CoalPSC","IGCC", "CCGT","OCGT","Biomass","Biogas","Wind","WindOffshore","PV")
write.csv(DataTable, "DataTableGenerationCapShareB.csv")
CapShareDataB <- melt(DataTable)

ggplot(CapShareDataB, aes(X2, value, fill = X1)) + 
  guides(fill=guide_legend(title=NULL)) +
  scale_fill_manual(values=technologyPalette) +
  geom_area(stat = "identity") + 
  xlab("Year") + 
  ylab("Fraction") +
  ggtitle("Relative capacity shares Country B")
ggsave(filename = paste(filePrefix, "relativeGenerationCapShareBplot.png", sep=""))


# Renewable generation in terms of demand (compare this with NREAP targets)
meanRealizedTargetA=0
meanRealizedTargetB=0
for(j in 0:39) {
  meanRealizedTargetA[j] <- mean(subset( 
    ((bigDF$GenerationinMWhCountryA_Photovoltaic + bigDF$GenerationinMWhCountryA_Wind + bigDF$GenerationinMWhCountryA_Biomass + 
        bigDF$GenerationinMWhCountryA_HydroPower + bigDF$GenerationinMWhCountryA_Biogas + bigDF$GenerationinMWhCountryA_WindOffshore) / bigDF$Total_DemandinMWh_Country_A) 
    , tick == j))
  meanRealizedTargetB[j] <- mean(subset( 
    ((bigDF$GenerationinMWhCountryB_Photovoltaic + bigDF$GenerationinMWhCountryB_Wind + bigDF$GenerationinMWhCountryB_Biomass + 
        bigDF$GenerationinMWhCountryB_HydroPower + bigDF$GenerationinMWhCountryB_Biogas + bigDF$GenerationinMWhCountryB_WindOffshore) / bigDF$Total_DemandinMWh_Country_B) 
    , tick == j))
}

realizedTargetAplot = ggplot(data=tickDF, aes(x=X0, y=meanRealizedTargetA*100)) + 
  geom_point() +
  xlab("Year") +  
  ylab("[%]") + 
  ggtitle("RES-E generation in terms of demand \n Country A") #give the plot a title
plot(realizedTargetAplot)
ggsave(filename = paste(filePrefix, "realizedTargetAplot.png", sep=""),scale=1)

nreapAplot = ggplot(data=targetDF, aes(x=tick, y=nl_target*100)) + 
  geom_point() + 
  xlab("Year") +  
  ylab("[%]") + 
  ggtitle("NREAP Target \n Country A") #give the plot a title
plot(nreapAplot)
ggsave(filename = paste(filePrefix, "NREAP_target_nl.png", sep=""),scale=1)

realizedTargetBplot = ggplot(data=tickDF, aes(x=X0, y=meanRealizedTargetB*100)) + 
  geom_point() +
  xlab("Year") +  
  ylab("[%]") + 
  ggtitle("RES-E generation in terms of demand \n Country B") #give the plot a title
plot(realizedTargetBplot)
ggsave(filename = paste(filePrefix, "realizedTargetBplot.png", sep=""),scale=1)

nreapBplot = ggplot(data=targetDF, aes(x=tick, y=de_target*100)) + 
  geom_point() + 
  xlab("Year") +  
  ylab("[%]") + 
  ggtitle("NREAP Target \n Country B") #give the plot a title
plot(nreapBplot)
ggsave(filename = paste(filePrefix, "NREAP_target_de.png", sep=""),scale=1)


# Average electricity wholesale price in country
meanElectricityPriceA=0
meanElectricityPriceB=0

for(j in 0:39) {
  meanElectricityPriceA[j] <- mean(subset(bigDF$Avg_El_PricesinEURpMWh_Country_A, tick == j))
  meanElectricityPriceB[j] <- mean(subset(bigDF$Avg_El_PricesinEURpMWh_Country_B, tick == j))
}
meanElectricityPriceA
meanElectricityPriceB

meanElectricityPriceAplot = ggplot(data=tickDF, aes(x=X0, y=meanElectricityPriceA)) + 
  geom_line() +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Mean electricity price \n Country A") #give the plot a title
plot(meanElectricityPriceAplot)
ggsave(filename = paste(filePrefix, "meanElectricityPriceA.png", sep=""),scale=1)

meanElectricityPriceBplot = ggplot(data=tickDF, aes(x=X0, y=meanElectricityPriceB)) + 
  geom_line() +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Mean electricity price \n Country B") #give the plot a title
plot(meanElectricityPriceBplot)
ggsave(filename = paste(filePrefix, "meanElectricityPriceB.png", sep=""),scale=1)

diff_el_price_AB = ggplot(data=tickDF , aes(x=X0, y=(meanElectricityPriceA-meanElectricityPriceB))) + 
  geom_line() +  #(aes(colour = runNumber))
  xlab("Year") +  
  ylab("Eur/MWh") + 
  ggtitle("Difference Electricity Price \n Country A - Country B") #give the plot a title
plot(diff_el_price_AB)
ggsave(filename = paste(filePrefix, "Diff_ElectricityPriceAverageAB.png", sep=""))

#Tender Clearing Prices, subsidies
meanTenderClearingPriceA=0
meanTenderClearingPriceB=0

for(j in 0:39) {
  meanTenderClearingPriceA[j] <- mean(subset(bigDF$tenderClearingPrice_Country_A, tick == j))
  meanTenderClearingPriceB[j] <- mean(subset(bigDF$tenderClearingPrice_Country_B, tick == j))
}
meanTenderClearingPriceA
meanTenderClearingPriceB

tenderClearingPriceCountryAplot = ggplot(data=tickDF, aes(x=X0, y=meanTenderClearingPriceA)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Mean tender clearing price \n Country A") #give the plot a title
plot(tenderClearingPriceCountryAplot)
ggsave(filename = paste(filePrefix, "meanTenderClearingPriceA.png", sep=""),scale=1)

tenderClearingPriceCountryBplot = ggplot(data=tickDF, aes(x=X0, y=meanTenderClearingPriceB)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Mean tender clearing price \n Country B") #give the plot a title
plot(tenderClearingPriceCountryBplot)
ggsave(filename = paste(filePrefix, "meanTenderClearingPriceB.png", sep=""),scale=1)


# Tender subsidies
meanTotalTenderSubsidyA=0
meanTotalTenderSubsidyB=0

for(j in 0:39) {
  meanTotalTenderSubsidyA[j] <- mean(subset(bigDF$yearlyTotalTenderSubsidyCountryA_Tender_Subsidy_Yearly_Country_A, tick == j))
  meanTotalTenderSubsidyB[j] <- mean(subset(bigDF$yearlyTotalTenderSubsidyCountryB_Tender_Subsidy_Yearly_Country_B, tick == j))
}
meanTotalTenderSubsidyA
meanTotalTenderSubsidyB

TotalTenderSubsidyCountryAplot = ggplot(data=tickDF, aes(x=X0, y=meanTotalTenderSubsidyA)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Mean total tender subsidies \n Country A") #give the plot a title
plot(TotalTenderSubsidyCountryAplot)
ggsave(filename = paste(filePrefix, "meanTotalTenderSubsidyA.png", sep=""),scale=1)

TotalTenderSubsidyCountryBplot = ggplot(data=tickDF, aes(x=X0, y=meanTotalTenderSubsidyB)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Mean total tender subsidies \n Country B") #give the plot a title
plot(TotalTenderSubsidyCountryBplot)
ggsave(filename = paste(filePrefix, "meanTotalTenderSubsidyB.png", sep=""),scale=1)


#supply ratio
meanSupplyRatioA=0
meanSupplyRatioB=0

for(j in 0:39) {
  meanSupplyRatioA[j] <- mean(subset(bigDF$TotalOperationalCapacityPerZoneInMW_Country_A/bigDF$PeakDemandPerZoneInMW_Country_A, tick == j))
  meanSupplyRatioB[j] <- mean(subset(bigDF$TotalOperationalCapacityPerZoneInMW_Country_B/bigDF$PeakDemandPerZoneInMW_Country_B, tick == j))
}
meanSupplyRatioA
meanSupplyRatioB

meanSupplyRatioAplot = ggplot(data=tickDF, aes(x=X0, y=meanSupplyRatioA)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Mean supply ratio \n Country A") #give the plot a title
plot(meanSupplyRatioAplot)
ggsave(filename = paste(filePrefix, "meanSupplyRatioA.png", sep=""),scale=1)

meanSupplyRatioBplot = ggplot(data=tickDF, aes(x=X0, y=meanSupplyRatioB)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Mean supply ratio \n Country B") #give the plot a title
plot(meanSupplyRatioBplot)
ggsave(filename = paste(filePrefix, "meanSupplyRatioB.png", sep=""),scale=1)


#Demand
demandCountryAplot = ggplot(data=bigDF, aes(x=tick, y=Total_DemandinMWh_Country_A, group=runId)) + 
  geom_smooth() + #(aes(colour = runNumber))
  xlab("Year") +  
  ylab("Demand (MWh)") + 
  ggtitle("Demand \n Country A") #give the plot a title
plot(demandCountryAplot)
ggsave(filename = paste(filePrefix, "demand_A.png", sep=""))

demandCountryBplot = ggplot(data=bigDF, aes(x=tick, y=Total_DemandinMWh_Country_B, group=runId)) + 
  geom_smooth() + #(aes(colour = runNumber))
  xlab("Year") +  
  ylab("Demand (MWh)") + 
  ggtitle("Demand \n Country B") #give the plot a title
plot(demandCountryBplot)
ggsave(filename = paste(filePrefix, "demand_B.png", sep=""))


#Segment prices
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


#Market Value of RES-E (PV, Wind, WindOfshore)
# market value = Volume PV per segment * Sgement Clearing Price / generation in MWh in tick
# system base price = Average electricity price in tick
# value factor = market value / system base price
