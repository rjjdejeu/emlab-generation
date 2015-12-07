setwd("~/emlab-generation/rscripts")
source("rConfig.R")
source("batchRunAnalysis.R")

library(gridExtra)
library(TeachingDemos)
library(grid)
library(ggplot2)
library(reshape2)
library(plotrix)
library(dplyr)

#Axis sizes for plotting
titleSize=20
legendSize=20
xTitle=20
xAxis=20
yTitle=20
yAxis=20

#Read initial csv files
tickDF <- read.csv("/home/rob/emlabGen/analysis/39ticks.csv")
# segmentDF <- read.csv("/home/rob/emlabGen/output/nameFile/nameFile-SegmentClearingPoints.csv")
targetDF <- read.csv("/home/rob/emlabGen/analysis/R2_policyGoalNREAP_NL_DE_2050.csv")

#File and folder initiation
nameFile <- "BaseCaseInfCap"
analysisFolder <- "/home/rob/emlabGen/output/"
analysisFolder <- paste(analysisFolder, nameFile, "/", sep="")
analysisFolder
setwd(analysisFolder)
analysisFile <- paste(nameFile, ".csv", sep="")
analysisFile
scaleFactor <- 1
filePrefix <- nameFile

#Read scenario output csv-file
bigDF <- read.csv(analysisFile)
# tcp1DF <- read.csv("/home/rob/emlabGen/output/TechSpecFullInOneCountry/TechSpecFullInOneCountry-TenderClearingPoints.csv")
# tcp2DF <- read.csv("/home/rob/emlabGen/output/TechSpecFullInBothCountries/TechSpecFullInBothCountries-TenderClearingPoints.csv")

tick <- bigDF$tick

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

##Costs, Cash, Profits and Welfare
meanConsumerCostsA=0
meanProducerCostsA=0
meanGovernmentCostsA=0
meanProducerRevenuesA=0
meanConsumerCostsB=0
meanProducerCostsB=0
meanGovernmentCostsB=0
meanProducerRevenuesB=0
meanWelfareLossENSA=0
meanWelfareLossENSB=0
meanAggProfit=0
meanAggProfitA=0
meanAggProfitB=0
meanProducerCashA=0
meanProducerCashB=0

seConsumerCostsA=0
seProducerCostsA=0
seGovernmentCostsA=0
seWelfareLossENSA=0
seAggProfitA=0

seConsumerCostsB=0
seProducerCostsB=0
seGovernmentCostsB=0
seWelfareLossENSB=0
seAggProfitB=0

for(j in 0:39) {
  meanAggProfit[j] <- mean(subset(bigDF$CountryAProdFinances_Profit + bigDF$CountryBProdFinances_Profit, tick == j))
  meanAggProfitA[j] <- mean(subset(bigDF$CountryAProdFinances_Profit, tick == j))
  meanAggProfitB[j] <- mean(subset(bigDF$CountryBProdFinances_Profit, tick == j))
  meanConsumerCostsA[j] <- mean(subset(bigDF$ConsumerExpenditure_Country_A_electricity_spot_market, tick == j))
  meanProducerCostsA[j] <- mean(subset(bigDF$CountryAProdCosts_Fixed_O_M + 
                                         bigDF$CountryAProdCosts_Loan + 
                                         bigDF$CountryAProdCosts_Commodity + 
                                         bigDF$CountryAProdCosts_Downpayment, tick == j))
  meanProducerRevenuesA[j] <-mean(subset(bigDF$CountryAProdFinances_Revenue_Spot, tick == j))
  meanGovernmentCostsA[j] <- mean(subset(bigDF$CountryAProdFinances_Tender_Subsidy, tick == j))
  meanWelfareLossENSA[j] <- mean(subset(bigDF$WelfareLossThroughENS_Country_A, tick == j))
  meanProducerCashA[j] <- mean(subset(bigDF$ProducerCash_Energy_Producer_A +
                                        bigDF$ProducerCash_Energy_Producer_B +
                                        bigDF$ProducerCash_Energy_Producer_C +
                                        bigDF$ProducerCash_Energy_Producer_D, tick == j))
  
  meanConsumerCostsB[j] <- mean(subset(bigDF$ConsumerExpenditure_Country_B_electricity_spot_market, tick == j))
  meanProducerCostsB[j] <- mean(subset(bigDF$CountryBProdCosts_Fixed_O_M + 
                                         bigDF$CountryBProdCosts_Loan + 
                                         bigDF$CountryBProdCosts_Commodity + 
                                         bigDF$CountryBProdCosts_Downpayment, tick == j))
  meanProducerRevenuesB[j] <-mean(subset(bigDF$CountryBProdFinances_Revenue_Spot, tick == j))
  meanGovernmentCostsB[j] <- mean(subset(bigDF$CountryBProdFinances_Tender_Subsidy, tick == j))
  meanProducerCashB[j] <- mean(subset(bigDF$ProducerCash_Energy_Producer_E +
                                        bigDF$ProducerCash_Energy_Producer_F +
                                        bigDF$ProducerCash_Energy_Producer_G +
                                        bigDF$ProducerCash_Energy_Producer_H +
                                        bigDF$ProducerCash_Energy_Producer_I, tick == j))
  meanWelfareLossENSB[j] <- mean(subset(bigDF$WelfareLossThroughENS_Country_B, tick == j))
  
  seAggProfitA[j] <- std.error(subset(bigDF$CountryAProdFinances_Profit, tick == j))
  seAggProfitB[j] <- std.error(subset(bigDF$CountryBProdFinances_Profit, tick == j))
  seConsumerCostsA[j] <- std.error(subset(bigDF$ConsumerExpenditure_Country_A_electricity_spot_market, tick == j))
  seProducerCostsA[j] <- std.error(subset(bigDF$CountryAProdCosts_Fixed_O_M + 
                                            bigDF$CountryAProdCosts_Loan + 
                                            bigDF$CountryAProdCosts_Commodity + 
                                            bigDF$CountryAProdCosts_Downpayment, tick == j))
  seGovernmentCostsA[j] <- std.error(subset(bigDF$CountryAProdFinances_Tender_Subsidy, tick == j))
  seWelfareLossENSA[j] <- std.error(subset(bigDF$WelfareLossThroughENS_Country_A, tick == j))
  
  seConsumerCostsB[j] <- std.error(subset(bigDF$ConsumerExpenditure_Country_B_electricity_spot_market, tick == j))
  seProducerCostsB[j] <- std.error(subset(bigDF$CountryBProdCosts_Fixed_O_M + 
                                            bigDF$CountryBProdCosts_Loan + 
                                            bigDF$CountryBProdCosts_Commodity + 
                                            bigDF$CountryBProdCosts_Downpayment, tick == j))
  seGovernmentCostsB[j] <- std.error(subset(bigDF$CountryBProdFinances_Tender_Subsidy, tick == j))
  seWelfareLossENSB[j] <- std.error(subset(bigDF$WelfareLossThroughENS_Country_B, tick == j))
}

profitsAplot = ggplot(data=tickDF, aes(x=X0, y=meanAggProfitA)) + 
  geom_errorbar(aes(ymin=meanAggProfitA-seAggProfitA, ymax=meanAggProfitA+seAggProfitA, width=1)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Profits \n Country A") +   
  theme(plot.title = element_text(size = titleSize),plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
plot(profitsAplot)
ggsave(filename = paste(filePrefix, "profitsAplot.pdf", sep=""),scale=1)

profitsBplot = ggplot(data=tickDF, aes(x=X0, y=meanAggProfitB)) + 
  geom_errorbar(aes(ymin=meanAggProfitB-seAggProfitB, ymax=meanAggProfitB+seAggProfitB, width=1)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Profits \n Country B") +   
  theme(plot.title = element_text(size = titleSize),plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis),
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
plot(profitsBplot)
ggsave(filename = paste(filePrefix, "profitsBplot.pdf", sep=""),scale=1)

consCostsAplot = ggplot(data=tickDF, aes(x=X0, y=meanConsumerCostsA)) + 
  # geom_errorbar(aes(ymin=meanAggProfitA-seConsumerCostsA, ymax=meanAggProfitA+seConsumerCostsA, width=1)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Consumer expenditures \n Country A") +  
  theme(plot.title = element_text(size = titleSize),plot.title = element_text(size = titleSize),axis.title.x = element_text(size=xTitle), axis.text.x=element_text(size = xAxis),  
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
#plot(consCostsAplot)
ggsave(filename = paste(filePrefix, "consumerCostsAplot", sep=""),scale=1)

consCostsBplot = ggplot(data=tickDF, aes(x=X0, y=meanConsumerCostsB)) + 
  # geom_errorbar(aes(ymin=meanAggProfitA-seConsumerCostsA, ymax=meanAggProfitA+seConsumerCostsA, width=1)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Consumer expenditures \n Country B") +  
  theme(plot.title = element_text(size = titleSize),plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis),
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
#plot(consCostsBplot)
ggsave(filename = paste(filePrefix, "consumerCostsBplot", sep=""),scale=1)



revenuesAplot = ggplot(data=bigDF, aes(x=tick, y=CountryAProdFinances_Total_Revenue)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Revenues \n Country A") +   
  theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
#plot(revenuesAplot)
ggsave(filename = paste(filePrefix, "revenuesAplot.pdf", sep=""),scale=1)

totalAcosts = ggplot(data=bigDF, aes(x=tick, y=TotalCostsA)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Total costs \n Country A") +    
  theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis),
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
#plot(totalAcosts)
ggsave(filename = paste(filePrefix, "totalCostsAplot.pdf", sep=""),scale=1)

revenuesBplot = ggplot(data=bigDF, aes(x=tick, y=CountryBProdFinances_Total_Revenue)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Revenues \n Country B") +  
  theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
#plot(revenuesBplot)
ggsave(filename = paste(filePrefix, "revenuesBplot.pdf", sep=""),scale=1)

totalBcosts = ggplot(data=bigDF, aes(x=tick, y=TotalCostsB)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Total costs \n Country B") +  
  theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis),   
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
#plot(totalBcosts)
ggsave(filename = paste(filePrefix, "totalCostsBplot.pdf", sep=""),scale=1)

## Costs breakdown
meanFixedOMCostsA=0
meanLoanCostsA=0
meanCommodityCostsA=0
meanDownpaymentsCostsA=0
meanFixedOMCostsB=0
meanLoanCostsB=0
meanCommodityCostsB=0
meanDownpaymentsCostsB=0
seFixedOMCostsA=0
seLoanCostsA=0
seCommodityCostsA=0
seDownpaymentsCostsA=0
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
  ggtitle("Fixed O&M Costs \n Country A") +   
  theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
#plot(fixedOMcostsAplot)
ggsave(filename = paste(filePrefix, "fixedOMcostsAplot.pdf", sep=""),scale=1)

loanCostsAplot = ggplot(data=tickDF, aes(x=X0, y=meanLoanCostsA)) + 
  geom_errorbar(aes(ymin=meanLoanCostsA-seLoanCostsA, ymax=meanLoanCostsA+seLoanCostsA, width=1)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Loan Costs \n Country A") +  
  theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis),  
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
#plot(loanCostsAplot)
ggsave(filename = paste(filePrefix, "loanCostsAplot.pdf", sep=""),scale=1)

commodityCostsAplot = ggplot(data=tickDF, aes(x=X0, y= meanCommodityCostsA)) + 
  geom_errorbar(aes(ymin=meanCommodityCostsA-seCommodityCostsA, ymax= meanCommodityCostsA+seCommodityCostsA, width=1)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Commodity Costs \n Country A") +  
  theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis),   
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
#plot(commodityCostsAplot)
ggsave(filename = paste(filePrefix, "commodityCostsAplot.pdf", sep=""),scale=1)

downpaymentsCostsAplot = ggplot(data=tickDF, aes(x=X0, y=meanDownpaymentsCostsA)) + 
  geom_errorbar(aes(ymin=meanDownpaymentsCostsA-seDownpaymentsCostsA, ymax=meanDownpaymentsCostsA+seDownpaymentsCostsA, width=1)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Downpayment Costs \n Country A") + 
  theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis),  
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
#plot(downpaymentsCostsAplot)
ggsave(filename = paste(filePrefix, "downpaymentsCostsAplot.pdf", sep=""),scale=1)

fixedOMcostsBplot = ggplot(data=tickDF, aes(x=X0, y=meanFixedOMCostsB)) + 
  geom_errorbar(aes(ymin=meanFixedOMCostsB-seFixedOMCostsB, ymax=meanFixedOMCostsB+seFixedOMCostsB, width=1)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Fixed O&M Costs \n Country B") +  
  theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis),  
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
#plot(fixedOMcostsBplot)
ggsave(filename = paste(filePrefix, "fixedOMcostsBplot.pdf", sep=""),scale=1)

# loanCostsBplot = ggplot(data=tickDF, aes(x=X0, y=meanLoanCostsB)) + 
#   geom_errorbar(aes(ymin=meanLoanCostsB-seLoanCostsB, ymax=meanLoanCostsB+seLoanCostsB, width=1)) + 
#   geom_point() +
#   xlab("Year") +  
#   ylab("Eur") + 
#   ggtitle("Loan Costs \n Country B") +  
#   theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis),  
#         axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
# #plot(loanCostsBplot)
# ggsave(filename = paste(filePrefix, "loanCostsBplot.pdf", sep=""),scale=1)

# loanCostsBplot = ggplot(data=tickDF, aes(x=X0, y=meanLoanCostsB)) + 
#   stat_smooth(fun.data="median_hilow", conf.int=0.95, geom="smooth", method="gam")+
#   geom_point() +
#   xlab("Year") +  
#   ylab("Eur") + 
#   ggtitle("Loan Costs \n Country B") +  
#   theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis),  
#         axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
# plot(loanCostsBplot)
# #ggsave(filename = paste(filePrefix, "loanCostsBplot.pdf", sep=""),scale=1)

commodityCostsBplot = ggplot(data=tickDF, aes(x=X0, y= meanCommodityCostsB)) + 
  geom_errorbar(aes(ymin=meanCommodityCostsB-seCommodityCostsB, ymax= meanCommodityCostsB+seCommodityCostsB, width=1)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Commodity Costs \n Country B") +    
  theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis),  
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
#plot(commodityCostsBplot)
ggsave(filename = paste(filePrefix, "commodityCostsBplot.pdf", sep=""),scale=1)

downpaymentsCostsBplot = ggplot(data=tickDF, aes(x=X0, y=meanDownpaymentsCostsB)) + 
  geom_errorbar(aes(ymin=meanDownpaymentsCostsB-seDownpaymentsCostsB, ymax=meanDownpaymentsCostsB+seDownpaymentsCostsB, width=1)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Downpayment Costs \n Country B") +   
  theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
#plot(downpaymentsCostsBplot)
ggsave(filename = paste(filePrefix, "downpaymentsCostsBplot.pdf", sep=""),scale=1)


#Initiate
technologyOrder=c("Nuclear","Lignite","CoalPSC","IGCC","CCGT","OCGT","Biomass","Biogas","Wind","WindOffshore","PV")
technologyPalette=c("CoalPSC" = "black", "Biomass" = "darkgreen", "Biogas"="darkolivegreen3", "Nuclear" = "purple", "Lignite" = "saddlebrown",
                    "OCGT" = "darkred", "CCGT" = "blue", "PV" = "yellow", "Wind" = "chartreuse4",
                    "CoalPscCCS" = "darkgray", "IGCC" = "orange", "IgccCCS"="orangered", "CcgtCCS" = "red",
                    "WindOffshore" = "navyblue", "HydroPower" = "skyblue3")
producerPalette=c("A"="black","B"="darkgreen","C"="purple","D"="darkred","E"="blue","F"="yellow","G"="orange","H"="navyblue","I"="darkgrey")

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
  ggtitle("Relative generation shares Country A") +  
  theme(plot.title = element_text(size = titleSize),legend.text = element_text(size = legendSize),
        axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
plot(b)
ggsave(filename = paste(filePrefix, "relativeGenerationShareAplot.pdf", sep=""))


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
  ggtitle("Relative generation shares Country B") +  
  theme(plot.title = element_text(size = titleSize),legend.text = element_text(size = legendSize),
        axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
ggsave(filename = paste(filePrefix, "relativeGenerationShareBplot.pdf", sep=""))

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

estimationErrorExpectedGenerationA <- (renewableGenerationA - expectedRenewableGenerationA)*100/renewableGenerationA 
estimationErrorExpectedGenerationA <- (renewableGenerationB - expectedRenewableGenerationB)*100/renewableGenerationB  
seEstimationErrorExpectedGenerationA <- (seRenewableGenerationA - seExpectedRenewableGenerationA)/seExpectedRenewableGenerationA 
seEstimationErrorExpectedGenerationA <- (seRenewableGenerationB - seExpectedRenewableGenerationB)/seExpectedRenewableGenerationB

estimationErrorExpectedGenerationAplot = ggplot(data=tickDF, aes(x=X0, y=estimationErrorExpectedGenerationA )) + 
  geom_errorbar(aes(ymin=estimationErrorExpectedGenerationA-seEstimationErrorExpectedGenerationA, ymax=estimationErrorExpectedGenerationA+seEstimationErrorExpectedGenerationA), width=1) + 
  geom_point() +
  xlab("Year") +  
  ylab("[%]") + 
  ggtitle("Estimation Error \n Expected Generation Country A") +    
  theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis),
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
plot(estimationErrorExpectedGenerationAplot)
ggsave(filename = paste(filePrefix, "ForecastingErrorExpectedGenA.pdf", sep=""))

estimationErrorExpectedGenerationBplot = ggplot(data=tickDF, aes(x=X0, y=estimationErrorExpectedGenerationB )) + 
  geom_errorbar(aes(ymin=estimationErrorExpectedGenerationB-seEstimationErrorExpectedGenerationB, ymax=estimationErrorExpectedGenerationB+seEstimationErrorExpectedGenerationB), width=1) + 
  geom_point() +
  xlab("Year") +  
  ylab("[%]") + 
  ggtitle("Estimation Error \n Expected Generation Country B") +    
  theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis),  
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
plot(estimationErrorExpectedGenerationBplot)
ggsave(filename = paste(filePrefix, "ForecastingErrorExpectedGenB.pdf", sep=""))






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
  meanCapSharePVA[j] <- mean(subset(bigDF$CapacityinMWinA_Photovoltaic / (bigDF$CapacityinMWinA_Photovoltaic + bigDF$CapacityinMWinA_Wind + bigDF$CapacityinMWinA_WindOffshore + bigDF$CapacityinMWinA_Biomass + bigDF$CapacityinMWinA_Biogas + bigDF$CapacityinMWinA_CoalPSC + bigDF$CapacityinMWinA_IGCC + bigDF$CapacityinMWinA_Lignite + bigDF$CapacityinMWinA_OCGT +   bigDF$CapacityinMWinA_CCGT + bigDF$CapacityinMWinA_Nuclear), tick == j))
  meanCapShareWindA[j] <- mean(subset(bigDF$CapacityinMWinA_Wind / (bigDF$CapacityinMWinA_Photovoltaic + bigDF$CapacityinMWinA_Wind + bigDF$CapacityinMWinA_WindOffshore + bigDF$CapacityinMWinA_Biomass + bigDF$CapacityinMWinA_Biogas + bigDF$CapacityinMWinA_CoalPSC + bigDF$CapacityinMWinA_IGCC + bigDF$CapacityinMWinA_Lignite + bigDF$CapacityinMWinA_OCGT +   bigDF$CapacityinMWinA_CCGT + bigDF$CapacityinMWinA_Nuclear), tick == j))
  meanCapShareWindOffshoreA[j] <- mean(subset(bigDF$CapacityinMWinA_WindOffshore / (bigDF$CapacityinMWinA_Photovoltaic + bigDF$CapacityinMWinA_Wind + bigDF$CapacityinMWinA_WindOffshore + bigDF$CapacityinMWinA_Biomass + bigDF$CapacityinMWinA_Biogas + bigDF$CapacityinMWinA_CoalPSC + bigDF$CapacityinMWinA_IGCC + bigDF$CapacityinMWinA_Lignite + bigDF$CapacityinMWinA_OCGT +   bigDF$CapacityinMWinA_CCGT + bigDF$CapacityinMWinA_Nuclear), tick == j))
  meanCapShareBiomassA[j] <- mean(subset(bigDF$CapacityinMWinA_Biomass / (bigDF$CapacityinMWinA_Photovoltaic + bigDF$CapacityinMWinA_Wind + bigDF$CapacityinMWinA_WindOffshore + bigDF$CapacityinMWinA_Biomass + bigDF$CapacityinMWinA_Biogas + bigDF$CapacityinMWinA_CoalPSC + bigDF$CapacityinMWinA_IGCC + bigDF$CapacityinMWinA_Lignite + bigDF$CapacityinMWinA_OCGT +   bigDF$CapacityinMWinA_CCGT + bigDF$CapacityinMWinA_Nuclear), tick == j))
  meanCapShareBiogasA[j] <- mean(subset(bigDF$CapacityinMWinA_Biogas / (bigDF$CapacityinMWinA_Photovoltaic + bigDF$CapacityinMWinA_Wind + bigDF$CapacityinMWinA_WindOffshore + bigDF$CapacityinMWinA_Biomass + bigDF$CapacityinMWinA_Biogas + bigDF$CapacityinMWinA_CoalPSC + bigDF$CapacityinMWinA_IGCC + bigDF$CapacityinMWinA_Lignite + bigDF$CapacityinMWinA_OCGT +   bigDF$CapacityinMWinA_CCGT + bigDF$CapacityinMWinA_Nuclear), tick == j))
  meanCapShareCoalA[j] <- mean(subset(bigDF$CapacityinMWinA_CoalPSC / (bigDF$CapacityinMWinA_Photovoltaic + bigDF$CapacityinMWinA_Wind + bigDF$CapacityinMWinA_WindOffshore + bigDF$CapacityinMWinA_Biomass + bigDF$CapacityinMWinA_Biogas + bigDF$CapacityinMWinA_CoalPSC + bigDF$CapacityinMWinA_IGCC + bigDF$CapacityinMWinA_Lignite + bigDF$CapacityinMWinA_OCGT +   bigDF$CapacityinMWinA_CCGT + bigDF$CapacityinMWinA_Nuclear), tick == j))
  meanCapShareIGCCA[j] <- mean(subset(bigDF$CapacityinMWinA_IGCC / (bigDF$CapacityinMWinA_Photovoltaic + bigDF$CapacityinMWinA_Wind + bigDF$CapacityinMWinA_WindOffshore + bigDF$CapacityinMWinA_Biomass + bigDF$CapacityinMWinA_Biogas + bigDF$CapacityinMWinA_CoalPSC + bigDF$CapacityinMWinA_IGCC + bigDF$CapacityinMWinA_Lignite + bigDF$CapacityinMWinA_OCGT +   bigDF$CapacityinMWinA_CCGT + bigDF$CapacityinMWinA_Nuclear), tick == j))
  meanCapShareLigniteA[j] <- mean(subset(bigDF$CapacityinMWinA_Lignite / (bigDF$CapacityinMWinA_Photovoltaic + bigDF$CapacityinMWinA_Wind + bigDF$CapacityinMWinA_WindOffshore + bigDF$CapacityinMWinA_Biomass + bigDF$CapacityinMWinA_Biogas + bigDF$CapacityinMWinA_CoalPSC + bigDF$CapacityinMWinA_IGCC + bigDF$CapacityinMWinA_Lignite + bigDF$CapacityinMWinA_OCGT +   bigDF$CapacityinMWinA_CCGT + bigDF$CapacityinMWinA_Nuclear), tick == j))
  meanCapShareOCGTA[j] <- mean(subset(bigDF$CapacityinMWinA_OCGT / (bigDF$CapacityinMWinA_Photovoltaic + bigDF$CapacityinMWinA_Wind + bigDF$CapacityinMWinA_WindOffshore + bigDF$CapacityinMWinA_Biomass + bigDF$CapacityinMWinA_Biogas + bigDF$CapacityinMWinA_CoalPSC + bigDF$CapacityinMWinA_IGCC + bigDF$CapacityinMWinA_Lignite + bigDF$CapacityinMWinA_OCGT +   bigDF$CapacityinMWinA_CCGT + bigDF$CapacityinMWinA_Nuclear), tick == j))
  meanCapShareCCGTA[j] <- mean(subset(bigDF$CapacityinMWinA_CCGT / (bigDF$CapacityinMWinA_Photovoltaic + bigDF$CapacityinMWinA_Wind + bigDF$CapacityinMWinA_WindOffshore + bigDF$CapacityinMWinA_Biomass + bigDF$CapacityinMWinA_Biogas + bigDF$CapacityinMWinA_CoalPSC + bigDF$CapacityinMWinA_IGCC + bigDF$CapacityinMWinA_Lignite + bigDF$CapacityinMWinA_OCGT +   bigDF$CapacityinMWinA_CCGT + bigDF$CapacityinMWinA_Nuclear), tick == j))
  meanCapShareNuclearA[j] <- mean(subset(bigDF$CapacityinMWinA_Nuclear / (bigDF$CapacityinMWinA_Photovoltaic + bigDF$CapacityinMWinA_Wind + bigDF$CapacityinMWinA_WindOffshore + bigDF$CapacityinMWinA_Biomass + bigDF$CapacityinMWinA_Biogas + bigDF$CapacityinMWinA_CoalPSC + bigDF$CapacityinMWinA_IGCC + bigDF$CapacityinMWinA_Lignite + bigDF$CapacityinMWinA_OCGT +   bigDF$CapacityinMWinA_CCGT + bigDF$CapacityinMWinA_Nuclear), tick == j))
  meanCapSharePVB[j] <- mean(subset(bigDF$CapacityinMWinB_Photovoltaic / (bigDF$CapacityinMWinB_Photovoltaic + bigDF$CapacityinMWinB_Wind + bigDF$CapacityinMWinB_WindOffshore + bigDF$CapacityinMWinB_Biomass + bigDF$CapacityinMWinB_Biogas + bigDF$CapacityinMWinB_CoalPSC + bigDF$CapacityinMWinB_IGCC + bigDF$CapacityinMWinB_Lignite + bigDF$CapacityinMWinB_OCGT +   bigDF$CapacityinMWinB_CCGT + bigDF$CapacityinMWinB_Nuclear), tick == j))
  meanCapShareWindB[j] <- mean(subset(bigDF$CapacityinMWinB_Wind / (bigDF$CapacityinMWinB_Photovoltaic + bigDF$CapacityinMWinB_Wind + bigDF$CapacityinMWinB_WindOffshore + bigDF$CapacityinMWinB_Biomass + bigDF$CapacityinMWinB_Biogas + bigDF$CapacityinMWinB_CoalPSC + bigDF$CapacityinMWinB_IGCC + bigDF$CapacityinMWinB_Lignite + bigDF$CapacityinMWinB_OCGT +   bigDF$CapacityinMWinB_CCGT + bigDF$CapacityinMWinB_Nuclear), tick == j))
  meanCapShareWindOffshoreB[j] <- mean(subset(bigDF$CapacityinMWinB_WindOffshore / (bigDF$CapacityinMWinB_Photovoltaic + bigDF$CapacityinMWinB_Wind + bigDF$CapacityinMWinB_WindOffshore + bigDF$CapacityinMWinB_Biomass + bigDF$CapacityinMWinB_Biogas + bigDF$CapacityinMWinB_CoalPSC + bigDF$CapacityinMWinB_IGCC + bigDF$CapacityinMWinB_Lignite + bigDF$CapacityinMWinB_OCGT +   bigDF$CapacityinMWinB_CCGT + bigDF$CapacityinMWinB_Nuclear), tick == j))
  meanCapShareBiomassB[j] <- mean(subset(bigDF$CapacityinMWinB_Biomass / (bigDF$CapacityinMWinB_Photovoltaic + bigDF$CapacityinMWinB_Wind + bigDF$CapacityinMWinB_WindOffshore + bigDF$CapacityinMWinB_Biomass + bigDF$CapacityinMWinB_Biogas + bigDF$CapacityinMWinB_CoalPSC + bigDF$CapacityinMWinB_IGCC + bigDF$CapacityinMWinB_Lignite + bigDF$CapacityinMWinB_OCGT +   bigDF$CapacityinMWinB_CCGT + bigDF$CapacityinMWinB_Nuclear), tick == j))
  meanCapShareBiogasB[j] <- mean(subset(bigDF$CapacityinMWinB_Biogas / (bigDF$CapacityinMWinB_Photovoltaic + bigDF$CapacityinMWinB_Wind + bigDF$CapacityinMWinB_WindOffshore + bigDF$CapacityinMWinB_Biomass + bigDF$CapacityinMWinB_Biogas + bigDF$CapacityinMWinB_CoalPSC + bigDF$CapacityinMWinB_IGCC + bigDF$CapacityinMWinB_Lignite + bigDF$CapacityinMWinB_OCGT +   bigDF$CapacityinMWinB_CCGT + bigDF$CapacityinMWinB_Nuclear), tick == j))
  meanCapShareIGCCB[j] <- mean(subset(bigDF$CapacityinMWinB_IGCC / (bigDF$CapacityinMWinB_Photovoltaic + bigDF$CapacityinMWinB_Wind + bigDF$CapacityinMWinB_WindOffshore + bigDF$CapacityinMWinB_Biomass + bigDF$CapacityinMWinB_Biogas + bigDF$CapacityinMWinB_CoalPSC + bigDF$CapacityinMWinB_IGCC + bigDF$CapacityinMWinB_Lignite + bigDF$CapacityinMWinB_OCGT +   bigDF$CapacityinMWinB_CCGT + bigDF$CapacityinMWinB_Nuclear), tick == j))
  meanCapShareCoalB[j] <- mean(subset(bigDF$CapacityinMWinB_CoalPSC / (bigDF$CapacityinMWinB_Photovoltaic + bigDF$CapacityinMWinB_Wind + bigDF$CapacityinMWinB_WindOffshore + bigDF$CapacityinMWinB_Biomass + bigDF$CapacityinMWinB_Biogas + bigDF$CapacityinMWinB_CoalPSC + bigDF$CapacityinMWinB_IGCC + bigDF$CapacityinMWinB_Lignite + bigDF$CapacityinMWinB_OCGT +   bigDF$CapacityinMWinB_CCGT + bigDF$CapacityinMWinB_Nuclear), tick == j))
  meanCapShareLigniteB[j] <- mean(subset(bigDF$CapacityinMWinB_Lignite / (bigDF$CapacityinMWinB_Photovoltaic + bigDF$CapacityinMWinB_Wind + bigDF$CapacityinMWinB_WindOffshore + bigDF$CapacityinMWinB_Biomass + bigDF$CapacityinMWinB_Biogas + bigDF$CapacityinMWinB_CoalPSC + bigDF$CapacityinMWinB_IGCC + bigDF$CapacityinMWinB_Lignite + bigDF$CapacityinMWinB_OCGT +   bigDF$CapacityinMWinB_CCGT + bigDF$CapacityinMWinB_Nuclear), tick == j))
  meanCapShareOCGTB[j] <- mean(subset(bigDF$CapacityinMWinB_OCGT / (bigDF$CapacityinMWinB_Photovoltaic + bigDF$CapacityinMWinB_Wind + bigDF$CapacityinMWinB_WindOffshore + bigDF$CapacityinMWinB_Biomass + bigDF$CapacityinMWinB_Biogas + bigDF$CapacityinMWinB_CoalPSC + bigDF$CapacityinMWinB_IGCC + bigDF$CapacityinMWinB_Lignite + bigDF$CapacityinMWinB_OCGT +   bigDF$CapacityinMWinB_CCGT + bigDF$CapacityinMWinB_Nuclear), tick == j))
  meanCapShareCCGTB[j] <- mean(subset(bigDF$CapacityinMWinB_CCGT / (bigDF$CapacityinMWinB_Photovoltaic + bigDF$CapacityinMWinB_Wind + bigDF$CapacityinMWinB_WindOffshore + bigDF$CapacityinMWinB_Biomass + bigDF$CapacityinMWinB_Biogas + bigDF$CapacityinMWinB_CoalPSC + bigDF$CapacityinMWinB_IGCC + bigDF$CapacityinMWinB_Lignite + bigDF$CapacityinMWinB_OCGT +   bigDF$CapacityinMWinB_CCGT + bigDF$CapacityinMWinB_Nuclear), tick == j))
  meanCapShareNuclearB[j] <- mean(subset(bigDF$CapacityinMWinB_Nuclear / (bigDF$CapacityinMWinB_Photovoltaic + bigDF$CapacityinMWinB_Wind + bigDF$CapacityinMWinB_WindOffshore + bigDF$CapacityinMWinB_Biomass + bigDF$CapacityinMWinB_Biogas + bigDF$CapacityinMWinB_CoalPSC + bigDF$CapacityinMWinB_IGCC + bigDF$CapacityinMWinB_Lignite + bigDF$CapacityinMWinB_OCGT +   bigDF$CapacityinMWinB_CCGT + bigDF$CapacityinMWinB_Nuclear), tick == j))
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
  ggtitle("Relative capacity shares Country A") +  
  theme(plot.title = element_text(size = titleSize),legend.text = element_text(size = legendSize),
        axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
ggsave(filename = paste(filePrefix, "relativeCapacityShareAplot.pdf", sep=""))

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
  ggtitle("Relative capacity shares Country B") +  
  theme(plot.title = element_text(size = titleSize),legend.text = element_text(size = legendSize),
        axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
#plot(a)
ggsave(filename = paste(filePrefix, "relativeCapacityShareBplot.pdf", sep=""))

segmentPricesA = 
  ggplot(data=bigDF, aes(x=tick)) + 
  geom_point(aes(y=bigDF$PriceInEURperMWh_Segment_Country_A_1)) + 
  geom_point(aes(y=bigDF$PriceInEURperMWh_Segment_Country_A_2)) + 
  geom_point(aes(y=bigDF$PriceInEURperMWh_Segment_Country_A_3)) + 
  geom_point(aes(y=bigDF$PriceInEURperMWh_Segment_Country_A_4)) + 
  geom_point(aes(y=bigDF$PriceInEURperMWh_Segment_Country_A_5)) + 
  geom_point(aes(y=bigDF$PriceInEURperMWh_Segment_Country_A_6)) + 
  geom_point(aes(y=bigDF$PriceInEURperMWh_Segment_Country_A_7)) + 
  geom_point(aes(y=bigDF$PriceInEURperMWh_Segment_Country_A_8)) + 
  geom_point(aes(y=bigDF$PriceInEURperMWh_Segment_Country_A_9)) + 
  geom_point(aes(y=bigDF$PriceInEURperMWh_Segment_Country_A_10)) + 
  geom_point(aes(y=bigDF$PriceInEURperMWh_Segment_Country_A_11)) + 
  geom_point(aes(y=bigDF$PriceInEURperMWh_Segment_Country_A_12)) + 
  geom_point(aes(y=bigDF$PriceInEURperMWh_Segment_Country_A_13)) + 
  geom_point(aes(y=bigDF$PriceInEURperMWh_Segment_Country_A_14)) + 
  geom_point(aes(y=bigDF$PriceInEURperMWh_Segment_Country_A_15)) + 
  geom_point(aes(y=bigDF$PriceInEURperMWh_Segment_Country_A_16)) + 
  geom_point(aes(y=bigDF$PriceInEURperMWh_Segment_Country_A_17)) + 
  geom_point(aes(y=bigDF$PriceInEURperMWh_Segment_Country_A_18)) + 
  geom_point(aes(y=bigDF$PriceInEURperMWh_Segment_Country_A_19)) + 
  geom_point(aes(y=bigDF$PriceInEURperMWh_Segment_Country_A_20)) + 
  xlab("Tick") +  
  ylab("Price (EUR/MWh)") + 
  ggtitle("Segment Price development - Country A") + 
  theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
plot(segmentPricesA)
ggsave(filename = paste(filePrefix, "PriceSegmentA.pdf", sep=""))

segmentPricesB = 
  ggplot(data=bigDF, aes(x=tick)) + 
  geom_point(aes(y=bigDF$PriceInEURperMWh_Segment_Country_B_1)) + 
  geom_point(aes(y=bigDF$PriceInEURperMWh_Segment_Country_B_2)) + 
  geom_point(aes(y=bigDF$PriceInEURperMWh_Segment_Country_B_3)) + 
  geom_point(aes(y=bigDF$PriceInEURperMWh_Segment_Country_B_4)) + 
  geom_point(aes(y=bigDF$PriceInEURperMWh_Segment_Country_B_5)) + 
  geom_point(aes(y=bigDF$PriceInEURperMWh_Segment_Country_B_6)) + 
  geom_point(aes(y=bigDF$PriceInEURperMWh_Segment_Country_B_7)) + 
  geom_point(aes(y=bigDF$PriceInEURperMWh_Segment_Country_B_8)) + 
  geom_point(aes(y=bigDF$PriceInEURperMWh_Segment_Country_B_9)) + 
  geom_point(aes(y=bigDF$PriceInEURperMWh_Segment_Country_B_10)) + 
  geom_point(aes(y=bigDF$PriceInEURperMWh_Segment_Country_B_11)) + 
  geom_point(aes(y=bigDF$PriceInEURperMWh_Segment_Country_B_12)) + 
  geom_point(aes(y=bigDF$PriceInEURperMWh_Segment_Country_B_13)) + 
  geom_point(aes(y=bigDF$PriceInEURperMWh_Segment_Country_B_14)) + 
  geom_point(aes(y=bigDF$PriceInEURperMWh_Segment_Country_B_15)) + 
  geom_point(aes(y=bigDF$PriceInEURperMWh_Segment_Country_B_16)) + 
  geom_point(aes(y=bigDF$PriceInEURperMWh_Segment_Country_B_17)) + 
  geom_point(aes(y=bigDF$PriceInEURperMWh_Segment_Country_B_18)) + 
  geom_point(aes(y=bigDF$PriceInEURperMWh_Segment_Country_B_19)) + 
  geom_point(aes(y=bigDF$PriceInEURperMWh_Segment_Country_B_20)) + 
  xlab("Tick") +  
  ylab("Price (EUR/MWh)") + 
  ggtitle("Segment Price development - Country B") +   
  theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
plot(segmentPricesB)
ggsave(filename = paste(filePrefix, "PriceSegmentB.pdf", sep=""))


# a <- table(bigDF$PriceInEURperMWh_Segment_Country_B_1)
# a[names(a)==2000]
# 
# a <- table(  bigDF$PriceInEURperMWh_Segment_Country_B_2)
# a[names(a)==2000]
#              a <- table(  bigDF$PriceInEURperMWh_Segment_Country_B_3)
#              a[names(a)==2000]
#              a <- table( bigDF$PriceInEURperMWh_Segment_Country_B_4)
#              a[names(a)==2000]
#              a <- table(  bigDF$PriceInEURperMWh_Segment_Country_B_5)
#              a[names(a)==2000]
#              a <- table(  bigDF$PriceInEURperMWh_Segment_Country_B_6)
#              a[names(a)==2000]
#              a <- table( bigDF$PriceInEURperMWh_Segment_Country_B_7)
#              a[names(a)==2000]
#              a <- table( bigDF$PriceInEURperMWh_Segment_Country_B_8)
#              a[names(a)==2000]
#              a <- table( bigDF$PriceInEURperMWh_Segment_Country_B_9)
#              a[names(a)==2000]
#              a <- table( bigDF$PriceInEURperMWh_Segment_Country_B_10)
#              a[names(a)==2000]
#              a <- table( bigDF$PriceInEURperMWh_Segment_Country_B_11)
#              a[names(a)==2000]
#              a <- table( bigDF$PriceInEURperMWh_Segment_Country_B_12)
#              a[names(a)==2000]
#              a <- table(  bigDF$PriceInEURperMWh_Segment_Country_B_13)
#              a[names(a)==2000]
#              a <- table( bigDF$PriceInEURperMWh_Segment_Country_B_14)
#              a[names(a)==2000]
#              a <- table( bigDF$PriceInEURperMWh_Segment_Country_B_15)
#              a[names(a)==2000]
#              a <- table(  bigDF$PriceInEURperMWh_Segment_Country_B_16)
#              a[names(a)==2000]
#              a <- table(  bigDF$PriceInEURperMWh_Segment_Country_B_17)
#              a[names(a)==2000]
#              a <- table(  bigDF$PriceInEURperMWh_Segment_Country_B_18)
#              a[names(a)==2000]
#              a <- table(  bigDF$PriceInEURperMWh_Segment_Country_B_19)
#              a[names(a)==2000]
#              a <- table(bigDF$PriceInEURperMWh_Segment_Country_B_20)
#              a[names(a)==2000]

# 610 + 448 + 332 + 248 + 189 + 148 + 103 +63 + 19 + 8 + 3

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

renewableTargetDeviationAplot = ggplot(data=targetDF, aes(x=tick, y=(meanRealizedTargetA-nl_target)*100)) + 
  geom_line() + 
  xlab("Year") +  
  ylab("[%]") + 
  ggtitle("Difference Realized Renewable Target and \n NREAP target (Country A)") +  
  theme(plot.title = element_text(size = titleSize),
        axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
plot(renewableTargetDeviationAplot)
ggsave(filename = paste(filePrefix, "renewableTargetDeviationA.pdf", sep=""),scale=1)

renewableTargetDeviationBplot = ggplot(data=targetDF, aes(x=tick, y=(meanRealizedTargetB-de_target)*100)) + 
  geom_line() + 
  xlab("Year") +  
  ylab("[%]") + 
  ggtitle("Difference Realized Renewable Target and \n NREAP target (Country B)") + 
  theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
plot(renewableTargetDeviationBplot)
ggsave(filename = paste(filePrefix, "renewableTargetDeviationB.pdf", sep=""),scale=1)


## Same Target editions
renewableTargetDeviationAplot = ggplot(data=targetDF, aes(x=tick, y=(meanRealizedTargetA-nl_target)*100)) + 
  geom_line() + 
  xlab("Year") +  
  ylab("[%]") + 
  ggtitle("Difference Realized Renewable Target and \n NREAP target (Country A)") +  
  theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
plot(renewableTargetDeviationAplot)
ggsave(filename = paste(filePrefix, "SAMETARGETrenewableTargetDeviationA.pdf", sep=""),scale=1)

renewableTargetDeviationBplot = ggplot(data=targetDF, aes(x=tick, y=(meanRealizedTargetB-nl_target)*100)) + 
  geom_line() + 
  xlab("Year") +  
  ylab("[%]") + 
  ggtitle("Difference Realized Renewable Target and \n NREAP target (Country B)") + 
  theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
plot(renewableTargetDeviationBplot)
ggsave(filename = paste(filePrefix, "SAMETARGETrenewableTargetDeviationB.pdf", sep=""),scale=1)

## Paced Target editions
renewableTargetDeviationAplot = ggplot(data=targetDF, aes(x=tick, y=(meanRealizedTargetA-nl_target_slow)*100)) + 
  geom_line() + 
  xlab("Year") +  
  ylab("[%]") + 
  ggtitle("Difference Realized Renewable Target and \n NREAP target (Country A)") +  
  theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
plot(renewableTargetDeviationAplot)
ggsave(filename = paste(filePrefix, "PACEDTARGETrenewableTargetDeviationA.pdf", sep=""),scale=1)

renewableTargetDeviationBplot = ggplot(data=targetDF, aes(x=tick, y=(meanRealizedTargetB-de_target)*100)) + 
  geom_line() + 
  xlab("Year") +  
  ylab("[%]") + 
  ggtitle("Difference Realized Renewable Target and \n NREAP target (Country B)") + 
  theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
plot(renewableTargetDeviationBplot)
ggsave(filename = paste(filePrefix, "PACEDTARGETrenewableTargetDeviationB.pdf", sep=""),scale=1)

## Tech Spec edition
meanRealizedTargetPVA=0 
meanRealizedTargetWindA=0
meanRealizedTargetWindOffA=0 
meanRealizedTargetBiomassA=0 
meanRealizedTargetBiogasA=0 
meanRealizedTargetPVB=0 
meanRealizedTargetWindB=0
meanRealizedTargetWindOffB=0 
meanRealizedTargetBiomassB=0 
meanRealizedTargetBiogasB=0 

for(j in 0:39) {
  meanRealizedTargetPVA[j] <- mean(subset(bigDF$GenerationinMWhCountryA_Photovoltaic/ bigDF$Total_DemandinMWh_Country_A, tick == j))
  meanRealizedTargetWindA[j] <- mean(subset(bigDF$GenerationinMWhCountryA_Wind/ bigDF$Total_DemandinMWh_Country_A , tick == j))
  meanRealizedTargetWindOffA[j] <- mean(subset(bigDF$GenerationinMWhCountryA_WindOffshore/ bigDF$Total_DemandinMWh_Country_A , tick == j))
  meanRealizedTargetBiomassA[j] <- mean(subset(bigDF$GenerationinMWhCountryA_Biomass/ bigDF$Total_DemandinMWh_Country_A, tick == j))
  meanRealizedTargetBiogasA[j] <- mean(subset(bigDF$GenerationinMWhCountryA_Biogas/ bigDF$Total_DemandinMWh_Country_A, tick == j))
  meanRealizedTargetPVB[j] <- mean(subset(bigDF$GenerationinMWhCountryB_Photovoltaic/ bigDF$Total_DemandinMWh_Country_B, tick == j))
  meanRealizedTargetWindB[j] <- mean(subset(bigDF$GenerationinMWhCountryB_Wind/ bigDF$Total_DemandinMWh_Country_B , tick == j))
  meanRealizedTargetWindOffB[j] <- mean(subset(bigDF$GenerationinMWhCountryB_WindOffshore/ bigDF$Total_DemandinMWh_Country_B , tick == j))
  meanRealizedTargetBiomassB[j] <- mean(subset(bigDF$GenerationinMWhCountryB_Biomass/ bigDF$Total_DemandinMWh_Country_B, tick == j))
  meanRealizedTargetBiogasB[j] <- mean(subset(bigDF$GenerationinMWhCountryB_Biogas/ bigDF$Total_DemandinMWh_Country_B, tick == j))
}



renewableTargetDeviationPVAplot = ggplot(data=targetDF, aes(x=tick, y=(meanRealizedTargetPVA-nl_target_photovoltaics)*100)) + 
  geom_line() + 
  xlab("Year") +  
  ylab("[%]") + 
  ggtitle("Difference Realized Renewable Target and \n NREAP target PV (Country A)") +  
  theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
plot(renewableTargetDeviationPVAplot)
ggsave(filename = paste(filePrefix, "TechSpecrenewableTargetDeviationPVA.pdf", sep=""),scale=1)

renewableTargetDeviationWindAplot = ggplot(data=targetDF, aes(x=tick, y=(meanRealizedTargetWindA-nl_target_windon)*100)) + 
  geom_line() + 
  xlab("Year") +  
  ylab("[%]") + 
  ggtitle("Difference Realized Renewable Target and \n NREAP target Wind (Country A)") +  
  theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
plot(renewableTargetDeviationWindAplot)
ggsave(filename = paste(filePrefix, "TechSpecrenewableTargetDeviationWindA.pdf", sep=""),scale=1)

renewableTargetDeviationWindOffAplot = ggplot(data=targetDF, aes(x=tick, y=(meanRealizedTargetWindOffA-nl_target_windoff)*100)) + 
  geom_line() + 
  xlab("Year") +  
  ylab("[%]") + 
  ggtitle("Difference Realized Renewable Target and \n NREAP target WindOff (Country A)") +  
  theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
plot(renewableTargetDeviationWindOffAplot)
ggsave(filename = paste(filePrefix, "TechSpecrenewableTargetDeviationWindOffA.pdf", sep=""),scale=1)

renewableTargetDeviationBiomassAplot = ggplot(data=targetDF, aes(x=tick, y=(meanRealizedTargetBiomassA-nl_target_biomass)*100)) + 
  geom_line() + 
  xlab("Year") +  
  ylab("[%]") + 
  ggtitle("Difference Realized Renewable Target and \n NREAP target Biomass (Country A)") +  
  theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
plot(renewableTargetDeviationBiomassAplot)
ggsave(filename = paste(filePrefix, "TechSpecrenewableTargetDeviationBiomassA.pdf", sep=""),scale=1)

renewableTargetDeviationBiogasAplot = ggplot(data=targetDF, aes(x=tick, y=(meanRealizedTargetBiogasA-nl_target_biogas)*100)) + 
  geom_line() + 
  xlab("Year") +  
  ylab("[%]") + 
  ggtitle("Difference Realized Renewable Target and \n NREAP target Biogas (Country A)") +  
  theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
plot(renewableTargetDeviationBiogasAplot)
ggsave(filename = paste(filePrefix, "TechSpecrenewableTargetDeviationBiogasA.pdf", sep=""),scale=1)


renewableTargetDeviationPVBplot = ggplot(data=targetDF, aes(x=tick, y=(meanRealizedTargetPVB-nl_target_photovoltaics)*100)) + 
  geom_line() + 
  xlab("Year") +  
  ylab("[%]") + 
  ggtitle("Difference Realized Renewable Target and \n NREAP target PV (Country B)") +  
  theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
plot(renewableTargetDeviationPVBplot)
ggsave(filename = paste(filePrefix, "TechSpecrenewableTargetDeviationPVB.pdf", sep=""),scale=1)

renewableTargetDeviationWindBplot = ggplot(data=targetDF, aes(x=tick, y=(meanRealizedTargetWindB-nl_target_windon)*100)) + 
  geom_line() + 
  xlab("Year") +  
  ylab("[%]") + 
  ggtitle("Difference Realized Renewable Target and \n NREAP target Wind (Country B)") +  
  theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
plot(renewableTargetDeviationWindBplot)
ggsave(filename = paste(filePrefix, "TechSpecrenewableTargetDeviationWindB.pdf", sep=""),scale=1)

renewableTargetDeviationWindOffBplot = ggplot(data=targetDF, aes(x=tick, y=(meanRealizedTargetWindOffB-nl_target_windoff)*100)) + 
  geom_line() + 
  xlab("Year") +  
  ylab("[%]") + 
  ggtitle("Difference Realized Renewable Target and \n NREAP target WindOff (Country B)") +  
  theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
plot(renewableTargetDeviationWindOffBplot)
ggsave(filename = paste(filePrefix, "TechSpecrenewableTargetDeviationWindOffB.pdf", sep=""),scale=1)

renewableTargetDeviationBiomassBplot = ggplot(data=targetDF, aes(x=tick, y=(meanRealizedTargetBiomassB-nl_target_biomass)*100)) + 
  geom_line() + 
  xlab("Year") +  
  ylab("[%]") + 
  ggtitle("Difference Realized Renewable Target and \n NREAP target Biomass (Country B)") +  
  theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
plot(renewableTargetDeviationBiomassBplot)
ggsave(filename = paste(filePrefix, "TechSpecrenewableTargetDeviationBiomassB.pdf", sep=""),scale=1)

renewableTargetDeviationBiogasBplot = ggplot(data=targetDF, aes(x=tick, y=(meanRealizedTargetBiogasB-nl_target_biogas)*100)) + 
  geom_line() + 
  xlab("Year") +  
  ylab("[%]") + 
  ggtitle("Difference Realized Renewable Target and \n NREAP target Biogas (Country B)") +  
  theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
plot(renewableTargetDeviationBiogasBplot)
ggsave(filename = paste(filePrefix, "TechSpecrenewableTargetDeviationBiogasB.pdf", sep=""),scale=1)

# Average electricity wholesale price in country
meanElectricityPriceA=0
meanElectricityPriceB=0
seElectricityPriceA=0
seElectricityPriceB=0
sdElectricityPriceA=0
sdElectricityPriceB=0

for(j in 0:39) {
  meanElectricityPriceA[j] <- mean(subset(bigDF$Avg_El_PricesinEURpMWh_Country_A, tick == j))
  meanElectricityPriceB[j] <- mean(subset(bigDF$Avg_El_PricesinEURpMWh_Country_B, tick == j))
  seElectricityPriceA[j] <- std.error(subset(bigDF$Avg_El_PricesinEURpMWh_Country_A, tick == j))
  seElectricityPriceB[j] <- std.error(subset(bigDF$Avg_El_PricesinEURpMWh_Country_B, tick == j))
  sdElectricityPriceA[j] <- sd(subset(bigDF$Avg_El_PricesinEURpMWh_Country_A, tick == j))
  sdElectricityPriceB[j] <- sd(subset(bigDF$Avg_El_PricesinEURpMWh_Country_B, tick == j))
}
meanElectricityPriceAplot = ggplot(data=tickDF, aes(x=X0, y=meanElectricityPriceA)) + 
  # geom_errorbar(aes(ymin=meanElectricityPriceA-seElectricityPriceA, ymax=meanElectricityPriceA+seElectricityPriceA, width=1)) + 
  geom_ribbon(aes(ymin=meanElectricityPriceA-(1.96*sdElectricityPriceA), ymax=meanElectricityPriceA+(1.96*sdElectricityPriceA), width=1), alpha=0.2, colour="grey") +
  geom_ribbon(aes(ymin=meanElectricityPriceA-(0.67*sdElectricityPriceA), ymax=meanElectricityPriceA+(0.67*sdElectricityPriceA), width=1), alpha=0.5, colour="grey") +
  geom_line(colour="blue") +
  xlab("Year") +  
  ylab("Eur/MWh") + 
  ggtitle("Mean electricity price \n Country A") + 
  theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
plot(meanElectricityPriceAplot)
ggsave(filename = paste(filePrefix, "meanElectricityPriceA.pdf", sep=""),scale=1)

# meanElectricityPriceAplot = ggplot(data=tickDF, aes(x=X0, y=meanElectricityPriceA)) + 
#   # geom_errorbar(aes(ymin=meanElectricityPriceA-seElectricityPriceA, ymax=meanElectricityPriceA+seElectricityPriceA, width=1)) + 
#   geom_ribbon(aes(ymin=meanElectricityPriceA-(1.96*seElectricityPriceA), ymax=meanElectricityPriceA+(1.96*seElectricityPriceA), width=1), alpha=0.2, colour="grey") +
#   geom_ribbon(aes(ymin=meanElectricityPriceA-(0.67*seElectricityPriceA), ymax=meanElectricityPriceA+(0.67*seElectricityPriceA), width=1), alpha=0.5, colour="grey") +
#   geom_line(colour="blue") +
#   xlab("Year") +  
#   ylab("Eur/MWh") + 
#   ggtitle("Mean electricity price \n Country A") + 
#   theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
#         axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
# plot(meanElectricityPriceAplot)
# ggsave(filename = paste(filePrefix, "meanElectricityPriceA.pdf", sep=""),scale=1)

meanElectricityPriceAplot = ggplot(data=tickDF, aes(x=X0, y=meanElectricityPriceA)) + 
  # geom_errorbar(aes(ymin=meanElectricityPriceA-seElectricityPriceA, ymax=meanElectricityPriceA+seElectricityPriceA, width=1)) + 
  geom_ribbon(aes(ymin=meanElectricityPriceA-(1.96*sdElectricityPriceA), ymax=meanElectricityPriceA+(1.96*sdElectricityPriceA), width=1), alpha=0.2, colour="grey") +
  geom_ribbon(aes(ymin=meanElectricityPriceA-(0.67*sdElectricityPriceA), ymax=meanElectricityPriceA+(0.67*sdElectricityPriceA), width=1), alpha=0.5, colour="grey") +
  geom_line(colour="blue") +
  xlab("Year") +  
  ylab("Eur/MWh") + 
  ggtitle("Mean electricity price \n Country A") + 
  theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
plot(meanElectricityPriceAplot)
ggsave(filename = paste(filePrefix, "meanElectricityPriceA.pdf", sep=""),scale=1)

meanElectricityPriceBplot = ggplot(data=tickDF, aes(x=X0, y=meanElectricityPriceB)) + 
  # geom_errorbar(aes(ymin=meanElectricityPriceB-seElectricityPriceB, ymax=meanElectricityPriceB+seElectricityPriceB, width=1)) + 
  geom_ribbon(aes(ymin=meanElectricityPriceB-(1.96*sdElectricityPriceB), ymax=meanElectricityPriceB+(1.96*sdElectricityPriceB), width=1), alpha=0.2, colour="grey") +
  geom_ribbon(aes(ymin=meanElectricityPriceB-(0.67*sdElectricityPriceB), ymax=meanElectricityPriceB+(0.67*sdElectricityPriceB), width=1), alpha=0.5, colour="grey") +
  geom_line(colour="blue") +
  xlab("Year") +  
  ylab("Eur/MWh") + 
  ggtitle("Mean electricity price \n Country B") + 
  theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
plot(meanElectricityPriceBplot)
ggsave(filename = paste(filePrefix, "meanElectricityPriceB.pdf", sep=""),scale=1)


diff_el_price_AB = ggplot(data=tickDF , aes(x=X0, y=(meanElectricityPriceA-meanElectricityPriceB))) + 
  geom_line() +  #(aes(colour = runNumber))
  xlab("Year") +  
  ylab("Eur/MWh") + 
  ggtitle("Difference Electricity Price \n Country A - Country B") +   
  theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
#plot(diff_el_price_AB)
ggsave(filename = paste(filePrefix, "Diff_ElectricityPriceAverageAB.pdf", sep=""))


#Tender Clearing Prices
meanTenderClearingPriceA=0
meanTenderClearingPriceB=0
seTenderClearingPriceA=0
seTenderClearingPriceB=0

for(j in 0:39) {
  meanTenderClearingPriceA[j] <- mean(subset(bigDF$tenderClearingPrice_Country_A, tick == j))
  meanTenderClearingPriceB[j] <- mean(subset(bigDF$tenderClearingPrice_Country_B, tick == j))
  seTenderClearingPriceA[j] <- std.error(subset(bigDF$tenderClearingPrice_Country_A, tick == j))
  seTenderClearingPriceB[j] <- std.error(subset(bigDF$tenderClearingPrice_Country_B, tick == j))

}


tenderClearingPriceCountryAplot = ggplot(data=tickDF, aes(x=X0, y=meanTenderClearingPriceA)) + 
  geom_errorbar(aes(ymin=meanTenderClearingPriceA-seTenderClearingPriceA, ymax=meanTenderClearingPriceA+seTenderClearingPriceA, width=1)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur/MWh") + 
  ggtitle("Mean tender clearing price \n Country A") + 
  theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
plot(tenderClearingPriceCountryAplot)
ggsave(filename = paste(filePrefix, "meanTenderClearingPriceA.pdf", sep=""),scale=1)

tenderClearingPriceCountryBplot = ggplot(data=tickDF, aes(x=X0, y=meanTenderClearingPriceB)) + 
  geom_errorbar(aes(ymin=meanTenderClearingPriceB-seTenderClearingPriceB, ymax=meanTenderClearingPriceB+seTenderClearingPriceB, width=1)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur/MWh") + 
  ggtitle("Mean tender clearing price \n Country B") +   
  theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
plot(tenderClearingPriceCountryBplot)
ggsave(filename = paste(filePrefix, "meanTenderClearingPriceB.pdf", sep=""),scale=1)

# Relation El price and Tender clearing price
scatterElectricityTenderPriceAplot = ggplot(data=tickDF, aes(x=meanTenderClearingPriceA, y=meanElectricityPriceA)) + 
  geom_point() +
  geom_smooth() +
  xlab("Tender Clearing Price [Eur/MWh]") +  
  ylab("Electricity Price [Eur/MWh]") + 
  ggtitle("Relationship Electricity price versus \n tender clearing price  Country A") +    
  theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
plot(scatterElectricityTenderPriceAplot)
ggsave(filename = paste(filePrefix, "scatterElectricityTenderPriceA.pdf", sep=""),scale=1)

scatterElectricityTenderPriceBplot = ggplot(data=tickDF, aes(x=meanTenderClearingPriceB, y=meanElectricityPriceB)) + 
  geom_point() +
  geom_smooth() +
  xlab("Tender Clearing Price [Eur/MWh]") +  
  ylab("Electricity Price [Eur/MWh]") +
  ggtitle("Relationship Electricity price versus \n tender clearing price  Country B") +    
  theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
plot(scatterElectricityTenderPriceBplot)
ggsave(filename = paste(filePrefix, "scatterElectricityTenderPriceB.pdf", sep=""),scale=1)


# Tender clearing prices Tech Spec 1 edition
tcpPVA <- tcp1DF %>% 
  group_by(scheme, price) %>%
  select(scheme, price, tick) %>%
  filter(scheme == "RenewableTenderNLphotovoltaicPGT")

tcpWindOffA <- tcp1DF %>% 
  group_by(scheme, price) %>%
  select(scheme, price, tick) %>%
  filter(scheme == "RenewableTenderNLwindOffshorePGT")

tcpWindA <- tcp1DF %>% 
  group_by(scheme, price) %>%
  select(scheme, price, tick) %>%
  filter(scheme == "RenewableTenderNLwindPGT")

tcpBiomassA <- tcp1DF %>% 
  group_by(scheme, price) %>%
  select(scheme, price, tick) %>%
  filter(scheme == "RenewableTenderNLbiomassPGT")

tcpBiogasA <- tcp1DF %>% 
  group_by(scheme, price) %>%
  select(scheme, price, tick) %>%
  filter(scheme == "RenewableTenderNLbiogasPGT")


tcpPVB <- tcp1DF %>% 
  group_by(scheme, price) %>%
  select(scheme, price, tick) %>%
  filter(scheme == "RenewableTenderDEphotovoltaicPGT")

tcpWindOffB <- tcp1DF %>% 
  group_by(scheme, price) %>%
  select(scheme, price, tick) %>%
  filter(scheme == "RenewableTenderDEwindOffshorePGT")

tcpWindB <- tcp1DF %>% 
  group_by(scheme, price) %>%
  select(scheme, price, tick) %>%
  filter(scheme == "RenewableTenderDEwindPGT")

tcpBiomassB <- tcp1DF %>% 
  group_by(scheme, price) %>%
  select(scheme, price, tick) %>%
  filter(scheme == "RenewableTenderDEbiomassPGT")

tcpBiogasB <- tcp1DF %>% 
  group_by(scheme, price) %>%
  select(scheme, price, tick) %>%
  filter(scheme == "RenewableTenderDEbiogasPGT")


meanTechSpecTenderClearingPricePVA=0
meanTechSpecTenderClearingPriceWindOffshoreA=0
meanTechSpecTenderClearingPriceWindA=0
meanTechSpecTenderClearingPriceBiomassA=0
meanTechSpecTenderClearingPriceBiogasA=0
meanTechSpecTenderClearingPricePVB=0
meanTechSpecTenderClearingPriceWindOffshoreB=0
meanTechSpecTenderClearingPriceWindB=0
meanTechSpecTenderClearingPriceBiomassB=0
meanTechSpecTenderClearingPriceBiogasB=0

for(j in 0:39) {
  meanTechSpecTenderClearingPricePVA[j] <- mean(subset(tcpPVA$price, tick == j))
  meanTechSpecTenderClearingPriceWindOffshoreA[j] <- mean(subset(tcpWindOffA$price, tick == j))
  meanTechSpecTenderClearingPriceWindA[j] <- mean(subset(tcpWindA$price, tick == j))
  meanTechSpecTenderClearingPriceBiomassA[j] <- mean(subset(tcpBiomassA$price, tick == j))
  meanTechSpecTenderClearingPriceBiogasA[j] <- mean(subset(tcpBiogasA$price, tick == j))
  meanTechSpecTenderClearingPricePVB[j] <- mean(subset(tcpPVB$price, tick == j))
  meanTechSpecTenderClearingPriceWindOffshoreB[j] <- mean(subset(tcpWindOffB$price, tick == j))
  meanTechSpecTenderClearingPriceWindB[j] <- mean(subset(tcpWindB$price, tick == j))
  meanTechSpecTenderClearingPriceBiomassB[j] <- mean(subset(tcpBiomassB$price, tick == j))
  meanTechSpecTenderClearingPriceBiogasB[j] <- mean(subset(tcpBiogasB$price, tick == j))
}


techSpec1PVAplot = ggplot(data=tickDF, aes(x=X0, y=meanTechSpecTenderClearingPricePVA)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur/MWh") + 
  ggtitle("Mean tender clearing price PV \n Country A") + 
  theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
plot(techSpec1PVAplot)
ggsave(filename = paste(filePrefix, "techSpec1PVAplot.pdf", sep=""),scale=1)

techSpec1WindOffAplot = ggplot(data=tickDF, aes(x=X0, y= meanTechSpecTenderClearingPriceWindOffshoreA)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur/MWh") + 
  ggtitle("Mean tender clearing price WindOff \n Country A") + 
  theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
plot(techSpec1WindOffAplot)
ggsave(filename = paste(filePrefix, "techSpec1WindOffAplot.pdf", sep=""),scale=1)

techSpec1WindAplot = ggplot(data=tickDF, aes(x=X0, y=meanTechSpecTenderClearingPriceWindA)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur/MWh") + 
  ggtitle("Mean tender clearing price Wind \n Country A") + 
  theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
plot(techSpec1WindAplot)
ggsave(filename = paste(filePrefix, "techSpec1WindAplot.pdf", sep=""),scale=1)

techSpec1BiomassAplot = ggplot(data=tickDF, aes(x=X0, y=meanTechSpecTenderClearingPriceBiomassA)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur/MWh") + 
  ggtitle("Mean tender clearing price Biomass \n Country A") + 
  theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
plot(techSpec1BiomassAplot)
ggsave(filename = paste(filePrefix, "techSpec1BiomassAplot.pdf", sep=""),scale=1)

techSpec1BiogasAplot = ggplot(data=tickDF, aes(x=X0, y=meanTechSpecTenderClearingPriceBiogasA)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur/MWh") + 
  ggtitle("Mean tender clearing price Biogas \n Country A") + 
  theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
plot(techSpec1BiogasAplot)
ggsave(filename = paste(filePrefix, "techSpec1BiogasAplot.pdf", sep=""),scale=1)



techSpec1PVBplot = ggplot(data=tickDF, aes(x=X0, y=meanTechSpecTenderClearingPricePVB)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur/MWh") + 
  ggtitle("Mean tender clearing price PV \n Country B") + 
  theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
plot(techSpec1PVBplot)
ggsave(filename = paste(filePrefix, "techSpec1PVBplot.pdf", sep=""),scale=1)

techSpec1WindOffBplot = ggplot(data=tickDF, aes(x=X0, y= meanTechSpecTenderClearingPriceWindOffshoreB)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur/MWh") + 
  ggtitle("Mean tender clearing price WindOff \n Country B") + 
  theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
plot(techSpec1WindOffBplot)
ggsave(filename = paste(filePrefix, "techSpec1WindOffBplot.pdf", sep=""),scale=1)

techSpec1WindBplot = ggplot(data=tickDF, aes(x=X0, y=meanTechSpecTenderClearingPriceWindB)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur/MWh") + 
  ggtitle("Mean tender clearing price Wind \n Country B") + 
  theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
plot(techSpec1WindBplot)
ggsave(filename = paste(filePrefix, "techSpec1WindBplot.pdf", sep=""),scale=1)

techSpec1BiomassBplot = ggplot(data=tickDF, aes(x=X0, y=meanTechSpecTenderClearingPriceBiomassB)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur/MWh") + 
  ggtitle("Mean tender clearing price Biomass \n Country B") + 
  theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
plot(techSpec1BiomassBplot)
ggsave(filename = paste(filePrefix, "techSpec1BiomassBplot.pdf", sep=""),scale=1)

techSpec1BiogasBplot = ggplot(data=tickDF, aes(x=X0, y=meanTechSpecTenderClearingPriceBiogasB)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur/MWh") + 
  ggtitle("Mean tender clearing price Biogas \n Country B") + 
  theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
plot(techSpec1BiogasBplot)
ggsave(filename = paste(filePrefix, "techSpec1BiogasBplot.pdf", sep=""),scale=1)


# ## Tender clearing prices Tech Spec 2 edition
# tcp2PVA <- tcp2DF %>% 
#   group_by(scheme, price) %>%
#   select(scheme, price, tick) %>%
#   filter(scheme == "RenewableTenderNLphotovoltaicPGT")
# 
# tcp2WindOffA <- tcp2DF %>% 
#   group_by(scheme, price) %>%
#   select(scheme, price, tick) %>%
#   filter(scheme == "RenewableTenderNLwindOffshorePGT")
# 
# tcp2WindA <- tcp2DF %>% 
#   group_by(scheme, price) %>%
#   select(scheme, price, tick) %>%
#   filter(scheme == "RenewableTenderNLwindPGT")
# 
# tcp2BiomassA <- tcp2DF %>% 
#   group_by(scheme, price) %>%
#   select(scheme, price, tick) %>%
#   filter(scheme == "RenewableTenderNLbiomassPGT")
# 
# tcp2BiogasA <- tcp2DF %>% 
#   group_by(scheme, price) %>%
#   select(scheme, price, tick) %>%
#   filter(scheme == "RenewableTenderNLbiogasPGT")
# 
# 
# tcp2PVB <- tcp2DF %>% 
#   group_by(scheme, price) %>%
#   select(scheme, price, tick) %>%
#   filter(scheme == "RenewableTenderDEphotovoltaicPGT")
# 
# tcp2WindOffB <- tcp2DF %>% 
#   group_by(scheme, price) %>%
#   select(scheme, price, tick) %>%
#   filter(scheme == "RenewableTenderDEwindOffshorePGT")
# 
# tcp2WindB <- tcp2DF %>% 
#   group_by(scheme, price) %>%
#   select(scheme, price, tick) %>%
#   filter(scheme == "RenewableTenderDEwindPGT")
# 
# tcp2BiomassB <- tcp2DF %>% 
#   group_by(scheme, price) %>%
#   select(scheme, price, tick) %>%
#   filter(scheme == "RenewableTenderDEbiomassPGT")
# 
# tcp2BiogasB <- tcp2DF %>% 
#   group_by(scheme, price) %>%
#   select(scheme, price, tick) %>%
#   filter(scheme == "RenewableTenderDEbiogasPGT")
# 
# 
# meanTechSpec2TenderClearingPricePVA=0
# meanTechSpec2TenderClearingPriceWindOffshoreA=0
# meanTechSpec2TenderClearingPriceWindA=0
# meanTechSpec2TenderClearingPriceBiomassA=0
# meanTechSpec2TenderClearingPriceBiogasA=0
# meanTechSpec2TenderClearingPricePVB=0
# meanTechSpec2TenderClearingPriceWindOffshoreB=0
# meanTechSpec2TenderClearingPriceWindB=0
# meanTechSpec2TenderClearingPriceBiomassB=0
# meanTechSpec2TenderClearingPriceBiogasB=0
# 
# for(j in 0:39) {
#   meanTechSpec2TenderClearingPricePVA[j] <- mean(subset(tcp2PVA$price, tick == j))
#   meanTechSpec2TenderClearingPriceWindOffshoreA[j] <- mean(subset(tcp2WindOffA$price, tick == j))
#   meanTechSpec2TenderClearingPriceWindA[j] <- mean(subset(tcp2WindA$price, tick == j))
#   meanTechSpec2TenderClearingPriceBiomassA[j] <- mean(subset(tcp2BiomassA$price, tick == j))
#   meanTechSpec2TenderClearingPriceBiogasA[j] <- mean(subset(tcp2BiogasA$price, tick == j))
#   meanTechSpec2TenderClearingPricePVB[j] <- mean(subset(tcp2PVB$price, tick == j))
#   meanTechSpec2TenderClearingPriceWindOffshoreB[j] <- mean(subset(tcp2WindOffB$price, tick == j))
#   meanTechSpec2TenderClearingPriceWindB[j] <- mean(subset(tcp2WindB$price, tick == j))
#   meanTechSpec2TenderClearingPriceBiomassB[j] <- mean(subset(tcp2BiomassB$price, tick == j))
#   meanTechSpec2TenderClearingPriceBiogasB[j] <- mean(subset(tcp2BiogasB$price, tick == j))
# }
# 
# 
# techSpec2PVAplot = ggplot(data=tickDF, aes(x=X0, y=meanTechSpec2TenderClearingPricePVA)) + 
#   geom_point(aes(size=1)) +
#   xlab("Year") +  
#   ylab("Eur/MWh") + 
#   ggtitle("Mean tender clearing price PV \n Country A") + 
#   theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
#         axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
# plot(techSpec2PVAplot)
# ggsave(filename = paste(filePrefix, "techSpec2PVAplot.pdf", sep=""),scale=1)
# 
# techSpec2WindOffAplot = ggplot(data=tickDF, aes(x=X0, y= meanTechSpec2TenderClearingPriceWindOffshoreA)) + 
#   geom_point() +
#   xlab("Year") +  
#   ylab("Eur/MWh") + 
#   ggtitle("Mean tender clearing price WindOff \n Country A") + 
#   theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
#         axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
# plot(techSpec2WindOffAplot)
# ggsave(filename = paste(filePrefix, "techSpec2WindOffAplot.pdf", sep=""),scale=1)
# 
# techSpec2WindAplot = ggplot(data=tickDF, aes(x=X0, y=meanTechSpec2TenderClearingPriceWindA)) + 
#   geom_point() +
#   xlab("Year") +  
#   ylab("Eur/MWh") + 
#   ggtitle("Mean tender clearing price Wind \n Country A") + 
#   theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
#         axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
# plot(techSpec2WindAplot)
# ggsave(filename = paste(filePrefix, "techSpec2WindAplot.pdf", sep=""),scale=1)
# 
# techSpec2BiomassAplot = ggplot(data=tickDF, aes(x=X0, y=meanTechSpec2TenderClearingPriceBiomassA)) + 
#   geom_point() +
#   xlab("Year") +  
#   ylab("Eur/MWh") + 
#   ggtitle("Mean tender clearing price Biomass \n Country A") + 
#   theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
#         axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
# plot(techSpec2BiomassAplot)
# ggsave(filename = paste(filePrefix, "techSpec2BiomassAplot.pdf", sep=""),scale=1)
# 
# techSpec2BiogasAplot = ggplot(data=tickDF, aes(x=X0, y=meanTechSpec2TenderClearingPriceBiogasA)) + 
#   geom_point() +
#   xlab("Year") +  
#   ylab("Eur/MWh") + 
#   ggtitle("Mean tender clearing price Biogas \n Country A") + 
#   theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
#         axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
# plot(techSpec2BiogasAplot)
# ggsave(filename = paste(filePrefix, "techSpec2BiogasAplot.pdf", sep=""),scale=1)
# 
# 
# 
# techSpec2PVBplot = ggplot(data=tickDF, aes(x=X0, y=meanTechSpec2TenderClearingPricePVB)) + 
#   geom_point() +
#   xlab("Year") +  
#   ylab("Eur/MWh") + 
#   ggtitle("Mean tender clearing price PV \n Country B") + 
#   theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
#         axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
# plot(techSpec2PVBplot)
# ggsave(filename = paste(filePrefix, "techSpec2PVBplot.pdf", sep=""),scale=1)
# 
# techSpec2WindOffBplot = ggplot(data=tickDF, aes(x=X0, y= meanTechSpec2TenderClearingPriceWindOffshoreB)) + 
#   geom_point() +
#   xlab("Year") +  
#   ylab("Eur/MWh") + 
#   ggtitle("Mean tender clearing price WindOff \n Country B") + 
#   theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
#         axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
# plot(techSpec2WindOffBplot)
# ggsave(filename = paste(filePrefix, "techSpec2WindOffBplot.pdf", sep=""),scale=1)
# 
# techSpec2WindBplot = ggplot(data=tickDF, aes(x=X0, y=meanTechSpec2TenderClearingPriceWindB)) + 
#   geom_point() +
#   xlab("Year") +  
#   ylab("Eur/MWh") + 
#   ggtitle("Mean tender clearing price Wind \n Country B") + 
#   theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
#         axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
# plot(techSpec2WindBplot)
# ggsave(filename = paste(filePrefix, "techSpec2WindBplot.pdf", sep=""),scale=1)
# 
# techSpec2BiomassBplot = ggplot(data=tickDF, aes(x=X0, y=meanTechSpec2TenderClearingPriceBiomassB)) + 
#   geom_point() +
#   xlab("Year") +  
#   ylab("Eur/MWh") + 
#   ggtitle("Mean tender clearing price Biomass \n Country B") + 
#   theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
#         axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
# plot(techSpec2BiomassBplot)
# ggsave(filename = paste(filePrefix, "techSpec2BiomassBplot.pdf", sep=""),scale=1)
# 
# techSpec2BiogasBplot = ggplot(data=tickDF, aes(x=X0, y=meanTechSpec2TenderClearingPriceBiogasB)) + 
#   geom_point() +
#   xlab("Year") +  
#   ylab("Eur/MWh") + 
#   ggtitle("Mean tender clearing price Biogas \n Country B") + 
#   theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
#         axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
# plot(techSpec2BiogasBplot)
# ggsave(filename = paste(filePrefix, "techSpec2BiogasBplot.pdf", sep=""),scale=1)


# Tender subsidies
meanTotalTenderSubsidyA=0
meanTotalTenderSubsidyB=0


for(j in 0:39) {
  meanTotalTenderSubsidyA[j] <- mean(subset(bigDF$CountryAProdFinances_Tender_Subsidy , tick == j))
  meanTotalTenderSubsidyB[j] <- mean(subset(bigDF$CountryBProdFinances_Tender_Subsidy, tick == j))
}
meanTotalTenderSubsidyA
meanTotalTenderSubsidyB

TotalTenderSubsidyCountryAplot = ggplot(data=tickDF, aes(x=X0, y=meanTotalTenderSubsidyA)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Mean total tender subsidies \n Country A") +   
  theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
plot(TotalTenderSubsidyCountryAplot)
ggsave(filename = paste(filePrefix, "meanTotalTenderSubsidyA.pdf", sep=""),scale=1)

TotalTenderSubsidyCountryBplot = ggplot(data=tickDF, aes(x=X0, y=meanTotalTenderSubsidyB)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Mean total tender subsidies \n Country B") +    
  theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
#plot(TotalTenderSubsidyCountryBplot)
ggsave(filename = paste(filePrefix, "meanTotalTenderSubsidyB.pdf", sep=""),scale=1)


# Relation El price and Tender clearing price

scatterElectricityTenderPriceAplot = ggplot(data=tickDF, aes(x=meanTenderClearingPriceA, y=meanElectricityPriceA)) + 
  geom_point() +
  geom_smooth() +
  xlab("Tender Clearing Price [Eur/MWh]") +  
  ylab("Electricity Price [Eur/MWh]") + 
  ggtitle("Relationship Electricity price versus \n tender clearing price  Country A") +    
  theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
plot(scatterElectricityTenderPriceAplot)
ggsave(filename = paste(filePrefix, "scatterTechSpechElectricityTenderPriceA.pdf", sep=""),scale=1)

scatterElectricityTenderPriceBplot = ggplot(data=tickDF, aes(x=meanTenderClearingPriceB, y=meanElectricityPriceB)) + 
  geom_point() +
  geom_smooth() +
  xlab("Tender Clearing Price [Eur/MWh]") +  
  ylab("Electricity Price [Eur/MWh]") +
  ggtitle("Relationship Electricity price versus \n tender clearing price  Country B") +    
  theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
plot(scatterElectricityTenderPriceBplot)
ggsave(filename = paste(filePrefix, "scatterTechSpecElectricityTenderPriceB.pdf", sep=""),scale=1)

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
  ggtitle("Mean supply ratio \n Country A") +    
  theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
plot(meanSupplyRatioAplot)
ggsave(filename = paste(filePrefix, "meanSupplyRatioA.pdf", sep=""),scale=1)

meanSupplyRatioBplot = ggplot(data=tickDF, aes(x=X0, y=meanSupplyRatioB)) + 
  geom_point() +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Mean supply ratio \n Country B") +    
  theme(plot.title = element_text(size = titleSize),axis.title.x = element_text( size=xTitle), axis.text.x=element_text(size = xAxis), 
        axis.title.y = element_text( size=yTitle), axis.text.y=element_text(size = yAxis))
#plot(meanSupplyRatioBplot)
ggsave(filename = paste(filePrefix, "meanSupplyRatioB.pdf", sep=""),scale=1)

DataTable <- c(format(mean(meanConsumerCostsA), scientific=TRUE))
DataTable <- rbind(DataTable, c(format(mean(meanAggProfitA), scientific=TRUE)))
DataTable <- rbind(DataTable, c(format(mean(meanProducerCashA), scientific=TRUE)))
DataTable <- rbind(DataTable, c(format(mean(meanWelfareLossENSA), scientific=TRUE)))
DataTable <- rbind(DataTable, c(format(mean(meanProducerRevenuesA), scientific=TRUE)))
DataTable <- rbind(DataTable, c(format(mean(meanProducerCostsA), scientific=TRUE)))
DataTable <- rbind(DataTable, c(format(mean(meanGovernmentCostsA), scientific=TRUE)))
DataTable <- rbind(DataTable, c(mean(meanFixedOMCostsA)))
DataTable <- rbind(DataTable, c(mean(meanLoanCostsA)))
DataTable <- rbind(DataTable, c(mean(meanCommodityCostsA)))
DataTable <- rbind(DataTable, c(mean(meanDownpaymentsCostsA)))
DataTable <- rbind(DataTable, c(mean(meanElectricityPriceA)))
DataTable <- rbind(DataTable, c(sd(meanElectricityPriceA)))
DataTable <- rbind(DataTable, c(mean(meanSupplyRatioA)))
DataTable <- rbind(DataTable, c(sd(meanSupplyRatioA)))
DataTable <- rbind(DataTable, c(mean(meanTenderClearingPriceA)))
DataTable <- rbind(DataTable, c(mean(meanTenderClearingPriceB)))
DataTable <- rbind(DataTable, c(format(mean(meanConsumerCostsB), scientific=TRUE)))
DataTable <- rbind(DataTable, c(format(mean(meanAggProfitB), scientific=TRUE)))
DataTable <- rbind(DataTable, c(format(mean(meanProducerCashB), scientific=TRUE)))
DataTable <- rbind(DataTable, c(format(mean(meanWelfareLossENSB), scientific=TRUE)))
DataTable <- rbind(DataTable, c(format(mean(meanProducerRevenuesB), scientific=TRUE)))
DataTable <- rbind(DataTable, c(format(mean(meanProducerCostsB), scientific=TRUE)))
DataTable <- rbind(DataTable, c(format(mean(meanGovernmentCostsB), scientific=TRUE)))
DataTable <- rbind(DataTable, c(mean(meanFixedOMCostsB)))
DataTable <- rbind(DataTable, c(mean(meanLoanCostsB)))
DataTable <- rbind(DataTable, c(mean(meanCommodityCostsB)))
DataTable <- rbind(DataTable, c(mean(meanDownpaymentsCostsB)))
DataTable <- rbind(DataTable, c(mean(meanElectricityPriceB)))
DataTable <- rbind(DataTable, c(sd(meanElectricityPriceB)))
DataTable <- rbind(DataTable, c(mean(meanSupplyRatioB)))
DataTable <- rbind(DataTable, c(sd(meanSupplyRatioB)))
DataTable <- rbind(DataTable, c(mean(meanElectricityPriceA-meanElectricityPriceB)))
DataTable <- rbind(DataTable, c(format(sd(meanConsumerCostsA), scientific=TRUE)))
DataTable <- rbind(DataTable, c(format(sd(meanProducerCostsA), scientific=TRUE)))
DataTable <- rbind(DataTable, c(format(sd(meanGovernmentCostsA), scientific=TRUE)))
DataTable <- rbind(DataTable, c(format(sd(meanConsumerCostsB), scientific=TRUE)))
DataTable <- rbind(DataTable, c(format(sd(meanProducerCostsB), scientific=TRUE)))
DataTable <- rbind(DataTable, c(format(sd(meanGovernmentCostsB), scientific=TRUE)))
DataTable <- rbind(DataTable, c(format(sd(meanAggProfitA), scientific=TRUE)))
DataTable <- rbind(DataTable, c(format(sd(meanAggProfitB), scientific=TRUE)))
DataTable <- rbind(DataTable, c(format(sd(meanProducerCashA), scientific=TRUE)))
DataTable <- rbind(DataTable, c(format(sd(meanProducerCashB), scientific=TRUE)))
DataTable <- rbind(DataTable, c(format(sd(meanWelfareLossENSA), scientific=TRUE)))
DataTable <- rbind(DataTable, c(format(sd(meanWelfareLossENSB), scientific=TRUE)))
DataTable <- rbind(DataTable, c(sd(meanTenderClearingPriceA)))
DataTable <- rbind(DataTable, c(sd(meanTenderClearingPriceB)))
colnames(DataTable) <- c("Value")
rownames(DataTable) <- c("ConsumerCostsA","AggProfitA","ProducerCashA","WelfareLossENSA","ProducerRevenuesA","ProducerCostsA","GovernmentCostsA","FixedOMCostsA","LoanCostsA","CommodityCostsA","DownpaymentsCostsA","ElectricityPriceA","ElectricityPriceA","SupplyRatioA","SupplyRatioA","TenderClearingPriceA","TenderClearingPriceB","ConsumerCostsB","AggProfitB","ProducerCashB","WelfareLossENSB","ProducerRevenuesB","ProducerCostsB","GovernmentCostsB","FixedOMCostsB","LoanCostsB","CommodityCostsB","DownpaymentsCostsB","ElectricityPriceB","ElectricityPriceB","SupplyRatioB","SupplyRatioB","ElectricityPriceAElectricityPriceB","ConsumerCostsA","ProducerCostsA","GovernmentCostsA","ConsumerCostsB","ProducerCostsB","GovernmentCostsB","AggProfitA","AggProfitB","ProducerCashA","ProducerCashB","WelfareLossENSA","WelfareLossENSB","TenderClearingPriceA","TenderClearingPriceB")
write.csv(DataTable, "DataTableAllData.csv")

