setwd("~/emlab-generation/rscripts")
source("rConfig.R")
source("batchRunAnalysis.R")

#File and folder initiation
nameFile <- "PacedTarget"
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
#segmentDF <- read.csv("~/Desktop/emlabGen/output/BaseCase/BaseCase-SegmentClearingPoints.csv")
targetDF <- read.csv("~/Desktop/emlabGen/analysis/R_policyGoalNREAP_NL_DE_2050.csv")

#Adjust 
drops <- c("CapacityinMWinA_CcgtCCS", "CapacityinMWinA_CoalPscCSS","CapacityinMWinA_HydroPower","CapacityinMWinA_IgccCCS","CapacityinMWinA_Igcc")
drops <- c("CapacityinMWinB_CcgtCCS", "CapacityinMWinB_CoalPscCSS","CapacityinMWinB_HydroPower","CapacityinMWinB_IgccCCS","CapacityinMWinA_Igcc")
tick <- bigDF$tick
tick

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
summary(bigDF)

#Melt dataframe
# moltenDF <- melt(bigDF, id.vars = c("runId","tick"))
# moltenDF

library(gridExtra)
library(TeachingDemos)
library(grid)
library(ggplot2)
library(reshape2)

#Multiplot function
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# Check tender clearing prices versus average electricity prices or segment prices

bigDF$diffTenderAndElectricityPriceA <- bigDF$Avg_El_PricesinEURpMWh_Country_A - bigDF$tenderClearingPrice_Country_A
bigDF$diffTenderAndElectricityPriceB <- bigDF$Avg_El_PricesinEURpMWh_Country_B - bigDF$tenderClearingPrice_Country_B

diffTenderAndElectricityPriceAplot = ggplot(data=bigDF, aes(x=tick)) + 
  geom_smooth(aes(y=diffTenderAndElectricityPriceA), method="loess") +  
  xlab("Year") +  
  ylab("Eur/MWh") + 
  ggtitle("Difference Electricity Price - Tender Price \n Country B") #give the plot a title 
plot(diffTenderAndElectricityPriceAplot)
ggsave(filename = paste(filePrefix, "diffTenderAndElectricityPriceABplot.png", sep=""))

diffTenderAndElectricityPriceBplot = ggplot(data=bigDF, aes(x=tick)) + 
  geom_smooth(aes(y=diffTenderAndElectricityPriceAB), method="loess") +  
  xlab("Year") +  
  ylab("Eur/MWh") + 
  ggtitle("Difference Electricity Price - Tender Price \n Country B") #give the plot a title 
plot(diffTenderAndElectricityPriceABplot)
ggsave(filename = paste(filePrefix, "diffTenderAndElectricityPriceABplot.png", sep=""))


# Interconnector
# moltenSegmentDF <- melt(segmentDF, id.vars = c("runId","tick", "interconectorFlow", "segmentID"))
# moltenSegmentDF

# System Costs: fixed and variable costs over the entire simulation period and over all power plants
ConsumerCostsA <- bigDF$ConsumerExpenditure_Country_A_electricity_spot_market

ProducerCostsA <- bigDF$CountryAProdCosts_Fixed_O_M + 
  bigDF$CountryAProdCosts_Loan + 
  bigDF$CountryAProdCosts_Commodity + 
  bigDF$CountryAProdCosts_Downpayment
GovernmentCostsA <- bigDF$CountryAProdFinances_Tender_Subsidy

ConsumerCostsB <- bigDF$ConsumerExpenditure_Country_B_electricity_spot_market 
ProducerCostsB <- bigDF$CountryBProdCosts_Fixed_O_M + 
  bigDF$CountryBProdCosts_Loan + 
  bigDF$CountryBProdCosts_Commodity + 
  bigDF$CountryBProdCosts_Downpayment
GovernmentCostsB <- bigDF$CountryBProdFinances_Tender_Subsidy

SystemCostsA <- ConsumerCostsA + ProducerCostsA + GovernmentCostsA
SystemCostsB <- ConsumerCostsB + ProducerCostsB + GovernmentCostsB

systemCostsOveral <- SystemCostsA + SystemCostsB

consumerCostsAplot = ggplot(data=bigDF, aes(x=tick)) + 
  geom_smooth(aes(y=ConsumerCostsA), method="loess") +  #(aes(colour = runNumber)) + 
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("consumerCosts \n Country A ") #give the plot a title
plot(consumerCostsAplot)


producerCostsAplot = ggplot(data=bigDF, aes(x=tick)) + 
  geom_smooth(aes(y=ProducerCostsA), method="loess") +  #(aes(colour = runNumber)) + 
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("producer costs \n Country A ") #give the plot a title
plot(producerCostsAplot)

governmentCostsAplot = ggplot(data=bigDF, aes(x=tick)) + 
  geom_smooth(aes(y=GovernmentCostsA), method="loess") +  #(aes(colour = runNumber)) + 
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Government Costs \n Country A ") #give the plot a title
plot(governmentCostsAplot)

systemCostsAplot = ggplot(data=bigDF, aes(x=tick)) + 
  geom_smooth(aes(y=SystemCostsA), method="loess") +  #(aes(colour = runNumber)) + 
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("System Costs \n Country A ") #give the plot a title
plot(systemCostsAplot)

consumerCostsBplot = ggplot(data=bigDF, aes(x=tick)) + 
  geom_smooth(aes(y=ConsumerCostsB), method="loess") +  #(aes(colour = runNumber)) + 
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("consumerCosts \n Country B ") #give the plot a title
plot(consumerCostsBplot)

producerCostsBplot = ggplot(data=bigDF, aes(x=tick)) + 
  geom_smooth(aes(y=ProducerCostsB), method="loess") +  #(aes(colour = runNumber)) + 
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("producer costs \n Country B ") #give the plot a title
plot(producerCostsBplot)

governmentCostsBplot = ggplot(data=bigDF, aes(x=tick)) + 
  geom_smooth(aes(y=GovernmentCostsB), method="loess") +  #(aes(colour = runNumber)) + 
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Government Costs \n Country B ") #give the plot a title
plot(governmentCostsBplot)

systemCostsBplot = ggplot(data=bigDF, aes(x=tick)) + 
  geom_smooth(aes(y=SystemCostsB), method="loess") +  #(aes(colour = runNumber)) + 
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("System Costs \n Country B ") #give the plot a title
plot(systemCostsBplot)

systemCostsOveralplot = ggplot(data=bigDF, aes(x=tick)) + 
  geom_smooth(aes(y=systemCostsOveral), method="loess") +  #(aes(colour = runNumber)) + 
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("System Costs Overal ") #give the plot a title
plot(systemCostsOveralplot)
ggsave(filename = paste(filePrefix, "systemCostsOveral.png", sep=""))

meanConsumerCostsA <- mean(ConsumerCostsA)
meanProducerCostsA <- mean(ProducerCostsA)
meanGovernmentCostsA <- mean(GovernmentCostsA)
meanSystemCostsA <- mean(SystemCostsA)

meanConsumerCostsB <- mean(ConsumerCostsB)
meanProducerCostsB <- mean(ProducerCostsB)
meanGovernmentCostsB <- mean(GovernmentCostsB)
meanSystemCostsB <- mean(SystemCostsB)

# fracConsumerCostsA <- ConsumerCostsA / (ConsumerCostsA + ProducerCostsA + GovernmentCostsA) 
# fracProducerCostsA <- ProducerCostsA / (ConsumerCostsA + ProducerCostsA + GovernmentCostsA) 
# fracGovernmentCostsA <- GovernmentCostsA/ (ConsumerCostsA + ProducerCostsA + GovernmentCostsA) 

fracMeanConsumerCostsA <- meanConsumerCostsA*100 / (meanConsumerCostsA + meanProducerCostsA + meanGovernmentCostsA) 
fracMeanProducerCostsA <- meanProducerCostsA*100 / (meanConsumerCostsA + meanProducerCostsA + meanGovernmentCostsA) 
fracMeanGovernmentCostsA <- meanGovernmentCostsA*100/ (meanConsumerCostsA + meanProducerCostsA + meanGovernmentCostsA) 

fracMeanConsumerCostsB <- meanConsumerCostsB*100 / (meanConsumerCostsB + meanProducerCostsB + meanGovernmentCostsB) 
fracMeanProducerCostsB <- meanProducerCostsB*100 / (meanConsumerCostsB + meanProducerCostsB + meanGovernmentCostsB) 
fracMeanGovernmentCostsB <- meanGovernmentCostsB*100/ (meanConsumerCostsB + meanProducerCostsB + meanGovernmentCostsB) 

meanSystemCostsOveral <- mean(systemCostsOveral)
sdSystemCostsOveral <- sd(systemCostsOveral)

DataTable <- c(fracMeanConsumerCostsA)
DataTable <- rbind(DataTable, c(fracMeanProducerCostsA))
DataTable <- rbind(DataTable, c(fracMeanGovernmentCostsA))
DataTable <- rbind(DataTable, c(fracMeanConsumerCostsB))
DataTable <- rbind(DataTable, c(fracMeanProducerCostsB))
DataTable <- rbind(DataTable, c(fracMeanGovernmentCostsB))
DataTable <- rbind(DataTable, c(meanSystemCostsOveral))
DataTable <- rbind(DataTable, c(sdSystemCostsOveral))
colnames(DataTable) <- c(nameFile)
rownames(DataTable) <- c("Percentage Consumer Costs A","Percentage Producer Costs A",
                         "Percentage Government Costs A","Percentage Consumer Costs B","Percentage Producer Costs B",
                         "Percentage Government Costs B", 
                         "Mean Overal System Costs","Standard Deviation Overal System Costs")
write.csv(DataTable, "DataTableSystemCosts.csv")
DataTable


## Producer welfare changes
moltenDFprofitsExcSubProdA <- melt(bigDF, id.vars = "tick", "ProfitProducersYearlyExcludingTenderSubsidy_ProfitProdA")
moltenDFprofitsIncSubProdA <- melt(bigDF, id.vars = "tick", "ProfitProducersYearlyIncludingTenderSubsidy_ProfitProdA")
moltenDFprofitsExcSubProdB <- melt(bigDF, id.vars = "tick", "ProfitProducersYearlyExcludingTenderSubsidy_ProfitProdB")
moltenDFprofitsIncSubProdB <- melt(bigDF, id.vars = "tick", "ProfitProducersYearlyIncludingTenderSubsidy_ProfitProdB")
moltenDFprofitsExcSubProdC <- melt(bigDF, id.vars = "tick", "ProfitProducersYearlyExcludingTenderSubsidy_ProfitProdC")
moltenDFprofitsIncSubProdC <- melt(bigDF, id.vars = "tick", "ProfitProducersYearlyIncludingTenderSubsidy_ProfitProdC")
moltenDFprofitsExcSubProdD <- melt(bigDF, id.vars = "tick", "ProfitProducersYearlyExcludingTenderSubsidy_ProfitProdD")
moltenDFprofitsIncSubProdD <- melt(bigDF, id.vars = "tick", "ProfitProducersYearlyIncludingTenderSubsidy_ProfitProdD")
moltenDFprofitsExcSubProdE <- melt(bigDF, id.vars = "tick", "ProfitProducersYearlyExcludingTenderSubsidy_ProfitProdE")
moltenDFprofitsIncSubProdE <- melt(bigDF, id.vars = "tick", "ProfitProducersYearlyIncludingTenderSubsidy_ProfitProdE")
moltenDFprofitsExcSubProdF <- melt(bigDF, id.vars = "tick", "ProfitProducersYearlyExcludingTenderSubsidy_ProfitProdF")
moltenDFprofitsIncSubProdF <- melt(bigDF, id.vars = "tick", "ProfitProducersYearlyIncludingTenderSubsidy_ProfitProdF")
moltenDFprofitsExcSubProdG <- melt(bigDF, id.vars = "tick", "ProfitProducersYearlyExcludingTenderSubsidy_ProfitProdG")
moltenDFprofitsIncSubProdG <- melt(bigDF, id.vars = "tick", "ProfitProducersYearlyIncludingTenderSubsidy_ProfitProdG")
moltenDFprofitsExcSubProdH <- melt(bigDF, id.vars = "tick", "ProfitProducersYearlyExcludingTenderSubsidy_ProfitProdH")
moltenDFprofitsIncSubProdH <- melt(bigDF, id.vars = "tick", "ProfitProducersYearlyIncludingTenderSubsidy_ProfitProdH")
moltenDFprofitsExcSubProdI <- melt(bigDF, id.vars = "tick", "ProfitProducersYearlyExcludingTenderSubsidy_ProfitProdI")
moltenDFprofitsIncSubProdI <- melt(bigDF, id.vars = "tick", "ProfitProducersYearlyIncludingTenderSubsidy_ProfitProdI")

s1 <- subset(moltenDFprofitsExcSubProdA, tick==0)
s2 <- subset(moltenDFprofitsExcSubProdA, tick==39)
changeProducerExcSubProfitsA <- (s2$value - s1$value)
pcChangeProducerExcSubProfitsA <- changeProducerExcSubProfitsA*100/s1$value
meanPcChangeProducerExcSubProfitsA <- mean(pcChangeProducerExcSubProfitsA)

s1 <- subset(moltenDFprofitsIncSubProdA, tick==0)
s2 <- subset(moltenDFprofitsIncSubProdA, tick==39)
changeProducerIncSubProfitsA <- (s2$value - s1$value)
pcChangeProducerIncSubProfitsA <- changeProducerIncSubProfitsA*100/s1$value
meanPcChangeProducerIncSubProfitsA <- mean(pcChangeProducerIncSubProfitsA)

s1 <- subset(moltenDFprofitsExcSubProdB, tick==0)
s2 <- subset(moltenDFprofitsExcSubProdB, tick==39)
changeProducerExcSubProfitsB <- (s2$value - s1$value)
pcChangeProducerExcSubProfitsB <- changeProducerExcSubProfitsB*100/s1$value
meanPcChangeProducerExcSubProfitsB <- mean(pcChangeProducerExcSubProfitsB)

s1 <- subset(moltenDFprofitsIncSubProdB, tick==0)
s2 <- subset(moltenDFprofitsIncSubProdB, tick==39)
changeProducerIncSubProfitsB <- (s2$value - s1$value)
pcChangeProducerIncSubProfitsB <- changeProducerIncSubProfitsB*100/s1$value
meanPcChangeProducerIncSubProfitsB <- mean(pcChangeProducerIncSubProfitsB)

s1 <- subset(moltenDFprofitsExcSubProdC, tick==0)
s2 <- subset(moltenDFprofitsExcSubProdC, tick==39)
changeProducerExcSubProfitsC <- (s2$value - s1$value)
pcChangeProducerExcSubProfitsC <- changeProducerExcSubProfitsC*100/s1$value
meanPcChangeProducerExcSubProfitsC <- mean(pcChangeProducerExcSubProfitsC)

s1 <- subset(moltenDFprofitsIncSubProdC, tick==0)
s2 <- subset(moltenDFprofitsIncSubProdC, tick==39)
changeProducerIncSubProfitsC <- (s2$value - s1$value)
pcChangeProducerIncSubProfitsC <- changeProducerIncSubProfitsC*100/s1$value
meanPcChangeProducerIncSubProfitsC <- mean(pcChangeProducerIncSubProfitsC)

s1 <- subset(moltenDFprofitsExcSubProdD, tick==0)
s2 <- subset(moltenDFprofitsExcSubProdD, tick==39)
changeProducerExcSubProfitsD <- (s2$value - s1$value)
pcChangeProducerExcSubProfitsD <- changeProducerExcSubProfitsD*100/s1$value
meanPcChangeProducerExcSubProfitsD <- mean(pcChangeProducerExcSubProfitsD)

s1 <- subset(moltenDFprofitsIncSubProdD, tick==0)
s2 <- subset(moltenDFprofitsIncSubProdD, tick==39)
changeProducerIncSubProfitsD <- (s2$value - s1$value)
pcChangeProducerIncSubProfitsD <- changeProducerIncSubProfitsD*100/s1$value
meanPcChangeProducerIncSubProfitsD <- mean(pcChangeProducerIncSubProfitsD)

s1 <- subset(moltenDFprofitsExcSubProdE, tick==0)
s2 <- subset(moltenDFprofitsExcSubProdE, tick==39)
changeProducerExcSubProfitsE <- (s2$value - s1$value)
pcChangeProducerExcSubProfitsE <- changeProducerExcSubProfitsE*100/s1$value
meanPcChangeProducerExcSubProfitsE <- mean(pcChangeProducerExcSubProfitsE)

s1 <- subset(moltenDFprofitsIncSubProdE, tick==0)
s2 <- subset(moltenDFprofitsIncSubProdE, tick==39)
changeProducerIncSubProfitsE <- (s2$value - s1$value)
pcChangeProducerIncSubProfitsE <- changeProducerIncSubProfitsE*100/s1$value
meanPcChangeProducerIncSubProfitsE <- mean(pcChangeProducerIncSubProfitsE)

s1 <- subset(moltenDFprofitsExcSubProdF, tick==0)
s2 <- subset(moltenDFprofitsExcSubProdF, tick==39)
changeProducerExcSubProfitsF <- (s2$value - s1$value)
pcChangeProducerExcSubProfitsF <- changeProducerExcSubProfitsF*100/s1$value
meanPcChangeProducerExcSubProfitsF <- mean(pcChangeProducerExcSubProfitsF)

s1 <- subset(moltenDFprofitsIncSubProdF, tick==0)
s2 <- subset(moltenDFprofitsIncSubProdF, tick==39)
changeProducerIncSubProfitsF <- (s2$value - s1$value)
pcChangeProducerIncSubProfitsF <- changeProducerIncSubProfitsF*100/s1$value
meanPcChangeProducerIncSubProfitsF <- mean(pcChangeProducerIncSubProfitsF)

s1 <- subset(moltenDFprofitsExcSubProdG, tick==0)
s2 <- subset(moltenDFprofitsExcSubProdG, tick==39)
changeProducerExcSubProfitsG <- (s2$value - s1$value)
pcChangeProducerExcSubProfitsG <- changeProducerExcSubProfitsG*100/s1$value
meanPcChangeProducerExcSubProfitsG <- mean(pcChangeProducerExcSubProfitsG)

s1 <- subset(moltenDFprofitsIncSubProdG, tick==0)
s2 <- subset(moltenDFprofitsIncSubProdG, tick==39)
changeProducerIncSubProfitsG <- (s2$value - s1$value)
pcChangeProducerIncSubProfitsG <- changeProducerIncSubProfitsG*100/s1$value
meanPcChangeProducerIncSubProfitsG <- mean(pcChangeProducerIncSubProfitsG)

s1 <- subset(moltenDFprofitsExcSubProdH, tick==0)
s2 <- subset(moltenDFprofitsExcSubProdH, tick==39)
changeProducerExcSubProfitsH <- (s2$value - s1$value)
pcChangeProducerExcSubProfitsH <- changeProducerExcSubProfitsH*100/s1$value
meanPcChangeProducerExcSubProfitsH <- mean(pcChangeProducerExcSubProfitsH)

s1 <- subset(moltenDFprofitsIncSubProdH, tick==0)
s2 <- subset(moltenDFprofitsIncSubProdH, tick==39)
changeProducerIncSubProfitsH <- (s2$value - s1$value)
pcChangeProducerIncSubProfitsH <- changeProducerIncSubProfitsH*100/s1$value
meanPcChangeProducerIncSubProfitsH <- mean(pcChangeProducerIncSubProfitsH)

s1 <- subset(moltenDFprofitsExcSubProdI, tick==0)
s2 <- subset(moltenDFprofitsExcSubProdI, tick==39)
changeProducerExcSubProfitsI <- (s2$value - s1$value)
pcChangeProducerExcSubProfitsI <- changeProducerExcSubProfitsI*100/s1$value
meanPcChangeProducerExcSubProfitsI <- mean(pcChangeProducerExcSubProfitsI)

s1 <- subset(moltenDFprofitsIncSubProdI, tick==0)
s2 <- subset(moltenDFprofitsIncSubProdI, tick==39)
changeProducerIncSubProfitsI <- (s2$value - s1$value)
pcChangeProducerIncSubProfitsI <- changeProducerIncSubProfitsI*100/s1$value
meanPcChangeProducerIncSubProfitsI <- mean(pcChangeProducerIncSubProfitsI)

DataTable <- c(meanPcChangeProducerExcSubProfitsA)
DataTable <- rbind(DataTable, c(meanPcChangeProducerIncSubProfitsA))
DataTable <- rbind(DataTable, c(meanPcChangeProducerExcSubProfitsB))
DataTable <- rbind(DataTable, c(meanPcChangeProducerIncSubProfitsB))
DataTable <- rbind(DataTable, c(meanPcChangeProducerExcSubProfitsC))
DataTable <- rbind(DataTable, c(meanPcChangeProducerIncSubProfitsC))
DataTable <- rbind(DataTable, c(meanPcChangeProducerExcSubProfitsD))
DataTable <- rbind(DataTable, c(meanPcChangeProducerIncSubProfitsD))
DataTable <- rbind(DataTable, c(meanPcChangeProducerExcSubProfitsE))
DataTable <- rbind(DataTable, c(meanPcChangeProducerIncSubProfitsE))
DataTable <- rbind(DataTable, c(meanPcChangeProducerExcSubProfitsF))
DataTable <- rbind(DataTable, c(meanPcChangeProducerIncSubProfitsF))
DataTable <- rbind(DataTable, c(meanPcChangeProducerExcSubProfitsG))
DataTable <- rbind(DataTable, c(meanPcChangeProducerExcSubProfitsG))
DataTable <- rbind(DataTable, c(meanPcChangeProducerExcSubProfitsH))
DataTable <- rbind(DataTable, c(meanPcChangeProducerExcSubProfitsH))
DataTable <- rbind(DataTable, c(meanPcChangeProducerExcSubProfitsI))
DataTable <- rbind(DataTable, c(meanPcChangeProducerExcSubProfitsI))
colnames(DataTable) <- c(nameFile)
rownames(DataTable) <- c("Change Welfare Producer A ExcSub","Change Welfare Producer A IncSub",
                         "Change Welfare Producer B ExcSub","Change Welfare Producer B IncSub",
                         "Change Welfare Producer C ExcSub","Change Welfare Producer C IncSub",
                         "Change Welfare Producer D ExcSub","Change Welfare Producer D IncSub",
                         "Change Welfare Producer E ExcSub","Change Welfare Producer E IncSub",
                         "Change Welfare Producer F ExcSub","Change Welfare Producer F IncSub",
                         "Change Welfare Producer G ExcSub","Change Welfare Producer G IncSub",
                         "Change Welfare Producer H ExcSub","Change Welfare Producer H IncSub",
                         "Change Welfare Producer I ExcSub","Change Welfare Producer I IncSub")
write.csv(DataTable, "DataTableProdWelfareChange.csv")
DataTable

profitsExcSubProdA = ggplot(data=moltenDFprofitsExcSubProdA, aes(x=tick)) + 
  geom_smooth(aes(y=value), colour="green", method = "loess") +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Profits exc subsidy Producer A") #give the plot a title
plot(profitsExcSubProdA)

profitsIncSubProdA = ggplot(data=moltenDFprofitsIncSubProdA, aes(x=tick)) + 
  geom_smooth(aes(y=value), colour="red", method = "loess") + 
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Profits inc subsidy Producer A") #give the plot a title
plot(profitsIncSubProdA)

profitsExcSubProdB = ggplot(data=moltenDFprofitsExcSubProdB, aes(x=tick)) + 
  geom_smooth(aes(y=value), colour="green", method = "loess") +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Profits exc subsidy Producer B") #give the plot a title
plot(profitsExcSubProdB)

profitsIncSubProdB = ggplot(data=moltenDFprofitsIncSubProdB, aes(x=tick)) + 
  geom_smooth(aes(y=value), colour="red", method = "loess") + 
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Profits inc subsidy Producer B") #give the plot a title
plot(profitsIncSubProdB)

profitsExcSubProdC = ggplot(data=moltenDFprofitsExcSubProdC, aes(x=tick)) + 
  geom_smooth(aes(y=value), colour="green", method = "loess") +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Profits exc subsidy Producer C") #give the plot a title
plot(profitsExcSubProdC)

profitsIncSubProdC = ggplot(data=moltenDFprofitsIncSubProdC, aes(x=tick)) + 
  geom_smooth(aes(y=value), colour="red", method = "loess") + 
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Profits inc subsidy Producer C") #give the plot a title
plot(profitsIncSubProdC)

profitsExcSubProdD = ggplot(data=moltenDFprofitsExcSubProdD, aes(x=tick)) + 
  geom_smooth(aes(y=value), colour="green", method = "loess") +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Profits exc subsidy Producer D") #give the plot a title
plot(profitsExcSubProdD)

profitsIncSubProdD = ggplot(data=moltenDFprofitsIncSubProdD, aes(x=tick)) + 
  geom_smooth(aes(y=value), colour="red", method = "loess") + 
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Profits inc subsidy Producer D") #give the plot a title
plot(profitsIncSubProdD)

profitsExcSubProdE = ggplot(data=moltenDFprofitsExcSubProdE, aes(x=tick)) + 
  geom_smooth(aes(y=value), colour="green", method = "loess") +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Profits exc subsidy Producer E") #give the plot a title
plot(profitsExcSubProdE)

profitsIncSubProdE = ggplot(data=moltenDFprofitsIncSubProdE, aes(x=tick)) + 
  geom_smooth(aes(y=value), colour="red", method = "loess") + 
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Profits inc subsidy Producer E") #give the plot a title
plot(profitsIncSubProdE)

profitsExcSubProdF = ggplot(data=moltenDFprofitsExcSubProdF, aes(x=tick)) + 
  geom_smooth(aes(y=value), colour="green", method = "loess") +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Profits exc subsidy Producer F") #give the plot a title
plot(profitsExcSubProdF)

profitsIncSubProdF = ggplot(data=moltenDFprofitsIncSubProdF, aes(x=tick)) + 
  geom_smooth(aes(y=value), colour="red", method = "loess") + 
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Profits inc subsidy Producer F") #give the plot a title
plot(profitsIncSubProdF)

profitsExcSubProdG = ggplot(data=moltenDFprofitsExcSubProdG, aes(x=tick)) + 
  geom_smooth(aes(y=value), colour="green", method = "loess") +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Profits exc subsidy Producer G") #give the plot a title
plot(profitsExcSubProdG)

profitsIncSubProdG = ggplot(data=moltenDFprofitsIncSubProdG, aes(x=tick)) + 
  geom_smooth(aes(y=value), colour="red", method = "loess") + 
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Profits inc subsidy Producer G") #give the plot a title
plot(profitsIncSubProdG)

profitsExcSubProdH = ggplot(data=moltenDFprofitsExcSubProdH, aes(x=tick)) + 
  geom_smooth(aes(y=value), colour="green", method = "loess") +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Profits exc subsidy Producer H") #give the plot a title
plot(profitsExcSubProdH)

profitsIncSubProdH = ggplot(data=moltenDFprofitsIncSubProdH, aes(x=tick)) + 
  geom_smooth(aes(y=value), colour="red", method = "loess") + 
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Profits inc subsidy Producer H") #give the plot a title
plot(profitsIncSubProdH)

profitsExcSubProdI = ggplot(data=moltenDFprofitsExcSubProdI, aes(x=tick)) + 
  geom_smooth(aes(y=value), colour="green", method = "loess") +
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Profits exc subsidy Producer I") #give the plot a title
plot(profitsExcSubProdI)

profitsIncSubProdI = ggplot(data=moltenDFprofitsIncSubProdI, aes(x=tick)) + 
  geom_smooth(aes(y=value), colour="red", method = "loess") + 
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Profits inc subsidy Producer I") #give the plot a title
plot(profitsIncSubProdI)

#Consumer Welfare = Difference in consumer expenditure at the beginning and end of simulation
moltenDFconsumerExpenditureA <- melt(bigDF, id.vars = "tick", "ConsumerExpenditure_Country_A_electricity_spot_market")
moltenDFconsumerExpenditureB <- melt(bigDF, id.vars = "tick", "ConsumerExpenditure_Country_B_electricity_spot_market")

s1 <- subset(moltenDFconsumerExpenditureA , tick==0)
s2 <- subset(moltenDFconsumerExpenditureA , tick==39)
changeConsumerExpenditureA <- (s2$value - s1$value)
pcChangeConsumerExpenditureA <- changeConsumerExpenditureA*100/s1$value
meanPcChangeConsumerExpenditureA <- mean(pcChangeConsumerExpenditureA)

s1 <- subset(moltenDFconsumerExpenditureB , tick==0)
s2 <- subset(moltenDFconsumerExpenditureB , tick==39)
changeConsumerExpenditureB <- (s2$value - s1$value)
pcChangeConsumerExpenditureB <- changeConsumerExpenditureB*100/s1$value
meanPcChangeConsumerExpenditureB <- mean(pcChangeConsumerExpenditureB)

DataTable <- c(meanPcChangeConsumerExpenditureA)
DataTable <- rbind(DataTable, c(meanPcChangeConsumerExpenditureB))
colnames(DataTable) <- c(nameFile)
rownames(DataTable) <- c("Change Welfare Consumer Country A","Change Welfare Consumer Country B")
write.csv(DataTable, "DataTableConsumerWelfareChange.csv")
DataTable

consumerExpenditureAplot = ggplot(data=moltenDFconsumerExpenditureA, aes(x=tick)) + 
  geom_smooth(aes(y=value), colour="red", method = "loess") + 
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Consumer expenditure \n Country A") #give the plot a title
plot(consumerExpenditureAplot)

consumerExpenditureBplot = ggplot(data=moltenDFconsumerExpenditureB, aes(x=tick)) + 
  geom_smooth(aes(y=value), colour="red", method = "loess") + 
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Consumer expenditure \n Country B") #give the plot a title
plot(consumerExpenditureBplot)

## Welfare now?

## Expected Gen check
ExpGenA <- bigDF$ExpectedRenewableGeneration_RenewableTenderDE
ExpGenB <- bigDF$ExpectedRenewableGeneration_RenewableTenderNL

write.table(ExpGenA , file = "ExpGenA.csv",row.names=FALSE, na="",col.names=FALSE, sep=",")
write.table(ExpGenB, file = "ExpGenB.csv",row.names=FALSE, na="",col.names=FALSE, sep=",")

## Generation shares per technology
generationSharePVA <- bigDF$GenerationinMWhCountryA_Photovoltaic / bigDF$NationalTotalProductioninMWh_Country_A

generationSharePVAplot = ggplot(data=bigDF, aes(x=tick)) + 
  geom_smooth(aes(y=generationSharePVA), colour="red", method = "loess") + 
  xlab("Year") +  
  ylab("Fraction") + 
  ggtitle("generationSharePV  \n Country A") #give the plot a title
plot(generationSharePVAplot)
ggsave(filename = paste(filePrefix, "generationSharePVAplot.png", sep=""))

generationShareWindA <- bigDF$GenerationinMWhCountryA_Wind / bigDF$NationalTotalProductioninMWh_Country_A

generationShareWindAplot = ggplot(data=bigDF, aes(x=tick)) + 
  geom_smooth(aes(y=generationShareWindA), colour="red", method = "loess") + 
  xlab("Year") +  
  ylab("Fraction") + 
  ggtitle("generation share Wind  \n Country A") #give the plot a title
plot(generationShareWindAplot)
ggsave(filename = paste(filePrefix, "generationShareWindAplot.png", sep=""))

generationShareWindOffshoreA <- bigDF$GenerationinMWhCountryA_WindOffshore / bigDF$NationalTotalProductioninMWh_Country_A

generationShareWindOffshoreAplot = ggplot(data=bigDF, aes(x=tick)) + 
  geom_smooth(aes(y=generationShareWindOffshoreA), colour="red", method = "loess") + 
  xlab("Year") +  
  ylab("Fraction") + 
  ggtitle("generation share Wind Offshore  \n Country A") #give the plot a title
plot(generationShareWindOffshoreAplot)
ggsave(filename = paste(filePrefix, "generationShareWindOffshoreAplot.png", sep=""))

generationShareBiomassA <- bigDF$GenerationinMWhCountryA_Biomass / bigDF$NationalTotalProductioninMWh_Country_A

generationShareBiomassAplot = ggplot(data=bigDF, aes(x=tick)) + 
  geom_smooth(aes(y=generationShareBiomassA), colour="red", method = "loess") + 
  xlab("Year") +  
  ylab("Fraction") + 
  ggtitle("generation share Biomass  \n Country A") #give the plot a title
plot(generationShareBiomassAplot)
ggsave(filename = paste(filePrefix, "generationShareBiomassAplot.png", sep=""))

generationShareBiogasA <- bigDF$GenerationinMWhCountryA_Biogas / bigDF$NationalTotalProductioninMWh_Country_A

generationShareBiogasAplot = ggplot(data=bigDF, aes(x=tick)) + 
  geom_smooth(aes(y=generationShareBiogasA), colour="red", method = "loess") + 
  xlab("Year") +  
  ylab("Fraction") + 
  ggtitle("generation share Biogas  \n Country A") #give the plot a title
plot(generationShareBiogasAplot)
ggsave(filename = paste(filePrefix, "generationShareBiogasAplot.png", sep=""))

generationShareCoalPSCA <- bigDF$GenerationinMWhCountryA_CoalPSC / bigDF$NationalTotalProductioninMWh_Country_A

generationShareCoalPSCAplot = ggplot(data=bigDF, aes(x=tick)) + 
  geom_smooth(aes(y=generationShareCoalPSCA), colour="red", method = "loess") + 
  xlab("Year") +  
  ylab("Fraction") + 
  ggtitle("generation share Coal PSC  \n Country A") #give the plot a title
plot(generationSharePVAplot)
ggsave(filename = paste(filePrefix, "generationShareCoalPSCAplot.png", sep=""))

generationShareLigniteA <- bigDF$GenerationinMWhCountryA_Lignite / bigDF$NationalTotalProductioninMWh_Country_A

generationShareLigniteAplot = ggplot(data=bigDF, aes(x=tick)) + 
  geom_smooth(aes(y=generationShareLigniteA), colour="red", method = "loess") + 
  xlab("Year") +  
  ylab("Fraction") + 
  ggtitle("generation share Lignite A  \n Country A") #give the plot a title
plot(generationShareLigniteAplot)
ggsave(filename = paste(filePrefix, "generationShareLigniteAplot.png", sep=""))

generationShareOCGTA <- bigDF$GenerationinMWhCountryA_OCGT / bigDF$NationalTotalProductioninMWh_Country_A

generationShareOCGTAplot = ggplot(data=bigDF, aes(x=tick)) + 
  geom_smooth(aes(y=generationShareOCGTA), colour="red", method = "loess") + 
  xlab("Year") +  
  ylab("Fraction") + 
  ggtitle("generation share OCGT  \n Country A") #give the plot a title
plot(generationShareOCGTAplot)
ggsave(filename = paste(filePrefix, "generationShareOCGTAplot.png", sep=""))

generationShareCCGTA <- bigDF$GenerationinMWhCountryA_CCGT / bigDF$NationalTotalProductioninMWh_Country_A

generationShareCCGTAplot = ggplot(data=bigDF, aes(x=tick)) + 
  geom_smooth(aes(y=generationShareCCGTA), colour="red", method = "loess") + 
  xlab("Year") +  
  ylab("Fraction") + 
  ggtitle("generation share CCGT  \n Country A") #give the plot a title
plot(generationShareCCGTAplot)
ggsave(filename = paste(filePrefix, "generationShareCCGTAplot.png", sep=""))

generationShareNuclearA <- bigDF$GenerationinMWhCountryA_Nuclear / bigDF$NationalTotalProductioninMWh_Country_A

generationShareNuclearAplot = ggplot(data=bigDF, aes(x=tick)) + 
  geom_smooth(aes(y=generationShareNuclearA), colour="red", method = "loess") + 
  xlab("Year") +  
  ylab("Fraction") + 
  ggtitle("generation share Nuclear  \n Country A") #give the plot a title
plot(generationShareNuclearAplot)
ggsave(filename = paste(filePrefix, "generationShareNuclearAplot.png", sep=""))



generationSharePVBplot = ggplot(data=bigDF, aes(x=tick)) + 
  geom_smooth(aes(y=generationSharePVB), colour="red", method = "loess") + 
  xlab("Year") +  
  ylab("Fraction") + 
  ggtitle("generationSharePV  \n Country B") #give the plot a title
plot(generationSharePVBplot)
ggsave(filename = paste(filePrefix, "generationSharePVBplot.png", sep=""))

generationShareWindB <- bigDF$GenerationinMWhCountryB_Wind / bigDF$NationalTotalProductioninMWh_Country_B

generationShareWindBplot = ggplot(data=bigDF, aes(x=tick)) + 
  geom_smooth(aes(y=generationShareWindB), colour="red", method = "loess") + 
  xlab("Year") +  
  ylab("Fraction") + 
  ggtitle("generation share Wind  \n Country B") #give the plot a title
plot(generationShareWindBplot)
ggsave(filename = paste(filePrefix, "generationShareWindBplot.png", sep=""))

generationShareWindOffshoreB <- bigDF$GenerationinMWhCountryB_WindOffshore / bigDF$NationalTotalProductioninMWh_Country_B

generationShareWindOffshoreBplot = ggplot(data=bigDF, aes(x=tick)) + 
  geom_smooth(aes(y=generationShareWindOffshoreB), colour="red", method = "loess") + 
  xlab("Year") +  
  ylab("Fraction") + 
  ggtitle("generation share Wind Offshore  \n Country B") #give the plot a title
plot(generationShareWindOffshoreBplot)
ggsave(filename = paste(filePrefix, "generationShareWindOffshoreBplot.png", sep=""))

generationShareBiomassB <- bigDF$GenerationinMWhCountryB_Biomass / bigDF$NationalTotalProductioninMWh_Country_B

generationShareBiomassBplot = ggplot(data=bigDF, aes(x=tick)) + 
  geom_smooth(aes(y=generationShareBiomassB), colour="red", method = "loess") + 
  xlab("Year") +  
  ylab("Fraction") + 
  ggtitle("generation share Biomass  \n Country B") #give the plot a title
plot(generationShareBiomassBplot)
ggsave(filename = paste(filePrefix, "generationShareBiomassBplot.png", sep=""))

generationShareBiogasB <- bigDF$GenerationinMWhCountryB_Biogas / bigDF$NationalTotalProductioninMWh_Country_B

generationShareBiogasBplot = ggplot(data=bigDF, aes(x=tick)) + 
  geom_smooth(aes(y=generationShareBiogasB), colour="red", method = "loess") + 
  xlab("Year") +  
  ylab("Fraction") + 
  ggtitle("generation share Biogas  \n Country B") #give the plot a title
plot(generationShareBiogasBplot)
ggsave(filename = paste(filePrefix, "generationShareBiogasBplot.png", sep=""))

generationShareCoalPSCB <- bigDF$GenerationinMWhCountryB_CoalPSC / bigDF$NationalTotalProductioninMWh_Country_B

generationShareCoalPSCBplot = ggplot(data=bigDF, aes(x=tick)) + 
  geom_smooth(aes(y=generationShareCoalPSCB), colour="red", method = "loess") + 
  xlab("Year") +  
  ylab("Fraction") + 
  ggtitle("generation share Coal PSC  \n Country B") #give the plot a title
plot(generationSharePVBplot)
ggsave(filename = paste(filePrefix, "generationShareCoalPSCBplot.png", sep=""))

generationShareLigniteB <- bigDF$GenerationinMWhCountryB_Lignite / bigDF$NationalTotalProductioninMWh_Country_B

generationShareLigniteBplot = ggplot(data=bigDF, aes(x=tick)) + 
  geom_smooth(aes(y=generationShareLigniteB), colour="red", method = "loess") + 
  xlab("Year") +  
  ylab("Fraction") + 
  ggtitle("generation share Lignite B  \n Country B") #give the plot a title
plot(generationShareLigniteBplot)
ggsave(filename = paste(filePrefix, "generationShareLigniteBplot.png", sep=""))

generationShareOCGTB <- bigDF$GenerationinMWhCountryB_OCGT / bigDF$NationalTotalProductioninMWh_Country_B

generationShareOCGTBplot = ggplot(data=bigDF, aes(x=tick)) + 
  geom_smooth(aes(y=generationShareOCGTB), colour="red", method = "loess") + 
  xlab("Year") +  
  ylab("Fraction") + 
  ggtitle("generation share OCGT  \n Country B") #give the plot a title
plot(generationShareOCGTBplot)
ggsave(filename = paste(filePrefix, "generationShareOCGTBplot.png", sep=""))

generationShareCCGTB <- bigDF$GenerationinMWhCountryB_CCGT / bigDF$NationalTotalProductioninMWh_Country_B

generationShareCCGTBplot = ggplot(data=bigDF, aes(x=tick)) + 
  geom_smooth(aes(y=generationShareCCGTB), colour="red", method = "loess") + 
  xlab("Year") +  
  ylab("Fraction") + 
  ggtitle("generation share CCGT  \n Country B") #give the plot a title
plot(generationShareCCGTBplot)
ggsave(filename = paste(filePrefix, "generationShareCCGTBplot.png", sep=""))

generationShareNuclearB <- bigDF$GenerationinMWhCountryB_Nuclear / bigDF$NationalTotalProductioninMWh_Country_B

generationShareNuclearBplot = ggplot(data=bigDF, aes(x=tick)) + 
  geom_smooth(aes(y=generationShareNuclearB), colour="red", method = "loess") + 
  xlab("Year") +  
  ylab("Fraction") + 
  ggtitle("generation share Nuclear  \n Country B") #give the plot a title
plot(generationShareNuclearBplot)
ggsave(filename = paste(filePrefix, "generationShareNuclearBplot.png", sep=""))

## Renewable generation in terms of demand (compare this with NREAP targets)
renewableGenerationA <-  
  bigDF$GenerationinMWhCountryA_Photovoltaic + bigDF$GenerationinMWhCountryA_Wind + bigDF$GenerationinMWhCountryA_Biomass + 
  bigDF$GenerationinMWhCountryA_HydroPower + bigDF$GenerationinMWhCountryA_Biogas + bigDF$GenerationinMWhCountryA_WindOffshore 

renewableGenerationOverTotalGeneration <- renewableGenerationA / bigDF$NationalTotalProductioninMWh_Country_A

demandA <- bigDF$Total_DemandinMWh_Country_A
realizedTargetA <- renewableGenerationA / demandA 

realizedTargetAplot = ggplot(data=bigDF, aes(x=tick, y=realizedTargetA*100)) + 
  geom_smooth() + 
  xlab("Year") +  
  ylab("(%)") + 
  ggtitle("RES-E generation in terms of demand \n Country A") #give the plot a title
plot(realizedTargetAplot)
ggsave(filename = paste(filePrefix, "realizedTargetAplot.png", sep=""),scale=1)

nreapAplot = ggplot(data=targetDF, aes(x=tick, y=nl_target*100)) + 
  geom_smooth() + 
  xlab("Year") +  
  ylab("(%)") + 
  ggtitle("NREAP Target \n Country A") #give the plot a title
plot(nreapAplot)
ggsave(filename = paste(filePrefix, "NREAP_target_nl.png", sep=""),scale=1)

# renewableShareVStargetA <-multiplot(RESgenerationShareAplot, nreapAplot, cols=2)
# ggsave(filename = paste(filePrefix, "renewableShareVStargetA.png",  sep=""),scale=1)

renewableGenerationB <-  
  bigDF$GenerationinMWhCountryB_Photovoltaic + bigDF$GenerationinMWhCountryB_Wind + bigDF$GenerationinMWhCountryB_Biomass + 
  bigDF$GenerationinMWhCountryB_HydroPower + bigDF$GenerationinMWhCountryB_Biogas + bigDF$GenerationinMWhCountryB_WindOffshore 

demandB <- bigDF$Total_DemandinMWh_Country_B
realizedTargetB <- renewableGenerationB / demandB 
realizedTargetB 

realizedTargetBplot = ggplot(data=bigDF, aes(x=tick, y=realizedTargetB*100)) + 
  geom_smooth() + 
  xlab("Year") +  
  ylab("(%)") + 
  ggtitle("RES-E generation in terms of demand \n Country B") #give the plot a title
plot(realizedTargetBplot)
ggsave(filename = paste(filePrefix, "realizedTargetBplot.png", sep=""),scale=1)

nreapBplot = ggplot(data=targetDF, aes(x=tick, y=de_target*100)) + 
  geom_smooth() + 
  xlab("Year") +  
  ylab("(%)") + 
  ggtitle("NREAP Target \n Country B") #give the plot a title
plot(nreapBplot)
ggsave(filename = paste(filePrefix, "NREAP_target_de.png", sep=""),scale=1)

write.table(renewableGenerationA , file = "renewableGenerationA.csv",row.names=FALSE, na="",col.names=FALSE, sep=",")
write.table(renewableGenerationB, file = "renewableGenerationB.csv",row.names=FALSE, na="",col.names=FALSE, sep=",")



# Average electricity wholesale price in country
AverageElectricityPriceCountryAplot = ggplot(data=bigDF, aes(x=tick,y=Avg_El_PricesinEURpMWh_Country_A)) + 
  geom_smooth() +
  xlab("Year") +  
 ylab("Eur/MWh") + 
  ggtitle("Average Electricity Prices \n  Country A") #give the plot a title
plot(AverageElectricityPriceCountryAplot)
ggsave(filename = paste(filePrefix, "Avg_el_price_A.png", sep=""))

AverageElectricityPriceCountryBplot = ggplot(data=bigDF, aes(x=tick, y=Avg_El_PricesinEURpMWh_Country_B)) + 
  geom_smooth() +  #(aes(colour = runNumber)) + 
  xlab("Year") +  
  ylab("Eur/MWh") + 
  ggtitle("Average Electricity Prices \n Country B") #give the plot a title
plot(AverageElectricityPriceCountryBplot)
ggsave(filename = paste(filePrefix, "Avg_el_price_B.png", sep=""))

# ElectricityPricesAB <-multiplot(AverageElectricityPriceCountryAplot, AverageElectricityPriceCountryBplot, diff_el_price_AB, cols=2)
# ggsave(filename = paste(filePrefix, "ElectricityPriceAverageNL_DE.png", sep=""))

bigDF$diffPriceAB <- bigDF$Avg_El_PricesinEURpMWh_Country_A - bigDF$Avg_El_PricesinEURpMWh_Country_B
diff_el_price_AB = ggplot(data=bigDF , aes(x=tick, y=diffPriceAB)) + 
  geom_smooth() +  #(aes(colour = runNumber))
  xlab("Year") +  
  ylab("Eur/MWh") + 
  ggtitle("Difference Electricity Price \n Country A - Country B") #give the plot a title
plot(diff_el_price_AB)
ggsave(filename = paste(filePrefix, "Diff_ElectricityPriceAverageNL_DE.png", sep=""))


#Tender Clearing Prices
tenderClearingPriceCountryAplot = ggplot(data=bigDF, aes(x=tick, y=tenderClearingPrice_Country_A)) + 
  geom_point() +  #(aes(colour = runId)) + 
  xlab("Year") +  
  ylab("Eur/MWh") + 
  ggtitle("Tender Clearing Prices \n Country A") #give the plot a title
plot(tenderClearingPriceCountryAplot)
ggsave(filename = paste(filePrefix, "tender_clearing_price_A.png", sep=""))

tenderClearingPriceCountryBplot = ggplot(data=bigDF, aes(x=tick, y=tenderClearingPrice_Country_B)) + 
  geom_line() +  #(aes(colour = runNumber)) + 
  xlab("Year") +  
  ylab("Eur/MWh") + 
  ggtitle("Tender Clearing Prices \n Country B") #give the plot a title
plot(tenderClearingPriceCountryBplot)
ggsave(filename = paste(filePrefix, "tender_clearing_price_B.png", sep=""))


# #Tender Clearing Volumes
# tenderClearingVolumeCountryAplot = ggplot(data=bigDF, aes(x=tick, y=tenderClearingVolume_Country_A)) + 
#   geom_points() +  #(aes(colour = runNumber)) + 
#   xlab("Year") +  
#   ylab("MWh") + 
#   ggtitle("Tender Clearing Volumes \n Country A") #give the plot a title
# plot(tenderClearingVolumeCountryAplot)
# ggsave(filename = paste(filePrefix, "tender_clearing_volume_A.png", sep=""))
# 
# tenderClearingVolumeCountryBplot = ggplot(data=bigDF, aes(x=tick, y=tenderClearingVolume_Country_B, group=runNumber)) + 
#   geom_line() +  (aes(colour = runNumber)) + 
#   xlab("Year") +  
#   ylab("MWh") + 
#   ggtitle("Tender Clearing Volumes \n Country B") #give the plot a title
# plot(tenderClearingVolumeCountryBplot)
# ggsave(filename = paste(filePrefix, "tender_clearing_volume_B.png", sep=""))
# 
# 
# #Tender Clearing price*volume
# tenderClearingMoneyA <- bigDF$tenderClearingPrice_Country_A * bigDF$tenderClearingVolume_Country_A
# tenderClearingMoneyB <- bigDF$tenderClearingPrice_Country_B * bigDF$tenderClearingVolume_Country_B

# write.table(tenderClearingMoneyA, file = "tenderClearingMoneyA.csv",row.names=FALSE, na="",col.names=FALSE, sep=",")
# write.table(tenderClearingMoneyB, file = "tenderClearingMoneyB.csv",row.names=FALSE, na="",col.names=FALSE, sep=",")



# #CHANGE IN System Costs 
# s1 <- subset(bigDF, tick==1)
# s2 <- subset(bigDF, tick==39)
# changeConsumerCostsA <- (s2$ConsumerExpenditure_Country_A_electricity_spot_market - s1$ConsumerExpenditure_Country_A_electricity_spot_market)
# pcChangeConsumerCostsA <- changeConsumerCostsA*100/s1$ConsumerExpenditure_Country_A_electricity_spot_market
# meanPcChangeConsumerCostsA <- mean(pcChangeConsumerCostsA)
# 
# changeProducerCostsA <- ((s2$CountryAProdCosts_Fixed_O_M + 
#                            s2$CountryAProdCosts_Loan + 
#                            s2$CountryAProdCosts_Commodity + 
#                            s2$CountryAProdCosts_Downpayment) - (s1$CountryAProdCosts_Fixed_O_M + 
#                            s1$CountryAProdCosts_Loan + 
#                            s1$CountryAProdCosts_Commodity + 
#                            s1$CountryAProdCosts_Downpayment))
# pcChangeProducerCostsA <- changeProducerCostsA*100/(s1$CountryAProdCosts_Fixed_O_M + 
#   s1$CountryAProdCosts_Loan + 
#   s1$CountryAProdCosts_Commodity + 
#   s1$CountryAProdCosts_Downpayment)
# meanPcChangeProducerCostsA <- mean(pcChangeProducerCostsA)
# 
# changeGovernmentCostsA <- (s2$CountryAProdFinances_Tender_Subsidy - s1$CountryAProdFinances_Tender_Subsidy)
# pcChangeGovernmentCostsA <- (changeGovernmentCostsA*100/s1$CountryAProdFinances_Tender_Subsidy)



#supply ratio
# An indicator therefore, would be the the ratio of operational capacity to the peak demand,
# This indicator is measured as the ratio of Total Operational Capacity Per
# Zone(in MW) to Peak Demand Per Zone (in MW). A value of supply ratio below 1 would clearly indicate a shortage.
# namely the Supply Ratio

SupplyRatioA <- bigDF$TotalOperationalCapacityPerZoneInMW_Country_A/bigDF$PeakDemandPerZoneInMW_Country_A
SupplyRatioB <- bigDF$TotalOperationalCapacityPerZoneInMW_Country_B/bigDF$PeakDemandPerZoneInMW_Country_B

bigDF$TotalOperationalCapacityPerZoneInMW_Country_A
bigDF$PeakDemandPerZoneInMW_Country_A

write.table(SupplyRatioA, file = "SupplyRatioA.csv",row.names=FALSE, na="",col.names=FALSE, sep=",")
write.table(SupplyRatioB, file = "SupplyRatioB.csv",row.names=FALSE, na="",col.names=FALSE, sep=",")

supplyRatioAplot = ggplot(data=bigDF, aes(x=tick, y=SupplyRatioA)) + 
  geom_point() +  (aes(colour = runNumber)) + 
  xlab("Year") +  
  ylab("Ratio Operational Capacity over \n Peak demand") + 
  ggtitle("Supply Ratio \n Country A") #give the plot a title
plot(supplyRatioAplot)
ggsave(filename = paste(filePrefix, "supplyRatioA.png", sep=""),scale=1)

# supplyRatioAplot = ggplot(data=bigDF, aes(x=tick, y=SupplyRatioA, group=runId)) + 
#   stat_density2d(geom="tile", aes(color=1), contour = FALSE) +
#   xlab("Year") +  
#   ylab("Ratio Operational Capacity over \n Peak demand") + 
#   ggtitle("Supply Ratio \n Country A") #give the plot a title
# plot(supplyRatioAplot)
# ggsave(filename = paste(filePrefix, "supplyRatioA.png", sep=""),scale=1)


supplyRatioBplot = ggplot(data=bigDF, aes(x=tick, y=SupplyRatioB)) + 
  geom_point() +  #(aes(colour = runNumber)) + 
  xlab("Year") +  
  ylab("Ratio Operational Capacity over \n Peak demand") + 
  ggtitle("Supply Ratio \n Country B") #give the plot a title
plot(supplyRatioBplot)
ggsave(filename = paste(filePrefix, "supplyRatioB.png", sep=""),scale=1)

#Verification Aggregate profits: are the profits zero? If not, why not?
# table with profits

#Verification Expected RES-E Generation & Demand/Consumption
# Is expected generation in tick t equal to actual RES-E generation in tick t + 1?

# renewableGenerationA <-  
#   bigDF$GenerationinMWhCountryA_Photovoltaic + bigDF$GenerationinMWhCountryA_Wind + bigDF$GenerationinMWhCountryA_Biomass + 
#   bigDF$GenerationinMWhCountryA_HydroPower + bigDF$GenerationinMWhCountryA_Biogas + bigDF$GenerationinMWhCountryA_WindOffshore 
# 
# renewableGenerationB <-  
#   bigDF$GenerationinMWhCountryB_Photovoltaic + bigDF$GenerationinMWhCountryB_Wind + bigDF$GenerationinMWhCountryB_Biomass + 
#   bigDF$GenerationinMWhCountryB_HydroPower + bigDF$GenerationinMWhCountryB_Biogas + bigDF$GenerationinMWhCountryB_WindOffshore 

## compare this with logger output like before

# Is expected consumption in tick t equal to actual demand in tick t + 1?
# IDEM


#RES-E generation mix: are targets met? (per country if there is no JointTarget) and if not why not?
# table with targets
# table realized res-e
# table with generation mix 
# graph with generation mix


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


# Capacity mix
plotStackedCapacities <- function(df) {
  localEnv <- environment()
  technologyCapacities <- df[grepl( "CapacityinMWinA_" , names( df ))]
  colnames(technologyCapacities)
  moltenTechnologyCapacities <- melt(df, id.vars = "tick", measure.vars = colnames(technologyCapacities))
  stack <- ggplot(moltenTechnologyCapacities, aes(x = moltenTechnologyCapacities$tick, y = moltenTechnologyCapacities$value, fill = moltenTechnologyCapacities$variable, order = moltenTechnologyCapacities$variable),
                  environment = localEnv)+
    geom_area(position="stack")+
    guides(fill = guide_legend(reverse=TRUE, title = "Legend", ncol = 1, keywidth = .8, keyheight = .8))+
    ggtitle("Capacity mix Country A")+
    theme(plot.title = element_text(lineheight = 0.8, face = "bold", size = 11))+
    scale_fill_discrete(name = "Legend",
                        breaks = colnames(technologyCapacities),
                        labels = substring(colnames(technologyCapacities), 17))+
    scale_x_continuous(name = "Time (year)")+
    scale_y_continuous(name = "Capacity (MW)")+
    theme(axis.title.y = element_text(size = 9, angle = 90),
          axis.title.x = element_text(size = 9, angle = 0),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 10))
  ggsave(filename = paste(filePrefix, "stackedCapacityPlotNL.png", sep=""),
         plot = stack, width=30, height=16.51, units="cm", scale=scaleFactor)}
plotStackedCapacities(bigDF)

plotStackedCapacities <- function(df) {
  localEnv <- environment()
  technologyCapacities <- df[grepl( "CapacityinMWinB_" , names( df ))]
  colnames(technologyCapacities)
  moltenTechnologyCapacities <- melt(df, id.vars = "tick", measure.vars = colnames(technologyCapacities))
  stack <- ggplot(moltenTechnologyCapacities, aes(x = moltenTechnologyCapacities$tick, y = moltenTechnologyCapacities$value, fill = moltenTechnologyCapacities$variable, order = moltenTechnologyCapacities$variable),
                  environment = localEnv)+
    geom_area(position="stack")+
    guides(fill = guide_legend(reverse=TRUE, title = "Legend", ncol = 1, keywidth = .8, keyheight = .8))+
    ggtitle("Capacity mix Country B")+
    theme(plot.title = element_text(lineheight = 0.8, face = "bold", size = 11))+
    scale_fill_discrete(name = "Legend",
                        breaks = colnames(technologyCapacities),
                        labels = substring(colnames(technologyCapacities), 17))+
    scale_x_continuous(name = "Time (year)")+
    scale_y_continuous(name = "Capacity (MW)")+
    theme(axis.title.y = element_text(size = 9, angle = 90),
          axis.title.x = element_text(size = 9, angle = 0),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 10))
  ggsave(filename = paste(filePrefix, "stackedCapacityPlotDE.png", sep=""),
         plot = stack, width=30, height=16.51, units="cm", scale=scaleFactor)}
plotStackedCapacities(bigDF)

# Generation Mix 
plotStackedGeneration <- function(df) {
  localEnv <- environment()
  technologyGeneration <- df[grepl( "GenerationinMWhCountryA_" , names( df ))]
  colnames(technologyGeneration)
  moltenTechnologyGeneration <- melt(df, id.vars = "tick", measure.vars = colnames(technologyGeneration))
  stack <- ggplot(moltenTechnologyGeneration, aes(x = moltenTechnologyGeneration$tick, y = moltenTechnologyGeneration$value, fill = moltenTechnologyGeneration$variable, order = moltenTechnologyGeneration$variable),
                  environment = localEnv)+
    geom_area(position="stack")+
    guides(fill = guide_legend(reverse=TRUE, title = "Legend", ncol = 1, keywidth = .8, keyheight = .8))+
    ggtitle("Generation mix Country A")+
    theme(plot.title = element_text(lineheight = 0.8, face = "bold", size = 11))+
    scale_fill_discrete(name = "Legend",
                        breaks = colnames(technologyGeneration),
                        labels = substring(colnames(technologyGeneration), 17))+
    scale_x_continuous(name = "Time (year)")+
    scale_y_continuous(name = "Generation (MWh)")+
    theme(axis.title.y = element_text(size = 9, angle = 90),
          axis.title.x = element_text(size = 9, angle = 0),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 10))
  ggsave(filename = paste(filePrefix, "stackedGenerationPlotNL.png", sep=""),
         plot = stack, width=30, height=16.51, units="cm", scale=scaleFactor)}
plotStackedGeneration(bigDF)

plotStackedGeneration <- function(df) {
  localEnv <- environment()
  technologyGeneration <- df[grepl( "GenerationinMWhCountryB_" , names( df ))]
  colnames(technologyGeneration)
  moltenTechnologyGeneration <- melt(df, id.vars = "tick", measure.vars = colnames(technologyGeneration))
  stack <- ggplot(moltenTechnologyGeneration, aes(x = moltenTechnologyGeneration$tick, y = moltenTechnologyGeneration$value, fill = moltenTechnologyGeneration$variable, order = moltenTechnologyGeneration$variable),
                  environment = localEnv)+
    geom_area(position="stack")+
    guides(fill = guide_legend(reverse=TRUE, title = "Legend", ncol = 1, keywidth = .8, keyheight = .8))+
    ggtitle("Generation mix Country B")+
    theme(plot.title = element_text(lineheight = 0.8, face = "bold", size = 11))+
    scale_fill_discrete(name = "Legend",
                        breaks = colnames(technologyGeneration),
                        labels = substring(colnames(technologyGeneration), 17))+
    scale_x_continuous(name = "Time (year)")+
    scale_y_continuous(name = "Generation (MWh)")+
    theme(axis.title.y = element_text(size = 9, angle = 90),
          axis.title.x = element_text(size = 9, angle = 0),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 10))
  ggsave(filename = paste(filePrefix, "stackedGenerationPlotDE.png", sep=""),
         plot = stack, width=30, height=16.51, units="cm", scale=scaleFactor)}
plotStackedGeneration(bigDF)


# Tender subsidy costs per year: threshold is 150 MEuro per year
yearlyTenderSubsidyplotA = ggplot(data=bigDF, aes(x=tick, y=yearlyTotalTenderSubsidyCountryA_Tender_Subsidy_Yearly_Country_A, group=runNumber)) + 
  geom_line() + #(aes(colour = runNumber))
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Tender Subsidy \n Country A") #give the plot a title
plot(yearlyTenderSubsidyplotA)
ggsave(filename = paste(filePrefix, "yearlyTenderSubsidyplotA.png", sep=""))

yearlyTenderSubsidyplotB = ggplot(data=bigDF, aes(x=tick, y=yearlyTotalTenderSubsidyCountryB_Tender_Subsidy_Yearly_Country_B, group=runNumber)) + 
  geom_line() + #(aes(colour = runNumber))
  xlab("Year") +  
  ylab("Eur") + 
  ggtitle("Tender Subsidy \n Country B") #give the plot a title
plot(yearlyTenderSubsidyplotB)
ggsave(filename = paste(filePrefix, "yearlyTenderSubsidyplotB.png", sep=""))

#Producer Welfare
#Producer Cash
plotCashBalances <- function(df){
  localEnv <- environment()
  cashA <- df$ProducerCash_Energy_Producer_A
  cashB <- df$ProducerCash_Energy_Producer_B
  cashC <- df$ProducerCash_Energy_Producer_C
  cashD <- df$ProducerCash_Energy_Producer_D
  cashE <- df$ProducerCash_Energy_Producer_E
  cashF <- df$ProducerCash_Energy_Producer_F
  cashG <- df$ProducerCash_Energy_Producer_G
  cashH <- df$ProducerCash_Energy_Producer_H
  cashI <- df$ProducerCash_Energy_Producer_I
  prodCashPlot <- ggplot(df, aes(x=df$tick), environment = localEnv)+
    geom_line(aes(y=cashA, colour="cashA"))+
    geom_line(aes(y=cashB, colour="cashB"))+
    geom_line(aes(y=cashC, colour="cashC"))+
    geom_line(aes(y=cashD, colour="cashD"))+
    geom_line(aes(y=cashE, colour="cashE"))+
    geom_line(aes(y=cashF, colour="cashF"))+
    geom_line(aes(y=cashG, colour="cashG"))+
    geom_line(aes(y=cashH, colour="cashH"))+
    geom_line(aes(y=cashI, colour="cashI"))+
    ggtitle("Overview of producer cash balances")+
    scale_x_continuous(name = "Time (year)")+
    scale_y_continuous(name = "Cash balance (EUR)")+
    scale_colour_manual(name = "Legend", values = c(cashA = "green", cashB = "blue", cashC = "yellow", cashD = "seashell4",
                                                    cashE = "purple", cashF = "red", cashG = "pink", cashH = "black", cashH = "grey"),
                        labels = c(cashA = "Energy producer A",cashB = "Energy producer B",cashC = "Energy producer C",
                                   cashD = "Energy producer D",cashE = "Energy producer E",cashF = "Energy producer F",
                                   cashG = "Energy producer G",cashH = "Energy producer H",cashI = "Energy producer I"))+
    theme(plot.title = element_text(lineheight = 0.8, face = "bold", size = 11),
          axis.title.x = element_text(size = 9, angle = 0),
          axis.title.y = element_text(size = 9, angle = 90),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 10))
  ggsave(filename = paste(filePrefix, "prodCashBalancePlot.png", sep=""),
         plot = prodCashPlot, width=30, height=16.51, units="cm", scale=scaleFactor)
}
plotCashBalances(bigDF)

#Producer welfare = Producers costs - revenues = profit per producer over time, including Tender subsidy
plotProfitIncludingTenderSubsidy <- function(df){
  localEnv <- environment()
  profitA <- df$ProfitProducersYearlyIncludingTenderSubsidy_ProfitProdA
  profitB <- df$ProfitProducersYearlyIncludingTenderSubsidy_ProfitProdB
  profitC <- df$ProfitProducersYearlyIncludingTenderSubsidy_ProfitProdC
  profitD <- df$ProfitProducersYearlyIncludingTenderSubsidy_ProfitProdD
  profitE <- df$ProfitProducersYearlyIncludingTenderSubsidy_ProfitProdE
  profitF <- df$ProfitProducersYearlyIncludingTenderSubsidy_ProfitProdF
  profitG <- df$ProfitProducersYearlyIncludingTenderSubsidy_ProfitProdG
  profitH <- df$ProfitProducersYearlyIncludingTenderSubsidy_ProfitProdH
  profitI <- df$ProfitProducersYearlyIncludingTenderSubsidy_ProfitProdI
  prodProfitPlot <- ggplot(df, aes(x=df$tick), environment = localEnv)+
    geom_line(aes(y=profitA, colour="profitA"))+
    geom_line(aes(y=profitB, colour="profitB"))+
    geom_line(aes(y=profitC, colour="profitC"))+
    geom_line(aes(y=profitD, colour="profitD"))+
    geom_line(aes(y=profitE, colour="profitE"))+
    geom_line(aes(y=profitF, colour="profitF"))+
    geom_line(aes(y=profitG, colour="profitG"))+
    geom_line(aes(y=profitH, colour="profitH"))+
    geom_line(aes(y=profitI, colour="profitI"))+
    ggtitle("Overview of producer profit \n Including Tender Subsidy")+
    scale_x_continuous(name = "Time (year)")+
    scale_y_continuous(name = "Profit (EUR)")+
    scale_colour_manual(name = "Legend", values = c(profitA = "green", profitB = "blue", profitC = "yellow", profitD = "seashell4",
                                                    profitE = "purple", profitF = "red", profitG = "pink", profitH = "black" , profitI = "grey"),
                        labels = c(profitA = "Energy producer A",profitB = "Energy producer B",profitC = "Energy producer C",
                                   profitD = "Energy producer D",profitE = "Energy producer E",profitF = "Energy producer F",
                                   profitG = "Energy producer G",profitH = "Energy producer H",profitI = "Energy producer I"))+
    theme(plot.title = element_text(lineheight = 0.8, face = "bold", size = 11),
          axis.title.x = element_text(size = 9, angle = 0),
          axis.title.y = element_text(size = 9, angle = 90),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 10))
  ggsave(filename = paste(filePrefix, "prodProfitIncludingTenderSubsidyPlot.png", sep=""),
         plot = prodProfitPlot, width=30, height=16.51, units="cm", scale=scaleFactor)
}
plotProfitIncludingTenderSubsidy(bigDF)


#Producer welfare = Producers costs - revenues = profit per producer over time, excluding Tender subsidy
producerWelfareExcTenderA <- bigDF$ProfitProducersYearlyExcludingTenderSubsidy_ProfitProdA + bigDF$ProfitProducersYearlyExcludingTenderSubsidy_ProfitProdB + bigDF$ProfitProducersYearlyExcludingTenderSubsidy_ProfitProdC + bigDF$ProfitProducersYearlyExcludingTenderSubsidy_ProfitProdD
producerWelfareExcTenderB <- bigDF$ProfitProducersYearlyExcludingTenderSubsidy_ProfitProdA + bigDF$ProfitProducersYearlyExcludingTenderSubsidy_ProfitProdB + bigDF$ProfitProducersYearlyExcludingTenderSubsidy_ProfitProdC + bigDF$ProfitProducersYearlyExcludingTenderSubsidy_ProfitProdD

producerWelfareIncTenderA <- bigDF$ProfitProducersYearlyIncludingTenderSubsidy_ProfitProdA + bigDF$ProfitProducersYearlyIncludingTenderSubsidy_ProfitProdB + bigDF$ProfitProducersYearlyIncludingTenderSubsidy_ProfitProdC + bigDF$ProfitProducersYearlyIncludingTenderSubsidy_ProfitProdD
producerWelfareIncTenderB <- bigDF$ProfitProducersYearlyIncludingTenderSubsidy_ProfitProdA + bigDF$ProfitProducersYearlyIncludingTenderSubsidy_ProfitProdB + bigDF$ProfitProducersYearlyIncludingTenderSubsidy_ProfitProdC + bigDF$ProfitProducersYearlyIncludingTenderSubsidy_ProfitProdD

#Income Distribution
# check different profits of producer whether the tender makes them really skewed or not
# profits with and without subsidy
plotProfitExcludingTenderSubsidy <- function(df){
  localEnv <- environment()
  profitA <- df$ProfitProducersYearlyExcludingTenderSubsidy_ProfitProdA
  profitB <- df$ProfitProducersYearlyExcludingTenderSubsidy_ProfitProdB
  profitC <- df$ProfitProducersYearlyExcludingTenderSubsidy_ProfitProdC
  profitD <- df$ProfitProducersYearlyExcludingTenderSubsidy_ProfitProdD
  profitE <- df$ProfitProducersYearlyExcludingTenderSubsidy_ProfitProdE
  profitF <- df$ProfitProducersYearlyExcludingTenderSubsidy_ProfitProdF
  profitG <- df$ProfitProducersYearlyExcludingTenderSubsidy_ProfitProdG
  profitH <- df$ProfitProducersYearlyExcludingTenderSubsidy_ProfitProdH
  prodProfitPlot <- ggplot(df, aes(x=df$tick), environment = localEnv)+
    geom_line(aes(y=profitA, colour="profitA"))+
    geom_line(aes(y=profitB, colour="profitB"))+
    geom_line(aes(y=profitC, colour="profitC"))+
    geom_line(aes(y=profitD, colour="profitD"))+
    geom_line(aes(y=profitE, colour="profitE"))+
    geom_line(aes(y=profitF, colour="profitF"))+
    geom_line(aes(y=profitG, colour="profitG"))+
    geom_line(aes(y=profitH, colour="profitH"))+
    ggtitle("Overview of producer profit \n Excluding Tender Subsidy")+
    scale_x_continuous(name = "Time (year)")+
    scale_y_continuous(name = "Profit (EUR)")+
    scale_colour_manual(name = "Legend", values = c(profitA = "green", profitB = "blue", profitC = "yellow", profitD = "seashell4",
                                                    profitE = "purple", profitF = "red", profitG = "pink", profitH = "black"),
                        labels = c(profitA = "Energy producer A",profitB = "Energy producer B",profitC = "Energy producer C",
                                   profitD = "Energy producer D",profitE = "Energy producer E",profitF = "Energy producer F",
                                   profitG = "Energy producer G",profitH = "Energy producer H"))+
    theme(plot.title = element_text(lineheight = 0.8, face = "bold", size = 11),
          axis.title.x = element_text(size = 9, angle = 0),
          axis.title.y = element_text(size = 9, angle = 90),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 10))
  ggsave(filename = paste(filePrefix, "prodProfitExcludingTenderSubsidyPlot.png", sep=""),
         plot = prodProfitPlot, width=30, height=16.51, units="cm", scale=scaleFactor)
}
plotProfitExcludingTenderSubsidy(bigDF)

#Producer yearly tender subsidies
plotTenderSubsidy <- function(df){
  localEnv <- environment()
  subsidyA <- df$TenderYearlySubsidyProducer_Tender_Subsidy_Yearly_Producer_A
  subsidyB <- df$TenderYearlySubsidyProducer_Tender_Subsidy_Yearly_Producer_B
  subsidyC <- df$TenderYearlySubsidyProducer_Tender_Subsidy_Yearly_Producer_C
  subsidyD <- df$TenderYearlySubsidyProducer_Tender_Subsidy_Yearly_Producer_D
  subsidyE <- df$TenderYearlySubsidyProducer_Tender_Subsidy_Yearly_Producer_E
  subsidyF <- df$TenderYearlySubsidyProducer_Tender_Subsidy_Yearly_Producer_F
  subsidyG <- df$TenderYearlySubsidyProducer_Tender_Subsidy_Yearly_Producer_G
  subsidyH <- df$TenderYearlySubsidyProducer_Tender_Subsidy_Yearly_Producer_H
  subsidyI <- df$TenderYearlySubsidyProducer_Tender_Subsidy_Yearly_Producer_I
  prodSubsidyPlot <- ggplot(df, aes(x=df$tick), environment = localEnv)+
    geom_line(aes(y=subsidyA, colour="subsidyA"))+
    geom_line(aes(y=subsidyB, colour="subsidyB"))+
    geom_line(aes(y=subsidyC, colour="subsidyC"))+
    geom_line(aes(y=subsidyD, colour="subsidyD"))+
    geom_line(aes(y=subsidyE, colour="subsidyE"))+
    geom_line(aes(y=subsidyF, colour="subsidyF"))+
    geom_line(aes(y=subsidyG, colour="subsidyG"))+
    geom_line(aes(y=subsidyH, colour="subsidyH"))+
    geom_line(aes(y=subsidyI, colour="subsidyI"))+
    ggtitle("Overview of producer Tender Subsidy")+
    scale_x_continuous(name = "Time (year)")+
    scale_y_continuous(name = "Subsidy (EUR)")+
    scale_colour_manual(name = "Legend", values = c(subsidyA = "green", subsidyB = "blue", subsidyC = "yellow", subsidyD = "seashell4",
                                                    subsidyE = "purple", subsidyF = "red", subsidyG = "pink", subsidyH = "black", subsidyI = "grey"),
                        labels = c(subsidyA = "Energy producer A",subsidyB = "Energy producer B",subsidyC = "Energy producer C",
                                   subsidyD = "Energy producer D",subsidyE = "Energy producer E",subsidyF = "Energy producer F",
                                   subsidyG = "Energy producer G",subsidyH = "Energy producer H", subsidyI = "Energy producer I"))+
    theme(plot.title = element_text(lineheight = 0.8, face = "bold", size = 11),
          axis.title.x = element_text(size = 9, angle = 0),
          axis.title.y = element_text(size = 9, angle = 90),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 10))
  ggsave(filename = paste(filePrefix, "prodSubsidyPlot.png", sep=""),
         plot = prodSubsidyPlot, width=30, height=16.51, units="cm", scale=scaleFactor)
}
plotTenderSubsidy(bigDF)



# Total welfare = consumer welfare and producer welfare

#Market Value of RES-E (PV, Wind, WindOfshore)
# market value = Volume PV per segment * Sgement Clearing Price / generation in MWh in tick
# system base price = Average electricity price in tick
# value factor = market value / system base price

pSA_1 = 
  ggplot(data=bigDF, aes(x=tick)) + 
  geom_smooth(aes(y=bigDF$PriceInEURperMWh_Segment_Country_A_1), method= "loess") + #(aes(colour = runId)) +
  xlab("Tick") +  
  ylab("Price (EUR/MWh)") + 
  ggtitle("Segment 1 - Country A") #give the plot a title
plot(pSA_1)

pSA_2 = 
  ggplot(data=bigDF, aes(x=tick)) + 
  geom_smooth(aes(y=bigDF$PriceInEURperMWh_Segment_Country_A_2), method= "loess") + #(aes(colour = runId)) +
  xlab("Tick") +  
  ylab("Price (EUR/MWh)") + 
  ggtitle("Segment 2 - Country A") #give the plot a title
plot(pSA_2)

pSA_3 = 
  ggplot(data=bigDF, aes(x=tick)) + 
  geom_smooth(aes(y=bigDF$PriceInEURperMWh_Segment_Country_A_3), method= "loess") + #(aes(colour = runId)) +
  xlab("Tick") +  
  ylab("Price (EUR/MWh)") + 
  ggtitle("Segment 3 - Country A") #give the plot a title
plot(pSA_3)

pSA_4 = 
  ggplot(data=bigDF, aes(x=tick)) + 
  geom_smooth(aes(y=bigDF$PriceInEURperMWh_Segment_Country_A_4), method= "loess") + #(aes(colour = runId)) +
  xlab("Tick") +  
  ylab("Price (EUR/MWh)") + 
  ggtitle("Segment 4 - Country A") #give the plot a title
plot(pSA_4)

pSB_1 = 
  ggplot(data=bigDF, aes(x=tick)) + 
  geom_smooth(aes(y=bigDF$PriceInEURperMWh_Segment_Country_B_1), method= "loess") + #(aes(colour = runId)) +
  xlab("Tick") +  
  ylab("Price (EUR/MWh)") + 
  ggtitle("Segment 1 - Country B") #give the plot a title
plot(pSB_1)

pSB_2 = 
  ggplot(data=bigDF, aes(x=tick)) + 
  geom_smooth(aes(y=bigDF$PriceInEURperMWh_Segment_Country_B_2), method= "loess") + #(aes(colour = runId)) +
  xlab("Tick") +  
  ylab("Price (EUR/MWh)") + 
  ggtitle("Segment 2 - Country B") #give the plot a title
plot(pSB_2)

pSB_3 = 
  ggplot(data=bigDF, aes(x=tick)) + 
  geom_smooth(aes(y=bigDF$PriceInEURperMWh_Segment_Country_B_3), method= "loess") + #(aes(colour = runId)) +
  xlab("Tick") +  
  ylab("Price (EUR/MWh)") + 
  ggtitle("Segment 3 - Country B") #give the plot a title
plot(pSB_3)

pSB_4 = 
  ggplot(data=bigDF, aes(x=tick)) + 
  geom_smooth(aes(y=bigDF$PriceInEURperMWh_Segment_Country_B_4), method= "loess") + #(aes(colour = runId)) +
  xlab("Tick") +  
  ylab("Price (EUR/MWh)") + 
  ggtitle("Segment 4 - Country B") #give the plot a title
plot(pSB_4)

noInitialShortagesPlot <-multiplot(pSA_1,pSA_2,pSA_3,pSA_4, pSB_1,pSB_2,pSB_3,pSB_4, cols=4)
plot(noInitialShortagesPlot)
ggsave(filename = paste(filePrefix, "noInitialShortages.png", sep=""))
