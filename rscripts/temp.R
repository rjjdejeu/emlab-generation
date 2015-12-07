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
nameFile <- "FuturePointFive"
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
estimationErrorExpectedGenerationB <- (renewableGenerationB - expectedRenewableGenerationB)*100/renewableGenerationB  
seEstimationErrorExpectedGenerationA <- (seRenewableGenerationA - seExpectedRenewableGenerationA)/seExpectedRenewableGenerationA 
seEstimationErrorExpectedGenerationB <- (seRenewableGenerationB - seExpectedRenewableGenerationB)/seExpectedRenewableGenerationB

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