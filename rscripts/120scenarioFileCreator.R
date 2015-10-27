#Placeholders

# Step 1 building the scenarios: insert dataframe and read the scenario file. Name parameters
# that need to be replaced with #
xmlFilePath<-"~/Desktop/emlabGen/scenarios/RDJ-RT-TC-ThreeMultiNodes-SENS-SupportPeriod15-Template.xml"
filestump<-'RDJ-RT-TC-ThreeMultiNodes-SENS-SupportPeriod15-'
# Step 2 building the scenarios: make separate data vectors
noOfRepetitions = 120 
for(runID in seq(1:noOfRepetitions))
  {
  xmlFileContent<-readLines(xmlFilePath, encoding = "UTF-8")
  xmlFileContent<-gsub("#repetitionNumber", runID, xmlFileContent)
  writeLines(xmlFileContent, paste("~/Desktop/emlabGen/scenarios/RDJ-RT-TC-ThreeMultiNodes-SENS-SupportPeriod15-Template/", filestump, runID, ".xml", sep=""))
  }

