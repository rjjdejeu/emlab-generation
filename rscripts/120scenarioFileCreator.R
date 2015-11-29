#Placeholders

# Step 1 building the scenarios: insert dataframe and read the scenario file. Name parameters
# that need to be replaced with #
# This folder path in Step 1 is different than in Step 2 (scenarios/Scenarios), so make sure that the Scenario File is in the highest scenario file, and that there exist a sub folder called Scenarios in the folder scenarios
xmlFilePath<-"/home/rob/emlabGen/scenarios/TechSpecFullTwoCountriesInfCap.xml"
filestump<-'TechSpecFullTwoCountriesInfCap-'
# Step 2 building the scenarios: make separate data vectors
noOfRepetitions = 120 
for(runID in seq(1:noOfRepetitions))
  {
  xmlFileContent<-readLines(xmlFilePath, encoding = "UTF-8")
  xmlFileContent<-gsub("#repetitionNumber", runID, xmlFileContent)
  writeLines(xmlFileContent, paste("/home/rob/emlabGen/scenarios/Scenarios/", filestump, runID, ".xml", sep=""))
  }

