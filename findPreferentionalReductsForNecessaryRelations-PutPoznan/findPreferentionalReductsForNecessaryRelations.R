##########################################
# Usage:
# R --slave --vanilla --args "[inDirectory]" "[outDirectory]" < findNecessaryAndPossiblePreferenceRelations.R
# Example: 
# R --slave --vanilla --args "${PWD}/in" "${PWD}/out" < findNecessaryAndPossiblePreferenceRelations.R
##########################################

inDirectory <- commandArgs()[5] 
outDirectory <- commandArgs()[6]  
#inDirectory <- "/home/pawel/Projects/ror-workspace/robust-ordinal-regresion/tests/in2" 
#outDirectory <-"/home/pawel/Projects/ror-workspace/robust-ordinal-regresion/tests/out2" 
##########################################
# Set the working directory as the "in" directory
##########################################
setwd(inDirectory)
options( java.parameters = "-Xmx2g" )
library(rorranking)

errFile<-NULL
errData<-NULL
errCalc<-NULL
execFlag<-FALSE

#INPUT FILES:
alternatives.filename = "alternatives.xml"
criteria.filename = "criteria.xml"
performances.filename = "performances.xml"
preferences.filename = "preferences.xml"
characteristic.points.filename = "characteristic-points.xml"
input.necessary.relations.filename <- "necessary-relations.xml"
parameters.filename = "parameters.xml"

#OUTPUT FILES:
reducts.filename <- "reducts-by-necessary-relations.xml"
result.file.messages <- "messages.xml"

is_proper_data = TRUE
performances <- rorranking:::getPerformancesFromXmcdaFiles(alternatives.filename=alternatives.filename,
                                                                criteria.filename=criteria.filename,
                                                                performances.filename=performances.filename)

nec.rel = rorranking:::getNecessaryRelationsMatrixFromXmcdaFile(input.necessary.relations.filename,performances$data)

if (performances$status != "OK") {
  errFile <- paste(errFile, performances$errFile)
  errData <- paste(errData, performances$errData)
  is_proper_data = FALSE
} 
if (nec.rel$status != "OK") {
  errFile <- paste(errFile, nec.rel$errFile)
  errData <- paste(errData, nec.rel$errData)
  is_proper_data = FALSE
}

#optional
preferences = list("strong" = NULL, "weak" = NULL, "indif" = NULL)
nums.of.characteristic.points = NULL
nec.relations = list()
strict = FALSE
if (is_proper_data) { #optional paramenters
  preferences.data <- rorranking:::getPreferencesFromXmcdaFile(filename=preferences.filename, performances=performances$data)
  if (preferences.data$status == "OK") {
    preferences = preferences.data$data
  } else {
    errData <- paste(errData,  preferences.data$errData)
  }
  print(preferences.data)
  characteristic.points.data <- rorranking:::getCharacteristicPointsFromXmcdaFile(filename=characteristic.points.filename,
                                                                                  performances=performances$data)
  if (characteristic.points.data$status == "OK") {
    nums.of.characteristic.points <- characteristic.points.data$data  
  } else {
    errData <- paste(errData,  characteristic.points.data$errData)
  }
  params.data <- rorranking:::getParametersDataFromXmcdaFile(filename=parameters.filename,
                                                             keys=c("strict"), defaults=list("strict" = TRUE))
  
  if (params.data$status == "OK") {
    strict <- params.data$data[['strict']]
  } else {
    errData <- paste(errData, params.data$errData)
  }
  
}
results <-NULL
print("perfomances")
print(performances$data)
print("strong.prefs")
print(preferences$strong)
print("weak.prefs")
print(preferences$weak)
print("indif.prefs")
print(preferences$indif)
print("strict")
print(strict)
print("nums.of.characteristic.points")
print(nums.of.characteristic.points)
print("nec.rel")
print(nec.rel[[1]])
if (is.null(errFile) && is_proper_data){
  tmpErr<- try (
    {
      results <- findPreferentionalReductsForNecessaryRelations(
        perf = performances$data, nec.relations.matrix=nec.rel$data, strict.vf=strict, 
        strong.prefs = preferences$strong, weak.prefs=preferences$weak, indif.prefs=preferences$indif,
        nums.of.characteristic.points=nums.of.characteristic.points) 
    }, silent=TRUE
  )  
  if (inherits(tmpErr, 'try-error')){
    errCalc<<-"Cannot find reducts."
  } else {
    execFlag<-TRUE
  }
}
setwd(outDirectory)
if (execFlag) {
  outTreeReducts = newXMLDoc()
  
  newXMLNode("xmcda:XMCDA",
             attrs=c("xsi:schemaLocation" = "http://www.decision-deck.org/2012/XMCDA-2.2.0 http://www.decision-deck.org/xmcda/_downloads/XMCDA-2.2.0.xsd"),
             suppressNamespaceWarning=TRUE,
             namespace = c("xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmcda" = "http://www.decision-deck.org/2012/XMCDA-2.2.0"),
             parent=outTreeReducts)
  
  id <- 0
  relations = list()
  reducts.by.relations = list()
  for (key in names(results)) {
    id = id + 1
    relation <- strsplit(key, " >=^N ", TRUE)[[1]]
    relations[[key]] = relation
    reducts.by.relations[[key]] <- list()
    reducts <- results[[key]]
    if ((is.character(reducts)) && (reducts == " EMPTY SET ")) {
      next
    }
    for (reduct in reducts) {
      reduct.str <- ""
      if (length(reduct) > 0) {
        i <- 0
        for (comparison in reduct) {
          if (i > 0) {
            reduct.str <- paste(reduct.str, comparison, sep=", ") 
          } else {
            reduct.str <- comparison
            i = i + 1
          }
        }
        reduct.str <- sub(">=", "weak", reduct.str, ignore.case = TRUE)
        reduct.str <- sub(">", "strong", reduct.str, ignore.case = TRUE)
        reduct.str <- sub("==", "indif", reduct.str, ignore.case = TRUE)
        reducts.by.relations[[key]] <- append(reducts.by.relations[[key]], reduct.str)
      }
    }    
  }
  print(reducts.by.relations)
  rorranking:::putAlternativesComparisonsWithReductsData(outTreeReducts, relations, reducts.by.relations)
  saveXML(outTreeReducts, file=reducts.filename)
}

if (!is.null(errCalc)){
  outTreeMessage = newXMLDoc()
  newXMLNode("xmcda:XMCDA", 
             attrs=c("xsi:schemaLocation" = "http://www.decision-deck.org/2012/XMCDA-2.2.0 http://www.decision-deck.org/xmcda/_downloads/XMCDA-2.2.0.xsd"),
             suppressNamespaceWarning=TRUE, 
             namespace = c("xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmcda" = "http://www.decision-deck.org/2012/XMCDA-2.2.0"), 
             parent=outTreeMessage)
  status<-putErrorMessage(outTreeMessage, errCalc, name="Error")
  status<-saveXML(outTreeMessage, file=result.file.messages)
  
}

if ((!is.null(errData)) && (length(errData) > 0)){
  print("DATA")
  print(errData)
  outTreeMessage = newXMLDoc()  
  newXMLNode("xmcda:XMCDA", 
             attrs=c("xsi:schemaLocation" = "http://www.decision-deck.org/2012/XMCDA-2.2.0 http://www.decision-deck.org/xmcda/_downloads/XMCDA-2.2.0.xsd"),
             suppressNamespaceWarning=TRUE, 
             namespace = c("xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmcda" = "http://www.decision-deck.org/2012/XMCDA-2.2.0"), 
             parent=outTreeMessage)
  status<-putErrorMessage(outTreeMessage, errData, name="Error")
  status<-saveXML(outTreeMessage, file=result.file.messages)
}

if (!is.null(errFile)) {
  outTreeMessage = newXMLDoc()
  newXMLNode("xmcda:XMCDA", 
             attrs=c("xsi:schemaLocation" = "http://www.decision-deck.org/2012/XMCDA-2.2.0 http://www.decision-deck.org/xmcda/_downloads/XMCDA-2.2.0.xsd"),
             suppressNamespaceWarning=TRUE, 
             namespace = c("xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmcda" = "http://www.decision-deck.org/2012/XMCDA-2.2.0"), 
             parent=outTreeMessage)
  status<-putErrorMessage(outTreeMessage, errFile, name="Error") 
  status<-saveXML(outTreeMessage, file=result.file.messages)
}
