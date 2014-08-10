##########################################
# Usage:
# R --slave --vanilla --args "[inDirectory]" "[outDirectory]" < RORfindPreferentialReductsForNecessaryRelationsHierarchical.R
# Example: 
# R --slave --vanilla --args "${PWD}/in" "${PWD}/out" < RORfindPreferentialReductsForNecessaryRelationsHierarchical.R
##########################################

inDirectory <- commandArgs()[5] 
outDirectory <- commandArgs()[6]  

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
input.necessary.relations.filename <- "necessary-relations-hierarchical.xml"
parameters.filename = "parameters.xml"
hierarchy.of.criteria.filename = "hierarchy-of-criteria.xml"

#OUTPUT FILES:
reducts.filename <- "reducts-by-necessary-relations-hierarchical.xml"
result.file.messages <- "messages.xml"

is_proper_data = TRUE
performances <- rorranking:::getPerformancesFromXmcdaFiles(alternatives.filename=alternatives.filename,
                                                                criteria.filename=criteria.filename,
                                                                performances.filename=performances.filename)

hierarchy.data = rorranking:::getHierarchyOfCriteriaFromXmcdaFile(hierarchy.of.criteria.filename)
nec.relations.by.nodes = rorranking:::getNecessaryRelationsFromXmcdaFile(input.necessary.relations.filename, performances$data)

if (performances$status != "OK") {
  errFile <- paste(errFile, performances$errFile)
  errData <- paste(errData, performances$errData)
  is_proper_data = FALSE
} 

if (hierarchy.data$status != "OK") {
  errFile <- paste(errFile, hierarchy.data$errFile)
  errData <- paste(errData, hierarchy.data$errData)
  is_proper_data = FALSE
}

if (nec.relations.by.nodes$status != "OK") {
  errFile <- paste(errFile, nec.relations.by.nodes$errFile)
  errData <- paste(errData, nec.relations.by.nodes$errData)
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

if (is.null(errFile) && is_proper_data){
  tmpErr<- try (
    {
      results <- findPreferentionalReductsForNecessaryRelationsHierarchical(
        perf = performances$data, nec.relations=nec.relations.by.nodes$data,
        strong.prefs = preferences$strong, weak.prefs=preferences$weak, indif.prefs=preferences$indif,
        strict.vf=strict, nums.of.characteristic.points=nums.of.characteristic.points, hierarchy.data=hierarchy.data$data) 
    }, silent=FALSE
  )  

  if (inherits(tmpErr, 'try-error')){
    errCalc<<-"Cannot find reducts"
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
  
  for (nodeid in names(results)) {
    results.for.node <- results[[nodeid]]
    id <- 0
    relations = list()
    reducts.by.relations = list()
    for (key in names(results.for.node)) {
      id = id + 1
      relation <- strsplit(key, " >=^N ", TRUE)[[1]]
      relations[[key]] = relation
      reducts.by.relations[[key]] <- list()
      reducts <- results.for.node[[key]]
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
    rorranking:::putAlternativesComparisonsWithReductsData(outTreeReducts, relations, reducts.by.relations, attributes=c("id"=nodeid))
  }
  saveXML(outTreeReducts, file=reducts.filename)

  outTreeMessage = newXMLDoc()
  newXMLNode("xmcda:XMCDA", 
      attrs=c("xsi:schemaLocation" = "http://www.decision-deck.org/2009/XMCDA-2.0.0 http://www.decision-deck.org/xmcda/_downloads/XMCDA-2.0.0.xsd"),
      suppressNamespaceWarning=TRUE, 
      namespace = c("xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmcda" = "http://www.decision-deck.org/2009/XMCDA-2.0.0"), 
      parent=outTreeMessage)

  status<-putLogMessage(outTreeMessage, "OK", name = "executionStatus")
  status<-saveXML(outTreeMessage, file=result.file.messages)
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
