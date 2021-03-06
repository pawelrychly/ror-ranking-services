##########################################
# Usage:
# R --slave --vanilla --args "[inDirectory]" "[outDirectory]" < RORUTA-RankRelatedPreferentialReductsHierarchical.R
# Example: 
# R --slave --vanilla --args "${PWD}/in" "${PWD}/out" < RORUTA-RankRelatedPreferentialReductsHierarchical.R
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
input.best.ranks.filename = "best-ranking-hierarchical.xml"
input.worst.ranks.filename = "worst-ranking-hierarchical.xml"
parameters.filename = "parameters.xml"
hierarchy.of.criteria.filename = "hierarchy-of-criteria.xml"
preference.direction = "criteria-preference-directions.xml"

#OUTPUT FILES:
reducts.filename <- "reducts-by-alternatives-hierarchical.xml"
result.file.messages <- "messages.xml"

is_proper_data = TRUE
performances <- rorranking:::getPerformancesFromXmcdaFiles(alternatives.filename=alternatives.filename,
                                                                criteria.filename=criteria.filename,
                                                                performances.filename=performances.filename)

hierarchy.data = rorranking:::getHierarchyOfCriteriaFromXmcdaFile(hierarchy.of.criteria.filename)
ranks.by.nodes.data = rorranking:::getExtremeRanksByNodesFromXmcdaFile(worst.ranking.hierarchical.filename = input.worst.ranks.filename,
                                                  best.ranking.hierarchical.filename = input.best.ranks.filename, performances = performances$data)


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

if (ranks.by.nodes.data$status != "OK") {
  errFile <- paste(errFile, ranks.by.nodes.data$errFile)
  errData <- paste(errData, ranks.by.nodes.data$errData)
  is_proper_data = FALSE
} 

#optional
preferences = list("strong" = NULL, "weak" = NULL, "indif" = NULL)
nums.of.characteristic.points = NULL
criteria.preference.directions = NULL

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

  criteria.data <- rorranking:::getCriteriaPreferenceDirectionFromXmcdaFile(filename=preference.direction,
                                                                                  performances=performances$data)
  
  if (criteria.data$status == "OK") {
    criteria.preference.directions <- criteria.data$data 
  } else {
    errData <- paste(errData,  criteria.data$errData)
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
      results <- findAllRankRelatedPreferentionalReductsHierarchical(
        perf = performances$data, ranks=ranks.by.nodes.data$data, strict.vf=strict, 
        strong.prefs = preferences$strong, weak.prefs=preferences$weak, indif.prefs=preferences$indif,
        nums.of.characteristic.points=nums.of.characteristic.points, criteria=criteria.preference.directions, hierarchy.data=hierarchy.data$data) 
      }, silent=TRUE
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
    reducts.by.alternatives <- list()
    for (key in names(results[[nodeid]])) {
      alternativeId <- strsplit(key, " :[", TRUE)[[1]][[1]]
      
      reducts <- results[[nodeid]][[key]]
      reducts.by.alternatives[[alternativeId]] <- list()
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
          reducts.by.alternatives[[alternativeId]] <- append(reducts.by.alternatives[[alternativeId]], reduct.str)
        }
      } 
      
    }
    rorranking:::putAlternativesValuesWithReductsData(outTreeReducts, reducts.by.alternatives, attributes= c(id=nodeid))
    
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

if ((!is.null(errData)) && (length(errData) > 0)) {
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
