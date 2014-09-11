##########################################
# Usage:
# R --slave --vanilla --args "[inDirectory]" "[outDirectory]" < RORUTA-PostFactum-PreferenceRelatedImprovementOrMissingValue.R
# Example: 
# R --slave --vanilla --args "${PWD}/in" "${PWD}/out" < RORUTA-PostFactum-PreferenceRelatedImprovementOrMissingValue.R
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
target.filename = "target.xml"
preferences.filename = "preferences.xml"
intensities.of.preferences.filename = "intensities-of-preferences.xml"
rank.related.preferences.filename = "rank-related-requirements.xml"
characteristic.points.filename = "characteristic-points.xml"
selected.criteria.filename = "selected-criteria.xml"
parameters.filename = "parameters.xml"

#OUTPUT FILES:
result.file <- "result.xml"
result.file.messages <- "messages.xml"

is_proper_data = TRUE
performances <- rorranking:::getPerformancesFromXmcdaFiles(alternatives.filename=alternatives.filename,
                                                                criteria.filename=criteria.filename,
                                                                performances.filename=performances.filename)
target <- rorranking:::getAlternativesComparisonFromXmcdaFile(filename=target.filename, performances=performances$data)

target.variant.a <- NULL
target.variant.b <- NULL
if (performances$status != "OK") {
  errFile <- paste(errFile, performances$errFile)
  errData <- paste(errData, performances$errData)
  is_proper_data = FALSE
} 
if (target$status != "OK") {
  errFile <- paste(errFile, target$errFile)
  errData <- paste(errData, target$errData)
  is_proper_data = FALSE
} else {
  target.variant.a <- target$data[[1]]
  target.variant.b <- target$data[[2]]
} 
preferences = list("strong" = NULL, "weak" = NULL, "indif" = NULL)
intensities.of.preferences =  list("strong" = NULL, "weak" = NULL, "indif" = NULL)
rank.related.preferences = NULL
nums.of.characteristic.points = NULL

selected.attributes = NULL
is.possible = FALSE
strict = FALSE
type.of.result = "improvement"  
precision = 0.005
possible.or.necessary <- "necessary"

if (is_proper_data) { #optional paramenters
  #preferences
  preferences.data <- rorranking:::getPreferencesFromXmcdaFile(filename=preferences.filename, performances=performances$data)
  if (preferences.data$status == "OK") {
    preferences = preferences.data$data
  } else {
    errData <- paste(errData, preferences.data$errData)
  }
  #intensities of preferences
  intensities.of.preferences.data <- rorranking:::getIntensitiesOfPreferencesFromXmcdaFile(
    filename=intensities.of.preferences.filename ,performances=performances$data)
  if (intensities.of.preferences.data$status == "OK") {
    intensities.of.preferences =  intensities.of.preferences.data$data
  } else {
    errData <- paste(errData,  intensities.of.preferences.data$errData)
  }
  #rank related preferences data
  rank.related.preferences.data = rorranking:::getRankRelatedPreferencesFromXmcdaFile(rank.related.preferences.filename, performances$data)
  if (rank.related.preferences.data$status == "OK") {
    rank.related.preferences = rank.related.preferences.data$data
  } else {
    errData <- paste(errData,  rank.related.preferences.data$errData)
  }
  #numbers of characteristic points
  characteristic.points.data <- rorranking:::getCharacteristicPointsFromXmcdaFile(filename=characteristic.points.filename,
                                                                                  performances=performances$data)
  if (characteristic.points.data$status == "OK") {
    nums.of.characteristic.points <- characteristic.points.data$data  
  } else {
    errData <- paste(errData,  characteristic.points.data$errData)
  }
  #params data
  params.data <- rorranking:::getParametersDataFromXmcdaFile(filename=parameters.filename,
                                                             keys=c("strict", "type_of_result", "precision", "possible_or_necessary"), defaults=list("strict" = TRUE))
  if (params.data$status == "OK") {
    if ('strict' %in% names(params.data$data)) {
      strict <- params.data$data[['strict']]  
    }
    if ('possible_or_necessary' %in% names(params.data$data)) {
      possible.or.necessary <- params.data$data[['possible_or_necessary']] 
    }
    if ('type_of_result' %in% names(params.data$data)) {
      type.of.result <- params.data$data[['type_of_result']]  
    }
    if ('precision' %in% names(params.data$data)) {
      precision <- params.data$data[['precision']]  
    }
  } else {
    errData <- paste(errData,  params.data$errData)
  }
  #selected attributes
  selected.attributes.data <- rorranking:::getSelectedCriteriaFromXmcdaFile(filename=selected.criteria.filename,
                                                                            criteriaIDs=dimnames(performances$data)[[2]])
  if (selected.attributes.data$status == "OK") {
    selected.attributes <- selected.attributes.data$data  
  } else {
    errData <- paste(errData, selected.attributes.data$errData)
  }
}


result <-NULL
if (is.null(errFile) && is_proper_data){
  tmpErr<- try (
    {
      if (possible.or.necessary == "necessary") {
        if (type.of.result == "missing-value") {
          result <- findMissingOrRedundantUtilityValueForRelation(
            perf=performances$data, a = target.variant.a, b = target.variant.b,
            strict.vf = strict, is.possibly.preffered=FALSE,  
            strong.prefs = preferences$strong, weak.prefs=preferences$weak, 
            indif.prefs=preferences$indif, 
            strong.intensities.of.prefs = intensities.of.preferences$strong,
            weak.intensities.of.prefs = intensities.of.preferences$weak, 
            indif.intensities.of.prefs = intensities.of.preferences$indif,
            rank.related.requirements = rank.related.preferences, 
            nums.of.characteristic.points = nums.of.characteristic.points,
            improvement=TRUE)
        } else {
         
          result <- findPerformanceModificationForNecessaryRelation(
            perf=performances$data, a = target.variant.a, b = target.variant.b,
            strict.vf = strict,
            strong.prefs = preferences$strong, weak.prefs=preferences$weak, 
            indif.prefs=preferences$indif, 
            strong.intensities.of.prefs = intensities.of.preferences$strong, 
            weak.intensities.of.prefs = intensities.of.preferences$weak,
            indif.intensities.of.prefs = intensities.of.preferences$indif,
            rank.related.requirements=rank.related.preferences, 
            nums.of.characteristic.points=nums.of.characteristic.points,
            precision = precision,
            which.attributes = selected.attributes, 
            greater.than.one = TRUE)
        } 
      } else {
        if (type.of.result == "missing-value") {
          result <- findMissingOrRedundantUtilityValueForRelation(
            perf=performances$data, a = target.variant.a, b = target.variant.b,
            strict.vf = strict, is.possibly.preffered=TRUE,  
            strong.prefs = preferences$strong, weak.prefs=preferences$weak, 
            indif.prefs=preferences$indif, 
            strong.intensities.of.prefs = intensities.of.preferences$strong,
            weak.intensities.of.prefs = intensities.of.preferences$weak, 
            indif.intensities.of.prefs = intensities.of.preferences$indif,
            rank.related.requirements = rank.related.preferences, 
            nums.of.characteristic.points = nums.of.characteristic.points,
            improvement=TRUE)
        } else {
          result <- findPerformanceModificationForPossibleRelation(
            perf=performances$data, a = target.variant.a, b = target.variant.b,
            strict.vf = strict,
            strong.prefs = preferences$strong, weak.prefs=preferences$weak, 
            indif.prefs=preferences$indif, 
            strong.intensities.of.prefs = intensities.of.preferences$strong, 
            weak.intensities.of.prefs = intensities.of.preferences$weak,
            indif.intensities.of.prefs = intensities.of.preferences$indif,
            rank.related.requirements=rank.related.preferences, 
            nums.of.characteristic.points=nums.of.characteristic.points,
            precision = precision,
            which.attributes = selected.attributes, 
            greater.than.one = TRUE)
        }
      } 
    }, silent=TRUE
  )  
  if (inherits(tmpErr, 'try-error') || (("status" %in% result) && (result$status != "OK")) || (is.null(result$result))){
    errCalc<<-"Can't find a result."
  } else {
    execFlag<-TRUE
  }
}
setwd(outDirectory)
if (execFlag) {
  tree = newXMLDoc()
  newXMLNode("xmcda:XMCDA",
             attrs=c("xsi:schemaLocation" = "http://www.decision-deck.org/2012/XMCDA-2.2.0 http://www.decision-deck.org/xmcda/_downloads/XMCDA-2.2.0.xsd"),
             suppressNamespaceWarning=TRUE,
             namespace = c("xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmcda" = "http://www.decision-deck.org/2012/XMCDA-2.2.0"),
             parent=tree)
  putAlternativeValue(tree, result$result, target.variant.a, mcdaConcept=NULL)
  saveXML(tree, file=result.file)

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
