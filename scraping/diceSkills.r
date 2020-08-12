library(XML)
library(RCurl)
library(rjson)

parseSkillPage <- function(url, skillName){
  cat(" ...downloading page: ", paste("https://www.dice.com",url,sep=""))
  page <- htmlTreeParse(getURL(paste("https://www.dice.com",url,sep="")), useInternalNodes=TRUE)
  #Breadcrumb extraction
  breadcrumbBlock  <- xpathSApply(page, '/html/body/div[@class="skills-header"]//h5[@class="breadcrumb-links"]', xmlValue)
  if(is.null(breadcrumbBlock)){
    breadcrumbTag <- ""
  }else{
    breadcrumbTag <- gsub("\\s+", "", breadcrumbBlock)
  }
  skillDetails <- data.frame(skill=skillName,url=url, breadcrumb=breadcrumbTag)
  globalSkillDetails <<- rbind(globalSkillDetails,skillDetails)
  
  #Skill hierarchy and trends extraction
  headScripts  <- xpathSApply(page, '/html/head/script[not(@type)]', xmlValue)
  jsonSection <-tail(headScripts,1)
  if(nchar(jsonSection)>0){
    jsonSectionSplit <- strsplit(jsonSection,";")
    jsonFragmentWithSkills <- sub("^\\W*var \\w+ = ", "", jsonSectionSplit[[1]][1])
    if(nchar(trimws(jsonFragmentWithSkills))>0){
      parsedSkillhierarchy <- fromJSON(jsonFragmentWithSkills)
      if(length(parsedSkillhierarchy$children) > 0) {
        childrenSkills <- NULL
        for(i in 1:length(parsedSkillhierarchy$children)){
          childrenSkills <- c(childrenSkills, parsedSkillhierarchy$children[[i]]$name)
        }
        skillHierarchyDF <- data.frame(skill=rep(skillName,length(childrenSkills)),childrenSkills)
        globalSkilHierarchy<<-rbind(globalSkilHierarchy,skillHierarchyDF)
      }
    }
    
    
    jsonFragmentWithTrends <- sub("^\\W*var \\w+ = ", "", jsonSectionSplit[[1]][2])
    if(nchar(trimws(jsonFragmentWithTrends))>0){
      parsedSkillTimeSeries <- fromJSON(jsonFragmentWithTrends)
      if(length(parsedSkillTimeSeries) > 0) {
        values <- NULL
        years <- NULL
        for(i in 1:length(parsedSkillTimeSeries)){
          values <- c(values,parsedSkillTimeSeries[[i]])
          years <- c(years,names(parsedSkillTimeSeries[i]))
        }
        timeSeriesDF <- data.frame(skill=rep(skillName,length(years)),years,values)
        globalSkillTrends<<-rbind(globalSkillTrends,timeSeriesDF)
      }
    }
  }
}

globalSkilHierarchy <- NULL
globalSkillTrends <- NULL
globalSkillDetails <- NULL
setwd("~/Documents/Data/dice_skills_data")
indexlist <- c("dot", "1", "2", "3", "4", "5", "A", "B", "C", "D", "E", "F", "G", "H", "I","J","K","L","M","N", "O","P","Q","R","S","T","U","V","W","X","Y","Z")
for (i in 1:length(indexlist)) {  
  page <- htmlTreeParse(getURL(paste0("https://www.dice.com/skills/browse/",indexlist[i])), useInternalNodes=TRUE)
  elements  <- xpathSApply(page, '/html/body/div[@class="container"]/div[@class="row"]//div[@class="col-md-3"]/a')
  for (j in 1:length(elements)){
    skillName <- xpathSApply(elements[[j]],'.',xmlValue)
    skillUrl <- xpathSApply(elements[[j]],'.',xmlGetAttr,'href')
    cat('\n Skill: ',skillName, ', id: ', skillUrl)
    #if added to fix problem
    if (substr(skillName,0,13)!="SystemBuilder"){
      parseSkillPage(skillUrl,skillName)
    }
  }
  cat("...saving to disk")
  write.csv(globalSkillDetails, file = paste0(indexlist[i],"_skillDetails.csv"), row.names = FALSE)
  write.csv(globalSkillTrends, file = paste0(indexlist[i],"_skillTrends.csv"), row.names = FALSE)
  write.csv(globalSkilHierarchy, file = paste0(indexlist[i],"_skillHierarchy.csv"), row.names = FALSE)
  globalSkilHierarchy <- NULL
  globalSkillTrends <- NULL
  globalSkillDetails <- NULL
  cat("done!")
}




