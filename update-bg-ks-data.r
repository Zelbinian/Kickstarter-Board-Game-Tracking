library(httr)
library(rvest)
library(stringr)
library(tidyverse)
library(jsonlite)
library(magrittr)
library(lubridate)
library(R.utils)
library(tkclt)

sleeptime__ <- 5

# GET ACTIVE BG PROJECTS ON KICKTRAQ

# Helper function for extracting details from Kicktraq's textblob
extractProjectInfo <- function(textblob, toExtract) {
  extracted <- grep(toExtract, textblob, fixed = TRUE, value = TRUE) %>%
    strsplit(":") %>% unlist() %>% .[2] %>% trimws()
  
  if (length(extracted) == 0) return(NA)
  else return(extracted)
}

# Helper function that's essentially a switch statement to detect the current state
# of a project
determineProjectStatus <- function(attrString) {
  if (grepl("rblue", attrString)) {
    return("Active") 
  } else if (grepl("rgreen", attrString)) {
    return("Funded")
  } else if (grepl("rorange", attrString)) {
    return("Failed") 
  } else if (grepl("rred", attrString)) {
    return ("Cancelled")
  }
}

# Takes a kicktraq project page (or pages) as input and spits out the relevant project
# data they contain
scrapeProjectInfo <- function(ktURLs) {
  
  title <- NULL
  status <- NULL
  description <- NULL
  backers <- NULL
  avgDailyPledges <- NULL
  avgPledge <- NULL
  fundingGoal <- NULL
  curFunding <- NULL
  fundingPcnt <- NULL
  
  for(url in ktURLs) {
    
    # get data from page
    ktResp <- RETRY(verb = "GET",
                         url = url,
                         body = FALSE,
                         times = 5) 
    
    # if we got a good response, keep going, otherwise harmlessly return the empty lists
  
    if (ktResp$status_code == 200) {
      
      projectPage <- content(ktResp)
      
      # If the project doesn't exist or has been deleted, kicktraq doesn't give a 404, instead
      # they try to dynamically generate a new page. The way to check for this is to check to see
      # if content that should be there is not
      if (length(projectPage %>% html_node("#button-backthis")) > 0) {
        # yay! page exists! 
        print(paste("Processing", url))
        
        projectPageInfo <- projectPage %>%  
          html_node("#project-info-text") %>%   #selects the div with the project details in it
          html_text() %>%                     #pulling the text out
          strsplit('\n', fixed = TRUE) %>%                     #storing each peice of data separately
          unlist() %>%
          trimws() %>%                            # Trimming white space to make life easier later
          .[. != ""]                          # eliminating empty vector entries
        
        # adding new data to the vectors
        title <- 
          c(title, 
            projectPage %>% html_node("h2") %>% html_text())
        status <- 
          c(status,
            projectPage %>% html_node(".ribbon-inner") %>% html_attrs() %>% determineProjectStatus())
        description <- 
          c(description,
            projectPageInfo[1])
        backers <- 
          c(backers,
            extractProjectInfo(projectPageInfo, "Backers: ") %>% as.integer())
        avgDailyPledges <- 
          c(avgDailyPledges,
            extractProjectInfo(projectPageInfo, "Pledges: "))
        avgPledge <- 
          c(avgPledge,
            extractProjectInfo(projectPageInfo, "Per Backer: "))
        fundingGoal <- 
          c(fundingGoal,
            extractProjectInfo(projectPageInfo, "Funding: ") %>% str_split(" of ", simplify = TRUE) %>% .[1])
        curFunding <- 
          c(curFunding,
            extractProjectInfo(projectPageInfo, "Funding: ") %>% str_split(" of ", simplify = TRUE) %>% .[2])
        fundingPcnt <- 
          c(fundingPcnt, 
            projectPage %>% html_node("#project-pledgilizer-top a") %>% html_attr("title") %>%
              sub("%", "", .) %>% as.integer()) # this part converts the text pcnt string to an integer
      
      } 
      
    } else {
      message_for_status(ktResp, paste("retrieve",url,"from Kicktraq, processing skipped."))
    }
  }
  
  # put the filled fectors in a named list
  return(list("title"=title, "status"=status, "description"=description, "backers"=backers, 
              "avgDailyPledges"=avgDailyPledges, "avgPledge"=avgPledge, "fundingGoal"=fundingGoal,
              "curFunding"=curFunding, "fundingPcnt"=fundingPcnt))
}

# GET KICKSTARTER PROJECTS CURRENTLY LOGGED IN AIRTABLE

queryAirtable <- function() {
    
  atData <- tibble("ID"=character(0), 
                   "Name"=character(0),
                   "Campaign Link"=character(0),
                   "Funded"=logical(0))
  
  curOffset <- NULL
    
  while(nrow(atData) == 0 || !is.null(curOffset)) {
    
    atResp <- RETRY(verb = "GET", 
                    url = "https://api.airtable.com/v0/app39KNHnKwNQrMC4/Campaign%20List",
                    query = list(view = "Data Entry", api_key = "keyiM4nxBFTZDjAPI", offset = curOffset), 
                    body = FALSE,
                    times = 5)
    
    stop_for_status(atResp, paste("retrieve AirTable data from", atResp$request$url))
    
    atJSON <- content(atResp, "text") %>% fromJSON()
    
    atData %<>% add_row("ID"=atJSON$records$id, 
                        "Name"=atJSON$records$fields$Name,
                        "Campaign Link"=atJSON$records$fields$`Campaign Link`,
                        "Funded"=atJSON$records$fields$Funded)
    
    curOffset <- atJSON$offset
    
    Sys.sleep(.25)
  }
  
  return(atData)
}

updateAirtable <- function(atData) {
    
    pb <- tkProgressBar(min = 0, max = nrow(atData), width = 300, title = "Updating Airtable Data")
    
    for(i in 1:nrow(atData)) {
        
        setTkProgressBar(pb, i, label = paste(round(i/nrow(atData)*100,2),"% done"))
        
        curRecord <- atData[i,]
        
        # retrieve the kicktraq page information
        ktPageData <- curRecord$`Campaign Link` %>% sub("starter", "traq", .) %>% scrapeProjectInfo()
        
        # piece together the update string
        updateString <- paste('"Backers":', ktPageData$backers)
        
        if(ktPageData$status == "Cancelled") updateString <- c(updateString, '"Cancelled": true', paste0('"End Date": "', today(), '"'))
        
        if(ktPageData$fundingPcnt > 99) updateString <- c(updateString, '"Funded": true')
        
        updateString <- paste(updateString, collapse = ", ")
        
        reqUrl <- paste0("https://api.airtable.com/v0/app39KNHnKwNQrMC4/Campaign%20List/", curRecord$ID)
        resp <- RETRY(verb = "PATCH",
                      url = reqUrl,
                      query = list(api_key = "keyiM4nxBFTZDjAPI"), 
                      content_type_json(), 
                      body = paste('{"fields":{', updateString, '}}'),
                      times = 5)
        
        warning_for_status(resp)
        
        Sys.sleep(sleeptime__)
        
    }
    
    close(pb)
    
}

queryAirtable() %>% updateAirtable()