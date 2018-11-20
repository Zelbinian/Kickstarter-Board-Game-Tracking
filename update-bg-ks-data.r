library(httr)
library(rvest)
library(stringr)
library(tidyverse)
library(jsonlite)
library(magrittr)
library(R.utils)

sleeptime__ <- 5

# GET ACTIVE BG PROJECTS ON KICKTRAQ

extractProjectInfo <- function(textblob, toExtract) {
  extracted <- grep(toExtract, textblob, fixed = TRUE, value = TRUE) %>%
    strsplit(":") %>% unlist() %>% .[2] %>% trimws()
  
  if (length(extracted) == 0) return(NA)
  else return(extracted)
}

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
      
      # Sys.sleep(sleeptime__) # try not to hammer their server
    } else {
      message_for_status(ktResp, paste("retrieve",url,"from Kicktraq, processing skipped."))
    }
  }
  
  return(list("title"=title, "status"=status, "description"=description, "backers"=backers, 
              "avgDailyPledges"=avgDailyPledges, "avgPledge"=avgPledge, "fundingGoal"=fundingGoal,
              "curFunding"=curFunding, "fundingPcnt"=fundingPcnt))
}

fetchProjectsData <- function(content, data) {
  
  # this is the meaty function, the thing that actually processes the scraped data
  ktURLs <- content %>% html_nodes("h2 a") %>% html_attr("href")
  prj_info <- scrapeProjectInfo(ktURLs)
  
  add_row(data,
          "Title"=content %>% html_nodes("h2 a") %>% html_text(),
          "URL"=prj_info$url,
          "Description"=content %>% html_nodes(".project-infobox > div:nth-child(2)") %>% html_text() %>%
            gsub("[\r\n]", "", .),
          "Funding Percent"=prj_info$fundingPcnt) %>%
  return()
}

# ktData <- scrapeKicktraq()

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
  }
  
  return(atData)
}

# FILTER DATA

# filter data sets, we only want unfunded projects from AirTable, and only funded projects from Kicktraq
# atData %<>% filter(is.na(Funded))
# ktData %<>% filter(`Funding Percent` > 99)

# Create reference IDs for each record in each tibble by sanitizing the URL
# atData %<>% mutate(uniqueid = str_match(`Campaign Link`, "([^?]+).*")[,2] %>% gsub("/", "", .) %>% sub("https?:", "", .))
# ktData %<>% mutate(uniqueid = str_match(URL, "([^?]+).*")[,2] %>% gsub("/", "", .) %>% sub("https?:", "", .))

# subset the AirTable Data further to select only the projects that need updating
# atData %<>% filter(uniqueid %in% ktData$uniqueid)

# UPDATE AIRTABLE

for(record in atData$ID) {
  reqUrl <- paste0("https://api.airtable.com/v0/app39KNHnKwNQrMC4/Campaign%20List/", record)
  resp <- PATCH(reqUrl,
        query = list(api_key = "keyiM4nxBFTZDjAPI"), 
        content_type_json(), 
        body = '{"fields":{"Funded": true}}')
  
  # print out req url and status
  cat(paste("Updating funding status for",resp$request$url))
  if(resp$status_code == 200) {
    cat(" was a success.\n")
  } else {
    cat(paste(" failed. Error, status code", resp$status_code,"\n"))
  }
}


for(i in 1:nrow(head(atData))) {
  
  curRecord <- atData[i,]

  # retrieve and disassemble kicktraq page

  ktPage <- RETRY(verb = "GET",
                  url = curRecord$`Campaign Link` %>% sub("starter", "traq", .),
                  body = FALSE,
                  times = 5)
  
  print(paste(ktPage$request$url, ktPage$status_code))
  
  

  Sys.sleep(sleeptime__)
  
}