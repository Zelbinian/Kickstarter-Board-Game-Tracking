library(httr)
library(rvest)
library(stringr)
library(tidyverse)
library(R.utils)

sleeptime__ <- 5

# GET ACTIVE BG PROJECTS ON KICKTRAQ

extractProjectInfo <- function(textblob, toExtract) {
  extracted <- grep(toExtract, textblob, fixed = TRUE, value = TRUE) %>%
    strsplit(":") %>% unlist() %>% .[2] %>% trimws()
  
  if (length(extracted) == 0) return(NA)
  else return(extracted)
}

scrapeProjectInfo <- function(ktURLs) {
  
  fundingPcnt <- integer(0)
  ksURLs <- character(0)
  
  for(url in ktURLs) {
    
    repeat{
      projectPage <- withTimeout(
        read_html(paste0("http://www.kicktraq.com",url)), timeout = sleeptime__ * 10
      )
      
      if(!is.null(projectPage)) break;
    }
    
    # first, grab the url for the actual Kickstarter project
    thisKsUrl <- projectPage %>% html_node("#button-backthis") %>% html_attr("href")
    
    # On occasion, the project page does disappear between grabbing the reference to
    # it on the project listing and trying to access it directly. It's bizarre.
    # When this happens, Kicktraq does not return a 404. Instead they generate
    # some dynamic placeholder page. These placeholder pages have none of the 
    # elements we're looking for, so the way we figure out if this happens is if
    # one of the attempts to grab them yeilds an empty list.
    if (length(thisKsUrl) > 0) {
      # yay! page exists! 
      print(paste("Processing",thisKsUrl))
      
      projectPageInfo <- projectPage %>%  
        html_node("#project-info-text") %>%   #selects the div with the project details in it
        html_text() %>%                     #pulling the text out
        strsplit('\n', fixed = TRUE) %>%                     #storing each peice of data separately
        unlist() %>%
        trimws()                            # Trimming white space to make life easier later
      
      # adding new data to the vectors
      fundingPcnt <- c(fundingPcnt, 
                      projectPage %>% html_node("#project-pledgilizer-top a") %>% html_attr("title") %>%
                        sub("%", "", .) %>% as.integer()) # this part converts the text pcnt string to an integer
      ksURLs <- c(ksURLs, thisKsUrl)
      
      print(paste("There are now",length(ksURLs),"items processed."))
    } 
    
    Sys.sleep(sleeptime__) # try not to hammer their server
  }
  
  return(list("url"=ksURLs, "fundingPcnt"=fundingPcnt))
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

scrapeKicktraq <- function(startPage = 1) {
  
  ktBgProjListUrl <- "http://www.kicktraq.com/categories/games/tabletop%20games"
  pageMod <- "?page="
  
  outputData <- tibble("Title"=character(0),
                       "URL"=character(0),
                       "Description"=character(0),
                       "Funding Percent"=integer(0))
  
  page <- startPage
  currentUrl <- paste0(ktBgProjListUrl, pageMod, page)
  
  repeat {
    webdata <- read_html(currentUrl)
    
    print(paste(currentUrl,"has been read."))
    
    # if we hit a page with no projects, that means we've consumed everything; break out of the loop
    if (webdata %>% html_nodes(".project") %>% length() == 0) break
    
    outputData <- fetchProjectsData(webdata, outputData)
    
    page <- page + 1
    currentUrl <- paste0(ktBgProjListUrl, pageMod, page)
  }
  
  return(outputData)
  
}

ktData <- scrapeKicktraq()

# GET KICKSTARTER PROJECTS CURRENTLY LOGGED IN AIRTABLE

# GET the data
atResp <- GET("https://api.airtable.com/v0/app39KNHnKwNQrMC4/Campaign%20List?view=Active%20Kickstarters",
             query = list(view = "Active Kickstarters", api_key = "keyiM4nxBFTZDjAPI", offset = NULL))

if(atResp$status_code == 200) {
  
  atJSON <- content(atResp, "text") %>% fromJSON()
  
  atData <- tibble("ID"=atJSON$records$id, 
                   "Campaign Link"=atJSON$records$fields$`Campaign Link`,
                   "Funded"=atJSON$records$fields$Funded)
  
  while(!is.null(atJSON$offset)) {
    atResp <- GET("https://api.airtable.com/v0/app39KNHnKwNQrMC4/Campaign%20List",
                  query = list(view = "Active Kickstarters", api_key = "keyiM4nxBFTZDjAPI", offset = atJSON$offset))
    
    atJSON <- content(atResp, "text") %>% fromJSON()
    
    atData %<>% add_row("ID"=atJSON$records$id, 
            "Campaign Link"=atJSON$records$fields$`Campaign Link`,
            "Funded"=atJSON$records$fields$Funded)
  }
  
} else {
  print(paste("AirTable returned Status Code", atResp$status_code, "for request", atResp$request$url))
}

# FILTER DATA

# filter data sets, we only want unfunded projects from AirTable, and only funded projects from Kicktraq
atData %<>% filter(is.na(Funded))
ktData %<>% filter(`Funding Percent` > 99)

# Create reference IDs for each record in each tibble by sanitizing the URL
atData %<>% mutate(uniqueid = str_match(`Campaign Link`, "([^?]+).*")[,2] %>% gsub("/", "", .) %>% sub("https?:", "", .))
ktData %<>% mutate(uniqueid = str_match(URL, "([^?]+).*")[,2] %>% gsub("/", "", .) %>% sub("https?:", "", .))

# subset the AirTable Data further to select only the projects that need updating
atData %<>% filter(uniqueid %in% ktData$uniqueid)

# UPDATE AIRTABLE

