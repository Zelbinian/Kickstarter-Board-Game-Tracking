library(rvest)
library(stringr)
library(tidyverse)
library(R.utils)

sleeptime__ <- 5

# get active boardgame projects from Kicktraq

extractProjectInfo <- function(textblob, toExtract) {
  extracted <- grep(toExtract, textblob, fixed = TRUE, value = TRUE) %>%
    strsplit(":") %>% unlist() %>% .[2] %>% trimws()
  
  if (length(extracted) == 0) return(NA)
  else return(extracted)
}

scrapeProjectInfo <- function(ktURLs) {
  
  fundingGoal <- character(0)
  amntPledged <- character(0)
  fundingPcnt <- character(0)
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
      fundingInfo <- extractProjectInfo(projectPageInfo, "Funding:") %>% str_split(" of ") %>% unlist()
      fundingGoal <- c(fundingGoal, fundingInfo[2])
      amntPledged <- c(amntPledged, fundingInfo[1])
      fundingPcnt <- c(fundingPcnt, 
                      projectPage %>% html_node("#project-pledgilizer-top a") %>% html_attr("title") %>%
                        sub("%", "", .) %>% as.numeric()) # this part converts the text pcnt string to an integer
      ksURLs <- c(ksURLs, thisKsUrl)
      
      print(paste("There are now",length(ksURLs),"items processed."))
    } 
    
    Sys.sleep(sleeptime__) # try not to hammer their server
  }
  
  return(list("url"=ksURLs, "fundingGoal"=fundingGoal, "amntPledged"=amntPledged, "fundingPcnt"=fundingPcnt))
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
          "Funding Goal"=prj_info$fundingGoal,
          "Amount Pledged"=prj_info$amntPledged,
          "Funding Percent"=prj_info$fundingPcnt) %>%
  return()
}

scrapeKicktraq <- function(startPage = 1) {
  
  ktBgProjListUrl <- "http://www.kicktraq.com/categories/games/tabletop%20games"
  pageMod <- "?page="
  
  outputData <- tibble("Title"=character(0),
                       "URL"=character(0),
                       "Description"=character(0),
                       "Funding Goal"=character(0),
                       "Amount Pledged"=character(0),
                       "Funding Percent"=numeric(0))
  
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
  
}

ktData <- scrapeKicktraq()
