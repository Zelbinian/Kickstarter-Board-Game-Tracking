# I maintain an AirTable database of board games on Kickstarter, past, present, and future.
# This Rscript helps keep the data fresh by periodically updating it with the newest information
# from Kicktraq.

library(httr)
library(tidyr)
library(stringr)
library(tibble)
library(jsonlite)
library(magrittr)
library(lubridate)
library(dplyr)
library(tcltk)

sleeptime__ <- 1

# Takes in a country code and an amount and prints out an appropriate currency string with the correct currency symbol

priceString <- function(currency, amount) {
  if (currency == "USD") {
    return(paste0("$", prettyNum(amount, ",")))
  }
  
  if (currency == "MXN") {
    return(paste0("MX$", prettyNum(amount, ",")))
  }
  
  if (currency == "EUR") {
    return(paste0("\u20AC", prettyNum(amount, ",")))
  }
  
  if (currency == "GBP") {
    return(paste0("\u00A3", prettyNum(amount, ",")))
  }
  
  if (currency == "CAD") {
    return(paste0("C$",prettyNum(amount, ",")))
  }
  
  if (currency == "SEK") {
    return(paste0("kr", prettyNum(amount, ","), " SEK"))
  }
  
  if (currency == "PLN") {
    return(paste0(prettyNum(amount, ","), " zl"))
  }
  
  if (currency == "CHF") {
    return(paste0(currency, prettyNum(amount, ",")))
  }
  
  if (currency == "SGD") {
    return(paste0("S$", prettyNum(amount, ",")))
  }
  
  if (currency == "JPY") {
    return(paste0("\u00A5", prettyNum(amount, ",")))
  }
  
  if (currency == "NZD") {
    return(paste0("NZ$", prettyNum(amount, ",")))
  }
  
  if (currency == "HKD") {
    return(paste0("HK$", prettyNum(amount, ",")))
  }
  
  if (currency == "AUD") {
    return(paste0("A$", prettyNum(amount, ",")))
  }
  
  if (currency == "DKK") {
    return(paste0("kr", prettyNum(amount, ",")," DKK"))
  }
  
  if (currency == "INR") {
    return(paste0("\u20B9",prettyNum(amount, ",")))
  }
  
  if (currency == "NOK") {
    return(paste0("kr", prettyNum(amount, ","), " NOK"))
  }
  
  stop(paste("Currency", currency,"not recognized"))
  
}

processKicktraq <- function(apiKey) {
  ktData <- tibble("Name"=character(),
                   "KTID"=character(),
                   "Description"=character(),
                   "Campaign Link"=character(),
                   "Launch Date"=Date(),
                   "End Date"=Date(),
                   "Funded"=logical(),
                   "Backers"=numeric(),
                   "Current Funding"=character(),
                   "Funding Percent"=numeric(),
                   "Avg Pledge"=character(),
                   "Cancelled"=logical())
  
  
  # get kicktraq data from custom endpoint
  
  ktResp <- RETRY(verb = "GET", 
                  url = "http://api.kicktraq.com/zelbinian/raw/0.1/",
                  query = list(key = apiKey), 
                  body = FALSE,
                  times = 5)
  
  stop_for_status(ktResp, paste(ktResp$status_code, ktResp$request$url))
  
  ktJSON <- content(ktResp, "text") %>% fromJSON()
  
  # focus only on the nodes with game info
  
  projects <- c(ktJSON$data$Games, ktJSON$data$`Tabletop Games`, ktJSON$data$`Playing Cards`)
  
  
  for(p in projects) {
    
    name <- p$name
    raised <- as.integer(p$raised)
    goal <- as.integer(p$goal)
    
    if (str_detect(name, "(Cancelled)")) {
      cancelled = TRUE
      p$name %<>% str_remove(fixed(" (Cancelled)"))
      p$end <- now()
    } else {
      cancelled = FALSE
    }
    
    name %<>% str_replace_all(fixed("|"), "-")
    
    ktData %<>% add_row("Name" = name,
                        "KTID" = p$uuid,
                        "Description" = p$description %>% str_replace_all("[\r\n]", "") %>% str_replace_all("\\|", "-"),
                        "Campaign Link" = p$url$kickstarter,
                        "Launch Date" = dmy_hms(p$start) %>% floor_date("minute"),
                        "End Date" = dmy_hms(p$end) %>% floor_date("minute"),
                        "Funded" = ifelse(raised >= goal, TRUE, FALSE),
                        "Avg Pledge" = priceString(p$currency, p$avg_pledge),
                        "Backers" = as.integer(p$backers),
                        "Current Funding" = paste(priceString(p$currency, raised),"of", priceString(p$currency, goal)),
                        "Funding Percent" = round(raised/goal * 100),
                        "Cancelled" = cancelled)
    
  }
  
  return (ktData)
}

queryAirtable <- function(viewChoice = "Data Entry", apiKey) {
  
  # building a blank Tibble to add rows to later
  atData <- tibble("ID"=character(),
                   "KTID"=character(),
                   "Name"=character(),
                   "Description"=character(),
                   "Metadata"=list(),
                   "Campaign Link"=character(),
                   "BGG Link"=character(),
                   "Launch Date"=ymd(),
                   "End Date"=ymd(),
                   "Players"=character(),
                   "Funded"=logical(),
                   "Min Pledge"=numeric(),
                   "Avg Pledge"=character(),
                   "Backers"=numeric(),
                   "Current Funding"=character(),
                   "Funding Percent"=numeric())
  
  curOffset <- NULL
  
  while(nrow(atData) == 0 || !is.null(curOffset)) {
    
    atResp <- RETRY(verb = "GET", 
                    url = "https://api.airtable.com/v0/app39KNHnKwNQrMC4/Campaign%20List",
                    query = list(view = viewChoice, api_key = apiKey, offset = curOffset), 
                    body = FALSE,
                    times = 5)
    
    stop_for_status(atResp, paste("retrieve AirTable data from", atResp$request$url))
    
    atJSON <- content(atResp, "text") %>% fromJSON()
    
    if(is.null(atJSON$records$fields$Funded)) {
      atJSON$records$fields$Funded <- NA
    }
    
    if(is.null(atJSON$records$fields$`BGG Link`)) {
      atJSON$records$fields$`BGG Link` <- NA
    }
    
    if(is.null(atJSON$records$fields$Metadata)) {
      atJSON$records$fields$Metadata <- NA
    }
    
    atData %<>% add_row("ID"=atJSON$records$id,
                        "KTID"=atJSON$records$fields$KTID,
                        "Name"=atJSON$records$fields$Name,
                        "Description"=atJSON$records$fields$Description,
                        "Metadata"=atJSON$records$fields$Metadata,
                        "Campaign Link"=atJSON$records$fields$`Campaign Link`,
                        "BGG Link"=atJSON$records$fields$`BGG Link`,
                        "Launch Date"=atJSON$records$fields$`Launch Date` %>% ymd_hms(),
                        "End Date"=atJSON$records$fields$`End Date` %>% ymd_hms(),
                        "Players"=atJSON$records$fields$Players,
                        "Funded"=ifelse(is.na(atJSON$records$fields$Funded), FALSE, TRUE),
                        "Min Pledge"=atJSON$records$fields$`Min Pledge`,
                        "Avg Pledge"=atJSON$records$fields$`Avg Pledge`,
                        "Backers"=atJSON$records$fields$Backers,
                        "Current Funding"=atJSON$records$fields$`Current Funding`,
                        "Funding Percent"=atJSON$records$fields$`Funding Percent`)
    
    curOffset <- atJSON$offset
    
    Sys.sleep(sleeptime__)
  }
  
  return(atData)
}

addToAirtable <- function(ktData, atKey, focusDate = today() - 1) {
  
  newProj <- ktData %>% filter(date(`Launch Date`) == focusDate) 
  
  if (nrow(newProj) == 0 ) {
    
    print("No new projects to add! Take the day off ;)")
    
  } else {
  
    newProj %>% select(Name, Description) %>% print(n = Inf)
    uResp <- readline("Which of these projects should be added to the database? ")
    
    if (uResp != "") {
      
      # we want to add some proejcts, the only difference is one or many
      # if one, we don't need to do anything, if there's many, we just need to process
      # the user input and turn it into a vector
      
      uResp <- str_split(uResp, ",")  %>% unlist()
      
      uRespLen <- length(uResp)
      
      # transform the selected rows into JSON
      # select(Name, KTID, Description, `Campaign Link`, `Launch Date`, `End Date`)
      
      dbUpdateStr <- '{"records": ['
      
      loop <- 1
      
      for (record in uResp) {
        
        projJSON <- newProj[record,] %>% select(Name, KTID, Description, `Campaign Link`, `Launch Date`, `End Date`,
                                                Funded, `Avg Pledge`, Backers, `Current Funding`, 
                                                `Funding Percent`, Cancelled) %>% toJSON()
        projJSON <- str_remove_all(projJSON, '\\[|\\]')
        
        dbUpdateStr <- paste(dbUpdateStr, 
                             '{ "fields":', projJSON, '}')
        
        if (loop != uRespLen) {
          dbUpdateStr <- paste0(dbUpdateStr, ",")
          loop <- loop + 1
        }
        
      }
      
      dbUpdateStr <- paste(dbUpdateStr, "]}")
      
      # use the JSON to update the airtable database 
      
      return(RETRY(verb = "POST", 
                      url = "https://api.airtable.com/v0/app39KNHnKwNQrMC4/Campaign%20List",
                      add_headers(Authorization = paste("Bearer", atKey)),
                      content_type_json(),
                      body = dbUpdateStr,
                      times = 2))
      
      
    } else {
      
      print("No projects have been added to the database.")
      
    }
  }
}

updateAirtable <- function(ktData, atKey) {
  
  # collect the data
  # also filter the data and select just what we need to join it
  #ktData <- processKicktraq(ktKey) %>% select(!Name)
  atData <- queryAirtable(apiKey = atKey) %>% filter(`End Date` > now()) %>% 
    select(ID, KTID, Name)
  joinedData <- left_join(atData, ktData %>% select(!Name), by = "KTID")
  
  manualUpdates <- joinedData %>% filter(is.na(Description) && date(`End Date`) == today())
  
  if(nrow(manualUpdates > 0)) {
    print(paste(nrow(manualUpdates), "projects need to be updated manually."))
    
    outputFile <- file("manualUpdates.txt", open = "w+", encoding = "native.enc")
    
    writeLines(as.character(now()), outputFile)
    writeLines("-------", outputFile)
    manualUpdates %>% select(Name, KTID) %>% write.table(outputFile)
    
    close(outputFile)
  }
  
  #joinedData %<>% filter(!is.na(Name))
  
  pb <- tkProgressBar(min = 0, max = nrow(joinedData), width = 500, title = "Updating Airtable Data")
  
  for(i in 1:nrow(joinedData)) {
    
    curRecord <- joinedData[i,]
    
    setTkProgressBar(pb, i, label = paste0("Processing (", i, "/", nrow(joinedData), "): "))
    
    if(is.na(curRecord$Description)) {
      curRecord$Cancelled <- TRUE
      curRecord$`End Date` <- today()
    }
    
    # prepare the update string
    updateString <- curRecord %>% select(Description, `End Date`, Funded, `Avg Pledge`, Backers, `Current Funding`, 
                                         `Funding Percent`, Cancelled) %>% toJSON()
    updateString <- str_remove_all(updateString, '\\[|\\]')
    updateString <- paste('{"fields":', updateString, '}')
    
    # use the update string to 
    resp <- RETRY(verb = "PATCH",
                  url = paste0("https://api.airtable.com/v0/app39KNHnKwNQrMC4/Campaign%20List/", curRecord$ID),
                  add_headers(Authorization = paste("Bearer", atKey)), 
                  content_type_json(), 
                  body = updateString,
                  times = 5,
                  terminate_on = c(200, 422))
    
    warn_for_status(resp, paste("processing", curRecord))
    
    Sys.sleep(sleeptime__)
    
  }
  
  close(pb)
  
}

# produces "hashtags" from a vector of strings, dolled up in markdown code formatting 
hashtagify <- function(x) {
  
  x %<>% tolower() %>% gsub(" ", "", .) %>% paste0("`#",.,"`") %>% paste(., collapse = " ")
  
  return(x)
}

# we want to include a project if it has enough funding to possibly succeed, which depends
# on how much time it has left to secure backers
fundingPossible <- function(fundingAmount, endDate, startDate = today(), thMax = 70, perDayPenalty = 3.58) {
  
  daysUntilEnd <- (startDate %--% endDate) / days()
  threshold <- thMax - (daysUntilEnd * perDayPenalty)
  
  return (fundingAmount >= threshold) 
}

writePostTable <- function(data, kicktraq) {
  
  # posts a formatted version of the passed in data to the output file 
  writeLines("Project Info|Players|Backers|Min / Avg Pledge|Ends|Comments\n:--|:--|:--|:--|:--|:--", outputFile, useBytes = T)
  
  checkmark <- "\u2611"
  
  for(i in 1:nrow(data)) {
    curRecord <- data[i,]
    
    # Project info is the most complicated column as it's calculated from many columns from the source data
    projectInfo <- paste0("**[",curRecord$Name,"](",curRecord$`Campaign Link`,")** ",
                          curRecord$Description,
                          " // *",
                          ifelse(curRecord$Funded == TRUE, 
                                 # if funded, bold the funding info add a neat little checkmark
                                 paste0("*Has raised ", curRecord$`Current Funding`, " so far. (~", curRecord$`Funding Percent`, "%) ", checkmark, "*"), 
                                 # if not skip bolding and display percentage
                                 paste0("Has raised ", curRecord$`Current Funding`, " so far. (~", curRecord$`Funding Percent`, "%)")),
                          "*") 
    
    # comments are too complicated to attempt in-place in a cat statement, this will stitch together a comment string
    # if certain conditions are met
    comments <- ""
    
    if (kicktraq) {
      comments <- c(comments, 
                    paste0("[kicktraq](",curRecord$`Campaign Link` %>% sub("starter", "traq", .), ")"))
    }
    
    if(!is.na(curRecord$`BGG Link`)) {
      comments <- c(comments, 
                    paste0("[bgg](",curRecord$`BGG Link`, ")"))
    }
    
    if(!is.na(curRecord$Metadata)) {
      # unlist
      metadata <- curRecord$Metadata %>% unlist() 
      # check if null
      if (!is.null(metadata)) {
        
        # for each element in the vector, hashtagify
        comments <- c(comments, hashtagify(metadata))
      }
    }
    
    # to make it easy to read, each line below is a column in the table
    paste(projectInfo,                                                            # Project Info                          
          curRecord$Players,                                                     # Players
          curRecord$Backers,                                                      # Backers
          paste0("$", curRecord$`Min Pledge`, " / ",curRecord$`Avg Pledge`),      # Pledges
          strftime(curRecord$`End Date`, format = "%b %d"),                       # Ends
          paste(comments, collapse = ' '),                                        # Comments
          sep="|") %>% writeLines(outputFile, useBytes = T)
  }
  
}

# this function is effectively the script

createKsPost <- function(atKey, begDate = today()) {
  
  data <- queryAirtable(apiKey = atKey)
  
  endInterval <- interval(begDate + days(1), begDate + weeks(1)) # the "ending soon" list consists of projects that will end within 7 days after the posting date of the list
  newInterval <- interval(begDate - weeks(1), begDate - days(1)) # the "new" list consists of projects with startdates within the 7 days prior to the posting date of the list
  
  
  paste("## What this is:\n\n",
        "This is a weekly, curated listing of Kickstarter board game projects that are either:\n\n",
        "- **newly posted in the past 7 days**, or\n",
        "- **ending in the next 7 days (starting ", strftime(int_start(endInterval), format = "%b %d"), ")**",
        " and have at least a fighting chance of being funded.\n\n",
        "All board game projects meeting those criteria will automatically be included, **no need to ask.** (The occasional non-board game project may also sneak in!)\n\n",
        "Expect new lists each Sunday sometime between midnight and noon PST.\n*****\n",
        "## Ending Soon", 
        sep="") %>% writeLines(outputFile, useBytes = T)
  
  # write the projects that end within the endInterval out to the file in Markdown formatm in chronological order
  writePostTable(data = data %>% filter(date(`End Date`) %within% endInterval, fundingPossible(`Funding Percent`, `End Date`)) %>% arrange(`End Date`), 
                 kicktraq = T)
  
  writeLines("## New This Week", outputFile, useBytes = T)
  
  # write the projects that launched within the newInterval out to the file in Markdown format, in alphabetical order
  writePostTable(data = data %>% filter(date(`Launch Date`) %within% newInterval) %>% arrange(Name),
                 kicktraq = F) 
  
  # write the post footer and then close the file stream
  paste("## Need moar Kickstarter goodness?\n",
        "Check out... \n\n",
        "- BoardGameGeek's variety of [Kickstarter-oriented Geeklists](https://boardgamegeek.com/geeklist/166152/kickstarter-project-metalist)\n",
        "- [Kicktraq's data-driven views](https://www.kicktraq.com/categories/games/tabletop%20games/)\n\n",
        "## Footnotes\n", 
        "- `#hmm` means that something about the project seems a little off. Buyer beware kinda thing.\n", 
        "- `#lolwut` is reserved for projects that seem like copycat games or campaigns put together with little thought. Check 'em out for amusement.\n", 
        "- `#take` tags are for projects that have been restarted for some reason, with the number indicating what iteration we're currently on.\n",
        "- `#reprint` when used along with `#expansion` indicates this campaign allows you to pledge for the base game by itself. \n",
        "- `#bling` tags are for accessories, upgrades, sleeves, toys, tables, etc.\n",
        "- `#dtpick` tags identify the games the various Dice Tower folks identified as their pick of the week\n",
        "- Did I miss something? Particularly something **new in the last 7 days** or **ending in the next 7 days**?", 
        " Let me know in the comments and I'll add it in.\n\n", 
        "## Tip Jar\n",
        "If you enjoy these lists, maybe [toss me a buck](https://www.paypal.me/Zelbinian/1) now and then.",
        " [Signing up for a free AirTable account](https://airtable.com/invite/r/wJL1rj8U) via my referral link",
        " can help, too. Plus, it's swell!\n",
        sep="") %>% writeLines(outputFile, useBytes = T)
  
  return(data)
}


# prep steps
# kicktraqData <- processKicktraq()
# addToAirTable(kicktraqData)
# updateAirtable()

# open the connection
#outputFile <- file("kspost.txt", open = "w+", encoding = "native.enc")

# gather data , write out and close the connection
#tryCatch(postData <- createKsPost(), finally = close(outputFile))