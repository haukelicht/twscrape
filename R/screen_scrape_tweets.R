#' Scroll for more tweets
#' 
#' @description Function takes remote driver currently at a Twitter users' stream,
#'      and scrolls till no more new tweets, replies, quotes, etc.
#'      are loaded (i.e., until the "Back to top ↑" icon is displayed).
#'      It then extracts all tweet IDs from the HTML source (tag 'div.tweet', attribute 'data-item-id'),
#'      and returns them as a character vector.
#'
#' @param account Twitter screen name or account ID
#' @param remdr an \bold{active} RSelenium \code{\link[RSelenium]{remoteDriver}} object 
#'     (check \code{remdr$getStatus()} to see if the driver is running.)
#' @param max.scrolls Maximun number of times down-scrolling is tried. Defaults to 30.
#' @param sleep number of second to pause between down-scroll trials. Defaults to .5
#'
#' @return A character vector of tweet IDs.
#' 
#' @import RSelenium 
#' @import dplyr
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes
#' @importFrom rvest html_attr
scroll_more_tweets <- function(account, remdr, max.scrolls = 30L, sleep = .5){  
  
  # get current window size
  ws <- remdr$getWindowSize()

  # fix very small height (otherwise scrolling will have no effect)
  remdr$setWindowSize(1000, 10)

  # scroll down until not loading anymore data
  i_ <- 1L
  while(i_ <= max.scrolls){
    stream_loading <- remdr$findElement("class", "stream-loading")
    if (!stream_loading$isElementDisplayed()[[1]])
      break
    
    remdr$executeScript("window.scrollBy(0, document.body.scrollHeight);")
    Sys.sleep(sleep)
    
    i_ <- i_ + 1L
  }
  
  # reset size
  remdr$setWindowSize(ws$x, ws$y)
  
  # get tweet IDs
  ids <- tryCatch(
    remdr$getPageSource()[[1]] %>% 
      xml2::read_html(.) %>% 
      rvest::html_nodes("div.tweet") %>% 
      rvest::html_attr("data-item-id")
    , error = function(err) err
  )
  
  if (inherits(ids, "error"))
    stop("Could not get tweet IDs. Reason: ", ids$message)
  
  return(ids)
}

#' Scrape Tweets from screen
#' 
#' @description Given a twitter account screen name or ID, and start and end dates, 
#'      function screen-scrapes IDs of historical tweets in time range and returns them
#'      in a data frame.
#'      Optionally, the scraped IDs can additionally be written to disk (if \code{write.out = TRUE}). 
#'      
#' @section WARNING:
#' \itemize{
#'     \item Function presuposses an active remote Selenium driver.     
#'     \item Function only accepts dates in format '\%Y-\%m-\%d' (Year-month-day: 'YYYY-mm-dd')     
#' }
#' 
#' @param tw.account a scalar character vector, specifying a Twitter screen name or account ID
#' 
#' @param remdr an \bold{active} RSelenium \code{\link[RSelenium]{remoteDriver}} object 
#'     (check \code{remdr$getStatus()} to see if the driver is running.)
#'
#' @param since.date create date of oldest tweets to get 
#'     Only accepts dates in format '\%Y-\%m-\%d' (Year-month-day: 'YYYY-mm-dd')
#'     
#' @param until.date create date of most recent (youngest) tweets to get
#'     Only accepts dates in format '\%Y-\%m-\%d' (Year-month-day: 'YYYY-mm-dd')
#'     
#' @param date.interval date interval passed to 'by' argument of \code{\link[base]{seq.Date}}. Defaults to 'month'.
#'   
#' @param write.out logical. write out tweet IDs as JSON to disk?
#'     If \code{TRUE} (the default), JSON file will be written 
#'     to path \code{write.out.path} and named \code{write.out.name}.
#'     If \code{FALSE}, \code{write.out.path} and \code{write.out.name} will be ignored.
#' 
#' @param write.out.path Write out path (directory where to write scraped IDs file)
#'     Will be ignored if \code{write.out = FALSE}
#'     
#' @param write.out.name JSON file name.
#'     Defaults to 'tw_user_<\code{tw.account}>_tweet_ids_<\code{since.date}>_to_<\code{until.date}>.json' 
#'     Will be ignored if \code{write.out = FALSE}
#' 
#' @param sleep Seconds to pause between date ranges when iterating over date intervals defined by 
#'     \code{since.date}, \code{until.date} and \code{date.interval}.
#'     Defautls to .5
#' 
#' @param verbose logical. Print out status messages?
#' 
#' @return A \code{\link[tibble]{tibble}} data frame.
#'      The data frame is empty if an error occurs or no tweet IDs were scraped in the given time range.
#'      Otherwise it has columns 'account' (<chr>), 'since' (<date>), 'until' (<date>) and 'tweet_id' (<chr>), 
#'      and one row is one tweet.
#'
#' @importFrom lubridate ymd is.Date
#' @import RSelenium
#' @import dplyr
#' @importFrom purrr map2 map2_dfr set_names
#' @importFrom tidyr separate
#' @importFrom jsonlite toJSON write_json
scrape_tweets <- function(
  tw.account
  , remdr
  , since.date
  , until.date
  , date.interval = "month"
  , write.out = TRUE
  , write.out.path
  , write.out.name = sprintf("tw_user_%s_tweet_ids_%s.json", tw.account, paste0(since.date, "_to_", until.date))
  , sleep = .5
  , verbose = TRUE
){
  
  valid_dates <- tryCatch(validate_dates(since.date, until.date), error = function(err) err)
  
  if (inherits(valid_dates, "error") | any(nas_ <- lapply(valid_dates, is.na))){
    problems_ <- paste(c("since.date", "until.date")[which(nas_)], ", ")
    stop("Problem with dates passed to", problems_)
  } else {
    since.date <- valid_dates[[1]]
    until.date <- valid_dates[[2]]
  }

  stopifnot(since.date <= until.date)
  
  stopifnot(since.date <= Sys.Date())
  
  if (!verbose){
    message <- function(...) NULL
    on.exit(message <- base::message)
  }
  
  if (!tryCatch(remdr$getStatus()$ready, error = function(err) FALSE)) {
    remdr$open()
  }
  
  # construct date ranges vector
  dates <- tryCatch(
    seq.Date(since.date, until.date, date.interval)
    , error = function(err) err
  )
  
  if (inherits(dates, "error")){
    warning(
      "Failed constructng date range for account ", tw.account
      , "Reason: ", dates$message
      , ifelse(write.out, ". No data written to disk")
      , ". Returning empty data frame."
    )
    return(tibble())
  }
  
  if (!until.date %in% dates)
    dates <- c(dates, until.date)
  
  # create output container
  account_tweets <- list()
  
  message("Getting tweets for account ", tw.account, " from ", since.date, " until ", until.date, ".")
  
  # loop over date ranges
  for (i in 1:(length(dates)-1)){
    
    if (i > 1L)
      Sys.sleep(sleep)
    
    # get from and until dates
    from = dates[i]
    
    if (i == length(dates)-1){
      until = dates[i+1]
    } else {
      until = dates[i+1]-1
    }
    
    url_stem <- "https://twitter.com/search?l=&q=from%%3A%s%%20since%%3A%s%%20until%%3A%s&src=typd" 
    url <- sprintf(url_stem, tw.account, from, until)
    
    # navigate to URL
    remdr$navigate(url)
    
    # check if any tweets 
    stream_items <- remdr$findElements("class", "tweet")
    
    if (length(stream_items) == 0L) {
      ids <- character()
    } else {
      # scroll down until no more tweets
      ids <- tryCatch(
        scroll_more_tweets(tw.account, remdr, max.scrolls = 666L, sleep = .5)
        , error = function(err) err
      )
      
      if (inherits(ids, "error")){
        warning(
          "Failed scrolling for more tweets of account ", tw.account
          , " when parsing date range ", from, ":", until, ". "
          , ifelse(
            until == dates[length(dates)-1]
            , "Stopped getting old tweets for this account."
            , "Continue with next time window."
          )
        )
        break
      }
      
    }
    
    message("Got ", length(ids), " tweets for account ", tw.account, " from ", from, " until ", until, ".")
    
    account_tweets[[paste0(from, ":", until)]] <- ids
  }
  
  if (!is.null(write.out) && write.out && !is.null(write.out.path)) {
    
    if (!file.exists(write.out.path)) {
      warning("Cannot write data to disk. Connection '", write.out.path, "' does not exist.")
    } else {
      
      out <- purrr::map2(account_tweets, names(account_tweets), function(.x, .y) {
        list(
          since = sub("(.+):(.+)", "\\1", .y, perl = TRUE)
          , until = sub("(.+):(.+)", "\\2", .y, perl = TRUE)
          , ids = .x
        )
      }) %>% 
        list() %>% 
        purrr::set_names(tw.account)
      
      
      jsonlite::write_json(
        x = jsonlite::toJSON(out)
        , path = (out_path <- file.path(write.out.path, write.out.name))
      )
      
      message(
        "Wrote IDs of tweets by account ", tw.account
        , " from ", dates[1], " to ", dates[length(dates)]
        , " as JSON file to disk at '", out_path, "'."
      )
      
    }
  }
  
  # return as tibble data frame 
  purrr::map2_dfr(account_tweets, names(account_tweets), function(.x, .y, account = tw.account) {
    tibble(
      account
      , dr = .y
      , tweet_id = .x
    ) %>% 
      tidyr::separate(dr, c("since", "until"), sep = ":") %>% 
      mutate_at(2:3, ymd)
  })
  
}



#' Get a user's tweet IDs for a given date range
#' 
#' @description Given a start and end date, function looks for tweet ID files already to disk
#'    and gets tweet IDs for the remaining date range(s) by calling \link{scrape_tweets}.
#'
#' @param screen.name is the screen (or account) name of a twitter user
#' 
#' @param user.id is the user ID of a twitter user
#' 
#' @param remdr an \bold{active} RSelenium \code{\link[RSelenium]{remoteDriver}} object 
#'     (check \code{remdr$getStatus()} to see if the driver is running.)
#'     
#' @param since a date (format '\%Y-\%m-\%d'), specifying the start of the date range to be requested
#' 
#' @param until a date (format '\%Y-\%m-\%d'), specifying the end of the date range to be requested
#'
#' @param date.interval date interval passed to 'by' argument of \code{\link[base]{seq.Date}}. Defaults to 'year'.
#' 
#' @param sleep Seconds to pause between non-adjacent date ranges. Defautls to 2.
#'     
#' @param .write.out logical. write out tweet IDs as JSON to disk?
#'     If \code{TRUE} (the default), JSON file will be written 
#'     to path \code{.data.path}
#' 
#' @param .data.path Path to look at for existing tweet ID files
#'     Also the path where new ID files are written if \code{write.out = TRUE}.
#'     
#' @param .file.stem JSON file name stem (stem ignores date ranges)
#'     Defaults to glob 'tw_user_<\code{tw.account}>_tweet_ids_*.json'.
#'     Used both for looking for existing tweet ID JSON files, and to name new ones when writing to disk.
#'
#' @param verbose logical. Print out status messages?
#' 
#' @return
#' 
#' @importFrom purrr map_int
#' @importFrom lubridate ymd
#' @import dplyr
#' @import RSelenium
#' 
#' @export
get_user_tweet_ids_in_date_range <- function(
  screen.name
  , user.id
  , remdr
  , since
  , until
  , date.interval = "year"
  , sleep = 2
  , .write.out = TRUE
  , .data.path
  , .file.stem = paste0("tw_user_", user.id, "_tweet_ids_%s")
  , verbose = TRUE
){
  # validate input
  stopifnot(file.exists(.data.path))
  
  valid_dates <- tryCatch(validate_dates(since, until), error = function(err) err)
  
  if (inherits(valid_dates, "error") | any(nas_ <- lapply(valid_dates, is.na))){
    problems_ <- paste(c("since", "until")[which(nas_)], ", ")
    stop("Problem with dates passed to", problems_)
  } else {
    since <- valid_dates[[1]]
    until <- valid_dates[[2]]
  }
  
  if (!verbose){
    message <- function(...) NULL
    on.exit(message <- base::message)
  }
  
  # get user's tweet ID files that exist on disk 
  existing_data <- list.files(.data.path, sprintf(.file.stem, "*"), full.names = TRUE)
  
  # get date ranges to be requested
  dates <- strsplit(gsub(".+(\\d{4}-\\d{2}-\\d{2})_to_(\\d{4}-\\d{2}-\\d{2}).+", "\\1_\\2", existing_data), "_")
  req_date_ranges <- get_required_date_ranges(lubridate::ymd(sort(unique(unlist(dates)))), since, until) 
  
  if (is.null(req_date_ranges)) {
    message(
      "IDs in date range requested for user ", user.id, " were already scraped"
      , " (see files ", paste(existing_data, collapse = ", "), ")."
      , " Returning empty and continuing."
    )
    return(tibble())
  }
  
  # collect tweet IDs
  user_tweet_ids <- list()
  
  if (!tryCatch(remdr$getStatus()$ready, error = function(err) FALSE)) {
    remdr$open()
  }
  
  # ... for each date range to be requested
  for (dr in seq_along(req_date_ranges)) {
    
    if (dr > 1L)
      Sys.sleep(sleep)
    
    out_name <- sprintf(.file.stem, paste0(req_date_ranges[[dr]][1], "_to_", req_date_ranges[[dr]][2], ".json"))
    
    # try scrape tweet IDs from screen
    tweet_ids <- tryCatch(
      scrape_tweets_from_screen(
        tw.account = screen.name
        , remdr = remdr
        , since.date = req_date_ranges[[dr]][1]
        , until.date = req_date_ranges[[dr]][2]
        , date.interval = date.interval
        # write IDS to disk as JSON file
        , write.out = .write.out
        , write.out.name = out_name
        , write.out.path = .data.path
        , sleep = 1
        , verbose = verbose
      )
      , error = function(err) err
    )
    
    if (inherits(tweet_ids, "error")){
      warning(
        "Error when trying to get tweets for account ", screen.name 
        , " in date range ", paste(req_date_ranges[[dr]], collapse = " to ")
        , ". Reason: ", tweet_ids$message
        , ".\npassing empty  data frame and continuing."
        , call. = FALSE
      )
      user_tweet_ids[[dr]] <- tibble()
      next
    }
    
    # write IDS to output list
    user_tweet_ids[[dr]] <- tweet_ids %>% 
      rename(screen_name = account) %>% 
      mutate(user_id = user.id)
  }
  
  if ( length(user_tweet_ids) == 0L || all(purrr::map_int(user_tweet_ids, nrow) == 0L) ){
    warning(
      "Got no tweets for account ", screen.name 
      , ".\npassing empty data frame."
      , call. = FALSE
    )
    return(tibble())
  }
  
  # rbind output list and return data frame
  return(do.call(rbind, user_tweet_ids))
}


