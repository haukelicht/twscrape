#' Scroll for more tweets
#'
#' @description Function takes remote driver currently at a Twitter users' stream,
#'      and scrolls till no more new tweets, replies, quotes, etc.
#'      are loaded (i.e., until the "Back to top ↑" icon is displayed).
#'      It then extracts all tweet IDs from the HTML source (tag 'div.tweet', attribute 'data-item-id'),
#'      and returns them as a character vector.
#'
#' @section Scroll sleep:
#'    Argument \code{sleep} determines how much the Twitter timeline has to fully load.
#'    \bold{WARNING}: Setting low values (<.75 seconds) endangers not getting all tweet IDs,
#'    as the scraping process can be aborted prematurely due to too little scroll sleep.
#'    The default setting of .75 seconds is a minumum with fast internet connection.
#'
#' @section WARNING:
#'     Function presuposses an active remote Selenium driver.
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

  get_ids <- function(remdr) {
    remdr$getPageSource()[[1]] %>%
      xml2::read_html() %>%
      rvest::html_nodes("div.tweet") %>%
      rvest::html_attr("data-item-id")
  }

  # get current window size
  ws <- remdr$getWindowSize()

  # fix very small height (otherwise scrolling will have no effect)
  remdr$setWindowSize(1000, 300)
  # remdr$screenshot(display = T)

  # scroll down until not loading anymore data
  i_ <- 1L
  n_ids <- 0L
  while(i_ <= max.scrolls){

    ids <- tryCatch(get_ids(remdr), error = function(err) err)

    if (inherits(ids, "error"))
      break

    if (n_ids == length(ids)) {
      n_ids <- length(ids)
      remdr$executeScript("window.scrollBy(0, document.body.scrollHeight);")
      Sys.sleep(sleep*2)
      ids <- tryCatch(get_ids(remdr), error = function(err) err)
      if (n_ids == length(ids))
        break
    }

    n_ids <- length(ids)

    i_ <- i_ + 1L

    remdr$executeScript("window.scrollBy(0, document.body.scrollHeight);")
    Sys.sleep(sleep)
  }

  # reset size
  remdr$setWindowSize(ws$x, ws$y)

  if (inherits(ids, "error"))
    stop("Could not get tweet IDs. Reason: ", ids$message)

  return(ids)
}

#' Scrape Tweets IDs from screen
#'
#' @description Given a twitter account screen name or ID, and start and end dates,
#'      function screen-scrapes IDs of historical tweets in time range and returns them
#'      in a data frame.
#'      Optionally, the scraped IDs can additionally be written to disk (if \code{write.out = TRUE}).
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
#' @param max.tweets.pi maximum nuber of tweets per intevall to load.
#'     Defaults to 10'000. (See Dtails section)
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
#'     Defaults to .5 seconds
#'
#' @param .scroll.sleep Seconds to pause between scrolls when scrolling for more tweets. Defautls to .75 seconds.
#'     (See section 'scroll sleep' for details.)
#'
#' @param verbose logical. Print out status messages?
#'
#' @details
#'     Note that the maximum number of tweets loaded per date interval (\code{max.tweets.pi}) needs to be adapted to the date interval.
#'     Per scroll, 20 new tweets are loaded.
#'     By default, there comes a pause of .75 seconds between scrolls.
#'     This means that at maximum, waiting for 10'000 tweets to load takes \eqn{((10000/20) * .75)/60 = 6.25} minutes.
#'
#' @section Scroll sleep:
#'    Argument \code{.scroll.sleep} determines how much the Twitter timeline has to fully load.
#'    \bold{WARNING}: Setting low values (<.75 seconds) endangers not getting all tweet IDs,
#'    as the scraping process can be aborted prematurely due to too little scroll sleep.
#'    The default setting of .75 seconds is a minumum with fast internet connection.
#'
#' @section WARNING:
#' \itemize{
#'     \item Function presuposses an active remote Selenium driver.
#'     \item Function only accepts dates in format '\%Y-\%m-\%d' (Year-month-day: 'YYYY-mm-dd')
#' }
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
#'
#' @export
scrape_tweet_ids <- function(
  tw.account
  , remdr
  , since.date
  , until.date
  , date.interval = "month"
  , max.tweets.pi = 10000
  , write.out = TRUE
  , write.out.path
  , write.out.name = sprintf("tw_user_%s_tweet_ids_%s.json", tw.account, paste0(since.date, "_to_", until.date))
  , sleep = .5
  , .scroll.sleep = .75
  , verbose = TRUE
){

  if (!is.null(write.out) && write.out && !file.exists(write.out.path))
    stop("Cannot write data to disk. Connection '", write.out.path, "' does not exist.")

  valid_dates <- tryCatch(validate_dates(since.date, until.date), error = function(err) err)

  if (inherits(valid_dates, "error") | any(nas_ <- map_lgl(valid_dates, is.na))){
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
    stop(
      "Failed constructing date range for account ", tw.account
      , "Reason: ", dates$message
      , ifelse(write.out, ". No data written to disk")
    )
  }

  if (since.date == until.date | !until.date %in% dates)
    dates <- c(dates, until.date)

  # create output container
  account_tweets <- list()

  message("Getting tweet IDs for account ", tw.account, " from ", since.date, " until ", until.date, ".")

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

    # check if any tweet IDs
    stream_items <- remdr$findElements("class", "tweet")

    if (length(stream_items) == 0L) {
      ids <- character()
    } else {
      # scroll down until no more tweet IDs
      ids <- tryCatch(
        scroll_more_tweets(tw.account, remdr, max.scrolls = ceiling(max.tweets.pi/20), sleep = .scroll.sleep)
        , error = function(err) err
      )

      if (inherits(ids, "error")){
        warning(
          "Failed scrolling for more tweet IDs of account ", tw.account
          , " when parsing date range ", from, ":", until, ". "
          , ifelse(
            until == dates[length(dates)-1]
            , "Stopped getting tweet IDs for this account."
            , "Continue with next time window."
          )
        )
        next
      }

    }

    message("Got ", length(ids), " tweet IDs for account ", tw.account, " from ", from, " until ", until, ".")

    account_tweets[[paste0(from, ":", until)]] <- ids
  }

  if (!is.null(write.out) && write.out) {

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

  n_ids = sum(lengths(account_tweets))
  message("Got total ", n_ids, " tweet IDs for account ", tw.account, " from ", since.date, " until ", until.date, ".")

  if (n_ids == 0L) {
    message("Returning empty data frame.")
    return(tibble())
  }

  # return as tibble data frame
  message("Returning data frame.")
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
#'    and gets tweet IDs for the remaining date range(s) by calling \code{\link{scrape_tweet_ids}}.
#'
#' @section WARNING:
#' \itemize{
#'     \item Function presuposses an active remote Selenium driver.
#'     \item Function only accepts dates in format '\%Y-\%m-\%d' (Year-month-day: 'YYYY-mm-dd')
#' }
#'
#' @param screen.name is the screen (or account) name of a twitter user
#'
#' @param user.id is the user ID of a twitter user
#'
#' @param since a date (format '\%Y-\%m-\%d'), specifying the start of the date range to be requested
#'
#' @param until a date (format '\%Y-\%m-\%d'), specifying the end of the date range to be requested
#'
#' @param remdr an \bold{active} RSelenium \code{\link[RSelenium]{remoteDriver}} object
#'     (check \code{remdr$getStatus()} to see if the driver is running.)
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
#' @param ... further arguments passed to \code{\link{scrape_tweet_ids}}.
#'
#' @return A \code{\link[tibble]{tibble}} data frame.
#'      The data frame is empty if an error occurs or no tweet IDs were scraped in the given time range.
#'      Otherwise it has columns
#'      \enumerate{
#'           \item 'screen_name' (<chr>, as passed to argument \code{screen.name}),
#'           \item 'since' (<date>, as returned by interval-specific calls to \link{scrape_tweet_ids}),
#'           \item 'until' (<date>, as returned by interval-specific calls to \link{scrape_tweet_ids}),
#'           \item 'tweet_id' (<chr>) and
#'           \item 'user_id' (<chr>, as passed to argument \code{user.id})
#'      }
#'      , and one row is one tweet.
#'
#' @importFrom purrr map_int map_lgl
#' @importFrom lubridate ymd
#' @import dplyr
#' @import RSelenium
#'
#' @export
get_user_tweet_ids <- function(
  screen.name
  , user.id
  , since
  , until
  , remdr
  , date.interval = "year"
  , sleep = 2
  , .write.out = TRUE
  , .data.path
  , .file.stem = paste0("tw_user_", user.id, "_tweet_ids_%s")
  , verbose = TRUE
  , ...
){
  # validate input
  stopifnot(file.exists(.data.path))

  if (grepl(paste0(.Platform$file.sep, "$"), .data.path))
    .data.path <- gsub(paste0(.Platform$file.sep, "$"), "", .data.path)

  valid_dates <- tryCatch(validate_dates(since, until), error = function(err) err)

  if (inherits(valid_dates, "error") | any(nas_ <- purrr::map_lgl(valid_dates, is.na))){
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
      scrape_tweet_ids(
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
        , ...
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

    # write IDs to output list
    if (nrow(tweet_ids) > 0L) {
      user_tweet_ids[[dr]] <- tweet_ids %>%
        rename(screen_name = account) %>%
        mutate(user_id = user.id)
    } else {
      user_tweet_ids[[dr]] <- tweet_ids
    }
  }

  if ( length(user_tweet_ids) == 0L || all(purrr::map_int(user_tweet_ids, nrow) == 0L) ){
    warning(
      "Got no tweets for account ", screen.name
      , ". Passing empty data frame."
      , call. = FALSE
    )
    return(tibble())
  }

  # rbind output list and return data frame
  message("\n")
  return(do.call(rbind, user_tweet_ids))
}

#' Get tweet IDs meta data from tweet IDs JSON file
#'
#' Returns number of IDs collected per account and date range window
#'
#' @param file Path to the tweets IDs JSON file
#'
#' @param .uid.pattern PERL compatible regular expression used to parse numeric user ID from \code{file} basename
#'
#' @param .daterange.pattern PERL compatible regular expression used to parse date range from \code{file} basename
#'
#' @return a \link[tibble]{tibble} data frame with columns
#' \itemize{
#'     \item 'user_id' (parsed from JSON file stem using \code{.uid.pattern}),
#'     \item 'screen_name' (parent element of JSON file),
#'     \item 'start' (first date parsed from \code{.daterange.pattern}),
#'     \item 'end' (second date parsed from \code{.daterange.pattern}),
#'     \item 'since' (Twitter search query'since' date parsed from JSON elements),
#'     \item 'until' (Twitter search query 'until' date parsed from JSON elements),
#'     \item 'n_tweet_ids' (length of list in nested 'ids' elements of JSON elements).
#'     \item 'file' (as passed to \code{file}).
#' }
#'
#' @import dplyr
#' @importFrom jsonlite read_json fromJSON
#' @importFrom purrr map map_int map_lgl pmap_df compose
#' @importFrom tibble enframe
#' @importFrom lubridate ymd
#'
#' @export
get_tweet_ids_meta <- function(
  file
  , .uid.pattern = "^tw_user_(\\d+)_.+"
  , .daterange.pattern = "^[[:alnum:]_]+_tweet_ids_([[:alnum:]_-]+)\\.json$"
) {

  stopifnot(file.exists(file))

  json <- tryCatch(
    jsonlite::fromJSON(jsonlite::read_json(file)[[1]], simplifyVector = TRUE)
    , error = function(err) err
  )

  if (inherits(json, "error"))
    stop("Cannot read JSON file. Reason: ", json$message)

  # helper
  validate_json_format <- function(e) {
    if(!all(lengths(e) == 3L))
      return(FALSE)

    purrr::map(e, names) %>%
      purrr::map(`%in%`, c("since", "until", "ids")) %>%
      purrr::map_lgl(all) %>%
      all()
  }

  if (!all(purrr::map_lgl(json, validate_json_format))) {
    which_ <- which(purrr::map_lgl(json, purrr::compose(`!`, validate_json_format)))
    stop("Element(s) ", paste(which_, collapse = ", "), " have the wrong format.")
  }

  meta <- tryCatch(
    list(
      jsn = json
      , snm = names(json)
      , uid = gsub(.uid.pattern, "\\1", basename(file))
      , dr = gsub(.daterange.pattern, "\\1", basename(file))
      , fp = file
    ) %>%
      purrr::pmap_df(function(jsn, snm, uid, dr, fp){

        cbind(
          user_id = uid
          , screen_name = snm
          , date_range = dr
          , tibble::enframe(purrr::map_int(jsn, function(dr) length(dr[["ids"]])))
        ) %>%
          as_tibble() %>%
          tidyr::separate(date_range, c("start", "end"), sep = "_to_") %>%
          tidyr::separate(name, c("since", "until"), sep = ":") %>%
          mutate_at(3:6, lubridate::ymd) %>%
          mutate_at(1:2, as.character) %>%
          mutate_at(vars(value), as.integer) %>%
          rename(n_tweet_ids = value) %>%
          mutate(file = fp)
      })
    , error = function(err) err
  )

  if (inherits(meta, "error"))
    stop("Cannot read meta information from JSON file")

  return(meta)
}


