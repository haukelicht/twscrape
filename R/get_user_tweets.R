#' Get a user's tweets for a given date range
#'
#' @description Given a tweet IDs JSON, a user.id, and a start and end date,
#'     function looks for existing tweets data
#'     and gets tweets for the remaining date range(s) by calling \code{\link{get_all_tweets}}.
#'
#' @section WARNING:
#' \itemize{
#'     \item Function presuposses a valid Twitter OAuth token.
#'     \item Function only accepts dates in format '\%Y-\%m-\%d' (Year-month-day: 'YYYY-mm-dd')
#' }
#'
#' @param ids.file path to a tweet IDs JSON (as written by \link{scrape_tweet_ids} or \link{get_user_tweet_ids})
#'
#' @param user.id is the user ID of a twitter user
#'
#' @param since a date (format '\%Y-\%m-\%d'), specifying the start of the date range to be requested
#'
#' @param until a date (format '\%Y-\%m-\%d'), specifying the end of the date range to be requested
#'
#' @param token an Twitter OAuth \code{rtweet}'s 'Token' object.
#'     Defaults to \code{rtweet}'s \code{get_token}.
#'
#' @param .write.out logical. write out tweet IDs as JSON to disk?
#'     If \code{TRUE} (the default), JSON file will be written
#'     to path \code{.data.path}
#'
#' @param .data.path Path to look at for existing tweet ID files
#'     Also the path where new ID files are written if \code{write.out = TRUE}.
#'
#' @param .file.stem file name stem (stem ignores date ranges).
#'     Defaults to glob 'tw_user_<\code{user.id}>_tweets_*.RData'.
#'     Used both for looking for existing tweets data files, and to name new ones when writing to disk.
#'
#' @param .write.fun function used to write data output list.
#'     Note that you cannot use \link[utils]{write.csv} or any other table-like writer, as the outputted data is a list object.
#'
#' @param verbose logical. Print out status messages?
#'
#' @return A list object with three elements
#'      \enumerate{
#'           \item 'tweets': \code{\link[tibble]{tibble}} data frame containing the tweets data.
#'           \item 'data_pathes': a chracter vector, specifiying the paths of \emph{all} tweets data files if any exist or new were written
#'           \item 'failures': \code{\link[tibble]{tibble}} data frame reporting warnings and errors raised while trying to get tweets data.
#'      }
#'
#' @import dplyr
#' @importFrom purrr map map_lgl quietly
#' @importFrom rtweet get_token
#' @importFrom jsonlite read_json fromJSON
#' @importFrom lubridate ymd as_date
#'
#' @export
get_user_tweets <- function(
  ids.file
  , user.id
  , since
  , until
  , token = rtweet::get_token()
  , .write.out = TRUE
  , .data.path
  , .file.stem = paste0("tw_user_", user.id, "_tweets_%s.RData")
  , .write.fun = saveRDS
  , verbose = TRUE
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

  out_list <- list(
    tweets = tibble::tibble()
    , data_paths = character()
    , failures = tibble::tibble()
  )

  # get user's tweets files that exist on disk
  existing_data <- list.files(path = .data.path, pattern = sprintf(.file.stem, ".*"), full.names = TRUE, recursive = TRUE)
  out_list$data_paths <- existing_data

  # get date ranges of tweets to be requested
  dates <- strsplit(gsub(".+(\\d{4}-\\d{2}-\\d{2})_to_(\\d{4}-\\d{2}-\\d{2}).+", "\\1_\\2", existing_data), "_")
  req_date_ranges <- purrr::quietly(get_required_date_ranges)(lubridate::ymd(sort(unique(unlist(dates)))), since, until)

  if (is.null(req_date_ranges$result)) {
    message(
      "IDs in date range requested for user ", user.id, " were already scraped"
      , " (see files ", paste(existing_data, collapse = ", "), ")"
    )

    return(out_list)
  }

  req_date_ranges <- req_date_ranges$result

  # get ids from current JSON file
  ids <- jsonlite::fromJSON(jsonlite::read_json(ids.file)[[1]])

  # get screen name from file
  screen_name <- names(ids)[1]

  # collectors
  user_tweets_out <- list()
  failures <- list()

  # loop over date ranges
  for (dr in seq_along(req_date_ranges)) {

    curr_date_range <- paste(req_date_ranges[[dr]], collapse = " to ")

    # check which date-range elements contain tweet IDs of interest
    idxs <- which(
      purrr::map_lgl(ids[[1]], function(e, i = dr) {
        d <- intersect(
          seq(lubridate::as_date(e$since), lubridate::as_date(e$until), "day")
          , seq(lubridate::as_date(req_date_ranges[[i]][1]), lubridate::as_date(req_date_ranges[[i]][2]), "day")
        )
        length(d) > 0
      })
    )

    # get status IDs of to-be-requested tweets in date range of interest
    status_ids <- unique(unlist(purrr::map(ids[[1]][idxs], "ids"), use.names = FALSE))

    # next if none
    if (is.null(status_ids)) {
      message(
        "No status IDs for user ", screen_name, " (", user.id, ")"
        , " in date range ", curr_date_range
        , ifelse(
          dr < length(req_date_ranges)
          , ". Processing with next date range."
          , ""
        )
      )
      next
    }

    message(
      "Trying to get ", length(status_ids), " tweets for user ", screen_name, " (", user.id, ")"
      , " in date range ", curr_date_range
    )

    # undebug(get_all_statuses)
    user_tweets <- tryCatch(
      purrr::quietly(get_all_tweets)(status.ids = status_ids, order.ids = FALSE)
      , error = function(err) err
    )

    incomplete_ <- length(user_tweets$warnings) > 0L & any(grepl("number of rows in list returned from API differ", user_tweets$warnings))

    # if error or incomplete ...
    if (inherits(user_tweets, "error") | incomplete_) {

      # wait for request limit reset ...
      if (as.integer((req_lim <- get_api_request_limits("statuses/lookup")[[1]])$remaining) <= 470L){
        message("Waiting for request limit to reset at circa", req_lim$reset_at)
        suppressMessages(wait_until(req_lim$reset_at, pause.secs = 30))
      }

      # ... and try again
      user_tweets <- tryCatch(
        purrr::quietly(get_all_tweets)(status.ids = status_ids, order.ids = FALSE)
        , error = function(err) err
      )

      # if still error ...
      if (inherits(user_tweets, "error")) {

        # prompt message
        message(
          "Error when requesting statuses for user ", screen_name, " (", user.id, ")"
          , " in date range ", curr_date_range
          , ". Reason: `get_all_statuses` throws error '", user_tweets$message, "'."
        )

        # add to `failures` list
        failures[[length(failures) + 1L]] <- tibble::tibble(
          user_id = user.id
          , since = req_date_ranges[[dr]]["s"]
          , until = req_date_ranges[[dr]]["e"]
          , type = "error"
          , message = user_tweets$message
        )
        # and proceed with next date range (or otherwise with next user, if any)
        next
      }
    }

    if (!is.null(user_tweets$warnings) && length(user_tweets$warnings) > 0L) {
      message(
        "Warning when requesting statuses for user ", screen_name, " (", user.id, ")"
        , " in date range ", curr_date_range
        , ". Reason: '", user_tweets$warnings, "'."
      )

      failures[[length(failures) + 1L]] <- tibble::tibble(
        user_id = user.id
        , since = req_date_ranges[[dr]]["s"]
        , until = req_date_ranges[[dr]]["e"]
        , type = "warning"
        , message = user_tweets$warnings
      )
    }

    if (!is.null(user_tweets$messages))
      message(gsub("\n$" ,"", user_tweets$messages))

    if (is.data.frame(user_tweets$result) && nrow(user_tweets$result) > 0L){

      if (.write.out) {

        f_ <- file.path(.data.path, sprintf(.file.stem, gsub(" ", "_", curr_date_range)))
        .write.fun(user_tweets$result, f_)

        message(
          "Wrote statuses data for user ", screen_name, " (", user.id, ")"
          , " in date range ", curr_date_range
          , " to disk at ", f_
        )

        existing_data[length(existing_data)+1L] <- f_
      }

      user_tweets_out[[length(user_tweets_out)+1L]] <- user_tweets$result

    } else {

      fail_reason <- ifelse(
        !is.data.frame(user_tweets$result)
        , "Object returned from call to `get_all_statuses` is not a data frame."
        , "Data frame returned from call to `get_all_statuses` has zero rows."
      )

      msg <- paste0("Could not get tweets data for user ", screen_name, " (", user.id, "). Reason: ", fail_reason)

      message(msg, call. = FALSE)

      failures[[length(failures) + 1L]] <- tibble::tibble(
        user_id = user.id
        , since = req_date_ranges[[dr]]["s"]
        , until = req_date_ranges[[dr]]["e"]
        , type = "error"
        , message = msg
      )
    }
  }

  user_tweets_out <- do.call(rbind, user_tweets_out)
  if (is.null(user_tweets_out))
    user_tweets_out <- tibble::tibble()

  failures <- do.call(rbind, failures)
  if (is.null(failures))
    failures <- tibble::tibble()


  out_list <- list(
    tweets = user_tweets_out
    , data_paths = sort(existing_data)
    , failures = failures
  )

  message(
    "Got ", nrow(out_list$tweets), " tweets for user ", screen_name, " (", user.id, ")."
    , " Encountered ", nrow(out_list$failures), " failures.\n"
  )

  return(out_list)
}
