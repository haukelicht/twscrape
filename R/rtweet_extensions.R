#' Get Twitter API request limits
#'
#' @param query query string (e.g., 'statuses/user_timeline')
#' @param token an Twitter OAuth \code{rtweet}'s 'Token' object.
#'     Defaults to \code{rtweet}'s \code{get_token}
#'
#' @return Integer value giving the number of remaining available request for a given query.
#'     (Vector is named by query in order to allow for name-based indexing)
#'
#' @importFrom purrr map
#' @importFrom rtweet get_token rate_limit
#' @importFrom utils tail
#'
#' @export
get_api_request_limits <- function(
  query
  , token = rtweet::get_token()
) {

  if(!inherits(token, "Token"))
    stop("`token' is not a valid {rtweet} Twitter OAuth token (see ?rtweet::get_token)")

  limits <- rtweet::rate_limit(token = token)

  req_lim <- apply(limits[grep(query, limits$query), c("query", "remaining", "reset_at")], 1, as.list)

  names(req_lim) <- purrr::map(req_lim, "query")

  req_lim <- purrr::map(req_lim, utils::tail, -1)

  return(req_lim)
}

#' Get all tweets
#'
#' @description Function gets all statuses by ID
#'
#' @param status.ids vector of twitter status IDs
#'
#' @param token an Twitter OAuth \code{rtweet}'s 'Token' object.
#'     Defaults to \code{rtweet}'s \code{get_token}.
#'
#' @param batch.size Number of statuses to request per iteration.
#'
#'      Passed to \code{rtweet}'s \link[rtweet]{lookup_statuses} .
#'      Defaults to 90000 (the maximum value).
#'
#' @param order.ids logical. Order IDs in ascending order?
#'      Defaults to \code{TRUE}.
#'
#' @param verbose logical. print status messages?
#'
#' @importFrom tibble enframe
#' @import dplyr
#' @importFrom rtweet get_token lookup_statuses
#'
#' @return A data frame
#'
#' @export
get_all_tweets <- function(
  status.ids
  , token = rtweet::get_token()
  , batch.size = 90000L
  , order.ids = TRUE
  , verbose = TRUE
){

  if (!verbose){
    message <- function(...) NULL
    on.exit(message <- base::message)
  }

  if (batch.size < 0L | batch.size > 90000L)
    stop("`batch.size' must be a positive integer in {1, ..., 90000}.")

  # split into correctly sized chunks in order to not hit request limits
  if (order.ids)
    status.ids <- status.ids[order(status.ids)]

  status_ids_list <- split(status.ids, (seq_along(status.ids) %/% batch.size))
  l <- length(status_ids_list)

  # output container
  out <- list()
  # iter index
  i <- 1L

  query <- "statuses/lookup"

  # request statuses in batches
  while(TRUE){

    out[[i]] <- tryCatch(
      rtweet::lookup_statuses(
        status_ids_list[[i]]
        , parse = TRUE
      )
      , error = function(err) err
      , warning = function(wrn) wrn
    )

    if (inherits(out[[i]], "warning")) {

      if ((req_lim <- get_api_request_limits(query)[[query]])$remaining <= 470L){
        message("Waiting for request limit to reset at ", req_lim$reset_at)
        wait_until(req_lim$reset_at, pause.secs = 30)
      }

      out[[i]] <- tryCatch(
        rtweet::lookup_statuses(
          status_ids_list[[i]]
          , parse = TRUE
        )
        , error = function(err) err
      )
    }

    if (inherits(out[[i]], "error")){
      warning(
        "Failed getting statuses for indexes "
        , paste((i*batch.size-batch.size)+range(seq_along(status_ids_list[[i]])), collapse = " to ")
        , ifelse(
          order.ids
          , paste0(". (Note that IDs passed as ", typeof(status.ids), " vector were ordered ascendingly.)")
          , ""
        )
        , ifelse(
          l > i
          , ". Continuing with next batch."
          , ". Stopping, as no further batch is waiting"
        )
      )

    } else {

      if (nrow(out[[i]]) != length(status_ids_list[[i]])){
        warning(
          "Number of requested statuses (`status.ids`) and number of rows in list returned from API differ in length "
          , "(", length(status_ids_list[[i]]), " vs. ", nrow(out[[i]]), ")."
          , " Adding missing rows to output data frame."
        )
      }

      if (nrow(out[[i]]) > 0L){
        out[[i]] <- tibble::enframe(status_ids_list[[i]], name = NULL) %>%
          rename(status_id = value) %>%
          mutate(request_status = "requested") %>%
          full_join(out[[i]], by = "status_id") %>%
          mutate(
            request_status = case_when(
              is.na(user.id) ~ "requested and NOT returned"
              , is.na(request_status) ~ "NOT requested and returned"
              , TRUE ~ "requested and returned"
            )
          )
      } else {
        out[[i]] <- tibble()
      }
    }

    if (i == l) {
      # break while loop if new more batches waiting
      break
    } else {
      i <- i + 1L
      next
    }
  }

  # row-bind all data frames
  tweets <- do.call(rbind, out)

  # report
  message(
    "Got ", nrow(tweets), " tweets in ", l, ifelse(l == 1, " batch.", "batches.")
    , " Time range: ", paste(range(as.Date.character(tweets$created_at)), collapse = " to ")
  )

  # return
  return(tweets)
}

#' Get \code{rtweet}'s status-lookup columns
#'
#' @description Function returns column names of fields in twitter object returned
#'     on Twitter API call via the \code{rtweet}'s \link[rtweet]{lookup_statuses}
#'
#' @param status column names of main status info (status ID and text, created time)
#' @param user column names of main user info (user ID, screen name and name, and account created time)
#' @param status.plus column names of status meta info
#' @param status.engagements column names of status engagement count fields (No. favorites, retweets, quotes, replies)
#' @param status.entities column names of status entities objects
#' @param status.entities.hashtags column names of status hashtags object
#' @param status.entities.symbols column names of status symbols object
#' @param status.entities.urls column names of status urls object
#' @param status.entities.media column names of status media object
#' @param status.reply column names of status reply object (user ID, status ID, created time)
#' @param status.quote column names of status quote object (user ID, status ID and text, created time)
#' @param status.quote.status column names of additional status-related fields of status quote object
#' @param status.quote.user column names of additional user-related fields of status quote object
#' @param status.retweet column names of status retweet object (user ID, status ID and text, created time)
#' @param status.retweet.status column names of additional status-related fields of status retweet object
#' @param status.retweet.user column names of additional user-related fields of status retweet object
#' @param status.location column names of status' location objects
#' @param status.location.place column names of status' place info
#' @param status.location.coords column names of status' coordination info
#' @param user.bio column names of user account-related fields
#' @param user.engagements column names of user engagement fields
#' @param user.profile column names of user profile-related fields
#' @param user.misc user column names of miscalaneus user info fields
#'
#' @return  a character vector of column names compatible with the column names of
#'     the data frame obejct returned by valid calls to \code{rtweet}'s \link[rtweet]{lookup_statuses}
#'
tw_status_cols <- function(
  # main status info (ID, create time, text)
  status = TRUE
  # main user info (ID, screen name, name, account created time)
  , user = TRUE
  # Additional status info
  # status meta info (URL, source, display width)
  , status.plus = status
  # status enagements info (No. likes, retweets, quotes, and replies)
  , status.engagements = status
  # status entities (hashtags, symbols, urls, media)
  , status.entities = status
  , status.entities.hashtags = status.entities
  , status.entities.symbols = status.entities
  , status.entities.urls = status.entities
  , status.entities.media = status.entities
  # status replies to
  , status.reply = status
  # status quotes
  , status.quote = status
  , status.quote.status = status.quote
  , status.quote.user = status.quote
  # status retweets
  , status.retweet = status
  , status.retweet.status = status.retweet
  , status.retweet.user = status.retweet
  # status.location
  , status.location = status
  # status.location.place
  , status.location.place = status.location
  # status.location.coords
  , status.location.coords = status.location
  # Additional user info
  , user.bio = user
  , user.engagements = user
  , user.profile = user
  , user.misc = user
){

  cols <- character()

  if (user)
    cols <- c(
      cols
      , c(
        "user_id" # (chr) := ID of the user posting the tweet status
        , "screen_name" # (chr) := screen name of the user posting the tweet status
        , "name" # (chr) := full name of the user posting the tweet status
        , "account_created_at" # (dttm) := datetime when user account was created
      )
    )

  if (status)
    cols <- c(
      cols
      , c(
        "status_id" # (chr) := ID of the status post
        , "created_at" # (dttm) := UTC time when status was posted
        , "text" # (chr) := full status text
      )
    )

  if (status && status.plus)
    cols <- c(
      cols
      , c(
        "lang" # (chr) := status language
        , "status_url" # (chr) := URL of the status post
        , "source" # (chr) := utility used to post the status (text from HTML-formatted a-tag string)
        , "display_text_width" # (dbl) := unicode code point index identifying the exclusive end of the displayable content of the tweet
      )
    )

  if (status && status.engagements)
    cols <- c(
      cols
      , c(
        "favorite_count" # (int) := number of times this status has been marked as 'favorite'
        , "retweet_count" # (int) := number of retweets of this status
        , "quote_count" # (int) := number of quotes of this status
        , "reply_count" # (int) := number of replies to this status
      )
    )

  if (status && status.entities && status.entities.hashtags)
    cols <- c(
      cols
      , c(
        "hashtags" # (list of chrs) := names of hashtags used in this status (minus the leading '#' characters)
      )
    )

  if (status && status.entities && status.entities.symbols)
    cols <- c(
      cols
      , c(
        "symbols" # (list of chrs) := names of cashtags used in this status (minus the leading '$' characters)
      )
    )

  if (status && status.entities && status.entities.urls)
    cols <- c(
      cols
      , c(
        "urls_url" # (list of chrs) := URL pasted/typed into status
        , "urls_t.co" # (list of chrs) := URL pasted/typed into status
        , "urls_expanded_url" # (list of chrs) := expanded version of URL pasted/typed into status (i.e., `urls_url`)
      )
    )

  if (status && status.entities && status.entities.media)
    cols <- c(
      cols
      , c(
        "media_url" # (list of chrs) := URL pasted/typed into status
        , "media_t.co" # (list of chrs) := short version (t.co/) of URL pasted/typed into status
        , "media_expanded_url" # (list of chrs) := expanded version URL pasted/typed into status
        , "media_type" # (chr) := media type in {'photo', 'video', 'animated_gif'}
        , "ext_media_url" # (list of chrs) := URL pasted/typed into status
        , "ext_media_t.co" # (list of chrs) := short version (t.co/) of URL pasted/typed into status
        , "ext_media_expanded_url" # (list of chrs) := expanded version URL pasted/typed into status
        , "ext_media_type" # (chr) := media type in {'photo', 'video', 'animated_gif'}
      )
    )

  if (status && status.reply)
    cols <- c(
      cols
      , c(
        "reply_to_status_id" # (chr) := ID of the target status the current status post replies to (`NA` if current status not a reply)
        , "reply_to_user_id" # (chr) := ID of the user that posted a target status the current status post replies to (`NA` if current status not a reply)
        , "reply_to_screen_name" # (chr) := screen name of the user that posted a target status the current status post replies to (`NA` if current status not a reply)
      )
    )

  if (status && status.quote)
    cols <- c(
      cols
      , c(
        "is_quote" # (lgl) := boolean indicating whether this status quotes another status post
        , "quoted_user_id" # (chr) :=
        , "quoted_status_id" # (chr) :=
        , "quoted_text" # (chr) :=
        , "quoted_created_at" # (dttm) :=
      )
    )

  if (status && status.quote && status.quote.status)
    cols <- c(
      cols
      , c(
        "quoted_source" # (chr) :=
        , "quoted_favorite_count" # (int) :=
        , "quoted_retweet_count" # (int) :=
      )
    )

  if (status && status.quote && status.quote.user)
    cols <- c(
      cols
      , c(
        "quoted_screen_name" # (chr) :=
        , "quoted_name" # (chr) :=
        , "quoted_followers_count" # (int) :=
        , "quoted_friends_count" # (int) :=
        , "quoted_statuses_count" # (int) :=
        , "quoted_location" # (chr) :=
        , "quoted_description" # (chr) :=
        , "quoted_verified" # (lgl) :=
      )
    )

  if (status && status.retweet)
    cols <- c(
      cols
      , c(
        "is_retweet" # (lgl) := boolean indicating whether this status retweets another status post
        , "retweet_user_id" # (chr) :=
        , "retweet_status_id" # (chr) :=
        , "retweet_text" # (chr) :=
        , "retweet_created_at" # (dttm) :=
      )
    )

  if (status && status.retweet && status.retweet.status)
    cols <- c(
      cols
      , c(
        "retweet_source" # (chr) :=
        , "retweet_favorite_count" # (int) :=
        , "retweet_retweet_count" # (int) :=
      )
    )

  if (status && status.retweet && status.retweet.user)
    cols <- c(
      cols
      , c(
        "retweet_screen_name" # (chr) :=
        , "retweet_name" # (chr) :=
        , "retweet_followers_count" # (int) :=
        , "retweet_friends_count" # (int) :=
        , "retweet_statuses_count" # (int) :=
        , "retweet_location" # (chr) :=
        , "retweet_description" # (chr) :=
        , "retweet_verified" # (lgl) :=
      )
    )

  if (status && status.location && status.location.place)
    cols <- c(
      cols
      , c(
        "place_url" # (chr) := URL pointing to additional metadata for this place
        , "place_name" # (chr) := Short human-readable representation of the place's name
        , "place_full_name" # (chr) := Full human-readable representation of the place's name
        , "place_type" # (chr) := The type of location represented by this place
        , "country" # (chr) := Name of the country containing this place
        , "country_code" # (chr) := Shortened (2-character) country code representing the country containing this place
        , "bbox_coords" # (list) := eight coordinates (2 long, lat tuples per corner) defining a box which will contain the `place` this bounding box is related to
      )
    )

  if (status && status.location && status.location.coords)
    cols <- c(
      cols
      , c(
        "geo_coords" # (list) := latitude, longitude tuple of the status' exact geo-location (if available)
        , "coords_coords" # (list) := longitude, latitude tuple of the status' exact location (if available)
      )
    )

  if (user && user.bio)
    cols <- c(
      cols
      , c(
        "location" # (chr) := user-defined (permanent) location for this account’s profile
        , "description" # (chr) := user-defined UTF-8 string describing their account (user bio)
        , "url" # (chr) := URL provided by the user in association with their profile
      )
    )

  if (user && user.engagements)
    cols <- c(
      cols
      , c(
        "followers_count" # (int) := number of other users following this account
        , "friends_count" # (int) := number of other accounts this user follows
        , "listed_count" # (int) := number of public lists that this user is a member of
        , "statuses_count" # (int) := number of tweets (including retweets) issued by the user
        , "favourites_count" # (int) := number of tweets this user has liked in the account's lifetime
      )
    )

  if (user && user.profile)
    cols <- c(
      cols
      , c(
        "profile_url" # (chr) := short URL provided by the user in association with their profile
        , "profile_expanded_url" # (chr) := expanded URL provided by the user in association with their profile
        , "profile_image_url" # (chr) := HTTPS-based URL pointing to the user's profile image (WARNING: deprecated, use `profile_image_url_https` instead)
        , "profile_banner_url" # (chr) := HTTPS-based URL pointing to the standard web representation of the user's uploaded profile banner
        , "profile_background_url" # (chr) :=
      )
    )

  if (user && user.misc)
    cols <- c(
      cols
      , c(
        "protected" # (lgl) := boolean whether or not this user has chosen to protect their tweets
        , "verified" # (lgl) := boolean indicates whether or not this is a verified user account
        , "account_lang" # (lgl) := user-defined account language
      )
    )

  return(cols)
}

#' Read tweets data from disk
#'
#' @description Given a vector of file paths, function returns a (row-stacked)
#'     data frame object of the contained tweets data
#'
#' @details The files to be read need to be tweet data frame as returned by \code{rtweet}'s \link[rtweet]{lookup_statuses}.
#'     The file format needs to be readable by the reader function passed to parameter \code{read.fun}.
#'
#' @param paths character vector specifiying the paths to twitter data files
#' @param fields names of columns (fields) to be extracted and row-binded in output data frame
#' @param read.fun reader function (needs to be compatible with formats of files passed to \code{paths})
#'
#' @importFrom tibble tibble
#' @importFrom purrr map map_dfr
#'
#' @return A \link[tibble]{tibble} data frame with columns as specified by argument \code{fields}
#' @export
read_tweets_data <- function(
  paths
  # for available fields, see \code{tw_status_cols}
  , fields = tw_status_cols(
    status.location = FALSE
    , user.bio = FALSE
    , user.engagements = FALSE
    , user.profile = FALSE
    , user.misc = FALSE
  )
  , read.fun = readRDS
) {
  if (length(paths) == 0L)
    return(tibble::tibble())

  if (is.recursive(paths))
    paths <- unlist(paths)

  these_paths <- paths[path_exists <- file.exists(paths)]

  if (any(!path_exists)) {
    sprintf("File on path %s does no exist and is omitted in data read.", paths[!path_exists]) %>%
      purrr::map(warning, call. = FALSE)
  }

  idx <- which(fields %in% tw_status_cols(status = TRUE, user = TRUE))

  if (length(fields[-idx]) > 0L) {
    warning(
      paste(
        "The following column names passed to argument `fields' do not match column names read-in data frame and will be ignored:"
        , paste(sprintf("'%s'", fields[-idx]), collapse = ", ")
      )
      , call. = FALSE
    )
  }

  out <- purrr::map_dfr(these_paths, function(path, .fields = fields[idx], .rf = read.fun) {.rf(path)[, .fields]})

  return(unique(out))
}

