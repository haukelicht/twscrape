#' twscrape: Functionality to collect data from Twitter API and screen
#'
#' The \code{twscrape} package implements functionality to extract data from Twitter.
#' Specifically, it provides an interface to seamlessly screen-scrape tweet IDs, and it extends \code{rtweet} functionality (see \url{https://rtweet.info/}).
#'
#' @section \code{rtweet} extensions:
#'     \code{twscrape} implements the follwing extensions of \code{rtweet} functionality:
#'
#'     \itemize{
#'       \item \code{\link{get_api_request_limits}}:
#'           Passes \code{query} to \code{\link[rtweet]{rate_limits}} and returns current rate limit and reset time.
#'       \item \code{\link{get_all_tweets}}:
#'           Gets all rtweets for a given account in a fixed data window, taking care of rate limiting.
#'       \item \code{\link{read_tweets_data}}:
#'           Reads tweets data frame as returned by \code{\link[rtweet]{lookup_statuses}}
#'           and written to disk by \code{\link{get_all_tweets}},
#'           and returns a \code{\link[tibble]{tibble}} data frame with columns as specified by argument \code{fields}.
#'     }
#'
#' @section Tweet ID scrapers:
#'     \code{twscrape} implements the follwing functionality to screen-scrape tweet IDs:
#'     \itemize{
#'         \item \code{\link{scrape_tweet_ids}}:
#'              Given a twitter account screen name or ID, and start and end dates, it screen-scrapes IDs and returns them in a data frame.
#'              Also allows to additionally write IDs as a JSON file to to disk.
#'         \item \code{\link{get_user_tweet_ids}}:
#'              Given a start and end date, function looks for tweet ID files already to disk
#'              and gets tweet IDs for the remaining date range(s) by calling \code{\link{scrape_tweet_ids}}.
#'     }
#'
#' @docType package
#' @name twscrape
NULL
