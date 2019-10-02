#' twscrape: Functionality to collect data from Twitter API and screen
#'
#' The \code{twscrape} package implements functionality to extract data from Twitter.
#' Specifically, it provides an interface to seamlessly screen-scrape tweet IDs, and it extends \code{rtweet} functionality (see \url{https://rtweet.info/}).
#'
#' @section \code{rtweet} extensions:
#'     \code{twscrape} implements the follwing extensions of \code{rtweet} functionality:
#'
#'     \itemize{
#'       \item \code{\link{get_api_rate_limits}}:
#'           passes \code{query} to \code{\link[rtweet]{rate_limits}} and returns current rate limit and reset time
#'       \item \code{\link{get_all_tweets}}:
#'           gets all rtweets for a given account in a fixed data window, taking care of rate limiting
#'       \item \code{\link{read_tweets_data}}:
#'           reads tweets data frame as returned by \code{\link[rtweet]{lookup_statuses}}
#'           and written to disk by \code{\link{get_all_tweets}},
#'           and returns a \code{\link[tibble]{tibble}} data frame with columns as specified by argument \code{fields}
#'     }
#'
#' @docType package
#' @name twscrape
NULL
