emojis_unicodes_ <- xml2::read_html("http://www.unicode.org/emoji/charts/full-emoji-list.html#smileys_&_emotion")
emojis_unicodes_ <- gsub("U\\+", "", rvest::html_text(rvest::html_nodes(emojis_unicodes_, "td.code")))
emojis_unicodes_ <- Unicode::u_char_inspect(Unicode::as.u_char_range(emojis_unicodes_))
emojis_unicodes_ <- emojis_unicodes_$Char[!is.na(emojis_unicodes_$Char)]
emojis_regex_ <- paste(emojis_unicodes_, collapse = "|")

tweet_replace_patterns <- c(
  "@_" = "(?<=^|\\W)(@\\w{1,15})(?=\\s|$|\\W)"
  , "#_" = "(?<=\\.|^|\\s)(#\\w{1,139})(?=\\s|$|\\W)"
  # see https://emailregex.com/
  , "M_" = "\\b[a-z0-9!#$%&'*+/=?^_`{|}~-]+(@|\\[\\s?at\\s?\\])[A-Za-z0-9.-]+\\.[A-Za-z]{2,6}\\b"
  # see http://daringfireball.net/2010/07/improved_regex_for_matching_urls
  , "U_" = "\\b(([a-z][\\w-]+:(/{1,3}|[a-z0-9%])|www\\d{0,3}[.]|[a-z0-9.\\-]+[.][a-z]{2,4}/)([^\\s()<>]+|\\(([^\\s()<>]+|(\\([^\\s()<>]+\\)))*\\))+(\\(([^\\s()<>]+|(\\([^\\s()<>]+\\)))*\\)|[^\\s`!()\\[\\]{};:\\\'\\\".,<>?«»“”‘’]))"
  , "E_" = paste0("(", emojis_regex_, ")")
)

saveRDS(tweet_replace_patterns, file.path("data", "tweet_replace_patterns.rds"))

#' Regex patterns of special tokens in tweet texts
#'
#' A vector mapping replacment place holders to regex patterns matching
#'     special tokens in tweets, namely
#'       mentions (@...),
#'       hastags (#... ),
#'       email addresses,
#'       URLs, and
#'       common Emojis.
#'
#' @format A named character vector with five elements (names: :
#' \describe{
#'   \item{"@_": regex pattern capturing Twitter user mentions}
#'   \item{"#_": regex pattern capturing Twitter hastags}
#'   \item{"M_": regex pattern capturing email addresses (see \url{https://emailregex.com/})}
#'   \item{"U_": regex pattern capturing URLs (see \url{http://daringfireball.net/2010/07/improved_regex_for_matching_urls})}
#'   \item{"E_": regex pattern capturing common Emojis (thse listed here \url{http://www.unicode.org/emoji/charts/full-emoji-list.html#smileys_&_emotion})}
#' }
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_text
#' @importFrom Unicode u_char_inspect as.u_char_range
"tweet_replace_patterns"
