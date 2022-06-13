#' Theme for the Forest Fire automaton
#'
#' @return A vector containing the color scheme.
#' @export
#'
#' @examples
theme_forest_fire <- function() {
  return(c("transparent", "green", "orange", "firebrick"))
}


#' Theme for the Schelling automaton
#'
#' @return A vector containing the color scheme.
#' @export
#'
#' @examples
theme_schelling <- function() {
  return(c("transparent", "orange", "darkblue"))
}

#' Theme for the Life automaton
#'
#' @return A vector containing the color scheme.
#' @export
#'
#' @examples
theme_life <- function() {
  return(c("transparent", "black"))
}

#' Dark theme for the Brian's Brain automaton
#'
#' @return A vector containing the color scheme.
#' @export
#'
#' @examples
theme_brians_brain_dark <- function() {
  return(c("transparent", "white", "blue"))
}

#' Light theme for the Brian's Brain automaton
#'
#' @return A vector containing the color scheme.
#' @export
#'
#' @examples
theme_brians_brain_light <- function() {
  return(c("transparent", "black", "blue"))
}

#' Default theme if no other theme was supplied
#'
#' @return A vector containing the color scheme.
#' @export
#'
#' @examples
theme_default <- function() {
  return(
    c(
      "transparent",
      RColorBrewer::brewer.pal(name = "Dark2", n = 8)
    )
  )
}
