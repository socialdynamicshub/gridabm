#' Theme for the Forest Fire automaton
#'
#' @return A vector containing the color scheme.
#' @export
#'
#' @examples
#' theme_forest_fire()
theme_forest_fire <- function() {
  return(
    c(
      `0` = "transparent",
      `1` = "green",
      `2` = "orange",
      `3` = "firebrick"
    )
  )
}


#' Theme for the Schelling automaton
#'
#' @return A vector containing the color scheme.
#' @export
#'
#' @examples
#' theme_schelling()
theme_schelling <- function() {
  return(
    c(
      `0` = "transparent",
      `1` = "orange",
      `2` = "darkblue"
    )
  )
}

#' Theme for the Life automaton
#'
#' @return A vector containing the color scheme.
#' @export
#'
#' @examples
#' theme_life()
theme_life <- function() {
  return(
    c(
      `0` = "transparent",
      `1` = "black"
    )
  )
}

#' Dark theme for the Brian's Brain automaton
#'
#' @return A vector containing the color scheme.
#' @export
#'
#' @examples
#' theme_brians_brain_dark()
theme_brians_brain_dark <- function() {
  return(
    c(
      `0` = "transparent",
      `1` = "white",
      `2` = "blue"
    )
  )
}

#' Light theme for the Brian's Brain automaton
#'
#' @return A vector containing the color scheme.
#' @export
#'
#' @examples
#' theme_brians_brain_light()
theme_brians_brain_light <- function() {
  return(
    c(
      `0` = "transparent",
      `1` = "black",
      `2` = "blue"
    )
  )
}


#' Theme for Langton's Ant
#'
#' @return A vector containing the color scheme.
#' @export
#'
#' @examples
#' theme_langtons_ant()
theme_langtons_ant <- function() {
  return(
    c(
      `0` = "transparent",
      `1` = "black",
      `2` = "red",
      `3` = "red",
      `4` = "red",
      `5` = "red",
      `6` = "red",
      `7` = "red",
      `8` = "red",
      `9` = "red"
    )
  )
}


#' Default theme if no other theme was supplied
#'
#' @return A vector containing the color scheme.
#' @export
#'
#' @examples
#' theme_default()
theme_default <- function() {
  pal <- RColorBrewer::brewer.pal(name = "Dark2", n = 8)
  return(
    c(
      `0` = "transparent",
      `1` = pal[1],
      `2` = pal[2],
      `3` = pal[3],
      `4` = pal[4],
      `5` = pal[5],
      `6` = pal[6],
      `7` = pal[7],
      `8` = pal[8],
    )
  )
}
