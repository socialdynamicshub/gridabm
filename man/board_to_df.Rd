% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/board_to_df.R
\name{board_to_df}
\alias{board_to_df}
\title{Turn an automaton state \code{matrix} into a \code{data.frame}}
\usage{
board_to_df(m)
}
\arguments{
\item{m}{A \code{matrix} representing an automaton state.}
}
\value{
The \code{data.frame} representation of the automaton state.
}
\description{
Automaton states are represented as \code{matrix} objects in the \code{gridabm}
library.
However, \code{data.frame}s are more convenient for plotting, so this function
converts a state \code{matrix} into a \code{data.frame}.
}
\examples{
automaton_state <- create_board(2, c(0.5, 0.5), c(10, 10))

board_to_df(automaton_state)
}
