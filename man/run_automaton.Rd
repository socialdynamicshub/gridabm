% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_automaton.R
\name{run_automaton}
\alias{run_automaton}
\title{Run an automaton with a given stepping function}
\usage{
run_automaton(initial_state, steps, stepfunc, ...)
}
\arguments{
\item{initial_state}{The initial state \code{matrix} of the automaton.}

\item{steps}{The number of steps to run the automaton.}

\item{stepfunc}{The stepping function.}

\item{...}{Additional parameters for the stepping function.}
}
\value{
A \code{data.frame} with the results of the simulation.
}
\description{
Automata can be updated with stepping functions one step at a time.
However, the intended workflow of this package is to give a stepping function
to the \code{run_automaton} function as an input parameter.
This function takes the \code{stepfunc} and successively applies it \code{steps} times,
starting from an \code{initial_state}.
Some stepping functions require additional input that can be provided to this
function as additional parameters (\code{...}).
Several well-known automata are implemented in this package (see \code{life_step},
\code{schelling_step}, \code{forest_fire_step}, \code{brians_brain_step}).
}
\examples{
initial_state <- create_board(2, c(0.7, 0.3), c(20, 20))
run_automaton(initial_state, 50, life_step)
}
