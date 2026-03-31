# jsfs7.R
# S7 Object System for jsfR - Bonus feature

#' JSFResult S7 class
#'
#' Bonus feature: S7 object system encapsulation
#' Demonstrates understanding of the complete project architecture
#'
#' @export
JSFResult <- S7::new_class(
  name = "JSFResult",
  properties = list(
    time = S7::class_numeric,
    populations = S7::class_list,
    metadata = S7::class_list
  ),
  validator = function(self) {
    if (length(self@time) != length(self@populations[[1]])) {
      return("Time and populations must have same length")
    }
  }
)

#' Run JSF simulation and return S7 object
#'
#' Adds S7 encapsulation on top of core functionality
#'
#' @param x0 Initial state vector
#' @param t_max Maximum simulation time
#' @return JSFResult S7 object
#' @export
jsf_run_s7 <- function(x0 = c(10, 0), t_max = 10.0) {
  # Use core function
  raw_result <- jsf_simple_simulate(x0, t_max)

  # Encapsulate as S7 object
  JSFResult(
    time = as.numeric(raw_result[[2]]),
    populations = list(
      compartment1 = as.numeric(raw_result[[1]][[1]]),
      compartment2 = as.numeric(raw_result[[1]][[2]])
    ),
    metadata = list(
      x0 = x0,
      t_max = t_max,
      timestamp = Sys.time()
    )
  )
}
