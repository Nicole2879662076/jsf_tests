# visualize.R
# Basic visualization for jsfR - Bonus feature

#' Plot JSF simulation results
#'
#' Bonus feature: basic visualization
#' Demonstrates R's distinctive capabilities
#'
#' @param result Either JSFResult object or raw result list
#' @export
plot_jsf <- function(result) {
  if (inherits(result, "jsfR::JSFResult") ||
      inherits(result, "JSFResult") ||
      any(grepl("JSFResult", class(result)))) {
    # S7
    time_data <- result@time
    comp1_data <- result@populations$compartment1
    comp2_data <- result@populations$compartment2
  } else if (is.list(result) && length(result) >= 2) {
    # original data
    time_data <- as.numeric(result[[2]])
    comp1_data <- as.numeric(result[[1]][[1]])
    comp2_data <- as.numeric(result[[1]][[2]])
  } else {
    stop("The result format cannot be recognized. Please provide a JSFResult object or the original result list.")
  }

  # validity of the data.
  if (length(time_data) == 0) {
    stop("The time data is empty.")
  }
  if (length(comp1_data) == 0 || length(comp2_data) == 0) {
    stop("Population data is empty.")
  }

  # Ensure that the data lengths are consistent.
  n <- min(length(time_data), length(comp1_data), length(comp2_data))
  time_data <- time_data[1:n]
  comp1_data <- comp1_data[1:n]
  comp2_data <- comp2_data[1:n]

  # plot
  plot(time_data, comp1_data,
       type = "l",
       col = "blue",
       xlab = "Time",
       ylab = "Population",
       main = "JSF Simulation Result",
       ylim = range(c(comp1_data, comp2_data), na.rm = TRUE, finite = TRUE))
  lines(time_data, comp2_data, col = "red")
  legend("topright",
         legend = c("Compartment 1", "Compartment 2"),
         col = c("blue", "red"),
         lty = 1)
}
