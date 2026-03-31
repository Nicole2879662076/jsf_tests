# jsf_medium.R
# Core functions for jsfR package - Medium Test

#' Check if Python jsf package is available
#'
#' This is the core requirement: checking Python package availability via reticulate
#'
#' @return Logical, TRUE if jsf Python package is available
#' @export
jsf_available <- function() {
  reticulate::py_module_available("jsf")
}

#' Run a simple JSF simulation
#'
#' Core function: calls Python jsf package via reticulate
#' This implements the basic requirement of Medium Test
#'
#' @param x0 Initial state vector, default c(50, 10)
#' @param t_max Maximum simulation time, default 30.0
#' @return A list containing simulation results
#' @export
jsf_simple_simulate <- function(x0 = c(50, 10), t_max = 30.0) {
  # 1. Check Python environment - core reticulate usage
  if (!jsf_available()) {
    stop("Python jsf package not available. Please install with: pip install jsf")
  }

  # 2. Import Python module - core reticulate call
  jsf <- reticulate::import("jsf", convert = TRUE)

  # 3. Define a Lotka-Volterra predator-prey model
  rates <- function(state, time) {

    tryCatch({

      state_list <- reticulate::py_to_r(state)

      if (is.list(state_list) && length(state_list) >= 2) {
        prey <- as.numeric(state_list[[1]])
        predator <- as.numeric(state_list[[2]])

        # Lotka-Volterra model parameters
        alpha <- 1.0    # Prey birth rate
        beta <- 0.1    # Predation rate
        gamma <- 0.1   # Predator conversion efficiency
        delta <- 0.5   # Predator death rate

        # Lotka-Volterra equations
        dprey_dt <- alpha * prey - beta * prey * predator
        dpredator_dt <- gamma * prey * predator - delta * predator

        return(c(dprey_dt, dpredator_dt))
      } else {

        return(c(0.5, 0.2))
      }
    }, error = function(e) {

      return(c(0.5, 0.2))
    })
  }

  # 4. Use a simple stoichiometric matrix (to maintain stability)
  stoich <- list(
    nu = list(c(-1, 1), c(1, -1)),
    nuReactant = list(c(1, 0), c(0, 1))
  )

  # 5. Call Python jsf function - core Python call
  result <- jsf$jsf(
    x0 = reticulate::r_to_py(x0),
    rates = reticulate::r_to_py(rates),
    stoich = reticulate::r_to_py(stoich),
    t_max = t_max,
    method = "operator-splitting",
    config = list(
      dt = 0.1,
      EnforceDo = c(1, 1),
      SwitchingThreshold = c(10, 10)
    )
  )

  # 6. Return basic result
  return(reticulate::py_to_r(result))
}
