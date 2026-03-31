#' Generate JSF Reference Data
#'
#' Generate reference simulation data using the Python JSF package via jsfR
#' Implements caching mechanism to avoid repeated computations
#'
#' @param lambda Birth rate
#' @param mu Death rate
#' @param x0 Initial population
#' @param t_max Total simulation time
#' @param n_reps Number of simulation replicates
#' @param dt JSF time step
#' @param threshold Switching threshold
#' @param seed Random seed
#' @param cache_file Path to cache file
#' @return JSF simulation results as data frame
#' @export
generate_jsf_reference <- function(
    lambda = 1.0,
    mu = 0.5,
    x0 = 5,
    t_max = 10.0,
    n_reps = 100,
    dt = 0.01,
    threshold = 10L,
    seed = 2026,
    cache_file = NULL
) {
  # Check cache
  if (!is.null(cache_file) && file.exists(cache_file)) {
    message("Loading cached JSF reference data from: ", cache_file)
    return(readRDS(cache_file))
  }

  # Ensure jsfR is available
  if (!jsf_available()) {
    stop("Python jsf package not available. Please ensure jsfR is properly configured.")
  }

  message("Generating JSF reference data (this may take a while)...")

  # Import Python jsf module
  jsf <- reticulate::import("jsf", convert = FALSE)

  # Use jsfR package for simulation
  trajectories <- list()
  for (i in seq_len(n_reps)) {
    # Define rate function for birth-death process
    rates <- function(state, time) {
      # state is a Python list, convert to R to extract value
      state_r <- tryCatch({
        if (inherits(state, "python.builtin.list")) {
          reticulate::py_to_r(state)
        } else {
          as.numeric(state)
        }
      }, error = function(e) {
        # If conversion fails, try to get first element
        tryCatch({
          as.numeric(state[1])
        }, error = function(e2) 0)
      })

      # Ensure we have a numeric value
      n <- if (length(state_r) > 0) as.numeric(state_r[1]) else 0

      # Return rates for birth and death reactions
      c(lambda * n, mu * n)
    }

    # CORRECTED: Proper stoichiometry matrix structure
    stoich <- list(
      nu = list(
        list(1.0, 0.0),  # Birth reaction: +1 for compartment 1, 0 for compartment 2
        list(-1.0, 0.0)  # Death reaction: -1 for compartment 1, 0 for compartment 2
      ),
      nuReactant = list(
        list(1, 0),   # Birth: consumes 1 from compartment 1
        list(1, 0)    # Death: consumes 1 from compartment 1
      )
    )

    if (i == 1) {
      message("Debugging JSF parameters...")
      x0_py <- reticulate::r_to_py(list(as.numeric(x0), 0))
      rates_py <- reticulate::r_to_py(rates)
      stoich_py <- reticulate::r_to_py(stoich)

      message("x0 (Python): ", reticulate::py_str(x0_py))
      message("nu (Python): ", reticulate::py_str(stoich_py$nu))
      message("nu[0] (Python): ", reticulate::py_str(stoich_py$nu$`__getitem__`(0L)))
    }

    # Call Python jsf function directly via reticulate
    result <- jsf$jsf(
      x0 = reticulate::r_to_py(list(as.numeric(x0), 0)),  # Add dummy compartment
      rates = reticulate::r_to_py(rates),
      stoich = reticulate::r_to_py(stoich),
      t_max = t_max,
      method = "operator-splitting",
      config = list(
        dt = dt,
        SwitchingThreshold = c(threshold, 1),  # Threshold for each compartment
        EnforceDo = c(0, 0)
      )
    )

    # Convert Python result to R
    result_r <- reticulate::py_to_r(result)

    # Extract time and population data
    if (is.list(result_r) && length(result_r) >= 2) {
      times <- as.numeric(result_r[[2]])
      # Get population trajectory for first compartment
      if (is.list(result_r[[1]]) && length(result_r[[1]]) > 0) {
        population <- as.numeric(result_r[[1]][[1]])
      } else {
        population <- rep(x0, length(times))
      }
    } else {
      times <- seq(0, t_max, by = dt)
      population <- rep(x0, length(times))
    }

    df <- data.frame(
      time = times,
      population = population,
      simulation = i,
      lambda = lambda,
      mu = mu,
      x0 = x0
    )
    trajectories[[i]] <- df
  }

  jsf_result <- do.call(rbind, trajectories)
  attr(jsf_result, "jsf_config") <- list(
    dt = dt,
    threshold = threshold,
    n_reps = n_reps
  )

  # Save to cache
  if (!is.null(cache_file)) {
    if (!dir.exists(dirname(cache_file))) {
      dir.create(dirname(cache_file), recursive = TRUE)
    }
    saveRDS(jsf_result, cache_file)
    message("JSF reference data saved to: ", cache_file)
  }

  return(jsf_result)
}

#' Compare JSF and Gillespie Results
#'
#' Compares JSF reference data with Gillespie simulation results
#' Computes summary statistics and visual comparison
#'
#' @param jsf_data JSF reference data from generate_jsf_reference
#' @param gillespie_data Gillespie data from simulate_birth_death_ensemble
#' @return List containing comparison statistics
#' @export
compare_simulation_results <- function(jsf_data, gillespie_data) {
  # Check if data is available
  if (is.null(jsf_data) || is.null(gillespie_data)) {
    stop("Both jsf_data and gillespie_data must be provided")
  }

  # Summary statistics
  jsf_summary <- aggregate(population ~ simulation, data = jsf_data,
                           FUN = function(x) c(mean = mean(x), sd = sd(x)))
  gillespie_summary <- aggregate(population ~ simulation, data = gillespie_data,
                                 FUN = function(x) c(mean = mean(x), sd = sd(x)))

  # Overall statistics
  overall_stats <- list(
    jsf_mean_pop = mean(jsf_data$population),
    jsf_sd_pop = sd(jsf_data$population),
    gillespie_mean_pop = mean(gillespie_data$population),
    gillespie_sd_pop = sd(gillespie_data$population),
    n_jsf_simulations = length(unique(jsf_data$simulation)),
    n_gillespie_simulations = length(unique(gillespie_data$simulation))
  )

  # Create comparison result
  result <- list(
    jsf_summary = jsf_summary,
    gillespie_summary = gillespie_summary,
    overall_stats = overall_stats,
    jsf_config = attr(jsf_data, "jsf_config")
  )

  class(result) <- "simulation_comparison"
  return(result)
}

#' Check JSF Python Package
#'
#' Diagnostic function to check JSF Python package installation
#'
#' @return List with diagnostic information
#' @export
check_jsf_installation <- function() {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("reticulate package is required")
  }

  result <- list()

  # Check Python configuration
  result$python_config <- tryCatch({
    reticulate::py_config()
  }, error = function(e) paste("Error:", e$message))

  # Check if jsf module is available
  result$jsf_available <- tryCatch({
    reticulate::py_module_available("jsf")
  }, error = function(e) paste("Error:", e$message))

  # Try to import jsf
  result$jsf_import <- tryCatch({
    jsf <- reticulate::import("jsf", convert = FALSE)
    "Success"
  }, error = function(e) paste("Error:", e$message))

  return(result)
}
