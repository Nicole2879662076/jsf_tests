#' Gillespie Algorithm for Exact Stochastic Simulation of Birth-Death Process
#'
#' Implements the exact Gillespie stochastic simulation algorithm, supporting single trajectories
#'
#' @param lambda Birth rate
#' @param mu Death rate
#' @param x0 Initial population size
#' @param t_max Total simulation time
#' @param seed Random seed for reproducibility
#' @return A list containing time points and population states
#' @export
gillespie_birth_death_single <- function(
    lambda = 1.0,
    mu = 0.5,
    x0 = 5,
    t_max = 10.0,
    seed = NULL
) {
  # Set random seed if provided
  if (!is.null(seed)) set.seed(seed)

  # Initialize simulation
  t <- 0.0
  x <- x0
  times <- t
  states <- x
  events <- 0

  # Main Gillespie simulation loop
  while (t < t_max && x > 0 && x < 10000) {
    # Calculate reaction propensities
    rate_birth <- lambda * x
    rate_death <- mu * x
    rate_total <- rate_birth + rate_death

    # Break if no reactions are possible
    if (rate_total <= 0) break

    # Time to next reaction (exponentially distributed)
    dt <- rexp(1, rate_total)
    t <- t + dt

    # Check if simulation time exceeds maximum
    if (t > t_max) break

    # Determine which reaction occurs
    if (runif(1) < rate_birth / rate_total) {
      x <- x + 1  # Birth event
    } else {
      x <- x - 1  # Death event
    }

    # Record state
    times <- c(times, t)
    states <- c(states, x)
    events <- events + 1
  }

  # Ensure the time series includes the endpoint at t_max
  if (tail(times, 1) < t_max) {
    times <- c(times, t_max)
    states <- c(states, tail(states, 1))
  }

  # Create result list
  result <- list(
    time = times,
    states = states
  )

  # Add metadata as attributes
  attr(result, "parameters") <- list(
    lambda = lambda,
    mu = mu,
    x0 = x0,
    t_max = t_max,
    events = events
  )

  return(result)
}

#' Multiple Replicate Gillespie Simulations
#'
#' Runs multiple Gillespie simulations and returns ensemble trajectories
#'
#' @param n_reps Number of simulation replicates
#' @param seed Main random seed for reproducibility
#' @param ... Additional parameters passed to gillespie_birth_death_single
#' @return Data frame containing all simulation trajectories
#' @export
simulate_birth_death_ensemble <- function(
    lambda = 1.0,
    mu = 0.5,
    x0 = 5,
    t_max = 10.0,
    n_reps = 100,
    seed = 2026
) {
  # Set main random seed
  set.seed(seed)

  trajectories <- list()
  for (i in seq_len(n_reps)) {
    # Run single simulation (seed = NULL to get different trajectories)
    sim <- gillespie_birth_death_single(
      lambda = lambda,
      mu = mu,
      x0 = x0,
      t_max = t_max,
      seed = NULL
    )

    # Create data frame for this trajectory
    df <- data.frame(
      time = sim$time,
      population = sim$states,
      simulation = i,
      lambda = lambda,
      mu = mu,
      x0 = x0
    )
    trajectories[[i]] <- df
  }

  # Combine all trajectories
  result <- do.call(rbind, trajectories)

  # Add ensemble metadata
  attr(result, "ensemble_info") <- list(
    n_reps = n_reps,
    seed = seed,
    total_events = sum(sapply(trajectories,
                              function(df) length(df$time)))
  )

  return(result)
}
