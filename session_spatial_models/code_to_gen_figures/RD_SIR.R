library(deSolve)
library(ggplot2)

# -------------------------------------------------------
# Reaction-diffusion SIR: discretised in 1D space
# -------------------------------------------------------

spatial_sir <- function(t, state, parms) {
  n     <- parms$n
  beta  <- parms$beta
  gamma <- parms$gamma
  D     <- parms$D
  
  S <- state[1:n]
  I <- state[(n + 1):(2 * n)]
  R <- state[(2 * n + 1):(3 * n)]
  N <- S + I + R
  
  # SIR dynamics at each location
  dS <- -beta * S * I / N
  dI <-  beta * S * I / N - gamma * I
  dR <-  gamma * I
  
  # Diffusion: finite difference Laplacian with reflecting boundaries
  laplacian <- function(x) {
    c(x[2] - x[1],
      x[3:n] - 2 * x[2:(n - 1)] + x[1:(n - 2)],
      x[n - 1] - x[n])
  }
  
  dS <- dS + D * laplacian(S)
  dI <- dI + D * laplacian(I)
  dR <- dR + D * laplacian(R)
  
  list(c(dS, dI, dR))
}

# -------------------------------------------------------
# Parameters and initial conditions
# -------------------------------------------------------
n     <- 100    # number of spatial cells
beta  <- 0.4    # transmission rate
gamma <- 0.1    # recovery rate
D     <- 0.5    # diffusion coefficient

# All cells start with 990 susceptibles and 0 infecteds,
# except cell 1 which has 10 infecteds to seed the outbreak.
S0 <- rep(990, n)
I0 <- rep(0, n); I0[50] <- 10
R0 <- rep(0,   n)

state <- c(S0, I0, R0)
parms <- list(beta = beta, gamma = gamma, D = D, n = n)
times <- seq(0, 150, by = 1)

out <- ode(y = state, times = times, func = spatial_sir,
           parms = parms, method = "lsoda")

# -------------------------------------------------------
# Extract infected counts and plot snapshots over time
# -------------------------------------------------------
I_mat <- out[, (n + 2):(2 * n + 1)]  # I for each cell at each time

# Pick a few time snapshots to plot
snap_times <- c(1, 25, 50, 75, 100, 125, 150)
snap_idx   <- match(snap_times, times)

plot_df <- do.call(rbind, lapply(seq_along(snap_times), function(k) {
  data.frame(
    location = 1:n,
    infected = I_mat[snap_idx[k], ],
    time     = paste0("t = ", snap_times[k])
  )
}))
plot_df$time <- factor(plot_df$time,
                       levels = paste0("t = ", snap_times))

ggplot(plot_df, aes(x = location, y = infected, colour = time)) +
  geom_line(linewidth = 0.8) +
  labs(x = "Location (spatial cell)",
       y = "Number infected",
       colour = "Time",
       title = "Travelling wave of infection across space") +
  theme_bw()

