# ============================================================
# Generate fig1_spatial_vs_nonspatial.png
#
# Left panel:  standard (non-spatial) SIR epidemic curve
# Right panel: 1D reaction-diffusion SIR solved by finite
#              differences, showing the travelling wave at
#              five time snapshots
#
# The same five colours link corresponding time points
# across both panels via dashed vertical lines (left) and
# wave profiles (right).
#
# Written by Claude Sonnet 4.6 (Anthropic)
# ============================================================

library(deSolve)
library(ggplot2)
library(gridExtra)

# ------------------------------------------------------------
# Shared parameters
# ------------------------------------------------------------
beta  <- 0.4    # transmission rate (per day)
gamma <- 0.1    # recovery rate (per day)  ->  R0 = 4

# ------------------------------------------------------------
# Non-spatial SIR
# ------------------------------------------------------------
sir <- function(t, state, parms) {
  with(as.list(c(state, parms)), {
    N <- S + I + R
    list(c(
      -beta * S * I / N,
       beta * S * I / N - gamma * I,
       gamma * I
    ))
  })
}

out_ns <- as.data.frame(ode(
  y     = c(S = 99900, I = 100, R = 0),
  times = seq(0, 150, by = 1),
  func  = sir,
  parms = c(beta = beta, gamma = gamma)
))

# ------------------------------------------------------------
# Spatial SIR: 1D reaction-diffusion via finite differences
#
# Space is divided into n cells of width cell_spacing_km.
# The state vector is [S_1,...,S_n, I_1,...,I_n, R_1,...,R_n].
# The Laplacian is approximated with a second-order central
# finite difference and reflecting (Neumann) boundary conditions.
# ------------------------------------------------------------
n               <- 100     # number of spatial cells
D               <- 0.5     # diffusion coefficient
cell_spacing_km <- 0.5     # physical width of each cell (km)
                            # -> domain = 50 km

spatial_sir <- function(t, state, parms) {
  n     <- parms$n
  beta  <- parms$beta
  gamma <- parms$gamma
  D     <- parms$D

  S <- state[1:n]
  I <- state[(n + 1):(2 * n)]
  R <- state[(2 * n + 1):(3 * n)]
  N <- S + I + R

  # Local SIR dynamics
  dS <- -beta * S * I / N
  dI <-  beta * S * I / N - gamma * I
  dR <-  gamma * I

  # Finite-difference Laplacian with reflecting boundaries
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

# Initial conditions: infection seeded at cell 1 only
S0 <- rep(990, n)
I0 <- rep(0,   n); I0[1] <- 10
R0 <- rep(0,   n)

out_sp <- ode(
  y      = c(S0, I0, R0),
  times  = seq(0, 150, by = 1),
  func   = spatial_sir,
  parms  = list(beta = beta, gamma = gamma, D = D, n = n),
  method = "lsoda"
)

# Extract infected matrix: rows = time, cols = spatial cells
I_mat <- out_sp[, (n + 2):(2 * n + 1)]

# ------------------------------------------------------------
# Snapshot times chosen to span rise, peak, and tail of the
# non-spatial epidemic curve (peak is near t = 50)
# ------------------------------------------------------------
snap_times <- c(15, 30, 50, 75, 110)
snap_cols  <- c("#2166ac", "#4dac26", "#d6604d", "#984ea3", "#ff7f00")

# ------------------------------------------------------------
# Left panel: non-spatial epidemic curve with coloured
# dashed lines at each snapshot time
# ------------------------------------------------------------
p_left <- ggplot(out_ns, aes(x = time, y = I)) +
  geom_line(colour = "grey30", linewidth = 1) +
  geom_vline(xintercept = snap_times,
             colour     = snap_cols,
             linetype   = "dashed",
             linewidth  = 0.7) +
  labs(x     = "Time (days)",
       y     = "Number infected",
       title = "Non-spatial SIR\n(well-mixed population)") +
  theme_bw(base_size = 11)

# ------------------------------------------------------------
# Right panel: spatial wave profiles at each snapshot time
# ------------------------------------------------------------
plot_df <- do.call(rbind, lapply(seq_along(snap_times), function(k) {
  t_idx <- match(snap_times[k], seq(0, 150, by = 1))
  data.frame(
    distance_km = (1:n) * cell_spacing_km,
    infected    = I_mat[t_idx, ],
    time        = factor(
      paste0("t = ", snap_times[k], " days"),
      levels = paste0("t = ", snap_times, " days")
    )
  )
}))

p_right <- ggplot(plot_df, aes(x = distance_km, y = infected,
                                colour = time)) +
  geom_line(linewidth = 0.85) +
  scale_colour_manual(
    values = setNames(snap_cols, paste0("t = ", snap_times, " days"))
  ) +
  labs(x     = "Distance (km)",
       y     = "Number infected",
       title = "Spatial SIR\n(travelling wave at multiple time points)") +
  theme_bw(base_size = 11) +
  theme(legend.position = "none")

# ------------------------------------------------------------
# Combine and save
# ------------------------------------------------------------
png("fig1_spatial_vs_nonspatial.png", width = 960, height = 380, res = 120)
grid.arrange(p_left, p_right, ncol = 2)
dev.off()
