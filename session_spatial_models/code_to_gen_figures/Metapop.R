library(deSolve)
library(ggplot2)

# -------------------------------------------------------
# Two-patch SIR: commuter framework
# Subscript convention: S_{ij} = susceptibles from patch j
# currently in patch i. First subscript = current location,
# second subscript = home patch.
# -------------------------------------------------------
two_patch_sir <- function(t, state, parms) {
  with(as.list(c(state, parms)), {
    
    # Total current population in each patch (residents + visitors)
    # Patch 1: locals (11) + visitors from patch 2 (12)
    # Patch 2: locals (22) + visitors from patch 1 (21)
    N1 <- S11 + I11 + R11 + S12 + I12 + R12
    N2 <- S22 + I22 + R22 + S21 + I21 + R21
    
    # Total infecteds currently in each patch
    Itot1 <- I11 + I12
    Itot2 <- I22 + I21
    
    # Force of infection in each patch
    lambda1 <- beta * Itot1 / N1
    lambda2 <- beta * Itot2 / N2
    
    # --- Patch-1 residents at home (S_{11}) ---
    dS11 <- -lambda1 * S11 - l * S11 + r * S21
    dI11 <-  lambda1 * S11 - gamma * I11 - l * I11 + r * I21
    dR11 <-  gamma * I11   - l * R11 + r * R21
    
    # --- Patch-2 residents visiting patch 1 (S_{12}) ---
    dS12 <- -lambda1 * S12 + l * S22 - r * S12
    dI12 <-  lambda1 * S12 - gamma * I12 + l * I22 - r * I12
    dR12 <-  gamma * I12   + l * R22 - r * R12
    
    # --- Patch-2 residents at home (S_{22}) ---
    dS22 <- -lambda2 * S22 - l * S22 + r * S12
    dI22 <-  lambda2 * S22 - gamma * I22 - l * I22 + r * I12
    dR22 <-  gamma * I22   - l * R22 + r * R12
    
    # --- Patch-1 residents visiting patch 2 (S_{21}) ---
    dS21 <- -lambda2 * S21 + l * S11 - r * S21
    dI21 <-  lambda2 * S21 - gamma * I21 + l * I11 - r * I21
    dR21 <-  gamma * I21   + l * R11 - r * R21
    
    list(c(dS11, dI11, dR11,
           dS12, dI12, dR12,
           dS22, dI22, dR22,
           dS21, dI21, dR21))
  })
}

# -------------------------------------------------------
# Parameters
# -------------------------------------------------------
parms <- c(
  beta  = 0.4,   # transmission rate (per day)
  gamma = 0.1,   # recovery rate (per day, so mean duration = 10 days)
  l     = 0.01,  # rate of leaving home patch (per day)
  r     = 0.5    # rate of returning home (per day)
)

# -------------------------------------------------------
# Initial conditions
# -------------------------------------------------------
# Patch 1: high-burden community, TB established (10 infectious)
# Patch 2: low-burden community, initially TB-free
# No one is visiting at t = 0
state <- c(
  S11 = 990,  I11 = 10, R11 = 0,   # patch-1 residents at home
  S12 = 0,    I12 = 0,  R12 = 0,   # patch-2 residents visiting patch 1
  S22 = 1000, I22 = 0,  R22 = 0,   # patch-2 residents at home
  S21 = 0,    I21 = 0,  R21 = 0    # patch-1 residents visiting patch 2
)

times <- seq(0, 300, by = 0.5)
out   <- ode(y = state, times = times, func = two_patch_sir,
             parms = parms)
out   <- as.data.frame(out)

# -------------------------------------------------------
# Total infecteds in each patch (residents + visitors)
# -------------------------------------------------------
out$I_patch1 <- out$I11 + out$I12
out$I_patch2 <- out$I22 + out$I21

ggplot(out, aes(x = time)) +
  geom_line(aes(y = I_patch1, colour = "Patch 1 (high burden)"), linewidth = 0.9) +
  geom_line(aes(y = I_patch2, colour = "Patch 2 (low burden)"),  linewidth = 0.9) +
  labs(x = "Time (days)", y = "Number infected",
       colour = "Patch",
       title = "Two-patch TB model: infectious individuals in each patch") +
  theme_bw()

