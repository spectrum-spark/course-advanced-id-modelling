# ============================================================
# Generate fig4_synchrony.png
#
# Two-panel figure illustrating the transition from independent
# to synchronised epidemic dynamics as coupling strength
# increases in a two-patch SIR model.
#
# Left:  weak coupling (l = 0.001) -- patches largely independent
# Right: strong coupling (l = 0.08) -- patches synchronised
#
# Uses the same two-patch commuter SIR model as the practical.
#
# Written by Claude Sonnet 4.6 (Anthropic)
# ============================================================

library(deSolve)
library(ggplot2)
library(gridExtra)

# -------------------------------------------------------
# Two-patch SIR: commuter framework
# (identical to the practical code)
# -------------------------------------------------------
two_patch_sir <- function(t, state, parms) {
  with(as.list(c(state, parms)), {

    # Total current population in each patch (residents + visitors)
    N1 <- S11 + I11 + R11 + S21 + I21 + R21
    N2 <- S22 + I22 + R22 + S12 + I12 + R12

    # Total infecteds currently in each patch
    Itot1 <- I11 + I21
    Itot2 <- I22 + I12

    # Force of infection in each patch
    lam1 <- beta * Itot1 / N1
    lam2 <- beta * Itot2 / N2

    # Residents of patch 1 at home
    dS11 <- -lam1 * S11 - l * S11 + r * S12
    dI11 <-  lam1 * S11 - gamma * I11 - l * I11 + r * I12
    dR11 <-  gamma * I11 - l * R11 + r * R12

    # Residents of patch 1 visiting patch 2
    dS12 <- -lam2 * S12 + l * S11 - r * S12
    dI12 <-  lam2 * S12 - gamma * I12 + l * I11 - r * I12
    dR12 <-  gamma * I12 + l * R11 - r * R12

    # Residents of patch 2 at home
    dS22 <- -lam2 * S22 - l * S22 + r * S21
    dI22 <-  lam2 * S22 - gamma * I22 - l * I22 + r * I21
    dR22 <-  gamma * I22 - l * R22 + r * R21

    # Residents of patch 2 visiting patch 1
    dS21 <- -lam1 * S21 + l * S22 - r * S21
    dI21 <-  lam1 * S21 - gamma * I21 + l * I22 - r * I21
    dR21 <-  gamma * I21 + l * R22 - r * R21

    list(c(dS11, dI11, dR11,
           dS12, dI12, dR12,
           dS22, dI22, dR22,
           dS21, dI21, dR21))
  })
}

# -------------------------------------------------------
# Shared initial conditions and fixed parameters
# -------------------------------------------------------
state0 <- c(
  S11 = 990, I11 = 10, R11 = 0,
  S12 = 0,   I12 = 0,  R12 = 0,
  S22 = 1000, I22 = 0, R22 = 0,
  S21 = 0,   I21 = 0,  R21 = 0
)

times <- seq(0, 300, by = 0.5)

# Helper: run model for a given leaving rate l
run_patch <- function(l_val) {
  parms <- c(beta = 0.4, gamma = 0.1, l = l_val, r = 0.5)
  out   <- as.data.frame(ode(y = state0, times = times,
                              func = two_patch_sir, parms = parms))
  out$I_patch1 <- out$I11 + out$I21
  out$I_patch2 <- out$I22 + out$I12
  out
}

# -------------------------------------------------------
# Run for weak and strong coupling
# q = l / (r + l):
#   weak:   l = 0.001 -> q = 0.001 / 0.501 ≈ 0.002
#   strong: l = 0.08  -> q = 0.08  / 0.58  ≈ 0.138
# -------------------------------------------------------
out_weak   <- run_patch(0.001)
out_strong <- run_patch(0.08)

# -------------------------------------------------------
# Plotting helper
# -------------------------------------------------------
make_panel <- function(out, title) {
  ggplot(out, aes(x = time)) +
    geom_line(aes(y = I_patch1, colour = "Patch 1 (source)"),
              linewidth = 0.9) +
    geom_line(aes(y = I_patch2, colour = "Patch 2 (naive)"),
              linewidth = 0.9) +
    scale_colour_manual(
      values = c("Patch 1 (source)" = "#d7191c",
                 "Patch 2 (naive)"  = "#2c7bb6")
    ) +
    labs(x = "Time (days)", y = "Infected", colour = NULL,
         title = title) +
    theme_bw(base_size = 10) +
    theme(legend.position = "bottom")
}

p_weak   <- make_panel(out_weak,
                        "Weak coupling (l = 0.001)\nPatches largely independent")
p_strong <- make_panel(out_strong,
                        "Strong coupling (l = 0.08)\nPatches synchronised")

# -------------------------------------------------------
# Combine and save
# -------------------------------------------------------
png("fig4_synchrony.png", width = 900, height = 380, res = 120)
grid.arrange(p_weak, p_strong, ncol = 2)
dev.off()
