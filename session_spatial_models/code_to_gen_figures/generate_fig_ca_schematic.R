# ============================================================
# Generate fig_ca_schematic.png
#
# Three-panel figure illustrating the progression from
# continuous space to discretised patches to cellular automaton.
#
# Left:   smooth continuous infection density field
# Middle: same field averaged into coarse patches (metapopulation step)
# Right:  fully discretised cellular automaton with discrete S/I/R states
#
# Colourblind-friendly palettes throughout.
#
# Written by Claude Sonnet 4.6 (Anthropic)
# ============================================================

library(ggplot2)
library(gridExtra)

set.seed(42)

# -------------------------------------------------------
# Shared: continuous infection density field
# Two Gaussian foci representing outbreak locations
# -------------------------------------------------------
n_fine <- 80
x <- seq(0, 1, length.out = n_fine)
y <- seq(0, 1, length.out = n_fine)
grid_fine <- expand.grid(x = x, y = y)

gauss2d <- function(cx, cy, sx, sy, g) {
  exp(-((g$x - cx)^2 / (2 * sx^2) + (g$y - cy)^2 / (2 * sy^2)))
}

grid_fine$density <- (
  0.9  * gauss2d(0.35, 0.60, 0.12, 0.14, grid_fine) +
  0.6  * gauss2d(0.72, 0.30, 0.10, 0.10, grid_fine) +
  0.15 * gauss2d(0.55, 0.80, 0.07, 0.07, grid_fine)
)
grid_fine$density <- grid_fine$density / max(grid_fine$density)

# Shared sequential palette: white -> yellow -> orange -> dark red
cont_pal <- c("#FFFFFF", "#FFF7BC", "#FEC44F", "#D95F0E", "#7F0000")

# -------------------------------------------------------
# Left panel: continuous density
# -------------------------------------------------------
p_left <- ggplot(grid_fine, aes(x = x, y = y, fill = density)) +
  geom_tile() +
  scale_fill_gradientn(colours = cont_pal,
                       name    = "Infection\ndensity",
                       limits  = c(0, 1)) +
  coord_fixed() +
  labs(title    = "Continuous space",
       subtitle = "Smooth infection density") +
  theme_void(base_size = 10) +
  theme(
    plot.title        = element_text(hjust = 0.5, face = "bold", size = 10),
    plot.subtitle     = element_text(hjust = 0.5, size = 8, colour = "grey30"),
    legend.position   = "right",
    legend.key.height = unit(1.0, "cm"),
    legend.title      = element_text(size = 7),
    legend.text       = element_text(size = 7),
    plot.margin       = margin(5, 2, 5, 2)
  )

# -------------------------------------------------------
# Middle panel: coarsely discretised patches (8x8)
# Average the continuous density within each patch cell.
# Uses same colour scale as left panel to show it is still
# the same quantity, just more coarsely represented.
# -------------------------------------------------------
n_coarse <- 8
grid_fine$patch_x <- ceiling(grid_fine$x * n_coarse) / n_coarse - 1 / (2 * n_coarse)
grid_fine$patch_y <- ceiling(grid_fine$y * n_coarse) / n_coarse - 1 / (2 * n_coarse)

patch_df <- aggregate(density ~ patch_x + patch_y,
                      data = grid_fine, FUN = mean)

p_mid <- ggplot(patch_df, aes(x = patch_x, y = patch_y, fill = density)) +
  geom_tile(colour    = "grey60",
            linewidth = 0.4,
            width     = 1 / n_coarse * 0.98,
            height    = 1 / n_coarse * 0.98) +
  scale_fill_gradientn(colours = cont_pal,
                       name    = "Infection\ndensity",
                       limits  = c(0, 1)) +
  coord_fixed() +
  labs(title    = "Discretised patches",
       subtitle = "Average density per patch") +
  theme_void(base_size = 10) +
  theme(
    plot.title        = element_text(hjust = 0.5, face = "bold", size = 10),
    plot.subtitle     = element_text(hjust = 0.5, size = 8, colour = "grey30"),
    legend.position   = "right",
    legend.key.height = unit(1.0, "cm"),
    legend.title      = element_text(size = 7),
    legend.text       = element_text(size = 7),
    plot.margin       = margin(5, 2, 5, 2)
  )

# -------------------------------------------------------
# Right panel: cellular automaton (28x28)
# Each cell has a single discrete state: S, I, or R.
# Foci placed to match approximate locations in left panel.
# Colourblind-friendly: S = blue, I = orange, R = grey
# -------------------------------------------------------
n_ca <- 28
ca   <- matrix("S", nrow = n_ca, ncol = n_ca)

# Helper: seed cells of a given state within a circular region
seed_state <- function(mat, cx, cy, radius, n, state) {
  attempts <- 0
  placed   <- 0
  while (placed < n && attempts < n * 20) {
    r        <- sample(1:nrow(mat), 1)
    cc       <- sample(1:ncol(mat), 1)
    attempts <- attempts + 1
    if (sqrt((r / nrow(mat) - cx)^2 + (cc / ncol(mat) - cy)^2) < radius) {
      mat[r, cc] <- state
      placed     <- placed + 1
    }
  }
  mat
}

# Seed infected cells at the two outbreak foci
ca <- seed_state(ca, 0.60, 0.35, 0.18, 18, "I")
ca <- seed_state(ca, 0.30, 0.72, 0.14, 12, "I")
# Seed recovered cells near the centre of each focus
ca <- seed_state(ca, 0.60, 0.35, 0.10,  6, "R")
ca <- seed_state(ca, 0.30, 0.72, 0.08,  4, "R")

ca_df       <- expand.grid(row = 1:n_ca, col = 1:n_ca)
ca_df$state <- as.vector(ca)
ca_df$state <- factor(ca_df$state, levels = c("S", "I", "R"))

state_cols <- c("S" = "#66A5D9", "I" = "#E06D2D", "R" = "#AAAAAA")
state_labs <- c("S" = "Susceptible", "I" = "Infected", "R" = "Recovered")

p_right <- ggplot(ca_df, aes(x = col, y = row, fill = state)) +
  geom_tile(colour = "white", linewidth = 0.25) +
  scale_fill_manual(values = state_cols,
                    labels = state_labs,
                    name   = NULL) +
  coord_fixed() +
  labs(title    = "Cellular automaton",
       subtitle = "Each cell has one discrete state") +
  theme_void(base_size = 10) +
  theme(
    plot.title       = element_text(hjust = 0.5, face = "bold", size = 10),
    plot.subtitle    = element_text(hjust = 0.5, size = 8, colour = "grey30"),
    legend.position  = "right",
    legend.key.size  = unit(0.45, "cm"),
    legend.text      = element_text(size = 7),
    plot.margin      = margin(5, 2, 5, 2)
  )

# -------------------------------------------------------
# Combine and save
# -------------------------------------------------------
png("fig_ca_schematic.png", width = 1100, height = 400, res = 120)
grid.arrange(
  p_left, p_mid, p_right,
  ncol = 3,
  top  = grid::textGrob(
    "Discretising continuous space: from density to discrete states",
    gp = grid::gpar(fontsize = 11, fontface = "bold")
  )
)
dev.off()
