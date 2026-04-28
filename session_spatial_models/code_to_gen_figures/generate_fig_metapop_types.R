# ============================================================
# Generate fig_metapop_types.png
#
# Four-panel schematic illustrating different metapopulation
# model structures:
#   1. Island (hub) model
#   2. Stepping-stone (grid)
#   3. Stepping-stone (abstracted as ring)
#   4. Commuter model with heterogeneous flows
#
# Source-sink is noted in the caption as a special case of
# the island/commuter model with asymmetric rates rather
# than as a separate panel.
#
# Written by Claude Sonnet 4.6 (Anthropic)
# ============================================================

library(ggplot2)
library(gridExtra)

theta5 <- seq(0, 2*pi, length.out = 6)[-6]

# -------------------------------------------------------
# Panel 1: Island (hub) model
# -------------------------------------------------------
nodes_hub <- data.frame(
  x    = c(0, cos(theta5)),
  y    = c(0, sin(theta5)),
  id   = 1:6,
  size = c(7, rep(4, 5)),
  col  = c("#d7191c", rep("#2c7bb6", 5))
)
edges_hub <- data.frame(
  x    = nodes_hub$x[1],
  y    = nodes_hub$y[1],
  xend = nodes_hub$x[2:6],
  yend = nodes_hub$y[2:6]
)

# -------------------------------------------------------
# Panel 2a: Stepping-stone (3x3 grid, matching CA schematic)
# -------------------------------------------------------
grid_n <- 3
gx <- rep(1:grid_n, each = grid_n)
gy <- rep(1:grid_n, times = grid_n)
nodes_grid <- data.frame(x = gx, y = gy, id = 1:(grid_n^2))

from_g <- c(); to_g <- c()
for (r in 1:grid_n) {
  for (cc in 1:grid_n) {
    idx <- (r - 1) * grid_n + cc
    if (cc < grid_n) { from_g <- c(from_g, idx); to_g <- c(to_g, idx + 1) }
    if (r  < grid_n) { from_g <- c(from_g, idx); to_g <- c(to_g, idx + grid_n) }
  }
}
edges_grid <- data.frame(
  x    = nodes_grid$x[from_g],
  y    = nodes_grid$y[from_g],
  xend = nodes_grid$x[to_g],
  yend = nodes_grid$y[to_g]
)

# -------------------------------------------------------
# Panel 2b: Stepping-stone (abstracted as ring)
# -------------------------------------------------------
nodes_ring <- data.frame(x = cos(theta5), y = sin(theta5),
                          id = 1:5, size = 4, col = "#2c7bb6")
edges_ring <- data.frame(
  x    = nodes_ring$x[c(1, 2, 3, 4, 5)],
  y    = nodes_ring$y[c(1, 2, 3, 4, 5)],
  xend = nodes_ring$x[c(2, 3, 4, 5, 1)],
  yend = nodes_ring$y[c(2, 3, 4, 5, 1)]
)

# -------------------------------------------------------
# Panel 4: Commuter model (fully connected, varying widths)
# Line thickness represents mobility flow strength
# -------------------------------------------------------
nodes_comm <- data.frame(x = cos(theta5), y = sin(theta5),
                          id = 1:5, size = 4, col = "#2c7bb6")
from_all <- c(1, 1, 1, 1, 2, 2, 2, 3, 3, 4)
to_all   <- c(2, 3, 4, 5, 3, 4, 5, 4, 5, 5)
weights  <- c(3, 1, 2, 1, 1, 1, 2, 1, 1, 1)
edges_comm <- data.frame(
  x    = nodes_comm$x[from_all],
  y    = nodes_comm$y[from_all],
  xend = nodes_comm$x[to_all],
  yend = nodes_comm$y[to_all],
  w    = weights
)

# -------------------------------------------------------
# Plotting helpers
# -------------------------------------------------------
plot_circle <- function(nodes_df, edges_df, title, vary_width = FALSE) {
  p <- ggplot() +
    theme_void(base_size = 9) +
    labs(title = title) +
    theme(plot.title    = element_text(hjust = 0.5, face = "bold", size = 9),
          plot.margin   = margin(4, 4, 4, 4))

  if (vary_width) {
    p <- p + geom_segment(data = edges_df,
                          aes(x = x, y = y, xend = xend, yend = yend,
                              linewidth = I(w * 0.4)),
                          colour = "grey50", alpha = 0.8)
  } else {
    p <- p + geom_segment(data = edges_df,
                          aes(x = x, y = y, xend = xend, yend = yend),
                          colour = "grey50", linewidth = 0.9)
  }

  p + geom_point(data = nodes_df, aes(x = x, y = y),
                 size   = nodes_df$size,
                 colour = nodes_df$col) +
    xlim(-1.7, 1.7) + ylim(-1.7, 1.7) + coord_fixed()
}

plot_grid_panel <- function(nodes_df, edges_df, title) {
  ggplot() +
    theme_void(base_size = 9) +
    labs(title = title) +
    theme(plot.title  = element_text(hjust = 0.5, face = "bold", size = 9),
          plot.margin = margin(4, 4, 4, 4)) +
    geom_segment(data = edges_df,
                 aes(x = x, y = y, xend = xend, yend = yend),
                 colour = "grey50", linewidth = 0.9) +
    geom_tile(data = nodes_df, aes(x = x, y = y),
              width = 0.82, height = 0.82,
              fill = "#cce5ff", colour = "grey40", linewidth = 0.5) +
    xlim(0.3, grid_n + 0.7) + ylim(0.3, grid_n + 0.7) +
    coord_fixed()
}

# -------------------------------------------------------
# Build panels
# -------------------------------------------------------
p1  <- plot_circle(nodes_hub, edges_hub,
                   "Island (hub) model\n(equal flows to/from centre)")

p2a <- plot_grid_panel(nodes_grid, edges_grid,
                       "Stepping-stone\n(grid, neighbours only)")

p2b <- plot_circle(nodes_ring, edges_ring,
                   "Stepping-stone\n(abstracted as ring)")

p4  <- plot_circle(nodes_comm, edges_comm,
                   "Commuter model\n(heterogeneous flows)",
                   vary_width = TRUE)

# -------------------------------------------------------
# Combine and save
# -------------------------------------------------------
png("fig_metapop_types.png", width = 960, height = 380, res = 120)
grid.arrange(
  p1, p2a, p2b, p4,
  ncol = 4,
  top  = grid::textGrob(
    "Types of metapopulation model",
    gp = grid::gpar(fontsize = 12, fontface = "bold")
  )
)
dev.off()
