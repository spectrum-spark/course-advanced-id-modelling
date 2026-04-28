# ============================================================
# Generate fig_network_scales.png
#
# Three-panel figure illustrating what a "node" represents
# at different scales of network model:
#
#   Left:   Individual-level network (nodes = people, SIR colours)
#   Middle: Household-level network (connected households, SIR colours)
#   Right:  Population-level network (nodes = subpopulations,
#           node size = population, edge width = mobility flow)
#
# SIR colour scheme (colourblind-friendly):
#   S = blue (#66A5D9), I = orange (#E06D2D), R = grey (#AAAAAA)
#
# Written by Claude Sonnet 4.6 (Anthropic)
# ============================================================

library(ggplot2)
library(gridExtra)

set.seed(42)

# Colourblind-friendly SIR colours
col_S <- "#66A5D9"
col_I <- "#E06D2D"
col_R <- "#AAAAAA"

# -------------------------------------------------------
# Panel 1: Individual-level network
# -------------------------------------------------------
n_ind <- 18
set.seed(42)
ind_nodes <- data.frame(
  x      = runif(n_ind, 0.08, 0.92),
  y      = runif(n_ind, 0.08, 0.92),
  id     = 1:n_ind,
  status = sample(c(rep("S", 10), rep("I", 5), rep("R", 3)))
)
ind_nodes$col <- ifelse(ind_nodes$status == "S", col_S,
                 ifelse(ind_nodes$status == "I", col_I, col_R))

set.seed(7)
edges_ind <- do.call(rbind, lapply(1:n_ind, function(i) {
  nbrs <- sample(setdiff(1:n_ind, i), sample(1:3, 1))
  data.frame(x    = ind_nodes$x[i], y    = ind_nodes$y[i],
             xend = ind_nodes$x[nbrs], yend = ind_nodes$y[nbrs])
}))

p1 <- ggplot() +
  theme_void(base_size = 9) +
  labs(title = "Individual-level\n(nodes = people, colour = status)") +
  theme(plot.title   = element_text(hjust = 0.5, face = "bold", size = 9),
        plot.margin  = margin(5, 5, 5, 5)) +
  geom_segment(data = edges_ind,
               aes(x = x, y = y, xend = xend, yend = yend),
               colour = "grey75", linewidth = 0.4, alpha = 0.7) +
  geom_point(data = ind_nodes, aes(x = x, y = y),
             size = 3.5, colour = ind_nodes$col) +
  # Mini legend
  annotate("point", x = 0.05, y = c(0.96, 0.88, 0.80),
           size = 3, colour = c(col_S, col_I, col_R)) +
  annotate("text",  x = 0.12, y = c(0.96, 0.88, 0.80),
           label = c("S", "I", "R"), size = 2.5, hjust = 0,
           colour = "grey30") +
  xlim(-0.02, 1.02) + ylim(-0.02, 1.02) + coord_fixed()

# -------------------------------------------------------
# Panel 2: Household-level network
#
# Four households arranged in a 2x2 grid.
# SIR statuses tell a transmission narrative:
#   HH1: index case (one I, rest S)
#   HH2: secondary household (one I from contact with HH1)
#   HH3: susceptible household (all S)
#   HH4: recovered household (two R, one S)
# Solid lines = within-household contacts (dense)
# Dashed lines = between-household contacts (sparse)
# -------------------------------------------------------
hh_centres <- data.frame(
  hx  = c(0.22, 0.72, 0.22, 0.72),
  hy  = c(0.72, 0.72, 0.25, 0.25),
  hid = 1:4
)

make_circle <- function(cx, cy, r = 0.15, n = 60) {
  theta <- seq(0, 2 * pi, length.out = n)
  data.frame(x = cx + r * cos(theta), y = cy + r * sin(theta))
}

hh_bounds <- do.call(rbind, lapply(1:4, function(h) {
  circ      <- make_circle(hh_centres$hx[h], hh_centres$hy[h])
  circ$hid  <- h
  circ
}))

hh_statuses <- list(
  c("S", "I", "S"),
  c("S", "I", "S", "S"),
  c("S", "S", "S"),
  c("R", "R", "S")
)

hh_members <- do.call(rbind, lapply(1:4, function(h) {
  n_m    <- length(hh_statuses[[h]])
  angles <- seq(0, 2 * pi, length.out = n_m + 1)[-1]
  status <- hh_statuses[[h]]
  data.frame(
    x      = hh_centres$hx[h] + 0.085 * cos(angles),
    y      = hh_centres$hy[h] + 0.085 * sin(angles),
    hid    = h,
    status = status,
    col    = ifelse(status == "S", col_S,
             ifelse(status == "I", col_I, col_R))
  )
}))

within_edges <- do.call(rbind, lapply(1:4, function(h) {
  m     <- hh_members[hh_members$hid == h, ]
  pairs <- expand.grid(i = 1:nrow(m), j = 1:nrow(m))
  pairs <- pairs[pairs$i < pairs$j, ]
  data.frame(x    = m$x[pairs$i], y    = m$y[pairs$i],
             xend = m$x[pairs$j], yend = m$y[pairs$j])
}))

between_edges <- data.frame(
  x    = hh_centres$hx[c(1, 2, 1, 3)],
  y    = hh_centres$hy[c(1, 2, 1, 3)],
  xend = hh_centres$hx[c(2, 4, 3, 4)],
  yend = hh_centres$hy[c(2, 4, 3, 4)]
)

p2 <- ggplot() +
  theme_void(base_size = 9) +
  labs(title = "Household-level\n(connected households)") +
  theme(plot.title  = element_text(hjust = 0.5, face = "bold", size = 9),
        plot.margin = margin(5, 5, 5, 5)) +
  geom_segment(data = between_edges,
               aes(x = x, y = y, xend = xend, yend = yend),
               colour = "grey55", linewidth = 0.7, linetype = "dashed") +
  geom_polygon(data = hh_bounds,
               aes(x = x, y = y, group = hid),
               fill = "#f5f5f5", colour = "grey40", linewidth = 0.7) +
  geom_segment(data = within_edges,
               aes(x = x, y = y, xend = xend, yend = yend),
               colour = "grey60", linewidth = 0.5) +
  geom_point(data = hh_members, aes(x = x, y = y),
             size = 3, colour = hh_members$col) +
  annotate("point", x = 0.05, y = c(0.96, 0.88, 0.80),
           size = 3, colour = c(col_S, col_I, col_R)) +
  annotate("text",  x = 0.12, y = c(0.96, 0.88, 0.80),
           label = c("S", "I", "R"), size = 2.5, hjust = 0,
           colour = "grey30") +
  xlim(0.02, 0.98) + ylim(0.02, 0.98) + coord_fixed()

# -------------------------------------------------------
# Panel 3: Population-level
#
# Node size proportional to population.
# Edge width proportional to mobility flow.
# Labels manually offset to avoid overlapping edges.
# -------------------------------------------------------
pop_nodes <- data.frame(
  x   = c(0.20, 0.75, 0.50, 0.15, 0.82),
  y   = c(0.78, 0.75, 0.45, 0.22, 0.22),
  pop = c(8,    5,    4,    3,    6),
  lbl = c("City A", "City B", "Town C", "Village D", "City E"),
  lx  = c(0.08, 0.87, 0.50, 0.04, 0.93),   # label x positions
  ly  = c(0.84, 0.83, 0.35, 0.13, 0.13)    # label y positions
)
from_p <- c(1, 1, 2, 2, 3, 3, 4, 5)
to_p   <- c(2, 3, 3, 5, 4, 5, 5, 3)
flows  <- c(3, 1, 2, 2, 1, 1, 1, 2)
pop_edges <- data.frame(
  x    = pop_nodes$x[from_p], y    = pop_nodes$y[from_p],
  xend = pop_nodes$x[to_p],   yend = pop_nodes$y[to_p],
  w    = flows
)

p3 <- ggplot() +
  theme_void(base_size = 9) +
  labs(title = "Population-level\n(nodes = subpopulations)") +
  theme(plot.title  = element_text(hjust = 0.5, face = "bold", size = 9),
        plot.margin = margin(5, 5, 5, 5)) +
  geom_segment(data = pop_edges,
               aes(x = x, y = y, xend = xend, yend = yend,
                   linewidth = I(w * 0.5)),
               colour = "grey50", alpha = 0.8) +
  geom_point(data = pop_nodes,
             aes(x = x, y = y, size = I(pop * 1.3)),
             colour = "#2c7bb6") +
  geom_text(data = pop_nodes,
            aes(x = lx, y = ly, label = lbl),
            size = 2.3, colour = "grey25", hjust = 0.5) +
  xlim(0.0, 1.0) + ylim(0.0, 1.0) + coord_fixed()

# -------------------------------------------------------
# Combine and save
# -------------------------------------------------------
png("fig_network_scales.png", width = 960, height = 390, res = 120)
grid.arrange(
  p1, p2, p3,
  ncol = 3,
  top  = grid::textGrob(
    "Network models: what does a node represent?",
    gp = grid::gpar(fontsize = 12, fontface = "bold")
  )
)
dev.off()
