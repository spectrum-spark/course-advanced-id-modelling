# ============================================================
# Generate fig5_networks.png
#
# Three-panel figure showing example networks of each type:
#   Left:   Random (Erdős–Rényi)
#   Middle: Small-world (Watts–Strogatz)
#   Right:  Scale-free (Barabási–Albert)
#
# Node size is proportional to degree, making the hub
# structure in the scale-free network immediately visible.
#
# Written by Claude Sonnet 4.6 (Anthropic)
# ============================================================

library(igraph)

set.seed(42)
n <- 40  # small for visual clarity

# -------------------------------------------------------
# Build networks
# -------------------------------------------------------
g_random <- sample_gnp(n, p = 0.12)
g_sw     <- sample_smallworld(1, n, nei = 3, p = 0.05)
g_sf     <- sample_pa(n, m = 2, directed = FALSE)

# -------------------------------------------------------
# Plotting helper
# Node size scaled by degree so hubs are visible
# -------------------------------------------------------
plot_network <- function(g, title, node_col) {
  lay <- layout_with_fr(g)
  deg <- degree(g)
  node_sizes <- 3 + 6 * (deg - min(deg)) / (max(deg) - min(deg) + 0.01)
  par(mar = c(0, 0, 2, 0))
  plot(g,
       layout            = lay,
       vertex.size       = node_sizes,
       vertex.color      = node_col,
       vertex.label      = NA,
       vertex.frame.color = "white",
       edge.color        = adjustcolor("grey40", 0.6),
       edge.width        = 0.8,
       main              = title)
}

# -------------------------------------------------------
# Combine and save
# -------------------------------------------------------
png("fig5_networks.png", width = 900, height = 340, res = 120)
par(mfrow = c(1, 3))
plot_network(g_random, "Random\n(Erdős–Rényi)",       "#2c7bb6")
plot_network(g_sw,     "Small-world\n(Watts–Strogatz)", "#1a9641")
plot_network(g_sf,     "Scale-free\n(Barabási–Albert)", "#d7191c")
dev.off()
