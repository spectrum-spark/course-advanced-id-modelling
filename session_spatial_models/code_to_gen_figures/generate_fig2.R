# ============================================================
# Generate fig2_diffusion_schematic.png
#
# Schematic of a 1D discretised reaction-diffusion SIR model.
# Each cell runs local SIR dynamics (beta, gamma) and exchanges
# individuals with its immediate neighbours via diffusion (D).
# A distance axis at the bottom ties the figure to fig1.
#
# Written by Claude Sonnet 4.6 (Anthropic)
# ============================================================

library(ggplot2)

n_cells <- 7
cells <- data.frame(
  x         = 1:n_cells,
  y         = 0,
  label     = c("...", "S,I,R", "S,I,R", "S,I,R", "S,I,R", "S,I,R", "..."),
  highlight = c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)
)

# Diffusion arrows between all adjacent cell pairs
arrows_fwd  <- data.frame(x = c(2, 3, 4, 5, 6), xend = c(3, 4, 5, 6, 7))
arrows_back <- data.frame(x = c(3, 4, 5, 6, 7), xend = c(2, 3, 4, 5, 6))

p <- ggplot() +

  # Cell boxes
  geom_tile(data = cells, aes(x = x, y = 0, fill = highlight),
            width = 0.82, height = 0.7,
            colour = "grey40", linewidth = 0.5) +
  scale_fill_manual(values = c("FALSE" = "#cce5ff", "TRUE" = "#ffcc99"),
                    guide = "none") +
  geom_text(data = cells, aes(x = x, y = 0, label = label), size = 3.2) +

  # Forward diffusion arrows (above cells)
  geom_segment(data = arrows_fwd,
               aes(x = x + 0.43, xend = xend - 0.43, y = 0.52, yend = 0.52),
               arrow = arrow(length = unit(0.18, "cm"), type = "closed"),
               colour = "#2c7bb6", linewidth = 0.7) +

  # Return diffusion arrows (below cells)
  geom_segment(data = arrows_back,
               aes(x = x - 0.43, xend = xend + 0.43, y = -0.52, yend = -0.52),
               arrow = arrow(length = unit(0.18, "cm"), type = "closed"),
               colour = "#2c7bb6", linewidth = 0.7) +

  # D label below return arrows
  annotate("text", x = 4, y = -0.78,
           label = "D (diffusion between neighbours)",
           colour = "#2c7bb6", size = 3.0) +

  # Local beta/gamma arc on centre cell
  annotate("curve", x = 3.58, xend = 4.42, y = 0.35, yend = 0.35,
           curvature = -0.5,
           arrow = arrow(length = unit(0.18, "cm"), type = "closed"),
           colour = "#d7191c", linewidth = 0.7) +
  annotate("text", x = 4, y = 0.72,
           label = "β, γ (local SIR dynamics)",
           colour = "#d7191c", size = 3.0) +

  # Distance axis arrow at bottom
  annotate("segment",
           x = 1, xend = 7, y = -1.1, yend = -1.1,
           arrow = arrow(length = unit(0.25, "cm"), type = "closed",
                         ends = "both"),
           colour = "grey30", linewidth = 0.8) +
  annotate("text", x = 4, y = -1.32,
           label = "Distance (km)",
           colour = "grey30", size = 3.2) +

  xlim(0.3, 7.7) +
  ylim(-1.55, 1.1) +
  coord_fixed(ratio = 0.85) +
  theme_void() +
  labs(title = "Each spatial cell has local SIR dynamics (β, γ)\nand exchanges individuals with neighbours via diffusion (D)")

png("fig2_diffusion_schematic.png", width = 820, height = 320, res = 120)
print(p)
dev.off()
