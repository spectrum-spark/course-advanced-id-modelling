# ============================================================
# Generate fig3_commuter_schematic.png
#
# Schematic of the two-region commuter metapopulation model.
# Shows four subpopulations (Pop(i,j)) with orange leaving
# arrows (ℓ) and purple return arrows (r) between regions.
# White background boxes ensure ℓ labels are visible over
# the shaded region backgrounds.
#
# Written by Claude Sonnet 4.6 (Anthropic)
# ============================================================

library(ggplot2)

nodes <- data.frame(
  x     = c(1, 3, 1, 3),
  y     = c(3, 3, 1, 1),
  label = c("Pop(1,1)\nlocals\nat home",
            "Pop(1,2)\npatch-2 residents\nvisiting patch 1",
            "Pop(2,1)\npatch-1 residents\nvisiting patch 2",
            "Pop(2,2)\nlocals\nat home"),
  patch = c("1", "1", "2", "2")
)

border_df <- data.frame(x = c(0.3, 3.7), y = c(2, 2))

p <- ggplot() +

  # Region shading (drawn first, underneath everything)
  annotate("rect", xmin = 0.3, xmax = 3.7, ymin = 2.1, ymax = 3.7,
           fill = "#cce5ff", alpha = 0.4) +
  annotate("rect", xmin = 0.3, xmax = 3.7, ymin = 0.3, ymax = 1.9,
           fill = "#d4edda", alpha = 0.4) +

  # Border line
  geom_line(data = border_df, aes(x = x, y = y),
            linetype = "dashed", colour = "grey40", linewidth = 0.8) +
  annotate("text", x = 3.9, y = 2, label = "Border",
           colour = "grey40", hjust = 0, size = 3) +

  # Region labels
  annotate("text", x = 0.4, y = 3.6, label = "Region 1",
           colour = "#1a6faf", fontface = "bold", size = 3.5, hjust = 0) +
  annotate("text", x = 0.4, y = 0.4, label = "Region 2",
           colour = "#2e7d32", fontface = "bold", size = 3.5, hjust = 0) +

  # Node boxes
  geom_tile(data = nodes, aes(x = x, y = y, fill = patch),
            width = 1.2, height = 0.9, colour = "grey30", linewidth = 0.6) +
  scale_fill_manual(values = c("1" = "#aed6f1", "2" = "#a9dfbf"),
                    guide = "none") +
  geom_text(data = nodes, aes(x = x, y = y, label = label),
            size = 2.6, lineheight = 1.1) +

  # --- Left column ---
  # Orange: leaving arrow (downward, left side)
  annotate("segment", x = 0.82, xend = 0.82, y = 2.55, yend = 1.45,
           arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
           colour = "#e67e22", linewidth = 0.8) +
  # White box behind ℓ label so it sits on top of shading
  annotate("rect", xmin = 0.15, xmax = 0.68, ymin = 1.82, ymax = 2.18,
           fill = "white", alpha = 0.85) +
  annotate("text", x = 0.42, y = 2, label = "ℓ₂₁\n(leave)",
           colour = "#e67e22", size = 2.5, hjust = 0.5, lineheight = 0.9) +
  # Purple: return arrow (upward, right side of left column)
  annotate("segment", x = 1.0, xend = 1.0, y = 1.45, yend = 2.55,
           arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
           colour = "#8e44ad", linewidth = 0.8) +
  annotate("text", x = 1.55, y = 2, label = "r₂₁\n(return)",
           colour = "#8e44ad", size = 2.5, hjust = 0.5, lineheight = 0.9) +

  # --- Right column ---
  # Orange: leaving arrow (upward, left side of right column)
  annotate("segment", x = 2.82, xend = 2.82, y = 1.45, yend = 2.55,
           arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
           colour = "#e67e22", linewidth = 0.8) +
  # White box behind ℓ label
  annotate("rect", xmin = 2.15, xmax = 2.68, ymin = 1.82, ymax = 2.18,
           fill = "white", alpha = 0.85) +
  annotate("text", x = 2.42, y = 2, label = "ℓ₁₂\n(leave)",
           colour = "#e67e22", size = 2.5, hjust = 0.5, lineheight = 0.9) +
  # Purple: return arrow (downward, right side)
  annotate("segment", x = 3.0, xend = 3.0, y = 2.55, yend = 1.45,
           arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
           colour = "#8e44ad", linewidth = 0.8) +
  annotate("text", x = 3.58, y = 2, label = "r₁₂\n(return)",
           colour = "#8e44ad", size = 2.5, hjust = 0.5, lineheight = 0.9) +

  coord_fixed() +
  xlim(0.1, 4.5) + ylim(0.2, 3.8) +
  theme_void() +
  labs(title = expression(paste(
    S[ij], " = susceptibles who live in patch ", italic(j),
    " but are currently in patch ", italic(i))))

png("fig3_commuter_schematic.png", width = 760, height = 500, res = 120)
print(p)
dev.off()
