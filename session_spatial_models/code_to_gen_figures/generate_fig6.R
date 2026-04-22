# ============================================================
# Generate fig6_effective_distance.png
#
# Schematic illustrating that effective distance (based on
# mobility flow strength) differs from geographic distance.
# A nearby city with weak links can be epidemiologically
# "far", while a distant city with strong links is "close".
#
# Written by Claude Sonnet 4.6 (Anthropic)
# ============================================================

library(ggplot2)

# -------------------------------------------------------
# City locations and properties
# -------------------------------------------------------
cities <- data.frame(
  x    = c(0,   1,    5,    4.5),
  y    = c(0,   1.2,  0.5, -1),
  name = c("City A\n(outbreak source)",
           "City B\n(nearby)",
           "City C\n(distant hub)",
           "City D\n(distant, isolated)"),
  col  = c("#d7191c", "#fdae61", "#2c7bb6", "#abdda4")
)

# -------------------------------------------------------
# Edges: line width represents mobility flow strength
# -------------------------------------------------------
edges <- data.frame(
  x1  = c(0, 0, 0),
  y1  = c(0, 0, 0),
  x2  = c(1, 5, 4.5),
  y2  = c(1.2, 0.5, -1),
  lwd = c(1, 4, 0.5),
  lty = c("dashed", "solid", "dotted")
)

# -------------------------------------------------------
# Effective distance annotations
# -------------------------------------------------------
ann <- data.frame(
  x     = c(0.5,  2.5,  2.3),
  y     = c(0.9,  0.55, -0.55),
  label = c("geo: close\neff: FAR",
            "geo: distant\neff: CLOSE",
            "geo: distant\neff: very far"),
  col   = c("#e67e22", "#2c7bb6", "#27ae60")
)

p <- ggplot() +
  # Edges
  geom_segment(data = edges,
               aes(x = x1, y = y1, xend = x2, yend = y2,
                   linewidth = I(lwd), linetype = I(lty)),
               colour = "grey50") +
  # City nodes
  geom_point(data = cities, aes(x = x, y = y, colour = I(col)),
             size = 10) +
  # City labels
  geom_text(data = cities, aes(x = x, y = y - 0.32, label = name),
            size = 2.6, lineheight = 0.9) +
  # Effective distance annotation labels
  geom_label(data = ann, aes(x = x, y = y, label = label,
                              colour = I(col)),
             fill = "white", size = 2.5,
             label.padding = unit(0.15, "lines"),
             fontface = "bold", lineheight = 0.85) +
  # Legend note for line thickness
  annotate("text", x = 0, y = -1.5,
           label = "Line thickness = mobility flow strength",
           size = 2.8, colour = "grey40", hjust = 0) +
  xlim(-0.8, 6.5) +
  ylim(-2, 2) +
  theme_void() +
  labs(title    = "Effective distance ≠ geographic distance",
       subtitle = paste0("A city can be epidemiologically 'close' due to ",
                         "strong mobility links, regardless of geography"))

png("fig6_effective_distance.png", width = 820, height = 420, res = 120)
print(p)
dev.off()
