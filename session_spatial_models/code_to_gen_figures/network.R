library(igraph)
library(ggplot2)

# -------------------------------------------------------
# Stochastic SIR on a network
# -------------------------------------------------------
sim_network_sir <- function(g, beta, gamma, seed_node = 1, tmax = 80) {
  n     <- vcount(g)
  state <- rep("S", n)
  state[seed_node] <- "I"
  
  times <- 0
  S_ts  <- sum(state == "S")
  I_ts  <- sum(state == "I")
  R_ts  <- sum(state == "R")
  
  t <- 0
  while (sum(state == "I") > 0 && t < tmax) {
    t         <- t + 1
    new_state <- state
    
    for (v in which(state == "I")) {
      # Each infected node recovers with probability gamma
      if (runif(1) < gamma) {
        new_state[v] <- "R"
        next
      }
      # Each infected node tries to infect susceptible neighbours
      nbrs <- as.integer(neighbors(g, v))
      for (u in nbrs) {
        if (state[u] == "S" && runif(1) < beta) {
          new_state[u] <- "I"
        }
      }
    }
    
    state <- new_state
    times <- c(times, t)
    S_ts  <- c(S_ts, sum(state == "S"))
    I_ts  <- c(I_ts, sum(state == "I"))
    R_ts  <- c(R_ts, sum(state == "R"))
  }
  
  data.frame(time = times, S = S_ts, I = I_ts, R = R_ts)
}

# -------------------------------------------------------
# Build networks
# -------------------------------------------------------
set.seed(42)
n_nodes <- 300

# Random (Erdős–Rényi): each pair connected with probability p
g_random <- sample_gnp(n_nodes, p = 0.033)

# Scale-free (Barabási–Albert): preferential attachment, m new edges per node
g_scalefree <- sample_pa(n_nodes, m = 5, directed = FALSE)

cat("Random network    - mean degree:", round(mean(degree(g_random)), 1),
    " max degree:", max(degree(g_random)), "\n")

cat("Scale-free network - mean degree:", round(mean(degree(g_scalefree)), 1),
    " max degree:", max(degree(g_scalefree)), "\n")

# -------------------------------------------------------
# Run epidemics
# -------------------------------------------------------
beta  <- 0.15
gamma <- 0.10

res_random    <- sim_network_sir(g_random,    beta, gamma, seed_node = 1)
res_scalefree <- sim_network_sir(g_scalefree, beta, gamma, seed_node = 1)

# -------------------------------------------------------
# Plot epidemic curves
# -------------------------------------------------------
plot_df <- rbind(
  data.frame(res_random,    network = "Random"),
  data.frame(res_scalefree, network = "Scale-free")
)

ggplot(plot_df, aes(x = time, y = I, colour = network)) +
  geom_line(linewidth = 1) +
  labs(x = "Time step", y = "Number infected",
       colour = "Network type",
       title = "Stochastic SIR on two network types") +
  theme_bw()

# -------------------------------------------------------
# Degree distributions
# -------------------------------------------------------
deg_df <- rbind(
  data.frame(degree = degree(g_random),    network = "Random"),
  data.frame(degree = degree(g_scalefree), network = "Scale-free")
)

ggplot(deg_df, aes(x = degree, fill = network)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
  facet_wrap(~network, scales = "free") +
  labs(x = "Degree (number of connections)", y = "Count",
       title = "Degree distributions") +
  theme_bw() +
  theme(legend.position = "none")

