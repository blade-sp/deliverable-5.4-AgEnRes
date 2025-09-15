# SVJ Model - Energy Price Path Simulation
# Focus on price paths only
library(ggplot2)
library(gridExtra)

# Simplified SVJ Price Simulation
simulate_svj_prices <- function(S0 = 100,              # Initial price
                                v0 = 0.04,            # Initial variance
                                r = 0.02,             # Drift rate
                                kappa = 2.5,          # Vol mean reversion
                                theta = 0.04,         # Long-term variance
                                sigma_v = 0.3,        # Vol of vol
                                rho = -0.6,           # Price-vol correlation
                                # Jump parameters - Normal
                                lambda_normal = 2,     # Normal jump intensity
                                mu_s_normal = -0.02,  # Normal price jump mean
                                sigma_s_normal = 0.08, # Normal price jump std
                                mu_v_normal = 0.005,  # Normal vol jump mean
                                sigma_v_normal = 0.02, # Normal vol jump std
                                rho_j_normal = 0.3,   # Normal jump correlation
                                # Jump parameters - Crisis
                                lambda_crisis = 12,    # Crisis jump intensity
                                mu_s_crisis = 0.15,   # Crisis price jump mean
                                sigma_s_crisis = 0.25, # Crisis price jump std
                                mu_v_crisis = 0.08,   # Crisis vol jump mean
                                sigma_v_crisis = 0.15, # Crisis vol jump std
                                rho_j_crisis = 0.7,   # Crisis jump correlation
                                # Crisis regime
                                crisis_prob = 0.03,    # Enter crisis probability
                                crisis_exit_prob = 0.1, # Exit crisis probability
                                # Mean reversion
                                alpha = 0.6,           # Price mean reversion
                                mu_lt = log(60),       # Long-term price level
                                T = 20,                 # Time horizon
                                n = 100,               # Time steps
                                n_paths = 100) {       # Number of paths
  
  dt <- T / n
  sqrt_dt <- sqrt(dt)
  
  # Initialize price matrix only
  S <- matrix(0, nrow = n_paths, ncol = n + 1)
  v <- matrix(0, nrow = n_paths, ncol = n + 1)
  crisis_state <- matrix(FALSE, nrow = n_paths, ncol = n + 1)
  
  # Set initial values
  S[, 1] <- S0
  v[, 1] <- v0
  crisis_state[, 1] <- FALSE
  
  for (i in 1:n) {
    # Random numbers
    Z1 <- rnorm(n_paths)
    Z2 <- rnorm(n_paths)
    Z3 <- rnorm(n_paths)
    Z4 <- rnorm(n_paths)
    U_jump <- runif(n_paths)
    U_crisis <- runif(n_paths)
    
    # Correlated Brownian motions
    W_s <- Z1
    W_v <- rho * Z1 + sqrt(1 - rho^2) * Z2
    
    # Crisis state transitions
    enter_crisis <- (!crisis_state[, i]) & (U_crisis < crisis_prob)
    exit_crisis <- crisis_state[, i] & (U_crisis < crisis_exit_prob)
    crisis_state[, i + 1] <- crisis_state[, i]
    crisis_state[enter_crisis, i + 1] <- TRUE
    crisis_state[exit_crisis, i + 1] <- FALSE
    
    # Regime-dependent parameters
    current_lambda <- ifelse(crisis_state[, i], lambda_crisis, lambda_normal)
    current_mu_s <- ifelse(crisis_state[, i], mu_s_crisis, mu_s_normal)
    current_sigma_s <- ifelse(crisis_state[, i], sigma_s_crisis, sigma_s_normal)
    current_mu_v <- ifelse(crisis_state[, i], mu_v_crisis, mu_v_normal)
    current_sigma_v <- ifelse(crisis_state[, i], sigma_v_crisis, sigma_v_normal)
    current_rho_j <- ifelse(crisis_state[, i], rho_j_crisis, rho_j_normal)
    
    # Jump occurrence
    jump_occurs <- U_jump < current_lambda * dt
    
    # Generate correlated jumps
    J_s <- rep(0, n_paths)
    J_v <- rep(0, n_paths)
    
    if (any(jump_occurs)) {
      Z_s_jump <- Z3[jump_occurs]
      Z_v_jump <- current_rho_j[jump_occurs] * Z3[jump_occurs] + 
        sqrt(1 - current_rho_j[jump_occurs]^2) * Z4[jump_occurs]
      
      J_s[jump_occurs] <- current_mu_s[jump_occurs] + current_sigma_s[jump_occurs] * Z_s_jump
      J_v[jump_occurs] <- current_mu_v[jump_occurs] + current_sigma_v[jump_occurs] * Z_v_jump
    }
    
    # Update variance
    v_new <- v[, i] + kappa * (theta - pmax(v[, i], 0)) * dt + 
      sigma_v * sqrt(pmax(v[, i], 0)) * sqrt_dt * W_v + J_v
    v[, i + 1] <- pmax(v_new, 0.001)
    
    # Update price with mean reversion
    log_S <- log(S[, i])
    drift <- (r - 0.5 * v[, i] + alpha * (mu_lt - log_S)) * dt
    diffusion <- sqrt(v[, i]) * sqrt_dt * W_s
    
    S[, i + 1] <- S[, i] * exp(drift + diffusion + J_s)
    S[, i + 1] <- pmax(pmin(S[, i + 1], 1000), 0.1)  # Bounds
  }
  
  time <- seq(0, T, length.out = n + 1)
  return(list(prices = S, time = time, crisis_state = crisis_state))
}

# Run simulation
set.seed(42)
results <- simulate_svj_prices(n_paths = 50)  # Fewer paths for cleaner visualization

# Prepare data
price_paths <- results$prices
time_vec <- results$time
n_paths <- nrow(price_paths)

# Create long format data for ggplot
price_data <- data.frame(
  Time = rep(time_vec, n_paths),
  Price = as.vector(t(price_paths)),
  Path = rep(1:n_paths, each = length(time_vec))
)

# Add crisis information for first few paths
crisis_data <- data.frame(
  Time = time_vec,
  Price = price_paths[1, ],
  Crisis = results$crisis_state[1, ]
)

# Main price paths plot
p1 <- ggplot(price_data, aes(x = Time, y = Price, group = Path)) +
  geom_line(alpha = 0.4, color = "steelblue", size = 0.6) +
  # Highlight a few specific paths
  geom_line(data = price_data[price_data$Path == 1, ], 
            aes(x = Time, y = Price), color = "red", size = 1.2) +
  geom_line(data = price_data[price_data$Path == 2, ], 
            aes(x = Time, y = Price), color = "darkgreen", size = 1) +
  geom_line(data = price_data[price_data$Path == 3, ], 
            aes(x = Time, y = Price), color = "orange", size = 1) +
  # Add crisis shading for first path
  geom_ribbon(data = crisis_data, 
              aes(x = Time, ymin = 0, ymax = ifelse(Crisis, max(price_data$Price), 0),
                  group = NULL), 
              fill = "red", alpha = 0.1) +
  labs(title = "SVJ Model: Energy Price Paths",
       subtitle = paste("Simulated", n_paths, "price paths with stochastic volatility and jumps"),
       x = "Time (Years)",
       y = "Energy Price ($/unit)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

# Individual path comparison
sample_paths <- c(1, 5, 10, 15)
sample_data <- price_data[price_data$Path %in% sample_paths, ]
sample_data$PathLabel <- paste("Path", sample_data$Path)

p2 <- ggplot(sample_data, aes(x = Time, y = Price, color = PathLabel)) +
  geom_line(size = 1.2, alpha = 0.8) +
  scale_color_manual(values = c("red", "blue", "darkgreen", "purple")) +
  labs(title = "Individual Price Path Examples",
       subtitle = "Different realizations of the same SVJ process",
       x = "Time (Years)",
       y = "Energy Price ($/unit)",
       color = "Path") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 11),
        legend.position = "right")

# Price distribution over time (violin plot)
time_points <- seq(1, length(time_vec), by = 50)  # Sample time points
sample_times <- time_vec[time_points]
violin_data <- data.frame()

for(i in 1:length(time_points)) {
  temp_data <- data.frame(
    Time = sample_times[i],
    Price = price_paths[, time_points[i]],
    TimeLabel = paste("t =", round(sample_times[i], 2))
  )
  violin_data <- rbind(violin_data, temp_data)
}

p3 <- ggplot(violin_data, aes(x = TimeLabel, y = Price)) +
  geom_violin(fill = "lightblue", alpha = 0.7, color = "steelblue") +
  geom_boxplot(width = 0.2, fill = "white", alpha = 0.8) +
  labs(title = "Price Distribution Evolution",
       subtitle = "How price distribution changes over time",
       x = "Time Points",
       y = "Energy Price ($/unit)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 11),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Final price histogram
final_prices <- price_paths[, ncol(price_paths)]

p4 <- ggplot(data.frame(FinalPrice = final_prices), aes(x = FinalPrice)) +
  geom_histogram(bins = 20, fill = "lightgreen", color = "darkgreen", alpha = 0.7) +
  geom_vline(xintercept = mean(final_prices), color = "red", 
             linetype = "dashed", size = 1.2) +
  geom_vline(xintercept = median(final_prices), color = "blue", 
             linetype = "dashed", size = 1.2) +
  labs(title = "Final Price Distribution",
       subtitle = paste("Mean:", round(mean(final_prices), 1), 
                        "| Median:", round(median(final_prices), 1)),
       x = "Final Energy Price ($/unit)",
       y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 11))

# Combine plots
grid.arrange(p1, p2, p3, p4, ncol = 2)

# Print summary statistics
cat("SVJ Energy Price Path Simulation Summary\n")
cat("========================================\n")
cat("Simulation Parameters:\n")
cat("- Number of paths:", nrow(price_paths), "\n")
cat("- Time horizon:", max(time_vec), "years\n")
cat("- Time steps:", length(time_vec) - 1, "\n")
cat("- Initial price: $", price_paths[1, 1], "\n\n")

cat("Final Price Statistics:\n")
cat("- Mean: $", round(mean(final_prices), 2), "\n")
cat("- Median: $", round(median(final_prices), 2), "\n")
cat("- Std Dev: $", round(sd(final_prices), 2), "\n")
cat("- Min: $", round(min(final_prices), 2), "\n")
cat("- Max: $", round(max(final_prices), 2), "\n")
cat("- 95% Range: $", round(quantile(final_prices, 0.025), 2), 
    " - $", round(quantile(final_prices, 0.975), 2), "\n\n")

# Path statistics
returns <- diff(log(price_paths), lag = 1)
daily_vol <- apply(returns, 1, sd) * sqrt(252)  # Annualized volatility per path

cat("Path Characteristics:\n")
cat("- Average annual volatility:", round(mean(daily_vol), 3), "\n")
cat("- Volatility range:", round(min(daily_vol), 3), "-", round(max(daily_vol), 3), "\n")

# Crisis time analysis
crisis_time_pct <- rowMeans(results$crisis_state) * 100
cat("- Average time in crisis:", round(mean(crisis_time_pct), 1), "%\n")
cat("- Crisis time range:", round(min(crisis_time_pct), 1), "% -", 
    round(max(crisis_time_pct), 1), "%\n")

cat("\nModel Features:\n")
cat("✓ Stochastic volatility with mean reversion\n")
cat("✓ Correlated jumps in price and volatility\n")
cat("✓ Crisis regime switching\n")
cat("✓ Energy price mean reversion to fundamentals\n")
cat("✓ Realistic fat-tailed distributions\n")