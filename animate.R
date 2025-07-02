# Load necessary libraries
library(dplyr)
library(tidyr)
library(igraph)
library(ggplot2)
library(gganimate)
library(stringr)
library(lubridate) # For floor_date and ceiling_date

# --- 1. Global Parameters Definition ---
num_transactions <- 1500 # Total number of events (payments, injections, withdrawals) to generate
start_date <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
end_date <- as.POSIXct("2024-01-07 23:59:59", tz = "UTC")
num_agents <- 15 # Number of agents in the network
agents <- paste0("Agent", LETTERS[1:num_agents])
interval_time_voulue <- "4 hours" # Time interval for the animation (e.g., "4 hours", "6 hours", "1 day")

set.seed(123) # For reproducibility of random results

# Define "Hub" agents and their probabilities
hub_agents <- c("AgentA", "AgentB", "AgentC") # Example central agents (e.g., large companies)
prob_hub_receive <- 0.6 # Probability that a beneficiary is a hub agent (if the payer is not a hub)
prob_hub_pay_low <- 0.1 # Probability that a hub pays a low amount (simulates regular small expenses)
prob_regular_pay_hub <- 0.3 # Probability that a regular agent pays a hub (simulates using central services)

# Parameters for external flows (injections and withdrawals from the system)
prob_external_event_overall <- 0.10 # Probability that a "transaction" is an external flow (vs. an internal payment)

# Coefficients for external flow amounts (proportion of agent's balance)
# These coefficients can vary depending on whether the agent is "small" or "large"
coeff_montant_injection_petit_max <- 0.8 # Small agents can receive up to 80% of their balance as injection
coeff_montant_injection_grand_max <- 0.2 # Large agents can receive up to 20% of their balance as injection
coeff_montant_retrait_petit_max <- 0.2 # Small agents can lose up to 20% of their balance as withdrawal
coeff_montant_retrait_grand_max <- 0.8 # Large agents can lose up to 80% of their balance as withdrawal

min_flux_amount <- 50 # Minimum amount for any injection/withdrawal

# --- 2. Initial Stocks Generation ---
initial_stocks_df <- data.frame(
  Agent = agents,
  StockInitial = round(runif(num_agents, 500, 2000), 0) # Random stocks between 500 and 2000
)

# Create a vector to track current balances during transaction generation
current_balances <- initial_stocks_df$StockInitial
names(current_balances) <- initial_stocks_df$Agent

# --- 3. Event Generation (Classic Transactions, Injections, Withdrawals) ---
transactions_list <- list()
k_transactions_count <- 0 # Counter for successful events
max_overall_attempts <- num_transactions * 5 # Attempt limit to prevent infinite loop

while (k_transactions_count < num_transactions && max_overall_attempts > 0) {
  # Decide if the event is an external flow or a classic payment
  if (runif(1) < prob_external_event_overall) {
    # --- Attempt an External Flow (Injection or Withdrawal) ---
    
    # Calculate the median balance to determine "small" or "large" agent
    median_balance <- median(current_balances)
    # **FIX:** Ensure median_balance is never effectively zero for division
    if (median_balance == 0) {
      median_balance <- 1 # Use a small positive number as fallback
    }
    
    # Calculate selection probabilities for each agent for an INJECTION
    # Small agents (solde <= median) have a higher probability of receiving a gain
    # **FIX:** Add a small constant to ensure non-zero base probability and use pmax(0, ...)
    prob_target_for_injection_raw <- ifelse(current_balances <= median_balance, 
                                            (median_balance - current_balances) / median_balance + 0.1, # Add a base probability
                                            0.01) # Very small chance for large agents
    
    prob_target_for_injection <- pmax(0, prob_target_for_injection_raw) # Ensure non-negative
    
    # **FIX:** Normalize probabilities, handle case where sum is 0
    if (sum(prob_target_for_injection) == 0) {
      prob_target_for_injection <- rep(1/length(agents), length(agents)) # Equal probability fallback
    } else {
      prob_target_for_injection <- prob_target_for_injection / sum(prob_target_for_injection) 
    }
    
    # Calculate selection probabilities for each agent for a RETRAIT (Withdrawal)
    # Large agents (solde > median) have a higher probability of incurring a loss
    # **FIX:** Add a small constant to ensure non-zero base probability and use pmax(0, ...)
    prob_target_for_retrait_raw <- ifelse(current_balances > median_balance, 
                                          (current_balances - median_balance) / median_balance + 0.1, # Add a base probability
                                          0.01) # Very small chance for small agents
    
    prob_target_for_retrait <- pmax(0, prob_target_for_retrait_raw) # Ensure non-negative
    
    # **FIX:** Normalize probabilities, handle case where sum is 0
    if (sum(prob_target_for_retrait) == 0) {
      prob_target_for_retrait <- rep(1/length(agents), length(agents)) # Equal probability fallback
    } else {
      prob_target_for_retrait <- prob_target_for_retrait / sum(prob_target_for_retrait)
    }
    
    # Decide whether it's an INJECTION or a RETRAIT, then choose the agent accordingly
    if (runif(1) < 0.7) { # 50% chance to attempt an injection
      target_agent <- sample(agents, 1, prob = prob_target_for_injection) # Select an agent based on injection probability
      
      # Montant de l'injection depends on agent size
      max_possible_amount <- ifelse(current_balances[target_agent] <= median_balance,
                                    current_balances[target_agent] * coeff_montant_injection_petit_max,
                                    current_balances[target_agent] * coeff_montant_injection_grand_max)
      
      # **FIX:** Ensure flux_amount calculation is robust for very small or zero max_possible_amount
      flux_amount <- round(runif(1, min_flux_amount, max(min_flux_amount, max_possible_amount)), 0)
      if (max_possible_amount <= 0) { # If target agent has very low/zero balance, prevent injection.
        flux_amount <- 0
      } else if (flux_amount == 0 && max_possible_amount > 0) { # Ensure a minimum amount if possible
        flux_amount <- min_flux_amount # Fallback to min_flux_amount if random resulted in 0 but could be higher
      }
      
      if (flux_amount > 0) {
        current_balances[target_agent] <- current_balances[target_agent] + flux_amount
        
        k_transactions_count <- k_transactions_count + 1
        transactions_list[[k_transactions_count]] <- data.frame(
          Payeur = "SourceExterne", # Fictitious money source
          Bénéficiaire = target_agent,
          Montant = flux_amount,
          timestamp = as.POSIXct(runif(1, as.numeric(start_date), as.numeric(end_date)), origin = "1970-01-01", tz = "UTC"),
          Type = "Injection"
        )
      }
    } else { # 50% chance to attempt a withdrawal
      target_agent <- sample(agents, 1, prob = prob_target_for_retrait) # Select an agent based on withdrawal probability
      
      # Montant du retrait depends on agent size
      max_possible_amount <- ifelse(current_balances[target_agent] > median_balance,
                                    current_balances[target_agent] * coeff_montant_retrait_grand_max,
                                    current_balances[target_agent] * coeff_montant_retrait_petit_max)
      
      # **FIX:** Ensure flux_amount calculation is robust and doesn't exceed current balance
      flux_amount <- round(runif(1, min_flux_amount, max(min_flux_amount, max_possible_amount)), 0)
      
      # Cap withdrawal at current balance
      if (flux_amount > current_balances[target_agent]) {
        flux_amount <- current_balances[target_agent]
      }
      
      if (current_balances[target_agent] >= flux_amount && flux_amount > 0) {
        current_balances[target_agent] <- current_balances[target_agent] - flux_amount
        
        k_transactions_count <- k_transactions_count + 1
        transactions_list[[k_transactions_count]] <- data.frame(
          Payeur = target_agent,
          Bénéficiaire = "PuitsExterne", # Fictitious money sink
          Montant = flux_amount,
          timestamp = as.POSIXct(runif(1, as.numeric(start_date), as.numeric(end_date)), origin = "1970-01-01", tz = "UTC"),
          Type = "Retrait"
        )
      } 
    }
  } else {
    # --- CLASSIC PAYMENT between agents ---
    payeur <- sample(agents, 1)
    beneficiaire <- sample(agents, 1)
    
    if (payeur == beneficiaire) {
      max_overall_attempts <- max_overall_attempts - 1
      next # Skip to the next iteration if payer and beneficiary are the same
    }
    
    montant_propose <- round(runif(1, 10, 200), 0)
    
    # Logic to favor "hub" agents as beneficiaries and adjust amounts
    if (!(payeur %in% hub_agents) && runif(1) < prob_regular_pay_hub) {
      beneficiaire <- sample(hub_agents, 1)
    } else if (payeur %in% hub_agents && runif(1) < prob_hub_pay_low) {
      montant_propose <- round(runif(1, 5, 50), 0)
    }
    
    # BALANCE CHECK: Payer must have enough money
    if (current_balances[payeur] >= montant_propose) {
      montant_final <- montant_propose
      current_balances[payeur] <- current_balances[payeur] - montant_final
      current_balances[beneficiaire] <- current_balances[beneficiaire] + montant_final
      
      k_transactions_count <- k_transactions_count + 1
      transactions_list[[k_transactions_count]] <- data.frame(
        Payeur = payeur,
        Bénéficiaire = beneficiaire,
        Montant = montant_final,
        timestamp = as.POSIXct(runif(1, as.numeric(start_date), as.numeric(end_date)), origin = "1970-01-01", tz = "UTC"),
        Type = "Paiement"
      )
    } else if (current_balances[payeur] > 0) {
      # If not enough for the proposed amount, pay the maximum possible
      montant_final <- current_balances[payeur]
      current_balances[payeur] <- 0
      current_balances[beneficiaire] <- current_balances[beneficiaire] + montant_final
      
      k_transactions_count <- k_transactions_count + 1
      transactions_list[[k_transactions_count]] <- data.frame(
        Payeur = payeur,
        Bénéficiaire = beneficiaire,
        Montant = montant_final,
        timestamp = as.POSIXct(runif(1, as.numeric(start_date), as.numeric(end_date)), origin = "1970-01-01", tz = "UTC"),
        Type = "Paiement"
      )
    }
  }
  max_overall_attempts <- max_overall_attempts - 1
}

if (k_transactions_count < num_transactions) {
  warning(paste("Only", k_transactions_count, "events (payments, injections, withdrawals) could be generated."))
}

# Convert the list of transactions to a dataframe and sort by timestamp
transactions_df <- bind_rows(transactions_list) %>%
  arrange(timestamp) 

# Add fictitious "agents" (SourceExterne, PuitsExterne) for graph visualization
all_display_agents <- unique(c(agents, "SourceExterne", "PuitsExterne"))
# Ensure initial_stocks_display_df also accounts for non-existent initial stocks for external agents
initial_stocks_display_df <- data.frame(
  Agent = all_display_agents,
  StockInitial = ifelse(all_display_agents %in% agents, 
                        initial_stocks_df$StockInitial[match(all_display_agents, initial_stocks_df$Agent)], 
                        0) # External agents start with 0 visualized stock
)

print(paste("Total number of events generated:", nrow(transactions_df)))
print("Initial stocks preview:")
print(head(initial_stocks_df))
print("Transactions preview:")
print(head(transactions_df))

# --- Data Preparation for Animation ---

# Create a sequence of time intervals
time_intervals <- seq(from = floor_date(min(transactions_df$timestamp), unit = interval_time_voulue),
                      to = ceiling_date(max(transactions_df$timestamp), unit = interval_time_voulue),
                      by = interval_time_voulue)

# Prepare dataframes to store nodes and edges for each animation frame
all_nodes_data_for_animation <- data.frame()
# **FIX:** Initialize all_edges_data_for_animation with proper column types
all_edges_data_for_animation <- data.frame(
  from = character(),
  to = character(),
  weight = numeric(),
  time_interval = as.POSIXct(character(), tz = "UTC"),
  Type = character() # To differentiate transaction types
)

# Iterate over each time interval to calculate cumulative and instantaneous states
for (i in 1:(length(time_intervals) - 1)) {
  interval_start_time <- time_intervals[i]
  interval_end_time <- time_intervals[i+1] 
  
  # Calculate cumulative stocks for NODES up to the end of the current interval
  # Stocks are recalculated from the beginning for EACH interval to ensure consistency
  temp_cumulative_stocks_for_frame <- initial_stocks_df$StockInitial
  names(temp_cumulative_stocks_for_frame) <- initial_stocks_df$Agent
  
  transactions_up_to_current_interval_end_for_calc <- transactions_df %>%
    filter(timestamp < interval_end_time) # All past transactions and those within the current interval
  
  if (nrow(transactions_up_to_current_interval_end_for_calc) > 0) {
    for (j in 1:nrow(transactions_up_to_current_interval_end_for_calc)) {
      payeur <- transactions_up_to_current_interval_end_for_calc$Payeur[j]
      beneficiaire <- transactions_up_to_current_interval_end_for_calc$Bénéficiaire[j]
      montant <- transactions_up_to_current_interval_end_for_calc$Montant[j]
      type_trans <- transactions_up_to_current_interval_end_for_calc$Type[j]
      
      # Apply changes to temp_cumulative_stocks_for_frame only for 'actual' agents
      # External sources/sinks do not have fluctuating balances within the network visualization context
      if (payeur %in% agents) {
        temp_cumulative_stocks_for_frame[payeur] <- temp_cumulative_stocks_for_frame[payeur] - montant
      }
      if (beneficiaire %in% agents) {
        temp_cumulative_stocks_for_frame[beneficiaire] <- temp_cumulative_stocks_for_frame[beneficiaire] + montant
      }
    }
  }
  
  # Prepare node data for this frame (only classic agents are visualized nodes)
  nodes_for_frame <- data.frame(
    Agent = names(temp_cumulative_stocks_for_frame),
    Stock = as.numeric(temp_cumulative_stocks_for_frame),
    time_interval = interval_start_time 
  ) %>%
    filter(!(Agent %in% c("SourceExterne", "PuitsExterne"))) %>% # Filter out fictitious agents from node display
    mutate(Size = pmax(1, Stock / max(initial_stocks_df$StockInitial) * 20)) # Normalize size for visualization
  
  all_nodes_data_for_animation <- bind_rows(all_nodes_data_for_animation, nodes_for_frame)
  
  # Calculate payments for EDGES within the CURRENT interval
  transactions_in_current_interval <- transactions_df %>%
    filter(timestamp >= interval_start_time & timestamp < interval_end_time)
  
  edges_for_frame <- transactions_in_current_interval %>%
    group_by(from = Payeur, to = Bénéficiaire, Type) %>% 
    summarise(weight = sum(Montant), .groups = 'drop') %>%
    mutate(time_interval = interval_start_time) %>%
    filter(weight > 0) 
  
  all_edges_data_for_animation <- bind_rows(all_edges_data_for_animation, edges_for_frame)
}

# Ensure all agents (including fictitious ones) have a fixed position for visualization
initial_graph <- graph_from_data_frame(d = transactions_df, vertices = initial_stocks_display_df, directed = TRUE)
set.seed(42) # For reproducible graph layout
layout_coords <- layout_with_fr(initial_graph)

layout_df <- as.data.frame(layout_coords)
colnames(layout_df) <- c("x", "y")
layout_df$Agent <- V(initial_graph)$name

# Join coordinates to node and edge data
all_nodes_data_for_animation <- all_nodes_data_for_animation %>%
  left_join(layout_df %>% filter(Agent %in% agents) %>% select(Agent, x, y), by = "Agent") %>%
  group_by(Agent) %>% # To handle NAs if an agent doesn't appear at the very beginning
  fill(x, y, .direction = "downup") %>%
  ungroup()

all_edges_data_for_animation <- all_edges_data_for_animation %>%
  left_join(layout_df %>% select(Agent, x_from = x, y_from = y), by = c("from" = "Agent")) %>%
  left_join(layout_df %>% select(Agent, x_to = x, y_to = y), by = c("to" = "Agent"))

# --- Animation Creation with gganimate ---

p <- ggplot() +
  # Layer for edges (arrows representing money flows)
  geom_segment(data = all_edges_data_for_animation,
               aes(x = x_from, y = y_from, xend = x_to, yend = y_to,
                   alpha = weight, linewidth = weight, color = Type), 
               arrow = arrow(length = unit(4, 'mm'), type = "closed"),
               lineend = "round",
               show.legend = TRUE) + 
  # Layer for nodes (points representing agents)
  geom_point(data = all_nodes_data_for_animation,
             aes(x = x, y = y, size = Size, fill = Stock),
             shape = 21, # Circle with border and fill
             color = "black", # Border color
             show.legend = TRUE) +
  # Layer for node labels (agent names)
  geom_text(data = all_nodes_data_for_animation,
            aes(x = x, y = y, label = Agent),
            vjust = -1.5, # Vertical positioning of the label
            size = 3,
            color = "black") +
  # Scales for visual attributes
  scale_size_continuous(range = c(5, 25), name = "Node Size") +
  scale_linewidth_continuous(range = c(0.5, 5), name = "Payment Amount") +
  scale_fill_gradient(low = "red", high = "green", name = "Stock (€)") + # Node fill color based on stock
  scale_color_manual(values = c("Paiement" = "darkblue", "Injection" = "forestgreen", "Retrait" = "darkred"), name = "Flow Type") + # Arrow colors by flow type
  
  # Visual theme and titles
  theme_void() + # Minimalist theme without axes or background
  labs(
    title = 'Financial Movements between Agents (with Adaptive External Flows)',
    subtitle = 'Time: {frame_time}' # Dynamic title showing the current time interval
  ) +
  
  # Gganimate configuration
  transition_states(time_interval, transition_length = 1, state_length = 1) + # Defines the transition variable (time interval)
  ease_aes('linear') + # Smooth transition type between frames
  enter_fade() + exit_fade() + # Gradual appearance/disappearance of elements
  shadow_wake(wake_length = 0.1, alpha = FALSE, falloff = "linear") # Leaves a "trail" of previous elements

# To generate and save the animation (uncomment the lines below to run)
# The generation process may take time depending on the number of frames and complexity.
# num_intervals <- length(unique(all_nodes_data_for_animation$time_interval))
animate(p, fps = 10, nframes = num_intervals * 4,
         width = 800, height = 600, renderer = gifski_renderer())
 anim_save("mouvements_financiers_animation_adaptive_flux_final.gif", animation = last_animation())
 