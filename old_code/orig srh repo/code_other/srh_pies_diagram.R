# pie chart changing over time

# Load necessary libraries
library(tidyverse)
library(patchwork)

# Define the three colors to use
colors <- c("#0072B2", "#EF6C63", "#009E73") # Blue, Peach, Green (color-blind friendly)

# Create a base dataset for each pie chart
# Each list element represents one pie with 3 categories smoothly transitioning
pie_data <- tibble(
  pie_id = rep(1:5, each = 3),
  category = rep(c("A", "B", "C"), times = 5),
  proportion = c(
    0.7, 0.2, 0.1,    # Pie 1
    0.55, 0.3, 0.15,   # Pie 2
    0.4, 0.35, 0.25,   # Pie 3
    0.25, 0.4, 0.35,   # Pie 4
    0.1, 0.2, 0.7      # Pie 5
  )
)

# Create a function to plot a single pie
plot_single_pie <- function(data_subset) {
  ggplot(data_subset, aes(x = "", y = proportion, fill = category)) +
    geom_col(width = 1, color = "white") +
    coord_polar(theta = "y") +
    scale_fill_manual(values = colors) +
    theme_void() +
    theme(legend.position = "none")  # Remove legend
}

# Create individual pie plots
pies <- pie_data %>%
  group_split(pie_id) %>%
  map(plot_single_pie)

# Combine pie charts horizontally with an arrow underneath
pie_row <- wrap_plots(pies, nrow = 1) +
  plot_annotation(
    theme = theme(plot.background = element_rect(fill = "white", color = NA))
  )

# Create the arrow as a simple ggplot object
arrow_plot <- ggplot() +
  annotate("segment",
           x = 0, xend = 1,
           y = 0, yend = 0,
           arrow = arrow(length = unit(0.3, "inches")),
           size = 1) +
  theme_void() +
  theme(
    plot.margin = margin(t = 0, r = 10, b = 0, l = 10),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Stack pies and arrow
final_plot <- pie_row / arrow_plot +
  plot_layout(heights = c(8, 1))

# Display the final figure
final_plot


#######################################

# Load necessary libraries
library(tidyverse)
library(patchwork)

# Define the three colors with the correct mapping
colors <- c(
  "Daily Functioning" = "#0072B2",   # Blue
  "Physical Health" = "#EF6C63",     # Peach
  "Mental Health" = "#009E73"  # Green
)

# Create a base dataset for each pie chart
# Each list element represents one pie with 3 categories smoothly transitioning
pie_data <- tibble(
  pie_id = rep(1:5, each = 3),
  category = rep(c("Mental Health", "Physical Health", "Daily Functioning"), times = 5),
  proportion = c(
    0.7, 0.2, 0.1,    # Pie 1
    0.55, 0.3, 0.15,   # Pie 2
    0.4, 0.35, 0.25,   # Pie 3
    0.25, 0.4, 0.35,   # Pie 4
    0.1, 0.2, 0.7      # Pie 5
  )
)

# Function to plot a single pie
plot_single_pie <- function(data_subset, show_legend = FALSE) {
  ggplot(data_subset, aes(x = "", y = proportion, fill = category)) +
    geom_col(width = 1, color = "white") +
    coord_polar(theta = "y") +
    scale_fill_manual(values = colors, name = "Domain") + # Name for the legend
    theme_void() +
 #   theme(legend.text = element_text(size = 10)) +
    theme(
      legend.position = if (show_legend) "right" else "none",  # Only one pie has a legend
      plot.margin = margin(5, 5, 5, 5)
    )
}

# Create individual pie plots
# Only the last pie will have a legend to keep figure clean
pies <- pie_data %>%
  group_split(pie_id) %>%
  map2(.y = 1:5, .f = ~plot_single_pie(.x, show_legend = .y == 5))  # Legend only for 5th pie

# Combine pies horizontally
pie_row <- wrap_plots(pies, nrow = 1, guides = "collect") & 
  theme(legend.position = "top")  # Pull the legend out to the right

# Create the arrow as a simple ggplot object
arrow_plot <- ggplot() +
  annotate("segment",
           x = 0, xend = 1,
           y = 0, yend = 0,
           arrow = arrow(length = unit(0.3, "inches")),
           size = 1) +
  theme_void() +
  theme(
    plot.margin = margin(t = 0, r = 10, b = 0, l = 10),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Stack pies and arrow
final_plot <- pie_row / arrow_plot +
  plot_layout(heights = c(8, 1)) +
  plot_annotation(
    theme = theme(plot.background = element_rect(fill = "white", color = NA))
  )

# Display the final figure
final_plot
