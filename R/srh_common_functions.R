# This script is to source for common needs throughout the SRH analysis project such as:
#    Colors for consistency across plots
#    Themes for consistency across ggplots
#    Common functions for sensitivity analyses
#    Other functions that are used in multiple places

##########################
######### Colors #########
##########################

# Okabe-Ito colorblind-friendly palette
std_colors <- c(      
  "#56B4E9", # sky blue
  "#CC79A7",  # reddish purple
  "#009E73", # bluish green
  "#E69F00", # orange
  "#F0E442", # yellow
  "#0072B2", # blue
  "#D55E00" # vermillion
)

# Okabe-Ito colorblind-friendly palette in rainbow order
rainbow_order <- c(        
  "#D55E00", # vermillion
  "#E69F00", # orange
  "#F0E442", # yellow
  "#009E73", # bluish green
  "#56B4E9", # sky blue
  "#0072B2", # blue
  "#CC79A7"  # reddish purple
)

mental_health_colors <- c(
  "#CC79A7",  # reddish purple
  "#E69F00", # orange  
  "#D55E00", # vermillion
  "#999999", # grey
  "#000000",  # black
  "#F0E442", # yellow 
  "#009E73" # bluish green 
)

physical_health_colors <- c(
  "#56B4E9", # sky blue
  "#0072B2", # blue
  "#999999", # grey
  "#000000",  # black
  "#009E73", # bluish green
  "#F0E442", # yellow 
  "#D55E00" # vermillion
)

overall_health_colors <- c(
  "#009E73", # bluish green
  "#F0E442", # yellow
  "#999999", # grey
  "#000000",  # black
  "#CC79A7",  # reddish purple 
  "#56B4E9", # sky blue
  "#E69F00", # orange  
  "#D55E00", # vermillion
  "#F0E442" # yellow 
)

#################################
######### GGplot Themes #########
#################################

##################################################
######### Sensitivity Analysis Functions #########
##################################################





# Colors

# Function to bin the ages into groups
