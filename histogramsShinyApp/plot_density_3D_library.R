# Functions for loading and plotting cluster points.

# Setup -------------------------------------------------------------------

# libraries for manipulating data
library(readr)
library(stringr)
library(tidyr)
library(dplyr)

# libraries for plotting
library(plotly)

options(stringsAsFactors = F)

# Data function -----------------------------------------------------------
load_data <- function(file_path) {
# Load data ---------------------------------------------------------------

# read data with fixed columns width
# filename <- 'AA03_nearest39random.pdb'
columns <- fwf_cols(
  record_type = c(1, 4), atom_serial_number = c(7, 11), 
  atom_name = c(13, 16), alternate_location_indiciator = c(17, 17), 
  residue_name = c(18, 20), chain_identifier = c(22, 22), 
  residue_sequence_number = c(23, 26), 
  code_for_insertion_of_residues = c(27, 27), 
  x = c(31, 38),  y = c(39, 46), z = c(47, 54), 
  occupancy = c(55, 60), temperature_factor = c(61, 66), 
  segment_identifier = c(73, 76), element_symbol = c(77, 78))
data <- read_fwf(file_path, columns)

# add column with model number
fileLines <- readLines(file_path)
model_numbers <- str_match(fileLines, 'MODEL[ ]+([0-9]+)') # extract model number
data$model_number <- model_numbers[,2] 
data <- fill(data, model_number, .direction = 'down') # fill down model number

# keep only rows with atom record types
data <- data[data$record_type == 'ATOM',]

# add number of nucleotid in model: 
# first/second nucleotid will be 0/1 group
data <- data %>% 
  group_by(model_number) %>%
  mutate(nucleotid_number = residue_sequence_number - residue_sequence_number[1] + 1)

# mark backbone atoms
backbone_atoms_1 <- c("C5'", "C4'", "C3'", "O3'")
backbone_atoms_2 <- c("P", "O5'", "C5'", "C4'", "C3'", "O3'")

data <- data %>% 
  mutate(is_backbone = ifelse(
    (atom_name %in% backbone_atoms_1 & nucleotid_number == 1) | 
    (atom_name %in% backbone_atoms_2 & nucleotid_number == 2), 
    T, F))

# check
# data %>% select(atom_name, model_number, nucleotid_number, is_backbone) %>% print(n=50)
# data %>% group_by(nucleotid_number, atom_name) %>% summarise(n()) %>% print(n=90)

# Define groups for colors --------------------------------------------------

# define color mapping for backbone atoms
nucleotid_number <- c(rep(1, 4), rep(2, 6))
backbone_atoms <- c(backbone_atoms_1, backbone_atoms_2)
colors_mapping <- data.frame(backbone_atoms,  nucleotid_number, 
                             group = paste(nucleotid_number, backbone_atoms))

# add groups
data <- merge(data, colors_mapping, 
              by.x = c('atom_name', 'nucleotid_number'), 
              by.y = c('backbone_atoms', 'nucleotid_number'), 
              all.x = T)

# for atoms that are not on backbone define grey color
data <- data %>% mutate(
  group = ifelse(is.na(group), 'Other', group))

# sort data again
data <- data %>% arrange(model_number, nucleotid_number, atom_serial_number)

return(data)

}



# Plots function -------------------------------------------------------------
plot_density <- function(point_size = 8, opacity = 0.1, data) {

# set colors for each group (i-th group will have i-th color)
groups <- sort(unique(data$group))
groups_colors <- RColorBrewer::brewer.pal(length(groups), name = "Set3")

# i-th group (alphabetically sorted) will have i-th color from groups_colors
p <- plot_ly(data, x = ~x, y = ~y, z = ~z, 
             color = ~group, colors=groups_colors, 
             opacity = opacity, size = I(point_size)) %>% 
  add_markers() %>%
  # fix axes ranges: when we select subset of points, it will not scale the plot
  layout(title = "Density plot",
         scene =list(
           xaxis = list(range = c(min(data$x), max(data$x))),
           yaxis = list(range = c(min(data$y), max(data$y))),
           zaxis = list(range = c(min(data$z), max(data$z))))
         )

return(p)

}
