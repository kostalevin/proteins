# Library for printing plots on our website/dashboard.

library(bsselectR) 

view_plots <- function(path_to_plots = './plots', plot_name_filter, type = 'img') {
  # Select all plots from folder on 'path_to_plots' that have 'plot_name_filter'
  # in the file name and view them with select option.
  # The names of the plots will be extracted as part of the file names between '[' and ']'.
  # If it is a static plot, type = 'img', if interactive type = 'iframe'.
  
  plot_paths <- list.files(path = path_to_plots, pattern = plot_name_filter, full.names = TRUE)
  plot_names <- substr(x = plot_paths, 
                       start = regexpr(text = plot_paths, pattern = "\\[")+1,
                       stop = regexpr(text = plot_paths, pattern = "\\]")-1)
  names(plot_paths) <- plot_names
  bsselect(vector = plot_paths, type = type, live_search = TRUE, show_tick = TRUE)
}