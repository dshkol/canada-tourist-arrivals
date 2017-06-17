library(ggraph)

# function to create treemap for a given year's arrivals with option to EXCLUDE US
gen_tree <- function(tree_year, us = TRUE) {
  if(us == TRUE) {
    selected_data <- arrivals_annual
  } else  {
    selected_data <- overseas_annual
  }
  edge_df <- selected_data %>%
    filter(Year == tree_year) %>%
    select(Destination, Market)
  vert_df <- selected_data %>%
    filter(Year == tree_year) %>%
    select(Market, Year, Total, Region)
  graph <- graph_from_data_frame(edge_df, directed = TRUE, vertices = vert_df)
  # Modify visual look and feel here
  treemap <- ggraph(graph, 'treemap', weight = 'Total') + geom_node_tile(aes(fill = factor(Region)), size = 0.5, colour = "black") + geom_node_tile(aes(size = depth), colour = "white") +  scale_fill_brewer(palette = "Dark2","") + theme_graph() + labs(title = paste0("Overseas Arrivals to Canada in ",tree_year)) + theme(plot.title = element_text(hjust = 0.4)) + scale_size(range = c(0.25, 0.50), guide = 'none')
  treemap
  return(treemap)
}


