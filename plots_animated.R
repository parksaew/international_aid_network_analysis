
pacman::p_load(igraph,
               countrycode,
               GGally,
               network,
               sna,
               tidyverse,
               ggraph,
               plotly,
               gganimate,
               animation,
               gridExtra)


crscollapsed <- read.csv("bilateral_aidflow.csv", stringsAsFactors = F)


######## Bipartite #########

##all years 

aidedge <- crscollapsed %>%
  filter(year %in% c(1990:2014)) %>%
  mutate(ccode1 = as.character(ccode1),
         ccode2 = as.character(ccode2),
         year = as.integer(as.character(year)),
         aidflow = round(aidflow, digits = 2))


aidnetb <- graph.data.frame(aidedge, directed = F)

V(aidnetb)$type <- 1
V(aidnetb)[name %in% aidedge$ccode1]$type <- 0
V(aidnetb)$strength <- strength(aidnetb)
E(aidnetb)$weight <- as.numeric(as.matrix(aidedge)[,"aidflow"])

V(aidnetb)$color <- c("orange", "skyblue")[V(aidnetb)$type+1]

g_all <- ggraph(aidnetb, "bipartite") + 
  geom_edge_link(aes(width = weight/2, color = 3), alpha = 0.5) + 
  geom_node_point(aes(size = strength, color = color)) + 
  geom_node_text(aes(label = ifelse(strength > quantile(strength, .95), 
                                    countrycode(name, "cown", "iso3c"), 
                                    NA)),
                 repel = TRUE) + 
  theme_graph() +
  theme(legend.position="none") +
  facet_edges(~ year)

ggsave("all_years.png", g_all, width=20, height=33, dpi=100)




####

## each year

#creating a list of dataframes and networks for each year
aidflow_year <- list()
aidflow_plots <- list()

for(i in 1:length(unique(aidedge$year))) {
  edge <- filter(aidedge,
                 year == min(aidedge$year)-1 + i)
  network <- graph.data.frame(edge, directed = F)
  
  V(network)$type <- 1
  V(network)[name %in% edge$ccode1]$type <- 0
  V(network)$strength <- strength(network)
  E(network)$weight <- as.numeric(as.matrix(edge)[,"aidflow"])
  V(network)$color <- c("orange", "skyblue")[V(network)$type+1]
  
  set.seed(111)
  plot <- ggraph(network, "bipartite") + 
    geom_edge_link(aes(width = weight, color = weight, alpha = weight)) + 
    scale_edge_colour_gradient(low = "steelblue1", high = "royalblue4") +
    scale_edge_alpha_continuous(range = c(min(E(network)$weight)/max(E(network)$weight) + 0.15, 1)) +
    scale_edge_width_continuous(range = c(0.5, max(E(network)$weight)/80)) +
    geom_node_point(aes(size = strength, color = color), alpha = 0.95) + 
    scale_size_continuous(range = c(min(V(network)$strength)/3, max(V(network)$strength)/3)) +
    geom_node_point(aes(size = strength), color= "black", shape = 1, stroke = 0.01, alpha = 0.05) + 
    geom_node_text(aes(label = ifelse(type == 1, 
                                      ifelse(
                                        strength > quantile(strength[type == 1], .9), 
                                        countrycode(name, "cown", "iso3c"), 
                                        NA), 
                                      ifelse(
                                        strength > quantile(strength[type == 0], .95), 
                                        countrycode(name, "cown", "iso3c"), 
                                        NA))),
                   repel = TRUE) + 
    theme_graph() +
    labs(title = as.character(min(aidedge$year)-1 + i)) +
    theme(legend.position="none")
  
  aidflow_year[[i]] <- list(edge, network, plot)
  aidflow_plots[[i]] <- plot
  
  names(aidflow_year)[i] <- paste0("aidflow_", (min(aidedge$year)-1 + i))
  names(aidflow_plots)[i] <- paste0("aidflow_", (min(aidedge$year)-1 + i))
}


## make sure ImageMagick has been installed in your system
saveGIF(for(i in 1:length(aidflow_plots)) {
  print(aidflow_plots[[i]])
}, interval = .6, 
ani.height = 600,
ani.width = 960,
movie.name="aidflow.gif")


## 1. name top donors AND recipients (DONE)
## 2. animate
  ## gganimate is being fixed, so it may not work with ggraph now
  ## https://github.com/thomasp85/gganimate/issues/139


ggraph(network, "bipartite") + 
  geom_edge_link(aes(width = weight, color = weight, alpha = weight)) + 
  scale_edge_colour_gradient(low = "steelblue1", high = "royalblue4") +
  scale_edge_alpha_continuous(range = c(min(E(network)$weight)/max(E(network)$weight) + 0.15, 1)) +
  scale_edge_width_continuous(range = c(0.5, max(E(network)$weight)/50)) +
  geom_node_point(aes(size = strength, color = color), alpha = 0.95) + 
  geom_node_point(aes(size = strength), color= "black", shape = 1, stroke = 0.01, alpha = 0.05) + 
  scale_size_continuous(range = c(min(V(network)$strength)/5, max(V(network)$strength)/5)) +
  geom_node_text(aes(label = ifelse(type == 1, 
                                    ifelse(
                                      strength > quantile(strength[type == 1], .95), 
                                      countrycode(name, "cown", "iso3c"), 
                                      NA), 
                                    ifelse(
                                      strength > quantile(strength[type == 0], .95), 
                                      countrycode(name, "cown", "iso3c"), 
                                      NA))),
                 repel = TRUE) + 
  theme_graph()

#####



## selected years
aidedge_sel <- crscollapsed %>%
  filter(year %in% c(1986, 1991, 1996, 2001, 2002, 2006, 2011, 2014)) %>%
  mutate(ccode1 = as.character(ccode1),
         ccode2 = as.character(ccode2),
         year = as.integer(as.character(year)),
         aidflow = round(aidflow, digits = 2))


aidnetb_sel <- graph.data.frame(aidedge_sel, directed = F)

V(aidnetb_sel)$type <- 1
V(aidnetb_sel)[name %in% aidedge_sel$ccode1]$type <- 0
V(aidnetb_sel)$strength <- strength(aidnetb_sel)
E(aidnetb_sel)$weight <- as.numeric(as.matrix(aidedge_sel)[,"aidflow"])

V(aidnetb_sel)$color <- c("orange", "skyblue")[V(aidnetb_sel)$type+1]

g_sel <- ggraph(aidnetb_sel, "bipartite") + 
  geom_edge_link(aes(width = weight/2, color = 3), alpha = 0.5) + 
  geom_node_point(aes(size = strength, color = color)) + 
  geom_node_text(aes(label = ifelse(strength > quantile(strength, .95), 
                                    countrycode(name, "cown", "iso3c"), 
                                    NA)),
                 repel = TRUE) + 
  theme_graph() +
  theme(legend.position="none") +
  facet_edges(~ year)

ggsave("select_years.png", g_sel, width=20, height=33, dpi=100)

#the issue here is that the percentiles are calculated for all the years and not within each year
########

library(gapminder)

ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent) +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear')



g_sel <- ggraph(aidnetb_sel, "bipartite") + 
  geom_edge_link0(aes(width = weight/2, color = 3), alpha = 0.5) + 
  geom_node_point(aes(size = strength, color = color)) + 
  geom_node_text(aes(label = ifelse(strength > quantile(strength, .95), 
                                    countrycode(name, "cown", "iso3c"), 
                                    NA)),
                 repel = TRUE) + 
  theme_graph() +
  theme(legend.position="none") +
  transition_time(year) +
  ease_aes('linear')


####

g_all <- ggraph(aidnetb, "bipartite") + 
  geom_edge_link(aes(width = weight/2, color = 3), alpha = 0.5) + 
  geom_node_point(aes(size = strength, color = color)) + 
  geom_node_text(aes(label = ifelse(strength > quantile(strength, .95), 
                                    countrycode(name, "cown", "iso3c"), 
                                    NA)),
                 repel = TRUE) + 
  theme_graph() +
  theme(legend.position="none") +
  facet_edges(~ year)





ggraph(aidnetb, "bipartite") + 
  geom_edge_link0(aes(width = weight, color = weight, alpha = weight)) + 
  scale_edge_colour_gradient(low = "steelblue1", high = "royalblue4") +
  scale_edge_alpha_continuous(range = c(min(E(aidnetb)$weight)/max(E(aidnetb)$weight) + 0.15, 1)) +
  scale_edge_width_continuous(range = c(0.5, max(E(aidnetb)$weight)/100)) +
  geom_node_point(aes(size = strength, color = color), alpha = 0.95) + 
  geom_node_point(aes(size = strength), color= "black", shape = 1, stroke = 0.01, alpha = 0.05) + 
  scale_size_continuous(range = c(min(V(aidnetb)$strength)/50, max(V(aidnetb)$strength)/50)) +
  geom_node_text(aes(label = ifelse(type == 1, 
                                    ifelse(
                                      strength > quantile(strength[type == 1], .95), 
                                      countrycode(name, "cown", "iso3c"), 
                                      NA), 
                                    ifelse(
                                      strength > quantile(strength[type == 0], .95), 
                                      countrycode(name, "cown", "iso3c"), 
                                      NA))),
                 repel = TRUE) + 
  theme_graph() +
  labs(title = 'Year: {frame_time}') +
  theme(legend.position="none") + 
  transition_time(year) +
  ease_aes('linear')

#animation works with geom_edge_link0, but the nodes don't move


ggraph(aidnetb, "bipartite") + 
  geom_edge_link(aes(width = weight/2, color = 3), alpha = 0.5) + 
  geom_node_point(aes(size = strength, color = color)) + 
  geom_node_text(aes(label = ifelse(strength > quantile(strength, .95), 
                                    countrycode(name, "cown", "iso3c"), 
                                    NA)),
                 repel = TRUE) + 
  theme_graph() +
  theme(legend.position="none") +
  facet_edges(~ year)



####
#test animation with shorter years to load quicker
aidedge_short <- crscollapsed %>%
  filter(year %in% c(1990:2000)) %>%
  mutate(ccode1 = as.character(ccode1),
         ccode2 = as.character(ccode2),
         year = as.integer(as.character(year)),
         aidflow = round(aidflow, digits = 2))

aidnetb_short <- graph.data.frame(aidedge_short, directed = F)

V(aidnetb_short)$type <- 1
V(aidnetb_short)[name %in% aidedge_short$ccode1]$type <- 0
V(aidnetb_short)$strength <- strength(aidnetb_short)
E(aidnetb_short)$weight <- as.numeric(as.matrix(aidedge_short)[,"aidflow"])

V(aidnetb_short)$color <- c("orange", "skyblue")[V(aidnetb_short)$type+1]


ggraph(aidnetb_short, "bipartite") + 
  geom_edge_link0(aes(width = weight/2, color = 3), alpha = 0.5) + 
  geom_point(aes(x=x, y=y, size = strength, color = color)) + 
  geom_text(aes(x=x, y=y, label = ifelse(strength > quantile(strength, .95), 
                                    countrycode(name, "cown", "iso3c"), 
                                    NA)),
                 repel = TRUE) + 
  theme_graph() +
  labs(title = 'Year: {frame_time}') +
  theme(legend.position="none") + 
  transition_time(year) +
  ease_aes("linear")


