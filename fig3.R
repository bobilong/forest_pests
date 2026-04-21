library(data.table)
library(ggplot2)
library(dplyr)
library(stringr)
library(cowplot)

##### import data ############
native_moran <- fread('./data/native_moran.csv')
native_moran$type <- 'native'
non_native_moran <- fread('./data/non-native_moran.csv')
non_native_moran$type <- 'non_native'
all_moranData <- rbind(native_moran,non_native_moran)

plot_data <- copy(all_moranData)

# simplify pest type
plot_data$pest_group <- gsub("_native|_non_native", "", plot_data$pest_type)

# region order
plot_data$countrName_new <- factor(
  plot_data$countrName,
  levels = c("United States",'EU','Australia','Canada','China','United Kingdom'),
  labels =  c('USA','EU','Australia','Canada','China','UK')
)

# ecosystem service order
plot_data$tree_name <- factor(
  plot_data$tree_name,
  levels = c("timber","carbon","diversity"),
  labels=c('Timber','Carbon','Diversity')
)

plot_data$pest_group_new <- factor(
  plot_data$pest_group,
  levels = c("animals","plants","diseaes"),
  labels=c('Fauna','Flora','Pathogen')
)

plot_data$type_new <- factor(
  plot_data$type,
  levels = c("native","non_native"),
  labels=c('Within','Outside')
)


plot_data$sig <- dplyr::case_when(
  plot_data$p_value < 0.01 ~ "**",
  (plot_data$p_value > 0.01)&(plot_data$p_value <0.05) ~ "*",
  TRUE ~ ""
)

plot_data$moranIndex[abs(plot_data$moranIndex) < 0.005] <- 0

ggplot(plot_data, aes(
  x = countrName_new,
  y = pest_group_new,
  fill = moranIndex
)) +
  geom_tile(color = "white") +
  geom_text(
    aes(label = sprintf("%.2f", moranIndex)),
    size = 3
  ) +
  geom_text(
    aes(label = sig),
    size = 3,
    nudge_x = 0.28,
    nudge_y = 0.1
  ) +
  facet_grid(type_new ~ tree_name) +
  coord_fixed()+
  scale_fill_gradientn(
    colors=c("#2B5C8A","#81B2D4",'#FFD4A5',"#D9705A","#B42D34","#6E0D20"),
    # midpoint = 0,
    name = "Moran's I"
  ) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(size=9),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_blank(),
    strip.background = element_blank(),
    legend.position = 'none',
    # legend.direction = 'horizontal',
    strip.text = element_text(size = 10)
  )
