library(ggVennDiagram)
library(ggplot2)
library(terra)
library(data.table)
library(VennDiagram)
library(patchwork)

########## fig2a,b ############
#native
native_r <- rast('./data/native_hotspot.tif')
native_r_area <- cellSize(native_r,unit='ha')
area_stats <- zonal(native_r_area, native_r, fun="sum", na.rm=TRUE)
colnames(area_stats) <- c("value", "total_area")
area_stats <- subset(area_stats,area_stats$value!=0)

# check label names in ggVennDiagram
val_map <- setNames(area_stats$total_area, as.character(area_stats$value))
actual_areas <- c(
  ifelse(is.na(val_map["1"]), 0, val_map["1"]), # Timber only
  ifelse(is.na(val_map["3"]), 0, val_map["3"]), # Diversity only
  ifelse(is.na(val_map["5"]), 0, val_map["5"]), # Carbon only
  ifelse(is.na(val_map["4"]), 0, val_map["4"]), # T + D
  ifelse(is.na(val_map["6"]), 0, val_map["6"]), # T + C
  ifelse(is.na(val_map["8"]), 0, val_map["8"]), # D + C
  ifelse(is.na(val_map["9"]), 0, val_map["9"])  # All
)

plot_data <- process_data(Venn(list(Timber=1, Diversity=1, Carbon=1)))
region_label <- venn_regionlabel(plot_data)

# calculate percentage
region_label$count <- actual_areas 
region_label$my_percent <- (region_label$count / sum(region_label$count)) * 100

custom_colors <- c(
                   "1" = "#E69F00",#native timber 
                   "2" = "#56B4E9",#native diversity 
                   "3" = "#009E73",#native carbon 
                   "1/2" = "#F0E442",#native timber diversity 
                   "1/3" = "#0072B2",#native timber carbon
                   "2/3" = "#D55E00",#native diversity carbon
                   "1/2/3" = "#CC79A7"#native timber diversity carbon
)
my_breaks <- c("1", "2", "3", "1/2", "1/3", "2/3", "1/2/3")
# set location of lables
region_label$X[region_label$id == "1"] <- region_label$X[region_label$id == "1"] - 1 
region_label$Y[region_label$id == "1"] <- region_label$Y[region_label$id == "1"] - 1.4 

region_label$X[region_label$id == "2"] <- region_label$X[region_label$id == "2"] + 1
region_label$Y[region_label$id == "2"] <- region_label$Y[region_label$id == "2"] - 1.4 

# region_label$X[region_label$id == "3"] <- region_label$X[region_label$id == "3"] + 3
region_label$Y[region_label$id == "3"] <- region_label$Y[region_label$id == "3"] - 1.4 


legend_labels <- c(
  "1" = "Timber",
  "2" = "Diversity",
  "3" = "Carbon",
  "1/2" = "Timber & Diversity",
  "1/3" = "Timber & Carbon",
  "2/3" = "Diversity & Carbon",
  "1/2/3" = "All"
)

ggplot() +
  geom_polygon(data = venn_regionedge(plot_data), aes(X, Y, group = id, fill = id), show.legend = F) +
  geom_path(data = venn_setedge(plot_data), aes(X, Y, group = id), color = "#333333", linewidth = 1.2) + 
  geom_text(data = region_label, 
            aes(X, Y, label = paste0(round(my_percent, 1), "%")),
            size = 8,
            # fontface='bold'
            ) +
  # geom_text(data = venn_setlabel(plot_data), aes(X, Y, label = name), size = 6, fontface = "bold") +
  scale_fill_manual(values = custom_colors,breaks = my_breaks,labels = legend_labels) +
  coord_equal() +
  theme_void()

#non_native
non_native_r <- rast('./data/non_native_hotspot.tif')
non_native_r_area <- cellSize(non_native_r,unit='ha')
area_stats <- zonal(non_native_r_area, non_native_r, fun="sum", na.rm=TRUE)
colnames(area_stats) <- c("value", "total_area")
area_stats <- subset(area_stats,area_stats$value!=0)

plot_data <- process_data(Venn(list(Timber=1, Diversity=1, Carbon=1)))
region_label <- venn_regionlabel(plot_data)

region_label$count <- actual_areas 
region_label$my_percent <- (region_label$count / sum(region_label$count)) * 100


region_label$X[region_label$id == "1"] <- region_label$X[region_label$id == "1"] - 1 
region_label$Y[region_label$id == "1"] <- region_label$Y[region_label$id == "1"] - 1.4 


region_label$X[region_label$id == "2"] <- region_label$X[region_label$id == "2"] + 1
region_label$Y[region_label$id == "2"] <- region_label$Y[region_label$id == "2"] - 1.4 


# region_label$X[region_label$id == "3"] <- region_label$X[region_label$id == "3"] + 3
region_label$Y[region_label$id == "3"] <- region_label$Y[region_label$id == "3"] - 1.4 

legend_labels <- c(
  "1" = "Timber",
  "2" = "Diversity",
  "3" = "Carbon",
  "1/2" = "Timber & Diversity",
  "1/3" = "Timber & Carbon",
  "2/3" = "Diversity & Carbon",
  "1/2/3" = "All"
)
my_breaks <- c("1", "2", "3", "1/2", "1/3", "2/3", "1/2/3")

ggplot() +
  geom_polygon(data = venn_regionedge(plot_data), aes(X, Y, group = id, fill = id), show.legend = T) +
  geom_path(data = venn_setedge(plot_data), aes(X, Y, group = id), color = "#333333", linewidth = 1.2) + 
  geom_text(data = region_label, 
            aes(X, Y, label = paste0(round(my_percent, 1), "%")),
            size = 8,
            # fontface='bold'
  ) +
  # geom_text(data = venn_setlabel(plot_data), aes(X, Y, label = name), size = 6, fontface = "bold") +
  scale_fill_manual(values = custom_colors,breaks = my_breaks,labels = legend_labels) +
  coord_equal() +
  theme_void()+
  theme(
    legend.title = element_blank(),
    legend.key.width = unit(0.3, 'cm'),  
    legend.key.height = unit(0.3, 'cm'), 
    legend.text = element_text(size=10),
    legend.position = 'bottom',
    plot.margin = margin(10, 10, 10, 10)
  )

########## fig2g,h ############
countrys_forestArea <- fread('./data/countrys_forestArea.csv')
nativeDf <- fread('./data/nativeDf2.csv')
non_nativeDf2 <- fread('./data/non-nativeDf2.csv')

addplot_v3 <- function(df, type = "composition", y_limit = NULL){
  # prepare data
  df$id <- factor(df$id)
  plotDf <- df[, .(area_sum = sum(area, na.rm = TRUE)), .(EU753, id)]
  country_totals <- plotDf[, .(total_area_mha = sum(area_sum, na.rm = TRUE) / 1e6), .(EU753)]
  
  country_totals$percentage_exposed <- (country_totals$total_area_mha * 1e6 / countrys_forestArea) * 100
  
  top6_names <- c('United States','EU','Australia','Canada','China','United Kingdom')
  top6_labels <- c('USA','EU','Australia','Canada','China','UK')

  plotDf_top6 <- plotDf[EU753 %in% top6_names]
  plotDf_top6$EU753_new <- factor(plotDf_top6$EU753, levels = top6_names, labels = top6_labels)
  
  totals_top6 <- country_totals[EU753 %in% top6_names]
  totals_top6$EU753_new <- factor(totals_top6$EU753, levels = top6_names, labels = top6_labels)
  
  # plot
  p <- ggplot()
  
  if(type == "composition"){
    p <- p + 
      geom_bar(data = plotDf_top6, aes(x = EU753_new, y = area_sum, fill = id), 
               stat = "identity", position = "fill", width = 0.6) +
      # total area (Mha)
      geom_text(data = totals_top6, 
                aes(x = EU753_new, y = 1, label = round(total_area_mha, 1)), 
                vjust = -0.5, size = 5, fontface = "plain") +
      scale_fill_manual(values = custom_colors[1:7]) +
      scale_y_continuous(name = "Composition of exposure (%)", 
                         labels = scales::percent_format(),
                         breaks = seq(0, 1, by = 0.25),
                         # limits = c(0, 1),
                         expand = expansion(mult = c(0, 0.1))
                         )
    
  } else {
    p <- p + 
      geom_segment(data = totals_top6, 
                   aes(x = EU753_new, xend = EU753_new, y = 0, yend = percentage_exposed), 
                   color = "gray85", linetype = "dashed") +
      geom_point(data = totals_top6, aes(x = EU753_new, y = percentage_exposed), 
                 size = 3.5, color = "#3A83C1", fill = "white", shape = 21, stroke = 1) +
      scale_y_continuous(name = "Proportion exposed (%)", 
                         limits = c(0, y_limit),
                         # expand = expansion(mult = c(0, 0.1))
                         )
  }
  
  # publishable theme
  p <- p + 
    labs(x = NULL) +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = "none",
      axis.text.x = if(type == "composition") element_blank() else element_text(angle = 0)
    )
  
  return(p)
}

p1_native <- addplot_v3(nativeDf, type = "composition")
p2_nonnative <- addplot_v3(non_nativeDf2, type = "composition")

p3_native <- addplot_v3(nativeDf, type = "proportion", y_limit = 46)
p4_nonnative <- addplot_v3(non_nativeDf2, type = "proportion", y_limit = 46)

(p1_native | p2_nonnative) / (p3_native | p4_nonnative) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "none",
        axis.text = element_text(size=13),
        axis.title = element_text(size=14))



