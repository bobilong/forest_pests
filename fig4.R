library(data.table)
library(stringr)
library(dplyr)

######## import data ############
resultPaths_shap <- list.files('./data/','_.*_shap.csv',full.names = T)
results_shap <- lapply(resultPaths_shap, function(path){
  file_name <- basename(path)
  type <- str_split_i(file_name,'_',1)
  service <- str_split_i(file_name,'_',2)
  df <- fread(path)
  df$type <- type
  df$service <- service
  df <- df[!variable %in% c("x", "y")]
  return(df)
}) %>% rbindlist()

results_shap$variable <- results_shap$variable %>%
  str_replace("wc2.1_10m_bio_12", "mean_precipitation") %>%
  str_replace("wc2.1_10m_bio_1", "mean_temperature") %>%  
  str_replace("wc2.1_10m_bio_3", "isothermality") %>%  
  str_replace("^travel_time_to_cities_12", "travel_time") %>%  
  str_replace("^FFI2020", "FFI") %>% 
  str_replace("^ForestAge_TC010", "forest_age") %>% 
  str_replace("^GNI", "GNI")%>% 
  str_replace("^timberImport", "timber_import")%>% 
  str_replace("^LC_Type1", "forest_type")%>% 
  str_replace("^population_density", "population_density") 

# exlude non-forest of forest_type
results_shap <- results_shap[!(variable == "forest_type" & feature_value > 5),]

results_shap$feature_value <- ifelse(results_shap$variable%in%c('GNI','population_density','travel_time','timber_import'),log(results_shap$feature_value),results_shap$feature_value)
results_shap$variable <- ifelse(results_shap$variable=='GNI','log(GNI)',results_shap$variable)
results_shap$variable <- ifelse(results_shap$variable=='population_density','log(population_density)',results_shap$variable)
results_shap$variable <- ifelse(results_shap$variable=='travel_time','log(travel_time)',results_shap$variable)
results_shap$variable <- ifelse(results_shap$variable=='timber_import','log(timber_import)',results_shap$variable)

label_data <- data.frame(
  variable = c('FFI','forest_age','mean_temperature','mean_precipitation','isothermality','log(timber_import)',
               'log(GNI)','log(population_density)','log(travel_time)'),
  label = paste0(letters[1:9],')'),
  x = -Inf,
  y = Inf
)
add_shap_plot <- function(serviceName, label_data = NULL) {
  
  tmpDf <- results_shap[service == serviceName & feature_value != -Inf, ]
  
  contDf <- tmpDf[variable != "forest_type", ]
  catDf  <- tmpDf[variable == "forest_type", ]
  catDf$forest_type_label <- factor(
    catDf$feature_value,
    levels = c(1,2,3,4,5),
    labels = c("ENF","EBF","DNF","DBF","MF")
  )
  
  contDf <- contDf %>%
    group_by(variable) %>%
    mutate(
      bin = cut(feature_value, breaks = 50, include.lowest = TRUE),
      bin_count = n()
    ) %>%
    ungroup()
  
  median_data <- contDf %>%
    group_by(variable, bin, type) %>%
    filter(n() > 20) %>%
    summarise(
      median_shap = median(shap_value),
      median_value = median(feature_value),
      .groups = "drop"
    )
  

  if (!is.null(label_data)) {

    label_cont <- label_data[label_data$variable %in% unique(contDf$variable), ]
    label_cat  <- label_data[label_data$variable == "forest_type", ]
  } else {
    label_cont <- data.frame()
    label_cat  <- data.frame()
  }
  
  p_cont <- ggplot(contDf, aes(feature_value, shap_value)) +
    facet_wrap(~variable, scales = 'free', ncol = 3) +
    
    geom_point(aes(color = type),
               alpha = 0.1,
               show.legend = FALSE,
               size = 0.3) +
    geom_line(data = median_data,
              aes(x = median_value, y = median_shap, color = type),
              size = 1.2) +
    
    scale_color_manual(values = c('#2166AC','#D35400'),
                       labels = c("within", "outside")) +
    
    {if (nrow(label_cont) > 0) 
      geom_text(data = label_cont,
                aes(x = x, y = y, label = label),
                hjust = -0.1, vjust = 1.2,
                size = 4, fontface = "bold",
                inherit.aes = FALSE)
    } +
    
    labs(x = 'Feature value', y = 'SHAP value') +
    theme_bw() +
    theme(legend.title = element_blank())
  

  catDf$new_type <- factor(catDf$type, 
                           levels = c('native','non-native'),
                           labels = c("within", "outside"))
  
  p_cat <- ggplot(catDf,
                  aes(x = forest_type_label, y = shap_value)) +
    geom_boxplot(
      aes(fill = new_type),
      position = position_dodge2(width = 0.7, preserve = "single"),
      alpha = 0.6,
      na.rm = TRUE,
      outlier.shape = NA) +
    geom_jitter(aes(color = new_type),
                width = 0.2,
                show.legend = FALSE,
                size = 0.3) +
    scale_color_manual(values = c('#A9CBE8','#F7C58E')) +
    scale_fill_manual(
      values = c('#2166AC','#D35400'),
      drop = FALSE
    ) +
    
    {if (nrow(label_cat) > 0)
      geom_text(data = label_cat,
                aes(x = -Inf, y = Inf, label = label),
                hjust = -0.1, vjust = 1.2,
                size = 4, fontface = "bold",
                inherit.aes = FALSE)
    } +
    
    labs(x = 'Forest type', y = 'SHAP value') +
    theme_bw() +
    theme(legend.title = element_blank())
  
  p_final <- p_cont / p_cat + 
    plot_layout(heights = c(3, 1))
  
  return(p_final)
}

add_shap_plot('timber',label_data)
add_shap_plot('diversity',label_data)
add_shap_plot('carbon',label_data)
