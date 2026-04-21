library(patchwork)
library(tidyterra)
library(terra)
library(ggplot2)
library(dplyr)
library(data.table)

######## define parameter #################
crs <- '+proj=longlat +datum=WGS84'
globalRaster <- rast(vals=NA,nrows=180, ncols=360,xmin=-180, xmax=180,ymin=-90, ymax=90,crs=crs)
continentPolygon <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>% st_as_sf() %>% st_transform(crs)

country <- rnaturalearth::countries110 %>% vect() %>% .[,c('ISO_A3','NAME_LONG','INCOME_GRP','CONTINENT','SUBREGION')]
country$ISO_A3 <- countrycode::countrycode(
  sourcevar = country$NAME_LONG, 
  origin = "country.name", 
  destination = "iso3c",
  warn = TRUE 
)

##### sup fig.3 #############
allTif <- list.files('./data/',full.names = T)

addplot <- function(pestName, tagName){
  
  grepName <- paste0('^.*/', pestName, '_all\\.tif$')
  r <- grep(grepName, allTif, value = TRUE) %>% 
    rast() %>% 
    resample(globalRaster, method = 'near')
  
  r2 <- ifel(r > 0, r, NA)
  
  r_pct <- app(r2, fun = function(x){
    rank(x, na.last = "keep") / sum(!is.na(x))
  })
  
  p <- ggplot() +
    geom_spatraster(data = r_pct) +
    geom_spatvector(data = continentPolygon, fill = 'grey', alpha = 0.2) + 
    coord_sf(crs = crs, xlim = c(-160,165), ylim = c(-56,90)) +
    theme_bw() +   
    labs(tag = tagName) +
    scale_fill_gradientn(
      colours = c('yellow','red'), 
      limits = c(0.01, 0.99),
      breaks = c(0.01, 0.25, 0.5, 0.75, 0.99),
      labels = c("Top 99%", "Top 75%", "Top 50%", "Top 25%", "Top 1%"),
      
      na.value = 'transparent'
    ) +
    
    theme(
      legend.title = element_blank(),
      legend.text = element_text(size = 8),
      legend.position = 'none'
    )
  
  return(p)
}

all <- addplot('all','a)')
animal <- addplot('animals','b)')
plant <- addplot('plants','c)')
disease <- addplot('diseaes','d)')

(all+animal)/(plant+disease)

############ sup fig.4 ####################
srr_scaled_results <- fread('./data/srr_scaled_results.csv')
stand_plot <- left_join(country,srr_scaled_results)%>% .[!is.na(.$type),]

stand_plot <- stand_plot %>%
  mutate(type_new = case_when(
    type == "Animals" ~ "Fauna",
    type == "Dieases" ~ "Flora",
    type == "Plants" ~ "Pathogen",  
    TRUE ~ type
  ))

stand_plot$type_new <- factor(stand_plot$type_new,levels = c('Total','Fauna','Flora','Pathogen'))
stand_plot <- stand_plot %>%
  mutate(tag = case_when(
    type_new == "Total" ~ "a)",
    type_new == "Fauna" ~ "b)",
    type_new == "Flora" ~ "c)",
    type_new == "Pathogen" ~ "d)"
  ))

ggplot() +
  geom_spatvector(data = stand_plot, aes(fill = SRR_scaled), na.rm = TRUE,
                  color = "grey40", size = 0.1) +
  geom_text(
    data = stand_plot %>% group_by(type_new) %>% slice(1),
    aes(label = tag),
    x = -Inf, y = Inf,
    hjust = -0.2, vjust = 1.2,
    inherit.aes = FALSE,
    size = 5,
  ) +
  coord_sf(crs = crs, xlim = c(-160,165), ylim = c(-56,90), expand = FALSE) +
  facet_wrap(~type_new) +
  scale_fill_gradient2(
    low = "#2C7BB6",
    mid = "white",
    high = "#D7191C",
    na.value = 'white',
    name = "Scaled standardized report rate"
  ) +
  theme_bw() +
  theme(
    legend.position = 'bottom',
    strip.text = element_blank()
  )


#########sup fig.5 ###############
df_clean <- fread('./data/df_clean.csv')
null_model <- lm(SRR_scaled ~ log_GNI + log_forestArea+log_import, data = df_clean)
summary(null_model)

df_clean$Residuals <- residuals(null_model)
df_clean$Std_Residuals <- scale(df_clean$Residuals) 

ggplot(df_clean, aes(x = Income_Group, y = Std_Residuals, fill = Income_Group)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(y = "Standardized Residuals",
       x = "World Bank Income Group") +
  theme_bw()+
  theme(
    legend.title = element_blank()
  )


###########sup fig.6 ####################
addplot <- function(rName,plotName,tagName){
  path <- grep(rName,overlapPath,value=T)
  r <- rast(path)
  r2 <- mask(r,hasenForestcover2)
  
  ggplot() +
    geom_spatraster(data = r2) +
    geom_spatvector(data=continentPolygon,fill='grey',alpha=0.2)+
    coord_sf(crs = crs,xlim=c(-160,165),ylim=c(-56,90))+
    theme_bw()+
    labs(
      title = plotName,
      tag = tagName,
    )+
    scale_fill_gradientn(colours = c('#1E8E99FF','#FFE5CCFF','#993F00FF'),
                         # labels=scales::label_number(accuracy = 1),
                         # transform='log10',
                         breaks=c(0,1,2),
                         # limits=c(1,108),
                         na.value='white')+
    # guides(
    #   fill=guide_colorbar('Richness')
    # )+
    # labs(fill="Species Richness")+
    # theme_bw()+
    theme(
      plot.title = element_text(hjust=0.5),
      # plot.tag = element_text(size=6),
      # legend.title = element_blank(),
      #legend.title = element_text(hjust=0.5),
      # legend.title.align = -10,
      legend.position = 'none',
      # legend.key.height = unit(0.15,'cm')
    )
}
hasenForestcover <- rast('./data/hasenForestCover_resample.tif')
# filter tif under 10% to avoid the influence of non-forest areas
hasenForestcover2 <- ifel(hasenForestcover>10,hasenForestcover,NA)
overlapPath <- list.files('./data/',full.names = T)

native_timber <- addplot('/native_timber.tif','Within & timber','a)')
native_diversity <- addplot('/native_diversity.tif','Within & diversity','c)')
native_carbon <- addplot('/native_carbon.tif','Within & carbon','e)')
non_native_timber <- addplot('/non_native_timber.tif','Outside & timber','b)')
non_native_diversity <- addplot('/non_native_diversity.tif','Outside range & diversity','d)')
non_native_carbon <- addplot('/non_native_carbon.tif','Outside range & carbon','f)')


(native_timber+non_native_timber)/
(native_diversity+non_native_diversity)/
(native_carbon+non_native_carbon)


############sup fig.7 ##################
#native
nativeDf <- fread('./data/nativeExposed.csv')
nativeDf$id <- factor(nativeDf$id)

nativeColData <- nativeDf[EU753%in%c('United States','EU','Australia','Canada','China','United Kingdom'),]
nativeColData$EU753 <- factor(nativeColData$EU753,levels = c('United States','EU','Australia','Canada','China','United Kingdom'),
                              labels = c('USA','EU','Australia','Canada','China','UK'))

#non_native
non_nativeDf <- fread('./data/nonNativeExposed.csv')
non_nativeDf$id <- factor(non_nativeDf$id)

non_nativeColData <- non_nativeDf[EU753%in%c('United States','EU','Australia','Canada','China','United Kingdom'),]
non_nativeColData$EU753 <- factor(non_nativeColData$EU753,levels = c('United States','EU','Australia','Canada','China','United Kingdom'),
                                  labels = c('USA','EU','Australia','Canada','China','UK'))

                                  # timber
max_timber <- max(
  nativeColData$timber,
  non_nativeColData$timber,
  na.rm = TRUE
) / 1000000

# diversity
max_diversity <- max(
  nativeColData$diversity,
  non_nativeColData$diversity,
  na.rm = TRUE
)

# carbon
max_carbon <- max(
  nativeColData$carbon,
  non_nativeColData$carbon,
  na.rm = TRUE
) / 10000


addPlot <- function(colData,yVariable,scale_v,yName,tagName, ylim_max){
  ggplot()+
    geom_col(data=colData,
             aes(x=EU753,y={{yVariable}}/scale_v,
                 fill='#99C5E3'
             ),
             width = 0.5,show.legend = F)+
    scale_y_continuous(
      limits = c(0, ylim_max)
    )+
    # scale_fill_manual(
    #   # values=custom_colors,
    #   # breaks = labels,
    #   # labels=scales::label_number(accuracy = 1),
    #   expand=c(0,0)
    # )+
    labs(
      y=yName,tag=tagName
    )+
    theme_bw()+
    theme(
      panel.grid.minor = element_blank(),
      # legend.position = 'none',
      axis.title.x = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.text.x = element_text(angle =30,hjust = 0.5,vjust = 0.8)
    )
  
}



#timber 
non_native_timber <- addPlot(non_nativeColData,timber,1000000,expression("Exposed timber areas(10"^6~"ha)"),'d)', max_timber)
#diversity 
non_native_diversity <- addPlot(non_nativeColData,diversity,1,expression("Exposed forest diversity(species ha)"),'e)', max_diversity)
#carbon 
non_native_carbon <- addPlot(non_nativeColData,carbon,10000,expression("Exposed carbon stock(10"^4~"t)"),'f)', max_carbon)

#timber 
native_timber <- addPlot(nativeColData,timber,1000000,expression("Exposed timber areas(10"^6~"ha)"),'a)', max_timber)
#diversity 
native_diversity <- addPlot(nativeColData,diversity,1,expression("Exposed forest diversity(species ha)"),'b)', max_diversity)
#carbon 
native_carbon <- addPlot(nativeColData,carbon,10000,expression("Exposed carbon stock(10"^4~"t)"),'c)', max_carbon)

(native_timber+native_diversity+native_carbon)/
  (non_native_timber+non_native_diversity+non_native_carbon)&
  theme(
    axis.text = element_text(size=10)
  )


##############sup fig.8-10  #########################
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
# results <- fread('/root/autodl-tmp/pests/result/all_importance_results.csv')
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
# results_shap <- results_shap[results_shap$variable!='bio_15',]

# filter out non-forest areas (forest_type > 5)
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
add_shap_plot <- function(serviceName,label_data){
  
  tmpDf <- results_shap[service==serviceName & feature_value!=-Inf,]
  
  contDf <- tmpDf[variable != "forest_type",]
  catDf  <- tmpDf[variable == "forest_type",]
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
    # filter(n() > 20) %>%
    summarise(
      median_shap = median(shap_value),
      median_value = median(feature_value),
      .groups = "drop"
    )
  median_data$variable <- factor(median_data$variable, levels = label_data$variable)
  contDf$variable <- factor(contDf$variable, levels = label_data$variable)
  label_data$variable <- factor(label_data$variable, levels = label_data$variable)

  p_cont <- ggplot(contDf, aes(feature_value, shap_value)) +
    facet_wrap(~variable, scales = 'free', ncol = 3) +
    
    geom_point(aes(color = type),
               alpha = 0.1,
               show.legend = F,
               size = 0.3) +
    
    geom_line(data = median_data,
              aes(x = median_value, y = median_shap, color = type),
              size = 1.2) +
    
    
  geom_text(
    data = label_data,
    aes(x = x, y = y, label = label),
    hjust = -0.3, vjust = 1.5,   
    inherit.aes = FALSE,
    size = 5
  ) +
    
    scale_color_manual(values = c('#2166AC','#D35400'),
                       labels = c("within", "outside")) +
    labs(x = 'Feature value', y = 'SHAP value') +
    
    theme_bw() +
    theme(
      legend.title = element_blank()
    )
  
  
  catDf$new_type <- factor(catDf$type,levels=c('native','non-native'),
                           labels = c("within", "outside"))
  
  p_cat <- ggplot(catDf,
                  aes(x = forest_type_label, y = shap_value)) +
    geom_boxplot(
      aes(fill = new_type),
      position = position_dodge2(width = 0.7, preserve = "single"),
      alpha = 0.6,
      na.rm=T,
      outlier.shape = NA) +
    geom_jitter(aes(color = new_type),
                width = 0.2,
                # alpha = 0.2,
                show.legend = F,
                size = 0.3) +
    scale_color_manual(values = c('#A9CBE8','#F7C58E')) +
    scale_fill_manual(
      values = c('#2166AC',
                 '#D35400'),
      # breaks = c("within", "outside"),
      drop = F,
      # labels = c("within", "outside")
    )+
    labs(
      x = 'Forest type',
      y = 'SHAP value'
    ) +
    
    theme_bw() +
    theme(
      legend.title = element_blank()
    )
  p_cat <- p_cat +
    annotate("text",
             x = -Inf, y = Inf,
             label = "j)",
             hjust = -0.3, vjust = 1.5,
             size = 5)

  p_final <- p_cont / p_cat+ 
    plot_layout(heights = c(3, 1))&
    theme(strip.text = element_blank())
  
  return(p_final)
}

supfig8 <- add_shap_plot('timber',label_data)
supfig9 <- add_shap_plot('diversity',label_data)
supfig10 <- add_shap_plot('carbon',label_data)
