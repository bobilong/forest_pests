#####load enviroment#######
library(terra)
library(tidyterra)
library(data.table)
library(ggplot2)
library(paletteer)
library(scales)
library(patchwork)
######define parameters##########
crs <- '+proj=longlat +datum=WGS84'
globalRaster <- rast(vals=NA,nrows=360, ncols=720,xmin=-180, xmax=180,ymin=-90, ymax=90,crs=crs)
continentPolygon <- vect('/root/autodl-tmp/worldBorder/World_Continents.shp')
coast <- rnaturalearth::ne_coastline(scale = "small", returnclass = "sf") %>% vect()
`%notin%` <- Negate(`%in%`)


custom_colors <- c(#"0" = "white", "1" = "white","2" = "white","3" = "white","4" = "white",
  #"5" = "white","6" = "white","7" = "white","10" = "white",
  "0" = "lightgrey", #low risk
  "1" = "#FE97A4", #native timber 
  "3" = "#B450DC",#native diversity 
  "5" = "#EBBF00",#native carbon 
  "4" = "#EA68A2", #native timber diversity 
  "6" = "#EB7100",#native timber carbon
  "8" = "#D0886E", #native diversity carbon
  "9" = "#E53341"#native timber diversity carbon
)
labels <- c(
  "0" = "Low risk", 
  "1" = "Timber exposed zone", 
  "3" = "Diversity exposed zone", 
  "5" = "Carbon exposed zone", 
  "4" = "Timber and diversity exposed zone", 
  "6" = "Timber and carbon exposed zone",
  "8" = "Diversity and carbon exposed zone",
  "9" = "Timber, diversity and carbon exposed zone"
)
label_name <- data.frame(
  id=c(0,1,3,5,4,6,8,9),
  type=c("Low risk","Timber exposed zone",
         "Diversity exposed zone","Carbon exposed zone",
         "Timber and diversity exposed zone","Timber and carbon exposed zone",
         "Diversity and carbon exposed zone","Timber, diversity and carbon exposed zone")
)
#############---------Fig-------------#######################
######fig1 pest pattern ################
#all pests
pal <- c(paletteer_d("colorBlindness::Blue2DarkOrange12Steps")[1:3],
         paletteer_d("colorBlindness::Blue2DarkOrange12Steps")[7:12]) 
allTif <- list.files('/root/autodl-tmp/pests/GBIF/native_nonNative2/',full.names = T)

patternMap <- function(typeName,tagName1,tagName2){
  all_r <- grep(paste0(typeName,'_all.tif'),allTif,value=T) %>% rast() %>% resample(globalRaster,'near')
  all_r2 <- ifel(all_r>0,all_r,NA)
  r_cap <- ifel(all_r2 > 12, 12, all_r2)
  
  p1 <- ggplot() +
    geom_spatraster(data = r_cap) +
    geom_spatvector(data=continentPolygon,fill='grey',alpha=0.2)+
    coord_sf(crs = crs,xlim=c(-160,165),ylim=c(-56,90))+
    theme_bw()+
    scale_fill_gradientn(
      colours = pal,
      limits  = c(1, 12),          
      oob     = squish,             
      breaks  = c(1, 4, 8, 12),     
      labels  = c("1", "4", "8", "≥12"),
      # trans   = power_trans(0.6),   
      na.value = "white",
      name    = "Richness"
    )+
    labs(tag=tagName1)+
    # theme_bw()+
    theme(
      # plot.title = element_text(hjust=0.5),
      # legend.title = element_blank(),
      #legend.title = element_text(hjust=0.5),
      # legend.title.align = -10,
      legend.position = 'bottom',
      # legend.direction='vertical',
      legend.key.width = unit(1,'cm'),
      legend.key.height = unit(0.15,'cm')
    )
  
  native_r <- grep(paste0(typeName,'_native.tif'),allTif,value=T) %>% rast()
  nonnative_r <- grep(paste0(typeName,'_non_native.tif'),allTif,value=T) %>% rast()
  diff_r <- native_r - nonnative_r
  
  
  sum_r  <- native_r + nonnative_r

  sum_r[sum_r == 0] <- NA  
  rel_r  <- (native_r - nonnative_r) / sum_r
  rel_resample <- resample(rel_r,globalRaster,'bilinear')
  
  
  p2 <- ggplot() +
    geom_spatraster(data = rel_resample,maxcell = 1036800) +
    geom_spatvector(data=continentPolygon,fill='grey',alpha=0.2)+
    coord_sf(crs = crs,xlim=c(-160,165),ylim=c(-56,90))+
    theme_void()+
    guides(
      fill=guide_colorbar('Non-native to Native')
    )+
    scale_fill_gradientn(colours = paletteer_d("colorBlindness::Blue2DarkOrange12Steps"),
                         # labels=scales::label_number(accuracy = 1),
                         # transform='log10',
                         # limits=c(1,108),
                         na.value='white')+
    labs(tag  =tagName2)+
    theme_bw()+
    theme(
      # plot.tag.position = c(0.02, 0.98),
      plot.title = element_text(hjust=0.5),
      # legend.title = element_text(size=8),
      # legend.title.align = -10,
      legend.position = 'bottom',
      # legend.direction='vertical',
      legend.key.width = unit(1,'cm'), 
      legend.key.height = unit(0.15,'cm')
    )
  return(p1+p2)
}

allPlot <- patternMap('all','a)','b)')
animalPlot <- patternMap('animals','c)','d)')
plantPlot <- patternMap('plants','e)','f)')
diseasePlot <- patternMap('diseaes','g)','h)')

#######fig2############
native_r <- rast('/root/autodl-tmp/pests/GBIF/native_nonNative2/all_native.tif')
native <- ifel(native_r>0,native_r,NA)
nonnative_r <- rast('/root/autodl-tmp/pests/GBIF/native_nonNative2/all_non_native.tif')
non_native <- ifel(nonnative_r>0,nonnative_r,NA)


timber <- rast('/root/autodl-tmp/pests/Curtis_Forestry_10Percent_10km2.tif') %>% resample(native_r)
timber2 <- ifel(is.na(timber),0,1)
diversity <- rast('/root/autodl-tmp/pests/treeBiodiversity.tif')%>% resample(native_r)
# diversity2 <- ifel(is.na(diversity),0,diversity)
carbon <- rast('/root/autodl-tmp/pests/restoration/carbon_resample.tif')
# carbon2 <- ifel(is.na(carbon),0,carbon)

services <- list(
  diversity,
  carbon
)


make_binary_pest <- function(r,rr){
  thr <- global(r, quantile, probs=0.5, na.rm=TRUE)[[1]]
  r2 <- ifel(rr>thr,1,0)
  return(r2)
}
hot_native   <- make_binary_pest(native,native_r)
hot_non      <- make_binary_pest(non_native,nonnative_r)

make_binary <- function(r){
  thr <- global(r, quantile, probs=0.5, na.rm=TRUE)[[1]]
  r2 <- ifel(r>thr,1,0)
  return(r2)
}

service_high <- lapply(services, make_binary)
service_high[[3]] <- timber2

names(service_high) <- c('diversity','carbon','timber')

plot_list <- list()
idx <- 1
for(p in c("native","non_native")){
  hotp <- if(p=="native") hot_native else hot_non
  for(s in names(service_high)){
    hs <- service_high[[s]]
   
    comb <- hotp + hs
    names(comb) <- paste0(p,'_',s)

    # plot_list[[idx]] <- comb %>% resample(native,'near')
    # idx <- idx + 1
    writeRaster(comb,paste0('/root/autodl-tmp/pests/overlapResult/',paste0(p,'_',s),'.tif'),overwrite=T)
    print(paste0(p,'_',s))
  }
}

hasenForestcover <- rast('/root/autodl-tmp/pests/hasenForestCover_resample.tif')
hasenForestcover2 <- ifel(hasenForestcover>10,hasenForestcover,NA)
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
      # legend.direction='vertical',
      # legend.key.width = unit(1,'cm'), 
      # legend.key.height = unit(0.15,'cm')
    )
}

overlapPath <- list.files('/root/autodl-tmp/pests/overlapResult/','.tif',full.names = T)

native_timber <- addplot('/native_timber.tif','Native range & timber','a)')
native_diversity <- addplot('/native_diversity.tif','Native range & diversity','c)')
native_carbon <- addplot('/native_carbon.tif','Native range & carbon','e)')
non_native_timber <- addplot('/non_native_timber.tif','Non-native range & timber','b)')
non_native_diversity <- addplot('/non_native_diversity.tif','Non-native range & diversity','d)')
non_native_carbon <- addplot('/non_native_carbon.tif','Non-native range & carbon','f)')


(native_timber+non_native_timber)/
  (native_diversity+non_native_diversity)/
  (native_carbon+non_native_carbon)

##Fig.2 a calculate relative risk with native pests-pie plot#####
hotspot <- rast('/root/autodl-tmp/pests/overlapResult/native_hotspot.tif')
timber <- rast('/root/autodl-tmp/pests/Curtis_Forestry_10Percent_10km2.tif') %>% resample(hotspot)
# timber2 <- ifel(is.na(timber),0,1)
diversity <- rast('/root/autodl-tmp/pests/treeBiodiversity.tif')%>% resample(hotspot)
# diversity2 <- ifel(diversity>0,diversity,NA)
carbon <- rast('/root/autodl-tmp/pests/restoration/carbon_resample.tif')
# carbon2 <- ifel(carbon>0,carbon,NA)

#timber
timberOnly <- ifel(timber>0,1,NA)
timberArea <- cellSize(timberOnly,unit='ha',mask=T)
timberExposedArea <- terra::zonal(timberArea,hotspot,'sum',na.rm=T)
timberExposedArea$area <- ifelse(is.na(timberExposedArea$area),0,timberExposedArea$area)
# fwrite(timberExposedArea,'/root/autodl-tmp/pests/GBIF/pieData/native_timber.csv')


#diversity
highDiv <- ifel(diversity>5,1,NA)
diversityHot <- mask(diversity,highDiv) %>% cellSize(mask=T,unit='ha')
diversityExposedArea <- terra::zonal(diversityHot,hotspot,'sum',na.rm=T)
# timberExposedArea$area <- ifelse(is.na(timberExposedArea$area),0,timberExposedArea$area)
# fwrite(diversityExposedArea,'/root/autodl-tmp/pests/GBIF/pieData/native_diversity.csv')

#carbon
highCarbon <- ifel(carbon>26,1,NA)
carbonHot <- mask(carbon,highCarbon) %>% cellSize(mask=T,unit='ha')
carbonExposedArea <- terra::zonal(carbonHot,hotspot,'sum',na.rm=T)
carbonExposedArea$area <- ifelse(is.na(carbonExposedArea$area),0,carbonExposedArea$area)

# fwrite(carbonExposedArea,'/root/autodl-tmp/pests/GBIF/pieData/native_carbon.csv')
library(ggforce)

addPiePlot <- function(df){
  colnames(df) <- c('category','value')
  df <- df[order(df$value), ] %>% .[.$category != 0 & .$value > 0,]
  # df$category <- factor(df$category)
  
  df2 <- df %>%
    mutate(
      percentage = value / sum(value) * 100,
      label = paste0(round(percentage, 1), "%")
    )
  
  df2 <- df2 %>%
    arrange(desc(category)) %>%
    mutate(
      end_angle = 2 * pi * cumsum(percentage) / 100,
      start_angle = lag(end_angle, default = 0),
      mid_angle = (start_angle + end_angle) / 2,
      label_x = 1 * sin(mid_angle),
      label_y = 1 * cos(mid_angle)
    )
  
  custom_colors <- c("0" = "lightgrey",
                     "1" = "#FE97A4",
                     "3" = "#B450DC",
                     "5" = "#EBBF00",
                     "4" = "#EA68A2",
                     "6" = "#EB7100",
                     "8" = "#D0886E",
                     "9" = "#E53341")
  
  df2$category <- factor(df2$category)
  # df2$category <- factor(stringr::str_trim(as.character(df2$category)))

  ggplot(data=df2) +
    ggforce::geom_arc_bar(
      aes(
        x0 = 0, y0 = 0, r0 = 0.7, r = 1.0,
        start = start_angle, end = end_angle,
        fill = factor(category)
      ),
      color = "white", size = 0.5
    ) +
    ggrepel::geom_text_repel(
      aes(
        x = label_x, y = label_y,
        label = label
      ),
      size = 4,
      nudge_x = ifelse(df2$label_x > 0, 0.1, -0.1),
      
    
      nudge_y = ifelse(df2$label_y > 0, 0.1, -0.1),
      # direction='both',
      min.segment.length = 0,
      segment.color = "grey50", 
      
      # max.overlaps = Inf
    ) +
    scale_fill_manual(values = custom_colors,limits = levels(df2$category)) +
    coord_equal(xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5)) +
    theme_void() +
    theme(
      legend.position = "none"
    )
  
}

timberPie <- addPiePlot(timberExposedArea)
diversityPie <- addPiePlot(diversityExposedArea)
carbonPie <- addPiePlot(carbonExposedArea)


timberPie+diversityPie+carbonPie


##Fig.2 b calculate relative risk with native pests-pie plot#####
hotspot <- rast('/root/autodl-tmp/pests/overlapResult/non_native_hotspot.tif')
timber <- rast('/root/autodl-tmp/pests/Curtis_Forestry_10Percent_10km2.tif') %>% resample(hotspot)
# timber2 <- ifel(is.na(timber),0,1)
diversity <- rast('/root/autodl-tmp/pests/treeBiodiversity.tif')%>% resample(hotspot)
# diversity2 <- ifel(diversity>0,diversity,NA)
carbon <- rast('/root/autodl-tmp/pests/restoration/carbon_resample.tif')
# carbon2 <- ifel(carbon>0,carbon,NA)

#timber
timberOnly <- ifel(timber>0,1,NA)
timberArea <- cellSize(timberOnly,unit='ha',mask=T)
timberExposedArea <- terra::zonal(timberArea,hotspot,'sum',na.rm=T)
timberExposedArea$area <- ifelse(is.na(timberExposedArea$area),0,timberExposedArea$area)
# fwrite(timberExposedArea,'/root/autodl-tmp/pests/GBIF/pieData/native_timber.csv')


#diversity
highDiv <- ifel(diversity>5,1,NA)
diversityHot <- mask(diversity,highDiv) %>% cellSize(mask=T,unit='ha')
diversityExposedArea <- terra::zonal(diversityHot,hotspot,'sum',na.rm=T)
# timberExposedArea$area <- ifelse(is.na(timberExposedArea$area),0,timberExposedArea$area)
# fwrite(diversityExposedArea,'/root/autodl-tmp/pests/GBIF/pieData/native_diversity.csv')

#carbon
highCarbon <- ifel(carbon>26,1,NA)
carbonHot <- mask(carbon,highCarbon) %>% cellSize(mask=T,unit='ha')
carbonExposedArea <- terra::zonal(carbonHot,hotspot,'sum',na.rm=T)
carbonExposedArea$area <- ifelse(is.na(carbonExposedArea$area),0,carbonExposedArea$area)


timberPie <- addPiePlot(timberExposedArea)
diversityPie <- addPiePlot(diversityExposedArea)
carbonPie <- addPiePlot(carbonExposedArea)


timberPie+diversityPie+carbonPie



#######--------Supplementary---------###################
patternMap <- function(richness,tagName){
  richness2 <- ifel(richness>0,richness,NA) 
  richnessRe <- resample(richness2,globalRaster,'near')
  
  q15_nonzero <- global(richnessRe, fun = \(x) quantile(x, 0.85, na.rm  = TRUE))[[1]]
  q5_nonzero <- global(richnessRe, fun = \(x) quantile(x, 0.95, na.rm  = TRUE))[[1]]
  
  hotspot <- ifel(richnessRe >= q5_nonzero, 1, ifel(richnessRe >= q15_nonzero &richnessRe < q5_nonzero,2,NA)) 
  
  p1 <- ggplot() +
    geom_spatraster(data = hotspot,maxcell = 1036800) +
    geom_spatvector(data=continentPolygon,fill='grey',alpha=0.2)+
    coord_sf(crs = crs,xlim=c(-160,165),ylim=c(-56,90))+
    theme_bw()+
    labs(
      tag = tagName
    )+
    scale_fill_gradientn(colours = c('red','yellow'),
                         breaks=c(1,2),
                         labels = c(
                           '1'="Top 5%",
                           '2'= "Top 15%"
                         ),
                         guide = 'legend',
                         na.value='white')+
    theme(
      plot.title = element_text(hjust=0.5),
      legend.title = element_blank(),
      #legend.title = element_text(hjust=0.5),
      legend.title.align = -10,
      legend.position = 'none',
      # legend.key.size = c(0.15,0.15,'cm')
      # legend.direction='vertical',
      legend.key.width = unit(0.15,'cm'),
      legend.key.height = unit(0.15,'cm')
    )
  return(p1)
}

#both native and non-native
nativeList <- list.files('/root/autodl-tmp/pests/GBIF/native_nonNative2/',full.names = T)

allPest <- grep('all_all.tif',nativeList,value=T) %>% 
  rast() %>% patternMap(.,'a)')
# allPest
animalPest <- grep('animals_all.tif',nativeList,value=T) %>% 
  rast()%>% patternMap(.,'b)')
# animalPest
plantPest <- grep('plants_all.tif',nativeList,value=T) %>% 
  rast()%>% patternMap(.,'c)')
# plantPest
pathogenPest <- grep('diseaes_all.tif',nativeList,value=T) %>% 
  rast()%>% patternMap(.,'d)')

(allPest+animalPest)/
  (plantPest+pathogenPest)

pathogenPest+theme(
  legend.position = 'bottom'
)

#native or non-native
patternMap <- function(richness,richnessN,tagName1,tagName2){
  richness2 <- ifel(richness>0,richness,NA) 
  richnessRe1 <- resample(richness2,globalRaster,'near')
  
  p1 <- ggplot() +
    geom_spatraster(data = richnessRe1,maxcell = 1036800) +
    geom_spatvector(data=continentPolygon,fill='grey',alpha=0.2)+
    coord_sf(crs = crs,xlim=c(-160,165),ylim=c(-56,90))+
    theme_bw()+
    labs(
      tag = tagName1
    )+
    scale_fill_gradientn(colours = paletteer_d("colorBlindness::Blue2DarkOrange12Steps"),
                         labels=scales::label_number(accuracy = 1),
                         transform='log10',
                         # limits=c(1,108),
                         na.value='white')+
    guides(
      fill=guide_colorbar('Log10(richness)')
    )+
    # labs(fill="Species Richness")+
    # theme_bw()+
    theme(
      plot.title = element_text(hjust=0.5),
      # legend.title = element_blank(),
      #legend.title = element_text(hjust=0.5),
      # legend.title.align = -10,
      legend.position = 'none',
      # legend.direction='vertical',
      legend.key.width = unit(1,'cm'),
      legend.key.height = unit(0.15,'cm')
    )
  
  richnessN2 <- ifel(richnessN>0,richnessN,NA) 
  richnessRe2 <- resample(richnessN2,globalRaster,'near')
  
  p2 <- ggplot() +
    geom_spatraster(data = richnessRe2,maxcell = 1036800) +
    geom_spatvector(data=continentPolygon,fill='grey',alpha=0.2)+
    coord_sf(crs = crs,xlim=c(-160,165),ylim=c(-56,90))+
    theme_bw()+
    labs(
      tag = tagName2
    )+
    scale_fill_gradientn(colours = paletteer_d("colorBlindness::Blue2DarkOrange12Steps"),
                         labels=scales::label_number(accuracy = 1),
                         transform='log10',
                         # limits=c(1,108),
                         na.value='white')+
    guides(
      fill=guide_colorbar('Log10(richness)')
    )+
    # labs(fill="Species Richness")+
    # theme_bw()+
    theme(
      plot.title = element_text(hjust=0.5),
      # legend.title = element_blank(),
      #legend.title = element_text(hjust=0.5),
      # legend.title.align = -10,
      legend.position = 'none',
      # legend.direction='vertical',
      legend.key.width = unit(1,'cm'),
      legend.key.height = unit(0.15,'cm')
    )
  return(p1+p2)
}

allPestNative <- grep('all_native.tif',nativeList,value=T) %>% 
    rast() 
allPestNoNative <- grep('all_non_native.tif',nativeList,value=T) %>% 
  rast() 
patternMap(allPestNative,allPestNoNative,'a)','b)')

animalPestNative <- grep('animals_native.tif',nativeList,value=T) %>% 
    rast()
animalPestNoNative <- grep('animals_non_native.tif',nativeList,value=T) %>% 
  rast() 
patternMap(animalPestNative,animalPestNoNative,'c)','d)')

plantPestNative <- grep('plants_native.tif',nativeList,value=T) %>% 
    rast()
plantPestNoNative <- grep('plants_non_native.tif',nativeList,value=T) %>% 
  rast() 
patternMap(plantPestNative,plantPestNoNative,'e)','f)')

pathogenPestNative <- grep('diseaes_native.tif',nativeList,value=T) %>% 
      rast()
pathogenPestNoNative <- grep('diseaes_non_native.tif',nativeList,value=T) %>% 
  rast() 
patternMap(pathogenPestNative,pathogenPestNoNative,'g)','h)')&theme(
  legend.position = 'bottom'
)

#########fig. 3###############
native_r <- rast('/root/autodl-tmp/pests/GBIF/native_nonNative2/all_native.tif')

timber <- rast('/root/autodl-tmp/pests/Curtis_Forestry_10Percent_10km2.tif') %>% resample(native_r)
timber2 <- ifel(is.na(timber),0,1)
names(timber2) <- 'timber'
diversity <- rast('/root/autodl-tmp/pests/treeBiodiversity.tif')%>% resample(native_r)
names(diversity) <- 'diversity'
carbon <- rast('/root/autodl-tmp/pests/restoration/carbon_resample.tif')
names(carbon) <- 'carbon'

treeR <- c(timber2,diversity,carbon)

#native
pestTypepath <- list.files('/root/autodl-tmp/pests/GBIF/native_nonNative2/','s_native.tif',full.names = T)
countrys <- vect ('/root/autodl-tmp/WAEdata_new_y/result779/otherdata/Con_EU_dis.shp')

library(spdep)
library(gstat)
native_moran <- data.table()

get_k_moran <- function(df, coords, max_k = 20) {
  n_pts <- nrow(coords)
  if (n_pts < 3) {
    warning(sprintf("only %d points", n_pts))
    return(1)
  }
  

  k_max_real <- min(n_pts - 1, max_k)

  Ks <- seq(1, k_max_real, by = 2)
  
  Is <- numeric(length(Ks))
  for (i in seq_along(Ks)) {
    k <- Ks[i]

    if (k >= n_pts) {
      Is[i] <- NA
      next
    }

    nb <- tryCatch({
      knearneigh(coords, k = k) %>% knn2nb()
    }, error = function(e) {
      return(NULL)
    })
    if (is.null(nb) || n.comp.nb(nb)$nc != 1) {
      Is[i] <- NA
      next
    }
    lw <- nb2listw(nb, style="W", zero.policy=TRUE)
    mb <- spdep::moran_bv(df[[3]], df[[4]], lw, nsim=499)
    Is[i] <- mb$t0
  }
  
  if (all(is.na(Is))) {
    warning("explore all k, otherwise return k = 1")
    return(1)
  } else {
    best <- which.max(Is)
    return(Ks[best])
  }
}

for(type in pestTypepath){
  pestTypeName <- basename(type) %>% str_sub(1,-5)
  pestTypeR <- rast(type)
  pestTypeR2 <- ifel(pestTypeR>0,pestTypeR,NA)
  names(pestTypeR2) <- pestTypeName
  
  for (treeName in names(treeR)) {
    r <- treeR[treeName]
    r2 <- c(r,pestTypeR2)
    countryName <- c('United States','EU','Australia','Canada','China','United Kingdom')
    for (n in countryName) {
      countrShp <- subset(countrys,countrys$EU753==n)
      countrR <- mask(r2,countrShp)
      df <- as.data.frame(countrR,xy=T,na.rm=T)

      coords <- cbind(df$x, df$y)
      #optimal k
      optimal_k <- get_k_moran(df, coords, max_k = 20)
      #k neighborhood 
      knn <- knearneigh(coords, k =optimal_k)
      nb_list <-knn2nb(knn)

      W_std <-nb2listw(nb_list, style ="W", zero.policy =TRUE)
      res_xy <-  spdep::moran_bv(df[,3],df[,4],W_std, nsim=499)
      
      # lee_xy <- lee.mc(x, y, listw, nsim=499, return_boot=TRUE)
      moranSD <- boot::boot.ci(res_xy, conf=c(0.99, 0.95, 0.9), type="basic")
      tmpDf <- data.table(
        countrName=n,
        opt_k=optimal_k,
        pest_type=pestTypeName,
        tree_name=treeName,
        moranIndex=res_xy$t0,
        sd_99_1=moranSD$basic[1,4],
        sd_99_2=moranSD$basic[1,5],
        sd_95_1=moranSD$basic[2,4],
        sd_95_2=moranSD$basic[2,5]
      )
      native_moran <- rbind(native_moran,tmpDf)
      print(n)
    }
    
  }
}

# fwrite(native_moran,'/root/autodl-tmp/pests/result/native_moran.csv')


# native_moran <- fread('/root/autodl-tmp/pests/result/native_moran_sig.csv')
native_moran$pvalue <- ifelse(
  (native_moran$sd_99_1<0&native_moran$sd_99_2<0)|
    (native_moran$sd_99_1>0&native_moran$sd_99_2>0),
  'p<0.01',
  ifelse(
    (native_moran$sd_95_1<0&native_moran$sd_95_2<0)|
      (native_moran$sd_95_1>0&native_moran$sd_95_2>0),
    'p<0.05','Not significant'
  )
)
fwrite(native_moran,'/root/autodl-tmp/pests/result/native_moran_sig.csv')

#non_native
pestTypepath <- list.files('/root/autodl-tmp/pests/GBIF/native_nonNative2/','s_non_native.tif',full.names = T)
countrys <- vect ('/root/autodl-tmp/WAEdata_new_y/result779/otherdata/Con_EU_dis.shp')

library(spdep)
non_native_moran <- data.table()

for(type in pestTypepath){
  pestTypeName <- basename(type) %>% str_sub(1,-5)
  pestTypeR <- rast(type)
  pestTypeR2 <- ifel(pestTypeR>0,pestTypeR,NA)
  names(pestTypeR2) <- pestTypeName
  
  for (treeName in names(treeR)) {
    r <- treeR[treeName]
    r2 <- c(r,pestTypeR2)

    countryName <- c('United States','EU','Australia','Canada','United Kingdom','China')
    for (n in countryName) {
      countrShp <- subset(countrys,countrys$EU753==n)
      countrR <- mask(r2,countrShp)
      df <- as.data.frame(countrR,xy=T,na.rm=T)

      coords <- cbind(df$x, df$y)
      optimal_k <- get_k_moran(df, coords, max_k = 20)

      knn <- knearneigh(coords, k =optimal_k)
      nb_list <-knn2nb(knn)

      W_std <-nb2listw(nb_list, style ="W", zero.policy =TRUE)
      res_xy <-  moran_bv(df[,3],df[,4],W_std, nsim=499)
      moranSD <- boot::boot.ci(res_xy, conf=c(0.99, 0.95, 0.9), type="basic")
      tmpDf <- data.table(
        countrName=n,
        opt_k=optimal_k,
        pest_type=pestTypeName,
        tree_name=treeName,
        moranIndex=res_xy$t0,
        sd_99_1=moranSD$basic[1,4],
        sd_99_2=moranSD$basic[1,5],
        sd_95_1=moranSD$basic[2,4],
        sd_95_2=moranSD$basic[2,5]
      )
      non_native_moran <- rbind(non_native_moran,tmpDf)
      print(n)
    }
    
  }
}

# fwrite(native_moran,'/root/autodl-tmp/pests/result/non-native_moran.csv')


# native_moran <- fread('/root/autodl-tmp/pests/result/non-native_moran.csv')
non_native_moran$pvalue <- ifelse(
  (non_native_moran$sd_99_1<0&non_native_moran$sd_99_2<0)|
    (non_native_moran$sd_99_1>0&non_native_moran$sd_99_2>0),
  'p<0.01',
  ifelse(
    (non_native_moran$sd_95_1<0&non_native_moran$sd_95_2<0)|
      (non_native_moran$sd_95_1>0&nativnon_native_morane_moran$sd_95_2>0),
    'p<0.05','Not significant'
  )
)
fwrite(non_native_moran,'/root/autodl-tmp/pests/result/non-native_moran_sig.csv')


#plot
world.map <- rnaturalearth::ne_countries(returnclass = "sf") %>% filter(continent != "Antarctica")
Con_popentrpoul_sf <- vect ('/root/autodl-tmp/WAEdata_new_y/result779/otherdata/Con_EU_dis.shp');head(Con_popentrpoul_sf)
nativeCountryName <- c('United States','EU','Australia','Canada','China','United Kingdom')
# nonNativeCountryName <- c('United States','EU','Australia','Canada','United Kingdom')

ggplot() +
  geom_spatvector(data = world.map, fill = "lightgrey", color = NA) +
  geom_spatvector(data = subset(Con_popentrpoul_sf,Con_popentrpoul_sf$EU753%in%nativeCountryName),fill='#baa1a1')+
  coord_sf(crs = "+proj=longlat +datum=WGS84", xlim = c(-160, 165), ylim = c(-56, 90)) +
  theme_bw() + 
  theme(legend.position = "none",
        axis.text = element_text(size=12),
        panel.grid.major = element_blank() ) 


native_moran <- fread('/root/autodl-tmp/pests/result/native_moran_sig.csv')
native_moran$type <- 'native'
non_native_moran <- fread('/root/autodl-tmp/pests/result/non-native_moran_sig.csv')
non_native_moran$type <- 'non_native'
all_moranData <- rbind(native_moran,non_native_moran)
# all_moranData$p <- ifelse(all_moranData$sd_1!=0&all_moranData$sd_2!=0,0.05)

# v <- all_moranData[,.(pest_type_max=pest_type[which.max(moranIndex)],
#                       moranIndex_max=moranIndex[which.max(moranIndex)],
#                       pVal=pvalue[which.max(moranIndex)]),.(tree_name,type,countrName)]
#legend
all_moranData[, class := fcase(
  moranIndex  <= -0.1, "a",
  moranIndex  > -0.1 & moranIndex <=0, "b",
  moranIndex  > 0 & moranIndex<=0.1,"c",
  moranIndex  > 0.1 & moranIndex <=0.2,"d",
  moranIndex  > 0.2 & moranIndex <=0.3,"e",
  moranIndex  > 0.3, "f"
)]
colors_All <- data.table(class=c('a','b','c','d','e','f'),
                         color=c("#2B5C8A","#81B2D4",'#FFD4A5',"#D9705A","#B42D34","#6E0D20"))

all_moranData2 <- left_join(all_moranData,colors_All)
# moranMax <- native_moran[,.(pest_type_max=pest_type[which.max(moranIndex)],
# moranIndex_max=moranIndex[which.max(moranIndex)]),.(tree_name,countrName)]

addPlot <- function(countryName){
  piedata<- all_moranData2[all_moranData2$countrName==countryName,]
  piedata2 <- piedata[,.SD[which.max(moranIndex)],.(tree_name,type,countrName)]
  piedata2$Value<-1
  piedata2$treeType <- paste0(piedata2$tree_name,'_',piedata2$type)
  piedata2$treeType <- factor(piedata2$treeType,levels = c('timber_native','timber_non_native','diversity_native','diversity_non_native','carbon_native','carbon_non_native'))
  
  piedata2_sorted <- piedata2[order(piedata2$treeType), ]
  # color<-c("pop_All"="#6E0D20","pop_CH"="#B42D34","poul_All"="#6E0D20","poul_CH"="#6E0D20","cattle_All"="#FAE3D7","cattle_CH"="#FAE3D7")
  #color_China<-c("pop_All"="lightgrey","pop_CH"="grey","poul_All"="lightgrey","poul_CH"="grey","cattle_All"="lightgrey","cattle_CH"="grey")
  p <- ggplot(piedata2_sorted, aes(x = 2, y = Value, fill = treeType)) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar(theta = "y", start = 0) +
    xlim(0.5, 2.5) + 
    theme_void() +
    theme(legend.position = "none") +
    # labs(fill = "Bird Categories") +
    scale_fill_manual(values = piedata2_sorted$color)
  return(p)
}

nativeCountryName <- c('United States','EU','Australia','Canada','China','United Kingdom')
addPlot('United States')
piedata<- all_moranData2[all_moranData2$countrName=='United States',]
piedata2 <- piedata[,.SD[which.max(moranIndex)],.(tree_name,type,countrName)]

addPlot('Canada')
piedata<- all_moranData2[all_moranData2$countrName=='Canada',]
piedata2 <- piedata[,.SD[which.max(moranIndex)],.(tree_name,type,countrName)]

addPlot('United Kingdom')
piedata<- all_moranData2[all_moranData2$countrName=='United Kingdom',]
piedata2 <- piedata[,.SD[which.max(moranIndex)],.(tree_name,type,countrName)]


addPlot('EU')
piedata<- all_moranData2[all_moranData2$countrName=='EU',]
piedata2 <- piedata[,.SD[which.max(moranIndex)],.(tree_name,type,countrName)]


addPlot('China')
piedata<- all_moranData2[all_moranData2$countrName=='China',]
piedata2 <- piedata[,.SD[which.max(moranIndex)],.(tree_name,type,countrName)]


addPlot('Australia')
piedata<- all_moranData2[all_moranData2$countrName=='Australia',]
piedata2 <- piedata[,.SD[which.max(moranIndex)],.(tree_name,type,countrName)]


#plot
ggplot(all_moranData2, aes(x=countrName, y=moranIndex, fill=moranIndex)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_gradientn(colors=colors_All$color, 
                       # values=rescale(c(-0.1, 0, 0.1, 0.2, 0.3)),
                       name="Moran Index",
                       limits=c(-0.1, 0.3),
                       breaks=c(-0.1, 0, 0.1, 0.2, 0.3),
                       labels=c("< -0.1", "0", "0.1", "0.2", "> 0.3")) +
  theme(text = element_text(size=10),
        legend.position = "bottom",
        legend.background = element_rect(fill="transparent"),
        # legend.title = element_blank()
  )
ggplot(piedata2_sorted, aes(x = 2, y = Value, fill = treeType)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y", start = 0) +
  xlim(0.5, 2.5) + 
  theme_void() +
  theme(legend.position = "none") +
  # labs(fill = "Bird Categories") +
  scale_fill_manual(values = c('#E0E0E0','#757575','#E0E0E0','#757575','#E0E0E0','#757575'))


#######Fig. 4 modelling plot##########
resultPaths <- list.files(paste0('/root/autodl-tmp/pests/',tmpfile),'_.*_importance.csv',full.names = T)
results <- lapply(resultPaths, function(path){
  fread(path)
}) %>% rbindlist()
# results <- fread('/root/autodl-tmp/pests/result/all_importance_results.csv')
results$variable <- results$variable %>%
  str_replace("^wc2\\.1_10m_bio_(\\d+)", "bio_\\1") %>%  
  str_replace("^wc2\\.1_5m_elev$", "elevation") %>%     
  str_replace("^travel_time_to_cities_12", "travel_time") %>%  
  str_replace("^FFI2020", "FFI") %>% 
  str_replace("^ForestAge_TC010", "forest_age") %>% 
  str_replace("^gdp_md", "gdp")%>% 
  str_replace("^LC_Type1", "forest_type")%>% 
  str_replace("^population_density", "population_density") 
# results <- results[results$variable!='bio_15',]

resultPaths_shap <- list.files(paste0('/root/autodl-tmp/pests/',tmpfile),'_.*_shap.csv',full.names = T)
results_shap <- lapply(resultPaths_shap, function(path){
  file_name <- basename(path)
  type <- str_split_i(file_name,'_',1)
  service <- str_split_i(file_name,'_',2)
  df <- fread(path)
  df$type <- type
  df$service <- service
  return(df)
}) %>% rbindlist()
# results <- fread('/root/autodl-tmp/pests/result/all_importance_results.csv')
results_shap$variable <- results_shap$variable %>%
  str_replace("^wc2\\.1_10m_bio_(\\d+)", "bio_\\1") %>%  
  str_replace("^wc2\\.1_5m_elev", "elevation") %>%     
  str_replace("^travel_time_to_cities_12", "travel_time") %>%  
  str_replace("^FFI2020", "FFI") %>% 
  str_replace("^ForestAge_TC010", "forest_age") %>% 
  str_replace("^gdp_md", "gdp") %>% 
  str_replace("^LC_Type1", "forest_type")%>% 
  str_replace("^population_density", "population_density") 
# results_shap <- results_shap[results_shap$variable!='bio_15',]

cvResult <- list.files(paste0('/root/autodl-tmp/pests/',tmpfile),'.*_external_test_metrics.csv',full.names = T)  
# a <- lapply(cvResult,fread) %>% rbindlist()
addPlot <- function(serviceName,titleName){
  native_cvData <- grep(paste0('/native','_',serviceName,'_external_test_metrics.csv'),cvResult,value=T) %>% fread()
  # native_cvData <- native_cvDf[order(-Accuracy)][1]
  
  non_native_cvData <- grep(paste0('non-native','_',serviceName,'_external_test_metrics.csv'),cvResult,value=T) %>% fread()
  # non_native_cvData <- non_native_cvDf[order(-Accuracy)][1]
  
  df <- subset(results,results$service==serviceName)
  df$imp_scaled <- ifelse(df$type=='native',-df$imp_scaled,df$imp_scaled)
  df$importance <- ifelse(df$type=='native',-df$importance,df$importance)
  
  
  shapDf <- subset(results_shap,results_shap$service==serviceName)
  shapDf2 <- shapDf[,.(shap_abs=mean(abs(shap_value))),.(type,variable)]
  shapDf2 <- shapDf2[,shap_scale:=shap_abs/max(shap_abs),.(type)]
  shapDf2$shap_scale <- ifelse(shapDf2$type=='native',-shapDf2$shap_scale,shapDf2$shap_scale)
  shapDf2$shap_abs <- ifelse(shapDf2$type=='native',-shapDf2$shap_abs,shapDf2$shap_abs)
  
  df2 <- left_join(df,shapDf2)
  df_long <- df2 %>%
    tidyr::pivot_longer(
      cols = c(imp_scaled, shap_scale),
      names_to = "importance_type",
      values_to = "importance_value"
    ) 
  max_abs <- max(abs(df_long$importance_value), na.rm  = TRUE) * 1.1 
  df_long$variable <- factor(df_long$variable,
                             levels = c('travel_time','population_density',
                                        'forest_type','gdp','bio_3','bio_12','bio_1','forest_age','FFI'))
  ggplot(df_long, aes(x = importance_value, 
                      y = variable, 
                      group=importance_type,
                      fill =type,
                      alpha=importance_type)) +
    geom_col(width = 0.7,position = position_dodge(width = 0.7)) +
    scale_alpha_manual(values = c(1,0.5))+
    # facet_wrap(~service,nrow = 3,scales = 'free')+
    geom_vline(xintercept = 0, color = "black", size = 0.5) +
    scale_fill_manual(values = c("#1F77B4", "#FF7F0E"),
                      labels = c("native", "non-native")) +
    scale_x_continuous(
      # name = "Percentage",
      # limits = c(-max(importance_value), max(importance_value)),
      # breaks = c(seq(-70, -10, 10), seq(10, 70, 10)), 
      labels = function(x) abs(x)  
    ) +
    coord_cartesian(xlim = c(-max_abs, max_abs))+
    labs(
      title =paste0('Kappa=',floor(native_cvData$Kappa*100)/100,'   ','   ','Kappa=',round(non_native_cvData$Kappa,2)),
      x = "",
      y = "") +
    theme_bw() +
    theme(
      text = element_text(family = "Arial", color = "black"),
      plot.title = element_text(hjust = 0.5, size = 8, face = "bold"),
      axis.line = element_line(color = "black", size = 0.4),
      axis.ticks = element_line(color = "black", size = 0.4),
      axis.text = element_text(color = "black"),
      panel.grid.major.x = element_line(color = "grey90", size = 0.2),
      panel.grid.major.y = element_blank(),
      legend.position = "none",
      # legend.direction = 'horizontal',
      # legend.position.inside = c(0,1),
      # legend.title = element_blank(),
      # legend.key.size = unit(0.2, "cm"),
      # plot.margin = unit(c(-0.1, -0.1, -0.1, -0.1), "cm")
    )
}


timber_bar <- addPlot('timber','Timber')
diversity_bar <- addPlot('diversity','Diversity')
carbon_bar <- addPlot('carbon','Carbon')

timber_bar/diversity_bar/carbon_bar+
  theme(
    axis.title.x.bottom = element_text(size=8),
    # legend.position = 'bottom',
    # legend.direction = 'horizontal',
    # legend.title = element_blank(),
    # legend.key.size = unit(0.3,'cm')
    # axis.title = element_text(face='bold',size=12)
  )


minShap <- min(results_shap$shap_value)
maxShap <- max(results_shap$shap_value)
addShapPlot <- function(typeName, serviceName) {
  shap_data1 <- subset(results_shap, type == typeName & service == serviceName)
  shap_data <- shap_data1[,pct_rank := (dplyr::percent_rank(feature_value)),.(variable)]

  # quantiles <- quantile(shap_data$feature_value1, probs = seq(0, 1, 0.05))
  # shap_data$feature_qt <- cut(shap_data$feature_value1, 
  #                             breaks = quantiles,
  #                             include.lowest = TRUE,
  #                             labels = FALSE)
  
  # meanShap <- shap_data[,.(meanVal=mean(abs(shap_value))),.(variable)]
  shap_data$variable <- factor(shap_data$variable,
                               levels = c('travel_time','population_density',
                                          'forest_type','gdp','bio_3','bio_12','bio_1','forest_age','FFI'))
  ggplot(shap_data, aes(x = variable, y = shap_value)) +
    ggbeeswarm::geom_quasirandom(
      aes(color = pct_rank), 
      size = 0.4,
      width = 0.2,
      dodge.width = 0.9
    ) +
    stat_summary(
      fun = median, geom = "crossbar",
      aes(y = shap_value),
      width = 0.6, fatten = 0, color = "black"
    )+
    # geom_violin(fill = "grey90", color = NA, alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    scale_color_gradient(
      low = "#56B1F7", high = "#CE352E",
      breaks = c(min(shap_data$pct_rank), max(shap_data$pct_rank)),
      labels = c('low', 'high'),
      name = 'Feature value'
    ) +
    scale_y_continuous(
      limits = c(minShap,maxShap)
    ) +
    labs(y = "", x = "") +
    theme_bw() +
    theme(
      legend.position = 'none',
      plot.title = element_text(hjust = 0.5, size = 8, face = "bold"),
      # legend.title.position = 'right',
      # legend.title = element_text(angle = -90, vjust = -1, hjust = 0.5),
      panel.grid.major.y = element_blank(),
      # panel.grid.minor.x = element_blank(),
      axis.title.x = element_text(margin = margin(t = 10)),
      # axis.text.y = element_text(size = 10), 
      axis.text.y = element_blank(),  
      # plot.margin = unit(c(-0.1, -0.1, -0.1, -0.1), "cm"),
      text = element_text(color = "black")
    ) +
    coord_flip()  
}

native_carbon_shap <- addShapPlot('native','carbon')
native_timber_shap <- addShapPlot('native','timber')+labs(title='Native')
native_diversity_shap <- addShapPlot('native','diversity')

native_timber_shap/native_diversity_shap/native_carbon_shap+
  
  theme(
    legend.position = 'none',
    axis.title.x.bottom = element_text(size=8)
  )


non_native_timber_shap <- addShapPlot('non-native','timber')+labs(title='Outside')
non_native_diversity_shap <- addShapPlot('non-native','diversity')
non_native_carbon_shap <- addShapPlot('non-native','carbon')

non_native_timber_shap/non_native_diversity_shap/non_native_carbon_shap+
  theme(
    # axis.title.x.bottom = element_text(size=8)
  )




