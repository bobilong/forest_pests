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
globalRaster <- rast(vals=NA,nrows=180, ncols=360,xmin=-180, xmax=180,ymin=-90, ymax=90,crs=crs)
continentPolygon <- vect('/root/autodl-tmp/worldBorder/World_Continents.shp')
coast <- rnaturalearth::ne_coastline(scale = "small", returnclass = "sf") %>% vect()
`%notin%` <- Negate(`%in%`)

pal2 <-paletteer_d("colorBlindness::Blue2DarkOrange12Steps")
pal <- paletteer_dynamic("cartography::orange.pal", 20)

######fig1 pest pattern ################
#all pests
allTif <- list.files('pests/forest_pests/data/',full.names = T)

patternMap <- function(typeName,tagName1,tagName2){
  all_r <- grep(paste0(typeName,'_all.tif'),allTif,value=T) %>% rast() %>% resample(globalRaster,'near')
  all_r2 <- ifel(all_r>0,all_r,NA)
  r_cap <- ifel(all_r2 > 12, 12, all_r2)
  
  p1 <- ggplot() +
    geom_spatraster(data = r_cap) +
    geom_spatvector(data=continentPolygon,fill='grey70',alpha=0.2)+
    coord_sf(crs = crs,xlim=c(-160,163.5),ylim=c(-56,83))+
    theme_bw()+
    scale_fill_gradientn(
      colours = pal,
      limits  = c(1, 12),          
      # oob     = squish,
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
      # panel.background = element_rect(fill = "grey90"),
      # plot.background  = element_rect(fill = "grey90"),
      legend.text =  element_text(size = 12),
      legend.title =  element_text(hjust = -1,size = 12),
      #legend.title = element_text(hjust=0.5),
      # legend.title.align = -10,
      plot.caption = element_text(size = 15,face = 'bold'),
      axis.text = element_text(size = 12,face = 'bold'),
      # legend.position = 'none',
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
    geom_spatvector(data=continentPolygon,fill='grey70',alpha=0.2)+
    coord_sf(crs = crs,xlim=c(-160,163.5),ylim=c(-56,83))+
    theme_void()+
    guides(
      fill=guide_colorbar('Within to outside  ')
    )+
    scale_fill_gradientn(colours = pal2,
                         # labels=scales::label_number(accuracy = 1),
                         # transform='log10',
                         # limits=c(1,108),
                         na.value='white')+
    labs(tag  =tagName2)+
    theme_bw()+
    theme(
      # plot.tag.position = c(0.02, 0.98),
      plot.title = element_text(hjust=0.5),
      plot.caption = element_text(size = 15,face = 'bold'),
      axis.text = element_text(size = 12,face = 'bold'),
      # panel.background = element_rect(fill = "grey90"),
      # plot.background  = element_rect(fill = "grey90"),
      legend.text =  element_text(size = 12),
      legend.title =  element_text(size = 12),
      # legend.title.align = -10,
      # legend.position = 'none',
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
