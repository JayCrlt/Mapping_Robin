# Setting Environement ####
rm(list=ls()) ; options(mc.cores = parallel::detectCores()) ; CoralReefs_Grid <- read_excel("Data/Bio_Region.xlsx")
library(readxl) ; library(sp) ; library(ggforce) ; library(tidyverse) ; library(rgdal) ; library(patchwork)

# Setting map parameters ####
# warning messages due to an update from PROJ4 to PROJ6. In the meantime, the warning is just a nuisance and has no implications
load(url("https://github.com/valentinitnelav/RandomScripts/blob/master/NaturalEarth.RData?raw=true"))
PROJ <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" 
NE_countries_rob  <- spTransform(NE_countries, CRSobj = PROJ) 
NE_graticules_rob <- spTransform(NE_graticules, CRS(PROJ))
NE_box_rob <- spTransform(NE_box, CRSobj = PROJ) ; prj.coord <- project(cbind(lbl.Y$lon, lbl.Y$lat), proj=PROJ)
lbl.Y.prj <- cbind(prj.coord, lbl.Y) ; names(lbl.Y.prj)[1:2] <- c("X.prj","Y.prj")
prj.coord <- project(cbind(lbl.X$lon, lbl.X$lat), proj=PROJ)
lbl.X.prj <- cbind(prj.coord, lbl.X) ; names(lbl.X.prj)[1:2] <- c("X.prj","Y.prj")

data_Spatial_proj = project(cbind(CoralReefs_Grid$Long_WGS84, CoralReefs_Grid$Lat_WGS84), proj = PROJ) %>% 
  as.data.frame() %>% rename(Long_Robin = V1, Lat_Robin = V2) 
CoralReefs_Grid = CoralReefs_Grid %>% cbind(.,data_Spatial_proj)

col = c("#005f73", "#005f73", "#0a9396", "#0a9396", "#94d2bd", "#94d2bd", "#ee9b00", "#ee9b00", "#ca6702", "#ae2012", "#9b2226")

(Map = ggplot(data = CoralReefs_Grid, aes(x = Long_Robin, y = Lat_Robin, col = New_Cluster)) +
    geom_point(show.legend = F, size = 3) +
    geom_polygon(data=NE_countries_rob, aes(long,lat, group=group), colour="black", fill="gray80", size = 0.25) +
    geom_polygon(data=NE_box_rob, aes(x=long, y=lat), colour="black", fill="transparent", size = 0.25) +
    geom_path(data=NE_graticules_rob, aes(long, lat, group=group), linetype="dotted", color="grey50", size = 0.25) +
    geom_text(data = lbl.Y.prj, aes(x = X.prj, y = Y.prj, label = lbl), color="grey50", size=2) +
    geom_text(data = lbl.X.prj, aes(x = X.prj, y = Y.prj, label = lbl), color="grey50", size=2) +
    geom_mark_hull(aes(fill = New_Cluster, label = Cluster), expand = unit(3, "mm")) +
    scale_fill_manual(values = col) + scale_color_manual(values = col) +
    coord_fixed(ratio = 1) + theme_void() + theme(legend.position="bottom"))
