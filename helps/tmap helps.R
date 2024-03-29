
library(tidyverse)
library(sf)
library(tmap)
library(tmaptools)

tmap_mode('plot')

## change bounding box based on another layer----
#get bounding box of bathy
bbox_sunapee <- st_bbox(sun_bathy) # current bounding box

xrange <- bbox_sunapee$xmax - bbox_sunapee$xmin # range of x values
yrange <- bbox_sunapee$ymax - bbox_sunapee$ymin # range of y values

#create a new one and modify
bbox_sun_new <- st_bbox(sun_bathy) 
bbox_sun_new[1] <- bbox_sun_new[1] - (0.25 * xrange) # xmin - left
bbox_sun_new[3] <- bbox_sun_new[3] + (0.25 * xrange) # xmax - right
bbox_sun_new[2] <- bbox_sun_new[2] - (0.075 * yrange) # ymin - bottom
bbox_sun_new[4] <- bbox_sun_new[4] + (0.025 * yrange) # ymax - top

## make legend symbol fill white when color is faceted ---

tm_shape(cond_summary) +
  tm_symbols(size = 'mean_cond_uScm',
             col = 'site_type',
             shape = 21,
             title.size = 'average summer\nconductivity\n(uS/cm)',
             border.col = 'black',
             scale = 3,
             shapes.legend.fill = 'white') + ### this will make the fill white (or whatever color)
  tm_layout(legend.outside = T,
            legend.title.fontface = 'bold',
            legend.title.size = 1.5,
            legend.text.size = 1,
            title = 'Average Summer\nConductivity\n(Jun-Sept,\n2010-2020)\n ',
            title.fontface = 'bold')


## multiple plots in one frame ----

#make sunapee basemap 
#play with extent for legends to fit
sunbase = tm_shape(sun_bathy, bbox = bbox_sun_new) +
  tm_raster(palette = 'Blues',
            title = 'lake depth\n(meters)') +
  tm_shape(sun) +
  tm_borders() +
  tm_shape(sampling_georef) +
  tm_dots(col = 'frequency', 
          shape = 24,
             size = 0.5,
             title = 'sampling\nfrequency',
          palette = c('black', 'yellow')) +
  tm_compass(position = c('right', 'bottom')) +
  tm_scale_bar(position = c('right', 'bottom')) +
  tm_layout(frame.lwd = 3, frame = 'blue',
            legend.position = c('left', 'bottom'))
sunbase


#make North America

# NE focus
#adjust bounding to ~ new england
#get existing bounding box
bbox_existing <- st_bbox(northamer) # current bounding box
xrange <- bbox_existing$xmax - bbox_existing$xmin # range of x values
yrange <- bbox_existing$ymax - bbox_existing$ymin # range of y values

#create a new one and modify
bbox_NE <- st_bbox(northamer) 
bbox_NE[1] <- bbox_NE[1] + (0.29 * xrange) # xmin - left
bbox_NE[3] <- bbox_NE[3] - (0.69 * xrange) # xmax - right
bbox_NE[2] <- bbox_NE[2] + (0.40 * yrange) # ymin - bottom
bbox_NE[4] <- bbox_NE[4] - (0.49 * yrange) # ymax - top

#transform into polygon
bbox_NE <- bbox_NE %>%  
  st_as_sfc() 

# NA with better focus
NE_base = tm_shape(NA_wstates, bbox = bbox_NE) +
  tm_polygons() +
tm_shape(northamer) +
  tm_borders(lwd = 2)+
  tm_scale_bar(position = c('right', 'bottom')) +
tm_shape(bbox_sunapee) +
  tm_borders(lwd = 3, col = 'blue') +
  tm_layout(frame.lwd = 3, frame = 'red')
NE_base


#adjust bounding to ~ mexico/west coast
bbox_NA <- st_bbox(northamer) # current bounding box

bbox_NA[1] <- bbox_NA[1] + (0.13 * xrange) # xmin - left
bbox_NA[3] <- bbox_NA[3] - (0.65 * xrange) # xmax - right
bbox_NA[2] <- bbox_NA[2] + (0.15 * yrange) # ymin - bottom
bbox_NA[4] <- bbox_NA[4] - (0.32 * yrange) # ymax - top

bbox_NA <- bbox_NA %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

# NA with better focus
NA_base = tm_shape(NA_wstates, bbox = bbox_new) +
  tm_polygons() +
tm_shape(northamer) +
  tm_borders(lwd = 2) +
  tm_scale_bar(position = c('right', 'bottom')) +
tm_shape(bbox_NE) +
  tm_borders(lwd = 3, col = 'red')
NA_base


#put it all together
png(file = file.path(fig_dir, 'Fig2_DataExtent.png'),
                     width = 6,
                     height = 6,
    units = 'in',
    res = 300)
print(NA_base, vp = grid::viewport(0.22, 0.847, width= 0.4))
print(NE_base, vp = grid::viewport(0.22, 0.342, width = 0.4))
print(sunbase, vp = grid::viewport(0.7, 0.5, height = 1))
dev.off()
