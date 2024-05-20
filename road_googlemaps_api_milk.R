#-------------------------------------------------------#
# Code: googleway package
# 1. maps the routes (milk transportation in Colombia) between two municipalities,
# 2. identify all the municipalities that the route crosses.
# 3. then using the lanslides' municipalities id, create a dataset with the landslides that occurred in the route.
#-------------------------------------------------------#

#--------------------------#
# packages ----
#--------------------------#

rm(list=ls())
packageList<-c("tidyverse", "glue", "googleway", "sf", "readxl", "lubridate",
               "data.table", "haven")
lapply(packageList,require,character.only=TRUE)

#-------------------------------------------------------#
# Example route points ----
# https://cran.r-project.org/web/packages/googleway/vignettes/googleway-vignette.html
#-------------------------------------------------------#

#-------------------------------------------------------#
# route points for milk
#-------------------------------------------------------#

key <- "Enter Your Key Here"

#read origin-destination file for milk prices (crude milk) from code extract_tables_pdf_files.R
milk_ori_dest <- get(load(here::here("00_Data/prices_milk_col/milk_ori_dest.rda")))

# create a list of coordinates by municipality pairs
pair_codmun <- cbind(milk_ori_dest$lat_ori,milk_ori_dest$lon_ori,milk_ori_dest$lat_dest,milk_ori_dest$lon_dest)
coords <- split(pair_codmun , seq(nrow(pair_codmun )))

## create function that will recover the points between two municipalities
pointsroad2 <- function(n){
  
  print(n)
  lat1 <- coords[[n]][1]
  lon1 <- coords[[n]][2]
  lat2 <- coords[[n]][3]
  lon2 <- coords[[n]][4]
  
  df <- google_directions(origin = c(lat1, lon1),
                          destination = c(lat2, lon2),
                          key = key,
                          mode = "driving")
  
  # The data used to draw the route on the map is the overview_polyline. 
  # This string represents a sequence of lat/lon pairs, encoded using a lossy compression 
  # algorithm (https://developers.google.com/maps/documentation/utilities/polylinealgorithm) that allows you 
  # to store the series of coordinates as a single string.
  
  #saveRDS(df, file = here::here("01_Paper_Transport/00_Data/Road_milk_ggway/Road_milk_ggway_df.rds"))
  return(df)
}

# apply the function of finding google maps points between two municipalities
all_points_by_pair <- lapply(seq(1:length(coords)), pointsroad2)
saveRDS(all_points_by_pair, file = here::here("00_Data/Road_milk_ggway/Road_milk_ggway_vfin.rds"))


#----------------------------------------------------#
#  Identify the municipality of the roads points ----
#----------------------------------------------------#

# read shape file with municipalities polygons and identify codmun in route
municipio <- st_read(here::here("00_Data/map_municipios_col/mgn_codmun_d.shp"))


# function
point_to_mun <- function(n){
  print(n)
  if(get("status", all_points_by_pair[[n]]) == "OK"){
  pl <- all_points_by_pair[[n]]$routes$overview_polyline$points
  df <- decode_pl(pl)
  encode_pl(lat = df$lat, lon = df$lon)
  ruta <- st_as_sf(as.data.frame(df), coords = c('lon', 'lat'), crs = 4326)
  ruta_s <- st_transform(ruta, "+proj=merc +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +units=m +no_defs")
  ruta_mun <- st_join(ruta_s ,municipio, left=T) %>% as_tibble(.) %>% select(codmun) %>% unique(.)
  return(ruta_mun)
  }else{ 
    print("error")
    n  = n + 1
  }
}

road_mun <- lapply(c(1:length(coords)), point_to_mun)
saveRDS(road_mun, file = here::here("00_Data/Road_milk_ggway/Road_milk_ggway_mun_unique_vfin.rds"))

# export pairs routes as database 
route <- lapply(c(1:length(road_mun)), function(x){
  d <- cbind(as.data.frame(road_mun[[x]][[1]]),road_mun[[x]][[1]][1],road_mun[[x]][[1]][length(road_mun[[x]][[1]])], as.data.frame(seq(1:length(road_mun[[x]][[1]]))))
  names(d) <- c("codmun","codmun_origen","codmun_destino","orden")
  return(d)
}) %>% rbindlist(.)

saveRDS(route, file = here::here("00_Data/Road_milk_ggway/route.rds"))
write_dta(route, here::here("00_Data/Road_milk_ggway/route.dta"))


#----------------------------------------------------#
#  Derrumbe by route ----
#----------------------------------------------------#

#load data derrumbe consolidated from UNGRD for all years from 1998 to 2022
derrconsolidado <- read_dta(here::here("00_Data/datos_mov_masa/UGRD/UNGRD-2/datos/consolidado_cods_act.dta"))
# View(derrconsolidado)

#select only events were vias (road) where affected.
derrvias <- derrconsolidado %>% 
  filter(vias > 0)
#select only variables of interest for period of interest (2012-2022)
derrvias_slct <- derrvias %>% 
  select(cod_mp, fecha, depto, mnpio, cod_dp,
         departamento, municipio, evento, vias) %>%
  filter(fecha > "2011-12-26")
# dates
derrvias_slct$fecha <- as.Date(derrvias_slct$fecha, format = "%Y-%m-%d")

# load routes
road_mun <- readRDS(here::here("00_Data/Road_milk_ggway/Road_milk_ggway_mun_unique_vfin.rds"))

## Ejemplo
#road_mun[[1]]
#derrvias_slct[which(as.numeric(derrvias$cod_mp) %in% as.data.frame(road_mun[[1]])$codmun == T),]

# Create function
derr_by_road_fct <- function(n){
  print(n)
  from = milk_ori_dest$codmun_origen[n]
  to = milk_ori_dest$codmun_destino[n]
  detect_derr <- which(as.numeric(derrvias_slct$cod_mp) %in% as.data.frame(road_mun[[n]])$codmun == T)
  if(length(detect_derr) != 0){
  d <- derrvias_slct[detect_derr,]
  data_leche_derr <- cbind(from,to,d)
  return(data_leche_derr)
  }else{
    print("no derrumbe")
    return()
  }
}

derr_by_road <- lapply(c(1:length(coords)), derr_by_road_fct)
# derr_by_road[[28]]

#create a dataframe from the list object
data_cons <- rbindlist(derr_by_road)
typeof(data_cons)
data_cons <- data_cons[, -c("departamento","municipio")]

save(data_cons, file = here::here("01_Data_Cleaning/03_derrumbe_by_muni_pairs/data_cons_derr_road_vfin2.rda")) 
write_dta(data_cons, here::here("01_Data_Cleaning/03_derrumbe_by_muni_pairs/data_cons_derr_road_vfin2.dta"))




