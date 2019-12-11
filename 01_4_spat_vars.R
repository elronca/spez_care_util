
## Calculating driving times to the nearest specialist center/ outpatient clinic

library(tidyverse)
library(sf)
library(ggspatial)
library(ggmap)
library(mapview)
library(googleway)

mapviewOptions(basemaps = c("OpenStreetMap", "Esri.WorldImagery"))





# Identify places of highest population density in each Medstat region --------

# This place will serve as population centroid





# Read Medstat polygons

MS_spdf <- st_read(file.path("data", "MedStat_GIS", "MedStat_Base_2011_region.shp"), 
                   as_tibble = TRUE, options = "ENCODING=WINDOWS-1252",
                   stringsAsFactors = FALSE) %>% 
  filter(KT != "FL")


# Read number of persons by hectare

pop_hectare <- read_csv2(file.path("data", "STATPOP2012B.csv"), col_types = cols_only(X_KOORD = "d", Y_KOORD = "d", B12BTOT = "i"))


# Hectares with 1 to 3 inhabitants are coded with 3 inhabitans for data privacy reasons.
# To get a more appropirate picture of real the number of inhabitants we replace every 3 with a random number of 1 to 3.

set.seed(84)

pop_hectare <- mutate(pop_hectare, B12BTOT = if_else(B12BTOT == 3L, sample(1L:3L, n(), replace = TRUE), as.integer(B12BTOT)))


# Retrieve coordinate reference system from Medstat spatial polygon

crs_MS_spdf <- st_crs(MS_spdf)[["proj4string"]]


# Transform coordinates of persons per hectare file into spatial points using coordinates of Medstat spatial polygon file

pop_hectare_sp <- st_as_sf(x = pop_hectare, coords = c("X_KOORD", "Y_KOORD"), crs = crs_MS_spdf) %>% 
  select(B12BTOT, geometry)


# Get a sample of the very large persons per hectare spatial points file

pop_hectare_sp_sample <- pop_hectare_sp[sample(nrow(pop_hectare_sp), 10000), ]


# Plot points into Medstat polygons (they fit nicely into the polygons)
# Hence we use the same coordinates system for the medstat regions and the plot

plot(st_geometry(MS_spdf), col = "yellow")
plot(st_geometry(pop_hectare_sp_sample), type = "p", size = 0.1, col = "blue", add = TRUE)

# If the blue points are located on the yellow map of Switzerland the coordinates system tranformation worked well

rm(pop_hectare_sp_sample)



# which points (hectares and related numbers of people) fall inside which polygon (Medstat regions)

intersects <- st_intersects(MS_spdf, pop_hectare_sp)


# Using prepared data to find the center of the Medstat shapefiles --------

get_pop_centroids <- function(i, my_intersects, my_crs, pop_dens_file, map_file) {
  
  pb$tick()$print()
  
  # i = the nth Medstat region
  
  name_MS_region_i <- map_file %>% pull(MEDSTAT04) %>% pluck(i)
  
  intersects_region_i <- my_intersects %>% pluck(i)
  
  coords_MS_region_i <- pop_dens_file %>% 
    slice(intersects_region_i) %>% 
    st_coordinates() %>% 
    as_tibble()
  
  B12BTOT <- pop_dens_file %>% 
    slice(intersects_region_i) %>% 
    pull(B12BTOT)
  
  coords_pop <- coords_MS_region_i %>% 
    add_column(B12BTOT = B12BTOT)
  
  center_most_pop_place <- coords_pop %>% 
    filter(B12BTOT == max(B12BTOT)) %>% 
    filter(row_number() == 1) %>% 
    st_as_sf(coords = c("X", "Y"), crs = my_crs)
  
}


# Run function over all MedStat regions

if(F) {
  
  pb <- progress_estimated(nrow(MS_spdf))
  
  my_coordinates <- map(.x = seq_len(nrow(MS_spdf)), .f = get_pop_centroids, 
                        my_intersects = intersects, 
                        my_crs = crs_MS_spdf, 
                        pop_dens_file = pop_hectare_sp, 
                        map_file = MS_spdf)
  
  names(my_coordinates) <- MS_spdf$MEDSTAT04
  
  save(my_coordinates, file = file.path("workspace", "my_coordinates.Rdata"))
  
}

load(file.path("workspace", "my_coordinates.Rdata"))


# Extract the coordinates of the Medstat centroids

MS_coordinates <- do.call(rbind, my_coordinates) %>% 
  add_column(MedStat = names(my_coordinates), .before = 1) %>% 
  st_transform("+proj=longlat +datum=WGS84") %>% 
  select(-B12BTOT)

MS_coordinates <- MS_coordinates %>% 
  add_column(lon = st_coordinates(MS_coordinates)[, "X"]) %>% 
  add_column(lat = st_coordinates(MS_coordinates)[, "Y"])



# Calculate the driving times from Medstat to specialized centers/Clinics ---------


# Prepare the data of each center

SCI_centers <- tibble(
  place = c("Plein Soleil", "SPZ", "Crr Suva", "Balgrist", "REHAB", "Ente Ospedaliero Cantonale"), 
  street = c("Ch. de la Cigale 3", 
             "Guido A. Zäch Str. 1", 
             "Avenue du Grand-Champsec 90", 
             "Forchstrasse 340",
             "Im Burgfelderhof 40",
             "Viale Officina 3"),
  PLZ = c(1010, 6207, 1950, 8008, 4055, 6500),
  Ort =  c("Lausanne", "Nottwil", "Sion", "Zürich", "Basel", "Bellinzona")) %>%
  
  mutate(center_addr_string = str_c(street, PLZ, Ort, "Schweiz", sep = " + "))

if(F) {
  
  ggmap::register_google("AIzaSyATu6r_ZkcS672g5B9T9HqdFktaKN_shhk")
  
  cent_coordinates <- geocode(SCI_centers$center_addr_string, output = "latlon", source = "google")
  save(cent_coordinates, file = file.path("workspace", "cent_coordinates.RData"))
  
}


load(file.path("workspace", "cent_coordinates.RData"))

cent_addresses <- bind_cols(SCI_centers, cent_coordinates)
cent_addresses.sp <- st_as_sf(x = cent_addresses, coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84")

mapview(cent_addresses.sp)

FL00 <- tibble(MedStat = "FL00", 
               lon = 9.520915, lat = 47.141091) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84") %>% 
  add_column(lon = 9.520915, lat = 47.141091)

MS_coordinates <- rbind(MS_coordinates, FL00)

tail(MS_coordinates)


# Combine the medstat and the centers data to prepare a file for the calculation of the driving times

n_rows_MS <- nrow(MS_coordinates)
n_rows_cent <- nrow(cent_addresses)

MS_coordinates_cent <- MS_coordinates %>% 
  as_tibble() %>% 
  uncount(n_rows_cent) %>% 
  rename(lon_MS = lon, lat_MS = lat)

cent_addresses <- do.call("rbind", replicate(n_rows_MS, cent_addresses, simplify = FALSE)) %>% 
  rename(lon_cent = lon, lat_cent = lat)

dist_cent <- bind_cols(MS_coordinates_cent, cent_addresses)

if(F) {
  
  
  # We calculate the driving times using the google directions API ----------
  
  google_distance_delayed <- function(x, y) {
    
    pb$tick()$print()
    
    Sys.sleep(0.5)
    
    key <- "AIzaSyATu6r_ZkcS672g5B9T9HqdFktaKN_shhk"
    
    drv_dist <- googleway::google_distance(
      origins = x, 
      destinations = y, 
      key = key, 
      mode = "driving")
    
    # From some places like Zermatt it is not possible to drive by car, therefore we calculate the 
    # travel distance by public transport
    
    if(unlist(drv_dist$rows$elements)["status"] == "ZERO_RESULTS") {
      
      drv_dist <- google_distance(origins = x, destinations = y, key = key, 
                                  mode = "transit")
      
    }
    
    drv_dist_df <- data.frame(
      
      origin = drv_dist$origin_addresses,
      destination = drv_dist$destination_addresses,
      
      lapply(distance_elements(drv_dist), function(x) {
        
        data.frame(
          duration = mean(x[["duration"]][["value"]], na.rm = TRUE),
          distance = mean(x[["distance"]][["value"]], na.rm = TRUE)
        )
        
      }
      ) %>% bind_rows()
      
      , stringsAsFactors = FALSE)
    
    return(drv_dist_df)
    
  }
  
  #!! Take care, if travel time by car cannot be calculated as the place is not reachable by car
  #!! then the traval time by public transport is being calculated. For this, the time now is used
  #!! this means that if at the time when this script is run there are no train connections, then the 
  #!! travel times will be incorrect.
  
  
  # Save the coordinates of the Medstat regions as well of the center in a list
  
  cent_distances <- list()
  
  cent_distances[["MS_lat_lon"]] <- dist_cent %>% 
    select(lat_MS, lon_MS) %>% 
    split(seq(nrow(.))) %>% 
    set_names(dist_cent$MedStat) %>% 
    map(unlist)
  
  cent_distances[["cent_lat_lon"]] <- dist_cent %>% 
    select(lat_cent, lon_cent) %>% 
    split(seq(nrow(.))) %>% 
    set_names(dist_cent$MedStat) %>% 
    map(unlist)
  
  
  # Calculate the distances using the coordinates
  
  pb <- progress_estimated(length(cent_distances[["MS_lat_lon"]]))
  
  drv_dist_cent <- map2_dfr(
    cent_distances[["MS_lat_lon"]], 
    cent_distances[["cent_lat_lon"]], 
    google_distance_delayed) %>% 
    
    mutate(duration_min = duration / 60, distance_km = distance / 1000)
  
  save(drv_dist_cent, file = file.path("workspace", "drv_dist_centers.RData"))
  
}

load(file.path("workspace", "drv_dist_centers.RData"))



# This is the file with the driving times from each Medstat region to each of the centers/clinics

driving_times <- bind_cols(dist_cent, drv_dist_cent) %>% 
  select(MedStat, place, duration_min)



# Calculate the Sprachregion of each Medstat region -----------------------

# Get file

lang_reg_sp <- st_read(file.path("data", "Sprachregionen", "c_shp","K4sprg20170101gf_ch2007Poly.shp"), 
                       as_tibble = TRUE, options = "ENCODING=WINDOWS-1252",
                       stringsAsFactors = FALSE)


# Extract the coordinates of the Medstat centroids

coordinates.sp <- do.call(rbind, my_coordinates) %>% 
  add_column(MedStat = names(my_coordinates), .before = 1) %>% 
  st_transform(st_crs(lang_reg_sp)) %>% 
  select(-B12BTOT)

lang_reg <- st_intersects(coordinates.sp, lang_reg_sp) %>% 
  as_tibble() %>% 
  set_names(c("ORDNUM", "language")) %>% 
  mutate(language = fct_recode(as.factor(language), 
                               German = "1", 
                               French = "2" , 
                               Italian = "3", 
                               German = "4"))


# Create dataset spatial variables ----------------------------------------

spatial_vars <- st_read(file.path("data", "MedStat_GIS", "MedStat_Base_2011_region.shp"), 
                        as_tibble = TRUE, options = "ENCODING=WINDOWS-1252",
                        stringsAsFactors = FALSE) %>% 
  st_drop_geometry() %>% 
  select(ORDNUM, MEDSTAT04) %>% 
  full_join(driving_times, by = c("MEDSTAT04" = "MedStat")) %>% 
  full_join(lang_reg, by = "ORDNUM")



# Degree of urbanization -----------------------------------------------------------------

# https://ec.europa.eu/eurostat/de/web/gisco/geodata/reference-data/population-distribution-demography/degurba

# There is newer information from 2018, however it is lacking a few regions

degurba_Europe_18 <- st_read(file.path("data", "degurba_2018", "DGURBA_2018_01M.shp"),
                             as_tibble = TRUE, options = "ENCODING=UTF-8",
                             stringsAsFactors = FALSE)

degurba_CH_18 <- degurba_Europe_18 %>% filter(str_detect(NUTS, "CH"))

medstat_missing_degurba_18 <- st_transform(degurba_CH_18, st_crs(coordinates.sp)) %>% 
  st_contains(coordinates.sp) %>% 
  as_tibble() %>% 
  right_join(tibble(medstat = 1:705), by = c("col.id" = "medstat")) %>% 
  filter(is.na(row.id)) %>% pull(col.id) %>% coordinates.sp[.,] %>% pull(MedStat)


# Identification of missing degurba informatiion for medstat regions

mapview(list(degurba_CH_18, coordinates.sp %>% filter(MedStat %in% medstat_missing_degurba_18)),
        layer.name = c("degurba", "missing Medstats"))


# The older information seems to be more complete


degurba_Europe_14 <- st_read(file.path("data", "degurba_2014", "DGURBA_RG_01M_2014.shp"), 
                             as_tibble = TRUE, options = "ENCODING=UTF-8",
                             stringsAsFactors = FALSE)


degurba_CH_14 <- degurba_Europe_14 %>% filter(str_detect(CNTR_CODE, "CH"))

medstat_missing_degurba_14 <- st_transform(degurba_CH_14, st_crs(coordinates.sp)) %>% 
  st_contains(coordinates.sp) %>% 
  as_tibble() %>% 
  right_join(tibble(medstat = 1:705), by = c("col.id" = "medstat")) %>% 
  filter(is.na(row.id)) %>% pull(col.id) %>% coordinates.sp[.,] %>% pull(MedStat)



# The coordinates of the regions AG27 and TG13 seem to be shifted to only a little bit too far
# north. We have to move the coordinates a bit so that they fall into the degurba polygons

mapview(list(degurba_CH_14, coordinates.sp %>% filter(MedStat %in% medstat_missing_degurba_14)),
        layer.name = c("degurba", "missing Medstats"))


# AG27: POINT (673700 269100)
st_geometry(coordinates.sp[coordinates.sp$MedStat == "AG27", ]) <- st_sfc(st_point(c(673700 , 269100 - 100)))

# TG13: POINT (698400 282900)
st_geometry(coordinates.sp[coordinates.sp$MedStat == "TG13", ]) <- st_sfc(st_point(c(698400 , 282900 - 100)))

mapview(list(degurba_CH_14, coordinates.sp %>% filter(MedStat %in% medstat_missing_degurba_14)),
        layer.name = c("degurba", "missing Medstats"))



# Now we see in what degurba polygons the coordinates fall to derive the degree of urbanization

matched_regions <- st_transform(degurba_CH_14, st_crs(coordinates.sp)) %>% 
  st_contains(coordinates.sp) %>% 
  as_tibble() %>% 
  rename(rows_degurba = "row.id", rows_coordinates_ms = "col.id")


# Here we get the degurba classification for each region

degurba <- bind_cols(
  coordinates.sp[matched_regions$rows_coordinates_ms,], 
  degurba_CH_14[matched_regions$rows_degurba,]) %>% 
  arrange(MedStat) %>% 
  select(MedStat, DGURBA_CLA) %>% 
  mutate(degurba = fct_recode(as.factor(DGURBA_CLA), urban = "1", suburban = "2", rural = "3"))


# Now we compare the old degree of urbanization classification with the newer one

degurba_old <- read.csv2(file.path("data", "Urbanity_Lang_SwiSCI12.csv"), stringsAsFactors = F) %>% 
  mutate(degurba_old = fct_recode(as.factor(DegreeUrban), urban = "City", suburban = "Agglomeration", rural = "Rural area"))

left_join(degurba, select(degurba_old, MedStat, degurba_old), by = "MedStat") %>% 
  mutate(diff_dgurb = if_else(as.character(degurba) != as.character(degurba_old), 1, 0)) %>% 
  mutate(my_legend = if_else(diff_dgurb == 1, str_c(MedStat, degurba , "<-", degurba_old, sep = " "), as.character(degurba))) %>% 
  mapview(zcol = "degurba", label = .$my_legend)

spatial_vars <- left_join(spatial_vars, select(degurba, MedStat, degurba), by = c("MEDSTAT04" = "MedStat"))

spatial_vars <- select(spatial_vars, -geometry)

save(spatial_vars, file = file.path("workspace", "spatial_vars.RData"))

rm("cent_addresses", "cent_addresses.sp", "cent_coordinates", 
   "coordinates.sp", "crs_MS_spdf", "degurba", "degurba_CH_14", 
   "degurba_CH_18", "degurba_Europe_14", "degurba_Europe_18", "degurba_old", 
   "dist_cent", "driving_times", "drv_dist_cent", "FL00", "get_pop_centroids", 
   "intersects", "lang_reg", "lang_reg_sp", "matched_regions", "medstat_missing_degurba_14", 
   "medstat_missing_degurba_18", "MS_coordinates", "MS_coordinates_cent", 
   "MS_spdf", "my_coordinates", "n_rows_cent", "n_rows_MS", "pop_hectare", 
   "pop_hectare_sp", "SCI_centers", "spatial_vars")
