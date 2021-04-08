library(rgeos)

# load packages and set directories
pacman::p_load(rnaturalearth,stars,cubelyr, viridis,lubridate,ggthemes,tidyverse)
#pcdir <- "R:/Gill/research/climate-resilience"
#inputdir <- paste0(pcdir, "/Indonesia_NS/Data/")
#outputdir <- paste0(pcdir, "/Indonesia_NS/Data/")
inputdir <- "/Volumes/research/climate-resilience/Indonesia_NS/Data/"
#inputdir <- "/Desktop/Indonesia_climate/climate_resilience/"

# --- read in data ----
#list.files(inputdir)
r = read_stars(paste0(inputdir,"1979-2020_allvariables.nc"))
rairtemp = r$t2m - c
c <- units::as_units(273.15, "K")

#Read in NOAA Degree Heating Weeks
r = read_stars(inputdir,"degreeheating.hdf")
library(rgdal)
library(gdalUtils)
library(raster)
dh <- get_subdatasets(inputdir, "degreeheating.hdf")
