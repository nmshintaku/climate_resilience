#install.packages('pacman') # run once

# load packages and set directories
pacman::p_load(rnaturalearth,stars,cubelyr, viridis,lubridate,ggthemes,tidyverse)
#pcdir <- "R:/Gill/research/climate-resilience"
#inputdir <- paste0(pcdir, "/Indonesia_NS/Data/")
#outputdir <- paste0(pcdir, "/Indonesia_NS/Data/")
inputdir <- "/Volumes/research/climate-resilience/Indonesia_NS/Data/"
outputdir <- "/Volumes/research/climate-resilience/Indonesia_NS/"
inputdir <- "/Desktop/Indonesia_climate/climate_resilience/"

# --- read in data ----
#list.files(inputdir)
#r = read_stars(paste0(inputdir,"1979-2020_allvariables.nc"))
r = read_stars(paste0(inputdir,"allvar_test.nc"))
r_shww = read_stars("1979-2020_shww.nc")
indonesia <- ne_countries(scale = 110, country = 'indonesia',returnclass = "sf") %>% 
  st_crop(st_bbox(r))
plot(st_geometry(indonesia))


# --- Yearly values ----
# get time values
time.val <- st_get_dimension_values(r, "time")
yr.time.val <- unique(year(st_get_dimension_values(r, "time"))) # get time values in years
yr.time.val

# Aggregate by year
by_t = "1 year"   # set time intervals
r.agg.yr <- aggregate(r, by = by_t, FUN = mean) # average values by year
r.agg.yr <- st_set_dimensions(r.agg.yr, "time", values = yr.time.val, names = "year") # convert date to years
r.agg.yr <- mutate(r.agg.yr, sst = sst-273.15) #convert SST from Kelvin to C
r.agg.yr <- mutate(r.agg.yr, t2m = t2m-273.15) #convert airtemp from Kelvin to C

# Anomaly function
fn_anom <-  function(x) ((x-mean(x, na.rm=T))/sd(x,na.rm=T)) # function to calculate anomalies (in standard deviation units as example)

# Aggregate over all years
r.agg.all.yrs <- st_apply(r.agg.yr, 2:3, fn_anom) # calculate the anomaly over all years (1979-2020)
r.agg.all.yrs <- st_set_dimensions(r.agg.all.yrs, "fn_anom", values = yr.time.val, names = "year") # rename values and band

# Subset to 2009-2019 data
wanted.yrs <- which(yr.time.val%in%c(2009:2019))
r.sub <- r.agg.all.yrs[,wanted.yrs,,] 

#Sample plot SST
ggplot() + 
  geom_stars(data=select(r.sub,sst)) +
  coord_equal() +
  facet_wrap(.~year) +
  scale_fill_viridis(option="D") +
  theme_void() +
  geom_sf(data = indonesia, fill = "gray95") +
  labs(title="sst")

# --- Monthly anomalies ----
# get time values
time.val <- st_get_dimension_values(r, "time") # get time stamps
month.val <- seq(1,length(time.val),12) # pull out the Januarys
month.val
yr.time.val <- unique(year(st_get_dimension_values(r, "time"))) # get time values in years

# example
r.jan <- r[,,,month.val]
st_get_dimension_values(r.jan, "time") # see if it worked

# Anomaly function
fn_anom <-  function(x) ((x-mean(x, na.rm=T))/sd(x,na.rm=T)) # function to use
r.jan <- st_apply(r[,,,month.val], 1:2, fn_anom) # calculate the anomaly over all years
r.jan <- st_set_dimensions(r.jan, "fn_anom", values = paste0("1-", yr.time.val), names = "month") # rename values and band

# Subset to 2009-2019 data
wanted.yrs <- which(yr.time.val%in%c(2010:2020))
r.jan.sub <- r.jan[,wanted.yrs,,] 

#Sample plot
ggplot() + 
  geom_stars(data=select(r.jan.sub,si10)) +
  coord_equal() +
  facet_wrap(.~month) +
  scale_fill_viridis_c(option = "D")+
  theme_void() +
  geom_sf(data = indonesia, fill = "gray95") +
  labs(title="Si10 - January")


# write a function to do the other months (for loop)
comb.sub <- r.jan.sub
for (i in 1:11){
  print(i)
  dat <- st_apply(r[,,,month.val + i], 1:2, fn_anom) # calculate the anomaly over all years
  dat <- st_set_dimensions(dat, "fn_anom", values =  paste0(i+ 1,"-", yr.time.val), names = "month-year") # convert date to years
  dat <- dat[,wanted.yrs,,] 
  comb.sub <- c(comb.sub,dat, along = 1)
} 

st_get_dimension_values(comb.sub, "month")

#Sample plot SST
ggplot() + 
  geom_stars(data=select(comb.sub,sst)) +
  coord_equal() +
  facet_wrap(.~month, ncol = 11) +
  scale_fill_viridis_c(option = "D")+
  theme_void() +
  geom_sf(data = indonesia, fill = "gray95") +
  labs(title="SST", x="year", y="month")
ggsave(paste0(outputdir,'STT_monthly.png'),width = 12,height = 16)


# -----Process wind wave height separately because the all variables file is missing data ---
r_shww = read_stars("1979-2020_shww.nc")
#Year values 
time.val.shww <- st_get_dimension_values(r_shww, "time")
yr.time.val.shww <- unique(year(st_get_dimension_values(r_shww, "time"))) # get time values in years
yr.time.val.shww

# Aggregate by year
by_t = "1 year"   # set time intervals
r.agg.yr.shww <- aggregate(r_shww, by = by_t, FUN = mean) # average values by year
r.agg.yr.shww <- st_set_dimensions(r.agg.yr.shww, "time", values = yr.time.val.shww, names = "year") # convert date to years

# Anomaly function
fn_anom <-  function(x) ((x-mean(x, na.rm=T))/sd(x,na.rm=T)) # function to calculate anomalies (in standard deviation units as example)

# Aggregate over all years
r.agg.all.yrs.shww <- st_apply(r.agg.yr.shww, 2:3, fn_anom) # calculate the anomaly over all years (1979-2020)
r.agg.all.yrs.shww <- st_set_dimensions(r.agg.all.yrs.shww, "fn_anom", values = yr.time.val.shww, names = "year") # rename values and band

# Subset to 2009-2019 data
wanted.yrs <- which(yr.time.val%in%c(2009:2019))
r.sub.shww <- r.agg.all.yrs.shww[,wanted.yrs,,] 

ggplot() + 
  geom_stars(data=r.sub.shww) +
  coord_equal() +
  facet_wrap(.~year) +
  scale_fill_viridis(option="D") +
  theme_void() +
  geom_sf(data = indonesia, fill = "gray95") +
  labs(title="Wave Height")

# Month values
time.val <- st_get_dimension_values(r_shww, "time") # get time stamps
month.val <- seq(1,length(time.val),12) # pull out the Januarys
month.val
yr.time.val <- unique(year(st_get_dimension_values(r_shww, "time"))) # get time values in years

# example
r.jan <- r[,,,month.val]
st_get_dimension_values(r.jan, "time") # see if it worked

# Anomaly function
fn_anom <-  function(x) ((x-mean(x, na.rm=T))/sd(x,na.rm=T)) # function to use
r.jan <- st_apply(r_shww[,,,month.val], 1:2, fn_anom) # calculate the anomaly over all years
r.jan <- st_set_dimensions(r.jan, "fn_anom", values = paste0("1-", yr.time.val), names = "month") # rename values and band

# Subset to 2009-2019 data
wanted.yrs <- which(yr.time.val%in%c(2010:2020))
r.jan.sub <- r.jan[,wanted.yrs,,] 

# write a function to do the other months (for loop)
comb.sub <- r.jan.sub
for (i in 1:11){
  print(i)
  dat <- st_apply(r_shww[,,,month.val + i], 1:2, fn_anom) # calculate the anomaly over all years
  dat <- st_set_dimensions(dat, "fn_anom", values =  paste0(i+ 1,"-", yr.time.val), names = "month-year") # convert date to years
  dat <- dat[,wanted.yrs,,] 
  comb.sub <- c(comb.sub,dat, along = 1)
} 

st_get_dimension_values(comb.sub, "month")
#plot shww
ggplot() + 
  geom_stars(data=comb.sub) +
  coord_equal() +
  facet_wrap(.~month, ncol = 11) +
  scale_fill_viridis_c(option = "D")+
  theme_void() +
  geom_sf(data = indonesia, fill = "gray95") +
  labs(title="Wind Wave Height", x="year", y="month", fill = "shww")
ggsave(paste0(outputdir,'shww_month.png'),width = 12,height = 16)

# ---- time series plots ----
yr.mean <- st_apply(r.agg.yr, "year", mean, na.rm=T) # mean of all pixels for each band

yr.mean <-merge(yr.mean)  # merge variables into a dimension
yr.mean # take a look

yr.mean.avg <- yr.mean %>% 
                  as.tbl_cube() %>%
                  group_by(year,attributes) %>%
                  summarise(val=mean(X,na.rm = T)) %>% 
                  # summarise_at(c("si10", "tp"), mean, na.rm = TRUE) %>% 
                  #summarise_at(vars(si10:tp), mean, na.rm = TRUE) %>% 
                  as.data.frame()
head(yr.mean.avg)

yr.mean.avg <- yr.mean.avg %>% 
  ungroup() %>% 
  group_by(attributes) %>% 
  mutate(avg=mean(val),sd.val=sd(val))
yr.mean.avg

ggplot(yr.mean.avg, aes(x=year, y=val)) +
  geom_point(size=2, shape=23) +
  geom_line()+
  facet_wrap(.~attributes, scales = "free_y") +
  geom_line(aes(y=avg), linetype="dashed", col="blue") +
  geom_smooth(method=lm, se=T, 
              color="darkred") +
  theme_classic()
ggsave(paste0(outputdir,'time_series.png'),width = 10,height = 7)


