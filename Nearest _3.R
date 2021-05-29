library(sp)
library(sf)
library(tidyr)
library(nngeo)
library(dplyr)

#read files 
mp= read.csv("nearest.csv")%>%
  select(Health_facility._name, Latitude,Longitude)
h = read.csv("Households.csv")


#convert to spatial points
coordinates(mp) <- ~Longitude + Latitude
coordinates(h) <- ~Longitude + Latitude

#convert to sf
msf <- st_as_sf(mp)
st_crs(msf) = "+init=epsg:4326"
msf <- st_transform(msf, crs = 3857)
st_crs(msf)
class(msf)

hsf <- st_as_sf(h)
st_crs(hsf)= "+init=epsg:4326"
hsf <- st_transform(hsf, crs = 3857)
st_crs(hsf)


#find nearest 3 distances from households to health facilities
Nearest_5 <- st_nn(hsf,msf, returnDist = TRUE, k=3)


#convert result to dataframe and select distances from attribute
Nearest_5.df <- as.data.frame(t(do.call(rbind.data.frame,Nearest_5)))
finalnearest <- st_join(hsf,msf,st_nn, k=3)


final = finalnearest%>%
  group_by(Household)%>%
  mutate(Nearest_5 = seq_along(Health_facility._name))%>%
  spread(key = Nearest_5, value = Health_facility._name)


#merge to attach distance
distance_final <- cbind(final, Nearest_5.df)



#select needed columns
distance_final <- distance_final%>%
  select(X, Y ,Household , X1, X2, X3,dist.4, dist.5, dist.6, geometry)


#rename columns
rownames(distance_final) = NULL
colnames(distance_final) = c('Longitude', 'Latitude' ,'Household', 'First Nearest Health Facility', 'Second Nearest Health Facility', 'Third Nearest Health Facility', 'First Nearest Facility Distance', 'Second Nearest Facility Distance', 'Third Nearest Facility Distance', 'geometry')


# write to csv
write.csv(distance_final, file = "Three nearest health facilities.csv", row.names = FALSE )


