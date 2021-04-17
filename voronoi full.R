### Voronoi Diagram Code

# Make sure the following packages have been installed before you run library()
library(tidyverse)
library(ggplot2)
library(ggvoronoi)
library(cholera)
library(HistData)


#------------------------------------------------------------------------------------------------------------------#

### Cholera plots - using the cholera package

# London cholera maps, landmarks will be too small to read in the video so omit
snowMap(add.pumps = FALSE, add.cases = FALSE)
# Add water pumps
snowMap(add.cases=FALSE)
# Add deaths - start with the map having just water pumps or just deaths maybe?
snowMap()
# Add divisions to map (Voronoi diagram?) - other method doesn't change pumps' label
#colour, avoids covering p7, but only colours the first case
plot(neighborhoodVoronoi())
# Combining addVoronoi() and snowMap() also produced a Voronoi diagram
#addVoronoi()
# Point out broad street pump
streetHighlight("Broad Street")

### Cholera plots - using the HistData package

# Streets plotted only
Splot()
Sstreets()
#Add deaths
Sdeaths(pch = 0, col="indianred1")
# Add water pumps - added after deaths to make the street names more visible
Spumps(cex.lab = 1)
#Add Voronoi divisions
Spolygons(border = "black")

#------------------------------------------------------------------------------------------------------------------#

### Function to produce a Voronoi diagram of the pubs in Bath

voronoi_pub<-function(title=FALSE, simple=FALSE, pubs.simple=FALSE) {

  # Bath pubs coordinates
  
  # Take coordinates from Google maps for the city centre pubs and create a 
  #starting Voronoi diagrams
  # Input latitude and longitude coordinates with pub names 1 row at a time
  belvoir_castle<-c(51.382095122857734, -2.3766680092976946, "Belvoir Castle")
  victoria_pub_kit<-c(51.38563010516927, -2.378012643645249, "Victoria Pub & Kitchen")
  st_james<-c(51.38841088710996, -2.367839577595941, "St James Wine Vault")
  chequers<-c(51.38762100385879, -2.3649995877670342, "Chequers")
  the_hop_pole<-c(51.38517029671547, -2.3756072543869613, "Hop Pole")
  lamb_and_lion<-c(51.3799431757709, -2.36047030911684, "Lamb & Lion")
  the_ale_house<-c(51.381016894943464, -2.3578779542038055, "Ale House")
  the_boater<-c(51.38333814320346, -2.3567869657286713, "Boater")
  the_cork<-c(51.38067559262335, -2.3623612666137714, "Cork")
  the_grapes<-c(51.381369703795535, -2.361661880607395, "Grapes")
  the_west_gate<-c(51.38155615150952, -2.3607224369355255, "West Gate")
  the_raven<-c(51.38295626377942, -2.361827507010354, "Raven")
  the_crystal_palace<-c(51.3805190656581, -2.35910238276826, "Crystal Palace")
  the_trinity_inn<-c(51.380854428276955, -2.3636500681413475, "Trinity Inn")
  the_king_of_wessex<-c(51.38096944033116, -2.3644286596547244, "King of Wessex")
  garricks_head<-c(51.381998564300375, -2.3627998882304015, "Garricks Head")
  coeur_de_lion<-c(51.382515419372524, -2.359783744193164, "Coeur De Lion")
  the_old_green_tree<-c(51.383580080561174, -2.360266541815859, "Old Green Tree")
  belushis_bath<-c(51.383767565507426, -2.360116338110989, "Belushi's")
  saracens_head<-c(51.38396174552876, -2.3599983209143303, "Saracens Head")
  new_inn<-c(51.3829117407249, -2.3655554652213397, "New Inn")
  sam_wellers<-c(51.382851476620175, -2.360534369945461, "Sam Wellers")
  the_canon<-c(51.38244134413349, -2.362374365329714, "Canon")
  the_pig_and_fiddle<-c(51.384426686136024, -2.3599442839621574, "Pig & Fiddle")
  
  
  # Combine all our the data into one data frame
  pubs1<-as.data.frame(rbind(belvoir_castle, victoria_pub_kit, chequers, st_james, the_hop_pole, lamb_and_lion, the_ale_house, the_boater, the_cork,
                             the_grapes, the_west_gate, the_raven,
                             the_crystal_palace, the_trinity_inn,
                             the_king_of_wessex, garricks_head, coeur_de_lion,
                             the_old_green_tree, belushis_bath, saracens_head,
                             new_inn, sam_wellers, the_canon, 
                             the_pig_and_fiddle))
  
  colnames(pubs1)<-c("latitude", "longitude", "name")
  rownames(pubs1)<-pubs1$name
  
  if (pubs.simple==TRUE) {
    # Drop pubs from the city centre cluster - the full diagram is very crowded around the city centre
    pubs1<-pubs1[-c(7,10,13,14,16,18,20,22),]
  }
  
  # Create data for landmarks in Bath 
  cresent<-c(51.38726861009249, -2.368200298904708, "Royal Cresent")
  abbey<-c(51.381574099758325, -2.3587082390626217,"Bath Abbey")
  city<-c(51.380991384949645, -2.3601866852167026, "City Centre")
  queens<-c(51.38310088171536, -2.363465477916872, "Queen's Square")
  green<-c(51.380045803154566, -2.366953114808759, "Green Park")
  
  # Concatenate the landmark data
  bath.markers<-rbind(cresent, abbey, city, queens, green)
  
  # Create a data frame for bath.markers coordinates
  bath.markers<-data.frame(as.numeric(bath.markers[,1]), as.numeric(bath.markers[,2]), bath.markers[,3], row.names = NULL)
  colnames(bath.markers)<-c("latitude", "longitude", "name")
  bath.markers$longitude<-as.numeric(bath.markers$longitude)
  bath.markers$latitude<-as.numeric(bath.markers$latitude)
  
  # Convert coordinates back into numerics (the name entry of the original
  #vectors make every a character variable)
  pubs1$latitude<-as.numeric(pubs1$latitude)
  pubs1$longitude<-as.numeric(pubs1$longitude)
  
  # Store the minimum values
  #long.min<-min(min(pubs1$longitude), bath.markers$longitude, na.rm = TRUE)
  #lat.min<-min(min(pubs1$latitude), bath.markers$latitude, na.rm = TRUE)
  
  # Standardise the coords with long.in and lat.min - optional
  #pubs1<- pubs1 %>% mutate(longitude=longitude-long.min,
                           #latitude=latitude-lat.min, g=1)
  
  # Return a simple plot of the pub coordinates if simple==TRUE
  if (simple==TRUE) {
    # Return a cartesian plot
    q<-ggplot(data = pubs1, aes(longitude, latitude)) + geom_point(aes(longitude, latitude)) + geom_text(aes(label=name), vjust = -1, hjust =  0.05, size = 3.25) +
      geom_point(data = bath.markers,aes(x=bath.markers[,2], y=bath.markers[,1]), colour="red", size = 5, shape = 18) + geom_text(data=bath.markers, aes(label=name), vjust = -1.2, size = 5, col="red")
    return(q)
  }
  
  # Create the Voronoi diagram with the ggvoronoi package
  p<-ggplot(pubs1, aes(longitude, latitude)) +
    stat_voronoi(geom="path") +  geom_point() + geom_text(aes(label=name), vjust = -1, hjust = 0.05, size = 3.25) + theme(legend.position = "none")
  
  if (title==TRUE) {
    p<-p + ggtitle("Voronoi Diagram Constructed with standardised (longitude,
                   latitude) coordinates")
  }
  # Add on bath.markers's coordinates to the diagram
  # Again standardization is optional
    #bath.markers$longitude<-bath.markers$longitude-long.min
    #bath.markers$latitude<-bath.markers$latitude-lat.min
  # Plot the landmarks separate on top of
    p<-p + geom_point(data = bath.markers,aes(x=bath.markers[,2], y=bath.markers[,1]), colour="red", size = 5, shape = 18) +
      geom_text(data=bath.markers, aes(label=name), vjust = -1.2, size = 5, col="red")
  
    # Output final plot 
    p
  
}
voronoi_pub(pubs.simple = TRUE, simple = TRUE)
voronoi_pub(pubs.simple = TRUE)

#------------------------------------------------------------------------------------------------------------------#

### General function to create 

voronoi_gen<-function(coords, colour="red", markers=NA, col2="brown", marker.shape=18, marker.size=5, simple=FALSE, markers.simple=FALSE, markers.label=FALSE) {
  
  # Make sure coordinates are numeric
  coords[,1]<-as.numeric(coords[,1])
  coords[,2]<-as.numeric(coords[,2])
  
  # Return a simple plot if specified - in the schools case the plot contains the house info
  if (simple==TRUE & markers.simple==TRUE) {
    q<-ggplot(coords, aes(coords[,1], coords[,2])) +  geom_point() + xlab("x") + ylab("y") +
      geom_text(aes(label=coords[,3]), vjust = -1, size = 5, col=colour) + xlim(min=(min(coords[,1])-0.25), max=max(max(coords[,1]))+0.25) +
      ylim(min=(min(coords[,2])-0.25), max=max(max(coords[,2]))+0.25) + geom_point(data = markers,aes(x=markers[,1], y=markers[,2]), colour=col2, size = marker.size, shape = marker.shape)
    return(q)
  }
  if (simple==TRUE) {
    # Simple plot without any markers
    q<-ggplot(coords, aes(coords[,1], coords[,2])) +  geom_point() + xlab("x") + ylab("y") +
      geom_text(aes(label=coords[,3]), vjust = -1, size = 5, col=colour) + xlim(min=(min(coords[,1])-0.25), max=max(max(coords[,1]))+0.25) + ylim(min=(min(coords[,2])-0.25), max=max(max(coords[,2]))+0.25)
    return(q)
  }
  
  # Create a Voronoi diagram with the ggvoronoi package
  p<-ggplot(coords, aes(coords[,1], coords[,2]))+  geom_point() + xlab("x") + ylab("y")  +
    stat_voronoi(geom="path")  + geom_text(aes(label=coords[,3]), vjust = -1, size = 5, col=colour) + theme(legend.position = "none")
  
  if (sum(is.na(markers))==0) {
      markers[,1]<-as.numeric(markers[,1])
      markers[,2]<-as.numeric(markers[,2])
      p<-p + geom_point(data = markers,aes(x=markers[,1], y=markers[,2]), colour=col2, size = marker.size, shape = marker.shape) +
        geom_text(data = markers, aes(x=markers[,1], y=markers[,2] ,label=markers.label), vjust = -1, size = 5, col=colour)
  }
  p
  
}

#------------------------------------------------------------------------------------------------------------------#
  
### Phone mast Voronoi

# Create the phone tower data
towers.x<-sample(c(1:8),7)
towers.y<-sample(c(1:8),7)
towers.name<-c("Tower 1", "Tower 2", "Tower 3", "Tower 4", "Tower 5", "Tower 6", "Tower 7")
towers<-data.frame(cbind(towers.x, towers.y, towers.name))

voronoi_gen(coords = towers, colour="blue")


#------------------------------------------------------------------------------------------------------------------#

### Greggs Voronoi

# Initial Greggs data
greggs.x1<-c(0,1,3,4,3.5,4)
greggs.y1<-c(1,2,5,3,4,1)
greggs.name1<-c("Greggs 1", "Greggs 2", "Greggs 3", "Greggs 4", "Greggs 5", "Greggs 6")
               #, "Greggs 7", "Greggs 8", "Greggs 9", "Greggs 10")
greggs1<-data.frame(cbind(greggs.x1, greggs.y1, greggs.name1))

voronoi_gen(coords = greggs1, colour="orange", simple = FALSE, markers = data.frame(0.5, 4, "New Greggs"),
            marker.shape = 20, marker.size = 3, col2 = "black", markers.label = "New Greggs")

# Add in data for the new Greggs
greggs.x2<-c(greggs.x1, 0.5)
greggs.y2<-c(greggs.y1, 4)
greggs.name2<-c(greggs.name1, "New Greggs")
greggs2<-data.frame(cbind(greggs.x2, greggs.y2, greggs.name2))

voronoi_gen(coords = greggs2, colour="orange", simple=TRUE)


#------------------------------------------------------------------------------------------------------------------#

### School district Voronoi

# Plot houses with the schools and colour by catchment area
schools.x<-c(4,3,5,1,2)
schools.y<-c(2,5,1,3,4)
schools.name<-c("School 1", "School 2", "School 3", "School 4", "School 5")
#, "School 6", "School 7", "School 8", "School 9", "School 10")s
schools<-data.frame(cbind(schools.x, schools.y, schools.name))

# Plot houses with the schools and colour by catchment area 
school.markers.x<-runif(30, min=0.75, max=5.25)
school.markers.y<-runif(30, min=0.75, max=5.25)
school.markers.name<-c(rep("pink",3), rep("blue",2), "brown", "red", "green")
school.markers<-data.frame(cbind(school.markers.x, school.markers.y, school.markers.name[1:length(school.markers.x)]))
school.markers<-data.frame(cbind(school.markers.x, school.markers.y))
voronoi_gen(coords = schools, colour = "dark green", simple = TRUE, markers = school.markers, markers.simple = TRUE)
voronoi_gen(coords = schools, colour="dark green", markers = school.markers)


#------------------------------------------------------------------------------------------------------------------#

### COVID-19/Epidemiology Voronoi: COVID-19 cases and hospitals
hospital.x<-c(sample(c(1:8),4))
hospital.y<-c(sample(c(1:8),4))
hospital.name<-c("Hospital 1", "Hospital 2", "Hospital 3", "Hospital 4")
hospital<-data.frame(cbind(hospital.x, hospital.y, hospital.name))
hospital.markers.x<-runif(50, min=min(hospital.x), max=max(hospital.x))
hospital.markers.y<-runif(50, min=min(hospital.y), max=max(hospital.y))
hospital.markers<-data.frame(cbind(hospital.markers.x, hospital.markers.y))
voronoi_gen(coords = hospital, colour="darkorchid", markers = hospital.markers, col2="dimgrey", marker.shape = 16)


# Alternative idea: resource allocation to vaccination hubs

vac.x<-c(sample(c(1:8),4))
vac.y<-c(sample(c(1:8),4))
vac.name<-c("Vaccine Hub 1", "Vaccine Hub 3", "Vaccine Hub 2", "Vaccine Hub 4")
vac<-data.frame(cbind(vac.x, vac.y, vac.name))
vac.markers.x<-runif(50, min=1.5, max=8)
vac.markers.y<-runif(50, min=1.5, max=8)
vac.markers<-data.frame(cbind(vac.markers.x, vac.markers.y))
voronoi_gen(coords = vac, colour="darkorchid", markers = vac.markers, col2="dimgrey", marker.shape = 16)

#------------------------------------------------------------------------------------------------------------------#