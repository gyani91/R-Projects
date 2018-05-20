#--------------------------------------
# This file contains the code 
#for printing the maps of the different 
#variables used in the Flights Case Study
#--------------------------------------



#-----------------------
# ggmmap of map.italy
#-----------------------
map_italy <- ggmap (get_googlemap (center = 'italy', zoom = 6 , 
                                   maptype = 'roadmap', color = 'bw'), extent='device') +
  geom_path (data = routes_italy_coordinates, 
             aes ( x = Longitude , y = Latitude , group = group ) , 
             size = 0.4 , alpha = I(0.05) , color = 'red', na.rm=TRUE) +
  geom_point (data = airports_italy , 
              aes ( x = Longitude , y = Latitude ) , 
              color = '#1565C0' , alpha = I(0.3), na.rm=TRUE)

print(map.italy)

#-----------------------
# ggmmap of map.italy.outdegree
#-----------------------

map_italy_outdegree <- ggmap ( get_googlemap ( center = 'italy' , zoom = 6 , 
                                               maptype = 'roadmap', color = 'bw' ) , extent = 'device' ) +
  geom_path ( data = routes_italy_coordinates, 
              aes ( x = Longitude , y = Latitude , group = group ) , 
              size = 0.4 , alpha = I(0.05) , color = 'blue', na.rm=TRUE ) +
  geom_point ( data = airports_italy , 
               aes ( x = Longitude , y = Latitude, size = Outdegree ) , 
               color = 'blue' , alpha = I(0.6), na.rm=TRUE )

print(map.italy.outdegree)

#-----------------------
# ggmmap of map.italy.betweenness
#-----------------------

map.italy.betweenness <- (ggmap ( get_googlemap ( center = 'italy' , zoom = 6 , 
                                                  maptype = 'roadmap', color = 'bw' ) , extent = 'device' ) +
                            geom_path ( data = routes.italy.coordinates, 
                                        aes ( x = Longitude , y = Latitude , group = group ) , 
                                        size = 0.1 , alpha = I(0.05) , color = '#e1dfdf', na.rm=TRUE ) +
                            geom_point ( data = airports.italy , 
                                         aes ( x = Longitude , y = Latitude, 
                                               colour = as.vector(Betweenness), size = Outdegree), 
                                         alpha = I(0.8), na.rm=TRUE ) +
                            guides(colour=guide_colourbar("Betweenness")) +   
                            scale_color_gradientn(guide="colourbar", na.value="black", colours=rainbow(2)))

print(map_italy_betweenness)