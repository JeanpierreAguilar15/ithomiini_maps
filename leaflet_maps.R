require(stringr)
require(shiny)
require(dplyr)
require(DT)
require(leaflet)
require(grDevices)
require(htmlwidgets)
require(readxl)

#load excel with locality records, decimal latitude and longitude etc
dore_data <- read_excel("Dore_Ithomiini_records_with_ssp.xlsx")

head(dore_data)

#remove any records with no geographic coordinates
dore_data <- dore_data[!is.na(dore_data$Latitude),]
dore_data <- dore_data[!is.na(dore_data$Longitude),]

#Create a unique list of species by concatenating the genus and species columns
dore_data$genus_species <- paste0(dore_data$Genus, "_", dore_data$Species)
taxon_list <- unique(dore_data$genus_species)

#create output directory
dir.create(paste("HTML_maps/",sep=""))

#This loop will create a HTML leaflet map for each species in taxon_list
for (sp in taxon_list){
	print(sp)
	filtered_map <- dore_data[dore_data$genus_species==sp,]
	filtered_map_sp <- filtered_map
	filtered_map_sp$Sub.species <- sp
	filtered_map <- rbind(filtered_map_sp, filtered_map)
	map <- leaflet()
	#Choose which baselayers you want to make available on the map
	map <- addProviderTiles(map,providers$Esri.NatGeoWorldMap,group = "National Geographic")
	map <- addProviderTiles(map,providers$Esri.WorldImagery, group = "Satellite")
	map <- addProviderTiles(map,providers$OpenTopoMap, group = "Topography")
	map <- addProviderTiles(map,providers$OpenStreetMap.Mapnik, group = "OSM")
	subspecies <- unique(filtered_map$Sub.species)
	subspecies <- c(subspecies[1],sort(subspecies[2:length(subspecies)]))
	subspecies <- subspecies[subspecies!=""]
	marker_colours <- rainbow(length(subspecies))
	for (subsp in 1:length(subspecies)){		
		filtered_map_subsp <- filtered_map[filtered_map$Sub.species==subspecies[subsp],]
		col_as_RGB <- paste(c(as.vector(col2rgb(marker_colours[subsp])),1), collapse = ",")		
		map <- addMarkers(map, filtered_map_subsp$Longitude, filtered_map_subsp$Latitude, group = subspecies[subsp],
			#Choose which values from the Excel file you want to include in the pop-up bubbles
			popup=paste("Genus:", filtered_map_subsp$Genus, "<br>",
				"Species:", filtered_map_subsp$Species, "<br>",
				"Subspecies:", filtered_map_subsp$Sub.species, "<br>",
				"Country:", filtered_map_subsp$Country, "<br>",
				"Male mimicry:", filtered_map_subsp$M.mimicry, "<br>",
				"Female mimicry:", filtered_map_subsp$F.mimicry
			),
			popupOptions = popupOptions(autoClose = FALSE, closeOnClick = FALSE),
			clusterOptions = markerClusterOptions(maxClusterRadius = 5,transparent=TRUE,singleMarkerMode=TRUE,zoomToBoundsOnClick=FALSE,
				iconCreateFunction=JS(paste0("function(cluster) {
					var c = ' marker-cluster-small';
					var html = '<div style=\"background-color:rgba(",col_as_RGB,")\"><span>' + cluster.getChildCount() + '</div><span>'
					return new L.DivIcon({html: html, className: 'marker-cluster' + c,iconSize: new L.Point(40, 40) });
				}"))
			)
		)	
	}
	#Add the legend	
	map <- addLayersControl(map,
			baseGroups = c("National Geographic","Satellite", "Topography","OSM"),
			overlayGroups = subspecies,
			options = layersControlOptions(collapsed = FALSE),
		)%>%
      
	#list groups to hide on startup (i.e. have subspecies unchecked when map is opened)
	hideGroup(subspecies[2:length(subspecies)])		
	#save the output map for the species
	dir.create(paste("HTML_maps/",sp,"/",sep=""))
	saveWidget(map, paste("HTML_maps/",sp,"/","index.html",sep=""), selfcontained = FALSE)
}


