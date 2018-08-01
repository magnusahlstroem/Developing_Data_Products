library(leaflet)

##########################################################################
# Michael L. Bernauer
# mlbernauer@gmail.com
# 12/14/2014
# Module for parsing PubMed Medline files.
# Files should be downloaded to your
# computer and loaded into R by passing the
# file path into the medline function.
# The function returns a list containing
# each Medline entry.
#
# USAGE:
# source('medline.R')
# medline_records <- medline("/home/user/Downloads/pubmed_results.txt")
##########################################################################
medline = function(file_name){
  lines <- readLines(file_name)
  medline_records <- list()
  key <- 0
  record <- 0
  for(line in lines){
    header <- sub(" {1,20}", "", substring(line, 1, 4))
    value <- sub("^.{6}", "", line)
    if(header == "" & value == ""){
      next
    }
    else if(header == "PMID"){
      record = record + 1
      medline_records[[record]] <- list()
      medline_records[[record]][header] <- value
    }
    else if(header == "" & value != ""){
      medline_records[[record]][key] <- paste(medline_records[[record]][key], value)
    }
    else{
      key <- header
      if(is.null(medline_records[[record]][key][[1]])){
        medline_records[[record]][key] <- value
      }
      else { 
        medline_records[[record]][key] <- paste(medline_records[[record]][key], value, sep=";")
      }
    }
  }
  return(medline_records)
}

register_google(key = "AIzaSyBCUHHy3r_HDRgdIzzxS5cViXgWNibefuU")

#parsed <- medline("C:/Users/mgah/Downloads/pubmed_result (1).txt")
parsed <- medline("C:/Users/mgah/Downloads/pubmed_result.txt")

affiliations <- lapply(parsed, function(x) {
  fa_aff <- if (!is.null(x$AD)) strsplit(x$AD, ";")[[1]][1] else NA
  index_uni <- regexpr("\\,[[:alpha:][:space:]]*[Uu][Nn][Ii][[:alpha:][:space:]]*\\,", fa_aff)
  uni <- sub("^\\,\\ ", "", substr(fa_aff, index_uni, index_uni[1] + attributes(index_uni)[[1]]))
  uni_cleansed <- trimws(sub("\\,\\ $", "", sub("^\\,\\ *", "", uni)))
  list(University = uni_cleansed, PMID = x$PMID)
})

affiliations2 <- do.call(rbind.data.frame, c(affiliations, stringsAsFactors = F)) %>%
  #mutate(University = as.character(University)) %>%
  filter(University != "" & !is.na(University))

affiliations3 <- affiliations2 %>% group_by(University) %>% summarise(n = n())
affiliations2 <- tbl_df(left_join(affiliations2, affiliations3, by = "University")) %>% arrange(University)

#lon_lat <- geocode(affiliations3$University)

#affiliations2 <- affiliations[!(affiliations == "" | is.na(affiliations))]
#topUnis <- names(table(affiliations2))[order(table(affiliations2), decreasing = T)]

#lon_lat <- geocode(topUnis[1:1000], source = "google")
lon_lat2 <- data.frame(University = topUnis[1:1000], lon = lon_lat[,1], lat = lon_lat[,2])
lon_lat2 <- lon_lat2[complete.cases(lon_lat2),] #%>%
  #group_by(lon, lat) %>%
  #summarize(University = University[1], n = n())
dupl_lon_lon
lapply(dupl_lon_lon, function(x)
  filter(lon_lat2, lon == lon_lat2$lon[x] & lat == lon_lat2$lat[x])
  )
#save(lon_lat2, file = "C:/Kurser/Developing_Data_Products/homework1/lon_lat.Rdata")
load("C:/Kurser/Developing_Data_Products/homework1/lon_lat.Rdata")

affiliations4 <- left_join(affiliations3, lon_lat2, by = "University") %>%
  filter(!is.na(lon)) %>%
  mutate(n = n.x + n.y) %>%
  select(-n.x, -n.y) %>%
  arrange(-n) %>%
  mutate(group = cut(n, breaks = c(0,3,5,8,20,Inf), labels = c("green", "light_green", "yellow", "orange", "red")))

affiliations2 <- left_join(affiliations2, affiliations4, by = "University") %>%
  filter(!is.na(lon)) %>%
  group_by(University) %>%
  slice(1:5)

test <- lapply(unique(affiliations2$University), function(x) {
  pubs <- filter(affiliations2, University == x) %>% pull(PMID)
  as_a_link <- lapply(pubs, function(t) paste("<a href='https://www.ncbi.nlm.nih.gov/pubmed/", t, "'>", t, "</a>", sep = ""))
  data.frame(University = x, link = do.call(paste, c(as_a_link, sep = " <br> ")))
})
test.df <- do.call(rbind.data.frame, test)
affiliations4 <- left_join(affiliations4, test.df, by = "University")

quakeIcons <- iconList(green = makeIcon("C:/Kurser/Developing_Data_Products/Map_markers/map-marker-green.png", iconWidth = 32, iconHeight =32, iconAnchorX = 16, iconAnchorY = 32),
                       light_green = makeIcon("C:/Kurser/Developing_Data_Products/Map_markers/map-marker-light-green.png", iconWidth = 32, iconHeight =32, iconAnchorX = 16, iconAnchorY = 32),
                       yellow = makeIcon("C:/Kurser/Developing_Data_Products/Map_markers/map-marker-yellow.png", iconWidth = 32, iconHeight =32, iconAnchorX = 16, iconAnchorY = 32),
                       orange = makeIcon("C:/Kurser/Developing_Data_Products/Map_markers/map-marker-orange.png", iconWidth = 32, iconHeight =32, iconAnchorX = 16, iconAnchorY = 32),
                       red = makeIcon("C:/Kurser/Developing_Data_Products/Map_markers/map-marker-red.png", iconWidth = 32, iconHeight =32, iconAnchorX = 16, iconAnchorY = 32))




for_pop_ups <- paste0(affiliations4$University, 
                      "<br> Publications ", 
                      affiliations4$n,
                      "<br> First 5 publications <br>",
                      affiliations4$link
)


leaflet(data = affiliations4) %>% 
  addTiles() %>%
  addMarkers(lng = ~lon, 
             lat = ~lat, 
             popup = for_pop_ups, 
             icon = ~quakeIcons[group],
             clusterOptions = markerClusterOptions())