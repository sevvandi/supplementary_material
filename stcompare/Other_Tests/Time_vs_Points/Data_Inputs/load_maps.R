# Load the polygon data for all countries in Africa and make a grid of points
# over the continent

library(raster)
library(rmapshaper)

# Get country polygons
# ----------------------------------------------------------------------------


Africa_countries <-
  c("Algeria",
    "Angola",
    "Benin",
    "Botswana",
    "Burkina Faso",
    "Burundi",
    "Cameroon",
    "Cape Verde",
    "Central African Republic",
    "Chad",
    "Comoros",
    "Congo",
    "Côte d'Ivoire",
    "Democratic Republic of the Congo",
    "Djibouti",
    "Egypt",
    "Equatorial Guinea",
    "Eritrea",
    "Ethiopia",
    "Gabon",
    "Gambia",
    "Ghana",
    "Guinea",
    "Guinea-Bissau",
    "Kenya",
    "Lesotho",
    "Liberia",
    "Libya",
    "Madagascar",
    "Malawi",
    "Mali",
    "Mauritania",
    "Mauritius",
    "Morocco",
    "Mozambique",
    "Namibia",
    "Niger",
    "Nigeria",
    "Rwanda",
    "Sao Tome And Principe",
    "Senegal",
    "Seychelles",
    "Sierra Leone",
    "Somalia",
    "South Africa",
    "South Sudan",
    "Sudan",
    "Swaziland",
    "Tanzania",
    "Togo",
    "Tunisia",
    "Uganda",
    "Western Sahara",
    "Zambia",
    "Zimbabwe")

load("individual_maps.RData")

# Give each country's polygon an ascending ID so when we rbind, the data rows
# will match the corresponding polygon's ID
for (i in c(1:length(maps_list)))
{
  maps_list[[i]]@polygons[[1]]@ID <- as.character(i)
}
# Check if each country is a single polygon (they are)
npolygons <- unlist(lapply(maps_list, function(x) length(x@polygons)))
npolygons


Africa_map <- do.call(rbind, maps_list)

saveRDS(Africa_map, file = "Africa_border.rds")

# # simplify for plotting
# simple_Africa <- ms_simplify(Africa_map, keep = 0.05)
# plot(simple_Africa)
