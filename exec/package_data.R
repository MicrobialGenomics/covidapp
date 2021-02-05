catalunya_map <- rgdal::readOGR("www/comarques-compressed.geojson")
usethis::use_data(catalunya_map, overwrite = TRUE, compress = "xz")
