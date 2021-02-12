catalunya_map <- rgdal::readOGR("www/comarques-compressed.geojson")
ca_spain_gj <- rgdal::readOGR("data/georef-spain-comunidad-autonoma.geojson")

usethis::use_data(ca_spain_gj, overwrite = TRUE, compress = "xz")
usethis::use_data(catalunya_map, overwrite = TRUE, compress = "xz")
