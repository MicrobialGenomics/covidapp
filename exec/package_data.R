catalunya_map <- rgdal::readOGR("www/comarques-compressed.geojson")
ca_spain_gj <- rgdal::readOGR("data/georef-spain-comunidad-autonoma.geojson")
ca_inhabitants <- c(
    1063987,
    1511251,
    319914,
    2701819,
    8464411,
    5057353,
    2175952,
    0,
    1171543,
    1018784,
    2045221,
    2394918,
    87076,
    661197,
    1329391,
    7780479,
    582905,
    84202,
    6779888,
    2220504
)

usethis::use_data(ca_spain_gj, overwrite = TRUE, compress = "xz")
usethis::use_data(catalunya_map, overwrite = TRUE, compress = "xz")
usethis::use_data(ca_inhabitants, overwrite = TRUE, compress = "xz")

