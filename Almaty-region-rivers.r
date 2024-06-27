
install.packages("pacman")
pacman::p_load(
  terra,
  elevatr,
  sf,
  geodata,
  tidyverse,
  rayshader,
  dplyr
)

path <- getwd()


kazakhstan_sf <- geodata::gadm(country = "KAZ", level = 1, path = path) |>
  sf::st_as_sf()

almaty_region_sf <- kazakhstan_sf |>
  filter(NAME_1 == "Almaty")


plot(almaty_region_sf)


shapefile_path <- "C:/Users/User/Documents/HydroRIVERS_v10_as_shp"

filename <- list.files(
  path = shapefile_path,
  pattern = "\\.shp$",
  full.names = TRUE
)

country_bbox <- sf::st_bbox(almaty_region_sf)


bbox_wkt <- "POLYGON((
    72.0000 40.0000,
    72.0000 48.5000,
    82.5000 48.5000,
    82.5000 40.0000,
    72.0000 40.0000
))"

country_rivers <- sf::st_read(
  filename,
  wkt_filter = bbox_wkt
) |>
  sf::st_intersection(
    almaty_region_sf
  )

plot(sf::st_geometry(country_rivers))


sort(
  unique(
    country_rivers$ORD_FLOW
  )
)

crs_country <- "+proj=somerc +lat_0=46.9524055555556 +lon_0=7.43958333333333 +k_0=1 +x_0=600000 +y_0=200000 +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs +type=crs"

country_river_width <- country_rivers |>
  dplyr::mutate(
    width = as.numeric(
      ORD_FLOW
    ),
    width = dplyr::case_when(
      width == 3 ~ 16, 
      width == 4 ~ 14,
      width == 5 ~ 12,
      width == 6 ~ 10,
      width == 7 ~ 6,
      TRUE ~ 0
    )
  ) |>
  sf::st_as_sf() |>
  sf::st_transform(crs = crs_country)


dem <- elevatr::get_elev_raster(
  locations = almaty_region_sf,
  z = 7, clip = "locations"
)

dem_country <- dem |>
  terra::rast() |>
  terra::project(crs_country)

dem_matrix <- rayshader::raster_to_matrix(
  dem_country
)

dem_matrix |>
  rayshader::height_shade(
    texture = colorRampPalette(
      c(
        "#fcc69f",
        "#c67847"
      )
    )(128)
  ) |>
  rayshader::add_overlay(
    rayshader::generate_line_overlay(
      geometry = country_river_width,
      extent = dem_country,
      heightmap = dem_matrix,
      color = "#387B9C",
      linewidth = country_river_width$width,
      data_column_width = "width"
    ), alphalayer = 1
  ) |>
  rayshader::plot_3d(
    dem_matrix,
    zscale = 20,
    solid = FALSE,
    shadow = TRUE,
    shadow_darkness = 1,
    background = "white",
    windowsize = c(600, 600),
    zoom = .5,
    phi = 89,
    theta = 0
  )


rayshader::render_camera(
  zoom = .75
)



u <- "https://dl.polyhaven.org/file/ph-assets/HDRIs/hdr/4k/photo_studio_loft_hall_4k.hdr"
hdri_file <- basename(u)

download.file(
  url = u,
  destfile = hdri_file,
  mode = "wb"
)

file_name <- "almaty-region-3d-elevation-rivers.png"

rayshader::render_highquality(
  filename = file_name,
  preview = TRUE,
  light = FALSE,
  environment_light = hdri_file,
  intensity_env = 1,
  interactive = FALSE,
  width = 1000,
  height = 1000,
  camera_up = c(0, 0, 1)
)

