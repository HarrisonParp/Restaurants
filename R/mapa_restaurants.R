library(readr)
library(dplyr)
library(stringr)
library(tidygeocoder)
library(leaflet)
library(htmlwidgets)

dir.create("docs", showWarnings = FALSE)

sheet_csv <- "https://docs.google.com/spreadsheets/d/13xaY1vjBYn31O5sApf3BuOiyvpnS5oGp2cc8dH4E0jQ/export?format=csv&gid=0"

restaurants <- read_csv(sheet_csv, show_col_types = FALSE) %>%
  mutate(across(everything(), ~ str_squish(as.character(.)))) %>%
  transmute(
    restaurant = Restaurant,
    hem_anat = Hem_anat,
    municipi = Municipi,
    nota_harry = suppressWarnings(as.numeric(na_if(`Nota Harry`, ""))),
    nota_carla = suppressWarnings(as.numeric(na_if(`Nota Carla`, ""))),
    mitja = rowMeans(cbind(nota_harry, nota_carla), na.rm = TRUE),
    mitja = ifelse(is.nan(mitja), NA, mitja),
    pais = if ("Pais" %in% names(.)) ifelse(is.na(Pais) | Pais == "", "Spain", Pais) else "Spain",
    query_mapa = if ("Query_mapa" %in% names(.)) ifelse(is.na(Query_mapa) | Query_mapa == "", NA, Query_mapa) else NA_character_,
    lat = if ("Lat" %in% names(.)) suppressWarnings(as.numeric(Lat)) else NA_real_,
    lon = if ("Lon" %in% names(.)) suppressWarnings(as.numeric(Lon)) else NA_real_
  ) %>%
  filter(!if_all(c(restaurant, municipi), ~ is.na(.) | . == "")) %>%
  mutate(
    query_auto = paste(restaurant, municipi, pais, sep = ", "),
    query_final = ifelse(!is.na(query_mapa), query_mapa, query_auto),
    query_fallback = paste(municipi, pais, sep = ", ")
  )

sin_coords <- restaurants %>%
  filter(is.na(lat) | is.na(lon))

if (nrow(sin_coords) > 0) {
  geo_1 <- sin_coords %>%
    geocode(
      address = query_final,
      method = "osm",
      lat = lat_1,
      long = lon_1,
      min_time = 1,
      custom_query = list(layer = "poi")
    )

  geo_2 <- geo_1 %>%
    filter(is.na(lat_1) | is.na(lon_1)) %>%
    select(restaurant, municipi, hem_anat, query_fallback) %>%
    geocode(
      address = query_fallback,
      method = "osm",
      lat = lat_2,
      long = lon_2,
      min_time = 1
    ) %>%
    select(restaurant, municipi, hem_anat, lat_2, lon_2)

  geo_temp <- geo_1 %>%
    select(restaurant, municipi, hem_anat, lat_1, lon_1) %>%
    left_join(geo_2, by = c("restaurant", "municipi", "hem_anat")) %>%
    mutate(
      lat_new = coalesce(lat_1, lat_2),
      lon_new = coalesce(lon_1, lon_2)
    ) %>%
    select(restaurant, municipi, hem_anat, lat_new, lon_new)

  restaurants <- restaurants %>%
    left_join(geo_temp, by = c("restaurant", "municipi", "hem_anat")) %>%
    mutate(
      lat = coalesce(lat, lat_new),
      lon = coalesce(lon, lon_new)
    ) %>%
    select(-lat_new, -lon_new)
}

restaurants <- restaurants %>%
  mutate(
    hem_anat_clean = case_when(
      tolower(hem_anat) %in% c("si", "sí", "yes", "1", "true") ~ "Sí",
      tolower(hem_anat) %in% c("no", "0", "false") ~ "No",
      TRUE ~ hem_anat
    ),
    color = case_when(
      hem_anat_clean == "Sí" ~ "#27AE60",
      hem_anat_clean == "No" ~ "#E74C3C",
      TRUE ~ "#7F8C8D"
    ),
    popup = paste0(
      "<b>", restaurant, "</b><br>",
      "<b>Municipi:</b> ", municipi, "<br>",
      "<b>Hem anat:</b> ", ifelse(is.na(hem_anat_clean) | hem_anat_clean == "", "No disponible", hem_anat_clean), "<br>",
      "<b>Nota Harry:</b> ", ifelse(is.na(nota_harry), "No disponible", nota_harry), "<br>",
      "<b>Nota Carla:</b> ", ifelse(is.na(nota_carla), "No disponible", nota_carla), "<br>",
      "<b>Mitja:</b> ", ifelse(is.na(mitja), "No disponible", round(mitja, 1))
    )
  )

notes_valides <- restaurants$mitja[!is.na(restaurants$mitja)]

if (length(notes_valides) == 0) {
  restaurants <- restaurants %>% mutate(radio = 6)
} else {
  min_nota <- min(notes_valides)
  max_nota <- max(notes_valides)

  if (min_nota == max_nota) {
    restaurants <- restaurants %>% mutate(radio = 6)
  } else {
    restaurants <- restaurants %>%
      mutate(
        radio = ifelse(
          is.na(mitja),
          5,
          4 + (mitja - min_nota) / (max_nota - min_nota) * 8
        )
      )
  }
}

datos_mapa <- restaurants %>%
  filter(!is.na(lat), !is.na(lon))

if (nrow(datos_mapa) == 0) {
  stop("No hay restaurantes con coordenadas.")
}

mapa_restaurants <- leaflet(datos_mapa) %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(lng = 2.1734, lat = 41.3851, zoom = 11) %>%
  addCircleMarkers(
    lng = ~lon,
    lat = ~lat,
    radius = ~radio,
    stroke = TRUE,
    weight = 1,
    color = ~color,
    fillColor = ~color,
    fillOpacity = 0.75,
    popup = ~popup,
    label = ~restaurant
  )

saveWidget(mapa_restaurants, "docs/index.html", selfcontained = TRUE)
