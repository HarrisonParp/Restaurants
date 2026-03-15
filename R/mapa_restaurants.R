library(readr)
library(dplyr)
library(stringr)
library(tidygeocoder)
library(leaflet)
library(htmlwidgets)
library(httr2)
library(jsonlite)
library(purrr)
library(tibble)

dir.create("site", showWarnings = FALSE)

sheet_csv <- "https://docs.google.com/spreadsheets/d/13xaY1vjBYn31O5sApf3BuOiyvpnS5oGp2cc8dH4E0jQ/export?format=csv&gid=0"

google_api_key <- Sys.getenv("GOOGLE_MAPS_API_KEY")

buscar_google_place <- function(query, api_key) {
  if (is.na(query) || query == "" || api_key == "") {
    return(tibble(
      place_id = NA_character_,
      google_name = NA_character_,
      google_address = NA_character_,
      lat_google = NA_real_,
      lon_google = NA_real_,
      fuente_google = NA_character_
    ))
  }

  req <- request("https://places.googleapis.com/v1/places:searchText") %>%
    req_method("POST") %>%
    req_headers(
      "Content-Type" = "application/json",
      "X-Goog-Api-Key" = api_key,
      "X-Goog-FieldMask" = "places.id,places.displayName,places.formattedAddress,places.location"
    ) %>%
    req_body_json(list(
      textQuery = query,
      languageCode = "es",
      maxResultCount = 1
    ))

  resp <- tryCatch(req_perform(req), error = function(e) NULL)

  if (is.null(resp)) {
    return(tibble(
      place_id = NA_character_,
      google_name = NA_character_,
      google_address = NA_character_,
      lat_google = NA_real_,
      lon_google = NA_real_,
      fuente_google = NA_character_
    ))
  }

  dat <- tryCatch(resp_body_json(resp, simplifyVector = TRUE), error = function(e) NULL)

  if (is.null(dat) || is.null(dat$places) || length(dat$places) == 0) {
    return(tibble(
      place_id = NA_character_,
      google_name = NA_character_,
      google_address = NA_character_,
      lat_google = NA_real_,
      lon_google = NA_real_,
      fuente_google = NA_character_
    ))
  }

  place <- dat$places[1, , drop = FALSE]

  tibble(
    place_id = if ("id" %in% names(place)) as.character(place$id[[1]]) else NA_character_,
    google_name = if ("displayName.text" %in% names(place)) as.character(place$`displayName.text`[[1]]) else NA_character_,
    google_address = if ("formattedAddress" %in% names(place)) as.character(place$formattedAddress[[1]]) else NA_character_,
    lat_google = if ("location.latitude" %in% names(place)) as.numeric(place$`location.latitude`[[1]]) else NA_real_,
    lon_google = if ("location.longitude" %in% names(place)) as.numeric(place$`location.longitude`[[1]]) else NA_real_,
    fuente_google = if (!is.null(place$id[[1]])) "Google Places" else NA_character_
  )
}

restaurants <- read_csv(
  sheet_csv,
  show_col_types = FALSE,
  na = c("", "NA", "#DIV/0!")
) %>%
  mutate(across(everything(), ~ str_squish(as.character(.)))) %>%
  mutate(row_id = row_number()) %>%
  transmute(
    row_id,
    restaurant = Restaurant,
    hem_anat = Hem_anat,
    municipi = Municipi,
    nota_harry = suppressWarnings(as.numeric(`Nota Harry`)),
    nota_carla = suppressWarnings(as.numeric(`Nota Carla`)),
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

# 1) OSM primero
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
    select(row_id, query_fallback) %>%
    geocode(
      address = query_fallback,
      method = "osm",
      lat = lat_2,
      long = lon_2,
      min_time = 1
    ) %>%
    select(row_id, lat_2, lon_2)

  geo_temp <- geo_1 %>%
    select(row_id, lat_1, lon_1) %>%
    left_join(geo_2, by = "row_id") %>%
    mutate(
      lat_osm = coalesce(lat_1, lat_2),
      lon_osm = coalesce(lon_1, lon_2)
    ) %>%
    select(row_id, lat_osm, lon_osm)

  restaurants <- restaurants %>%
    left_join(geo_temp, by = "row_id") %>%
    mutate(
      lat = coalesce(lat, lat_osm),
      lon = coalesce(lon, lon_osm),
      fuente_coords = case_when(
        !is.na(lat) & !is.na(lon) & !is.na(lat_osm) & !is.na(lon_osm) ~ "OSM",
        TRUE ~ NA_character_
      )
    ) %>%
    select(-lat_osm, -lon_osm)
} else {
  restaurants <- restaurants %>%
    mutate(fuente_coords = ifelse(!is.na(lat) & !is.na(lon), "Manual", NA_character_))
}

# 2) fallback a Google Places solo para los que sigan sin coordenadas
faltan_google <- restaurants %>%
  filter(is.na(lat) | is.na(lon)) %>%
  select(row_id, query_final)

if (nrow(faltan_google) > 0 && google_api_key != "") {
  google_results <- map_dfr(faltan_google$query_final, buscar_google_place, api_key = google_api_key) %>%
    mutate(row_id = faltan_google$row_id, .before = 1)

  restaurants <- restaurants %>%
    left_join(google_results, by = "row_id") %>%
    mutate(
      lat = coalesce(lat, lat_google),
      lon = coalesce(lon, lon_google),
      direccion = google_address,
      place_id = place_id,
      fuente_coords = case_when(
        !is.na(lat) & !is.na(lon) & is.na(fuente_coords) & !is.na(lat_google) & !is.na(lon_google) ~ "Google Places",
        TRUE ~ fuente_coords
      )
    ) %>%
    select(-google_name, -lat_google, -lon_google, -fuente_google)
} else {
  restaurants <- restaurants %>%
    mutate(
      direccion = NA_character_,
      place_id = NA_character_
    )
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

restaurants <- restaurants %>%
  mutate(
    popup = paste0(
      "<b>", restaurant, "</b><br>",
      "<b>Municipi:</b> ", municipi, "<br>",
      "<b>Hem anat:</b> ", ifelse(is.na(hem_anat_clean) | hem_anat_clean == "", "No disponible", hem_anat_clean), "<br>",
      "<b>Nota Harry:</b> ", ifelse(is.na(nota_harry), "No disponible", nota_harry), "<br>",
      "<b>Nota Carla:</b> ", ifelse(is.na(nota_carla), "No disponible", nota_carla), "<br>",
      "<b>Mitja:</b> ", ifelse(is.na(mitja), "No disponible", round(mitja, 1)), "<br>",
      "<b>Dirección:</b> ", ifelse(is.na(direccion) | direccion == "", "No disponible", direccion), "<br>",
      "<b>Fuente coords:</b> ", ifelse(is.na(fuente_coords) | fuente_coords == "", "No disponible", fuente_coords)
    )
  )

datos_mapa <- restaurants %>%
  filter(!is.na(lat), !is.na(lon))

if (nrow(datos_mapa) == 0) {
  stop("No hay restaurantes con coordenadas.")
}

mapa_restaurants <- leaflet(datos_mapa) %>%
  addProviderTiles("CartoDB.Positron") %>%
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

if (nrow(datos_mapa) == 1) {
  mapa_restaurants <- mapa_restaurants %>%
    setView(
      lng = datos_mapa$lon[1],
      lat = datos_mapa$lat[1],
      zoom = 15
    )
} else {
  mapa_restaurants <- mapa_restaurants %>%
    fitBounds(
      lng1 = min(datos_mapa$lon, na.rm = TRUE),
      lat1 = min(datos_mapa$lat, na.rm = TRUE),
      lng2 = max(datos_mapa$lon, na.rm = TRUE),
      lat2 = max(datos_mapa$lat, na.rm = TRUE)
    )
}

saveWidget(mapa_restaurants, "site/index.html", selfcontained = TRUE)
