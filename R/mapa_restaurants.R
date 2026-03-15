library(readr)
library(dplyr)
library(stringr)
library(tidygeocoder)
library(leaflet)
library(htmlwidgets)
library(httr2)
library(purrr)
library(tibble)

dir.create("site", showWarnings = FALSE, recursive = TRUE)
dir.create("data", showWarnings = FALSE, recursive = TRUE)

sheet_csv <- "https://docs.google.com/spreadsheets/d/13xaY1vjBYn31O5sApf3BuOiyvpnS5oGp2cc8dH4E0jQ/export?format=csv&gid=0"
cache_file <- "data/geocoded_cache.csv"
tomtom_api_key <- Sys.getenv("TOMTOM_API_KEY")

empty_cache <- function() {
  tibble(
    cache_key = character(),
    query_final = character(),
    lat = numeric(),
    lon = numeric(),
    direccion = character(),
    place_id = character(),
    fuente_coords = character()
  )
}

read_cache_safe <- function(path) {
  if (!file.exists(path)) {
    return(empty_cache())
  }

  out <- tryCatch(
    read_csv(path, show_col_types = FALSE),
    error = function(e) empty_cache()
  )

  required_cols <- c("cache_key", "query_final", "lat", "lon", "direccion", "place_id", "fuente_coords")
  for (nm in required_cols) {
    if (!nm %in% names(out)) {
      out[[nm]] <- NA
    }
  }

  out %>%
    transmute(
      cache_key = as.character(cache_key),
      query_final = as.character(query_final),
      lat = suppressWarnings(as.numeric(lat)),
      lon = suppressWarnings(as.numeric(lon)),
      direccion = as.character(direccion),
      place_id = as.character(place_id),
      fuente_coords = as.character(fuente_coords)
    ) %>%
    distinct(cache_key, .keep_all = TRUE)
}

col_or_na <- function(df, colname) {
  if (colname %in% names(df)) {
    as.character(df[[colname]])
  } else {
    rep(NA_character_, nrow(df))
  }
}

na_if_empty <- function(x) {
  x <- str_squish(as.character(x))
  x[x %in% c("", "NA", "#DIV/0!")] <- NA_character_
  x
}

empty_provider_result <- function() {
  tibble(
    place_id_provider = NA_character_,
    provider_address = NA_character_,
    lat_provider = NA_real_,
    lon_provider = NA_real_,
    fuente_provider = NA_character_
  )
}

buscar_tomtom_place <- function(query, api_key) {
  if (is.na(query) || query == "" || api_key == "") {
    return(empty_provider_result())
  }

  url <- paste0(
    "https://api.tomtom.com/search/2/search/",
    URLencode(query, reserved = TRUE),
    ".json"
  )

  req <- request(url) %>%
    req_url_query(
      key = api_key,
      limit = 1,
      language = "es-ES",
      typeahead = "false"
    )

  resp <- tryCatch(req_perform(req), error = function(e) NULL)

  if (is.null(resp)) {
    return(empty_provider_result())
  }

  dat <- tryCatch(resp_body_json(resp, simplifyVector = TRUE), error = function(e) NULL)

  if (is.null(dat) || is.null(dat$results) || length(dat$results) == 0) {
    return(empty_provider_result())
  }

  res <- dat$results[1, , drop = FALSE]

  tibble(
    place_id_provider = if ("id" %in% names(res)) as.character(res$id[[1]]) else NA_character_,
    provider_address = if ("address.freeformAddress" %in% names(res)) as.character(res$`address.freeformAddress`[[1]]) else NA_character_,
    lat_provider = if ("position.lat" %in% names(res)) as.numeric(res$`position.lat`[[1]]) else NA_real_,
    lon_provider = if ("position.lon" %in% names(res)) as.numeric(res$`position.lon`[[1]]) else NA_real_,
    fuente_provider = "TomTom"
  )
}

raw <- read_csv(
  sheet_csv,
  show_col_types = FALSE,
  na = c("", "NA", "#DIV/0!")
) %>%
  mutate(across(everything(), ~ na_if_empty(.x)))

restaurants <- tibble(
  row_id = seq_len(nrow(raw)),
  restaurant = col_or_na(raw, "Restaurant"),
  hem_anat = col_or_na(raw, "Hem_anat"),
  municipi = col_or_na(raw, "Municipi"),
  nota_harry = suppressWarnings(as.numeric(col_or_na(raw, "Nota Harry"))),
  nota_carla = suppressWarnings(as.numeric(col_or_na(raw, "Nota Carla"))),
  pais = col_or_na(raw, "Pais"),
  query_mapa = col_or_na(raw, "Query_mapa"),
  lat_input = suppressWarnings(as.numeric(col_or_na(raw, "Lat"))),
  lon_input = suppressWarnings(as.numeric(col_or_na(raw, "Lon")))
) %>%
  mutate(
    restaurant = na_if_empty(restaurant),
    hem_anat = na_if_empty(hem_anat),
    municipi = na_if_empty(municipi),
    pais = na_if_empty(pais),
    query_mapa = na_if_empty(query_mapa),
    pais = if_else(is.na(pais), "Spain", pais),
    mitja = rowMeans(cbind(nota_harry, nota_carla), na.rm = TRUE),
    mitja = ifelse(is.nan(mitja), NA_real_, mitja)
  ) %>%
  filter(!if_all(c(restaurant, municipi), ~ is.na(.) | . == "")) %>%
  mutate(
    query_auto = paste(restaurant, municipi, pais, sep = ", "),
    query_final = if_else(!is.na(query_mapa), query_mapa, query_auto),
    query_fallback = paste(municipi, pais, sep = ", "),
    cache_key = str_to_lower(str_squish(query_final))
  )

cache_existing <- read_cache_safe(cache_file)
    
restaurants <- restaurants %>%
  left_join(
    cache_existing %>%
      select(-query_final) %>%
      rename(
        lat_cache = lat,
        lon_cache = lon,
        direccion_cache = direccion,
        place_id_cache = place_id,
        fuente_coords_cache = fuente_coords
      ),
    by = "cache_key"
  ) %>%
  mutate(
    has_manual_coords = !is.na(lat_input) & !is.na(lon_input),
    has_cached_coords = !is.na(lat_cache) & !is.na(lon_cache),
    lat = coalesce(lat_input, lat_cache),
    lon = coalesce(lon_input, lon_cache),
    direccion = direccion_cache,
    place_id = place_id_cache,
    fuente_coords = case_when(
      has_manual_coords ~ "Manual",
      has_cached_coords ~ fuente_coords_cache,
      TRUE ~ NA_character_
    )
  ) %>%
  select(-lat_cache, -lon_cache, -direccion_cache, -place_id_cache, -fuente_coords_cache)
    
# 1) OSM primero para los que siguen sin coordenadas
sin_coords <- restaurants %>%
  filter(is.na(lat) | is.na(lon))

if (nrow(sin_coords) > 0) {
  geo_1 <- tryCatch(
    sin_coords %>%
      geocode(
        address = query_final,
        method = "osm",
        lat = lat_1,
        long = lon_1,
        min_time = 1,
        custom_query = list(layer = "poi")
      ),
    error = function(e) {
      sin_coords %>% mutate(lat_1 = NA_real_, lon_1 = NA_real_)
    }
  )

  quedan_osm <- geo_1 %>%
    filter(is.na(lat_1) | is.na(lon_1)) %>%
    select(row_id, query_fallback)

  if (nrow(quedan_osm) > 0) {
    geo_2 <- tryCatch(
      quedan_osm %>%
        geocode(
          address = query_fallback,
          method = "osm",
          lat = lat_2,
          long = lon_2,
          min_time = 1
        ) %>%
        select(row_id, lat_2, lon_2),
      error = function(e) {
        quedan_osm %>% mutate(lat_2 = NA_real_, lon_2 = NA_real_)
      }
    )
  } else {
    geo_2 <- tibble(row_id = integer(), lat_2 = numeric(), lon_2 = numeric())
  }

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
        has_manual_coords ~ "Manual",
        !is.na(fuente_coords) ~ fuente_coords,
        !is.na(lat_osm) & !is.na(lon_osm) ~ "OSM",
        TRUE ~ NA_character_
      )
    ) %>%
    select(-lat_osm, -lon_osm)
}

# 2) fallback a TomTom solo para los que sigan sin coordenadas
faltan_provider <- restaurants %>%
  filter(is.na(lat) | is.na(lon)) %>%
  select(row_id, query_final)

if (nrow(faltan_provider) > 0 && tomtom_api_key != "") {
  provider_results <- map_dfr(
    faltan_provider$query_final,
    function(q) {
      Sys.sleep(0.25)
      buscar_tomtom_place(q, tomtom_api_key)
    }
  ) %>%
    mutate(row_id = faltan_provider$row_id, .before = 1)

  restaurants <- restaurants %>%
    left_join(provider_results, by = "row_id") %>%
    mutate(
      lat = coalesce(lat, lat_provider),
      lon = coalesce(lon, lon_provider),
      direccion = coalesce(direccion, provider_address),
      place_id = coalesce(place_id, place_id_provider),
      fuente_coords = case_when(
        has_manual_coords ~ "Manual",
        !is.na(fuente_coords) ~ fuente_coords,
        !is.na(lat_provider) & !is.na(lon_provider) ~ "TomTom",
        TRUE ~ NA_character_
      )
    ) %>%
    select(-place_id_provider, -provider_address, -lat_provider, -lon_provider, -fuente_provider)
}

# normalización de "hem anat"
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

# tamaño de puntos
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
      "<b>", ifelse(is.na(restaurant), "Restaurant sense nom", restaurant), "</b><br>",
      "<b>Municipi:</b> ", ifelse(is.na(municipi), "No disponible", municipi), "<br>",
      "<b>Hem anat:</b> ", ifelse(is.na(hem_anat_clean) | hem_anat_clean == "", "No disponible", hem_anat_clean), "<br>",
      "<b>Nota Harry:</b> ", ifelse(is.na(nota_harry), "No disponible", nota_harry), "<br>",
      "<b>Nota Carla:</b> ", ifelse(is.na(nota_carla), "No disponible", nota_carla), "<br>",
      "<b>Mitja:</b> ", ifelse(is.na(mitja), "No disponible", round(mitja, 1)), "<br>",
      "<b>Dirección:</b> ", ifelse(is.na(direccion) | direccion == "", "No disponible", direccion), "<br>",
      "<b>Fuente coords:</b> ", ifelse(is.na(fuente_coords) | fuente_coords == "", "No disponible", fuente_coords)
    )
  )

# guardar caché actualizado
cache_new <- restaurants %>%
  filter(!is.na(lat), !is.na(lon)) %>%
  transmute(
    cache_key,
    query_final,
    lat,
    lon,
    direccion = ifelse(is.na(direccion), NA_character_, direccion),
    place_id = ifelse(is.na(place_id), NA_character_, place_id),
    fuente_coords = ifelse(is.na(fuente_coords), NA_character_, fuente_coords)
  ) %>%
  distinct(cache_key, .keep_all = TRUE)

cache_final <- bind_rows(cache_new, cache_existing) %>%
  group_by(cache_key) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(query_final)

write_csv(cache_final, cache_file, na = "")

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

message("Mapa generado en site/index.html")
message("Caché actualizado en data/geocoded_cache.csv")
