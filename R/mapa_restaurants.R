library(readr)
library(dplyr)
library(stringr)
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

# -----------------------------
# utilidades generales
# -----------------------------

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

normalize_txt <- function(x) {
  x <- tolower(as.character(x))
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x[is.na(x)] <- ""
  x <- gsub("[^a-z0-9 ]", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

extract_first <- function(obj, field) {
  if (is.data.frame(obj)) {
    if (field %in% names(obj) && nrow(obj) >= 1) {
      return(obj[[field]][1])
    }
  }

  if (is.list(obj) && !is.data.frame(obj)) {
    if (field %in% names(obj)) {
      return(obj[[field]])
    }
  }

  NA
}

# -----------------------------
# OSM / Nominatim
# -----------------------------

empty_osm_result <- function() {
  tibble(
    osm_place_id = NA_character_,
    osm_display_name = NA_character_,
    osm_class = NA_character_,
    osm_type = NA_character_,
    lat_osm_raw = NA_real_,
    lon_osm_raw = NA_real_
  )
}

buscar_osm_place <- function(query) {
  if (is.na(query) || query == "") {
    return(empty_osm_result())
  }

  req <- request("https://nominatim.openstreetmap.org/search") %>%
    req_url_query(
      q = query,
      format = "jsonv2",
      limit = 1,
      addressdetails = 1,
      namedetails = 1
    ) %>%
    req_headers(
      "Accept-Language" = "es"
    ) %>%
    req_user_agent("restaurant-map-github-actions/1.0")

  resp <- tryCatch(req_perform(req), error = function(e) NULL)

  if (is.null(resp)) {
    return(empty_osm_result())
  }

  dat <- tryCatch(resp_body_json(resp, simplifyVector = TRUE), error = function(e) NULL)

  if (is.null(dat)) {
    return(empty_osm_result())
  }

  if (is.data.frame(dat) && nrow(dat) == 0) {
    return(empty_osm_result())
  }

  if (is.list(dat) && !is.data.frame(dat) && length(dat) == 0) {
    return(empty_osm_result())
  }

  tibble(
    osm_place_id = as.character(extract_first(dat, "place_id")),
    osm_display_name = as.character(extract_first(dat, "display_name")),
    osm_class = as.character(extract_first(dat, "class")),
    osm_type = as.character(extract_first(dat, "type")),
    lat_osm_raw = suppressWarnings(as.numeric(extract_first(dat, "lat"))),
    lon_osm_raw = suppressWarnings(as.numeric(extract_first(dat, "lon")))
  )
}

osm_result_is_good <- function(restaurant, display_name, class, type) {
  restaurant_n <- normalize_txt(restaurant)
  display_n <- normalize_txt(display_name)
  class_n <- normalize_txt(class)
  type_n <- normalize_txt(type)

  generic_place <- (
    class_n %in% c("place", "boundary") ||
      type_n %in% c(
        "city", "town", "village", "municipality",
        "administrative", "county", "state", "region",
        "suburb", "hamlet", "locality", "postcode",
        "neighbourhood", "quarter", "isolated_dwelling"
      )
  )

  poi_like <- (
    class_n %in% c("amenity", "shop", "tourism", "leisure", "building") ||
      type_n %in% c(
        "restaurant", "cafe", "fast_food", "bar", "pub",
        "food_court", "ice_cream", "bakery"
      )
  )

  words_rest <- unlist(strsplit(restaurant_n, " ", fixed = TRUE))
  words_rest <- words_rest[nchar(words_rest) >= 4]

  has_name_match <- FALSE
  if (length(words_rest) > 0 && !is.na(display_n) && display_n != "") {
    has_name_match <- any(vapply(
      words_rest,
      function(w) grepl(paste0("\\b", w, "\\b"), display_n),
      logical(1)
    ))
  }

  # Aceptamos si parece un POI real o si el nombre encaja,
  # pero rechazamos si claramente es una ciudad/área administrativa.
  (poi_like || has_name_match) && !generic_place
}

# -----------------------------
# TomTom
# -----------------------------

empty_tomtom_result <- function() {
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
    return(empty_tomtom_result())
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
    return(empty_tomtom_result())
  }

  dat <- tryCatch(resp_body_json(resp, simplifyVector = TRUE), error = function(e) NULL)

  if (is.null(dat) || !"results" %in% names(dat)) {
    return(empty_tomtom_result())
  }

  results <- dat$results

  if (is.null(results)) {
    return(empty_tomtom_result())
  }

  if (is.data.frame(results) && nrow(results) == 0) {
    return(empty_tomtom_result())
  }

  tibble(
    place_id_provider = as.character(extract_first(results, "id")),
    provider_address = as.character(extract_first(results, "address.freeformAddress")),
    lat_provider = suppressWarnings(as.numeric(extract_first(results, "position.lat"))),
    lon_provider = suppressWarnings(as.numeric(extract_first(results, "position.lon"))),
    fuente_provider = "TomTom"
  )
}

# -----------------------------
# lectura de datos
# -----------------------------

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
    cache_key = str_to_lower(str_squish(query_final))
  )

# -----------------------------
# cargar caché
# -----------------------------

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

# -----------------------------
# OSM primero, validando que no haya encontrado solo la ciudad
# -----------------------------

faltan_osm <- restaurants %>%
  filter(is.na(lat) | is.na(lon)) %>%
  select(row_id, restaurant, query_final)

if (nrow(faltan_osm) > 0) {
  message("Consultando OSM para ", nrow(faltan_osm), " registros...")

  osm_results <- pmap_dfr(
    list(faltan_osm$row_id, faltan_osm$restaurant, faltan_osm$query_final),
    function(row_id, restaurant, query_final) {
      Sys.sleep(1.1)

      res <- buscar_osm_place(query_final)

      good_match <- osm_result_is_good(
        restaurant = restaurant,
        display_name = res$osm_display_name[[1]],
        class = res$osm_class[[1]],
        type = res$osm_type[[1]]
      )

      tibble(
        row_id = row_id,
        lat_osm = if (good_match) res$lat_osm_raw[[1]] else NA_real_,
        lon_osm = if (good_match) res$lon_osm_raw[[1]] else NA_real_,
        direccion_osm = if (good_match) res$osm_display_name[[1]] else NA_character_,
        place_id_osm = if (good_match) res$osm_place_id[[1]] else NA_character_,
        osm_match_status = case_when(
          is.na(res$lat_osm_raw[[1]]) | is.na(res$lon_osm_raw[[1]]) ~ "sin_resultado",
          good_match ~ "aceptado",
          TRUE ~ "rechazado"
        )
      )
    }
  )

  restaurants <- restaurants %>%
    left_join(osm_results, by = "row_id") %>%
    mutate(
      lat = coalesce(lat, lat_osm),
      lon = coalesce(lon, lon_osm),
      direccion = coalesce(direccion, direccion_osm),
      place_id = coalesce(place_id, place_id_osm),
      fuente_coords = case_when(
        has_manual_coords ~ "Manual",
        !is.na(fuente_coords) ~ fuente_coords,
        !is.na(lat_osm) & !is.na(lon_osm) ~ "OSM",
        TRUE ~ NA_character_
      )
    ) %>%
    select(-lat_osm, -lon_osm, -direccion_osm, -place_id_osm)
}

# -----------------------------
# TomTom solo para los que OSM no resolvió correctamente
# -----------------------------

faltan_provider <- restaurants %>%
  filter(is.na(lat) | is.na(lon)) %>%
  select(row_id, query_final)

if (nrow(faltan_provider) > 0 && tomtom_api_key != "") {
  message("Consultando TomTom para ", nrow(faltan_provider), " registros...")

  provider_results <- pmap_dfr(
    list(faltan_provider$row_id, faltan_provider$query_final),
    function(row_id, query_final) {
      Sys.sleep(0.25)

      res <- buscar_tomtom_place(query_final, tomtom_api_key)

      tibble(
        row_id = row_id,
        place_id_provider = res$place_id_provider[[1]],
        provider_address = res$provider_address[[1]],
        lat_provider = res$lat_provider[[1]],
        lon_provider = res$lon_provider[[1]],
        fuente_provider = res$fuente_provider[[1]]
      )
    }
  )

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
} else if (nrow(faltan_provider) > 0 && tomtom_api_key == "") {
  message("No hay TOMTOM_API_KEY. Los registros no resueltos se quedarán sin coordenadas.")
}

# -----------------------------
# limpieza visual
# -----------------------------

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

# -----------------------------
# guardar caché actualizado
# -----------------------------

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

# -----------------------------
# mapa
# -----------------------------

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

# -----------------------------
# logs
# -----------------------------

message("Mapa generado en site/index.html")
message("Caché actualizado en data/geocoded_cache.csv")
message("Total restaurantes: ", nrow(restaurants))
message("Con coordenadas: ", nrow(datos_mapa))
message("Sin coordenadas: ", nrow(restaurants) - nrow(datos_mapa))

if ("osm_match_status" %in% names(restaurants)) {
  print(table(restaurants$osm_match_status, useNA = "ifany"))
}
