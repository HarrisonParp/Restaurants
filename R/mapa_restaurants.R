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

# --------------------------------
# utilidades
# --------------------------------

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

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

safe_paste_parts <- function(...) {
  parts <- c(...)
  parts <- parts[!is.na(parts)]
  parts <- as.character(parts)
  parts <- trimws(parts)
  parts <- parts[parts != ""]
  if (length(parts) == 0) {
    return(NA_character_)
  }
  paste(parts, collapse = ", ")
}

# --------------------------------
# OSM / Nominatim
# --------------------------------

country_to_code <- function(x) {
  x_n <- normalize_txt(x)

  dplyr::case_when(
    x_n %in% c("es", "espana", "spain") ~ "es",
    x_n %in% c("fr", "france", "francia") ~ "fr",
    x_n %in% c("be", "belgium", "belgica") ~ "be",
    x_n %in% c("pt", "portugal") ~ "pt",
    x_n %in% c("it", "italy", "italia") ~ "it",
    x_n %in% c("de", "germany", "alemania") ~ "de",
    x_n %in% c("gb", "uk", "united kingdom", "reino unido") ~ "gb",
    x_n %in% c("ie", "ireland", "irlanda") ~ "ie",
    x_n %in% c("nl", "netherlands", "paises bajos", "holanda", "holland") ~ "nl",
    nchar(x_n) == 2 ~ x_n,
    TRUE ~ NA_character_
  )
}

empty_osm_result <- function() {
  tibble(
    osm_place_id = NA_character_,
    osm_display_name = NA_character_,
    osm_class = NA_character_,
    osm_type = NA_character_,
    lat_osm_raw = NA_real_,
    lon_osm_raw = NA_real_,
    osm_name_score = NA_real_,
    osm_municipality_score = NA_real_,
    osm_total_score = NA_real_,
    osm_match_status = NA_character_,
    osm_query_used = NA_character_
  )
}

name_match_score <- function(target, candidate) {
  target_n <- normalize_txt(target)
  cand_n <- normalize_txt(candidate)

  if (target_n == "" || cand_n == "") {
    return(0)
  }

  if (target_n == cand_n) {
    return(1)
  }

  target_words <- unique(unlist(strsplit(target_n, " ", fixed = TRUE)))
  cand_words <- unique(unlist(strsplit(cand_n, " ", fixed = TRUE)))

  target_words <- target_words[nchar(target_words) >= 3]
  cand_words <- cand_words[nchar(cand_words) >= 3]

  token_score <- if (length(target_words) == 0) {
    0
  } else {
    sum(target_words %in% cand_words) / length(target_words)
  }

  edit_score <- 1 - (utils::adist(target_n, cand_n)[1] / max(nchar(target_n), nchar(cand_n), 1))
  edit_score <- max(0, min(1, edit_score))

  max(token_score, edit_score)
}

municipality_match_score <- function(municipi, display_name) {
  muni_n <- normalize_txt(municipi)
  display_n <- normalize_txt(display_name)

  if (muni_n == "" || display_n == "") {
    return(0)
  }

  as.numeric(grepl(paste0("\\b", muni_n, "\\b"), display_n))
}

osm_poi_score <- function(class, type) {
  class_n <- normalize_txt(class)
  type_n <- normalize_txt(type)

  poi_like <- (
    class_n %in% c("amenity", "shop", "tourism", "leisure", "building") ||
      type_n %in% c(
        "restaurant", "cafe", "fast_food", "bar", "pub", "bakery",
        "food_court", "ice_cream"
      )
  )

  generic_place <- (
    class_n %in% c("place", "boundary", "highway") ||
      type_n %in% c(
        "city", "town", "village", "municipality", "administrative",
        "county", "state", "region", "suburb", "hamlet",
        "locality", "postcode", "neighbourhood", "quarter", "road",
        "residential"
      )
  )

  if (generic_place) return(0)
  if (poi_like) return(1)
  0.4
}

build_osm_queries <- function(query_mapa, restaurant, municipi, pais) {
  qs <- c(
    query_mapa,
    safe_paste_parts(restaurant, municipi, pais),
    safe_paste_parts(restaurant, municipi),
    safe_paste_parts(restaurant, pais),
    restaurant
  )

  qs <- qs[!is.na(qs)]
  qs <- str_squish(qs)
  qs <- qs[qs != ""]
  unique(qs)
}

buscar_osm_candidates <- function(query, country_code = NA_character_) {
  if (is.na(query) || query == "") {
    return(tibble())
  }

  req <- request("https://nominatim.openstreetmap.org/search") %>%
    req_url_query(
      q = query,
      format = "jsonv2",
      limit = 5,
      addressdetails = 1,
      namedetails = 1,
      dedupe = 1
    ) %>%
    req_headers("Accept-Language" = "es") %>%
    req_user_agent("restaurant-map-github-actions/1.0")

  if (!is.na(country_code) && country_code != "") {
    req <- req %>% req_url_query(countrycodes = country_code)
  }

  resp <- tryCatch(req_perform(req), error = function(e) NULL)

  if (is.null(resp)) {
    return(tibble())
  }

  dat <- tryCatch(resp_body_json(resp, simplifyVector = TRUE), error = function(e) NULL)

  if (is.null(dat)) {
    return(tibble())
  }

  if (is.data.frame(dat) && nrow(dat) == 0) {
    return(tibble())
  }

  if (!is.data.frame(dat)) {
    return(tibble())
  }

  tibble(
    osm_place_id = as.character(dat$place_id %||% NA),
    osm_display_name = as.character(dat$display_name %||% NA),
    osm_class = as.character(dat$class %||% NA),
    osm_type = as.character(dat$type %||% NA),
    lat_osm_raw = suppressWarnings(as.numeric(dat$lat %||% NA)),
    lon_osm_raw = suppressWarnings(as.numeric(dat$lon %||% NA))
  )
}

buscar_osm_best <- function(restaurant, municipi, pais, query_mapa) {
  queries <- build_osm_queries(query_mapa, restaurant, municipi, pais)
  country_code <- country_to_code(pais)

  if (length(queries) == 0) {
    return(empty_osm_result() %>% mutate(osm_match_status = "sin_query"))
  }

  all_candidates <- list()

  for (q in queries) {
    Sys.sleep(1.1)

    cand <- buscar_osm_candidates(q, country_code = country_code)

    if (nrow(cand) > 0) {
      cand <- cand %>%
        mutate(
          osm_query_used = q,
          osm_name_score = vapply(
            osm_display_name,
            function(x) name_match_score(restaurant, x),
            numeric(1)
          ),
          osm_municipality_score = vapply(
            osm_display_name,
            function(x) municipality_match_score(municipi, x),
            numeric(1)
          ),
          osm_poi_score = vapply(
            seq_len(n()),
            function(i) osm_poi_score(osm_class[i], osm_type[i]),
            numeric(1)
          ),
          osm_total_score = 0.65 * osm_name_score + 0.20 * osm_municipality_score + 0.15 * osm_poi_score
        )

      all_candidates[[length(all_candidates) + 1]] <- cand
    }
  }

  if (length(all_candidates) == 0) {
    return(empty_osm_result() %>% mutate(osm_match_status = "sin_resultado"))
  }

  candidates <- bind_rows(all_candidates) %>%
    arrange(desc(osm_total_score)) %>%
    distinct(osm_place_id, .keep_all = TRUE)

  best <- candidates %>% slice(1)

  words_rest <- unlist(strsplit(normalize_txt(restaurant), " ", fixed = TRUE))
  words_rest <- words_rest[words_rest != ""]
  single_word_restaurant <- length(words_rest) <= 1

  accepted <- !is.na(best$lat_osm_raw[[1]]) &&
    !is.na(best$lon_osm_raw[[1]]) &&
    if (single_word_restaurant) {
      best$osm_name_score[[1]] >= 0.35 && best$osm_total_score[[1]] >= 0.45
    } else {
      best$osm_name_score[[1]] >= 0.50 && best$osm_total_score[[1]] >= 0.55
    }

  tibble(
    osm_place_id = if (accepted) best$osm_place_id[[1]] else NA_character_,
    osm_display_name = if (accepted) best$osm_display_name[[1]] else NA_character_,
    osm_class = best$osm_class[[1]],
    osm_type = best$osm_type[[1]],
    lat_osm_raw = if (accepted) best$lat_osm_raw[[1]] else NA_real_,
    lon_osm_raw = if (accepted) best$lon_osm_raw[[1]] else NA_real_,
    osm_name_score = best$osm_name_score[[1]],
    osm_municipality_score = best$osm_municipality_score[[1]],
    osm_total_score = best$osm_total_score[[1]],
    osm_match_status = if (accepted) "aceptado" else "rechazado",
    osm_query_used = best$osm_query_used[[1]]
  )
}

# --------------------------------
# lectura de datos
# --------------------------------

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
  filter(
    (!is.na(restaurant) & restaurant != "") |
      (!is.na(query_mapa) & query_mapa != "") |
      (!is.na(lat_input) & !is.na(lon_input))
  ) %>%
  mutate(
    query_auto = pmap_chr(
      list(restaurant, municipi, pais),
      ~ safe_paste_parts(..1, ..2, ..3)
    ),
    query_final = if_else(!is.na(query_mapa), query_mapa, query_auto),
    cache_key = case_when(
      !is.na(query_final) & query_final != "" ~ str_to_lower(str_squish(query_final)),
      TRUE ~ paste0("manual_row_", row_id)
    )
  )

# --------------------------------
# caché
# --------------------------------

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
    ),

    cache_incompleta = !has_manual_coords &
      !is.na(lat) & !is.na(lon) &
      (is.na(direccion) | direccion == ""),

    lat = ifelse(cache_incompleta, NA_real_, lat),
    lon = ifelse(cache_incompleta, NA_real_, lon),
    direccion = ifelse(cache_incompleta, NA_character_, direccion),
    place_id = ifelse(cache_incompleta, NA_character_, place_id),
    fuente_coords = ifelse(cache_incompleta, NA_character_, fuente_coords)
  ) %>%
  select(
    -lat_cache, -lon_cache, -direccion_cache,
    -place_id_cache, -fuente_coords_cache
  )

# --------------------------------
# OSM como único geocoder
# --------------------------------

faltan_osm <- restaurants %>%
  filter(is.na(lat) | is.na(lon)) %>%
  select(row_id, restaurant, municipi, pais, query_mapa)

if (nrow(faltan_osm) > 0) {
  message("Consultando OSM para ", nrow(faltan_osm), " registros...")

  osm_results <- pmap_dfr(
    list(
      faltan_osm$row_id,
      faltan_osm$restaurant,
      faltan_osm$municipi,
      faltan_osm$pais,
      faltan_osm$query_mapa
    ),
    function(row_id, restaurant, municipi, pais, query_mapa) {
      res <- buscar_osm_best(
        restaurant = restaurant,
        municipi = municipi,
        pais = pais,
        query_mapa = query_mapa
      )

      message(
        "OSM | restaurant=", ifelse(is.na(restaurant), "NA", restaurant),
        " | municipi=", ifelse(is.na(municipi), "NA", municipi),
        " | query=", ifelse(is.na(res$osm_query_used[[1]]), "NA", res$osm_query_used[[1]]),
        " | display_name=", ifelse(is.na(res$osm_display_name[[1]]), "NA", res$osm_display_name[[1]]),
        " | class=", ifelse(is.na(res$osm_class[[1]]), "NA", res$osm_class[[1]]),
        " | type=", ifelse(is.na(res$osm_type[[1]]), "NA", res$osm_type[[1]]),
        " | name_score=", ifelse(is.na(res$osm_name_score[[1]]), "NA", round(res$osm_name_score[[1]], 3)),
        " | total_score=", ifelse(is.na(res$osm_total_score[[1]]), "NA", round(res$osm_total_score[[1]], 3)),
        " | status=", ifelse(is.na(res$osm_match_status[[1]]), "NA", res$osm_match_status[[1]])
      )

      tibble(
        row_id = row_id,
        lat_osm = res$lat_osm_raw[[1]],
        lon_osm = res$lon_osm_raw[[1]],
        direccion_osm = res$osm_display_name[[1]],
        place_id_osm = res$osm_place_id[[1]],
        osm_match_status = res$osm_match_status[[1]],
        osm_query_used = res$osm_query_used[[1]],
        osm_name_score = res$osm_name_score[[1]],
        osm_total_score = res$osm_total_score[[1]]
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

# --------------------------------
# visual
# --------------------------------

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

# --------------------------------
# guardar caché
# --------------------------------

cache_new <- restaurants %>%
  filter(
    !is.na(lat), !is.na(lon),
    fuente_coords == "Manual" | !(is.na(direccion) | direccion == "")
  ) %>%
  transmute(
    cache_key,
    query_final = ifelse(is.na(query_final), NA_character_, query_final),
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

# --------------------------------
# mapa
# --------------------------------

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

# --------------------------------
# logs
# --------------------------------

message("Mapa generado en site/index.html")
message("Caché actualizada en data/geocoded_cache.csv")
message("Total restaurantes: ", nrow(restaurants))
message("Con coordenadas: ", nrow(datos_mapa))
message("Sin coordenadas: ", nrow(restaurants) - nrow(datos_mapa))

if ("osm_match_status" %in% names(restaurants)) {
  print(table(restaurants$osm_match_status, useNA = "ifany"))
}

if ("fuente_coords" %in% names(restaurants)) {
  print(table(restaurants$fuente_coords, useNA = "ifany"))
}
