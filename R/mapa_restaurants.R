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
geoapify_api_key <- Sys.getenv("GEOAPIFY_API_KEY")

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
    req_headers("Accept-Language" = "es") %>%
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

  if (is.na(display_name) || trimws(display_name) == "") {
    return(FALSE)
  }

  poi_like <- (
    class_n %in% c("amenity", "shop", "tourism", "leisure") ||
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

  words_rest <- unlist(strsplit(restaurant_n, " ", fixed = TRUE))
  words_rest <- words_rest[nchar(words_rest) >= 4]

  if (length(words_rest) == 0) {
    return(FALSE)
  }

  has_name_match <- any(vapply(
    words_rest,
    function(w) grepl(paste0("\\b", w, "\\b"), display_n),
    logical(1)
  ))

  poi_like && has_name_match && !generic_place
}

# --------------------------------
# Geoapify
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

empty_geoapify_city_result <- function() {
  tibble(
    city_query = NA_character_,
    city_place_id = NA_character_,
    city_name = NA_character_,
    city_lat = NA_real_,
    city_lon = NA_real_
  )
}

buscar_geoapify_city <- function(municipi, pais, api_key) {
  if (is.na(municipi) || municipi == "" || api_key == "") {
    return(empty_geoapify_city_result())
  }

  country_code <- country_to_code(pais)
  text_query <- safe_paste_parts(municipi, pais)

  req <- request("https://api.geoapify.com/v1/geocode/search") %>%
    req_url_query(
      text = text_query,
      type = "city",
      format = "json",
      limit = 1,
      lang = "es",
      apiKey = api_key
    )

  if (!is.na(country_code)) {
    req <- req %>%
      req_url_query(filter = paste0("countrycode:", country_code))
  }

  resp <- tryCatch(req_perform(req), error = function(e) NULL)

  if (is.null(resp)) {
    return(empty_geoapify_city_result())
  }

  dat <- tryCatch(resp_body_json(resp, simplifyVector = TRUE), error = function(e) NULL)

  if (is.null(dat) || !"results" %in% names(dat)) {
    return(empty_geoapify_city_result())
  }

  results <- dat$results

  if (is.null(results)) {
    return(empty_geoapify_city_result())
  }

  if (is.data.frame(results) && nrow(results) == 0) {
    return(empty_geoapify_city_result())
  }

  tibble(
    city_query = text_query,
    city_place_id = as.character(extract_first(results, "place_id")),
    city_name = as.character(extract_first(results, "formatted")),
    city_lat = suppressWarnings(as.numeric(extract_first(results, "lat"))),
    city_lon = suppressWarnings(as.numeric(extract_first(results, "lon")))
  )
}

empty_geoapify_result <- function() {
  tibble(
    place_id_provider = NA_character_,
    provider_address = NA_character_,
    lat_provider = NA_real_,
    lon_provider = NA_real_,
    fuente_provider = NA_character_,
    provider_match_score = NA_real_,
    provider_candidate_name = NA_character_,
    provider_candidate_city = NA_character_,
    provider_candidate_categories = NA_character_,
    provider_distance = NA_real_,
    provider_match_status = NA_character_
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

municipality_match_score <- function(municipi, candidate_city, formatted) {
  muni_n <- normalize_txt(municipi)
  haystack <- normalize_txt(paste(candidate_city, formatted))

  if (muni_n == "" || haystack == "") {
    return(0)
  }

  as.numeric(grepl(paste0("\\b", muni_n, "\\b"), haystack))
}

distance_match_score <- function(distance_m) {
  if (is.na(distance_m)) {
    return(0.5)
  }

  max(0, 1 - (distance_m / 10000))
}

build_geoapify_candidates <- function(features) {
  if (is.null(features) || length(features) == 0) {
    return(tibble())
  }

  map_dfr(features, function(ft) {
    props <- ft$properties %||% list()
    cats <- props$categories %||% character()

    tibble(
      provider_name = as.character(props$name %||% NA_character_),
      provider_address = as.character(props$formatted %||% NA_character_),
      place_id_provider = as.character(props$place_id %||% NA_character_),
      lat_provider = suppressWarnings(as.numeric(props$lat %||% NA_real_)),
      lon_provider = suppressWarnings(as.numeric(props$lon %||% NA_real_)),
      provider_distance = suppressWarnings(as.numeric(props$distance %||% NA_real_)),
      provider_city = as.character(props$city %||% NA_character_),
      provider_categories = paste(as.character(cats), collapse = "|")
    )
  })
}

buscar_geoapify_place <- function(restaurant, municipi, city_lat, city_lon, api_key) {
  if (is.na(restaurant) || restaurant == "" || api_key == "") {
    return(empty_geoapify_result())
  }

  if (is.na(city_lat) || is.na(city_lon)) {
    return(empty_geoapify_result() %>% mutate(provider_match_status = "sin_centroide_municipio"))
  }

  req <- request("https://api.geoapify.com/v2/places") %>%
    req_url_query(
      categories = "catering.restaurant,catering.cafe,catering.bar,catering.pub,catering.fast_food",
      name = restaurant,
      filter = paste0("circle:", city_lon, ",", city_lat, ",8000"),
      bias = paste0("proximity:", city_lon, ",", city_lat),
      limit = 5,
      lang = "es",
      apiKey = api_key
    )

  resp <- tryCatch(req_perform(req), error = function(e) NULL)

  if (is.null(resp)) {
    return(empty_geoapify_result())
  }

  dat <- tryCatch(resp_body_json(resp, simplifyVector = FALSE), error = function(e) NULL)

  if (is.null(dat) || is.null(dat$features) || length(dat$features) == 0) {
    return(empty_geoapify_result() %>% mutate(provider_match_status = "sin_resultado"))
  }

  candidates <- build_geoapify_candidates(dat$features)

  if (nrow(candidates) == 0) {
    return(empty_geoapify_result() %>% mutate(provider_match_status = "sin_resultado"))
  }

  candidates <- candidates %>%
    mutate(
      name_score = vapply(provider_name, function(x) name_match_score(restaurant, x), numeric(1)),
      municipality_score = vapply(
        seq_len(n()),
        function(i) municipality_match_score(municipi, provider_city[i], provider_address[i]),
        numeric(1)
      ),
      distance_score = vapply(provider_distance, distance_match_score, numeric(1)),
      provider_match_score = 0.7 * name_score + 0.2 * municipality_score + 0.1 * distance_score
    ) %>%
    arrange(desc(provider_match_score), provider_distance)

  best <- candidates %>% slice(1)

  accepted <- !is.na(best$lat_provider[[1]]) &&
    !is.na(best$lon_provider[[1]]) &&
    best$name_score[[1]] >= 0.55 &&
    best$provider_match_score[[1]] >= 0.60

  tibble(
    place_id_provider = if (accepted) best$place_id_provider[[1]] else NA_character_,
    provider_address = if (accepted) best$provider_address[[1]] else NA_character_,
    lat_provider = if (accepted) best$lat_provider[[1]] else NA_real_,
    lon_provider = if (accepted) best$lon_provider[[1]] else NA_real_,
    fuente_provider = if (accepted) "Geoapify" else NA_character_,
    provider_match_score = best$provider_match_score[[1]],
    provider_candidate_name = best$provider_name[[1]],
    provider_candidate_city = best$provider_city[[1]],
    provider_candidate_categories = best$provider_categories[[1]],
    provider_distance = best$provider_distance[[1]],
    provider_match_status = if (accepted) "aceptado" else "rechazado"
  )
}

    empty_geoapify_text_result <- function() {
  tibble(
    place_id_provider = NA_character_,
    provider_address = NA_character_,
    lat_provider = NA_real_,
    lon_provider = NA_real_,
    fuente_provider = NA_character_,
    provider_match_score = NA_real_,
    provider_candidate_name = NA_character_,
    provider_candidate_city = NA_character_,
    provider_candidate_categories = NA_character_,
    provider_distance = NA_real_,
    provider_match_status = NA_character_
  )
}

buscar_geoapify_text <- function(query, restaurant, municipi, city_lat, city_lon, api_key) {
  if (is.na(query) || query == "" || api_key == "") {
    return(empty_geoapify_text_result())
  }

  req <- request("https://api.geoapify.com/v1/geocode/search") %>%
    req_url_query(
      text = query,
      format = "json",
      limit = 5,
      lang = "es",
      apiKey = api_key
    )

  if (!is.na(city_lat) && !is.na(city_lon)) {
    req <- req %>%
      req_url_query(
        bias = paste0("proximity:", city_lon, ",", city_lat),
        filter = paste0("circle:", city_lon, ",", city_lat, ",12000")
      )
  }

  resp <- tryCatch(req_perform(req), error = function(e) NULL)

  if (is.null(resp)) {
    return(empty_geoapify_text_result())
  }

  dat <- tryCatch(resp_body_json(resp, simplifyVector = TRUE), error = function(e) NULL)

  if (is.null(dat) || !"results" %in% names(dat) || is.null(dat$results)) {
    return(empty_geoapify_text_result() %>% mutate(provider_match_status = "sin_resultado_texto"))
  }

  results <- dat$results

  if (!is.data.frame(results) || nrow(results) == 0) {
    return(empty_geoapify_text_result() %>% mutate(provider_match_status = "sin_resultado_texto"))
  }

  provider_name_vec <- if ("name" %in% names(results)) {
    as.character(results$name)
  } else {
    as.character(results$formatted)
  }

  provider_city_vec <- if ("city" %in% names(results)) {
    as.character(results$city)
  } else {
    rep(NA_character_, nrow(results))
  }

  provider_distance_vec <- if ("distance" %in% names(results)) {
    suppressWarnings(as.numeric(results$distance))
  } else {
    rep(NA_real_, nrow(results))
  }

  provider_result_type_vec <- if ("result_type" %in% names(results)) {
    as.character(results$result_type)
  } else {
    rep(NA_character_, nrow(results))
  }

  candidates <- tibble(
    provider_name = provider_name_vec,
    provider_address = as.character(results$formatted),
    place_id_provider = as.character(results$place_id),
    lat_provider = suppressWarnings(as.numeric(results$lat)),
    lon_provider = suppressWarnings(as.numeric(results$lon)),
    provider_distance = provider_distance_vec,
    provider_city = provider_city_vec,
    provider_result_type = provider_result_type_vec
  ) %>%
    mutate(
      name_score = vapply(provider_name, function(x) name_match_score(restaurant, x), numeric(1)),
      municipality_score = vapply(
        seq_len(n()),
        function(i) municipality_match_score(municipi, provider_city[i], provider_address[i]),
        numeric(1)
      ),
      distance_score = vapply(provider_distance, distance_match_score, numeric(1)),
      type_score = case_when(
        provider_result_type %in% c("city", "county", "state", "postcode", "suburb", "district", "street") ~ 0,
        is.na(provider_result_type) ~ 0.5,
        TRUE ~ 1
      ),
      provider_match_score = 0.65 * name_score + 0.20 * municipality_score + 0.10 * distance_score + 0.05 * type_score
    ) %>%
    arrange(desc(provider_match_score), provider_distance)

  best <- candidates %>% slice(1)

  words_rest <- unlist(strsplit(normalize_txt(restaurant), " ", fixed = TRUE))
  words_rest <- words_rest[words_rest != ""]
  single_word_restaurant <- length(words_rest) <= 1

  accepted <- !is.na(best$lat_provider[[1]]) &&
    !is.na(best$lon_provider[[1]]) &&
    if (single_word_restaurant) {
      best$name_score[[1]] >= 0.40 && best$provider_match_score[[1]] >= 0.45
    } else {
      best$name_score[[1]] >= 0.55 && best$provider_match_score[[1]] >= 0.60
    }

  tibble(
    place_id_provider = if (accepted) best$place_id_provider[[1]] else NA_character_,
    provider_address = if (accepted) best$provider_address[[1]] else NA_character_,
    lat_provider = if (accepted) best$lat_provider[[1]] else NA_real_,
    lon_provider = if (accepted) best$lon_provider[[1]] else NA_real_,
    fuente_provider = if (accepted) "GeoapifyText" else NA_character_,
    provider_match_score = best$provider_match_score[[1]],
    provider_candidate_name = best$provider_name[[1]],
    provider_candidate_city = best$provider_city[[1]],
    provider_candidate_categories = best$provider_result_type[[1]],
    provider_distance = best$provider_distance[[1]],
    provider_match_status = if (accepted) "aceptado_texto" else "rechazado_texto"
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
# OSM primero, sin fallback por municipio
# --------------------------------

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

      display_name_osm <- res$osm_display_name[[1]]
      class_osm <- res$osm_class[[1]]
      type_osm <- res$osm_type[[1]]

      good_match <- osm_result_is_good(
        restaurant = restaurant,
        display_name = display_name_osm,
        class = class_osm,
        type = type_osm
      )

      message(
        "OSM | query=", query_final,
        " | display_name=", ifelse(is.na(display_name_osm), "NA", display_name_osm),
        " | class=", ifelse(is.na(class_osm), "NA", class_osm),
        " | type=", ifelse(is.na(type_osm), "NA", type_osm),
        " | accepted=", good_match
      )

      tibble(
        row_id = row_id,
        lat_osm = if (good_match) res$lat_osm_raw[[1]] else NA_real_,
        lon_osm = if (good_match) res$lon_osm_raw[[1]] else NA_real_,
        direccion_osm = if (good_match) display_name_osm else NA_character_,
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

# --------------------------------
# centroides de municipio para Geoapify
# --------------------------------

city_lookup <- restaurants %>%
  filter((is.na(lat) | is.na(lon)) & !is.na(municipi) & municipi != "") %>%
  distinct(municipi, pais)

if (nrow(city_lookup) > 0 && geoapify_api_key != "") {
  message("Consultando centroides de municipio con Geoapify para ", nrow(city_lookup), " municipios...")

  city_results <- pmap_dfr(
    list(city_lookup$municipi, city_lookup$pais),
    function(municipi, pais) {
      Sys.sleep(0.25)

      res <- buscar_geoapify_city(municipi, pais, geoapify_api_key)

      tibble(
        municipi = municipi,
        pais = pais,
        city_place_id = res$city_place_id[[1]],
        city_name = res$city_name[[1]],
        city_lat = res$city_lat[[1]],
        city_lon = res$city_lon[[1]]
      )
    }
  )

  restaurants <- restaurants %>%
    left_join(city_results, by = c("municipi", "pais"))
} else {
  restaurants <- restaurants %>%
    mutate(
      city_place_id = NA_character_,
      city_name = NA_character_,
      city_lat = NA_real_,
      city_lon = NA_real_
    )
}
# --------------------------------
# Geoapify solo para los no resueltos por OSM
# --------------------------------

faltan_provider <- restaurants %>%
  filter(is.na(lat) | is.na(lon)) %>%
  select(row_id, restaurant, municipi, pais, query_final, city_lat, city_lon)

if (nrow(faltan_provider) > 0 && geoapify_api_key != "") {
  message("Consultando Geoapify para ", nrow(faltan_provider), " registros...")

  provider_results <- pmap_dfr(
    list(
      faltan_provider$row_id,
      faltan_provider$restaurant,
      faltan_provider$municipi,
      faltan_provider$pais,
      faltan_provider$query_final,
      faltan_provider$city_lat,
      faltan_provider$city_lon
    ),
    function(row_id, restaurant, municipi, pais, query_final, city_lat, city_lon) {
      Sys.sleep(0.25)

      res <- buscar_geoapify_place(
        restaurant = restaurant,
        municipi = municipi,
        city_lat = city_lat,
        city_lon = city_lon,
        api_key = geoapify_api_key
      )

      if (is.na(res$lat_provider[[1]]) || is.na(res$lon_provider[[1]])) {
        res <- buscar_geoapify_text(
          query = query_final,
          restaurant = restaurant,
          municipi = municipi,
          city_lat = city_lat,
          city_lon = city_lon,
          api_key = geoapify_api_key
        )
      }

      message(
        "Geoapify | query=", ifelse(is.na(query_final), "NA", query_final),
        " | restaurant=", ifelse(is.na(restaurant), "NA", restaurant),
        " | municipi=", ifelse(is.na(municipi), "NA", municipi),
        " | candidato=", ifelse(is.na(res$provider_candidate_name[[1]]), "NA", res$provider_candidate_name[[1]]),
        " | score=", ifelse(is.na(res$provider_match_score[[1]]), "NA", round(res$provider_match_score[[1]], 3)),
        " | status=", ifelse(is.na(res$provider_match_status[[1]]), "NA", res$provider_match_status[[1]]),
        " | fuente=", ifelse(is.na(res$fuente_provider[[1]]), "NA", res$fuente_provider[[1]])
      )

      tibble(
        row_id = row_id,
        place_id_provider = res$place_id_provider[[1]],
        provider_address = res$provider_address[[1]],
        lat_provider = res$lat_provider[[1]],
        lon_provider = res$lon_provider[[1]],
        fuente_provider = res$fuente_provider[[1]],
        provider_match_score = res$provider_match_score[[1]],
        provider_candidate_name = res$provider_candidate_name[[1]],
        provider_candidate_city = res$provider_candidate_city[[1]],
        provider_candidate_categories = res$provider_candidate_categories[[1]],
        provider_distance = res$provider_distance[[1]],
        provider_match_status = res$provider_match_status[[1]]
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
        !is.na(lat_provider) & !is.na(lon_provider) ~ fuente_provider,
        TRUE ~ NA_character_
      )
    ) %>%
    select(
      -place_id_provider, -provider_address, -lat_provider, -lon_provider, -fuente_provider
    )
} else if (nrow(faltan_provider) > 0 && geoapify_api_key == "") {
  message("No hay GEOAPIFY_API_KEY. Los registros no resueltos se quedarán sin coordenadas.")
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

if ("provider_match_status" %in% names(restaurants)) {
  print(table(restaurants$provider_match_status, useNA = "ifany"))
}
