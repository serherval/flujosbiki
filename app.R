#rsconnect::connectCloudUser()
#rsconnect::deployApp("X:/SERGIO/VISUALIZACION VALLADOLID/ONLINE/bikiv2")


library(shiny)
library(dplyr)
library(readr)
library(lubridate)
library(plotly)
library(DT)

# ---------------------------------------------------------------------------
# Configuración
# ---------------------------------------------------------------------------

TZ <- "Europe/Madrid"

FALLBACK_URL   <- "https://raw.githubusercontent.com/serherval/flujosbiki/main/data/historico.csv"
GITHUB_CSV_URL <- Sys.getenv("GITHUB_CSV_URL", unset = FALLBACK_URL)
BASE_DATA_URL  <- sub("/[^/]*$", "", GITHUB_CSV_URL)
ARCHIVO_URL    <- paste0(BASE_DATA_URL, "/archivo")
ESTACIONES_URL <- Sys.getenv("ESTACIONES_CSV_URL", unset = paste0(BASE_DATA_URL, "/estaciones.csv"))
LEGACY_URL     <- Sys.getenv("LEGACY_CSV_URL",
                             unset = "https://raw.githubusercontent.com/serherval/bicis/main/data/biki_history.csv")
STATION_INFO_URL <- Sys.getenv("STATION_INFO_URL",
                               unset = "https://valladolid.publicbikesystem.net/customer/gbfs/v2/es/station_information")

INTERVALO_MS   <- 5 * 60 * 1000   # refresco del mes en curso (GitHub cachea ~5 min)
RESOLUCION_MIN <- 5               # al leer, se conserva 1 medición cada 5 min (rendimiento)
GAP_MAX_MIN    <- 15              # una medición "vale" como mucho 15 min (huecos en los datos)

BARRIOS_GEOJSON <- Sys.getenv("BARRIOS_GEOJSON", unset = "barrios_valladolid.geojson")

# Mapa base sin clave: CARTO (estilo "carto-positron") exige API key en sus teselas desde el
# 23/09/2026 y marca el mapa con «API KEY REQUIRED». Se usa el gris claro de Esri, que no la pide.
TILES_URL <- Sys.getenv("MAPA_TILES_URL",
                        unset = "https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}")

TODOS <- "__todos__"
SIN_BARRIO <- "Sin asignar"          # parada sin coordenadas
FUERA_BARRIO <- "Otros municipios"   # parada con coordenadas fuera de todo polígono
EXTRARRADIO <- "Extrarradio"

ETQ_PERIODO <- c(actual = "Dato actual", `24h` = "Últimas 24 horas", `7d` = "Últimos 7 días",
                 `30d` = "Últimos 30 días", todo = "Desde inicio del histórico")
SEG_PERIODO <- c(`24h` = 24 * 3600, `7d` = 7 * 86400, `30d` = 30 * 86400)

COL_MEC  <- "#f59e0b"
COL_ELEC <- "#16a34a"
COL_TOT  <- "#0f172a"
COL_LIBRE <- "#dbe3ec"       # hueco "libre" de la dona de ocupación (dona y leyenda comparten color)
COL_BICIS_TOT <- "#2563eb"   # barras de bicis totales (ni naranja ni verde)
COL_DEVOL <- "#16a34a"       # actividad: devoluciones
COL_RETIR <- "#ef4444"       # actividad: retiradas
COL_NETO  <- "#0f172a"       # actividad: saldo neto
ESCALA_OCUP <- list(list(0, "#dc2626"), list(0.25, "#f97316"), list(0.5, "#facc15"),
                    list(0.75, "#a3e635"), list(1, "#16a34a"))
COL_BANDAS <- c("0-10%" = "#dc2626", "11-20%" = "#f97316",
                "21-30%" = "#facc15", "31-50%" = "#a3e635", ">50%" = "#16a34a")

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---------------------------------------------------------------------------
# Formato (coma decimal)
# ---------------------------------------------------------------------------

# Todas las cifras: coma decimal, como máximo 1 decimal y sin decimal cuando este sería 0 (12,0 -> 12)
fmt_num <- function(x, dec = 1) {
  s <- formatC(round(x, dec), format = "f", digits = dec, decimal.mark = ",", big.mark = ".")
  s <- sub(paste0(",", strrep("0", dec), "$"), "", s)
  ifelse(is.na(x), "s/d", s)
}
fmt_pct <- function(x, dec = 1) ifelse(is.na(x), "s/d", paste0(fmt_num(x, dec), "%"))

# Mismo criterio en los gráficos (formato d3 de plotly): 1 decimal máx., sin ceros sobrantes, coma decimal
TF_NUM   <- ",.1~f"
SEP_ES   <- ",."
HF_FECHA <- "%d/%m/%Y %H:%M"
LEG_H    <- list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2)   # leyenda centrada bajo el gráfico
ht <- function(nombre, suf = "") paste0("%{y:", TF_NUM, "}", suf, "<extra>", nombre, "</extra>")
fmt_fecha <- function(t) format(t, "%d/%m/%Y %H:%M", tz = TZ)

banda_exclusiva <- function(pct) {
  cut(pct, breaks = c(-Inf, 10, 20, 30, 50, Inf),
      labels = names(COL_BANDAS), right = TRUE)
}

# ---------------------------------------------------------------------------
# Lectura desde GitHub con caché global (compartida entre sesiones)
# ---------------------------------------------------------------------------

.cache <- new.env(parent = emptyenv())

cache_get <- function(key, ttl, loader, vacio, reintento = 60) {
  e <- .cache[[key]]
  ahora <- Sys.time()
  if (!is.null(e) && ahora < e$expira) return(e$v)
  v <- loader()
  if (is.null(v)) {                       # fallo: conserva lo anterior y reintenta pronto
    if (!is.null(e)) { e$expira <- ahora + reintento; .cache[[key]] <- e; return(e$v) }
    .cache[[key]] <- list(v = vacio, expira = ahora + reintento)
    return(vacio)
  }
  .cache[[key]] <- list(v = v, expira = ahora + ttl)
  v
}

VACIO_RAW <- tibble(station_id = character(), nombre = character(),
                    mecanicas = double(), electricas = double(), capacidad = double(),
                    timestamp = as.POSIXct(character(), tz = TZ))

# Conserva la última medición de cada tramo de RESOLUCION_MIN minutos
adelgazar <- function(d) {
  if (nrow(d) == 0) return(d)
  snaps <- sort(unique(as.numeric(d$timestamp)))
  bin <- floor(snaps / (RESOLUCION_MIN * 60))
  keep <- snaps[!duplicated(bin, fromLast = TRUE)]
  d[as.numeric(d$timestamp) %in% keep, ]
}

leer_historico <- function(url) {
  tryCatch({
    d <- suppressWarnings(read_csv(url, show_col_types = FALSE, col_types = cols(
      station_id = col_character(),
      nombre = col_character(),
      mecanicas = col_double(),
      electricas = col_double(),
      capacidad = col_character(),
      timestamp = col_character(),
      .default = col_skip()
    )))
    if (!"capacidad" %in% names(d)) d$capacidad <- NA_character_
    
    # Filas de 5 campos (sin capacidad): el timestamp ha caído en la columna «capacidad»
    corta <- is.na(d$timestamp) & grepl("^[0-9]{4}-", d$capacidad)
    d$timestamp[corta] <- d$capacidad[corta]
    d$capacidad[corta] <- NA_character_
    
    d$capacidad <- suppressWarnings(as.numeric(d$capacidad))
    d$timestamp <- ymd_hms(d$timestamp, tz = TZ, quiet = TRUE)
    
    d <- d[!is.na(d$timestamp) & !is.na(d$mecanicas) & !is.na(d$electricas), ]
    adelgazar(d)
  }, error = function(e) {
    warning("No se pudo leer ", url, ": ", conditionMessage(e))
    NULL
  })
}

get_current <- function() {
  cache_get("actual", INTERVALO_MS / 1000 - 15, function() leer_historico(GITHUB_CSV_URL), VACIO_RAW)
}

get_archivo <- function(nombre) {
  cache_get(paste0("arch_", nombre), 1e9, function() leer_historico(paste0(ARCHIVO_URL, "/", nombre)), VACIO_RAW)
}

get_indice <- function() {
  cache_get("indice", 1800, function() {
    tryCatch({
      x <- trimws(suppressWarnings(readLines(paste0(ARCHIVO_URL, "/indice.txt"), warn = FALSE)))
      x[grepl("^historico_[0-9]{4}-[0-9]{2}\\.csv$", x)]
    }, error = function(e) NULL)
  }, character(0), reintento = 300)
}

VACIO_META <- tibble(station_id = character(), lat = double(), lon = double(),
                     capacidad = double(), barrio = character())

leer_meta_csv <- function(url) {
  tryCatch({
    x <- read_csv(url, show_col_types = FALSE, col_types = cols(.default = col_character()))
    for (nm in c("lat", "lon", "capacidad", "barrio")) if (!nm %in% names(x)) x[[nm]] <- NA_character_
    tibble(station_id = trimws(x$station_id),
           lat = suppressWarnings(as.numeric(x$lat)), lon = suppressWarnings(as.numeric(x$lon)),
           capacidad = suppressWarnings(as.numeric(x$capacidad)),
           barrio = na_if(trimws(x$barrio), ""))
  }, error = function(e) NULL)
}

leer_meta_legacy <- function(url) {
  tryCatch({
    x <- read_csv(url, show_col_types = FALSE, col_types = cols(
      puesto = col_character(), lat = col_double(), lon = col_double(),
      libres = col_double(), mecanicas = col_double(), electricas = col_double(),
      barrio = col_character(), .default = col_skip()))
    x %>% group_by(station_id = puesto) %>%
      summarise(lat = first(lat), lon = first(lon),
                capacidad = suppressWarnings(max(libres + mecanicas + electricas, na.rm = TRUE)),
                barrio = first(barrio[!is.na(barrio) & barrio != ""], default = NA_character_),
                .groups = "drop") %>%
      mutate(capacidad = ifelse(is.finite(capacidad), capacidad, NA_real_))
  }, error = function(e) NULL)
}

leer_meta_gbfs <- function(url) {
  tryCatch({
    s <- jsonlite::fromJSON(url, simplifyVector = TRUE)$data$stations
    tibble(station_id = as.character(s$station_id),
           lat        = suppressWarnings(as.numeric(s[["lat"]] %||% NA_real_)),
           lon        = suppressWarnings(as.numeric(s[["lon"]] %||% NA_real_)),
           capacidad  = suppressWarnings(as.numeric(s[["capacity"]] %||% NA_real_)),
           barrio     = NA_character_)
  }, error = function(e) {
    warning("No se pudo leer ", url, ": ", conditionMessage(e))
    NULL
  })
}

get_meta <- function() {
  cache_get("meta", 3600, function() {
    m <- leer_meta_gbfs(STATION_INFO_URL)
    if (is.null(m)) m <- leer_meta_legacy(LEGACY_URL)
    if (is.null(m)) return(NULL)
    m$barrio <- canonico_barrio(m$barrio, GEO)
    m$barrio <- coalesce(m$barrio, asignar_barrio(m$lat, m$lon, GEO))
    m
  }, VACIO_META)
}

# ---------------------------------------------------------------------------
# Barrios: GeoJSON + asignación punto-en-polígono (R base, sin paquete sf)
# ---------------------------------------------------------------------------

nombre_barrio <- function(x) {
  x <- tools::toTitleCase(tolower(trimws(x)))
  x <- gsub("(?<=\\s)(De|Del)(?=\\s)", "\\L\\1", x, perl = TRUE)
  x
}

# Versión ligera solo para dibujar: vértices a ~10 m y sin duplicados consecutivos
geometria_ligera <- function(polys_m) {
  ring_ligero <- function(m) {
    r <- round(m, 4)
    r <- r[c(TRUE, rowSums(abs(diff(r))) > 0), , drop = FALSE]
    if (nrow(r) < 4) r <- round(m, 5)
    lapply(seq_len(nrow(r)), function(k) r[k, ])
  }
  polys <- lapply(polys_m, function(poly) lapply(poly, ring_ligero))
  if (length(polys) == 1) list(type = "Polygon", coordinates = polys[[1]])
  else list(type = "MultiPolygon", coordinates = polys)
}

cargar_geo <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch({
    g <- jsonlite::fromJSON(path, simplifyVector = FALSE)
    feats <- lapply(seq_along(g$features), function(i) {
      f <- g$features[[i]]
      coords <- f$geometry$coordinates
      polys <- if (f$geometry$type == "Polygon") list(coords) else coords
      polys_m <- lapply(polys, function(poly) lapply(poly, function(ring)
        do.call(rbind, lapply(ring, function(pt) c(pt[[1]], pt[[2]])))))
      nom <- nombre_barrio(f$properties$NOMBRE_BAR)
      list(id = as.character(i), nombre = nom, extrarradio = identical(nom, EXTRARRADIO),
           polys = polys_m,
           geometry = geometria_ligera(polys_m))
    })
    list(feats = feats, nombres = unique(vapply(feats, function(f) f$nombre, "")))
  }, error = function(e) { warning("No se pudo leer ", path, ": ", conditionMessage(e)); NULL })
}

GEO <- cargar_geo(BARRIOS_GEOJSON)

# Ray casting vectorizado sobre los puntos (x = lon, y = lat)
pip_anillo <- function(x, y, anillo) {
  dentro <- rep(FALSE, length(x)); n <- nrow(anillo); j <- n
  for (i in seq_len(n)) {
    xi <- anillo[i, 1]; yi <- anillo[i, 2]; xj <- anillo[j, 1]; yj <- anillo[j, 2]
    if (yi != yj) {
      cruza <- ((yi > y) != (yj > y)) & (x < (xj - xi) * (y - yi) / (yj - yi) + xi)
      dentro <- xor(dentro, cruza)
    }
    j <- i
  }
  dentro
}

pip_poligono <- function(x, y, poly) {   # primer anillo = exterior; resto = huecos
  d <- pip_anillo(x, y, poly[[1]])
  for (h in poly[-1]) d <- d & !pip_anillo(x, y, h)
  d
}

asignar_barrio <- function(lat, lon, geo) {
  res <- rep(NA_character_, length(lat))
  ok <- !is.na(lat) & !is.na(lon)
  if (is.null(geo) || !any(ok)) return(res)
  orden <- order(vapply(geo$feats, function(f) f$extrarradio, TRUE))   # barrios antes que extrarradio
  for (k in orden) {
    f <- geo$feats[[k]]
    for (poly in f$polys) {
      sel <- ok & is.na(res)
      if (any(sel)) res[sel][pip_poligono(lon[sel], lat[sel], poly)] <- f$nombre
    }
  }
  res[ok & is.na(res)] <- FUERA_BARRIO
  res
}

# Alinea nombres puestos a mano (estaciones.csv) con los del GeoJSON, sin distinguir mayúsculas
canonico_barrio <- function(x, geo) {
  if (is.null(geo)) return(x)
  m <- match(tolower(x), tolower(geo$nombres))
  ifelse(is.na(m), x, geo$nombres[m])
}

# ---------------------------------------------------------------------------
# Procesado
# ---------------------------------------------------------------------------

procesar <- function(d, meta) {
  if (nrow(d) == 0) {
    d <- d %>% mutate(barrio = character(), lat = double(), lon = double(), capacidad = double(),
                      total = double(), pct_ocup = double(), vacia = logical(), le10 = logical(),
                      le20 = logical(), le30 = logical(), le50 = logical(), sinelec = logical(),
                      le2elec = logical(), banda = factor(character(), levels = names(COL_BANDAS)),
                      peso = double())
    return(d)
  }
  d <- d %>% arrange(timestamp, station_id)
  
  # peso temporal de cada medición = minutos hasta la siguiente (con tope)
  snaps <- sort(unique(as.numeric(d$timestamp)))
  dt <- c(diff(snaps) / 60, RESOLUCION_MIN)
  peso <- pmin(dt, GAP_MAX_MIN)
  d$peso <- peso[match(as.numeric(d$timestamp), snaps)]
  
  obs <- d %>% group_by(station_id) %>%
    summarise(obs_max = max(mecanicas + electricas),
              cap_hist = suppressWarnings(max(capacidad, na.rm = TRUE)), .groups = "drop") %>%
    mutate(cap_hist = ifelse(is.finite(cap_hist), cap_hist, NA_real_))
  d %>%
    select(-any_of(c("capacidad", "lat", "lon", "barrio"))) %>%
    left_join(obs, by = "station_id") %>%
    left_join(meta %>% select(station_id, lat, lon, capacidad, barrio), by = "station_id") %>%
    mutate(
      barrio = coalesce(barrio, SIN_BARRIO),
      capacidad = pmax(coalesce(cap_hist, capacidad), obs_max, na.rm = TRUE),
      capacidad = ifelse(capacidad <= 0, NA_real_, capacidad),
      total = mecanicas + electricas,
      pct_ocup = 100 * total / capacidad,
      vacia = total == 0,
      le10 = vacia | coalesce(pct_ocup <= 10, FALSE),
      le20 = vacia | coalesce(pct_ocup <= 20, FALSE),
      le30 = vacia | coalesce(pct_ocup <= 30, FALSE),
      le50 = vacia | coalesce(pct_ocup <= 50, FALSE),
      sinelec = electricas == 0,
      le2elec = electricas <= 2,
      banda = banda_exclusiva(pct_ocup)
    ) %>%
    select(-obs_max, -cap_hist)
}

filtrar_periodo <- function(d, periodo, anchor) {
  if (periodo == "actual") return(d[d$timestamp == anchor, ])
  if (periodo == "todo") return(d)
  d[d$timestamp > anchor - SEG_PERIODO[[periodo]], ]
}

# Promedios del periodo ponderados por tiempo. Para "Dato actual" son valores exactos.
calc_kpis <- function(d) {
  if (nrow(d) == 0) return(NULL)
  pt <- sum(d$peso[!duplicated(d$timestamp)])
  f <- function(x) sum(d$peso * x, na.rm = TRUE) / pt
  # Ocupación = bicis / capacidad estimada (solo paradas con capacidad), ponderada por tiempo
  cap_w <- sum(d$peso * d$capacidad, na.rm = TRUE)
  con_cap <- !is.na(d$capacidad)
  occ <- function(x) if (cap_w > 0) 100 * sum(d$peso * x * con_cap, na.rm = TRUE) / cap_w else NA_real_
  list(n = n_distinct(d$station_id), n_med = n_distinct(d$timestamp), prom_est = f(1),
       bicis = f(d$total), elec = f(d$electricas), mec = f(d$mecanicas),
       vacias = f(d$vacia), le10 = f(d$le10), le20 = f(d$le20), le30 = f(d$le30),
       le50 = f(d$le50), sinelec = f(d$sinelec), le2elec = f(d$le2elec),
       occ_tot = occ(d$total), occ_elec = occ(d$electricas), occ_mec = occ(d$mecanicas))
}

resumen_snap <- function(d) {
  d %>% group_by(timestamp) %>%
    summarise(n_est = n(), bicis_mec = sum(mecanicas), bicis_elec = sum(electricas),
              bicis_tot = sum(total), cap = sum(capacidad, na.rm = TRUE),
              across(c(vacia, le10, le20, le30, le50, sinelec), ~ 100 * mean(.x, na.rm = TRUE)),
              .groups = "drop") %>%
    mutate(cap = na_if(cap, 0),
           occ_mec = 100 * bicis_mec / cap, occ_elec = 100 * bicis_elec / cap,
           occ_tot = 100 * bicis_tot / cap)
}

unidad_auto <- function(t_ini, t_fin) {
  h <- as.numeric(difftime(t_fin, t_ini, units = "hours"))
  if (h <= 36) "5 minutes" else if (h <= 8 * 24) "1 hour" else if (h <= 40 * 24) "3 hours" else "1 day"
}

serie_bin <- function(snap, unidad) {
  snap %>% mutate(t = floor_date(timestamp, unidad)) %>% group_by(t) %>%
    summarise(across(-timestamp, ~ mean(.x, na.rm = TRUE)), .groups = "drop")
}

# Movimientos brutos de bicis entre mediciones consecutivas de cada parada
actividad_df <- function(d) {
  d %>% arrange(station_id, timestamp) %>% group_by(station_id) %>%
    mutate(dif = total - lag(total), ok = lag(peso) < GAP_MAX_MIN) %>% ungroup() %>%
    filter(!is.na(dif), ok) %>% group_by(timestamp) %>%
    summarise(retiradas = sum(pmax(-dif, 0)), devoluciones = sum(pmax(dif, 0)), .groups = "drop")
}

hay_datos <- function(d, msg = "Sin datos para este periodo o ámbito.") validate(need(nrow(d) > 0, msg))

kpi_card <- function(titulo, valor, sub = NULL, ayuda = NULL, clase = NULL) {
  div(class = paste("kpi-box", clase), title = ayuda,
      div(class = "etiqueta", titulo),
      div(class = "valor", valor),
      div(class = "sub", sub %||% HTML("&nbsp;")))
}

# ---------------------------------------------------------------------------
# Las 3 donas de cabecera: componentes comunes
# ---------------------------------------------------------------------------

# Tarjeta común a las 3 donas: título, subtítulo, dona con valor central (solo el dato) y leyenda
gauge_card <- function(titulo, sub, id, centro_val, leyenda_id) {
  div(class = "gauge-box",
      div(class = "etiqueta", titulo),
      div(class = "sub-gauge", sub),
      div(class = "gauge-cuerpo",
          div(class = "dona",
              plotlyOutput(id, width = "100%", height = "100%"),
              div(class = "dona-centro", tags$b(centro_val))),
          uiOutput(leyenda_id, class = "dl-wrap")))
}

# Color de texto legible (oscuro/claro) según el fondo de cada porción
color_texto <- function(hex) {
  rgb <- grDevices::col2rgb(hex)
  lum <- (0.299 * rgb[1, ] + 0.587 * rgb[2, ] + 0.114 * rgb[3, ]) / 255
  ifelse(lum > 0.6, "#0f172a", "#ffffff")
}

# Dona con el mismo estilo para las 3. Los valores van dentro del anillo, pero solo en las porciones
# con peso suficiente (min_frac); las porciones pequeñas llevan la etiqueta fuera del anillo.
dona_plotly <- function(df, colores, hover, etiquetas = NULL, min_frac = 0.06) {
  frac <- df$n / sum(df$n)
  txt  <- if (is.null(etiquetas)) rep("", nrow(df)) else etiquetas
  validos <- !is.na(df$n) & round(df$n, 1) > 0
  txt[!validos] <- ""                       # valor 0: sin etiqueta
  if (sum(validos) <= 1) txt[] <- ""        # una sola categoría: solo el color, sin dato
  pos  <- ifelse(!is.na(frac) & frac >= min_frac, "inside", "outside")   # pequeñas -> fuera
  plot_ly(df, labels = ~cat, values = ~n, type = "pie", hole = 0.4, sort = FALSE,
          direction = "clockwise", hovertemplate = hover,
          text = txt, textinfo = "text", textposition = pos, insidetextorientation = "horizontal",
          insidetextfont  = list(size = 13, color = color_texto(colores)),
          outsidetextfont = list(size = 12, color = "#0f172a"),
          marker = list(colors = colores, line = list(color = "#fff", width = 2))) %>%
    layout(showlegend = FALSE, margin = list(t = 20, b = 20, l = 20, r = 20), separators = SEP_ES,
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Leyenda lateral solo con nombres. items: lista de list(color, etq)
leyenda_dona <- function(items) {
  div(class = "dl",
      lapply(items, function(it)
        div(class = "dl-fila",
            tags$i(style = paste0("background:", it$color, ";")),
            tags$span(class = "n", it$etq))))
}

# ---------------------------------------------------------------------------
# Fuente, autor y metodología (textos visibles en la app)
# ---------------------------------------------------------------------------

ENLACE_X     <- tags$a(href = "https://x.com/serherval", target = "_blank", rel = "noopener", "@serherval")
ENLACE_EMAIL <- tags$a(href = "mailto:sergioh93@gmail.com", "sergioh93@gmail.com")

# Pie visible en todas las pestañas
PIE <- div(class = "pie",
           div(tags$b("Fuente: "), "feed GBFS público de BIKI Valladolid (datos abiertos del servicio) · zonas: polígonos de barrios del Ayuntamiento de Valladolid · Elaboración propia."),
           div(tags$b("Autor: "), "Sergio Hernández · X: ", ENLACE_X, " · ", ENLACE_EMAIL, " · ",
               actionLink("ir_metodologia_pie", "Metodología completa"))
)

METODOLOGIA <- div(class = "metodo",
                   div(class = "banner", HTML("<b>En una frase.</b> Todas las cifras describen la <b>disponibilidad de bicis en las estaciones</b> (cuántas están vacías o casi vacías, qué parte de sus anclajes ocupa una bici y cómo cambia con la hora, el día o la zona). Los periodos largos son <b>promedios ponderados por tiempo</b> y la <b>capacidad</b> de cada estación es una <b>estimación</b> (ver apartado 4).")),
                   
                   tags$h4("1. Fuente de los datos", class = "pregunta"),
                   tags$ul(
                     tags$li(HTML("<b>Disponibilidad:</b> feed GBFS público de BIKI Valladolid (<code>station_status</code>): bicis mecánicas y eléctricas disponibles en cada estación.")),
                     tags$li("Una tarea automática (GitHub Actions) captura el feed cada pocos minutos y guarda el histórico en ficheros CSV del repositorio github.com/serherval/flujosbiki. Esta app solo lee esos ficheros. De cada tramo de 5 minutos se conserva la última medición."),
                     tags$li("Zonas: polígonos de barrios del Ayuntamiento de Valladolid. Coordenadas de las estaciones: ficheros auxiliares del proyecto."),
                     tags$li("La app busca datos nuevos cada 5 minutos, aunque la fuente en GitHub puede tardar unos minutos en reflejarlos. El último dato disponible y el primero del histórico se indican en la parte superior.")
                   ),
                   
                   tags$h4("2. Filtros y periodos", class = "pregunta"),
                   tags$ul(
                     tags$li("Los filtros de Zona, Estación y Periodo de observación son comunes a todas las pestañas. Si eliges una estación, todas las cifras y gráficos se refieren solo a ella."),
                     tags$li(HTML("<b>«Dato actual»</b> es la última medición recibida (valores exactos). <b>Últimas 24 horas, 7 días y 30 días</b> se cuentan hacia atrás desde esa última medición; «Desde inicio del histórico» usa todos los datos disponibles.")),
                     tags$li("En la pestaña Evolutivos «Dato actual» no tiene evolución que mostrar, así que en ella se usan las últimas 24 horas."),
                     tags$li("En el resto de periodos, cada cifra es el promedio ponderado por tiempo de todas las mediciones: cada medición pesa lo que dura hasta la siguiente, con un máximo de 15 minutos, para que un hueco en la captura no se cuente como si la situación se hubiera mantenido horas."),
                     tags$li("Ejemplo: «Puestos vacíos 3,2» significa que, de media, había 3,2 estaciones vacías. Si el histórico es más corto que el periodo elegido, se promedia solo el tiempo disponible y la app lo avisa."),
                     tags$li("«Estaciones» es el número de estaciones con datos en el periodo y el ámbito elegidos."),
                     tags$li("Todas las cifras se muestran con coma decimal y, como máximo, un decimal (sin decimal cuando es 0).")
                   ),
                   
                   tags$h4("3. Definiciones", class = "pregunta"),
                   tags$ul(
                     tags$li(HTML("<b>Estación vacía:</b> estación con 0 bicis (mecánicas + eléctricas).")),
                     tags$li(HTML("<b>% de ocupación:</b> bicis disponibles / capacidad de la estación. Para una zona o para toda la red: suma de bicis / suma de capacidades. La <b>ocupación total, eléctrica y mecánica</b> son ese mismo cociente contando todas las bicis, solo las eléctricas o solo las mecánicas, siempre sobre la capacidad total.")),
                     tags$li(HTML("<b>Puestos ≤10 %, ≤20 %, ≤30 %:</b> estaciones con como máximo ese porcentaje de sus anclajes ocupados por una bici. Son umbrales acumulativos: incluyen las vacías.")),
                     tags$li(HTML("<b>Estaciones sin eléctricas:</b> estaciones con 0 bicis eléctricas, aunque tengan mecánicas.")),
                     tags$li(HTML("<b>Reparto por ocupación:</b> bandas excluyentes (0-10 %, 11-20 %, 21-30 %, 31-50 % y >50 %); cada estación cae en una sola. El gráfico muestra el número de estaciones de cada banda (promedio en los periodos largos).")),
                     tags$li("Las estaciones que nunca han tenido una bici en los datos cargados no tienen capacidad estimada: cuentan como vacías, pero quedan fuera de las bandas de ocupación y de la ocupación media.")
                   ),
                   
                   tags$h4("4. Capacidad de las estaciones (estimada)", class = "pregunta"),
                   tags$ul(
                     tags$li("El feed no informa del número de anclajes. Para cada estación se estima como el mayor de dos valores: (a) bicis + anclajes libres visto en un fichero auxiliar anterior del proyecto y (b) el máximo de bicis observado en esa estación en el histórico cargado."),
                     tags$li("Es una estimación, no un dato oficial. Suele quedarse algo corta, de modo que la ocupación puede salir algo sobrevalorada."),
                     tags$li("Como (b) depende del histórico cargado, al ampliar el periodo la capacidad estimada puede subir ligeramente y, con ella, cambiar un poco la ocupación.")
                   ),
                   
                   tags$h4("5. Zonas", class = "pregunta"),
                   tags$ul(
                     tags$li("La zona de cada estación se obtiene de sus coordenadas: se busca en qué polígono de barrio queda. Si en el fichero auxiliar se rellena la zona a mano, esa asignación tiene prioridad."),
                     tags$li("Las estaciones dentro del polígono «Extrarradio» figuran con ese nombre (el mapa no lo colorea por su tamaño); las que quedan fuera de todo polígono, como Santovenia o La Cistérniga, figuran como «Otros municipios», y las que no tienen coordenadas, como «Sin asignar».")
                   ),
                   
                   tags$h4("6. Cómo leer cada gráfico", class = "pregunta"),
                   tags$ul(
                     tags$li(HTML("<b>Mapa de calor:</b> cada punto es una estación, con su ocupación media ponderada por tiempo; el color de cada zona es la suma de bicis entre la suma de capacidades de sus estaciones.")),
                     tags$li(HTML("<b>Problemas recurrentes (pestaña General):</b> % del tiempo del periodo en cada situación. Las situaciones se solapan: una estación vacía cuenta también como ≤10 % y ≤20 %. «Ocupación media» es bicis / capacidad ponderada por tiempo. Respeta los filtros de Zona, Estación y Periodo.")),
                     tags$li(HTML("<b>Evolución de las bicis (Evolutivos):</b> puede verse como número de bicis disponibles o como % de ocupación, para todas las bicis (mecánicas y eléctricas apiladas, más la línea de Total), solo mecánicas o solo eléctricas. Cada punto es la media de las mediciones del intervalo.")),
                     tags$li(HTML("<b>Resolución temporal (Evolutivos):</b> en automático, 5 minutos hasta 36 horas, 1 hora hasta 8 días, 3 horas hasta 40 días y 1 día a partir de ahí; también puede fijarse a mano.")),
                     tags$li(HTML("<b>¿A qué horas y qué días es más difícil encontrar bici?</b> Media por hora del día (hora peninsular) y día de la semana; laborable = lunes a viernes. «Bicis disponibles» es la suma de la zona o la red (o de la estación elegida) y «% Ocupación» es bicis / capacidad. Rojo = poca disponibilidad; verde = mucha.")),
                     tags$li(HTML("<b>Actividad estimada:</b> las barras hacia arriba son las devoluciones (subidas de bicis entre mediciones consecutivas de cada estación) y las barras hacia abajo, las retiradas (bajadas). La línea continua es el saldo neto (devoluciones - retiradas). Es un mínimo, porque una retirada y una devolución dentro del mismo tramo de 5 minutos se compensan, e incluye el reparto de bicis que hagan los operarios.")),
                     tags$li(HTML("<b>¿Está cambiando el problema con el tiempo?</b> % de estaciones en cada situación (vacías, ≤10 %, ≤20 % y sin eléctricas) en cada momento; con una estación concreta, % del tiempo."))
                   ),
                   
                   tags$h4("7. Limitaciones", class = "pregunta"),
                   tags$ul(
                     tags$li("La ausencia de bicis no implica averías: los datos describen disponibilidad, no la experiencia de las personas usuarias."),
                     tags$li("Con una medición cada 5 minutos, los movimientos más rápidos no se ven."),
                     tags$li("Si la captura falla durante un tiempo, ese tramo no tiene datos y no se rellena."),
                     tags$li("La capacidad es una estimación (apartado 4).")
                   ),
                   
                   tags$h4("8. Autor y contacto", class = "pregunta"),
                   div(class = "banner",
                       tags$b("Sergio Hernández"), " · X: ", ENLACE_X, " · ", ENLACE_EMAIL, br(),
                       "Si detectas un error o tienes una sugerencia, escríbeme.")
)

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------

ui <- fluidPage(
  title = "BIKI Valladolid — disponibilidad real",
  tags$head(tags$style(HTML("
    :root{--navy:#0f172a;--slate:#475569;--muted:#64748b;--line:#e2e8f0;--bg:#f8fafc;--green:#16a34a;--orange:#f59e0b;--blue:#2563eb;}
    body{font-family:-apple-system,BlinkMacSystemFont,Segoe UI,Helvetica,Arial,sans-serif;background:var(--bg);color:var(--navy);}
    .container-fluid{max-width:1440px;margin:0 auto;padding-left:22px;padding-right:22px;}
    h1.titulo{font-size:clamp(23px,4vw,34px);line-height:1.05;margin:0;font-weight:800;letter-spacing:-.035em;}
    p.subtitulo{color:#dbeafe;margin:7px 0 0;font-size:14px;}
    .hero{position:relative;overflow:hidden;border-radius:22px;padding:25px 28px 22px;margin:8px 0 16px;color:white;background-image:linear-gradient(90deg,rgba(15,23,42,.97) 0%,rgba(15,23,42,.82) 48%,rgba(22,101,52,.48) 100%),url('https://images.unsplash.com/photo-1758764048405-61378e2abbc5?auto=format&fit=crop&fm=jpg&ixlib=rb-4.1.0&q=80&w=1800');background-size:cover;background-position:center 55%;box-shadow:0 10px 35px rgba(15,23,42,.12);}
    .hero:after{content:'🚲  ⚡  🚲';position:absolute;right:24px;bottom:-18px;font-size:74px;opacity:.12;transform:rotate(-7deg);letter-spacing:14px;}
    .hero a{color:#bbf7d0;font-weight:700;}.hero-badge{display:inline-flex;background:rgba(255,255,255,.12);border:1px solid rgba(255,255,255,.16);padding:5px 10px;border-radius:999px;font-size:11px;font-weight:700;text-transform:uppercase;letter-spacing:.08em;margin-bottom:10px;}
    .filtros-global{background:white;border:1px solid var(--line);border-radius:16px;padding:13px 15px 2px;margin:0 0 15px;box-shadow:0 3px 12px rgba(15,23,42,.04);}
    .filtros-global label{font-weight:700;font-size:11px;color:var(--slate);text-transform:uppercase;letter-spacing:.05em;}.estado-datos{font-size:12px;color:var(--slate);padding-top:4px;}
    .banner{background:#eff6ff;border:1px solid #dbeafe;border-left:4px solid var(--blue);padding:10px 13px;border-radius:10px;margin:9px 0 16px;font-size:13px;}.banner.aviso{background:#fffbeb;border-color:#fde68a;border-left-color:#d97706;}
    h4.pregunta{margin-top:23px;margin-bottom:7px;font-weight:800;letter-spacing:-.01em;}.leyenda{color:var(--muted);font-size:12px;margin:-3px 0 8px;line-height:1.45;}
    .nav-tabs{border-bottom:1px solid var(--line);}.nav-tabs>li>a{font-weight:700;color:var(--slate);border:0;border-bottom:3px solid transparent;padding:11px 17px;}.nav-tabs>li.active>a,.nav-tabs>li.active>a:hover{color:var(--navy);background:transparent;border:0;border-bottom:3px solid var(--green);}
    .subtitulo a{font-weight:700;}.pie{margin:30px 0 16px;padding:14px 17px;background:#eef2f7;border-radius:12px;font-size:12px;color:#334155;line-height:1.7;}.metodo{max-width:920px;padding-bottom:8px;}.metodo li{margin-bottom:5px;}
    .kpi-donuts{display:grid;grid-template-columns:1fr;gap:12px;margin:0 auto 16px;max-width:560px;}.gauge-box{background:white;border:1px solid var(--line);border-radius:16px;padding:13px;display:flex;flex-direction:column;align-items:center;text-align:center;box-shadow:0 3px 12px rgba(15,23,42,.035);}.gauge-box .etiqueta{font-size:11px;font-weight:800;color:var(--slate);text-transform:uppercase;letter-spacing:.07em;}.gauge-box .sub-gauge{font-size:11px;color:var(--muted);margin:2px 0 7px;}.gauge-cuerpo{display:flex;flex-direction:row;align-items:center;justify-content:center;width:100%;gap:10px;}.gauge-cuerpo .dona{align-self:center;flex:0 0 auto;}.dona{position:relative;width:158px;aspect-ratio:1;flex:none;}.dona .html-widget{width:100%!important;height:100%!important;}.dona-centro{position:absolute;inset:0;display:flex;flex-direction:column;align-items:center;justify-content:center;pointer-events:none;line-height:1.05;}.dona-centro b{font-size:21px;font-weight:800;color:var(--navy);}.dl-wrap{align-self:center;justify-self:center;flex:0 1 auto;min-width:0;max-width:calc(100% - 145px);padding-left:0;margin:0;}.dl-wrap .dl{width:max-content;max-width:100%;margin:0;}.dl{display:grid;gap:5px 14px;font-size:12px;color:#334155;}.dl-fila{display:flex;align-items:center;gap:6px;min-width:0;}.dl-fila i{width:9px;height:9px;border-radius:50%;flex:none;box-shadow:inset 0 0 0 1px rgba(15,23,42,.12);}.dl-fila .n{white-space:nowrap;overflow:hidden;text-overflow:ellipsis;}
    .chip{display:inline-block;padding:3px 10px;border-radius:999px;font-weight:700;font-size:11px;}.chip.ok{background:#dcfce7;color:#166534;}.chip.stale{background:#fef3c7;color:#92400e;}.est-detalle{margin-top:3px;}.selectize-dropdown{z-index:2000!important;}
    .kpi-extra{display:flex;flex-direction:column;align-self:stretch;justify-content:center;gap:0;width:100%;min-width:0;margin:0;background:white;border:1px solid var(--line);border-radius:16px;overflow:hidden;box-shadow:0 3px 12px rgba(15,23,42,.035);}
.kpi-extra-box{flex:1;min-width:0;background:transparent;border:0;border-radius:0;padding:14px 10px;display:flex;flex-direction:column;align-items:center;justify-content:center;text-align:center;gap:5px;box-shadow:none;}
.kpi-extra-box + .kpi-extra-box{border-left:0;border-top:1px solid var(--line);}
.kpi-extra-box .ico{font-size:19px;line-height:1;filter:grayscale(.05);}
.kpi-extra-box .etiqueta{font-size:9px;font-weight:800;color:var(--slate);text-transform:uppercase;letter-spacing:.045em;white-space:normal;line-height:1.2;text-align:center;}
.kpi-extra-box .valor{font-size:21px;font-weight:800;line-height:1;color:var(--navy);margin:0;text-align:center;}
.kpi-extra-box .sub{font-size:10px;color:var(--muted);margin:0;white-space:nowrap;text-align:center;}
.kpi-extra-box.vacia{border-top:3px solid #dc2626;}
.kpi-extra-box.sinelec{border-top:3px solid var(--green);}
    .escala-mapa{display:flex;align-items:center;gap:8px;font-size:11px;color:var(--muted);margin:0 0 6px;}.escala-mapa .barra{flex:1;height:8px;border-radius:4px;}#g_mapa{min-height:400px;max-height:680px;}.selectize-input{border-radius:9px!important;border-color:#cbd5e1!important;}.form-control{border-radius:9px!important;}
    @media(min-width:900px){.kpi-donuts{grid-template-columns:minmax(0,1fr) minmax(155px,.78fr) repeat(2,minmax(0,1fr));max-width:none;align-items:stretch;gap:12px;}.dona{width:clamp(130px,13vw,175px);}.dona-centro b{font-size:22px;}.kpi-extra-box{padding:14px 9px;}}
@media(min-width:600px) and (max-width:899px){.kpi-donuts{grid-template-columns:repeat(2,minmax(0,1fr));gap:10px;max-width:none;}.kpi-extra{min-height:150px;}.dona{width:150px;}.dona-centro b{font-size:20px;}}

    @media(max-width:767px){.container-fluid{padding-left:12px;padding-right:12px;}.hero{padding:21px 19px;border-radius:17px;}.hero:after{font-size:52px;right:8px;}.selectize-input,.selectize-input input,.form-control,.dataTables_filter input{font-size:16px!important;}.nav-tabs{position:sticky;top:0;z-index:1020;background:var(--bg);}.nav-tabs>li>a{padding:12px 12px;}.filtros-grid{display:grid;grid-template-columns:1fr 1fr;gap:0 10px;}.filtros-grid .f-periodo,.filtros-grid .estado-datos{grid-column:1/-1;}.filtros-grid .estado-datos{padding-bottom:8px;}.filtros-grid .form-group{margin-bottom:9px;}.filtros-grid.ev{grid-template-columns:1fr;}.kpi-extra{display:grid;grid-template-columns:1fr 1fr;gap:0;margin-top:0;}.kpi-extra-box{padding:10px 7px;gap:5px;justify-content:center;align-items:center;text-align:center;}.kpi-extra-box + .kpi-extra-box{border-left:1px solid var(--line);border-top:0;}.kpi-extra-box.vacia{border-top:3px solid #dc2626;}.kpi-extra-box.sinelec{border-top:3px solid var(--green);}.kpi-extra-box .ico{font-size:18px;}.kpi-extra-box .etiqueta{font-size:9px;letter-spacing:.04em;}.kpi-extra-box .valor{font-size:19px;}.kpi-extra-box .sub{display:block;font-size:9px;white-space:normal;}.banner{font-size:12px;}table.dataTable th,table.dataTable td{padding:6px 8px!important;font-size:12px;}.dona{width:135px;}.dona-centro b{font-size:17px;}}
    @media(min-width:768px){.filtros-grid{grid-template-columns:repeat(3,1fr) auto;align-items:end;}.filtros-grid .f-periodo,.filtros-grid .estado-datos{grid-column:auto;}.filtros-grid .estado-datos{padding-bottom:12px;}.filtros-grid.ev{grid-template-columns:260px 1fr;}}
  "))),

  
  div(class = "hero",
      div(class = "hero-badge", "🚲  BIKI Valladolid · datos abiertos"),
      tags$h1("Disponibilidad real del servicio", class = "titulo"),
      tags$p(class = "subtitulo", "Dónde faltan bicis, cuándo ocurre y qué estaciones concentran el problema.",
             " · Elaboración propia de ", tags$b("Sergio Hernández"), " (", ENLACE_X, ")",
             " · ", actionLink("ir_metodologia", "metodología y fuentes"))
  ),
  
  # ---- Filtros globales: Zona, Parada y Periodo (aplican a todas las pestañas) ----
  div(class = "filtros-global", div(class = "filtros-grid",
                                    selectInput("barrio", "Zona", choices = c("Todas" = TODOS), width = "100%"),
                                    selectInput("parada", "Estación", choices = c("Todas" = TODOS), width = "100%"),
                                    div(class = "f-periodo",
                                        selectInput("periodo", "Periodo de observación",
                                                    choices = setNames(names(ETQ_PERIODO), unname(ETQ_PERIODO)), selected = "actual", width = "100%")),
                                    div(class = "estado-datos", uiOutput("estado_datos"))
  )),
  uiOutput("aviso_barrios"),


  tabsetPanel(id = "tabs", type = "tabs",
              
              # ======================= GENERAL =======================
              tabPanel("General",
                       br(),
                       uiOutput("banner_periodo"),
                       uiOutput("kpis"),
                       
                       fluidRow(column(12,
                                       tags$h4("Mapa de calor: % de ocupación por zona", class = "pregunta"),
                                       div(class = "leyenda", "Rojo = pocas bicis en relación con los anclajes; verde = muchas. Cada zona se colorea según la ocupación media de sus estaciones (puntos). Las estaciones fuera de las zonas (extrarradio u otros municipios) solo aparecen como punto. Con una zona seleccionada solo se ve esa zona."),
                                       uiOutput("escala_mapa"),
                                       plotlyOutput("g_mapa", height = "60vh")
                       )),
                       
                       tags$h4("Problemas recurrentes de disponibilidad", class = "pregunta"),
                       fluidRow(
                         column(5, radioButtons("agrupar", NULL, inline = TRUE,
                                                choices = c("Por zona" = "barrio", "Por estación" = "estacion"), selected = "estacion")),
                         column(7, div(class = "leyenda", "% del tiempo del periodo en cada situación. Las situaciones se solapan: una estación vacía cuenta también como ≤10% y ≤20%. «Ocupación media» = % medio de anclajes ocupados por una bici."))
                       ),
                       DTOutput("tabla_persistencia"),
                       br(),
                       
                       div(class = "banner", style = "margin-top: 16px;",
                           HTML("<b>Fuente y metodología.</b> Datos abiertos de BIKI Valladolid (feed GBFS), medidos cada pocos minutos. Las cifras de un periodo son promedios ponderados por tiempo y la capacidad de las estaciones es una estimación. "),
                           actionLink("ir_metodologia_gen", "Ver la metodología completa"))
              ),
              
              # ======================= EVOLUTIVOS =======================
              tabPanel("Evolutivos",
                       br(),
                       uiOutput("aviso_hist"),
                       div(class = "filtros-global", div(class = "filtros-grid ev",
                                                         selectInput("resolucion", "Resolución temporal", width = "100%",
                                                                     choices = c("Automática" = "auto", "5 minutos" = "5 minutes", "15 minutos" = "15 minutes",
                                                                                 "1 hora" = "1 hour", "6 horas" = "6 hours", "1 día" = "1 day")),
                                                         div(class = "estado-datos", uiOutput("ambito_hist"))
                       )),
                       
                       tags$h4("Evolución de las bicis disponibles", class = "pregunta"),
                       fluidRow(
                         column(5, radioButtons("metrica_evol", NULL, inline = TRUE,
                                                choices = c("Nº de bicis disponibles" = "bicis", "% de ocupación" = "occ"))),
                         column(7, radioButtons("tipo_bici_hist", NULL, inline = TRUE,
                                                choices = c("Todas" = "todas", "Mecánicas" = "mecanicas", "Eléctricas" = "electricas")))
                       ),
                       plotlyOutput("g_evol_bicis", height = 320),
                       
                       tags$h4("¿A qué horas y qué días es más difícil encontrar bici?", class = "pregunta"),
                       fluidRow(
                         column(4, selectInput("metrica_perfil", NULL,
                                               choices = c("% Ocupación" = "occ_tot", "Bicis disponibles" = "bicis_tot",
                                                           "Mecánicas disponibles" = "bicis_mec", "Eléctricas disponibles" = "bicis_elec")))
                       ),
                       fluidRow(
                         column(6, plotlyOutput("g_perfil_hora", height = 340)),
                         column(6, plotlyOutput("g_heat_dia_hora", height = 340))
                       ),
                       
                       tags$h4("Movimientos estimados de bicicletas", class = "pregunta"),
                       div(class = "leyenda", "Devoluciones por encima del eje 0 y retiradas por debajo; la línea es el saldo neto (devoluciones - retiradas). Es un mínimo (retirada y devolución dentro de un mismo tramo de 5 min se compensan) e incluye el reparto de bicis que hagan los operarios."),
                       plotlyOutput("g_actividad", height = 320),
                       
                       tags$h4("¿Está mejorando o empeorando la disponibilidad?", class = "pregunta"),
                       checkboxGroupInput("metricas_evol", NULL, inline = TRUE,
                                          choices = c("Vacías" = "vacia", "≤10%" = "le10", "≤20%" = "le20", "Sin eléctricas" = "sinelec"),
                                          selected = c("vacia", "le20")),
                       plotlyOutput("g_evolucion", height = 300),
                       br()
              ),
              
              # ======================= METODOLOGÍA =======================
              tabPanel("Metodología", METODOLOGIA)
  ),
  
  PIE
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------

server <- function(input, output, session) {
  
  tick <- reactiveTimer(INTERVALO_MS, session)
  memo <- new.env()
  
  # ---- Adaptación a pantallas estrechas: se usa el ancho real de cada gráfico ----
  es_movil <- function(id) (session$clientData[[paste0("output_", id, "_width")]] %||% 1000) < 600
  
  # Ajustes comunes de plotly. En móvil: sin barra de herramientas y sin arrastre/zoom, para que
  # el dedo desplace la página en vez de mover el gráfico (fijar = FALSE para el mapa, que sí se arrastra)
  fino <- function(p, id, fijar = TRUE) {
    movil <- es_movil(id)
    p <- p %>% config(displaylogo = FALSE, responsive = TRUE,
                      displayModeBar = if (movil) FALSE else "hover")
    if (movil && fijar)
      p <- p %>% layout(dragmode = FALSE, xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
    p
  }
  
  # Enlaces «Ver metodología» -> pestaña Metodología
  for (id in c("ir_metodologia", "ir_metodologia_gen", "ir_metodologia_pie")) local({
    id <- id
    observeEvent(input[[id]], updateTabsetPanel(session, "tabs", selected = "Metodología"))
  })
  
  cur    <- reactive({ tick(); get_current() })
  meta   <- reactive({ tick(); get_meta() })
  indice <- reactive({ tick(); get_indice() })
  
  barrio_sel <- reactive(input$barrio %||% TODOS)
  per <- reactive(input$periodo %||% "actual")             # periodo global
  # En Evolutivos «Dato actual» no tiene evolución: se usan las últimas 24 h
  per_ev <- reactive(if (per() == "actual") "24h" else per())
  es_actual <- reactive(per() == "actual")
  
  # ---- Carga (solo los meses archivados que hacen falta para el periodo) ----
  base <- reactive({
    cu <- cur(); idx <- indice(); me <- meta(); periodo <- per_ev()
    arch <- character(0)
    if (length(idx)) {
      meses <- sub("^historico_([0-9]{4}-[0-9]{2})\\.csv$", "\\1", idx)
      if (periodo == "todo") {
        arch <- idx
      } else {
        anchor <- if (nrow(cu) > 0) max(cu$timestamp) else {
          u <- get_archivo(idx[order(meses, decreasing = TRUE)][1])
          if (nrow(u) > 0) max(u$timestamp) else Sys.time()
        }
        ini <- anchor - SEG_PERIODO[[periodo]]
        arch <- idx[meses >= format(ini, "%Y-%m", tz = TZ)]
      }
    }
    key <- paste(paste(arch, collapse = ","), nrow(cu),
                 if (nrow(cu) > 0) as.numeric(max(cu$timestamp)) else 0,
                 sum(!is.na(me$barrio)), sum(me$capacidad, na.rm = TRUE))
    if (identical(memo$key, key)) return(memo$val)
    d <- bind_rows(c(lapply(arch, get_archivo), list(cu)))
    val <- procesar(d, me)
    memo$key <- key; memo$val <- val
    val
  })
  
  anchor <- reactive({ b <- base(); req(nrow(b) > 0); max(b$timestamp) })
  
  primer <- reactive({
    idx <- indice(); cu <- cur()
    if (length(idx)) {
      meses <- sub("^historico_([0-9]{4}-[0-9]{2})\\.csv$", "\\1", idx)
      a <- get_archivo(idx[order(meses)][1])
      if (nrow(a) > 0) return(min(a$timestamp))
    }
    if (nrow(cu) > 0) min(cu$timestamp) else as.POSIXct(NA)
  })
  
  # ---- Selectores ----------------------------------------------------------
  observe({
    p <- primer()
    lab <- if (is.na(p)) ETQ_PERIODO[["todo"]] else paste0(ETQ_PERIODO[["todo"]], " (", fmt_fecha(p), ")")
    ch <- c(setNames(names(ETQ_PERIODO)[1:4], unname(ETQ_PERIODO)[1:4]), setNames("todo", lab))
    updateSelectInput(session, "periodo", choices = ch, selected = isolate(input$periodo) %||% "actual")
  })
  
  barrios_disp <- reactive({
    b <- sort(unique(na.omit(meta()$barrio)))
    b[b != SIN_BARRIO]
  })
  
  observe({
    b <- barrios_disp()
    hay_sin <- any(base()$barrio == SIN_BARRIO)
    ch <- c("Todas" = TODOS)
    if (length(b)) ch <- c(ch, setNames(b, b), if (hay_sin) setNames(SIN_BARRIO, SIN_BARRIO))
    updateSelectInput(session, "barrio", choices = ch, selected = isolate(input$barrio) %||% TODOS)
  })
  
  # ---- Conjuntos de datos -------------------------------------------------
  d_bar <- reactive({
    b <- base()
    if (barrio_sel() != TODOS) b <- b[b$barrio == barrio_sel(), ]
    b
  })
  
  # Parada dentro de la zona: si la parada elegida no pertenece a la zona, se ignora
  parada_valida <- reactive({
    p <- input$parada %||% TODOS
    if (p != TODOS && p %in% d_bar()$station_id) p else TODOS
  })
  
  observe({
    e <- d_bar() %>% distinct(station_id, nombre) %>%
      mutate(.num = ifelse(grepl("^\\s*[0-9]+", nombre),
                           suppressWarnings(as.numeric(sub("^\\s*([0-9]+).*$", "\\1", nombre))), NA_real_)) %>%
      arrange(is.na(.num), .num, nombre) %>% select(-.num)
    sel <- isolate(input$parada) %||% TODOS
    if (!sel %in% e$station_id) sel <- TODOS
    updateSelectInput(session, "parada", choices = c("Todas" = TODOS, setNames(e$station_id, e$nombre)), selected = sel)
  })
  
  # Ámbito global: zona + parada
  d_amb <- reactive({
    d <- d_bar(); p <- parada_valida()
    if (p != TODOS) d <- d[d$station_id == p, ]
    d
  })
  
  d_per  <- reactive(filtrar_periodo(d_amb(), per(), anchor()))       # General
  d_hist <- reactive(filtrar_periodo(d_amb(), per_ev(), anchor()))    # Evolutivos
  
  kpis_per <- reactive(calc_kpis(d_per()))
  
  # ---- Textos de contexto ---------------------------------------------------
  output$estado_datos <- renderUI({
    invalidateLater(60 * 1000, session)      # el «hace X min» se refresca cada minuto
    cu <- cur(); p <- primer()
    if (nrow(cu) == 0) return(tags$span("No se han podido cargar datos de GitHub.", style = "color:#b91c1c;"))
    ult <- max(cu$timestamp)
    mins <- max(0, floor(as.numeric(difftime(Sys.time(), ult, units = "mins"))))
    hace <- if (mins < 1) "hace menos de 1 min"
    else if (mins < 60) paste0("hace ", mins, " min")
    else if (mins < 1440) paste0("hace ", floor(mins / 60), " h")
    else paste0("hace ", floor(mins / 1440), " d")
    tagList(
      tags$span(class = paste("chip", if (mins > 30) "stale" else "ok"), paste("Actualizado", hace)),
      div(class = "est-detalle",
          HTML(paste0("Último dato: <b>", fmt_fecha(ult), "</b> · Primer dato del histórico: ", fmt_fecha(p))))
    )
  })
  
  output$aviso_barrios <- renderUI({
    if (length(barrios_disp()) == 0)
      div(class = "banner aviso", HTML("No hay <b>zonas asignadas</b> a las estaciones, así que el filtro de zona solo ofrece «Todas». Deja <code>barrios_valladolid.geojson</code> junto a <code>app.R</code> (asigna el barrio por coordenadas) o rellena la columna <code>barrio</code> de <code>estaciones.csv</code>."))
  })
  
  ambito_txt <- reactive({
    p <- parada_valida()
    if (p != TODOS) paste("Estación:", d_bar()$nombre[match(p, d_bar()$station_id)])
    else if (barrio_sel() != TODOS) paste("Zona:", barrio_sel(), "(todas sus estaciones)")
    else "Toda la red"
  })
  
  output$banner_periodo <- renderUI({
    d <- d_per(); hay_datos(d)
    ambito <- sub("^Zona: (.*) \\(todas sus estaciones\\)$", "zona \\1", ambito_txt())
    ambito <- if (ambito == "Toda la red") "toda la ciudad" else sub("^Estación: ", "estación ", ambito)
    ini <- min(d$timestamp); fin <- max(d$timestamp); nm <- n_distinct(d$timestamp)
    txt <- if (es_actual()) {
      HTML(paste0("<b>Periodo de cálculo: Dato actual.</b> Las cifras muestran la situación en la última medición recibida (",
                  fmt_fecha(fin), "). Ámbito: ", ambito, "."))
    } else {
      HTML(paste0("<b>Periodo de cálculo: ", ETQ_PERIODO[[per()]], ".</b> Cada cifra es el <b>promedio</b> de las ", fmt_num(nm),
                  " mediciones entre ", fmt_fecha(ini), " y ", fmt_fecha(fin), ". Ejemplo: «Puestos vacíos 3,2» = de media había 3,2 estaciones vacías. Ámbito: ", ambito, "."))
    }
    horas_disp <- as.numeric(difftime(anchor(), primer(), units = "hours"))
    corto <- per() %in% names(SEG_PERIODO) && horas_disp < SEG_PERIODO[[per()]] / 3600 * 0.95
    div(class = "banner", txt,
        if (corto) tags$div(style = "margin-top:4px; color:#92400e;",
                            paste0("Ojo: el histórico solo cubre ", fmt_num(horas_disp), " h, así que el promedio se calcula solo con ese tiempo.")))
  })
  
  output$aviso_hist <- renderUI({
    horas_disp <- as.numeric(difftime(anchor(), primer(), units = "hours"))
    tagList(
      if (es_actual())
        div(class = "banner", "Con «Dato actual» no hay evolución que mostrar: en esta pestaña se representan las últimas 24 horas."),
      if (per_ev() %in% names(SEG_PERIODO) && isTRUE(horas_disp < SEG_PERIODO[[per_ev()]] / 3600 * 0.95))
        div(class = "banner aviso", paste0("Ojo: el histórico solo cubre ", fmt_num(horas_disp), " h, menos que el periodo elegido."))
    )
  })
  
  output$ambito_hist <- renderUI({
    d <- d_hist()
    if (nrow(d) == 0) return(HTML("Sin datos"))
    HTML(paste0("Ámbito: <b>", ambito_txt(), "</b><br>Periodo mostrado: ", fmt_fecha(min(d$timestamp)), " – ", fmt_fecha(max(d$timestamp))))
  })
  
  # ======================= GENERAL =======================================
  
  # Tabla compartida por la dona de bandas y su leyenda
  tab_bandas <- reactive({
    d <- d_per(); hay_datos(d)
    pt <- sum(d$peso[!duplicated(d$timestamp)])            # tiempo total del periodo (min)
    d <- d[!is.na(d$banda), ]; hay_datos(d)
    # Nº de estaciones en cada banda: exacto en «Dato actual», promedio ponderado por tiempo en el resto
    tibble(banda = factor(names(COL_BANDAS), levels = names(COL_BANDAS))) %>%
      left_join(d %>% group_by(banda) %>% summarise(w = sum(peso), .groups = "drop"), by = "banda") %>%
      mutate(n = coalesce(w, 0) / pt)
  })
  
  # Las tres tarjetas comparten estructura, tamaño de dona, colores y tipografía
  output$kpis <- renderUI({
    k <- kpis_per(); validate(need(!is.null(k), "Sin datos para este periodo o ámbito."))
    tot_b <- tryCatch(sum(tab_bandas()$n), error = function(e) NA_real_)
    div(class = "kpi-donuts",
        gauge_card("Estaciones", "Reparto por ocupación", "g_stacked",
                   fmt_num(tot_b), "leyenda_stacked"),
        div(class = "kpi-extra",
            div(class = "kpi-extra-box vacia",
                div(class = "ico", "🚫"),
                div(div(class = "etiqueta", "Estaciones vacías"),
                    div(class = "valor", fmt_num(k$vacias)),
                    div(class = "sub", if (es_actual()) "ahora" else "de media"))),
            div(class = "kpi-extra-box sinelec",
                div(class = "ico", "⚡"),
                div(div(class = "etiqueta", "Estaciones sin eléctricas"),
                    div(class = "valor", fmt_num(k$sinelec)),
                    div(class = "sub", if (es_actual()) "ahora" else "de media")))),
        gauge_card(if (es_actual()) "Bicis disponibles ahora" else "Bicis disponibles de media",
                   "Mecánicas y eléctricas", "gauge_bicis",
                   fmt_num(k$bicis), "leyenda_bicis"),
        gauge_card("Ocupación estimada", "Bicis sobre anclajes", "gauge_ocupacion",
                   fmt_pct(k$occ_tot, 0), "leyenda_ocupacion"))
  })
  
  # --- 1) Bicis disponibles: mecánicas / eléctricas
  output$gauge_bicis <- renderPlotly({
    k <- kpis_per(); validate(need(!is.null(k), "Sin datos para este periodo o ámbito."))
    df <- tibble(cat = c("Mecánicas", "Eléctricas"), n = c(k$mec, k$elec))
    dona_plotly(df, c(COL_MEC, COL_ELEC),
                paste0("%{label}: %{value:", TF_NUM, "} (%{percent})<extra></extra>"),
                etiquetas = fmt_num(df$n)) %>%
      fino("gauge_bicis", fijar = FALSE)
  })
  
  output$leyenda_bicis <- renderUI({
    leyenda_dona(list(list(color = COL_MEC, etq = "Mecánicas"),
                      list(color = COL_ELEC, etq = "Eléctricas")))
  })
  
  # --- 2) Ocupación estimada: mecánicas / eléctricas / libre
  output$gauge_ocupacion <- renderPlotly({
    k <- kpis_per(); validate(need(!is.null(k), "Sin datos para este periodo o ámbito."))
    df <- tibble(cat = c("Mecánicas", "Eléctricas", "Libre"),
                 n = c(k$occ_mec, k$occ_elec, max(0, 100 - k$occ_tot)))
    dona_plotly(df, c(COL_MEC, COL_ELEC, COL_LIBRE),
                "%{label}: %{value:,.0f}%<extra></extra>",
                etiquetas = fmt_pct(df$n, 0)) %>%
      fino("gauge_ocupacion", fijar = FALSE)
  })
  
  output$leyenda_ocupacion <- renderUI({
    leyenda_dona(list(list(color = COL_MEC, etq = "Mecánicas"),
                      list(color = COL_ELEC, etq = "Eléctricas"),
                      list(color = COL_LIBRE, etq = "Libre")))
  })
  
  # --- 3) Reparto por bandas de ocupación
  output$g_stacked <- renderPlotly({
    tab <- tab_bandas()
    df <- tibble(cat = as.character(tab$banda), n = tab$n)
    dona_plotly(df, unname(COL_BANDAS[df$cat]),
                paste0("%{label}: %{value:", TF_NUM, "} estaciones<extra></extra>"),
                etiquetas = fmt_num(df$n)) %>%
      fino("g_stacked", fijar = FALSE)
  })
  
  output$leyenda_stacked <- renderUI({
    tab <- tab_bandas()
    leyenda_dona(Map(function(b) list(color = COL_BANDAS[[b]], etq = b), as.character(tab$banda)))
  })
  
  output$g_mapa <- renderPlotly({
    d <- d_per(); hay_datos(d)
    movil <- es_movil("g_mapa")
    d <- d[!is.na(d$lat) & !is.na(d$lon), ]
    validate(need(nrow(d) > 0, "No hay coordenadas de las estaciones (ver estaciones.csv)."))
    ocup <- function(g) g %>% summarise(
      occ = 100 * sum(peso * total * (!is.na(capacidad))) / sum(peso * capacidad, na.rm = TRUE),
      n = n_distinct(station_id), .groups = "drop")
    est <- d %>% group_by(station_id, nombre, barrio, lat, lon) %>% ocup() %>% filter(is.finite(occ))
    validate(need(nrow(est) > 0, "Sin datos de ocupación."))
    est$hover <- paste0(est$nombre, "<br>", est$barrio, "<br>Ocupación: ", fmt_pct(est$occ))
    
    ext <- max(diff(range(est$lat)), diff(range(est$lon)) * 0.75)
    zoom <- if (ext < 0.01) 15 else if (ext < 0.03) 14 else if (ext < 0.06) 13 else 12
    
    bo <- d %>% filter(!barrio %in% c(SIN_BARRIO, FUERA_BARRIO)) %>% group_by(barrio) %>% ocup() %>% filter(is.finite(occ))
    bo$hover <- paste0("<b>", bo$barrio, "</b><br>Ocupación: ", fmt_pct(bo$occ), "<br>", fmt_num(bo$n), " estaciones")
    
    p <- plot_ly()
    if (!is.null(GEO) && nrow(bo) > 0) {
      # Polígonos reales, coloreados por la ocupación del barrio (el extrarradio no se pinta: es enorme)
      fs <- Filter(function(f) !f$extrarradio && f$nombre %in% bo$barrio, GEO$feats)
      if (length(fs) > 0) {
        gj <- list(type = "FeatureCollection", features = lapply(fs, function(f)
          list(type = "Feature", id = f$id, properties = list(nombre = f$nombre), geometry = f$geometry)))
        zz <- bo[match(vapply(fs, function(f) f$nombre, ""), bo$barrio), ]
        p <- p %>% add_trace(type = "choroplethmapbox", geojson = gj, locations = vapply(fs, function(f) f$id, ""),
                             z = zz$occ, zmin = 0, zmax = 100, colorscale = ESCALA_OCUP, showscale = FALSE,
                             marker = list(opacity = 0.55, line = list(width = 1.2, color = "#ffffff")),
                             text = zz$hover, hoverinfo = "text", showlegend = FALSE)
      }
    } else if (nrow(bo) > 0) {
      # Sin GeoJSON: círculo en el centro de las paradas de cada barrio
      bp <- est %>% filter(!barrio %in% c(SIN_BARRIO, FUERA_BARRIO)) %>% group_by(barrio) %>%
        summarise(lat = mean(lat), lon = mean(lon), .groups = "drop")
      bb <- inner_join(bo, bp, by = "barrio")
      p <- p %>% add_trace(type = "scattermapbox", mode = "markers", lat = bb$lat, lon = bb$lon,
                           marker = list(size = 42, opacity = 0.5, color = bb$occ, colorscale = ESCALA_OCUP,
                                         cmin = 0, cmax = 100, showscale = FALSE),
                           text = bb$hover, hoverinfo = "text", showlegend = FALSE)
    }
    p %>%
      add_trace(type = "scattermapbox", mode = "markers", lat = est$lat, lon = est$lon,
                marker = list(size = if (movil) 12 else 8, color = est$occ, colorscale = ESCALA_OCUP, cmin = 0, cmax = 100,
                              showscale = !movil, colorbar = list(title = "% ocup.", len = 0.6, ticksuffix = "%"),
                              opacity = 1),
                text = est$hover, hoverinfo = "text", showlegend = FALSE) %>%
      layout(mapbox = list(style = "white-bg", zoom = zoom,
                           center = list(lat = mean(range(est$lat)), lon = mean(range(est$lon))),
                           layers = list(list(below = "traces", sourcetype = "raster",
                                              sourceattribution = "Tiles © Esri",
                                              source = list(TILES_URL)))),
             margin = list(l = 0, r = 0, t = 0, b = 0)) %>%
      fino("g_mapa", fijar = FALSE)
  })
  
  # Leyenda de color del mapa para pantallas estrechas (en escritorio se usa la barra de color de plotly)
  output$escala_mapa <- renderUI({
    if (!es_movil("g_mapa")) return(NULL)
    grad <- paste0("linear-gradient(to right, ",
                   paste(vapply(ESCALA_OCUP, function(z) paste0(z[[2]], " ", z[[1]] * 100, "%"), ""), collapse = ", "), ")")
    div(class = "escala-mapa", tags$span("0 %"),
        div(class = "barra", style = paste0("background:", grad, ";")),
        tags$span("100 % ocupación"))
  })
  
  output$tabla_persistencia <- renderDT({
    d <- d_per(); hay_datos(d)
    resumir <- function(g) g %>% summarise(
      `Vacía` = 100 * sum(peso * vacia) / sum(peso),
      `≤10%` = 100 * sum(peso * le10) / sum(peso),
      `≤20%` = 100 * sum(peso * le20) / sum(peso),
      `Sin eléctricas` = 100 * sum(peso * sinelec) / sum(peso),
      `Ocupación media` = 100 * sum(peso * total * (!is.na(capacidad))) / sum(peso * capacidad, na.rm = TRUE),
      .groups = "drop")
    if ((input$agrupar %||% "estacion") == "barrio") {
      tab <- d %>% group_by(Zona = barrio) %>% resumir()
    } else {
      tab <- d %>% group_by(`Estación` = nombre, Zona = barrio) %>% resumir()
    }
    pcols <- c("Vacía", "≤10%", "≤20%", "Sin eléctricas")
    tab <- tab %>% mutate(across(all_of(c(pcols, "Ocupación media")), ~ round(.x, 1)))
    idx_pct <- which(names(tab) %in% c(pcols, "Ocupación media")) - 1     # columnas (base 0) que son %
    # Se muestra con coma, 1 decimal como máximo (sin ",0") y símbolo %; ordenar/filtrar usa el valor numérico
    render_pct <- JS("function(data, type, row, meta) {
      if (type !== 'display' || data === null || data === '') return data;
      var v = Math.round(parseFloat(data) * 10) / 10;
      return v.toLocaleString('es-ES', {minimumFractionDigits: 0, maximumFractionDigits: 1}) + '%';
    }")
    datatable(tab, rownames = FALSE, selection = "none",
              options = list(pageLength = 10, scrollX = TRUE,
                             order = list(list(which(names(tab) == "Vacía") - 1, "desc")),
                             columnDefs = list(list(targets = idx_pct, render = render_pct)),
                             language = list(search = "Buscar:", lengthMenu = "Mostrar _MENU_",
                                             info = "_START_–_END_ de _TOTAL_", emptyTable = "Sin datos",
                                             zeroRecords = "Sin resultados",
                                             paginate = list(previous = "Anterior", `next` = "Siguiente")))) %>%
      formatStyle(pcols, background = styleColorBar(c(0, 100), "#fecaca"),
                  backgroundSize = "100% 80%", backgroundRepeat = "no-repeat", backgroundPosition = "center") %>%
      formatStyle("Ocupación media", background = styleColorBar(c(0, 100), "#bbf7d0"),
                  backgroundSize = "100% 80%", backgroundRepeat = "no-repeat", backgroundPosition = "center")
  })
  


  # ======================= EVOLUTIVOS ====================================
  
  snap_hist <- reactive(resumen_snap(d_hist()))
  
  unidad_hist <- reactive({
    sn <- snap_hist()
    if (nrow(sn) == 0) return("1 hour")
    r <- input$resolucion %||% "auto"
    if (r == "auto") unidad_auto(min(sn$timestamp), max(sn$timestamp)) else r
  })
  
  y_pct_lab <- reactive(if (parada_valida() == TODOS) "% de estaciones" else "% del tiempo")
  
  # Evolución de las bicis: nº de bicis o % de ocupación, por tipo de bici
  output$g_evol_bicis <- renderPlotly({
    sn <- snap_hist(); hay_datos(sn)
    m <- input$metrica_evol %||% "bicis"
    tipo <- input$tipo_bici_hist %||% "todas"
    if (m == "occ") validate(need(any(!is.na(sn$occ_tot)), "No hay capacidad estimada para calcular la ocupación."))
    s <- serie_bin(sn, unidad_hist())
    # Eje X como texto en hora local: así el rango fijo coincide exactamente con los datos
    xs <- format(s$t, "%Y-%m-%d %H:%M:%S", tz = TZ)
    cm <- if (m == "bicis") "bicis_mec" else "occ_mec"
    ce <- if (m == "bicis") "bicis_elec" else "occ_elec"
    ct <- if (m == "bicis") "bicis_tot" else "occ_tot"
    suf <- if (m == "occ") "%" else ""
    p <- plot_ly()
    etq_ult <- function(v, nombre) c(rep("", length(v) - 1), paste0(nombre, ": ", fmt_num(v[length(v)]), suf))
    if (tipo == "todas") {
      # Área apilada (mecánicas + eléctricas) y línea de Total
      p <- p %>%
        add_trace(x = xs, y = s[[cm]], name = "Mecánicas", type = "scatter", mode = "lines", stackgroup = "one",
                  fillcolor = "rgba(249,115,22,.55)", line = list(color = COL_MEC, width = 2),
                  hovertemplate = ht("Mecánicas", suf)) %>%
        add_trace(x = xs, y = s[[ce]], name = "Eléctricas", type = "scatter", mode = "lines", stackgroup = "one",
                  fillcolor = "rgba(22,163,74,.55)", line = list(color = COL_ELEC, width = 2),
                  hovertemplate = ht("Eléctricas", suf)) %>%
        add_trace(x = xs, y = s[[ct]], name = "Total", type = "scatter", mode = "lines+text",
                  line = list(color = "#0f172a", width = 2), text = etq_ult(s[[ct]], "Total"),
                  textposition = "top left", cliponaxis = FALSE, hovertemplate = ht("Total", suf))
    } else {
      # Mismo estilo que «Todas»: área rellena, línea de 2 px y etiqueta del último valor
      es_mec <- tipo == "mecanicas"
      cc  <- if (es_mec) cm else ce
      nom <- if (es_mec) "Mecánicas" else "Eléctricas"
      p <- p %>% add_trace(x = xs, y = s[[cc]], name = nom, type = "scatter", mode = "lines+text",
                           fill = "tozeroy", fillcolor = if (es_mec) "rgba(249,115,22,.55)" else "rgba(22,163,74,.55)",
                           line = list(color = if (es_mec) COL_MEC else COL_ELEC, width = 2),
                           text = etq_ult(s[[cc]], nom), textposition = "top left", cliponaxis = FALSE,
                           hovertemplate = ht(nom, suf))
    }
    ytit <- if (m == "occ") "% de ocupación (bicis / capacidad)"
    else if (parada_valida() == TODOS) "Bicis disponibles (suma del ámbito)" else "Bicis disponibles"
    # Escala Y común a Todas / Mecánicas / Eléctricas (la del total), eje X ajustado a los datos
    # y márgenes fijos: al cambiar de vista el gráfico no se mueve ni cambia de anchura
    ymax <- suppressWarnings(max(s[[ct]], na.rm = TRUE)); if (!is.finite(ymax) || ymax <= 0) ymax <- 1
    p %>% layout(yaxis = list(title = ytit, range = c(0, ymax * 1.12), tickformat = TF_NUM, ticksuffix = suf,
                              automargin = FALSE),
                 xaxis = list(title = "", type = "date", range = c(xs[1], xs[length(xs)]),
                              hoverformat = HF_FECHA, automargin = FALSE),
                 hovermode = "x unified",
                 margin = list(t = 10, l = 70, r = 40, b = 80), legend = LEG_H, separators = SEP_ES) %>%
      fino("g_evol_bicis")
  })
  
  output$g_evolucion <- renderPlotly({
    sn <- snap_hist(); hay_datos(sn)
    ms <- input$metricas_evol; validate(need(length(ms) > 0, "Selecciona al menos una métrica."))
    s <- serie_bin(sn, unidad_hist())
    etq <- c(vacia = "Vacías", le10 = "≤10%", le20 = "≤20%", sinelec = "Sin eléctricas")
    p <- plot_ly()
    for (m in ms) p <- add_trace(p, x = s$t, y = s[[m]], type = "scatter", mode = "lines", name = etq[[m]],
                                 hovertemplate = ht(etq[[m]], "%"))
    p %>% layout(yaxis = list(title = y_pct_lab(), rangemode = "tozero", tickformat = TF_NUM, ticksuffix = "%"),
                 xaxis = list(title = "", hoverformat = HF_FECHA), hovermode = "x unified",
                 margin = list(t = 10), legend = LEG_H, separators = SEP_ES) %>%
      fino("g_evolucion")
  })
  
  perfil_base <- reactive({
    sn <- snap_hist()
    sn %>% mutate(hora = hour(timestamp),
                  dia = wday(timestamp, week_start = 1),
                  tipo_dia = ifelse(dia >= 6, "Fin de semana", "Laborable"))
  })
  etq_metrica <- c(occ_tot = "% Ocupación", bicis_tot = "Bicis disponibles",
                   bicis_mec = "Mecánicas disponibles", bicis_elec = "Eléctricas disponibles")
  
  output$g_perfil_hora <- renderPlotly({
    pb <- perfil_base(); hay_datos(pb)
    m <- input$metrica_perfil %||% "occ_tot"
    suf <- if (m == "occ_tot") "%" else ""
    df <- pb %>% group_by(tipo_dia, hora) %>% summarise(val = mean(.data[[m]], na.rm = TRUE), .groups = "drop")
    plot_ly(df, x = ~hora, y = ~val, color = ~tipo_dia, text = ~tipo_dia, type = "scatter", mode = "lines+markers",
            colors = c("Laborable" = "#2563eb", "Fin de semana" = "#f97316"),
            hovertemplate = paste0("%{text}<br>%{x}h: %{y:", TF_NUM, "}", suf, "<extra></extra>")) %>%
      layout(xaxis = list(title = "Hora del día", dtick = if (es_movil("g_perfil_hora")) 4 else 2, range = c(-0.5, 23.5)),
             yaxis = list(title = etq_metrica[[m]], rangemode = "tozero", tickformat = TF_NUM, ticksuffix = suf),
             margin = list(t = 10), legend = LEG_H, separators = SEP_ES) %>%
      fino("g_perfil_hora")
  })
  
  output$g_heat_dia_hora <- renderPlotly({
    pb <- perfil_base(); hay_datos(pb)
    m <- input$metrica_perfil %||% "occ_tot"
    suf <- if (m == "occ_tot") "%" else ""
    h <- pb %>% group_by(dia, hora) %>% summarise(val = mean(.data[[m]], na.rm = TRUE), .groups = "drop")
    z <- matrix(NA_real_, 7, 24); z[cbind(h$dia, h$hora + 1)] <- h$val
    # Rojo = poca disponibilidad / ocupación baja; verde = mucha (en las 4 opciones más = mejor)
    dias <- c("Lun", "Mar", "Mié", "Jue", "Vie", "Sáb", "Dom")
    cb <- list(title = if (m == "occ_tot") "%" else "Bicis", len = 0.7, tickformat = TF_NUM, ticksuffix = suf)
    p <- if (es_movil("g_heat_dia_hora")) {
      # Vertical: 24 filas (horas) x 7 columnas (días), que encaja mejor en una pantalla en vertical
      plot_ly(x = dias, y = 0:23, z = t(z), type = "heatmap", colorscale = ESCALA_OCUP,
              colorbar = modifyList(cb, list(thickness = 10)),
              hovertemplate = paste0("%{x} %{y}h: %{z:", TF_NUM, "}", suf, "<extra></extra>")) %>%
        layout(xaxis = list(title = "", side = "top", categoryorder = "array", categoryarray = dias),
               yaxis = list(title = "Hora", dtick = 3, autorange = "reversed"),
               margin = list(t = 10), separators = SEP_ES)
    } else {
      plot_ly(x = 0:23, y = dias, z = z, type = "heatmap", colorscale = ESCALA_OCUP, colorbar = cb,
              hovertemplate = paste0("%{y} %{x}h: %{z:", TF_NUM, "}", suf, "<extra></extra>")) %>%
        layout(xaxis = list(title = "Hora del día", dtick = 2), yaxis = list(title = "", autorange = "reversed"),
               margin = list(t = 10), separators = SEP_ES)
    }
    fino(p, "g_heat_dia_hora")
  })
  
  # Actividad: devoluciones (+) por encima del eje, retiradas (-) por debajo y línea de saldo neto
  output$g_actividad <- renderPlotly({
    d <- d_hist(); hay_datos(d)
    a <- actividad_df(d); hay_datos(a, "No hay suficientes mediciones consecutivas.")
    u <- unidad_hist(); if (u %in% c("5 minutes", "15 minutes")) u <- "1 hour"
    a <- a %>% mutate(t = floor_date(timestamp, u)) %>% group_by(t) %>%
      summarise(retiradas = sum(retiradas), devoluciones = sum(devoluciones), .groups = "drop") %>%
      mutate(neto = devoluciones - retiradas)
    plot_ly(a, x = ~t) %>%
      add_bars(y = ~devoluciones, name = "Devoluciones", marker = list(color = COL_DEVOL),
               hovertemplate = ht("Devoluciones")) %>%
      add_bars(y = ~(-retiradas), name = "Retiradas", marker = list(color = COL_RETIR), customdata = ~retiradas,
               hovertemplate = paste0("%{customdata:", TF_NUM, "}<extra>Retiradas</extra>")) %>%
      add_trace(y = ~neto, name = "Saldo neto", type = "scatter", mode = "lines",
                line = list(color = COL_NETO, width = 2), hovertemplate = ht("Saldo neto")) %>%
      layout(barmode = "relative",
             yaxis = list(title = "Bicis (suma por intervalo)", tickformat = TF_NUM,
                          zeroline = TRUE, zerolinecolor = "#94a3b8", zerolinewidth = 1),
             xaxis = list(title = "", hoverformat = HF_FECHA), hovermode = "x unified",
             margin = list(t = 10), legend = LEG_H, separators = SEP_ES) %>%
      fino("g_actividad")
  })
}

shinyApp(ui, server)
