library(dplyr)
library(readr)
library(janitor)

cat("Cargando archivos...\n")

# 1) CARGA Y NORMALIZACIÓN ----

Ocupados_Abril <- read_delim(
  "data/diccionarioDatos/CSV_Abril/Ocupados.CSV",
  delim = ";", show_col_types = FALSE
) %>% clean_names() %>% mutate(mes = "Abril")

Ocupados_Mayo <- read_delim(
  "data/diccionarioDatos/CSV_Mayo/Ocupados.CSV",
  delim = ";", show_col_types = FALSE
) %>% clean_names() %>% mutate(mes = "Mayo")

Ocupados_Marzo <- read_delim(
  "data/diccionarioDatos/CSV_Marzo/Ocupados.CSV",
  delim = ";", show_col_types = FALSE
) %>% clean_names() %>% mutate(mes = "Marzo")

cat("Abril:", nrow(Ocupados_Abril), "\n")
cat("Mayo :", nrow(Ocupados_Mayo), "\n")
cat("Marzo:", nrow(Ocupados_Marzo), "\n")

# 2) UNIR ----

datos_completos <- bind_rows(
  Ocupados_Abril,
  Ocupados_Mayo,
  Ocupados_Marzo
)

cat("Total combinado:", nrow(datos_completos), "\n")

# 3) CONSOLIDAR POR DIRECTORIO ----

vars_needed <- c("directorio","inglabo","p6800","p6940","rama2d_r4","mes")

# verificar que existen
missing <- setdiff(vars_needed, names(datos_completos))
if(length(missing) > 0){
  stop(paste("Faltan columnas:", paste(missing, collapse=", ")))
}

# consolidación segura SIN default y sin errores de tipo
datos_trimestre <- datos_completos %>%
  filter(!is.na(directorio)) %>%
  group_by(directorio) %>%
  summarise(
    inglabo = first(inglabo[!is.na(inglabo)]),
    p6800   = first(p6800[!is.na(p6800)]),
    p6940   = first(p6940[!is.na(p6940)]),
    rama2d_r4 = first(rama2d_r4[!is.na(rama2d_r4)]),
    mes = first(mes[!is.na(mes)]),
    .groups = "drop"
  )

cat("Directorio únicos:", nrow(datos_trimestre), "\n")

# 4) CONVERSIÓN A NUMÉRICO ----

datos_trimestre <- datos_trimestre %>%
  mutate(
    inglabo_num = suppressWarnings(parse_number(as.character(inglabo))),
    p6800_num   = suppressWarnings(parse_number(as.character(p6800))),
    p6940_num   = suppressWarnings(parse_number(as.character(p6940))),
    rama_num    = suppressWarnings(parse_number(as.character(rama2d_r4)))
  )

# 5) DATASET FINAL ----

datos_analysis <- datos_trimestre %>%
  filter(!is.na(inglabo_num), !is.na(p6800_num)) %>%
  mutate(
    horas = p6800_num,
    dias  = p6940_num
  )

cat("Dataset final análisis:", nrow(datos_analysis), "\n")
