# Analisis_RAMA2D_R4.R
# Análisis exploratorio para la variable RAMA2D_R4
# Datos: datos_analysis (desde memoria o archivo .rds)

# ----------------- Paquetes -----------------
library(dplyr)
library(ggplot2)
library(scales)

# ----------------- Cargar datos -----------------
if (!exists("datos_analysis")) {
  if (file.exists("data/datos_analysis.rds")) {
    datos_analysis <- readRDS("data/datos_analysis.rds")
  } else {
    stop("ERROR: No encontré 'datos_analysis' ni el archivo .rds")
  }
}

# ----------------- Identificar variable -----------------
posibles_nombres <- c("RAMA2D_R4", "rama2d_r4", "rama2d", "rama")
rama_name <- intersect(names(datos_analysis), posibles_nombres) |> first()

if (is.null(rama_name)) {
  stop("No encontré la variable RAMA2D_R4. Revisa los nombres.")
}

message("Usando variable: ", rama_name)

# Extraer la variable
rama <- as.factor(datos_analysis[[rama_name]])

# ----------------- Frecuencias -----------------
tabla_frec <- table(rama)
tabla_porcentaje <- prop.table(tabla_frec) * 100

resumen_stats <- data.frame(
  Categoria = names(tabla_frec),
  Frecuencia = as.vector(tabla_frec),
  Porcentaje = round(as.vector(tabla_porcentaje), 2)
)

print(resumen_stats)

# ----------------- Crear carpeta de gráficos -----------------
if (!dir.exists("plots")) dir.create("plots")

# ----------------- Gráfico de barras -----------------
p1 <- ggplot(resumen_stats, aes(x = reorder(Categoria, Frecuencia), 
                                y = Frecuencia)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Distribución de Actividades Económicas (RAMA2D_R4)",
       x = "Actividad Económica",
       y = "Frecuencia") +
  theme_minimal()

ggsave("plots/barras_RAMA2D_R4.png", p1, width = 8, height = 6, dpi = 150)

# ----------------- Gráfico de porcentajes -----------------
p2 <- ggplot(resumen_stats, aes(x = reorder(Categoria, Porcentaje),
                                y = Porcentaje)) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  labs(title = "Porcentaje por Actividad Económica (RAMA2D_R4)",
       x = "Actividad Económica",
       y = "Porcentaje (%)") +
  theme_minimal()

ggsave("plots/porcentaje_RAMA2D_R4.png", p2, width = 8, height = 6, dpi = 150)

# ----------------- Relación con P6850 (si existe) -----------------
if ("P6850" %in% names(datos_analysis)) {
  datos_v <- datos_analysis %>%
    mutate(
      horas = as.numeric(P6850),
      sector = as.factor(!!sym(rama_name))
    ) %>%
    filter(!is.na(horas), !is.na(sector))
  
  p3 <- ggplot(datos_v, aes(x = sector, y = horas)) +
    geom_violin(fill = "skyblue", alpha = 0.7) +
    geom_boxplot(width = .15) +
    coord_flip() +
    labs(title = "Horas trabajadas (P6850) según actividad económica",
         x = "Actividad económica",
         y = "Horas trabajadas") +
    theme_minimal()
  
  ggsave("plots/violin_RAMA2D_R4_P6850.png", p3, width = 10, height = 7, dpi = 150)
}

# ----------------- Relación con INGLABO (si existe) -----------------
if ("INGLABO" %in% names(datos_analysis)) {
  datos_ing <- datos_analysis %>%
    mutate(
      ingreso = as.numeric(INGLABO),
      sector = as.factor(!!sym(rama_name))
    ) %>%
    filter(!is.na(ingreso), !is.na(sector))
  
  p4 <- ggplot(datos_ing, aes(x = sector, y = ingreso)) +
    geom_violin(fill = "lightgreen", alpha = 0.7) +
    geom_boxplot(width = .15) +
    coord_flip() +
    scale_y_continuous(labels = comma) +
    labs(title = "Ingreso laboral (INGLABO) por actividad económica",
         x = "Actividad económica",
         y = "Ingreso laboral") +
    theme_minimal()
  
  ggsave("plots/violin_RAMA2D_R4_INGLABO.png", p4, width = 10, height = 7, dpi = 150)
}

# ----------------- Exportar resumen -----------------
resumen <- capture.output({
  cat("ANÁLISIS DE RAMA2D_R4 (Actividad Económica)\n\n")
  print(resumen_stats)
})

writeLines(resumen, "plots/resumen_RAMA2D_R4.txt")

message("Análisis de RAMA2D_R4 completado. Revisa la carpeta 'plots/'")
