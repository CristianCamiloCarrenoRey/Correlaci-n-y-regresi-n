# Analisis_P6850.R
# Análisis exploratorio para P6850 (horas trabajadas semana pasada)
# Datos: datos_analysis (desde memoria o archivo .rds)

# ----------------- Paquetes -----------------
library(dplyr)
library(ggplot2)
library(scales)
library(moments)
library(nortest)

# ----------------- Cargar datos -----------------
if (!exists("datos_analysis")) {
  if (file.exists("data/datos_analysis.rds")) {
    datos_analysis <- readRDS("data/datos_analysis.rds")
  } else {
    stop("ERROR: No encontré 'datos_analysis' ni el archivo .rds")
  }
}

# ----------------- Identificar columna P6850 -----------------
posibles_nombres <- c("P6850", "p6850", "p6850_num", "horas_sem_pasada", "horas_sp")
p6850_name <- intersect(names(datos_analysis), posibles_nombres) |> first()

if (is.null(p6850_name)) {
  stop("No encontré la variable P6850. Revisa los nombres disponibles.")
}

message("Usando variable: ", p6850_name)

# Extraer la variable
h <- as.numeric(datos_analysis[[p6850_name]])

# ----------------- Información básica -----------------
n_total <- length(h)
n_na <- sum(is.na(h))
message("Observaciones totales: ", n_total, " | NA: ", n_na)

# ----------------- Estadísticas descriptivas -----------------
stats <- data.frame(
  n = sum(!is.na(h)),
  mean = mean(h, na.rm = TRUE),
  median = median(h, na.rm = TRUE),
  sd = sd(h, na.rm = TRUE),
  mad = mad(h, na.rm = TRUE),
  IQR = IQR(h, na.rm = TRUE),
  min = min(h, na.rm = TRUE),
  q1 = quantile(h, .25, na.rm = TRUE),
  q3 = quantile(h, .75, na.rm = TRUE),
  max = max(h, na.rm = TRUE),
  skewness = moments::skewness(h, na.rm = TRUE),
  kurtosis = moments::kurtosis(h, na.rm = TRUE)
)

print(round(stats, 3))

# Quantiles extendidos
qs <- quantile(h,
               probs = c(0, .01, .05, .1, .25, .5, .75, .9, .95, .99, 1),
               na.rm = TRUE)
print(qs)

# ----------------- Outliers (IQR y Z-score) -----------------
q1 <- qs["25%"]; q3 <- qs["75%"]
iqr <- q3 - q1
lower <- q1 - 1.5 * iqr
upper <- q3 + 1.5 * iqr

out_iqr <- which(h < lower | h > upper)
message("Outliers (IQR): ", length(out_iqr))

# Z-score
z <- (h - mean(h, na.rm = TRUE)) / sd(h, na.rm = TRUE)
out_z <- which(abs(z) > 3)
message("Outliers (|z|>3): ", length(out_z))

# ----------------- Prueba de normalidad -----------------
n_obs <- sum(!is.na(h))

if (n_obs <= 5000) {
  sh <- shapiro.test(h[!is.na(h)])
  message("Shapiro-Wilk p-value: ", sh$p.value)
} else {
  ad <- nortest::ad.test(h[!is.na(h)])
  message("Anderson-Darling p-value: ", ad$p.value)
}

# ----------------- Crear carpeta de gráficos -----------------
if (!dir.exists("plots")) dir.create("plots")

# ----------------- 1) Histograma -----------------
p1 <- ggplot(data.frame(horas = h), aes(x = horas)) +
  geom_histogram(bins = 50, fill = "orange", alpha = 0.7) +
  geom_density() +
  labs(title = "Histograma de P6850 (Horas trabajadas semana pasada)",
       x = "Horas trabajadas", y = "Frecuencia") +
  theme_minimal()

ggsave("plots/hist_p6850.png", p1, width = 8, height = 5, dpi = 150)

# ----------------- 2) Boxplot -----------------
p2 <- ggplot(data.frame(horas = h), aes(y = horas)) +
  geom_boxplot(outlier.shape = 1, fill = "tomato", alpha = 0.6) +
  labs(title = "Boxplot de P6850",
       y = "Horas semana pasada") +
  theme_minimal()

ggsave("plots/box_p6850.png", p2, width = 4, height = 6, dpi = 150)

# ----------------- 3) Violin por sector (si existe) -----------------
sector_var <- intersect(names(datos_analysis),
                        c("RAMA2D_R4","rama2d_r4","rama3","sector"))

if (length(sector_var) == 1) {
  datos_v <- datos_analysis %>%
    mutate(
      horas = as.numeric(!!sym(p6850_name)),
      sector = as.factor(!!sym(sector_var))
    ) %>%
    filter(!is.na(horas))

  p3 <- ggplot(datos_v, aes(x = sector, y = horas)) +
    geom_violin(trim = TRUE, fill = "skyblue") +
    geom_boxplot(width = 0.1) +
    coord_flip() +
    labs(title = paste("P6850 por Sector:", sector_var),
         x = "Sector", y = "Horas") +
    theme_minimal()

  ggsave("plots/violin_p6850_sector.png", p3, width = 9, height = 7, dpi = 150)
}

# ----------------- 4) Scatter con INGLABO (si existe) -----------------
if ("INGLABO" %in% names(datos_analysis) |
    "inglabo" %in% names(datos_analysis)) {

  ing_name <- intersect(names(datos_analysis), c("INGLABO","inglabo")) |> first()

  datos_scatter <- datos_analysis %>%
    mutate(
      horas = as.numeric(!!sym(p6850_name)),
      ingreso = as.numeric(!!sym(ing_name))
    ) %>%
    filter(!is.na(horas), !is.na(ingreso))

  p4 <- ggplot(datos_scatter, aes(x = horas, y = ingreso)) +
    geom_jitter(alpha = .15) +
    geom_smooth(method = "lm", se = TRUE) +
    labs(title = "Relación entre P6850 e INGLABO",
         x = "Horas semana pasada", y = "Ingreso laboral") +
    scale_y_continuous(labels = comma) +
    theme_minimal()

  ggsave("plots/scatter_p6850_inglabo.png", p4, width = 8, height = 5, dpi = 150)
}

# ----------------- Exportar resumen -----------------
resumen <- capture.output({
  cat("ANÁLISIS VARIABLE P6850 (Horas trabajadas semana pasada)\n\n")
  print(stats)
  cat("\nQuantiles:\n")
  print(qs)
  cat("\nOutliers IQR:", length(out_iqr),
      " | Outliers Z-score:", length(out_z), "\n")
})

writeLines(resumen, "plots/resumen_p6850.txt")

message("Análisis de P6850 completado. Revisa la carpeta 'plots/'")
