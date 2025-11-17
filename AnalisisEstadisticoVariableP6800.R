# Analisis_P6800.R
# Análisis exploratorio gráfico y estadístico para P6800
# Datos: datos_analysis (ya cargado o se cargará desde RDS)

# ---------- Paquetes ----------
library(dplyr)
library(ggplot2)
library(scales)
library(patchwork)
library(moments)
library(DescTools)
library(nortest)

# ---------- Cargar datos si no están en memoria ----------
if (!exists("datos_analysis")) {
  if (file.exists("data/datos_analysis.rds")) {
    datos_analysis <- readRDS("data/datos_analysis.rds")
  } else {
    stop("No existe 'datos_analysis' en la sesión ni el archivo en /data.")
  }
}

# ---------- Detectar nombre de P6800 (por si cambia) ----------
posibles_nombres <- c("P6800","p6800","p6800_num","horas_semana","horas_normales")
p6800_name <- intersect(names(datos_analysis), posibles_nombres) |> first()

if (is.null(p6800_name)) {
  stop("No encuentro la variable P6800. Ajusta los nombres buscados.")
}

message("Usando variable: ", p6800_name)

# ---------- Preparación ----------
h <- as.numeric(datos_analysis[[p6800_name]])

n_total <- length(h)
n_na <- sum(is.na(h))
message("Observaciones totales: ", n_total, "  | NA: ", n_na)

# ---------- Estadísticas descriptivas ----------
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

# Quantiles completos
qs <- quantile(h, probs = c(0, .01, .05, .1, .25, .5, .75, .9, .95, .99, 1), na.rm = TRUE)
print(qs)

# ---------- Detección de outliers ----------
q1 <- qs["25%"]; q3 <- qs["75%"]; iqr <- q3 - q1
lower <- q1 - 1.5 * iqr
upper <- q3 + 1.5 * iqr

out_iqr <- which(h < lower | h > upper)
message("Outliers (IQR rule): ", length(out_iqr))

z <- (h - mean(h, na.rm = TRUE)) / sd(h, na.rm = TRUE)
out_z <- which(abs(z) > 3)
message("Outliers (|z|>3): ", length(out_z))

# ---------- Normalidad ----------
n_obs <- sum(!is.na(h))

if (n_obs <= 5000) {
  sh <- shapiro.test(h[!is.na(h)])
  message("Shapiro-Wilk p-value: ", sh$p.value)
} else {
  ad <- nortest::ad.test(h[!is.na(h)])
  message("Anderson-Darling p-value: ", ad$p.value)
}

# ---------- Gráficos ----------
if (!dir.exists("plots")) dir.create("plots")

# 1) Histograma
p1 <- ggplot(data.frame(horas = h), aes(x = horas)) +
  geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
  geom_density() +
  labs(title = "Histograma de P6800 (Horas semana)",
       x = "Horas trabajadas por semana", y = "Frecuencia") +
  theme_minimal()

ggsave("plots/hist_p6800.png", p1, width = 8, height = 5, dpi = 150)

# 2) Boxplot
p2 <- ggplot(data.frame(horas = h), aes(y = horas)) +
  geom_boxplot(outlier.shape = 1) +
  labs(title = "Boxplot de P6800", y = "Horas semana") +
  theme_minimal()

ggsave("plots/box_p6800.png", p2, width = 4, height = 6, dpi = 150)

# 3) Violin plot por sector si existe RAMA
sector_var <- intersect(names(datos_analysis),
                        c("RAMA2D_R4","rama2d_r4","rama3","sector"))
if (length(sector_var) == 1) {
  datos_v <- datos_analysis |> 
    mutate(horas = as.numeric(!!sym(p6800_name)),
           sector = as.factor(!!sym(sector_var))) |> 
    filter(!is.na(horas))

  p3 <- ggplot(datos_v, aes(x = sector, y = horas)) +
    geom_violin(trim = TRUE) +
    geom_boxplot(width = 0.1) +
    coord_flip() +
    labs(title = paste("P6800 por sector -", sector_var), 
         y = "Horas semana", x = "Sector") +
    theme_minimal()

  ggsave("plots/violin_p6800_sector.png", p3, width = 9, height = 7, dpi = 150)
} else {
  message("No encontré variable RAMA para el violín.")
}

# 4) Scatter P6800 vs INGLABO si existe INGLABO
if ("INGLABO" %in% names(datos_analysis) |
    "inglabo" %in% names(datos_analysis)) {

  ing_name <- intersect(names(datos_analysis), c("INGLABO","inglabo")) |> first()

  datos_scatter <- datos_analysis |> 
    mutate(
      horas = as.numeric(!!sym(p6800_name)),
      ingreso = as.numeric(!!sym(ing_name))
    ) |> 
    filter(!is.na(horas), !is.na(ingreso))

  p4 <- ggplot(datos_scatter, aes(x = horas, y = ingreso)) +
    geom_jitter(alpha = 0.1) +
    geom_smooth(method = "lm") +
    scale_y_continuous(labels = comma) +
    labs(title = "P6800 vs INGLABO",
         x = "Horas trabajadas por semana",
         y = "Ingreso laboral") +
    theme_minimal()

  ggsave("plots/scatter_p6800_inglabo.png", p4, width = 8, height = 5, dpi = 150)
}

# ---------- Guardar resumen ----------
resumen <- capture.output({
  cat("ANÁLISIS VARIABLE P6800 (Horas trabajadas)\n\n")
  print(stats)
  cat("\nQuantiles:\n")
  print(qs)
  cat("\nOutliers IQR:", length(out_iqr), 
      " | Outliers z:", length(out_z), "\n")
})

writeLines(resumen, "plots/resumen_p6800.txt")

message("Análisis de P6800 completado. Archivos generados en 'plots/'.")
