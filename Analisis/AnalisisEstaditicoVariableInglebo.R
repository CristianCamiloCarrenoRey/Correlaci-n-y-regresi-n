# AnalisisInglabo.R
# Análisis exploratorio estadístico y gráfico para INGLABO
# Ejecutar en la misma sesión o antes cargar datos: datos_analysis <- readRDS("data/datos_analysis.rds")

# ---------- Paquetes (descomenta instalar si hace falta) ----------
install.packages(c("dplyr","ggplot2","readr","janitor","scales","patchwork","moments","DescTools","nortest","gridExtra"))
library(dplyr)
library(ggplot2)
library(scales)
library(patchwork)   # para combinar plots
library(moments)     # skewness/kurtosis
library(DescTools)   # Winsorize
library(nortest)     # pr. normalidad (Anderson-Darling)

# ---------- Cargar datos si no están en memoria ----------
if (!exists("datos_analysis")) {
  if (file.exists("data/datos_analysis.rds")) {
    datos_analysis <- readRDS("data/datos_analysis.rds")
  } else {
    stop("No existe 'datos_analysis' en la sesión ni el archivo 'data/datos_analysis.rds'.")
  }
}

# ---------- Detectar nombre de variable INGLABO (tolerante) ----------
possible_names <- c("INGLABO", "inglabo", "inglabo_num", "inglab", "ingreso_pesos", "ingreso_millones", "ingreso_miles")
ing_name <- intersect(names(datos_analysis), possible_names) %>% first()
if (is.null(ing_name) || is.na(ing_name)) {
  stop("No encuentro la variable de ingreso (busqué: INGLABO/inglabo/inglabo_num/ingreso_pesos). Ajusta 'possible_names' si tu variable tiene otro nombre.")
}
message("Usando variable: ", ing_name)

# ---------- Preparar vector y limpiar valores imposibles ----------
ing <- datos_analysis[[ing_name]]
# Asegurar numérico
ing <- as.numeric(ing)
# Reportar missing / non-finite
n_total <- length(ing)
n_na <- sum(is.na(ing))
n_inf <- sum(!is.finite(ing) & !is.na(ing))
message("Observaciones totales: ", n_total, 
        "  | NA: ", n_na, 
        "  | non-finite: ", n_inf)

# Opcional: eliminar negativos si se consideran erróneos
ing_clean <- ifelse(ing < 0, NA_real_, ing)

# ---------- Estadísticas descriptivas ----------
stats <- data.frame(
  n = sum(!is.na(ing_clean)),
  mean = mean(ing_clean, na.rm = TRUE),
  median = median(ing_clean, na.rm = TRUE),
  sd = sd(ing_clean, na.rm = TRUE),
  mad = mad(ing_clean, na.rm = TRUE),
  IQR = IQR(ing_clean, na.rm = TRUE),
  min = min(ing_clean, na.rm = TRUE),
  q1 = quantile(ing_clean, 0.25, na.rm = TRUE),
  q3 = quantile(ing_clean, 0.75, na.rm = TRUE),
  max = max(ing_clean, na.rm = TRUE),
  skewness = moments::skewness(ing_clean, na.rm = TRUE),
  kurtosis = moments::kurtosis(ing_clean, na.rm = TRUE),
  cv = sd(ing_clean, na.rm = TRUE) / abs(mean(ing_clean, na.rm = TRUE))
)
print(round(stats, 3))

# Quantiles detallados
qs <- quantile(ing_clean, probs = c(0, .01, .05, .1, .25, .5, .75, .9, .95, .99, 1), na.rm = TRUE)
print(qs)

# ---------- Detección de outliers ----------
# IQR rule
q1 <- qs["25%"]; q3 <- qs["75%"]; iqr <- q3 - q1
lower <- q1 - 1.5 * iqr
upper <- q3 + 1.5 * iqr
out_iqr <- which(ing_clean < lower | ing_clean > upper)
message("Outliers por regla IQR (1.5*IQR): ", length(out_iqr))

# Z-score > 3 (basado en media y sd)
z <- (ing_clean - mean(ing_clean, na.rm = TRUE)) / sd(ing_clean, na.rm = TRUE)
out_z <- which(abs(z) > 3)
message("Outliers por z-score (|z|>3): ", length(out_z))

# ---------- Pruebas de normalidad ----------
n_obs <- sum(!is.na(ing_clean))
if (n_obs <= 5000) {
  sh <- shapiro.test(ing_clean[!is.na(ing_clean)])
  message("Shapiro-Wilk p-value: ", sh$p.value)
} else {
  ad <- nortest::ad.test(ing_clean[!is.na(ing_clean)])
  message("Anderson-Darling p-value: ", ad$p.value)
}

# ---------- Transformaciones útiles ----------
# Log (agregar 1 para evitar log(0))
ing_log <- log1p(ing_clean)
# Estadísticas log
cat("Media log1p:", mean(ing_log, na.rm = TRUE), " Mediana log1p:", median(ing_log, na.rm = TRUE), "\n")

# ---------- Gráficos con ggplot2 ----------
# Crear carpeta plots si no existe
if (!dir.exists("plots")) dir.create("plots")

# 1) Histograma + densidad (lineal)
p_hist <- ggplot(data = data.frame(ing = ing_clean), aes(x = ing)) +
  geom_histogram(aes(y = ..density..), bins = 60, fill = "steelblue", alpha = 0.6, na.rm = TRUE) +
  geom_density(size = 1, na.rm = TRUE) +
  geom_vline(xintercept = stats$mean, color = "red", linetype = "dashed") +
  geom_vline(xintercept = stats$median, color = "darkgreen", linetype = "dotted") +
  scale_x_continuous(labels = comma) +
  labs(title = "Histograma y densidad de INGLABO", x = "Ingreso (moneda)", y = "Densidad") +
  theme_minimal()

ggsave("plots/hist_inglabo.png", p_hist, width = 8, height = 5, dpi = 150)

# 2) Histograma en escala log (recomendado para ingresos)
p_hist_log <- ggplot(data = data.frame(ing = ing_clean), aes(x = ing_log)) +
  geom_histogram(aes(y = ..density..), bins = 60, fill = "tomato", alpha = 0.6, na.rm = TRUE) +
  geom_density(size = 1, na.rm = TRUE) +
  labs(title = "Histograma (log1p) de INGLABO", x = "log1p(ingreso)", y = "Densidad") +
  theme_minimal()

ggsave("plots/hist_inglabo_log.png", p_hist_log, width = 8, height = 5, dpi = 150)

# 3) Boxplot (lineal)
p_box <- ggplot(data = data.frame(ing = ing_clean), aes(y = ing)) +
  geom_boxplot(na.rm = TRUE, outlier.shape = 1) +
  scale_y_continuous(labels = comma) +
  labs(title = "Boxplot de INGLABO", y = "Ingreso (moneda)") +
  theme_minimal()

ggsave("plots/box_inglabo.png", p_box, width = 4, height = 6, dpi = 150)

# 4) Violin/box por sector si existe rama2d_r4 o rama3
sector_var <- intersect(names(datos_analysis), c("rama2d_r4","rama3","rama_3","rama3_r4"))
if (length(sector_var) == 1) {
  datos_plot <- datos_analysis %>%
    mutate(ing = as.numeric(!!sym(ing_name)),
           sector = as.factor(!!sym(sector_var))) %>%
    filter(!is.na(ing))
  p_violin <- ggplot(datos_plot, aes(x = sector, y = ing)) +
    geom_violin(trim = TRUE) +
    geom_boxplot(width = 0.1) +
    scale_y_continuous(labels = comma) +
    coord_flip() +
    labs(title = paste("Distribución de INGLABO por", sector_var), y = "Ingreso (moneda)", x = "Sector") +
    theme_minimal()
  ggsave("plots/violin_by_sector.png", p_violin, width = 9, height = 8, dpi = 150)
} else {
  message("No encontré variable de sector para el gráfico violin (busqué: rama2d_r4, rama3, rama_3).")
}

# 5) Scatter INGLABO vs Horas trabajadas (p6800 o p6850)
hora_var <- intersect(names(datos_analysis), c("p6800","p6800_num","P6800","P6850","p6850","p6850_num"))
if (length(hora_var) >= 1) {
  hv <- hora_var[1]
  datos_scatter <- datos_analysis %>%
    mutate(ing = as.numeric(!!sym(ing_name)),
           horas = as.numeric(!!sym(hv))) %>%
    filter(!is.na(ing), !is.na(horas))
  p_scatter <- ggplot(datos_scatter, aes(x = horas, y = ing)) +
    geom_jitter(alpha = 0.15, width = 0.2, height = 0) +
    geom_smooth(method = "lm", se = TRUE, color = "darkred") +
    scale_y_continuous(labels = comma) +
    labs(title = paste("INGLABO vs", hv), x = hv, y = "Ingreso (moneda)") +
    theme_minimal()
  ggsave("plots/ing_vs_horas.png", p_scatter, width = 7, height = 5, dpi = 150)
} else {
  message("No encontré variable de horas (p6800/p6850) para scatter.")
}

# Combine principales plots (hist + box)
combined <- p_hist + p_box + plot_layout(widths = c(2,1))
ggsave("plots/hist_box_combined.png", combined, width = 11, height = 5, dpi = 150)

# ---------- Winsorización y comparación ----------
# Winsorizar en 1% y 99%
ing_w <- Winsorize(ing_clean, probs = c(0.01,0.99), na.rm = TRUE)
p_hist_wins <- ggplot(data = data.frame(ing = ing_w), aes(x = ing)) +
  geom_histogram(aes(y = ..density..), bins = 60, fill = "darkgreen", alpha = 0.6, na.rm = TRUE) +
  geom_density(na.rm = TRUE) +
  labs(title = "Histograma INGLABO (Winsorized 1%-99%)", x = "Ingreso (moneda)") +
  theme_minimal()
ggsave("plots/hist_inglabo_winsor.png", p_hist_wins, width = 8, height = 5, dpi = 150)

# ---------- Guardar resumen estadístico en archivo ----------
resumen_txt <- capture.output({
  cat("Resumen INGLABO (variable:", ing_name, ")\n\n")
  print(round(stats,3))
  cat("\nQuantiles:\n"); print(round(qs,2))
  cat("\nOutliers IQR:", length(out_iqr), "Outliers z:", length(out_z), "\n")
  if (n_obs <= 5000) print(sh) else print(ad)
})
writeLines(resumen_txt, "plots/resumen_inglabo.txt")

# ---------- Mensaje final ----------
message("Análisis completado. Plots y resumen en la carpeta 'plots/'.")
