# ============================================================
#   LIMPIEZA + ELIMINACIÓN DE SECTORES RAROS + REGRESIÓN
# ============================================================

library(dplyr)

# ------------------------------------------------------------
# 1. Cargar dataset
# ------------------------------------------------------------
data <- readRDS("data/datos_analysis.rds")

cat("Variables disponibles:\n")
print(names(data))

# ------------------------------------------------------------
# 2. Verificar variables requeridas
# ------------------------------------------------------------
vars_requeridas <- c("inglabo", "p6800", "p6850", "rama2d_r4")
faltantes <- vars_requeridas[!vars_requeridas %in% names(data)]

if(length(faltantes) > 0){
  stop(paste("ERROR: Faltan variables:", paste(faltantes, collapse = ", ")))
}

# ------------------------------------------------------------
# 3. Eliminar sectores con baja frecuencia
# ------------------------------------------------------------
min_frec <- 100   # ⭐ Puedes cambiar este valor

cat("\nFrecuencias originales de sectores:\n")
print(table(data$rama2d_r4))

freq <- data %>%
  count(rama2d_r4) %>%
  filter(n >= min_frec)

sectores_validos <- freq$rama2d_r4

data <- data %>%
  filter(rama2d_r4 %in% sectores_validos)

cat("\nSectores eliminados por baja frecuencia:\n")
print(setdiff(unique(data$rama2d_r4), sectores_validos))

cat("\nSectores restantes:\n")
print(table(data$rama2d_r4))

# ------------------------------------------------------------
# 4. Eliminar outliers por IQR
# ------------------------------------------------------------
remove_outliers_iqr <- function(df) {
  df %>% mutate(across(where(is.numeric), function(x) {
    Q1 <- quantile(x, 0.25, na.rm = TRUE)
    Q3 <- quantile(x, 0.75, na.rm = TRUE)
    IQR_value <- Q3 - Q1
    lower <- Q1 - 1.5 * IQR_value
    upper <- Q3 + 1.5 * IQR_value
    x[x < lower | x > upper] <- NA
    return(x)
  })) %>% 
    na.omit()
}

data_clean <- remove_outliers_iqr(data)

# ------------------------------------------------------------
# 5. Transformación log1p(inglabo)
# ------------------------------------------------------------
data_clean <- data_clean %>%
  mutate(log_inglabo = log1p(inglabo))

# ------------------------------------------------------------
# 6. Convertir rama2d_r4 en factor
# ------------------------------------------------------------
data_clean$rama2d_r4 <- as.factor(data_clean$rama2d_r4)

# ------------------------------------------------------------
# 7. Modelo de regresión
# ------------------------------------------------------------
modelo <- lm(
  log_inglabo ~ p6800 + p6850 + rama2d_r4,
  data = data_clean
)

# ------------------------------------------------------------
# 8. Crear carpeta de resultados
# ------------------------------------------------------------
dir.create("resultadosDatosLimpios", showWarnings = FALSE)

# ------------------------------------------------------------
# 9. Guardar resultados
# ------------------------------------------------------------

# --- Coeficientes ---
write.csv(
  summary(modelo)$coefficients,
  "resultadosDatosLimpios/coeficientes.csv"
)

# --- ANOVA ---
write.csv(
  anova(modelo),
  "resultadosDatosLimpios/anova.csv"
)

# --- R2 y Ajustado ---
r2_df <- data.frame(
  R2 = summary(modelo)$r.squared,
  R2_Ajustado = summary(modelo)$adj.r.squared
)

write.csv(r2_df, "resultadosDatosLimpios/r2.csv", row.names = FALSE)

# --- Significancia global ---
f <- summary(modelo)$fstatistic
p_global <- pf(f[1], f[2], f[3], lower.tail = FALSE)

significancia_global <- data.frame(
  Estadistico_F = f[1],
  gl1 = f[2],
  gl2 = f[3],
  p_value_global = p_global
)

write.csv(significancia_global,
          "resultadosDatosLimpios/significancia_global.csv",
          row.names = FALSE)

# --- Resumen completo ---
sink("resultadosDatosLimpios/ResumenModelo.txt")
cat("=====================================================\n")
cat("          RESUMEN COMPLETO DEL MODELO LIMPIO\n")
cat("=====================================================\n\n")
print(summary(modelo))
sink()

cat("\n✔️ Proceso completado. Resultados en 'resultadosDatosLimpios/'\n")
