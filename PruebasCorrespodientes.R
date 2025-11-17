library(dplyr)

# ============================================================
# 1. Cargar datos
# ============================================================
datos_analysis <- readRDS("data/datos_analysis.rds")

# Crear carpeta de resultados si no existe
if(!dir.exists("resultados")){
  dir.create("resultados")
}

# Asegurar factor
datos_analysis$rama2d_r4 <- as.factor(datos_analysis$rama2d_r4)

# ============================================================
# 2. Estimar modelo con nombres correctos
# ============================================================
modelo <- lm(inglabo_num ~ p6800_num + p6850_num + rama2d_r4,
             data = datos_analysis)

# ============================================================
# 3. COEFICIENTES INDIVIDUALES
# ============================================================
tabla_coef <- summary(modelo)$coefficients
write.csv(tabla_coef, "resultados/coeficientes.csv")

# ============================================================
# 4. SIGNIFICANCIA GLOBAL (F-test)
# ============================================================
tabla_global <- as.data.frame(t(summary(modelo)$fstatistic))
write.csv(tabla_global, "resultados/significancia_global.csv")

# ============================================================
# 5. ANOVA
# ============================================================
tabla_anova <- anova(modelo)
write.csv(tabla_anova, "resultados/anova.csv")

# ============================================================
# 6. R² y R² ajustado
# ============================================================
tabla_r2 <- data.frame(
  R2 = summary(modelo)$r.squared,
  R2_Ajustado = summary(modelo)$adj.r.squared
)
write.csv(tabla_r2, "resultados/r2.csv", row.names = FALSE)

# ============================================================
# 7. INTERVALOS DE CONFIANZA
# ============================================================
tabla_ci <- confint(modelo)
write.csv(tabla_ci, "resultados/intervalos_confianza.csv")

# ============================================================
# 8. Imprimir en consola (ahora sí funciona)
# ============================================================
cat("\n===== RESULTADOS DEL MODELO =====\n\n")

cat("1. Coeficientes:\n")
print(tabla_coef)

cat("\n2. F-test (Significancia Global):\n")
print(tabla_global)

cat("\n3. ANOVA:\n")
print(tabla_anova)

cat("\n4. R² y R² Ajustado:\n")
print(tabla_r2)

cat("\n5. Intervalos de Confianza:\n")
print(tabla_ci)

cat("\n\nArchivos CSV guardados en la carpeta 'resultados/'.\n")
