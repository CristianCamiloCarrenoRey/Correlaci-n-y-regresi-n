# Instalar si no están
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(lmtest)) install.packages("lmtest")
if(!require(car)) install.packages("car")
install.packages("farver")
  
# Cargar librerías
library(ggplot2)
library(lmtest)
library(car)

library(dplyr)

# ============================================================
# 1. Cargar datos
# ============================================================
datos_analysis <- readRDS("data/datos_analysis.rds")

# Crear carpeta de resultados si no existe
if (!dir.exists("resultados")) {
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


install.packages("ggplot2")

install.packages("ggplot2")
install.packages("lmtest")
install.packages("car")



# ============================================================
# 9. VALIDACIÓN DE SUPUESTOS (GRÁFICOS + PRUEBAS)
# ============================================================

# Crear carpeta si no existe
if(!dir.exists("resultados_supuestos")) dir.create("resultados_supuestos")

residuos <- modelo$residuals
ajustados <- modelo$fitted.values

# 9.1 SUPUESTO DE LINEALIDAD 
png("resultados_supuestos/linealidad_residuos_vs_ajustados.png")
 plot( 
  ajustados,
   residuos,
    main = "Residuos vs Ajustados",
     xlab = "Valores Ajustados",
      ylab = "Residuos"
       )
      
 abline(h = 0, col = "red")
 dev.off()
# =======================
# 9.2 Normalidad de los errores
# =======================
# Q-Q plot
png("resultados_supuestos/qqplot_residuos.png")
qqnorm(residuos)
qqline(residuos, col="red")
dev.off()

# Prueba de Shapiro-Wilk (solo 5000 obs si hay más)
shapiro_res <- shapiro.test(if(length(residuos) > 5000) sample(residuos, 5000) else residuos)
write.csv(data.frame(statistic = shapiro_res$statistic,
                     pvalue = shapiro_res$p.value),
          "resultados_supuestos/shapiro_normalidad.csv",
          row.names = FALSE)

# =======================
# 9.3 Homocedasticidad
# =======================
library(lmtest)
bp <- bptest(modelo)
write.csv(data.frame(statistic = bp$statistic,
                     pvalue = bp$p.value),
          "resultados_supuestos/breusch_pagan.csv",
          row.names = FALSE)

# Gráfico para visualizar heterocedasticidad
png("resultados_supuestos/residuos_vs_ajustados_hetero.png")
plot(ajustados, residuos,
     main="Residuos vs Ajustados ",
     xlab="Valores Ajustados", ylab="Residuos")
abline(h=0, col="red")
lines(lowess(ajustados, residuos), col="blue")
dev.off()

# =======================
# 9.4 Independencia
# =======================
library(lmtest)
dw <- dwtest(modelo)
write.csv(data.frame(statistic = dw$statistic,
                     pvalue = dw$p.value),
          "resultados_supuestos/durbin_watson.csv",
          row.names = FALSE)

# Gráfico de autocorrelación (respaldo visual)
png("resultados_supuestos/acf_residuos.png")
acf(residuos, main="Autocorrelación de Residuos (Independencia)")
dev.off()
