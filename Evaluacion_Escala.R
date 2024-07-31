#install.packages("ggplot2")
library(ggplot2)
library(dplyr)
library(ggpmisc)
library(ggthemes)

# Crear un dataframe con tus datos
datos1 <- Formulario_completo

###### Sin Escal LOG #######

# Crear el gráfico de dispersión con líneas de regresión usando facet_wrap
ggplot(Formulario_completo, aes(x = Severidad, y = Valor_real, label = Evaluador_N)) +
  geom_point() +  # Puntos de dispersión
  geom_smooth(method = "lm", se = TRUE, color = "blue") +  # Línea de regresión lineal con área de dispersión
  facet_wrap(~ Evaluacion_Tipo) +  # Crear facetas para cada tipo de evaluación
  labs(title = "Gráfico de dispersión con regresión lineal",
       x = "Severidad Estimada",
       y = "Severidad Real",
       caption = "Evaluador: eva_1") +
  theme_tufte() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5)) +
  stat_poly_eq(aes(label = paste(..eq.label.., ..adj.rr.label.., ..p.value.label.., sep = "~~~")),
               formula = y ~ x, 
               parse = TRUE, 
               label.x.npc = "right", 
               label.y.npc = "top",
               size = 4)  + 
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    panel.grid.major = element_line(color = "lightgray", size = 0.25),
    panel.grid.minor = element_line(color = "lightgray", size = 0.25)
  ) 


######### Con Escala Log ########

# Cargar los datos
valor_real_df <- Formulario_completo

# Convertir las columnas relevantes a numéricas
valor_real_df$Valor_real <- as.numeric(as.character(valor_real_df$Valor_real))
valor_real_df$Sev_sin_escala <- as.numeric(as.character(valor_real_df$Severidad))
valor_real_df$Sev_con_escala <- as.numeric(as.character(valor_real_df$Severidad))

# Filtrar los datos para eliminar valores cero y NA
valor_real_df <- valor_real_df %>%
  filter(Valor_real > 0 & Sev_sin_escala > 0 & Sev_con_escala > 0)

# Calcular la correlación y la regresión log-log para Sin Escala
log_valor_real <- log10(valor_real_df$Valor_real)
log_sev_sin_escala <- log10(valor_real_df$Sev_sin_escala)
cor_sin <- cor(log_valor_real, log_sev_sin_escala)
lm_sin <- lm(log_sev_sin_escala ~ log_valor_real)
summary(lm_sin)

# Calcular la correlación y la regresión log-log para Con Escala
log_sev_con_escala <- log10(valor_real_df$Sev_con_escala)
cor_con <- cor(log_valor_real, log_sev_con_escala)
lm_con <- lm(log_sev_con_escala ~ log_valor_real)
summary(lm_con)

# Generar el gráfico logarítmico para Sin Escala con línea de tendencia
ggplot(valor_real_df, aes(x = Valor_real, y = Sev_sin_escala)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(~ Evaluacion_Tipo) +  # Crear facetas para cada tipo de evaluación
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "red", linetype = "dashed") +
  labs(title = "Valor Real vs Sin Escala (Escala Logarítmica)",
       x = "Valor Real",
       y = "Sin Escala") +
  stat_poly_eq(aes(label = paste(..eq.label.., ..adj.rr.label.., ..p.value.label.., sep = "~~~")),
               formula = y ~ x, 
               parse = TRUE, 
               label.x.npc = "right", 
               label.y.npc = "top",
               size = 4) +
  theme_tufte()   + 
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    panel.grid.major = element_line(color = "lightgray", size = 0.25),
    panel.grid.minor = element_line(color = "lightgray", size = 0.25)
  ) 




######## Analisis de Dispersion ####
# Ajustar modelos de regresión lineal para cada grupo
modelo_sin_escala <- lm(Valor_real ~ Severidad, data = subset(Formulario_completo, Evaluacion_Tipo == "Sin Escala"))
modelo_con_escala <- lm(Valor_real ~ Severidad, data = subset(Formulario_completo, Evaluacion_Tipo == "Con Escala"))

# Calcular los residuos de cada modelo
residuos_sin_escala <- resid(modelo_sin_escala)
residuos_con_escala <- resid(modelo_con_escala)

# Calcular la desviación estándar de los residuos para cada grupo
sd_residuos_sin_escala <- sd(residuos_sin_escala)
sd_residuos_con_escala <- sd(residuos_con_escala)

# Imprimir las desviaciones estándar de los residuos
sd_residuos_sin_escala
sd_residuos_con_escala

# Realizar la prueba de F para comparar las varianzas de los residuos
var_test <- var.test(residuos_sin_escala, residuos_con_escala)

# Imprimir los resultados de la prueba de F
var_test
