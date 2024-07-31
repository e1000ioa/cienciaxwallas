#install.packages("ggplot2")
library(ggplot2)
library(tidyverse)
library(googlesheets4)

# URL del Google Sheet
sheet_url <- "https://docs.google.com/spreadsheets/d/1rIs6OsLAukEz8c0UbH0rwB_bm88S2phCVsOEk2dz_M4/edit?usp=sharing"

# Leer los archivos CSV
Evaluadores <- read.csv("evaluadores_lucas.csv") %>%
  filter(Persona == "Lucas Gimenez")

#Formulario <- read.csv("Formuilario_1.csv")
Formulario <- read_sheet(sheet_url)

# Limpiar los nombres de las columnas, reemplazando los espacios por puntos
colnames(Formulario) <- gsub(" ", ".", colnames(Formulario))

# Separar la columna Marca.temporal en dos columnas: Fecha y Hora.Final (si es necesario)
Formulario <- Formulario %>%
  separate(Marca.temporal, into = c("Fecha", "Hora.Final"), sep = " ") %>%
  separate(Hora.Actual, into = c("Fecha.final", "Hora.Inicio"), sep = " ")

# Eliminar todo lo que viene después del punto en Hora.Final y Hora.Inicio
Formulario <- Formulario %>%
  mutate(Hora.Final = sub("\\..*$", "", Hora.Final),
         Hora.Inicio = sub("\\..*$", "", Hora.Inicio))

# Convertir las columnas Hora.Final y Hora.Inicio a POSIXct
Formulario <- Formulario %>%
  mutate(Hora.Final = as.POSIXct(paste(Fecha, Hora.Final), format = "%Y-%m-%d %H:%M:%S"),
         Hora.Inicio = as.POSIXct(paste(Fecha, Hora.Inicio), format = "%Y-%m-%d %H:%M:%S"))

# Calcular la duración en segundos entre Hora y Hora.Actual
Formulario <- Formulario %>%
  mutate(Duracion_Segundos = as.numeric(difftime(Hora.Final, Hora.Inicio, units = "secs")))

# Convertir la duración a formato HH:MM:SS
Formulario <- Formulario %>%
  mutate(Duracion_HMS = sprintf("%02d:%02d:%02d", Duracion_Segundos %/% 3600,
                                (Duracion_Segundos %% 3600) %/% 60,
                                Duracion_Segundos %% 60))

# Eliminar las filas que tienen el nombre "Emilio"
Formulario <- Formulario %>%
  filter(Nombre.y.Apellido != "Emilio")

# Convertir la planilla de formato largo a formato corto
Formulario_corto <- Formulario %>%
  pivot_longer(cols = matches("Severidad(\\.\\.|\\.)en\\.Hoja\\.\\d+"),
               names_to = "Hoja",
               names_pattern = "Severidad.*en\\.Hoja\\.(\\d+)",
               values_to = "Severidad") %>%
  mutate(Hoja = as.integer(Hoja)) %>%
  select(-Duracion_Segundos)

# Seleccionar solo las columnas Hoja y Valor_real
valores_reales <- Evaluadores %>%
  select(Hoja, Valor_real)

# Unir los dataframes usando la columna 'Hoja' como comparador
Formulario_corto <- Formulario_corto %>%
  left_join(valores_reales, by = "Hoja") %>%
  distinct()

# Crear la columna numero_de_evaluador
Formulario_corto <- Formulario_corto %>%
  mutate(Evaluador_N = as.numeric(factor(Nombre.y.Apellido)))

# Reordenar las columnas
Formulario_corto <- Formulario_corto %>%
  select(Fecha, Hora.Inicio, Hora.Final, Duracion_HMS, Evaluador_N, Nombre.y.Apellido, 
         Selecciona.el.tipo.de.Evaluación, everything(), -Fecha.final) %>%
  rename(Evaluacion_Tipo = Selecciona.el.tipo.de.Evaluación,
         Experimentado = Experimentado.en.Evaluación)


# Convertir Evaluadores a formato largo
Evaluadores_largo <- Evaluadores %>%
  pivot_longer(cols = c(Sev_sin_escala, Sev_con_escala),
               names_to = "Evaluacion_Tipo",
               values_to = "Severidad",
               names_prefix = "Sev_") %>%
  mutate(Evaluacion_Tipo = recode(Evaluacion_Tipo, "sin_escala" = "Sin Escala", "con_escala" = "Con Escala")) %>%
  rename(Nombre.y.Apellido = Persona,
         Evaluador_N = Evaluador) %>%
  mutate(Experimentado = "SI") %>% # Agregar la columna Experimentado a Evaluadores_largo
  mutate(Fecha = '2024-07-31')

# Agregar las columnas Experimentado y Duracion_HMS a Evaluadores_largo
Evaluadores_largo <- Evaluadores_largo %>%
  mutate(
    Duracion_HMS = case_when(
      Evaluacion_Tipo == "Con Escala" ~ "00:10:48",
      Evaluacion_Tipo == "Sin Escala" ~ "00:09:41",
      TRUE ~ NA_character_
    )
  )

#Unir la información de Evaluadores a Formulario_corto
# Realizar la unión
Formulario_completo <- bind_rows(Formulario_corto, Evaluadores_largo)

# Reasignar la columna Evaluador_N en Formulario_corto
Formulario_completo <- Formulario_completo %>%
  mutate(Evaluador_N = dense_rank(Nombre.y.Apellido))

# Verificar que cada evaluador tenga datos de 40 hojas con y sin escala
# Crear un dataframe que cuenta las evaluaciones por evaluador y tipo de evaluación
conteo_hojas <- Formulario_completo %>%
  group_by(Nombre.y.Apellido, Evaluador_N) %>%
  summarise(n_hojas = n()) %>%
  ungroup()

# Filtrar evaluadores que tengan 40 hojas en ambos tipos de evaluación
evaluadores_validos <- conteo_hojas %>%
  filter(n_hojas == 80) %>%
  group_by(Nombre.y.Apellido) %>%
  filter(n() == 2) %>%
  pull(Nombre.y.Apellido)

#Escribir datos
write.csv(Formulario_completo, 'EvaluacionServeridadOidio_7evaluadores.csv')
