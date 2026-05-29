#install.packages("sf")
library(sf)
library(dplyr)
#install.packages("sampling")
library(sampling)
library(readxl)
##1.1) subir bases de datos
base=st_read("MGN_ANM_MANZANA.shp")
base_P=read_xlsx("BASE_PROBABILISTICA.xlsx")
##Filtrar y depuración
base_cund=base %>%
  filter(DPTO_CCDGO == "25") %>%
  filter(TVIVIENDA > 0)
base_NP=base_cund %>% filter(!COD_DANE_A %in% base_P$COD_DANE_A)

#muestra no probabilistica
base_NP <- base_NP %>%
  mutate(
    poblacion_joven = TP34_3_EDA + TP34_4_EDA,
    prop_apartamentos = ifelse(TVIVIENDA > 0, TP14_2_TIP / TVIVIENDA, 0),
    peso_no_prob = (prop_apartamentos + 0.1) * (poblacion_joven + 1) * (TVIVIENDA^1.1)
    )

#muestra no probabilistica
set.seed(76984)
tamanho_no_prob <- 1500

base_NP <- base_NP %>%
  slice_sample(n = tamanho_no_prob, weight_by = peso_no_prob, replace = FALSE)


# 5. Diagnóstico de control para verificar que el sesgo se aplicó con éxito
cat("--- VERIFICACIÓN DEL SESGO GENERADO --- \n")
cat("Proporción prom. de apartamentos en Marco Original: ", mean(base$TP14_2_TIP / base$TVIVIENDA, na.rm=TRUE), "\n")
cat("Proporción prom. de apartamentos en NO Probabilística:", mean(base_NP$TP14_2_TIP / base_NP$TVIVIENDA, na.rm=TRUE), "\n\n")
cat("Proporción prom. de apartamentos en Probabilística: ", mean(base_P$TP14_2_TIP / base_P$TOTAL_VIVIENDAS, na.rm=TRUE), "\n")

cat("Promedio de jóvenes (20-39) en Probabilística: ", mean(base_P$TP34_3_EDA + base_P$TP34_4_EDA), "\n")
cat("Promedio de jóvenes (20-39) en NO Probabilística:", mean(base_NP$TP34_3_EDA + base_NP$TP34_4_EDA), "\n")
####################################################################################################################

# 1. Cargar librerías
library(dplyr)

# --- PREPARACIÓN DE LAS BASES SEGÚN TU DICCIONARIO ---
# Muestra Probabilística (S_a): Creamos el indicador muestra_id = 0
S_a <- base_P %>%
  mutate(
    muestra_id = 0,
    # Homologamos el nombre para que coincida con la no probabilística
    prop_apartamentos = TP14_2_TIP / TOTAL_VIVIENDAS, 
    poblacion_joven = TP34_3_EDA + TP34_4_EDA
  ) %>%
  # Seleccionamos el ID, las variables auxiliares (X) y el identificador de muestra
  select(COD_DANE_A, prop_apartamentos, poblacion_joven, muestra_id)

# Muestra No Probabilística (S_b): Creamos el indicador muestra_id = 1
# Nota: Aquí debes incluir también tu VARIABLE OBJETIVO (la que quieres estimar, ej: Y)
S_b <- base_NP %>%
  mutate(
    muestra_id = 1,
    prop_apartamentos = TP14_2_TIP / TVIVIENDA,
    poblacion_joven = TP34_3_EDA + TP34_4_EDA
  ) %>%
  # Seleccionamos ID, auxiliares, el identificador y TU VARIABLE DE INTERÉS (Y)
  # (Reemplaza 'TU_VARIABLE_Y' por el nombre real de lo que deseas estimar)
  select(COD_DANE_A, prop_apartamentos, poblacion_joven, muestra_id, TP19_INTE1)


# 2. UNIÓN DE MUESTRAS (Pool)
# Tal como indica tu documento: pool <- bind_rows(S_a, S_b)
pool <- bind_rows(S_a, S_b)


# 3. MODELO DE PROPENSIÓN (Clasificador Logístico)
# Modelamos la pertenencia a la muestra no probabilística usando las variables que causaron el sesgo
modelo_psa <- glm(muestra_id ~ prop_apartamentos + poblacion_joven, 
                  data = pool, 
                  family = binomial(link = "logit"))

# Predecir las propensidades (probabilidades de inclusión simuladas)
pool$propensidad <- predict(modelo_psa, type = "response")


# 4. CÁLCULO DE PESOS DE AJUSTE (Valliant)
# Filtramos solo la muestra No Probabilística y aplicamos la fórmula: (1 - p) / p
S_b_ajustada <- pool %>%
  filter(muestra_id == 1) %>%
  mutate(peso = (1 - propensidad) / propensidad)


# 5. ESTIMACIÓN FINAL DE TU VARIABLE DE INTERÉS (Y)
# Estimación Sesgada (Sin ponderar)
media_sesgada <- mean(S_b_ajustada$TP19_INTE1, na.rm = TRUE)

# Estimación Ajustada usando los Pesos de Valliant
media_ajustada <- sum(S_b_ajustada$TP19_INTE1 * S_b_ajustada$peso, na.rm = TRUE) / 
  sum(S_b_ajustada$peso, na.rm = TRUE)

# --- RESULTADOS ---
cat("Estimación Sesgada (No Probabilística):", round(media_sesgada, 2), "\n")
cat("Estimación Ajustada (Método Valliant):   ", round(media_ajustada, 2), "\n")

###############################################################################################################
library(ggplot2)

# 1. Preparar los datos para la gráfica
# Necesitamos la muestra probabilística (referencia) y la no probabilística (con y sin peso)
graf_prob <- S_a %>% select(prop_apartamentos) %>% mutate(Distribucion = "Muestra Probabilística (Referencia)")
graf_np   <- S_b_ajustada %>% select(prop_apartamentos, peso)

# 2. Construcción del gráfico con ggplot2
ggplot() +
  # Línea/Área de la Muestra Probabilística (Base de Referencia)
  geom_density(data = graf_prob, aes(x = prop_apartamentos, fill = Distribucion), 
               alpha = 0.4, color = "grey30", linewidth = 0.6) +
  
  # Línea/Área de la Muestra No Probabilística SESGADA (Sin usar los pesos)
  geom_density(data = graf_np, aes(x = prop_apartamentos, fill = "Muestra No Probabilística (Sesgada)"), 
               alpha = 0.4, color = "red", linewidth = 0.6) +
  
  # Línea/Área de la Muestra No Probabilística AJUSTADA (Usando weight = peso)
  geom_density(data = graf_np, aes(x = prop_apartamentos, weight = peso, fill = "Muestra Ajustada (Valliant)"), 
               alpha = 0.4, color = "blue", linewidth = 0.7) +
  
  # 3. Personalización de colores e idéntica estética a tu PDF
  scale_fill_manual(values = c(
    "Muestra Probabilística (Referencia)" = "grey60", 
    "Muestra No Probabilística (Sesgada)" = "#E41A1C",       # Rojo institucional de sesgo
    "Muestra Ajustada (Valliant)"         = "#377EB8"        # Azul de corrección
  )) +
  
  # 4. Etiquetas del gráfico
  labs(
    title = "Corrección del Sesgo de Selección vía PSA",
    subtitle = "Ajuste de la distribución de la proporción de apartamentos mediante pesos de propensión",
    x = "Proporción de Apartamentos por Manzana",
    y = "Densidad",
    fill = "Distribución"
  ) +
  
  # 5. Formato limpio
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

