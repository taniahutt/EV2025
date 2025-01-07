# Instalar las librerias si es necesario
if (!require(rvest)) install.packages("rvest")
if (!require(stringr)) install.packages("stringr")
if (!require(dplyr)) install.packages("dplyr")
if (!require(tidyr)) install.packages("tidyr")
if (!require(readr)) install.packages("readr")

# Cargar las librerías necesarias
library(rvest)    # Para web scraping
library(stringr)  # Para manipulación de texto
library(dplyr)    # Para manipulación de datos
library(tidyr)    # Para dividir columnas y reorganizar datos

## Definir el directorio de trabajo: Nota, cambiar al directorio en su computador!
setwd("~/Dropbox/ISUC/Teaching/Summer School Nucleo")

# 1. Leer y procesar el HTML de la página
# -------------------------------------------------------------------------
# URL de la página web
url <- "https://cooperativa.cl/noticias/magazine/musica/shows-en-vivo/cartelera-de-conciertos-2025-en-chile/2024-07-30/115338.html"

# Leer la página web y cargar su contenido en formato HTML
pagina <- read_html(url)

# 2. Extraer el contenido de interés
# -------------------------------------------------------------------------
# Extraer la lista de conciertos de los nodos HTML correspondientes
conciertos_raw <- pagina %>%
  html_nodes("ul") %>%
  html_nodes("li")

# Extraer el texto de cada concierto
conciertos_text <- conciertos_raw %>%
  html_text() %>%
  str_trim()

# Extraer los enlaces asociados al texto "Entradas"
conciertos_links <- conciertos_raw %>%
  html_nodes("a") %>%
  html_attr("href")

# Asegurarse de que los enlaces tengan el mismo tamaño que los textos
if (length(conciertos_links) < length(conciertos_text)) {
  conciertos_links <- c(conciertos_links, rep(NA, length(conciertos_text) - length(conciertos_links)))
}

# 3. Procesar y estructurar los datos
# -------------------------------------------------------------------------
# Diccionario de meses permitidos
meses <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", 
           "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre")

# Combinar los datos de texto y enlaces en un tibble
conciertos_limpios <- tibble(
  texto = conciertos_text,
  link = conciertos_links
) %>%
  # Dividir la columna 'texto' en base al delimitador "-"
  separate(
    texto,
    into = c("Fecha", "Artista", "Lugar"),
    sep = "-",    # Usar el guion como delimitador
    extra = "merge", # Fusionar cualquier parte adicional en la última columna
    fill = "right"  # Rellenar con NA si faltan partes
  ) %>%
  # Limpiar la columna Lugar eliminando "- Entradas"
  mutate(Lugar = str_remove(Lugar, " - Entradas$")) %>%
  # Filtrar filas donde Artista y Fecha no son NA
  filter(!is.na(Artista), !is.na(Fecha)) %>%
  # Eliminar filas donde la fecha contiene "[SUSPENDIDO]"
  filter(!str_detect(Fecha, "\\[SUSPENDIDO\\]")) %>%
  # Generar una columna Mes usando el diccionario de meses
  mutate(
    Mes = str_extract(Fecha, paste(meses, collapse = "|"))  # Extraer cualquier coincidencia con los meses
  ) %>%
  # Expandir las fechas separadas por "y" o ","
  mutate(Fecha = str_replace_all(Fecha, " y ", ", ")) %>%  # Reemplazar " y " por ", "
  separate_rows(Fecha, sep = ", ") %>%  # Separar fechas individuales en filas
  # Reemplazar NA en fechas con el mes correspondiente
  mutate(
    Fecha = ifelse(str_detect(Fecha, "^\\d+$"), paste(Fecha, "de", Mes), Fecha)
  )

# 4. Guardar los datos en un archivo CSV
# -------------------------------------------------------------------------
write_csv(conciertos_limpios, "conciertos_2025_con_links.csv")
cat("¡Datos guardados en 'conciertos_2025_con_links.csv'!")


### PARTE 2: Analisis

# Cargar las librerías necesarias
library(tidyverse)

# Leer los datos
data <- read_csv("conciertos_2025_con_links.csv")

# 1. Ranking de los top 10 artistas con más fechas de conciertos
top_artistas <- data %>%
  count(Artista, sort = TRUE) %>%
  head(10)

# Mostrar el ranking de artistas
print(top_artistas)

# 2. Número de conciertos por mes
# Asegurarse de que la columna Mes está presente y en orden correcto
data <- data %>%
  mutate(Mes = str_extract(Fecha, "enero|febrero|marzo|abril|mayo|junio|julio|agosto|septiembre|octubre|noviembre|diciembre"))

conciertos_por_mes <- data %>%
  count(Mes, sort = TRUE)

# Graficar el número de conciertos por mes
ggplot(conciertos_por_mes, aes(x = reorder(Mes, -n), y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Número de Conciertos por Mes en 2025",
       x = "Mes",
       y = "Número de Conciertos") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 3. Top 10 lugares con más conciertos
top_lugares <- data %>%
  count(Lugar, sort = TRUE) %>%
  head(10)

# Mostrar el ranking de lugares
print(top_lugares)

# Graficar los top 10 lugares con más conciertos
ggplot(top_lugares, aes(x = n, y = reorder(Lugar, n))) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = "Top 10 Lugares con Más Conciertos en 2025",
       x = "Frecuencia de Conciertos",
       y = "Lugar") +
  theme_minimal()
