# Riesgo país y sociedad
# Andrés Soria
# El Quantificador

# Preliminares --------------------------------------------------------------------------------------------

# Cargar librerías
library(readr)
library(ggplot2)
library(lubridate)
library(stringr)
library(here)

# Importar base

url <- "https://raw.githubusercontent.com/laboratoriolide/datos-bce/main/Data/Puntos%20riesgo%20pa%C3%ADs.csv"

download.file(url, here("Data/Puntos riesgo país.csv"))

Puntos_riesgo_país <- read.delim("Data/Puntos riesgo país.csv", sep = ";", header = TRUE)

# Creación de tema

theme_article_rp <-
  theme_classic(base_size = 15) +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(color = "grey20"),
        plot.subtitle = element_text(color = "grey30"),
        legend.background = element_blank())

# Análisis ------------------------------------------------------------------------------------------------

# Dataframe de variables
data <- 
  data.frame(Periodo = periodo, 
             Puntos.base = puntos_base)

# Convertir la columna 'Periodo' a tipo Date
data$Periodo <- as.Date(data$Periodo, format = "%d/%m/%Y")

# Filtrar el data frame por las fechas deseadas
filtered_data <- data[data$Periodo >= as.Date("2019-01-01", format="%Y-%m-%d") & data$Periodo <= as.Date("2023-06-01", format="%Y-%m-%d"),]

# Crear los vectores a partir de los datos filtrados
periodo <- filtered_data$Periodo
puntos_base <- filtered_data$Puntos_base

# Secuencia de meses 

date_breaks <- seq(floor_date(min(data$Periodo), unit = "month"),
                   ceiling_date(max(data$Periodo), unit = "month"),
                   by = "4 months")

# Definir las coordenadas de la zona a pintar

zona_xmin <- as.Date("01/10/2019", format = "%d/%m/%Y")
zona_xmax <- as.Date("01/12/2019", format = "%d/%m/%Y")
zona_ymin <- 0
zona_ymax <- 5500

zona_xmin1 <- as.Date("01/02/2020", format = "%d/%m/%Y")
zona_xmax1 <- as.Date("01/04/2020", format = "%d/%m/%Y")

zona_xmin2 <- as.Date("01/01/2021", format = "%d/%m/%Y")
zona_xmax2 <- as.Date("01/04/2021", format = "%d/%m/%Y")

zona_xmin3 <- as.Date("01/05/2022", format = "%d/%m/%Y")
zona_xmax3 <- as.Date("01/09/2022", format = "%d/%m/%Y")

zona_xmin4 <- as.Date("01/01/2023", format = "%d/%m/%Y")
zona_xmax4 <- as.Date("01/03/2023", format = "%d/%m/%Y")

zona_xmin5 <- as.Date("01/04/2023", format = "%d/%m/%Y")
zona_xmax5 <- as.Date("01/06/2023", format = "%d/%m/%Y")

zona_xmin6 <- as.Date("01/07/2020", format = "%d/%m/%Y")
zona_xmax6 <- as.Date("01/09/2020", format = "%d/%m/%Y")

caption_grafico_rp <-
  'El riesgo país indica la diferencia entre la tasa de interés de los bonos soberanos del gobierno de un país (riesgosa) y la tasa de interés libre de riesgo (bonos E.E.U.U.). Un punto base es 0.01%. La primera zona roja del gráfico denota el período de la crisis política causada por el paro nacional en octubre del 2019. La segunda zona roja indica al inicio de la pandemia del COVID-19 a finales de febrero. La primera zona verde es la disminución asociada con el préstamo del FMI. La segunda zona verde indica la caída del RP durante posicionamiento del gobierno de Guillermo Lasso (CREO). La tercera zona roja muestra el período donde de otro paro nacional que tomó fuerza en Julio 2022. Por último, se denota una zona roja en el comienzo del 2023 donde se observó inestabilidad del gobierno durante el juicio político al presidente Guillermo Lasso. Finalmente en gris se denotan los eventos en curso a la fecha de elaboración del gráfico tras la disolución de la Asamblea Nacional por "muerte cruzada". Datos del Banco Central del Ecuador.'

# Gráfico de líneas para promedio y puntos (1)

grafico_riesgo <- 
  ggplot(data, aes(x = Periodo, y= Puntos.base)) +
  geom_line(colour = 'black') +
  geom_point(color = 'black') +
  scale_x_date(breaks = date_breaks, date_labels = '%b-%Y') +
  scale_y_continuous(breaks = seq(500, 6000, 1000))+
  labs(x = '',
       y = 'Puntos base (1 = 0.01%)',
       title = 'Riesgo país Ecuador 2019-2023',
       subtitle = 'Puntos base mensuales de los bonos soberanos sobre la tasa libre de riesgo',
       caption = str_wrap(caption_grafico_rp,150)) +
  theme_article_rp +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(face = 'bold'),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 12, hjust = 0, face = 'italic'),
        plot.title.position = 'plot',
        plot.caption.position = 'plot')+
  annotate('rect', xmin = zona_xmin, xmax = zona_xmax, ymin = zona_ymin, ymax = zona_ymax,
      alpha = 0.1, fill = 'red')+
  annotate('rect', xmin = zona_xmin1, xmax = zona_xmax1, ymin = zona_ymin, ymax = zona_ymax,
           alpha = 0.1, fill = 'red')+
  annotate('rect', xmin = zona_xmin2, xmax = zona_xmax2, ymin = zona_ymin, ymax = zona_ymax,
           alpha = 0.1, fill = 'green')+
  annotate('rect', xmin = zona_xmin3, xmax = zona_xmax3, ymin = zona_ymin, ymax = zona_ymax,
           alpha = 0.1, fill = 'red')+
  annotate('rect', xmin = zona_xmin4, xmax = zona_xmax4, ymin = zona_ymin, ymax = zona_ymax,
           alpha = 0.1, fill = 'red')+
  annotate('rect', xmin = zona_xmin5, xmax = zona_xmax5, ymin = zona_ymin, ymax = zona_ymax,
           alpha = 0.1, fill = 'grey60')+
  annotate('rect', xmin = zona_xmin6, xmax = zona_xmax6, ymin = zona_ymin, ymax = zona_ymax,
         alpha = 0.1, fill = 'green')

grafico_riesgo

ggsave(filename = "Figures/grafico_riesgo_pais.png", 
       plot = grafico_riesgo, 
       width = 12, 
       height = 8, 
       dpi = 900)







