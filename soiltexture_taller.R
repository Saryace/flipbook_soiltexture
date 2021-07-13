# Instalar los paquetes 

install.packages(c("tidyverse", #incluye ggplot2
                   "ggtern", #graficos ternarios
                   "soiltexture",# paquete para graficar textura
                   "RColorBrewer")) # paletas de colores

# Cargar los paquetes

library(tidyverse)
library(ggtern)
library(soiltexture)    
library(RColorBrewer)

# Creamos nuestro dataset

datos_suelo <- data.frame(
  suelo = c("a", "b", "c", "d"),
  arena = c(15, 18, 57, 32),
  limo = c(52, 70, 8, 26),
  arcilla = c(33, 12, 35, 42),
  om = c(1, 3, 4, 11),
  bd = c(1.33, 1.38, 1.76, 1.15)
)

# Triangulo usando {soiltexture}

TT.plot(
  class.sys = "USDA.TT",
  tri.data = datos_suelo,
  css.names = c("arcilla", "limo", "arena"),
  main = "Triángulo textura",
  z.name = "om",
  cex.axis = 0.8,
  cex.lab = 0.8
)

# Triangulo 1 usando {ggplot2} + {ggtern}

theme_set(theme_bw()) # dejamos definido theme

datos_suelo %>%
  ggtern(aes(
    x = arena,
    y = arcilla,
    z = limo,
    color = om
  )) +
  geom_point(size = 5) +
  theme_showarrows() +
  labs(yarrow = "Arcilla (%)",
       zarrow = "Limo (%)",
       xarrow = "Arena(%)") +
  scale_colour_gradient(low = "yellow",
                        high = "red") +
  theme_clockwise()

# Triangulo 2 usando {ggplot2} + {ggtern}

datos_suelo %>%
  ggtern(aes(
    x = arena,
    y = arcilla,
    z = limo,
    color = om,
    size = bd 
  )) + 
  geom_point() + 
  theme_showarrows() +
  labs(yarrow = "Arcilla (%)",
       zarrow = "Limo (%)",
       xarrow = "Arena(%)") +
  scale_colour_gradient(low = "yellow",
                        high = "red") +
  theme_clockwise()

# Triangulo 3 usando {ggplot2} + {ggtern}

data(USDA)
head(USDA)

USDA_texto <- USDA  %>% group_by(Label) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)

USDA_texto

ggplot(data = USDA, aes(
  y = Clay,
  x = Sand,
  z = Silt
)) +
  coord_tern(L = "x", T = "y", R = "z") +
  geom_polygon(
    aes(fill = Label),
    alpha = 0.0,
    size = 0.5,
    color = "black"
  ) +
  geom_text(data = USDA_texto,
            aes(label = Label),
            color = 'black',
            size = 2,
            family = "Times") +
  geom_point(
    data = datos_suelo,
    aes(
      x = arena,
      y = arcilla,
      z = limo
    ), 
  ) +
  theme_showarrows() +
  theme_clockwise() +
  theme(text = element_text(family = "Times")) +
  guides(fill=FALSE)

# Triangulo de cientificos de suelo

triangulo_cientificos <-
  USDA_texto %>% mutate(
    Label =  c(
      "Soil Surveyors",
      "Soil Judgers",
      "Digital Soil Mappers",
      "Soil Physicists",
      "Pedo-\nmetricians",
      "Pedo-\nmetricians",
      "Pedologists",
      "μ-morphologists",
      "Soil Ecologist",
      "Humus Expert",
      "Soil Crust Expert",
      "Soil Biologist"))
  
ggplot(data = USDA, aes(
  y = Clay,
  x = Sand,
  z = Silt
)) +
  coord_tern(L = "x", T = "y", R = "z") +
  geom_polygon(
    aes(fill = Label),
    alpha = 0.5,
    size = 0.5,
    color = "black"
  ) +
  geom_text(data = triangulo_cientificos,
            aes(label = Label),
            color = 'black',
            size = 2) +
  theme_showarrows() +
  theme_clockwise() +
  labs(yarrow = "Like camping and maps",
       zarrow = "Like worms and plants",
       xarrow = "Like computer and matemathics") +
  theme_hidetitles() +
  scale_fill_brewer(palette = "Paired") +
  guides(fill=FALSE)
