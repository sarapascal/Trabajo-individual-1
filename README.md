# Trabajo Individual 1: Analizando bases de datos del Eras Tour de Taylor Swift
###  Este proyecto analiza una base de datos del Tour mundial realizado por la artista Taylor Swift para realizar distintas funciones en R y Rmarkdown conectarlo con Github.

#### Cargamos las librerías a utilizar

  library(tidyverse)
  library(janitor)
  library(stringr)
  library(dplyr)

Cargamos la base de datos, usamos csv2 para que lea bien el archivo CSV ya que está separado por ";" y de esta forma se ordena directamente en columnas.
Taylor_dataset <- read_csv2("~/Desktop/Datos/Individual 1/Individual1/TaylorDataset.csv")

# Limpiamos base de datos

## Revisamos los nombres de las columnas
colnames(Taylor_dataset)

## Limpiamos los nombres de las columnas
Taylor_dataset <- Taylor_dataset |> 
  janitor::clean_names()

## Unificamos la base para que los nombres de las columnas estén en minúsculas.
Taylor_dataset <- Taylor_dataset |>
  mutate(city = str_to_lower(city),
         place = str_to_lower(place))

## Vamos a reducir la base de datos a los datos que queremos revisar
Taylor2 <- Taylor_dataset |>
  select(concert_id, city, date, tick_sales, opener_ar1, opener_ar2, place)

## Verificamos la nueva base
glimpse(Taylor2)

## Vamos a revisar cuántos conciertos hubo por ciudad, para esto utilizamos count que arroja el número de ocurrencias en este caso de cuantos conciertos hubo en cada una de las ciudades.
Taylor2 |>
  count(city, sort = TRUE)

## Veremos qué artistas telonearon como telonero principal y cuántas veces, para esto también utilizamos count.
Taylor2 |>
  count(opener_ar1, sort = TRUE)

## Y también veremos que artistas telonearon como telonero secundario y cuántas veces.
Taylor2 |>
  count(opener_ar2, sort = TRUE)

## ¿Cuántas entradas se vendieron en promedio? Para esto utilizamos mean.
mean(Taylor2$tick_sales, na.rm = TRUE)

## ¿En qué concierto se vendieron más entradas? 
Taylor2 |>
  filter(tick_sales == max(tick_sales, na.rm = TRUE)) |>
  select(city, date, tick_sales)

### max(tick_sales, na.rm = TRUE) nos ayuda a busca el valor máximo de la columna tick_sales (ventas de entradas), ignorando los NA si los hay, luego filter() selecciona solo las filas donde tick_sales es igual a ese valor máximo. Es decir, el concierto con más entradas vendidas. Después de filtrar, select () extrae solo las columnas city, date y tick_sales para mostrar la información relevante.

## Guardar nueva base como archivo
write_csv(Taylor2, "~/Desktop/Datos/Individual 1/Individual1/Taylor2.csv")

# Vamos a visualizar los datos

## Telonera/o principal por ciudad en un gráfico de barras.
Taylor2 |>
  count(city, opener_ar1) |>
  ggplot(aes(x = city, y = n, fill = opener_ar1)) +
  geom_col(position = "dodge") +
  labs(title = "¿Quiénes telonearon en cada ciudad? (Solo teloneros principales)",
       x = "Ciudad",
       y = "Cantidad de conciertos",
       fill = "Telonero principal") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Descripción:
### count(city): Cuenta cuántas veces aparece cada ciudad en la columna city, es decir, cuántos conciertos hubo en cada ciudad. El resultado es un nuevo data frame con dos columnas: city y n (el número de conciertos).
### ggplot(aes(x = reorder(city, n), y = n)): Crea el gráfico usando ggplot2 y reorder(city, n) ordena las ciudades según el número de conciertos (de menor a mayor). Así x será la ciudad (ordenada), y será el número de conciertos.
### geom_col(fill = "purple"): Dibuja las barras del gráfico con color púrpura.
### coord_flip(): Invierte los ejes, para que las ciudades se muestren en el eje vertical y las barras en horizontal para mejorar la legibilidad ya que hay muchas ciudades.
### labs(...): Nos sirve para agregar títulos a los ejes.
### Con theme_minimal(): La damos el aspecto al gráfico.

## Teloneos totales  en un gráfico de barras.
Taylor2 |>
  pivot_longer(cols = c(opener_ar1, opener_ar2),
               names_to = "tipo_telonero",
               values_to = "telonero") |>
  filter(!is.na(telonero)) |>
  count(city, telonero) |>
  ggplot(aes(x = city, y = n, fill = telonero)) +
  geom_col(position = "stack") +
  labs(title = "¿Quiénes telonearon por ciudad?",
       x = "Ciudad",
       y = "Cantidad de conciertos",
       fill = "Artista telonero") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Descripción:
### En este gráfico se añaden dos pasos más a los anteriores.
### pivot_longer(...): Convierte las columnas opener_ar1 y opener_ar2 (los teloneros) en una sola columna llamada telonero. Esto permite analizar todos los teloneros juntos, sin importar si fueron el primero o el segundo.
### filter(!is.na(telonero)): Elimina las filas donde no hay telonero (es decir, valores NA).

## Cantidad de conciertos por ciudad en un gráfico de barras.
Taylor2 |>
  count(city) |>
  ggplot(aes(x = reorder(city, n), y = n)) +
  geom_col(fill = "purple") +
  coord_flip() +
  labs(title = "Cantidad de conciertos por ciudad",
       x = "Ciudad",
       y = "Número de conciertos") +
  theme_minimal()

### Descripción:
### En este gráfico se realizan los mismos pasos anteriores, pero geom_col(fill = "purple") dibuja las barras del gráfico con color morado.

## Conclusión
### Este trabajo permitió revisar visualmente las ciudades en que se realizaron conciertos de Taylor Swift, eventualmente se podía generar un mapa para mejorar esta visualización. La base de datos estaba ordenada por lo que fue sencillo trabajar con ella y no fue necesario realizar tantos ajustes, por lo que se optó por simplificar en términos de cantidad de datos principalmente.
### Respecto a los datos se pudo revisar que la ciudad con más ventas fue Arlington y además, la revisión de teloneros se complejizaba ya que algunos estaban como principales en algunas ciudades y como secundarios en otras, por lo que se repetían, lo que se puede ver en el gráfico de teloneros totales.

# Gracias por revisar este análisis de datos del Tour Mundial "Eras" de Taylor Swift
## La base de datos fue obtenida desde https://www.kaggle.com/datasets/tymonbot/taylor-swift-eras-toure?resource=download

