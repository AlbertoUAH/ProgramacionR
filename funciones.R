# Script Practica Programacion R
# Author: Alberto Fernandez Hernandez
# Pregunta 1
generar_boletos <- function(num.digitos) {
  lista.combinaciones <- rep(list(seq(0,9)), num.digitos)
  expand.grid(lista.combinaciones)
}
# Prueba
df.combinaciones.sorteo <- generar_boletos(4)

head(df.combinaciones.sorteo, 5)
tail(df.combinaciones.sorteo, 5)

# Comprobamos que el numero de filas es 10000
nrow(df.combinaciones.sorteo)

# Lo pasamos a vector para mostrar cada combinacion con los cuatro digitos
vector.combinaciones <- apply(df.combinaciones.sorteo, 1, paste, collapse = "")
# Mostramos las primeras 50 combinaciones
head(vector.combinaciones, 50)

# Apartado 2
suma.combinaciones <- apply(df.combinaciones.sorteo, 1, sum)
# Ejemplo de salida
suma.combinaciones[1:20]

moda <- function(vector.suma) {
  vector.suma.frecuencias <- table(vector.suma)
  as.numeric(names(vector.suma.frecuencias)[vector.suma.frecuencias == max(vector.suma.frecuencias)])
}
cat("La suma de los numeros que mas se repite es ", moda(suma.combinaciones))

# Pregunta 2
# Apartado 1
# Comunidades Autonomas
cod.ccaa <- read.table("CodCCAA.csv", sep = "\t", header = TRUE)
head(cod.ccaa)
#Dimensiones (Filas x Columnas)
dim(cod.ccaa)

# Provincias
cod.prov <- read.table("CodProv.txt", sep = ",", header = TRUE)
head(cod.prov)
#Dimensiones (Filas x Columnas)
dim(cod.prov)

# Datos provincias
# na.string = "" => Para no confundir NA (Navarra) con un valor NA
datos.provincias <- read.table("datos_provincias.csv", sep = ",", header = TRUE, 
                               na.strings = "")
head(datos.provincias)
#Dimensiones (Filas x Columnas)
dim(datos.provincias)

# Renombramos las columnas que presentan tildes (ademas del campo denotado como "X")
colnames(cod.ccaa) <- c("Cod.Comunidad", "Nombre.Comunidad", "Num")
colnames(cod.prov) <- c("Codigo", "Nombre.de.la.subdivision.en.la.ISO1", 
                        "Comunidad.Autonoma")

# Renombramos los campos CCAA y Provincia
cod.prov <- transform(cod.prov, Comunidad.Autonoma = 
                        paste("ES-", Comunidad.Autonoma, sep = ""))
datos.provincias <- transform(datos.provincias, provincia_iso = 
                                paste("ES-", provincia_iso, sep = ""))

juntar_tres_dataframes <- function(vector.df, vector.claves) {
  merge(merge(vector.df[1], vector.df[2], by.x = vector.claves[1], 
              by.y = vector.claves[2], all.x = TRUE), vector.df[3], 
        by.x = vector.claves[3], by.y = vector.claves[4], all.x = TRUE)
}
datos.ccaa <- juntar_tres_dataframes(vector.df = list(datos.provincias, cod.prov, cod.ccaa), 
                                     vector.claves = c("provincia_iso", "Codigo", 
                                                       "Comunidad.Autonoma", "Cod.Comunidad") 
)

# Comprobemos que el numero de filas equivale al de datos.provincias
nrow(datos.ccaa) == nrow(datos.provincias)
# Consultemos las primeras filas
head(datos.ccaa)

# Prueba para comprobar que cada Provincia se encuentra en su correspondiente CCAA
lapply(unique(datos.ccaa[, "Comunidad.Autonoma"]), function(x) {
  if (is.na(x)) {
    vector.provincias <- c(paste0(x, " => "), 
                           unique(datos.ccaa[is.na(datos.ccaa["Comunidad.Autonoma"]), 
                                             "provincia_iso"]))
  }else {
    vector.provincias <- c(paste0(x, " => "), 
                           unique(datos.ccaa[datos.ccaa["Comunidad.Autonoma"] == x & 
                                               !is.na(datos.ccaa["Comunidad.Autonoma"]), "provincia_iso"]))
  }
  vector.provincias
})
# Almacenamos los datos de provincias con el codigo de la CC.AA
# write.csv(datos.ccaa[c("Comunidad.Autonoma", colnames(datos.provincias))],"datos_provincias.csv", row.names = FALSE)

# Apartado 2
# De forma previa, eliminamos las columnas Nombre.de.la.subdivision.en.la.ISO1 y Nombre.Comunidad
# correspondientes con las columnas 9 y 10
datos.ccaa <- datos.ccaa[, -c(9, 10)]

seleccionar_datos_comunidad <- function(datos.ccaa, dni) {
  subset(datos.ccaa, Num == (dni %% 17))
}
# Prueba con Castilla y Leon (DNI = 12345678 mod 17 -> 6)
head(seleccionar_datos_comunidad(datos.ccaa, 12345678), 5)

# Para este apartado nos corresponde Cantabria (DNI = 54003003 mod 17 -> 4)
datos.filtrado <- seleccionar_datos_comunidad(datos.ccaa, 54003003)
head(datos.filtrado, 5)

# Apartado 3
# Inicialmente, agrupamos por fecha
agrupar_datos <- function(datos.ccaa, clave, columnas) {
  agrupacion <- by(datos.ccaa, list(datos.ccaa[, clave]), function(fila) {
    data.frame(
      total = unique(fila[, clave]),
      do.call(cbind,
              lapply(columnas, function(columna) sum(fila[, columna]))
      )
    )
  })
  # Eliminamos los indices de fila (por defecto es la primera columna)
  rownames(agrupacion) <- NULL
  df.agrupado <- do.call(rbind, agrupacion)
  colnames(df.agrupado) <- c(clave, columnas)
  df.agrupado
}
# Prueba agrupar_datos
datos.agrupados.fecha <- agrupar_datos(datos.filtrado, "fecha", "num_casos")
# Echamos un primer vistazo a los datos obtenidos
tail(datos.agrupados.fecha)

# Lo convertimos a tipo de dato Date (en lugar de factor)
datos.agrupados.fecha[, "fecha"] <- as.Date(datos.agrupados.fecha[, "fecha"])
sapply(datos.agrupados.fecha, class)

imprimir_grafica <- function(datos, eje.x, eje.y, saltos.eje.x, color, segmentos = FALSE) {
  par(mar=c(11,4,4,1), xaxt = "n")
  plot(x = datos[, eje.x], y = datos[, eje.y], type = "l",
       ylim = range(pretty(c(0, datos[, eje.y]))),
       main = "EVOLUCION NUMERO DE CASOS COVID-19", 
       xlab = "", ylab = "NUMERO DE CASOS", font.lab = 2, 
       col = color, las = 2)
  par(xaxt = "s")
  sec <- seq(datos[1, eje.x], datos[nrow(datos[eje.x]), eje.x], 
             by = saltos.eje.x)
  
  axis.Date(1, at = sec, format = "%Y-%m-%d", las = 2)
  
  if (segmentos == TRUE) {
    segments(sec, 0, sec, subset(datos, datos[, eje.x] %in% sec)[, eje.y],
             lty = 2)
    points(x = subset(datos, datos[, eje.x] %in% sec), pch = 20)
    lines(smooth.spline(datos[, eje.x],
                        datos[, eje.y]),
          col = rgb(red = 0, green = 0, blue = 1, alpha = 0.7), 
          lwd = 2)
  }
  mtext(text = "FECHA", side = 1, line = 6, font = 2)
}
# Prueba imprimir_grafica
imprimir_grafica(datos.agrupados.fecha, "fecha", "num_casos", 14, "red", TRUE)

# Apartado 4
casos.por.columnas <- agrupar_datos(datos.filtrado, "fecha", c("num_casos", 
                                                               "num_casos_prueba_pcr", 
                                                               "num_casos_prueba_test_ac", "num_casos_prueba_otras", 
                                                               "num_casos_prueba_desconocida"))
# Modificamos nuevamente el campo fecha (factor -> Date)
casos.por.columnas[, "fecha"] <- as.Date(casos.por.columnas[, "fecha"])

# Mostramos las ultimas seis filas
tail(casos.por.columnas)

imprimir_multiples_lineas <- function(datos, eje.x, eje.y, paleta, saltos.eje.x) {
  imprimir_grafica(datos, eje.x, eje.y[1], saltos.eje.x, paleta[1])
  
  mapply(FUN = function(x, y) { 
    lines(datos[, eje.x], datos[, x], col = y, lwd = 2)
  }, eje.y, paleta)
  
  eje.y <- lapply(gsub('_', ' ', eje.y), toupper)
  legend(x= "top",  legend = eje.y, fill = paleta, cex = 0.7, text.font = 2, bg = 'white') 
}

# Prueba imprimir_multiples_lineas con la Comunidad Autonoma Santander
columnas <- c("num_casos", "num_casos_prueba_pcr", "num_casos_prueba_test_ac",
              "num_casos_prueba_otras", "num_casos_prueba_desconocida")
paleta <- c("red", "blue", "orange", "darkgreen", "purple")

imprimir_multiples_lineas(casos.por.columnas, "fecha", columnas, paleta, 14)

# Prueba imprimir_multiples_lineas en toda Espana
casos.por.columnas <- agrupar_datos(datos.ccaa, "fecha", columnas)
casos.por.columnas[, "fecha"] <- as.Date(casos.por.columnas[, "fecha"])

imprimir_multiples_lineas(casos.por.columnas, "fecha", columnas, paleta, 14)

# Pregunta 3
# Apartado 1
# Para importar un fichero sas7bdat, necesitamos instalarnos el paquete sas7bdat
install.packages("sas7bdat", repos = "http://cran.us.r-project.org")

# Una vez instalado, lo cargamos
library(sas7bdat)
# Vemos que aparece, junto con las librerias estandar de R
(.packages())

punt <- read.sas7bdat("Punt.sas7bdat")
# Comprobamos que se trata, efectivamente, de un DataFrame
class(punt)
# Una vez cargado, echamos un primer vistazo a las filas...
head(punt)
# ... Y a las columnas ...
sapply(punt, class)

# Apartado 2
calcular_total_puntuacion <- function(puntuaciones, id.alumno, columnas.test) {
  cbind(puntuaciones[c(id.alumno, columnas.test)], 
        OVERALL = apply(puntuaciones[columnas.test], 1, function(x) {
          mean(c((x[1:length(x) - 1]), 2*x[length(x)]))
        }))
}
overall <- calcular_total_puntuacion(punt, "SEGSOC", c("TEST1", "TEST2", "TEST3", "TEST4"))
overall

# Apartado 3
anadir_columna_fecha <- function(puntuaciones, dia.mes, columnas) {
  cbind(puntuaciones[columnas], START = apply(puntuaciones[dia.mes], 1, function(x) {
    paste0(format(Sys.Date(), "%Y"), "-", x)
  }))
}
start <- anadir_columna_fecha(punt, "ENROLLED", c("SEGSOC", "COURSE"))
start

# Apartado 4
dividir_columna <- function(puntuaciones, codigo, columna) {
  punt.filtrado <- puntuaciones[grep(paste0(codigo,"$"), puntuaciones[, columna]), ]
  regex <- "([A-Za-z]+)([0-9]+)"
  punt.filtrado <- transform(punt.filtrado, 
                             SUBJECT = gsub(regex, replacement = "\\1" , 
                                            x = punt.filtrado[, columna]),
                             LEVEL = gsub(regex, replacement = "\\2" , 
                                          x = punt.filtrado[, columna])
  )
  punt.filtrado[, c("SUBJECT", "LEVEL")] <- lapply(punt.filtrado[, c("SUBJECT", "LEVEL")], 
                                                   as.character)
  rownames(punt.filtrado) <- NULL
  punt.filtrado
}

level500 <- dividir_columna(punt, "500", "COURSE")
level500

# Comprobamos el tipo de dato de las nuevas columnas
sapply(level500, class)

# Apartado 5
write.table(level500, file = "level500.dat", row.names = FALSE)

# Pregunta 4
puntuaciones.hidrogel <- structure(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 
                                     16, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                                     1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                                     1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 1, 1, 1, 1, 1, 3, 
                                     2, 1, 1, 1, 1, 1, 1, 2, 1, 4, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 
                                     2, 3, 3, 1, 2, 1, 1, 1, 1, 1, 3, 4, 1, 1, 1, 2, 3, 4, 3, 1, 2, 
                                     1, 1, 4, 1, 1, 4, 4, 1, 1), .Dim = c(16L, 8L))
puntuaciones.hidrogel

# Apartado 1
calcular_vector_frecuencias <- function(puntuaciones, categorias) {
  frecuencias <- tabulate(puntuaciones) / length(puntuaciones)
  c(as.vector(rbind(frecuencias, 1 - frecuencias)), rep(c(0,1), 
              categorias - length(frecuencias)))
}
# Prueba con la semana 7 (columna 7 + 1)
calcular_vector_frecuencias(puntuaciones.hidrogel[ ,8], 4)

# Apartado 2
matriz.frecuencias <- matrix(unlist(apply(
  puntuaciones.hidrogel[, -1], 2, calcular_vector_frecuencias, categorias = 4)),
  nrow = 8)

rownames(matriz.frecuencias) <- c("1", "1-f1", "2", "1-f2", "3", "1-f3", "4", "1-f4")
colnames(matriz.frecuencias) <- c("S1", "S2", "S3", "S4", "S5", "S6", "S7")

# Comprobamos que se trata, efectivamente, de una matriz
class(matriz.frecuencias)
# Mostramos su contenido
matriz.frecuencias

# Apartado 3
par(xpd = TRUE, mar = par()$mar + c(0,0,0,4))
barplot(matriz.frecuencias, col = c("black", "white"), 
        main = "EVOLUCION SENSACION DE ARDOR CON EL TIEMPO")
legend(x= "topright",  legend = c("FREC. ARDOR", "FREC. SIN ARDOR"), 
       inset = c(-0.3, 0), fill = c("black", "white"), 
       cex = 0.6, text.font = 2, bg = 'white')

# Apartado 4
mostrar_frecuencias <- function(matriz, ancho, espacio, titulo) {
  par(xpd = TRUE, mar = c(0,4,5,6))
  barplot(matriz, col = c("red", "white"), width = ancho, 
          space = espacio, xaxt = "n", yaxt = "n",
          ylab = "PUNTUACION", cex.lab = 1.25)
  
  v <- Reduce(function(v, x) v + 2 * ancho / 2 + espacio, 
              x=numeric(length(colnames(matriz)) - 1),  
              init=ancho / 2 + espacio, 
              accumulate=TRUE)
  axis(side = 3, at = v, colnames(matriz), col.axis = "blue", font = 2, tick = FALSE)
  
  w <- Reduce(function(w, x) w + 2 * ancho / 2, 
              x=numeric(length(row.names(matriz)) / 2 - 1),
              init=ancho / 2, 
              accumulate=TRUE)
  axis(side = 2, at = w, row.names(matriz)[seq(1,length(row.names(matriz)),2)], 
       col.axis = "blue", cex = 125, font = 2, tick = FALSE)
  
  title(titulo, line = 4, cex.main = 1.25)
  legend(x= "topright",  legend = c("FREC. ARDOR", "FREC. SIN ARDOR"), 
         inset = c(-0.2, 0), fill = c("red", "white"), 
         cex = 0.6, text.font = 2, bg = 'white') 
}
# Prueba mostrar_frecuencias
mostrar_frecuencias(matriz.frecuencias, 1, 0.2, "FRECUENCIAS DE ARDOR DE HIDROGEL - SEMANAS 1 A LA 7")
