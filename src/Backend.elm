module Backend exposing(..)
import Models exposing(Movie, Preferences)
import List exposing (all,any,length,map, reverse)

completaAca = identity

filtrarPeliculasPorPalabrasClave : String -> List Movie -> List Movie
filtrarPeliculasPorPalabrasClave palabras listaDePeliculas = List.filter (tienePalabras (dePalabraAListaDePalabras palabras)) listaDePeliculas

dePalabraAListaDePalabras : String -> List String
dePalabraAListaDePalabras palabras = String.split " " (String.toUpper palabras)

tienePalabras : List String -> Movie -> Bool
tienePalabras palabras pelicula = 
  case palabras of
     [] -> False
     [cabeza] -> String.contains cabeza (String.toUpper pelicula.title) && cabeza /= "\0"
     (cabeza::cola) -> String.contains cabeza (String.toUpper pelicula.title) && cabeza /= "\0" && tienePalabras cola pelicula

filtrarPeliculasPorGenero : String -> List Movie -> List Movie
filtrarPeliculasPorGenero genero moviesCollection = List.filter (mismoGenero genero) moviesCollection

mismoGenero : String -> Movie -> Bool
mismoGenero genero pelicula = List.member genero pelicula.genre

filtrarPeliculasPorMenoresDeEdad : Bool -> List Movie -> List Movie
filtrarPeliculasPorMenoresDeEdad mostrarSoloMenores peliculas = if mostrarSoloMenores then aptoParaMenores peliculas else peliculas

aptoParaMenores : List Movie -> List Movie
aptoParaMenores = List.filter .forKids

ordenarPeliculasPorRating : List Movie -> List Movie
ordenarPeliculasPorRating listaDePeliculas = reverse ((List.sortBy .rating) listaDePeliculas)

darLikeAPelicula : Int -> List Movie -> List Movie
darLikeAPelicula id = map (incrementarUnLike id)

incrementarUnLike : Int -> Movie -> Movie
incrementarUnLike id pelicula = if (id == pelicula.id) then {pelicula | likes = pelicula.likes + 1} else pelicula

-- **************
-- Requerimiento: cargar preferencias a través de un popup modal,
--                calcular índice de coincidencia de cada película y
--                mostrarlo junto a la misma;
-- **************

calcularPorcentajeDeCoincidencia : Preferences -> List Movie -> List Movie
calcularPorcentajeDeCoincidencia preferencias = List.map (asignarPorcentaje  preferencias)

asignarPorcentaje  : Preferences -> Movie -> Movie
asignarPorcentaje  preferencias pelicula = {pelicula | matchPercentage = porcentajePelicula preferencias pelicula}

porcentajePelicula : Preferences -> Movie -> Int
porcentajePelicula preferencias pelicula = if (calcularPorcentaje preferencias pelicula) > 100 then 100 else (calcularPorcentaje preferencias pelicula)

calcularPorcentaje : Preferences -> Movie -> Int
calcularPorcentaje preferencias pelicula = (porcentajePorPalabra preferencias pelicula) + (calcularPorcentajeActor preferencias pelicula) + (calcularPorcentajeGenero preferencias pelicula)

calcularPorcentajeActor : Preferences -> Movie -> Int
calcularPorcentajeActor preferencias pelicula = if (List.member preferencias.favoriteActor pelicula.actors) then 50 else 0

calcularPorcentajeGenero : Preferences -> Movie -> Int
calcularPorcentajeGenero preferencias pelicula = if (List.member preferencias.genre pelicula.genre) then 60 else 0

porcentajePorPalabra : Preferences -> Movie -> Int
porcentajePorPalabra preferencias pelicula = 20 * (List.length (List.filter (condicion (dePalabraAListaDePalabras pelicula.title))(dePalabraAListaDePalabras preferencias.keywords))) 

condicion : List String -> String -> Bool 
condicion tituloPelicula elemento = List.member elemento tituloPelicula
