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
calcularPorcentajeDeCoincidencia preferencias = completaAca


--calcularPorcentajeDeCoincidencia : Preferences -> List Movie -> List Movie
--calcularPorcentajeDeCoincidencia preferencias peliculas= preferenciasPorActor + preferenciasPorGenero + preferenciasPorPalabraClave


--preferenciasPorGenero : Preferences -> List Movie -> Int
--preferenciasPorGenero preferencias peliculas =  preferencias.genre peliculas

--preferenciasPorActriz : Preferences -> List Movie -> Int
--preferenciasPorActor preferencias peliculas = preferencias.favoriteActor

--preferenciasPorPalabraClave : Preferences -> List Movie -> Int
--preferenciasPorPalabraClave preferencias peliculas = preferencias.keywords
