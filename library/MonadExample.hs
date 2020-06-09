-- la linea de abajo es para usar el typeclass Num
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MonadExample where

-- Agregamos un validado para validar el exito o el error de una validacion
data Validado a = Exito a | Error String deriving (Show, Eq)


-- Con newtype definimos un nuevo tipo, el deriving lo dejamos mas adelante
newtype Oro = Oro Int
  deriving (Show, Eq, Num)

-- Despues esta el personake, que debe tener un nombre para empezar
data Personaje = Personaje {
  dinero :: Oro,
  salud :: Int,
  nombre :: String
} deriving (Show, Eq)

-- ### Funciones adicionales sobre Personaje
inicialesDePersonaje :: Personaje -> [String]
inicialesDePersonaje unPersonaje = map (take 1) (words (nombre unPersonaje))

obtenerDineroPersonaje :: Personaje -> Oro
obtenerDineroPersonaje unPersonaje = (dinero unPersonaje)

-- Ahora tenemos una funcion que nos va a validar que el nombre no sea demasiado corto 
validarNombre :: String -> Validado String
validarNombre unNombre | length unNombre < 4 = Error "El nombre es muy corto"
                       | length unNombre > 20 = Error "El nombre es muy largo"
                       | otherwise = Exito unNombre


-- La idea seria de poder crear un personaje validado...                      
construirPersonajeValidado :: Validado String -> Oro -> Validado Personaje
--construirPersonajeValidado nombreValidado plata = case nombreValidado of
--    Exito unNombre -> Exito (Personaje plata 100 unNombre)
--    Error mensajeDeError -> Error mensajeDeError
construirPersonajeValidado nombreValidado plata = fmap (Personaje plata 100) nombreValidado

-- #### Fxs sobre Validado Personaje


inicialesDePersonajeValidado :: Validado Personaje -> Validado [String]
--inicialesDePersonajeValidado unPersonaje = case unPersonaje of
--    Exito personajeValidado -> Exito (inicialesDePersonaje personajeValidado)
--    Error mensajeDeError -> Error mensajeDeError
inicialesDePersonajeValidado unPersonaje = fmap inicialesDePersonaje unPersonaje

-- Ahora queremos obtener el dinero de un personaje validado
dineroPersonajeValidado :: Validado Personaje -> Validado Oro
--dineroPersonajeValidado unPersonaje = case unPersonaje of
--    Exito personajeValidado -> Exito (obtenerDineroPersonaje personajeValidado)
--    Error mensajeDeError -> Error mensajeDeError
dineroPersonajeValidado unPersonaje = fmap obtenerDineroPersonaje unPersonaje

-- podemos hacer esto de manera mas generica ahora...
instance Functor Validado where
    fmap funcion valor = case valor of
        Exito valorValidado -> Exito (funcion valorValidado)
        Error mensajeDeError -> Error mensajeDeError 