-- la linea de abajo es para usar el typeclass Num
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MonadExample where

-- Agregamos un validado para validar el exito o el error de una validacion
data Validado a = Exito a | Error String deriving (Show, Eq)


-- Con newtype definimos un nuevo tipo, el deriving lo dejamos mas adelante
newtype Oro = Oro Int
  deriving (Show, Eq, Num, Ord)

-- Ahora existen los items
data Item = Espada | Escudo | ScrollRojo deriving (Show, Eq)

-- ### Validaciones sobre items
count condicion = length . filter condicion

validarInventario :: [Item] -> Validado [Item]
validarInventario unInventario | count (== ScrollRojo) unInventario > 1 = Error "Nadie puede llevar mas de un scroll rojo"
                             | otherwise = Exito unInventario

-- Despues esta el personake, que debe tener un nombre para empezar
data Personaje = Personaje {
  salud :: Int,
  dinero :: Oro,
  inventario :: [Item],
  nombre :: String
} deriving (Show, Eq)

-- ### Funciones adicionales sobre Personaje
inicialesDePersonaje :: Personaje -> [String]
inicialesDePersonaje unPersonaje = map (take 1) (words (nombre unPersonaje))

obtenerDineroPersonaje :: Personaje -> Oro
obtenerDineroPersonaje unPersonaje = (dinero unPersonaje)

validarDinero :: Oro -> Validado Oro
validarDinero dinero | dinero >= 0 = Exito dinero
                     | otherwise = Error "Tiene que tener algo de oro o 0"

-- Ahora tenemos una funcion que nos va a validar que el nombre no sea demasiado corto 
validarNombre :: String -> Validado String
validarNombre unNombre | length unNombre < 4 = Error "El nombre es muy corto"
                       | length unNombre > 20 = Error "El nombre es muy largo"
                       | otherwise = Exito unNombre


-- La idea seria de poder crear un personaje validado...                      
construirPersonajeValidado :: Validado String -> Validado Oro -> Validado [Item] -> Validado Personaje
-- construirPersonajeValidado nombreValidado plataValidada inventarioValidado = case ((Personaje 100) <$> plataValidada, inventarioValidado, nombreValidado) of
--    (Exito constructorDePersonaje, Exito items, Exito unNombre) -> Exito (constructorDePersonaje items unNombre)
--    (Error mensajeDeError, _ , _) -> Error mensajeDeError
--    (_, Error mensajeDeError, _) -> Error mensajeDeError
--    (_, _, Error mensajeDeError) -> Error mensajeDeError
construirPersonajeValidado nombreValidado plataValidada inventarioValidado 
  = ((Personaje 100) <$> plataValidada) <*> inventarioValidado <*> nombreValidado 


-- #### Fxs sobre Validado Personaje

inicialesDePersonajeValidado :: Validado Personaje -> Validado [String]
--inicialesDePersonajeValidado unPersonaje = case unPersonaje of
--    Exito personajeValidado -> Exito (inicialesDePersonaje personajeValidado)
--    Error mensajeDeError -> Error mensajeDeError
-- inicialesDePersonajeValidado unPersonaje = fmap inicialesDePersonaje unPersonaje
inicialesDePersonajeValidado unPersonaje = inicialesDePersonaje <$> unPersonaje

-- Ahora queremos obtener el dinero de un personaje validado
dineroPersonajeValidado :: Validado Personaje -> Validado Oro
--dineroPersonajeValidado unPersonaje = case unPersonaje of
--    Exito personajeValidado -> Exito (obtenerDineroPersonaje personajeValidado)
--    Error mensajeDeError -> Error mensajeDeError
--dineroPersonajeValidado unPersonaje = fmap obtenerDineroPersonaje unPersonaje
dineroPersonajeValidado unPersonaje = obtenerDineroPersonaje <$> unPersonaje

instance Functor Validado where
    fmap funcion valor = case valor of
        Exito valorValidado -> Exito (funcion valorValidado)
        Error mensajeDeError -> Error mensajeDeError 

instance Applicative Validado where
    Exito funcion <*> Exito valor = Exito (funcion valor)
    Error mensajeDeError <*> _ = Error mensajeDeError
    _ <*> Error mensajeDeError = Error mensajeDeError