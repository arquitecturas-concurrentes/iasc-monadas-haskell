-- la linea de abajo es para usar el typeclass Num
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MonadExample(validarNombre, Personaje(..), Validado(..)) where

-- Agregamos un validado para validar el exito o el error de una validacion
data Validado a = Exito a | Error String deriving (Show, Eq)

-- Con newtype también podemos definir un nuevo tipo
newtype Oro = Oro Int
  deriving (Show, Eq, Num)

data Personaje = Personaje {
  dinero :: Oro,
  salud :: Int,
  nombre :: String
} deriving (Show, Eq)

-- Ahora tenemos una funcion que nos va a validar que el nombre no sea demasiado corto 
validarNombre :: String -> Validado String
validarNombre unNombre | length unNombre < 4 = Error "El nombre es muy corto"
                       | length unNombre > 20 = Error "El nombre es muy largo"
                       | otherwise = Exito unNombre


-- La idea seria de poder crear un personaje validado...                      
construirPersonajeValidado :: Validado String -> Oro -> Validado Personaje
construirPersonajeValidado nombreValidado plata = undefined