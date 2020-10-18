-- la linea de abajo es para usar el typeclass Num
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MonadExample(Personaje(..)) where

data Personaje = Personaje {
  dinero :: Int,
  salud :: Int,
  nombre :: String
}

instance Eq Personaje where
  (==) unPersonaje otroPersonaje =
    dinero unPersonaje == dinero otroPersonaje &&
    salud unPersonaje == salud otroPersonaje &&
    nombre unPersonaje == nombre otroPersonaje