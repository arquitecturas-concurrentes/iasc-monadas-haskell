-- la linea de abajo es para usar el typeclass Num
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MonadExample(Personaje(..)) where

data Personaje = Personaje {
  dinero :: Int,
  salud :: Int,
  nombre :: String
}