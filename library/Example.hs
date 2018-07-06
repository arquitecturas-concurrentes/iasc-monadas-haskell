{-# LANGUAGE OverloadedStrings #-}
module Example where
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import Data.Text (Text, unpack)
import Text.Read

getField :: (JSON.FromJSON a) => JSON.Object -> Text -> JSON.Parser a
getField = (JSON..:)

data Personaje = Personaje { nombre :: String, inventario :: [Item], rol :: Rol } deriving Show

data Rol = Warrior | Mage | Priest | DeathKnight deriving (Show, Eq, Read)

data Item = Espada | Escudo | GrogXD deriving (Show, Eq, Read)

data Validado a = Exito a | Error String deriving Show

count condicion = length . filter condicion

validarInventario :: [Item] -> Validado [Item]
validarInventario unInventario | count (== GrogXD) unInventario > 1 = Error "Nadie puede llevar mas de un GrogXD"
                             | otherwise = Exito unInventario

validarNombre :: String -> Validado String
validarNombre unNombre | length unNombre > 20 = Error "Nombre muy largo"
                       | otherwise = Exito unNombre

validarPersonaje :: Personaje -> Validado Personaje
validarPersonaje unPersonaje | fuerza unPersonaje <= 100 = Exito unPersonaje
                             | otherwise = Error "El personaje esta roto"

fuerza :: Personaje -> Int
fuerza unPersonaje = length (inventario unPersonaje) * indicePorRol (rol unPersonaje)
    where indicePorRol unRol = case unRol of
            Warrior -> 30
            Mage -> 30
            Priest -> 10
            DeathKnight -> 50

inicialesDePersonaje :: Personaje -> [String]
inicialesDePersonaje unPersonaje = map (take 1) (words (nombre unPersonaje))

lichKing :: Validado Personaje
lichKing = construirPersonajeValidado (validarNombre "Arthas Menethil") (validarInventario [Espada, Escudo]) (pure DeathKnight) validarPersonaje

inicialesDePersonajeValidado :: Validado Personaje -> Validado [String]
inicialesDePersonajeValidado unPersonaje = inicialesDePersonaje <$> unPersonaje

instance Functor Validado where
    fmap funcion valor = case valor of
        Exito valorValidado -> Exito (funcion valorValidado)
        Error mensajeDeError -> Error mensajeDeError

instance Applicative Validado where
    Exito funcion <*> Exito valor = Exito (funcion valor)
    Error mensajeDeError <*> _ = Error mensajeDeError
    _ <*> Error mensajeDeError = Error mensajeDeError
    pure valor = Exito valor

instance Monad Validado where
    Exito valor >>= funcion = funcion valor
    Error mensajeDeError >>= _ = Error mensajeDeError

construirPersonajeValidado :: Validado String -> Validado [Item] -> Validado Rol -> (Personaje -> Validado Personaje) -> Validado Personaje
construirPersonajeValidado nombreValidado inventarioValidado rolValidado validacionSobrePersonaje =
        (Personaje <$> nombreValidado <*> inventarioValidado <*> rolValidado) >>= validacionSobrePersonaje

posiblesPersonajes nombres inventarios roles transformaciones =
    (Personaje <$> nombres <*> inventarios <*> roles) >>= transformaciones

construirPersonaje nombre inventario rol transformacion = do
    unNombre <- nombre
    unInventario <- inventario
    unRol <- rol
    transformacion (Personaje unNombre unInventario unRol)

instance JSON.FromJSON Personaje where
    parseJSON = JSON.withObject "Personaje" $ \jsonObject ->
        construirPersonaje
            (getField jsonObject "nombre")
            (getField jsonObject "inventario")
            (getField jsonObject "rol")
            pure

instance JSON.FromJSON Item where
    parseJSON = parseReadableFromText

instance JSON.FromJSON Rol where
    parseJSON = parseReadableFromText

parseReadableFromText :: (Read a) => JSON.Value -> JSON.Parser a
parseReadableFromText = JSON.withText "Rol" $ maybe (fail "Nope") pure . readMaybe . unpack
