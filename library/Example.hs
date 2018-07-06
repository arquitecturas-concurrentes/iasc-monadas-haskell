module Example where

data Personaje = Personaje { nombre :: String, inventario :: [Item], rol :: Rol } deriving Show

data Rol = Warrior | Mage | Priest | DeathKnight deriving (Show, Eq)

data Item = Espada | Escudo | GrogXD deriving (Show, Eq)

data Validado a = Exito a | Error String deriving Show

count condicion = length . filter condicion

validarInventario :: [Item] -> Validado [Item]
validarInventario unInventario | count (== GrogXD) unInventario > 1 = Error "Nadie puede llevar mas de un GrogXD"
                             | otherwise = Exito unInventario

validarNombre :: String -> Validado String
validarNombre unNombre | length unNombre > 20 = Error "Nombre muy largo"
                       | otherwise = Exito unNombre

construirPersonajeValidado :: Validado String -> Validado [Item] -> Validado Rol -> Validado Personaje
construirPersonajeValidado nombreValidado inventarioValidado rolValidado = 
    Personaje <$> nombreValidado <*> inventarioValidado <*> rolValidado

inicialesDePersonaje :: Personaje -> [String]
inicialesDePersonaje unPersonaje = map (take 1) (words (nombre unPersonaje))

lichKing :: Validado Personaje
lichKing = construirPersonajeValidado (validarNombre "Arthas Menethil") (validarInventario [Espada, Escudo])

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