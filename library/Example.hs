module Example where

data Personaje = Personaje { nombre :: String } deriving Show

data Validado a = Exito a | Error String deriving Show

validarNombre :: String -> Validado String
validarNombre unNombre | length unNombre > 20 = Error "Nombre muy largo"
                       | otherwise = Exito unNombre

construirPersonajeValidado :: Validado String -> Validado Personaje
construirPersonajeValidado nombreValidado = Personaje <$> nombreValidado

inicialesDePersonaje :: Personaje -> [String]
inicialesDePersonaje unPersonaje = map (take 1) (words (nombre unPersonaje))

lichKing :: Validado Personaje
lichKing = construirPersonajeValidado (validarNombre "Arthas Menethil")

inicialesDePersonajeValidado :: Validado Personaje -> Validado [String]
inicialesDePersonajeValidado unPersonaje = inicialesDePersonaje <$> unPersonaje

instance Functor Validado where
    fmap funcion valor = case valor of
        Exito valorValidado -> Exito (funcion valorValidado)
        Error mensajeDeError -> Error mensajeDeError