module Example where

data Personaje = Personaje { nombre :: String } deriving Show

data Validado a = Exito a | Error String deriving Show

validarNombre :: String -> Validado String
validarNombre unNombre | length unNombre > 20 = Error "Nombre muy largo"
                       | otherwise = Exito unNombre

construirPersonajeValidado :: Validado String -> Validado Personaje
construirPersonajeValidado nombreValidado = case nombreValidado of
    Exito unNombre -> Exito (Personaje unNombre)
    Error mensajeDeError -> Error mensajeDeError

inicialesDePersonaje :: Personaje -> [String]
inicialesDePersonaje unPersonaje = map (take 1) (words (nombre unPersonaje))

lichKing :: Validado Personaje
lichKing = construirPersonajeValidado (validarNombre "Arthas Menethil")
