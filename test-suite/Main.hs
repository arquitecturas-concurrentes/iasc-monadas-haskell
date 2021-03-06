-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite. See its website for
-- help: <http://documentup.com/feuerbach/tasty>.
import qualified Test.Tasty
-- Hspec is one of the providers for Tasty. It provides a nice syntax for
-- writing tests. Its website has more info: <https://hspec.github.io>.
import Test.Tasty.Hspec
import MonadExample

main :: IO ()
main = do
    test <- testSpec "iasc-monads" spec
    Test.Tasty.defaultMain test

spec :: Spec
spec = parallel $ do
    describe "Personaje data type" $ do
        it "Deberia poder crear un Personaje" $ do
            let nombrePersonaje = "Arthas Menethil"
            let personaje = Personaje 0 100 [] nombrePersonaje Mage
            nombre personaje `shouldBe` nombrePersonaje

        it "Dos personajes son iguales si tienen mismo nombre, dinero y salud" $ do
            let nombrePersonaje = "Arthas Menethil"
            let personaje = Personaje 0 100 [] nombrePersonaje Mage
            let otroPersonaje = Personaje 0 100 [] nombrePersonaje Mage
            personaje `shouldBe` otroPersonaje

        it "Dos personajes son distintos si tienen distintos atributos" $ do
            let nombrePersonaje = "Arthas Menethil"
            let personaje = Personaje 0 100 [] nombrePersonaje Mage
            let otroPersonaje = Personaje 200 100 [] nombrePersonaje Mage
            personaje `shouldNotBe` otroPersonaje

    describe "validarNombre" $ do
        it "Deberia fallar cuando el nombre es muy corto..." $ do
            let nombrePersonaje = "inv"
            validarNombre nombrePersonaje == Error "El nombre es muy corto"

        it "Deberia fallar cuando el nombre es demasiado largo..." $ do
            let nombrePersonaje = "esteNoEsUnNOmbreValidoAlSerDemasiadoLargo"
            validarNombre nombrePersonaje == Error "El nombre es muy largo" 

        it "Deberia ser exitosa la validacion" $ do
            let nombrePersonaje = "Arthas Menethil"
            validarNombre nombrePersonaje == Exito nombrePersonaje
    
    describe "construirPersonajeValidado" $ do
        it "Deberia fallar cuando el nombre es muy corto..." $ do
            let nombrePersonaje = validarNombre "inv"
            let plata = validarDinero 100
            (construirPersonajeValidado nombrePersonaje plata (Exito []) (pure Knight) validarPersonaje) == Error "El nombre es muy corto"

        it "Deberia fallar cuando el nombre es demasiado largo..." $ do
            let nombrePersonaje = validarNombre "esteNoEsUnNOmbreValidoAlSerDemasiadoLargo"
            let plata = validarDinero 100
            (construirPersonajeValidado nombrePersonaje plata (Exito []) (pure Knight) validarPersonaje) == Error "El nombre es muy largo"      

        it "Deberia fallar cuando el inventario es invalido" $ do
            let nombrePersonaje = validarNombre "JuanWizard"
            let plata = validarDinero 250
            let items = validarInventario [ScrollRojo, Espada, ScrollRojo]
            (construirPersonajeValidado nombrePersonaje plata items (pure Mage) validarPersonaje) == Error "Nadie puede llevar mas de un scroll rojo"

        it "Deberia crearme el personaje cuando el nombre tiene la longitud correcta" $ do
            let nombrePersonaje = validarNombre "NicoKnight"
            let items = validarInventario [Escudo]
            let plata = validarDinero 250
            (construirPersonajeValidado nombrePersonaje plata items (pure Knight) validarPersonaje) == Exito (Personaje 100 250 [Escudo] "NicoKnight" Knight)

        it "Deberia fallar cuando el personaje esta roto" $ do
            let nombrePersonaje = validarNombre "NicoKnight"
            let items = validarInventario [ScrollRojo, Escudo, Espada]
            let plata = validarDinero 250
            (construirPersonajeValidado nombrePersonaje plata items (pure Knight) validarPersonaje) == Error "El personaje esta roto"

    describe "F(x)s adicionales" $ do
        it "deberia devolverme el plata seteado al personaje" $ do
            let nombrePersonaje = "Arthas Menethil"
            let personaje = Personaje 100 0 [] nombrePersonaje DeathKnight
            obtenerDineroPersonaje personaje `shouldBe` 0

        it "deberia devolverme las iniciales correctas del personaje" $ do
            let nombrePersonaje = "Arthas Menethil"
            let personaje = Personaje 100 0 [] nombrePersonaje DeathKnight
            (inicialesDePersonaje personaje) `shouldBe` ["A","M"]

    describe "inicialesDePersonajeValidado" $ do
        it "Deberia obtener las iniciales de un personaje validado" $ do
            let personajeValidado = construirPersonajeValidado (validarNombre "Arthas Menethil") (Exito 1000) (Exito []) (pure DeathKnight) validarPersonaje
            inicialesDePersonajeValidado personajeValidado == Exito (["A","M"])

        it "Deberia fallar al ser el nombre del personaje muy corto" $ do
            let personajeValidado = construirPersonajeValidado (validarNombre "inv") (Exito 1000) (Exito []) (pure DeathKnight) validarPersonaje
            inicialesDePersonajeValidado personajeValidado == Error "El nombre es muy corto"

    describe "dineroPersonajeValidado" $ do
        it "Deberia obtener el saldo de un personaje validado" $ do
            let personajeValidado = construirPersonajeValidado (validarNombre "Arthas Menethil") (Exito 1000) (Exito []) (pure DeathKnight) validarPersonaje
            dineroPersonajeValidado personajeValidado == Exito (1000)

        it "Deberia fallar al ser el nombre del personaje muy corto" $ do
            let personajeValidado = construirPersonajeValidado (validarNombre "inv") (Exito 1000) (Exito []) (pure DeathKnight) validarPersonaje
            dineroPersonajeValidado personajeValidado == Error "El nombre es muy corto"

    describe "validarInventario" $ do
        it "cuando hay mas de un scroll rojo en la lista de items" $ do
            let items = [ScrollRojo, Espada, ScrollRojo]
            validarInventario items == Error "Nadie puede llevar mas de un scroll rojo"

        it "cuando se puede validar sin problemas la lista de items" $ do
            let items = [ScrollRojo, Espada, Escudo]
            validarInventario items == Exito items

    describe "validarDinero" $ do
        it "deberia fallar al ser un numero negativo" $ do
            validarDinero (-1) `shouldBe` Error "Tiene que tener algo de oro o 0"

        it "deberia ser un exito al ser un numero positivo" $ do
            validarDinero (100) `shouldBe` Exito (100)

    describe "fuerza" $ do
        it "Deberia dar 0" $ do
            let personaje = Personaje 100 0 [] "caballero3842" DeathKnight
            fuerza personaje `shouldBe` 0

        it "Deberia dar 50 para el DeathKnight con un item" $ do
            let personaje = Personaje 100 0 [ScrollRojo] "caballero3842" DeathKnight
            fuerza personaje `shouldBe` 50

        it "Deberia dar 150 para el DeathKnight con tres items" $ do
            let personaje = Personaje 100 10 [ScrollRojo, Escudo, Espada] "caballero3842" DeathKnight
            fuerza personaje `shouldBe` 150

        it "Deberia dar 30 para el Mage con un item" $ do
            let personaje = Personaje 100 0 [ScrollRojo] "mage132" Mage
            fuerza personaje `shouldBe` 30
    
    describe "validarPersonaje" $ do
        it "deberia fallar al ser la fuerza mayor a 100" $ do
            let personaje = Personaje 100 10 [ScrollRojo, Escudo, Espada] "caballero3842" DeathKnight
            validarPersonaje personaje `shouldBe` Error "El personaje esta roto"

        it "deberia validarse al ser la fuerza menor a 100" $ do
            let personaje = Personaje 100 0 [ScrollRojo] "mage132" Mage
            validarPersonaje personaje `shouldBe` Exito personaje
    
    describe "construirPersonaje" $ do
        it "Deberia fallar cuando el nombre es demasiado largo..." $ do
            let nombrePersonaje = validarNombre "esteNoEsUnNOmbreValidoAlSerDemasiadoLargo"
            let plata = validarDinero 100
            (construirPersonaje nombrePersonaje plata (Exito []) (pure Knight) validarPersonaje) == Error "El nombre es muy largo"      

        it "Deberia crearme el personaje cuando el nombre tiene la longitud correcta" $ do
            let nombrePersonaje = validarNombre "NicoKnight"
            let items = validarInventario [Escudo]
            let plata = validarDinero 250
            (construirPersonaje nombrePersonaje plata items (pure Knight) validarPersonaje) == Exito (Personaje 100 250 [Escudo] "NicoKnight" Knight)

        it "Deberia fallar cuando el personaje esta roto" $ do
            let nombrePersonaje = validarNombre "NicoKnight"
            let items = validarInventario [ScrollRojo, Escudo, Espada]
            let plata = validarDinero 250
            (construirPersonaje nombrePersonaje plata items (pure Knight) validarPersonaje) == Error "El personaje esta roto"
