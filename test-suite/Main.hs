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
            let personaje = Personaje 0 100 [] nombrePersonaje
            nombre personaje `shouldBe` nombrePersonaje

        it "Dos personajes son iguales si tienen mismo nombre, dinero y salud" $ do
            let nombrePersonaje = "Arthas Menethil"
            let personaje = Personaje 0 100 [] nombrePersonaje
            let otroPersonaje = Personaje 0 100 [] nombrePersonaje 
            personaje `shouldBe` otroPersonaje

        it "Dos personajes son distintos si tienen distintos atributos" $ do
            let nombrePersonaje = "Arthas Menethil"
            let personaje = Personaje 0 100 [] nombrePersonaje
            let otroPersonaje = Personaje 200 100 [] nombrePersonaje 
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
            (construirPersonajeValidado nombrePersonaje 100 (Exito [])) == Error "El nombre es muy corto"

        it "Deberia fallar cuando el nombre es demasiado largo..." $ do
            let nombrePersonaje = validarNombre "esteNoEsUnNOmbreValidoAlSerDemasiadoLargo"
            (construirPersonajeValidado nombrePersonaje 100 (Exito [])) == Error "El nombre es muy largo"      

        it "Deberia fallar cuando el inventario es invalido" $ do
            let nombrePersonaje = validarNombre "JuanWizard"
            let items = validarInventario [ScrollRojo, Espada, ScrollRojo]
            (construirPersonajeValidado nombrePersonaje 250 items) == Error "Nadie puede llevar mas de un scroll rojo"

        it "Deberia crearme el personaje cuando el nombre tiene la longitud correcta" $ do
            let nombrePersonaje = validarNombre "NicoKnight"
            let items = validarInventario [Escudo]
            (construirPersonajeValidado nombrePersonaje 250 items) == Exito (Personaje 250 100 [Escudo] "NicoKnight")

    describe "F(x)s adicionales" $ do
        it "deberia devolverme el dinero seteado al personaje" $ do
            let nombrePersonaje = "Arthas Menethil"
            let personaje = Personaje 0 100 [] nombrePersonaje
            obtenerDineroPersonaje personaje `shouldBe` 0

        it "deberia devolverme las iniciales correctas del personaje" $ do
            let nombrePersonaje = "Arthas Menethil"
            let personaje = Personaje 0 100 [] nombrePersonaje
            (inicialesDePersonaje personaje) `shouldBe` ["A","M"]

    describe "inicialesDePersonajeValidado" $ do
        it "Deberia obtener las iniciales de un personaje validado" $ do
            let personajeValidado = construirPersonajeValidado (validarNombre "Arthas Menethil") 1000 (Exito [])
            inicialesDePersonajeValidado personajeValidado == Exito (["A","M"])

        it "Deberia fallar al ser el nombre del personaje muy corto" $ do
            let personajeValidado = construirPersonajeValidado (validarNombre "inv") 1000 (Exito [])
            inicialesDePersonajeValidado personajeValidado == Error "El nombre es muy corto"

    describe "dineroPersonajeValidado" $ do
        it "Deberia obtener el saldo de un personaje validado" $ do
            let personajeValidado = construirPersonajeValidado (validarNombre "Arthas Menethil") 1000 (Exito [])
            dineroPersonajeValidado personajeValidado == Exito (1000)

        it "Deberia fallar al ser el nombre del personaje muy corto" $ do
            let personajeValidado = construirPersonajeValidado (validarNombre "inv") 1000 (Exito [])
            dineroPersonajeValidado personajeValidado == Error "El nombre es muy corto"

    describe "validarInventario" $ do
        it "cuando hay mas de un scroll rojo en la lista de items" $ do
            let items = [ScrollRojo, Espada, ScrollRojo]
            validarInventario items == Error "Nadie puede llevar mas de un scroll rojo"

        it "cuando se puede validar sin problemas la lista de items" $ do
            let items = [ScrollRojo, Espada, Escudo]
            validarInventario items == Exito items