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
            let personaje = Personaje 0 100 nombrePersonaje 
            nombre personaje `shouldBe` nombrePersonaje

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
            (construirPersonajeValidado nombrePersonaje 100) == Error "El nombre es muy corto"

        it "Deberia fallar cuando el nombre es demasiado largo..." $ do
            let nombrePersonaje = validarNombre "esteNoEsUnNOmbreValidoAlSerDemasiadoLargo"
            (construirPersonajeValidado nombrePersonaje 100) == Error "El nombre es muy largo"      

        it "Deberia crearme el personaje cuando el nombre tiene la longitud correcta" $ do
            let nombrePersonaje = validarNombre "NicoKnight"
            (construirPersonajeValidado nombrePersonaje 250) == Exito (Personaje 250 100 "NicoKnight")   