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
        
        it "Dos personajes son iguales si tienen mismo nombre, dinero y salud" $ do
            let nombrePersonaje = "Arthas Menethil"
            let personaje = Personaje 0 100 nombrePersonaje
            let otroPersonaje = Personaje 0 100 nombrePersonaje 
            personaje `shouldBe` otroPersonaje

        it "Dos personajes son distintos si tienen distintos atributos" $ do
            let nombrePersonaje = "Arthas Menethil"
            let personaje = Personaje 0 100 nombrePersonaje
            let otroPersonaje = Personaje 200 100 nombrePersonaje 
            personaje `shouldNotBe` otroPersonaje