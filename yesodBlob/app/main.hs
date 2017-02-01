import Prelude     (IO)
import Application (appMain)

main :: IO ()
--ADDED
main = do
    man <- newManager
    warp 3000 $ App man
--END ADDED
--main = appMain
