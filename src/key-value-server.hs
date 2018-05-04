
import App
import Network.Wai.Handler.Warp
import System.IO

main :: IO ()
main = do
  let port = 4000
      settings =
        setPort port $
        setBeforeMainLoop
          (hPutStrLn stderr ("listening on " ++ show port ++ "...")) $
        defaultSettings
  app <- mkApp
  runSettings settings app
