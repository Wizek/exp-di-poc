{-# language NoMonomorphismRestriction #-}

import Data.Time
import Data.IORef

class MakeTimer x where
  getCurrentTime' :: x UTCTime
  putStrLn' :: String -> x ()
  liftIO' :: IO a -> x a
  newIORef' ::  a -> x (IORef a)
  readIORef' :: IORef a -> x a
  writeIORef' :: IORef a -> a -> x ()

instance MakeTimer IO where
  getCurrentTime' = getCurrentTime
  putStrLn' = putStrLn
  liftIO' = liftIO
  newIORef' = newIORef
  readIORef' = readIORef
  writeIORef' = writeIORef

-- makeTimer :: Foo
-- makeTimer :: MakeTimer x => x ()
makeTimer = liftIO' $ do
  prevTime <- newIORef' Nothing
  return $ liftIO' $ do
    pTime <- readIORef' prevTime
    time <- getCurrentTime'
    writeIORef' prevTime $ Just time
    case pTime of
      Nothing -> putStrLn' $ show time
      Just a  -> putStrLn' $ show time ++ ", diff: " ++ (show $ diffUTCTime time a)

main = do
  timer <- makeTimer
  timer
  timer

liftIO = id

shouldBe = shouldBeF show
shouldBeF f actual expected | actual == expected = putStrLn $ "OK " ++ f actual
                            | otherwise          = error $ "FAIL " ++ f actual ++ " /= " ++ f expected
