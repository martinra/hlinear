module HLinear.Test.Utility.DivideByZero
where


catchDivisionByZero :: IO (Maybe a) -> IO (Maybe a)
catchDivisionByZero a = a `catch` \e -> case e of
  DivideByZero         -> return Nothing
  RatioZeroDenominator -> return Nothing
  _                    -> throw e

wrapDivideByZero :: (a -> b)
                  -> Maybe a -> Maybe b
wrapDivideByZero f a = unsafePerformIO $
  catchDivisionByZero $
  case liftM f a of
    Nothing -> return Nothing
    Just c' -> Just <$> evaluate c'

wrapDivideByZero2 :: (a -> b -> c)
                  -> Maybe a -> Maybe b -> Maybe c
wrapDivideByZero2 f a b = unsafePerformIO $
  catchDivisionByZero $
  case liftM2 f a b of
    Nothing -> return Nothing
    Just c' -> Just <$> evaluate c'
