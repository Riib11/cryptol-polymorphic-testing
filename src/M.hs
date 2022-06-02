module M where

import Control.Monad.Except

type ErrorMessage = String

-- type M a = ExceptT ErrorMessage IO a

-- throwM :: ErrorMessage -> M a
-- throwM = throwError

-- caseM :: M a -> M (Either ErrorMessage a)
-- caseM = undefined

type M_Repr a = IO (Either ErrorMessage a)

newtype M a = M { unM :: M_Repr a }

overM :: (M_Repr a -> M_Repr b) -> M a -> M b
overM f m = M (f (unM m))

instance Functor M where 
  fmap f = overM (fmap . fmap $ f) 

instance Applicative M where 
  pure = M . pure . pure
  M io_e_f <*> M io_e_a = M $ do
    f <- io_e_f
    a <- io_e_a
    pure (f <*> a)

instance Monad M where 
  M me >>= k = M $ do
    e <- me
    case e of 
      Left msg -> pure (Left msg)
      Right a -> unM $ k a

throwM :: ErrorMessage -> M a
throwM = M . pure . Left

caseM :: M a -> M (Either ErrorMessage a)
caseM = fmap Right
-- caseM ma = do
--   e <- ma
--   case e of 
--     Left msg -> pure (Right (Left msg))
--     Right a -> pure (Right (Right a))

logM :: String -> String -> M ()
logM lbl msg = M (Right <$> putStrLn (lbl ++ ": " ++ msg))
