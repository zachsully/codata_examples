{-# LANGUAGE Copatterns #-}
module FileSystem where

import Data.Map
import Prelude hiding (lookup)

{-
Indexes for two different user types. These are used to control access to
different observations on the filesystem codata type.
-}
data Root
data User

{-
An interface for a simplified filesystem with access control provided by type
equality constraints.
-}
codata FS m u where
  Write :: FS m Root -> FilePath -> String -> m (FS m Root)
  Read  :: FS m User -> FilePath -> m String
  Su    :: FS m User
        -> String
        -> Either (FS m Root) (FS m User)
  Exit  :: FS m Root -> FS m User

{-
A filesystem implementation for simulation. That is, it stores files in memory
instead of writing to disc.
-}
fsUserMaybe :: Map FilePath String
            -> String
            -> FS Maybe User
fsUserMaybe st pass =
  { [Read #] file -> lookup file st
  ; [Su #] pass' -> if pass' == pass
                    then Left  (fsRootMaybe st pass)
                    else Right (fsUserMaybe st pass)
  }

fsRootMaybe :: Map FilePath String
            -> String
            -> FS Maybe Root
fsRootMaybe st pass =
  { [Write #] file content ->
      return (fsRootMaybe (alter (\_ -> Just content) file st) pass)
  ; Exit # -> fsUserMaybe st pass
  }

{-
A filesystem implementation that mutates a disc.
-}
fsUserIO :: String -> FS IO User
fsUserIO pass =
  { Read # -> readFile
  ; [Su #] pass' -> if pass' == pass
                    then Left  (fsRootIO pass)
                    else Right (fsUserIO pass)
  }

fsRootIO :: String -> FS IO Root
fsRootIO pass =
  { [Write #] file content -> do writeFile file content
                                 return (fsRootIO pass)
  ; Exit # -> fsUserIO pass
  }


{-
An example interaction that making use of any type of filesystem
-}
data Error a = Val a | Error String deriving Show

interaction1 :: Monad m => FS m User -> m (Error String)
interaction1 fs =
  case obs_Su fs "42" of
    Right fs' -> return (Error "wrong pass")
    Left fs' ->
      do fs'' <- obs_Write fs' "foo" "bar"
         Val <$> obs_Read (obs_Exit fs'') "foo"
