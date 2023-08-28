module Lib
    ( getGroupConfiguration
    ) where

import Text.Pretty.Simple
import System.Posix.Types
import System.Posix.User

newtype User = User String
  deriving (Show, Ord, Eq)

getGroupConfiguration :: IO ()
getGroupConfiguration = do
  findGroups <- userGIDList <$> getAllGroupEntries
  pPrint . findGroups $ User "visinski"

  where
    userGIDList :: [GroupEntry] -> User -> [GroupID]
    -- Given a set of groups and a username, return the gids of those groups
    -- containing the user.
    userGIDList groups (User user) = map groupID $ filter (elem user . groupMembers) groups

