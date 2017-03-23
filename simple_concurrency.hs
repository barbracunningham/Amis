import Data.List
import System.IO
import Data.Map (Map, (!)) 
import Data.Monoid
import qualified Data.Map as M
import Control.Monad
import Control.Monad.State

import Data.Hashable
import Data.Typeable
import Haxl.Core
import qualified Haxl.Prelude as HP


type Haxl a = GenHaxl () a --Create instance of the Haxl monad

--|| Create data source
data DeepThought a where
  Answer:: DeepThought Int
  deriving Typeable
deriving instance Show (DeepThought a)
deriving instance Eq (DeepThought a)

instance Show1 DeepThought where
    show1 Answer = "Answer"
instance Hashable (DeepThought a) where
    hashWithSalt salt Answer = hashWithSalt salt ()
  
  --|| Create function to take data source and do stuff
  runDeepThought :: DeepThought a -> ResultVar a -> IO ()
  runDeepThought (Answer) var = putSuccess var 42 
  
  --|| Tie data source to 'DataSourceName'
  instance DataSourceName DeepThought where
    dataSourceName _ = "Deep Thought"
 
 --|| Tie data source to 'DataSource'
 instance DataSource () DeepThought
    fetch _ _ _ reqs = SyncFetch $ HP.forM_ reqs $ \[BlockedFetch req var] -> runDeepThought req var
 
 --|| Tie data source to a 'StateKey'
 instance State DeepThought where
    data State DeepThought = NoState
 
 --|| Create an initial state 
 initialState :: StateStore
 initialState = stateSet NoState stateEmpty
 
 main = do
  myEnv <- initEnv initialState ()
  let x = runHaxl myEnv (dataFetch Answer)
  
 
