{-# LANGUAGE DeriveGeneric, OverloadedLabels, OverloadedStrings #-}
module Database where
import Data.Text (Text)
import Database.Selda
import Database.Selda.PostgreSQL (PG, PGConnectInfo (PGConnectInfo, pgHost, pgPort, pgDatabase, pgUsername, pgPassword, pgSchema))
import Text.Printf (printf)
import Control.Concurrent (threadDelay)


data VendingMachine = VendingMachine
    { mid :: ID VendingMachine
    , machineName :: Text
    } deriving Generic
instance SqlRow VendingMachine

data Item = Item
    { iid :: ID Item
    , itemName :: Text
    } deriving Generic
instance SqlRow Item

data VendingMachineItem = VendingMachineItem
    { miid :: ID VendingMachineItem
    , item :: ID Item
    , machine :: ID VendingMachine
    , amount :: Int
    } deriving (Generic, Show)
instance SqlRow VendingMachineItem

vendingMachines :: Table VendingMachine
vendingMachines = table "vending_machines" [#mid :- autoPrimary]

items :: Table Item
items = table "items" [#iid :- autoPrimary]

vendingMachineItems :: Table VendingMachineItem
vendingMachineItems = table "vending_machine_items" [ #miid :- autoPrimary
                                                    , #item :- foreignKey items #iid
                                                    , #machine :- foreignKey vendingMachines #mid]


connectionConfiguration :: PGConnectInfo
connectionConfiguration = PGConnectInfo
    { pgHost = "localhost"
    , pgPort = 5432
    , pgDatabase = "test"
    , pgUsername = Just "test"
    , pgPassword = Just "test"
    , pgSchema = Nothing
    }

createAllTables :: MonadSelda m => m ()
createAllTables = do
    _ <- createTable items
    _ <- createTable vendingMachines
    _ <- createTable vendingMachineItems
    return ()


getAllMachines :: MonadSelda m => m [VendingMachine]
getAllMachines = query $ do
    select vendingMachines

findMachineByName :: MonadSelda m => Text-> m [VendingMachine]
findMachineByName name = query $ do
    machines <- select vendingMachines
    restrict (machines ! #machineName .== literal name)
    return machines

getMachineById :: MonadSelda m => ID VendingMachine -> m [VendingMachine]
getMachineById id = query . limit 0 1 $ do
    machines <- select vendingMachines
    restrict (machines ! #mid .== literal id)
    return machines

insertMachine ::  Text -> SeldaM PG (ID VendingMachine)
insertMachine machineName = insertWithPK vendingMachines [VendingMachine def machineName]


getItems :: SeldaM PG [Item]
getItems = query $ do
    select items

findItemsByNames :: MonadSelda m => [Text] -> m [Item]
findItemsByNames names = query $ do
    its <- select items
    restrict(its ! #itemName `isIn` map literal names)
    return its

insertItem :: MonadSelda m => Text -> m (ID Item)
insertItem name = insertWithPK items [Item def name]

findItemsByIds :: MonadSelda m => [ID Item] -> m [Item]
findItemsByIds ids = query $ do
    its <- select items
    restrict(its ! #iid `isIn` map literal ids)
    return its


findMachineItemsByMachine :: MonadSelda m => ID VendingMachine -> m [VendingMachineItem]
findMachineItemsByMachine machineId = query $ do
    its <- select vendingMachineItems
    restrict(its ! #machine .== literal machineId)
    return its

findMachineItemsByMachineName :: MonadSelda m => Text -> m [VendingMachineItem]
findMachineItemsByMachineName machineName = query $ do
    its <- select vendingMachineItems
    machines <- select vendingMachines
    restrict(machines ! #machineName .== literal machineName)
    restrict(its ! #machine .== machines ! #mid)
    return its


deleteMachineItems :: MonadSelda m => [ID VendingMachineItem] -> m Int
deleteMachineItems machineIds = do
    deleteFrom vendingMachineItems (\x -> x ! #miid `isIn` map literal machineIds)

insertMachineItem :: (MonadSelda m, MonadMask m) => VendingMachineItem -> m (Maybe (ID VendingMachineItem))
insertMachineItem machineItem = do
    upsert vendingMachineItems
        (\row -> (row ! #machine .== literal (machine machineItem)) .&& (row ! #item .== literal (item machineItem)))
        (\row -> row `with` [#item := literal (item machineItem), #machine := literal (Database.machine machineItem), #amount := literal (amount machineItem)])
        [machineItem]

findMachinesByItemName :: (MonadSelda m) => Text -> m [VendingMachine]
findMachinesByItemName itemName = query $ do
    its <- select items
    restrict((its ! #itemName) `like` literal itemName)
    vits <- select vendingMachineItems
    machines <- select vendingMachines
    restrict(vits ! #item .== its ! #iid)
    restrict(machines ! #mid .== vits ! #machine)
    return machines
