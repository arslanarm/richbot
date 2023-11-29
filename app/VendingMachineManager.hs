module VendingMachineManager where
import VendingMachineApi (VendingMachineData (VendingMachineData, machine), VendingMachineInfo (VendingMachineInfo, planogram), VendingMachineItem (..))
import Data.Text ( Text )
import Database (findMachineByName, insertMachine, getMachineById, findItemsByNames, Item (itemName, iid), insertItem, findItemsByIds, findMachineItemsByMachine, VendingMachine (mid), VendingMachineItem (item, miid, amount, VendingMachineItem, machine), deleteMachineItems, insertMachineItem)
import Database.Selda (SeldaM, MonadSelda, def, MonadIO (liftIO))
import Database.Selda.PostgreSQL (PG)
import Data.Maybe (listToMaybe)
import Data.List (find, nub)
import GHC.TypeLits (ErrorMessage(Text))
import Text.Printf (printf)


save :: Text -> VendingMachineData -> SeldaM PG ()
save machineName machineData = do
    machines <- findMachineByName machineName
    m <- case listToMaybe machines of
          Nothing -> do
            machineId <- insertMachine machineName
            m <- getMachineById machineId
            return (head m)
          Just vm -> return vm

    let items = planogram (VendingMachineApi.machine machineData)
    let itemNames = nub $ map name items
    alreadyExistingItems <- findItemsByNames itemNames
    liftIO $ printf "Already Exists %d items\n" (length alreadyExistingItems)
    let alreadyExistingItemNames = map itemName alreadyExistingItems
    let itemsToCreate = filter (not . (`elem` alreadyExistingItemNames)) itemNames
    liftIO $ printf "Items to create %d items\n" (length itemsToCreate)
    createdItemIds <- mapM insertItem itemsToCreate
    liftIO $ printf "Created items: %d\n" (length createdItemIds)
    createdItems <- findItemsByIds createdItemIds

    let allItems = alreadyExistingItems ++ createdItems
    liftIO $ printf "All items count: %d\n" (length allItems)
    let itemIds = map iid allItems

    currentItems <- findMachineItemsByMachine (mid m)
    liftIO $ printf "Current items count: %d\n" (length currentItems)

    let itemsToDelete = filter (\x -> item x `elem` itemIds) currentItems
    liftIO $ printf "Items to delete count: %d\n" (length itemsToDelete)
    _ <- deleteMachineItems (map miid itemsToDelete)
    liftIO $ printf "Deleted\n"
    let vendingMachineItems = map (itemsToEntity  m allItems currentItems) items
    mapM_ insertMachineItem vendingMachineItems
    liftIO $ printf "Inserted machine items count: %d\n" (length vendingMachineItems)

    where
        findByName itemToName items itemName = case findByNameMaybe itemToName items itemName of
            Just a -> a
            Nothing -> error "Cannot happen"
        findByNameMaybe itemToName items itemName = find (\x -> itemToName x == itemName) items

        itemsToEntity m allItems currentItems x = Database.VendingMachineItem {
            miid = def
            , item = iid itemEntity
            , Database.machine = mid m
            , amount = itemAmount x
            }
            where
                itemEntity = findByName itemName allItems (name x)
