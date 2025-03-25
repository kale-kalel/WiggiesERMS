{-# LANGUAGE OverloadedStrings #-}

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Control.Exception (bracket_)

-- Function to initialize the SQLite database by creating tables and inserting products if necessary.
initializeDatabase :: IO ()
initializeDatabase = do
    conn <- open "store.db"  -- Opens the SQLite database file (or creates one)
    execute_ conn createProductsTable  -- Creates the 'products' table
    execute_ conn createSalesTable  -- Creates the 'sales' table
    checkAndInsertProducts conn  -- Checks and inserts default products if the table is empty
    close conn  -- Closes the connection to the database

-- SQL statement to create the 'products' table
createProductsTable :: Query
createProductsTable = 
    "CREATE TABLE IF NOT EXISTS products (\
    \ id INTEGER PRIMARY KEY AUTOINCREMENT, \
    \ category TEXT, \
    \ item TEXT, \
    \ srp REAL, \
    \ dealer_price REAL)"

-- SQL statement to create the 'sales' table
createSalesTable :: Query
createSalesTable = 
    "CREATE TABLE IF NOT EXISTS sales (\
    \ id INTEGER PRIMARY KEY AUTOINCREMENT, \
    \ product_id INTEGER, \
    \ quantity INTEGER, \
    \ total_sales REAL, \
    \ total_profit REAL, \
    \ date TEXT, \
    \ FOREIGN KEY(product_id) REFERENCES products(id))"

-- Function that checks if the 'products' table is empty, and inserts default products if needed
checkAndInsertProducts :: Connection -> IO ()
checkAndInsertProducts conn = do
    [Only count] <- query_ conn "SELECT COUNT(*) FROM products" :: IO [Only Int]
    if count == 0
        then do
            let products = 
                    [ ("Premium IceCream", "Regular Gallon", 560, 460)
                    , ("Premium IceCream", "Regular 1.5L", 270, 230)
                    , ("Premium IceCream", "Regular 750ml", 155, 135)
                    , ("Supreme", "Supreme Gallon", 610, 480)
                    , ("Supreme", "Supreme 1.5L", 300, 250)
                    , ("Supreme", "Supreme 750ml", 185, 160)
                    , ("Others", "Sugar cone", 40, 25)
                    , ("Others", "Wafer Cone", 40, 25)
                    , ("Others", "Styro", 30, 42)
                    , ("Novelty", "Festive Cone", 22, 20)
                    , ("Novelty", "Festive Stick", 22, 20)
                    , ("Novelty", "Festive Cup", 30, 26)
                    , ("Novelty", "Dluxe Bar", 45, 40)
                    , ("Novelty", "Icy Pop", 10, 9)
                    , ("Novelty", "Vanilla Crunch", 25, 22)
                    , ("Novelty", "Party Cup", 15, 13)
                    , ("Novelty", "Sundae", 20, 18)
                    , ("Novelty", "Pint (reg)", 75, 67)
                    , ("Novelty", "Pint (S)", 95, 80)
                    , ("Novelty", "Café Mocha (1.5L)", 260, 240)
                    , ("Novelty", "Café Mocha (750ml)", 145, 135)
                    , ("Novelty", "Cookies & Cream (1.5L)", 260, 240)
                    , ("Novelty", "Cookies & Cream (750ml)", 145, 135)
                    ]
            executeMany conn "INSERT INTO products (category, item, srp, dealer_price) VALUES (?, ?, ?, ?)" products
            putStrLn "Inserted default products into the database."
        else putStrLn "Products already exist in the database."

-- Main function that calls the database initialization function
main :: IO ()
main = initializeDatabase
