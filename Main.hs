module Main where

import Scrappy.Scrape (scrape, scrapeFirst', ScraperT)
import Scrappy.Elem.SimpleElemParser (el)
import Scrappy.Elem.TreeElemParser (table, htmlGroup)
import Scrappy.Elem.Types (ElementRep(..), GroupHtml(..), TreeHTML(..), Elem'(..), ungroup)

import Data.Maybe (fromMaybe)
import qualified Data.Map as Map


-- Sample HTML: simple single-column table (works with tree-based `table` parser)
simpleTable :: String
simpleTable = concat
  [ "<h1>Scores</h1>"
  , "<table>"
  , "<tr><td>Alice</td></tr>"
  , "<tr><td>Bob</td></tr>"
  , "<tr><td>Carol</td></tr>"
  , "<tr><td>Dave</td></tr>"
  , "</table>"
  ]

-- Sample HTML: multi-column employee table
employeeTable :: String
employeeTable = concat
  [ "<html><body>"
  , "<h1>Employee Directory</h1>"
  , "<table class=\"employees\">"
  , "<tr><td>Alice Chen</td><td>Engineer</td><td>San Francisco</td></tr>"
  , "<tr><td>Bob Smith</td><td>Designer</td><td>New York</td></tr>"
  , "<tr><td>Carol Jones</td><td>Manager</td><td>London</td></tr>"
  , "<tr><td>Dave Wilson</td><td>Analyst</td><td>Toronto</td></tr>"
  , "<tr><td>Eve Brown</td><td>Engineer</td><td>Berlin</td></tr>"
  , "</table>"
  , "</body></html>"
  ]

-- Sample HTML: product table with links
productTable :: String
productTable = concat
  [ "<table id=\"products\">"
  , "<tr><td><a href=\"/products/1\">Widget A</a></td><td>$9.99</td><td>In Stock</td></tr>"
  , "<tr><td><a href=\"/products/2\">Widget B</a></td><td>$14.99</td><td>Out of Stock</td></tr>"
  , "<tr><td><a href=\"/products/3\">Gadget C</a></td><td>$24.99</td><td>In Stock</td></tr>"
  , "<tr><td><a href=\"/products/4\">Gadget D</a></td><td>$7.50</td><td>In Stock</td></tr>"
  , "</table>"
  ]


main :: IO ()
main = do
  putStrLn "=== Scrappy Table Scraping Demo ===\n"

  ---------------------------------------------------------
  -- Demo 1: Built-in `table` parser (groups <tr> by tree structure)
  ---------------------------------------------------------
  putStrLn "--- Demo 1: `table` parser (single-column) ---"
  case scrapeFirst' table simpleTable of
    Nothing -> putStrLn "No table found!"
    Just (GroupHtml rows count maxLen) -> do
      putStrLn $ "Found " <> show count <> " rows (max inner length: " <> show maxLen <> ")"
      putStrLn "Row contents:"
      mapM_ (\row -> putStrLn $ "  " <> innerText' row) rows

  putStrLn ""

  ---------------------------------------------------------
  -- Demo 2: Scrape all <tr> then extract <td> cells from each
  ---------------------------------------------------------
  putStrLn "--- Demo 2: el \"tr\" + el \"td\" (multi-column table) ---"
  case scrape (el "tr" []) employeeTable of
    Nothing -> putStrLn "No rows found!"
    Just rows -> do
      putStrLn $ "Found " <> show (length rows) <> " rows:"
      mapM_ (\row -> do
        let cells = fromMaybe [] $ scrape (el "td" []) (innerHtmlFull row)
        let cellTexts = map innerHtmlFull cells
        putStrLn $ "  " <> show cellTexts
        ) rows

  putStrLn ""

  ---------------------------------------------------------
  -- Demo 3: Extract links and data from product table
  ---------------------------------------------------------
  putStrLn "--- Demo 3: Product table with link extraction ---"
  case scrape (el "tr" []) productTable of
    Nothing -> putStrLn "No rows found!"
    Just rows -> do
      putStrLn $ "Found " <> show (length rows) <> " product rows:"
      mapM_ (\row -> do
        let rowHtml = innerHtmlFull row
        let cells = fromMaybe [] $ scrape (el "td" []) rowHtml
        let links = fromMaybe [] $ scrape (el "a" []) rowHtml
        let linkInfo = case links of
              (link:_) -> " [link: " <> fromMaybe "?" (Map.lookup "href" (_attrs link)) <> "]"
              []       -> ""
        let cellTexts = map innerHtmlFull cells
        putStrLn $ "  " <> show cellTexts <> linkInfo
        ) rows

  putStrLn ""

  ---------------------------------------------------------
  -- Demo 4: htmlGroup to find repeated <li> elements
  ---------------------------------------------------------
  putStrLn "--- Demo 4: htmlGroup for <li> elements ---"
  let listHtml = concat
        [ "<ul>"
        , "<li class=\"item\">Apple</li>"
        , "<li class=\"item\">Banana</li>"
        , "<li class=\"item\">Cherry</li>"
        , "<li class=\"item\">Date</li>"
        , "</ul>"
        ]
  let liGroup = htmlGroup (Just ["li"]) Nothing [("class", Just "item")]
                  :: ScraperT (GroupHtml TreeHTML String)
  case scrapeFirst' liGroup listHtml of
    Nothing -> putStrLn "No list items found!"
    Just (GroupHtml items count _) -> do
      putStrLn $ "Found " <> show count <> " list items:"
      mapM_ (\item -> putStrLn $ "  - " <> innerText' item) items

  putStrLn ""

  ---------------------------------------------------------
  -- Demo 5: el with attribute matching
  ---------------------------------------------------------
  putStrLn "--- Demo 5: el with attribute filter ---"
  let attrHtml = concat
        [ "<div>"
        , "<span class=\"name\">Alice</span>"
        , "<span class=\"role\">Engineer</span>"
        , "<span class=\"name\">Bob</span>"
        , "<span class=\"role\">Designer</span>"
        , "</div>"
        ]
  putStrLn "All spans:"
  case scrape (el "span" []) attrHtml of
    Nothing -> putStrLn "  (none)"
    Just spans -> mapM_ (\s -> putStrLn $ "  " <> innerHtmlFull s) spans
  putStrLn "Only name spans:"
  case scrape (el "span" [("class", "name")]) attrHtml of
    Nothing -> putStrLn "  (none)"
    Just spans -> mapM_ (\s -> putStrLn $ "  " <> innerHtmlFull s) spans

  putStrLn "\n=== Done ==="
