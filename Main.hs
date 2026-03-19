module Main where

import Scrappy.Scrape (scrape, scrapeFirst', ScraperT)
import Scrappy.Elem.SimpleElemParser (el)
import Scrappy.Elem.TreeElemParser (table, htmlGroup)
import Scrappy.Elem.Types (ElementRep(..), GroupHtml(..), TreeHTML(..), Elem'(..), ungroup)
import Scrappy.Requests (getHtml')

import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import System.Environment (getArgs)


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

  ---------------------------------------------------------
  -- Demo 6: Scrape sample.html - extract all data rows
  ---------------------------------------------------------
  putStrLn "\n--- Demo 6: Scraping sample.html - all employee rows ---"
  html <- readFile "sample.html"
  putStrLn $ "Loaded sample.html (" <> show (length html) <> " chars)"
  case scrape (el "tr" []) html of
    Nothing -> putStrLn "No rows found!"
    Just rows -> do
      let dataRows = filter (hasTag "td") rows
      putStrLn $ "Found " <> show (length dataRows) <> " data rows across all tables:"
      mapM_ (\row -> do
        let cells = fromMaybe [] $ scrape (el "td" []) (innerHtmlFull row)
        let cellTexts = map innerHtmlFull cells
        putStrLn $ "  " <> show cellTexts
        ) dataRows

  putStrLn ""

  ---------------------------------------------------------
  -- Demo 7: Scrape sample.html - extract profile links
  ---------------------------------------------------------
  putStrLn "--- Demo 7: Scraping sample.html - employee profile links ---"
  case scrape (el "a" []) html of
    Nothing -> putStrLn "No links found!"
    Just links -> do
      let profileLinks = filter (\l -> maybe False (isPrefixOf' "/employees/") (Map.lookup "href" (_attrs l))) links
      putStrLn $ "Found " <> show (length profileLinks) <> " profile links:"
      mapM_ (\l -> do
        let href = fromMaybe "?" $ Map.lookup "href" (_attrs l)
        putStrLn $ "  " <> innerHtmlFull l <> " -> " <> href
        ) profileLinks

  putStrLn ""

  ---------------------------------------------------------
  -- Demo 8: Scrape sample.html - filter by attribute (active vs on-leave)
  ---------------------------------------------------------
  putStrLn "--- Demo 8: Scraping sample.html - status filtering ---"
  case scrape (el "td" [("class", "status-active")]) html of
    Nothing -> putStrLn "Active employees: 0"
    Just actives -> putStrLn $ "Active employees: " <> show (length actives)
  case scrape (el "td" [("class", "status-onleave")]) html of
    Nothing -> putStrLn "On leave: 0"
    Just onleave -> putStrLn $ "On leave: " <> show (length onleave)

  putStrLn ""

  ---------------------------------------------------------
  -- Demo 9: Scrape sample.html - project list via htmlGroup
  ---------------------------------------------------------
  putStrLn "--- Demo 9: Scraping sample.html - project list ---"
  let projectGroup = htmlGroup (Just ["li"]) Nothing [("class", Just "project")]
                       :: ScraperT (GroupHtml TreeHTML String)
  case scrapeFirst' projectGroup html of
    Nothing -> putStrLn "No projects found!"
    Just (GroupHtml items count _) -> do
      putStrLn $ "Found " <> show count <> " projects:"
      mapM_ (\item -> putStrLn $ "  - " <> innerText' item) items

  putStrLn ""

  ---------------------------------------------------------
  -- Demo 10: Scrape sample.html - per-department breakdown
  ---------------------------------------------------------
  putStrLn "--- Demo 10: Scraping sample.html - per department ---"
  let departments = ["engineering", "design", "management"]
  mapM_ (\dept -> do
    case scrape (el "table" [("id", dept)]) html of
      Nothing -> putStrLn $ "  " <> dept <> ": not found"
      Just (t:_) -> do
        let dataRows = filter (hasTag "td") $ fromMaybe [] $ scrape (el "tr" []) (innerHtmlFull t)
        let names = map (innerHtmlFull . head) $ filter (not . null) $
                    map (fromMaybe [] . scrape (el "td" []) . innerHtmlFull) dataRows
        putStrLn $ "  " <> dept <> ": " <> show names
      Just [] -> putStrLn $ "  " <> dept <> ": empty"
    ) departments

  ---------------------------------------------------------
  -- Demo 11: Scrape from a live URL
  ---------------------------------------------------------
  putStrLn "\n--- Demo 11: Scraping from URL ---"
  args <- getArgs
  case args of
    (url:_) -> do
      putStrLn $ "Fetching: " <> url
      liveHtml <- getHtml' url
      putStrLn $ "Got " <> show (length liveHtml) <> " chars"

      putStrLn "\nAll links on page:"
      case scrape (el "a" []) liveHtml of
        Nothing -> putStrLn "  (none)"
        Just allLinks -> do
          putStrLn $ "  Found " <> show (length allLinks) <> " links"
          mapM_ (\l -> do
            let href = fromMaybe "" $ Map.lookup "href" (_attrs l)
            let text = innerHtmlFull l
            let label = if length text > 60 then take 60 text <> "..." else text
            putStrLn $ "  " <> label <> " -> " <> href
            ) (take 15 allLinks)
          if length allLinks > 15
            then putStrLn $ "  ... and " <> show (length allLinks - 15) <> " more"
            else pure ()

      putStrLn "\nAll tables on page:"
      case scrape (el "table" []) liveHtml of
        Nothing -> putStrLn "  No tables found"
        Just tables -> do
          putStrLn $ "  Found " <> show (length tables) <> " tables"
          mapM_ (\t -> do
            let dataRows = filter (hasTag "td") $ fromMaybe [] $ scrape (el "tr" []) (innerHtmlFull t)
            putStrLn $ "  Table with " <> show (length dataRows) <> " data rows"
            mapM_ (\row -> do
              let cells = fromMaybe [] $ scrape (el "td" []) (innerHtmlFull row)
              let cellTexts = map (\c -> let t' = innerHtmlFull c in if length t' > 40 then take 40 t' <> "..." else t') cells
              putStrLn $ "    " <> show cellTexts
              ) (take 5 dataRows)
            if length dataRows > 5
              then putStrLn $ "    ... and " <> show (length dataRows - 5) <> " more rows"
              else pure ()
            ) tables

    [] -> putStrLn "  (skipped - pass a URL as argument to try it)"
           >> putStrLn "  Usage: cabal run table-demo -- \"https://example.com\""

  putStrLn "\n=== Done ==="


-- Helpers

hasTag :: String -> Elem' String -> Bool
hasTag tag e = case scrape (el tag []) (innerHtmlFull e) of
  Just (_:_) -> True
  _          -> False

isPrefixOf' :: String -> String -> Bool
isPrefixOf' [] _ = True
isPrefixOf' _ [] = False
isPrefixOf' (p:ps) (x:xs) = p == x && isPrefixOf' ps xs
