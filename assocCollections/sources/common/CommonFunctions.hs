module CommonFunctions where

import qualified Data.Char as DC (
    isSpace
    )

import qualified Data.Text.Lazy as DTL (
      Text(..)
    , all
    , isPrefixOf
    , lines
    , null , pack
    , strip
    , stripStart
    )

import qualified Data.Text.Lazy.IO as DTLI (
    readFile
    )


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

infix 4 ><


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

commentText :: DTL.Text
commentText = DTL.pack "--"

-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

getAssocCollectionImplementationsFromFile :: FilePath -> IO [ DTL.Text ]
getAssocCollectionImplementationsFromFile f = do
    activeLines <- getActiveLinesFromFile f commentText
    return . map DTL.strip $ activeLines


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

getActiveLinesFromFile :: FilePath -> DTL.Text -> IO [ DTL.Text ]
getActiveLinesFromFile f cs = do
    fileContents <- DTLI.readFile f
    return . dropCommentLines cs . dropEmptyLines . DTL.lines $ fileContents


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

dropEmptyLines :: [ DTL.Text ] -> [ DTL.Text ]
dropEmptyLines = filter ( not . DTL.all DC.isSpace ) . filter ( not . DTL.null )


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

dropCommentLines :: DTL.Text -> [ DTL.Text ] -> [ DTL.Text ]
dropCommentLines cs = filter ( not . DTL.isPrefixOf cs . DTL.stripStart )


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

groupHeads :: [ [ a ] ] -> [ [ a ] ]
groupHeads [] = []
groupHeads ll
    | any null ll = []
    | otherwise =
        let
            heads = map ( \ l -> head l ) ll
        in
            heads : groupHeads ( map tail ll )


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

split :: ( a -> b ) -> ( a -> c ) -> a -> ( b , c )
split f g x = ( f x , g x )


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

(><) :: ( a -> b ) -> ( c -> d ) -> ( a , c ) -> ( b , d )
( f >< g ) ( a , b ) = ( f a , g b )


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------

splitInChunksOf :: Int -> [ a ] -> [ [ a ] ]
splitInChunksOf _ [] = []
splitInChunksOf n l
    | n > 0 =
        let
            ( fstChunck , theRestOfTheList ) = splitAt n l
        in
            fstChunck : splitInChunksOf n theRestOfTheList
    | otherwise = []


-- ---------- ---------- ---------- ---------- ---------- ---------- ----------


