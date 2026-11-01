-- Đọc bản đồ từ file văn bản 

module Parser.MapParser
  ( Cell (..)
  , parseMapText
  , loadMapFile
  , cellPos
  ) where

import Data.Bifunctor (first)
import Data.Maybe     (catMaybes)
import Data.Void      (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

import Model.Config
import Model.Error
import Model.Types
import Model.World  (wallMaxHp)
import Storage.Files (readFileStrict)

type Parser = Parsec Void String

data Cell
  = CEmpty | CBrick | CSteel | CRandom
  | CMud | CTrap | CSpring | CBarrel | CTele
  deriving (Show, Eq)

data Row = Row { rowOffset :: Int, rowCells :: [Cell] }

cellP :: Parser Cell
cellP = choice
  [ CSteel  <$ char '#' -- tường thép (không vỡ)
  , CBrick  <$ char 'B' -- tường gạch (1 viên đạn là vỡ)
  , CEmpty  <$ char '.' -- ô trống
  , CRandom <$ char '?' -- ô ngẫu nhiên (thép/gạch/trống, quyết định lại mỗi trận)
  , CMud    <$ char '~' -- bùn lầy
  , CTrap   <$ char 'T' -- bẫy điện từ 
  , CSpring <$ char 'S' -- lò xo
  , CBarrel <$ char 'X' -- thùng nổ
  , CTele   <$ char 'O' -- cổng dịch chuyển
  ] <?> "map cell (# steel, B brick, . empty, ? random, ~ mud, T trap, S spring, X barrel, O teleporter)"

rowP :: Parser Row
rowP = do
  o  <- getOffset
  cs <- some cellP
  pure (Row o cs)

commentP :: Parser ()
commentP = char ';' *> takeWhileP Nothing (/= '\n') *> pure ()

-- Một dòng: hàng ô, chú thích, hoặc dòng trống 
lineP :: Parser (Maybe Row)
lineP = (Just <$> rowP) <|> (Nothing <$ commentP) <|> pure Nothing

mapP :: Parser [Row]
mapP = catMaybes <$> (lineP `sepEndBy` eol) <* eof

-- Phân tích nội dung file map thành 'MapLayout'
parseMapText :: FilePath -> String -> Either GameError MapLayout
parseMapText path raw = do
  rows <- first (MapParseError path . errorBundlePretty) (parse mapP path src)
  buildLayout path src rows
  where
    src = dropBom raw
    dropBom ('\xFEFF' : xs) = xs
    dropBom xs              = xs

-- toạ độ ô (cột, hàng) => tọa độ thế giới (x, y)
cellPos :: Int -> Int -> Vec2
cellPos c r = Vec2 ((fromIntegral c - fromIntegral (arenaCols - 1) / 2) * tileSize)
                   ((fromIntegral (arenaRows - 1) / 2 - fromIntegral r) * tileSize)

-- Kiểm tra hình dạng lưới rồi đổi sang tọa độ thế giới
buildLayout :: FilePath -> String -> [Row] -> Either GameError MapLayout
buildLayout path src rows
  | null rows = bad "map file is empty"
  | (r : _) <- badRows =
      bad ("line " ++ show (lineOf (rowOffset r)) ++ ": row has "
           ++ show (length (rowCells r)) ++ " cells, expected " ++ show width
           ++ " (same as the first row)")
  | width /= arenaCols || height /= arenaRows =
      bad ("map is " ++ show width ++ "x" ++ show height ++ " cells, expected "
           ++ show arenaCols ++ "x" ++ show arenaRows)
  | otherwise = do
      teleFloors <- pairTeleporters
      Right MapLayout
        { layoutWalls  = [Wall p wt (wallMaxHp wt) | (p, c) <- cells, Just wt <- [wallOf c]]
        , layoutFloors = [Floor p k | (p, c) <- cells, Just k <- [floorOf c]] ++ teleFloors
        , layoutRandom = [p | (p, CRandom) <- cells]
        }
  where
    bad msg  = Left (MapParseError path msg)
    height   = length rows
    width    = length (rowCells (head rows))
    badRows  = filter ((/= width) . length . rowCells) rows
    lineOf o = 1 + length (filter (== '\n') (take o src))
    cells    = [ (cellPos ci ri, cell)
               | (ri, Row _ cs) <- zip [0 :: Int ..] rows
               , (ci, cell)     <- zip [0 :: Int ..] cs ]
    -- Cổng dịch chuyển: đúng hai ô 'O' (theo thứ tự đọc), nối với nhau
    pairTeleporters = case [p | (p, CTele) <- cells] of
      []     -> Right []
      [a, b] -> Right [Floor a (Teleporter b), Floor b (Teleporter a)]
      ps     -> bad ("teleporter 'O' must appear exactly twice (or not at all), found "
                     ++ show (length ps))

wallOf :: Cell -> Maybe WallType
wallOf CSteel  = Just SolidSteel
wallOf CBrick  = Just BreakableBrick
wallOf CBarrel = Just ExplosiveBarrel
wallOf _       = Nothing

floorOf :: Cell -> Maybe FloorKind
floorOf CMud    = Just Mud
floorOf CTrap   = Just Trap
floorOf CSpring = Just Spring
floorOf _       = Nothing

-- Đọc file map. Mọi lỗi (thiếu file, sai cú pháp) là @Left@
loadMapFile :: FilePath -> IO (Either GameError MapLayout)
loadMapFile path = do
  r <- readFileStrict path
  pure (r >>= parseMapText path)
