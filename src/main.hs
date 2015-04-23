{-# LANGUAGE ImplicitParams, OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo #-}

module Main where
import qualified FreeGame as F
import Prelude hiding(const)
import Control.Monad hiding(const)
-- import Control.Monad.Fix(fix)
import Control.Applicative((<*>), (<$>))
import Control.Lens 
import qualified FRP.Elerea.Simple as E
import Data.Range.Range 
import Data.List ((\\))
import Data.Maybe (catMaybes, fromMaybe, fromJust, listToMaybe, maybeToList)
import qualified Data.Map as Map
import qualified Data.Array as Array
import Language.Haskell.TH hiding(Range)
import Paths_Grids

-- debug
import System.IO(putStrLn)

defaultWidth, defaultHeight, cellLong :: (Num a) => a
defaultWidth = 800
defaultHeight = 600
cellLong = 40 

F.loadBitmapsWith [|getDataFileName|] "../images"

newtype Cell = Cell (Int, Int) deriving (Eq,Show,Ord)

type XCoord = Int -- →
type YCoord = Int -- ↓
type Coord = (XCoord, YCoord)
type Board = [Cell]
type Rect = (Double,Double,Double,Double)

data Direct = UP | DOWN | LEFT | RIGHT deriving (Eq, Show, Ord)

instance Num(Cell) where 
    Cell (a,b) + Cell (c,d) = Cell (a+c, b+d)
    Cell (a,b) * Cell (c,d) = Cell (a*c, b*d)
    Cell (a,b) - Cell (c,d) = Cell (a-c, b-d)
    abs (Cell (a,b)) = Cell (abs a, abs b)
    signum (Cell (a,b)) = Cell (signum a, signum b) 
    fromInteger i = Cell (fromInteger i, fromInteger i)

coordVec :: Coord -> F.Vec2
coordVec (x, y) =  F.V2 (fromIntegral x) (fromIntegral y)

data Edge = UpperLeft | LowerLeft | UpperRight | LowerRight
               deriving (Eq,Show)

allEdge = [UpperLeft,LowerLeft,LowerRight,UpperRight]

cornerPoint :: F.Vec2 -> Double -> Edge -> Cell -> F.Vec2
cornerPoint origin long edge c = let cpoint :: Cell -> F.Vec2
                                     cpoint (Cell (r,c)) = origin + F.V2 (fromIntegral c * long) (fromIntegral r * long) 
                                 in cpoint $ celledge c edge

rectCell :: F.Vec2 -> Double -> Cell -> [F.Vec2]
rectCell origin long c = map (\e -> cornerPoint origin long e c) allEdge

edgeIn :: F.Affine p => F.Vec2 -> Double -> Cell -> p a -> p a
edgeIn o l c = F.translate $ cornerPoint o l UpperLeft c

allDirection :: [Direct]
allDirection = [UP, LEFT, DOWN, RIGHT]

fromDirect :: Direct -> Direct -> Edge
fromDirect UP    LEFT  = UpperLeft
fromDirect LEFT  UP    = UpperLeft
fromDirect DOWN  LEFT  = LowerLeft
fromDirect LEFT  DOWN  = LowerLeft
fromDirect RIGHT DOWN  = LowerRight
fromDirect DOWN  RIGHT = LowerRight
fromDirect UP    RIGHT = UpperRight
fromDirect RIGHT UP    = UpperRight

directLineEdge :: Direct -> [Edge]
directLineEdge UP    = [UpperRight, UpperLeft]
directLineEdge LEFT  = [UpperLeft , LowerLeft]
directLineEdge DOWN  = [LowerLeft , LowerRight]
directLineEdge RIGHT = [LowerRight, UpperRight]

adjacentCell :: Cell -> Direct -> Cell
adjacentCell (Cell (r,c)) UP    = Cell (r-1,c)
adjacentCell (Cell (r,c)) DOWN  = Cell (r+1,c)
adjacentCell (Cell (r,c)) LEFT  = Cell (r,c-1)
adjacentCell (Cell (r,c)) RIGHT = Cell (r,c+1)

celledge :: Cell -> Edge -> Cell
celledge (Cell (r,c)) UpperLeft  = Cell (r, c) 
celledge (Cell (r,c)) LowerLeft  = Cell (r, c+1)
celledge (Cell (r,c)) LowerRight = Cell (r+1, c+1) 
celledge (Cell (r,c)) UpperRight = Cell (r+1, c)   

adjacentDirection :: Cell -> Cell -> Maybe Direct
adjacentDirection ours theirs = listToMaybe $ filter (\d -> adjacentCell ours d == theirs) allDirection 

sum_move :: Cell -> [Direct] -> [Direct] -> Cell
sum_move c dirs filter_dir = let dirs' = dirs \\ filter_dir
                             in flip (foldr (flip adjacentCell)) dirs' c

class Ranged a b where
    range :: a -> Range b -- (type a) holding Range 
    rangedValue :: a -> b -- retrieve ranged value (type b) from (type a)

data RangedValue a = RangedValue 
                   { rangeSize :: Range a
                   , _rangeInsideValue :: a 
                   }

makeLenses ''RangedValue

ranged :: Ord a => Range a -> a -> RangedValue a 
ranged r v | inRange r v = RangedValue {rangeSize = r, _rangeInsideValue = v} 

instance Ranged (RangedValue a) a where
    range :: RangedValue a -> Range a
    range = rangeSize
    rangedValue :: RangedValue a -> a
    rangedValue = _rangeInsideValue

newtype SizeTuple = SizeTuple Coord

sizeTupleCell :: SizeTuple -> Cell
sizeTupleCell (SizeTuple c) = Cell c

maxCoord :: SizeTuple -> Coord
maxCoord (SizeTuple crd) = crd

newtype SizedBlock25x25 = SizedBlock25x25 Cell
                          deriving(Eq,Show,Ord)

class SizedBlock a where
    cellValue :: a -> Cell
    createBlock :: Cell -> a
    blockSize :: a -> SizeTuple

instance SizedBlock SizedBlock25x25 where
    cellValue (SizedBlock25x25 c) = c
    blockSize a = SizeTuple (25,25)
    createBlock = SizedBlock25x25 

blockSizeCell :: (SizedBlock a) => a -> Cell
blockSizeCell sb = sizeTupleCell $ blockSize sb

instance Ranged SizedBlock25x25 Cell where
    range = sBrange
    rangedValue = sBrangedValue

-- instance (SizedBlock a) => Ranged a Cell where
sBrange sb = SpanRange (Cell(0,0)) (blockSizeCell sb)
sBrangedValue = cellValue

sBsucc sb | cs == cellValue sb   = createBlock $ cv + Cell (1,0)
          | otherwise = createBlock $ cv + Cell (0,1)
  where cs = blockSizeCell sb
        cv = cellValue sb
sBfromEnum sb = r * mc + c
  where (_, mc) = maxCoord.blockSize $ sb
        Cell (r, c) = cellValue sb

instance Enum(SizedBlock25x25) where
    succ = sBsucc
    fromEnum = sBfromEnum
    toEnum i = SizedBlock25x25 $Cell (divMod i 25)

data Slider = Slider 
            { inner_range :: Range Int
            , sliderSize :: Int
            , _percent :: RangedValue Int
            }

makeLenses ''Slider

slider :: Int -> Int -> Slider
slider max per = Slider 
               { inner_range = SpanRange (-max) (max)
               , sliderSize = max
               , _percent = ranged (SpanRange (-100) 100) per
               }


slideUp :: Int -> Slider -> Slider
slideUp i sl  | per >= 100 = sl&percent.rangeInsideValue .~ 100
              | otherwise  = sl&percent.rangeInsideValue +~ i
           where per = sl^.percent^.rangeInsideValue
slideDown :: Int -> Slider -> Slider
slideDown i sl | per <= -100 = sl&percent.rangeInsideValue .~ -100
               | otherwise  = sl&percent.rangeInsideValue -~ i
           where per = sl^.percent^.rangeInsideValue

isSlideUpped :: Slider -> Bool
isSlideUpped s = s^.percent^.rangeInsideValue >= 100
isSlideDowned :: Slider -> Bool
isSlideDowned s = s^.percent^.rangeInsideValue <= -100

isSlideMax :: Slider -> Bool
isSlideMax s = val <= -100 || val >= 100
    where val = s^.percent^.rangeInsideValue

type RCoord = (Slider, Slider)

getRCoord :: RCoord -> F.Vec2
getRCoord (rx, ry) = F.V2 (fromR rx) (fromR ry)
    where 
      sliderSize' = fromIntegral.sliderSize 
      persent rc = fromIntegral $ rc^.percent^.rangeInsideValue :: Double
      fromR :: Slider -> Double
      fromR rc = sliderSize' rc  * (persent rc / 100) + cellLong / 2

slidePositive = slider (cellLong`div`2)  100
slideNegative = slider (cellLong`div`2) (-100)
slide0 = slider (cellLong`div`2) 0

slideUpx = slideUp 16 
slideDownx = slideDown 16 

center :: RCoord
center = (slide0,slide0)

slideRCoord :: RCoord -> Direct -> RCoord
slideRCoord (rcx, rcy) RIGHT = (slideUpx rcx, rcy) 
slideRCoord (rcx, rcy) LEFT  = (slideDownx rcx, rcy) 
slideRCoord (rcx, rcy) UP    = (rcx, slideDownx rcy) 
slideRCoord (rcx, rcy) DOWN  = (rcx, slideUpx rcy) 

nextDirect :: RCoord -> (RCoord, [Direct])
nextDirect (rcx, rcy) = ((rcx', rcy'), xdir ++ ydir)
    where
        maybeToListTuple (rc, dr) = (rc, maybeToList dr)
        (rcx', xdir) = maybeToListTuple $
                         case rcx of 
                           x | isSlideUpped x -> (slideNegative, Just RIGHT)
                             | isSlideDowned x -> (slidePositive, Just LEFT)
                             | otherwise -> (rcx, Nothing)
        (rcy', ydir) = maybeToListTuple $
                         case rcy of 
                           y | isSlideUpped y -> (slideNegative, Just DOWN)
                             | isSlideDowned y -> (slidePositive, Just UP)
                             | otherwise -> (rcy, Nothing)


data MoveCommand = Nuetral | Walk Direct | Stop Direct deriving (Eq, Show, Ord)

data ActionCommand = ActionCommand 
                   { moveCommand :: MoveCommand
                   }
                   
data CharaAction = Stopping | Walking Direct deriving (Eq, Show, Ord)


data CharaProps = CharaProps 
                { _cellProps :: CellProps
                , _fourSides :: Map.Map Direct [F.Bitmap]
                }

data CharaState = CharaState 
                { _hp :: Int
                , _direct :: Direct
                , _charaAction :: CharaAction
                , _cellState :: CellState
                }

class CharaStateI a where
    charaDirection :: a -> Direct

me_enty :: CharaProps
me_enty =  CharaProps (CellProps True True ) 
                      (Map.fromList [(UP   , [_back0_png,_back1_png,_back2_png,_back3_png,_back4_png]),
                                     (DOWN , [_front0_png,_front1_png,_front2_png,_front3_png,_front4_png]),
                                     (LEFT , [_left0_png,_left1_png,_left2_png,_left3_png,_left4_png]),
                                     (RIGHT, [_right0_png,_right1_png,_right2_png,_right3_png,_right4_png])
                                    ]
                      )
me_state :: CharaState
me_state = CharaState 100 DOWN Stopping (CellState (Cell(5,5)) center 0) 

me = (me_enty, me_state)

type Commands = Map.Map Cell ActionCommand

class FieldObj a where
    clip :: a -> F.Bitmap
    actOn :: a -> ActionCommand -> a
    effect :: a -> Commands -> Commands


type CellObj = (CellProps, CellState)
type Character = (CharaProps, CharaState)

frameLoop = 1500


data CellProps = CellProps 
              { _movable :: Bool 
              , _block :: Bool 
              }

data CellState = CellState 
               { _cell :: Cell -- 位置
               , _pos :: RCoord -- Cellからの相対位置
               , _elapsedFrames :: Int -- 現在の状況での経過フレーム
               }

picPos :: F.Vec2 -> Cell -> RCoord -> F.Vec2
picPos o c rc = cornerPoint o cellLong UpperLeft c + getRCoord rc

data FieldMap = FieldMap 
              { fieldIndex :: Cell
              , mobj :: [(CellProps,CellState)] -- (Enty, 初期State)
              , mchr :: [(CharaProps,CharaState)] -- (Enty, 初期State)
              , backpict :: Map.Map (SizedBlock25x25 , SizedBlock25x25) F.Bitmap
              , mapsize :: SizeTuple
              }

class FieldMapI a where
    mapIndex :: a -> Cell
    mapObjects :: a -> [(CellProps, CellState)]
    mapCharacters :: a -> [(CharaProps,CharaState)] 
    tileMaps :: a -> F.Vec2 -> (F.Vec2 -> F.Vec2) -> F.Frame ()
    mapSize :: a -> SizeTuple

instance FieldMapI FieldMap where
    mapIndex = fieldIndex 
    mapObjects = mobj 
    mapCharacters = mchr
    mapSize = mapsize
    tileMaps f vp trans =
        let bp = backpict f
        in forM_ (Map.keys bp) $
                 \crange -> let b = fromJust (Map.lookup crange bp) :: F.Bitmap
                            in forM [(fst crange)..(snd crange)] $
                                    \rc -> let transVal = trans $ picPos vp (cellValue rc) center
                                           in F.translate transVal (F.bitmap b)

fieldMap = Map.fromList [(Cell(5,5),
                          FieldMap
                              (Cell(5,5))
                              [(CellProps True True, (CellState (Cell(1,1)) center 0))
                              ,(CellProps True True, (CellState (Cell(2,2)) center 0))
                              ]
                              []
                              (Map.fromList [((SizedBlock25x25  (Cell(0,0)) ,SizedBlock25x25 (Cell(0,10)))
                                             , _maptips_ami_png)
                                            ,((SizedBlock25x25  (Cell(5,0)) ,SizedBlock25x25 (Cell(25,25)))
                                             , _maptips_grass_png)
                                            ]
                              ) 
                              (SizeTuple (25,25))
                         ),
                         (Cell(5,6),
                          FieldMap 
                              (Cell(5,6))
                              [] 
                              []
                              (Map.fromList [((SizedBlock25x25  (Cell(0,0)) ,SizedBlock25x25 (Cell(20,25)))
                                              , _maptips_ami_png)
                                            ]
                              )
                              (SizeTuple (25, 25))
                         )
                      ]

makeLenses ''CellProps
makeLenses ''CellState
makeLenses ''CharaProps
makeLenses ''CharaState

instance FieldObj Character where
    clip ((CharaProps (CellProps _ _) fside),
         (CharaState chp cdir action (CellState cell pos elapsed) )) 
         = case action of
               Stopping -> (fromJust ( Map.lookup cdir fside)) !! 0
               Walking dir -> fromJust (Map.lookup dir fside) !! (index elapsed) 
               where
                   index et | et < 8 = 1
                            | et < 16 = 2
                            | et < 24 = 3
                            | et < 32 = 4
                            | otherwise = 4
    actOn (charaEnty, charaState) (ActionCommand Nuetral)
          = (charaEnty, charaState&cellState.elapsedFrames.~elapsed') 
          where
              elapsed = charaState^.cellState^.elapsedFrames
              elapsed' = if  elapsed > frameLoop then 0 else elapsed + 1
    actOn (charaEnty, CharaState chp cdir action (CellState cell pos elapsed)) 
          (ActionCommand (Walk cmd_dir))
          = case action of
             Stopping -> (charaEnty
                         , CharaState chp cmd_dir (Walking cmd_dir) (CellState cell pos (elapsed+1))) 
             Walking current_dir 
                 | current_dir == cmd_dir -> (charaEnty , state') 
                 | otherwise ->  (charaEnty , CharaState chp cdir Stopping (CellState cell pos 0))
                where elapsed' = if elapsed > 32 then 0 else elapsed + 1
                      pos' = slideRCoord pos current_dir
                      (pos'', dirs') = nextDirect pos'
                      cell' = sum_move cell dirs' [] :: Cell
                      state' = CharaState chp current_dir (Walking current_dir ) $
                                               CellState cell' pos'' elapsed'                  
    actOn (charaEnty, charaState) (ActionCommand (Stop dir))
                = (charaEnty, stoppedState) 
                    where stoppedState = charaState&charaAction.~Stopping
                                                   &cellState.elapsedFrames.~0
                                                   &direct.~dir
    effect (_, state) cmd = Map.insert (state^.cellState^.cell) (ActionCommand Nuetral) cmd

main :: IO (Maybe a)
main = F.runGame F.Windowed (F.Box (F.V2 0 0) (F.V2 defaultWidth defaultHeight)) $ do
    font <- F.embedIO $ F.loadFont "VL-Gothic-Regular.ttf"
    let ?font = font
    mainLoop me

drawBackPict :: (Enum a, Monad m, Ord a, F.Picture2D m, SizedBlock a) =>
                 F.Vec2 -> (F.Vec2 -> F.Vec2) -> Map.Map (a, a) F.Bitmap -> m ()
drawBackPict origin trans bp =
    forM_ (Map.keys bp) $
          \crange -> let b = fromJust (Map.lookup crange bp) :: F.Bitmap
                     in forM [(fst crange)..(snd crange)] $
                        \rc -> let transVal = trans $ picPos origin (cellValue rc) center
                               in F.translate transVal (F.bitmap b)
          

mainLoop :: (?font:: F.Font) => (CharaProps, CharaState) -> F.Game a
mainLoop me = do
    let initial_origin = F.V2 0 0
        whole_rect = (0,0,defaultWidth, defaultHeight)
        initial_mapIndex = Cell(5,5)
        initial_map = fromJust $ Map.lookup initial_mapIndex fieldMap
        initial_cmd = Map.fromList [] :: Commands

    (directionKey, directionKeySink) <- F.embedIO $ E.external []

    network <- F.embedIO $ do
        E.start $ mdo
            cmd      <- E.transfer3 initial_cmd (\k p f cmd -> readCommands cmd k p f) directionKey player' field'
            player   <- E.transfer2 me (\c f p -> movePlayer c p f) cmd field' :: (E.SignalGen (E.Signal Character))
            player'  <- E.delay  me player 
            field    <- E.transfer initial_map (\p f -> f) player 
            field'   <- E.delay initial_map field
            viewport <- E.transfer2 initial_origin (\p f v -> moveView f p v) player field :: (E.SignalGen (E.Signal F.Vec2))
            return $ render whole_rect <$> player <*> viewport <*> field
    F.foreverFrame $ do
        inp <- filterM F.keyPress $ Map.keys key_map 
        join $ F.embedIO $ do
            directionKeySink $ inp
            network 

readCommands :: Commands -> [F.Key] -> Character -> FieldMap -> Commands
readCommands cmd keys c f =
    let dirInput =  listToMaybe $ map (\k -> fromJust $ Map.lookup k key_map) keys :: Maybe Direct
        cCell = (snd c)^.cellState^.cell :: Cell
        objCells = map ((^.cell).snd) (mapObjects f)
        mACmd = Map.lookup cCell cmd :: Maybe ActionCommand
        blockedDir = adjacentDirections objCells cCell :: [Direct]
        moveCmd :: MoveCommand
        moveCmd = case mACmd of
                     Just mc -> case dirInput of
                                    Just dir | dir `elem` blockedDir -> Stop dir
                                             | otherwise -> Walk dir 
                                    Nothing  -> Nuetral
                     Nothing -> Nuetral 
    in Map.insert cCell (ActionCommand{moveCommand = moveCmd}) cmd

render :: (?font :: F.Font) => Rect -> Character -> F.Vec2 -> FieldMap -> F.Frame ()
render displaySize m@(me, state) vp f = do
    renderBackGround vp f
    renderOwn m vp f
    renderInfo displaySize m vp f
    where
        renderInfo :: (?font :: F.Font) => Rect -> Character -> F.Vec2 -> FieldMap -> F.Frame ()
        renderInfo displaySize (me, state) vp f = do
            let c = state^.cellState^.cell
                fobj_ary = mapObjects f
                fchars = mapCharacters f
                msize = mapSize f
            F.color F.yellow 
                $ F.thickness 3 
                $ renderGrids displaySize cellLong
            F.color F.black 
                $ F.translate (F.V2 150 150) 
                $ F.text ?font 30 $ show c
            F.color F.black 
                $ F.translate (F.V2 40 40) 
                $ F.text ?font 40 $ show vp
        renderOwn :: Character -> F.Vec2 -> FieldMap -> F.Frame ()
        renderOwn (me, state) vp f = 
            let obj_cells = map ((^.cell).snd) (mapObjects f)
                c  = state^.cellState^.cell :: Cell
                p  = state^.cellState^.pos :: RCoord
                abpos = picPos vp c p
                in  F.translate abpos $ F.bitmap $ clip (me, state)

renderBackGround :: (FieldMapI f) => F.Vec2 -> f -> F.Frame ()
renderBackGround vp f = do
    let inx = mapIndex f
    mapM_ (\d -> let i= adjacentCell inx d
                 in  case Map.lookup i fieldMap of
                     Nothing -> return ()
                     Just m -> tileMaps m vp $ transMap (mapSize m) d 
          ) allDirection

    tileMaps f vp id
    where
        _transMap ::  Double -> Direct -> F.Vec2 -> F.Vec2
        _transMap y UP    = (-) $ F.V2 0 (cellLong*y)
        _transMap y DOWN  = (+) $ F.V2 0 (cellLong*y)
        _transMap x LEFT  = (-) $ F.V2 (cellLong*x) 0
        _transMap x RIGHT = (+) $ F.V2 (cellLong*x) 0
        transMap :: SizeTuple -> Direct -> F.Vec2 -> F.Vec2
        transMap st dir = let F.V2 sx sy = coordVec.maxCoord $ st 
                          in case dir of
                             UP    -> _transMap sy dir
                             DOWN  -> _transMap sy dir
                             LEFT  -> _transMap sx dir
                             RIGHT -> _transMap sx dir

key_map :: Map.Map F.Key Direct
key_map = Map.fromList _tbl
    where _tbl = [(F.KeyJ, DOWN)
                 ,(F.KeyK, UP)
                 ,(F.KeyH, LEFT)
                 ,(F.KeyL, RIGHT)]

turnBack :: Direct -> Direct
turnBack RIGHT = LEFT
turnBack LEFT  = RIGHT
turnBack UP    = DOWN
turnBack DOWN  = UP

-- 
movePlayer :: Commands -> Character -> FieldMap -> Character
movePlayer cmd (me, state) f = actOn (me, state) $ fromJust (Map.lookup (state^.cellState^.cell) cmd)

moveView :: FieldMap -> Character -> F.Vec2 -> F.Vec2
moveView f (me, state) vp =
    let
        c = state^.cellState^.cell :: Cell
        p  = state^.cellState^.pos :: RCoord
        abpos = picPos vp c p
    in (case abpos of 
             F.V2 px py | px <= (defaultWidth/3)    -> (+) $ F.V2 4 0 
                        | px >= (defaultWidth*2/3)  -> flip (-) $ F.V2 4 0
                        | py <= (defaultHeight/3)   -> (+) $ F.V2 0 4
                        | py >= (defaultHeight*2/3) -> flip (-) $ F.V2 0 4
                        | otherwise -> id
       ) vp

renderGrids :: Rect -> Double -> F.Frame ()
renderGrids (x0, y0, w, h) interval = renderStripeH (x0,y0,w,h) interval 
                                      >> renderStripeV (x0,y0,w,h) interval

renderStripeV' :: Rect -> Double -> F.Frame ()
renderStripeV' (x0, y0, w, h) interval  = zipWithM_ (\x y -> F.line [x,y])
            [F.V2 (x0 + p)  y0      | p <- [0,interval .. w]] 
            [F.V2 (x0 + p) (y0 + h) | p <- [0,interval ..]] 

renderStripeH :: Rect -> Double -> F.Frame ()
renderStripeH (x0, y0, w, h) interval = zipWithM_ (\x y -> F.line [x,y])
            [F.V2 x0       (y0 + p) | p <- [0,interval .. h]] 
            [F.V2 (x0 + w) (y0 + p) | p <- [0,interval ..]] 

renderStripeV (x0, y0, w, h) interval = zipWithM_ (\x y -> F.line [x,y])
            [F.V2 (diff (x0 + p))  y0 | p <- [0,interval .. w]] 
            [F.V2 (x0 + p) (y0 + h) | p <- [0,interval ..]] 
        where
            diff x = let c = x0 + w / 2
                         p = c - x
                     in  x + p * 0.3 

renderCellOutline :: F.Picture2D p => F.Vec2 -> Double -> Direct -> Cell -> p ()
renderCellOutline origin long dir c = F.line $ map (\edge -> cornerPoint origin long edge c)  $ directLineEdge dir

fillCell :: F.Picture2D p => F.Vec2 -> Double -> Cell -> p () 
fillCell origin long c = F.polygon $ rectCell origin long c 

strokeCell :: F.Picture2D p => F.Vec2 -> Double -> Cell -> p ()
strokeCell origin long c = F.polygonOutline $ rectCell origin long c 

fillCells :: (Monad p, F.Picture2D p) => F.Vec2 -> Double -> [Cell] -> p ()
fillCells origin long cs = mapM_ (fillCell origin long) cs 

strokeCells :: (Monad p, F.Picture2D p) => F.Vec2 -> Double -> [Cell] -> p ()
strokeCells origin long cs = mapM_ render cs
     where renderCellOutline' = renderCellOutline origin long
           adjacents c = adjacentDirections cs c
           render c =  mapM_ (\d -> renderCellOutline' d c) (adjacents c)

adjacentDirections :: Board -> Cell -> [Direct]
adjacentDirections board c = catMaybes $ map (adjacentDirection c) board

peripheralCells :: Cell -> Int -> [Cell]
peripheralCells (Cell (r,c)) w = [Cell (r',c') | r' <- [r-w..r+w] ,c' <- [c-w..c+w]]

adjacentCells :: Board -> Cell -> [Cell]
adjacentCells board c = filter (`elem` board) (peripheralCells c 1)
