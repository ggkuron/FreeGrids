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
import Control.Monad.Fix(fix)
import Control.Applicative((<*>), (<$>))
import Control.Lens 
import qualified FRP.Elerea.Simple as E
import Data.IORef
import Data.Range.Range 
import Data.List ((\\))
import Data.Maybe (catMaybes, fromMaybe, fromJust, listToMaybe, maybeToList)
import qualified Data.Map as Map
import qualified Data.Array as Array
import Language.Haskell.TH hiding(Range)
import Paths_Grids
import Control.Concurrent(threadDelay)

-- debug
import System.IO(putStrLn)

defaultWidth, defaultHeight :: (Num a) => a
defaultWidth = 800
defaultHeight = 600

F.loadBitmapsWith [|getDataFileName|] "../images"

newtype Cell = Cell (Int, Int) deriving (Eq,Show,Ord)
type Coord = (Int,Int)
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
    range :: a -> Range b -- (type a) holding Range (:type  b orelse type a)
    rangedValue :: a -> b -- retrieve ranged value (type b) from (type a)

data RangedValue a = RangedValue 
                   { _rangeSize :: Range a
                   , _rangeInsideValue :: a 
                   }

makeLenses ''RangedValue

ranged :: Ord a => Range a -> a -> RangedValue a 
ranged r v | inRange r v = RangedValue {_rangeSize = r, _rangeInsideValue = v} 

instance Ranged (RangedValue a) a where
    range :: RangedValue a -> Range a
    range = _rangeSize
    rangedValue :: RangedValue a -> a
    rangedValue = _rangeInsideValue

type XCoord = Int
type YCoord = Int
-- (→XCoord, ↓YCoord)
type SizeTuple = (XCoord, YCoord) 

data Size25x25 = Size25x25
newtype SizedBlock25x25 = SizedBlock25x25 Cell
                          deriving(Eq,Show,Ord)

class SizedBlock a where
    cellValue :: a -> Cell
    cellSize :: a -> SizeTuple

instance SizedBlock Size25x25 where
    cellValue a = Cell (0,0)
    cellSize a = (25,25)

instance SizedBlock SizedBlock25x25 where
    cellValue (SizedBlock25x25 c)  = c
    cellSize c = (25,25)

instance Ranged SizedBlock25x25 Cell where
    range rc = SpanRange (Cell(0,0)) (Cell (cellSize rc))
    rangedValue = cellValue

--  1. instance Enum SizedBlock s => s where ををかければいい…
--  2. instance RangedValue (SizeCell s) したい

instance Enum(SizedBlock25x25) where
    succ rc@(SizedBlock25x25 cell@(Cell (r, c))) 
                                  | mcell <= cell = error "over sized"
                                  | mc == c = SizedBlock25x25  $ cell + Cell (1,0)
                                  | otherwise = SizedBlock25x25 $ cell + Cell (0,1)
      where msize@(mr,mc) = cellSize rc
            mcell = Cell(msize)
    fromEnum rc@(SizedBlock25x25 (Cell (r, c))) = r * mc + c
      where (_, mc) = cellSize rc
    toEnum i = SizedBlock25x25 $Cell (divMod i 25)

data Slider = Slider 
            { _inner_range :: Range Int
            , _slider_size :: Int
            , _percent :: RangedValue Int
            }

makeLenses ''Slider

slider :: Int -> Int -> Slider
slider max per = Slider 
               { _inner_range = SpanRange (-max) (max)
               , _slider_size = max
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

isSlideUp :: Slider -> Bool
isSlideUp s = s^.percent^.rangeInsideValue >= 100
isSlideDown :: Slider -> Bool
isSlideDown s = s^.percent^.rangeInsideValue <= -100

isSlideMax :: Slider -> Bool
isSlideMax s = val <= -100 || val >= 100
    where val = s^.percent^.rangeInsideValue

type RCoord = (Slider, Slider)

getRCoord :: RCoord -> F.Vec2
getRCoord (rx, ry) = F.V2 (fromR rx) (fromR ry)
    where 
      slider_size = fromIntegral._slider_size 
      persent rc = fromIntegral $ rc^.percent^.rangeInsideValue :: Double
      fromR :: Slider -> Double
      fromR rc = slider_size rc  * (persent rc / 100) + cell_long / 2

slidePositive = slider (cell_long`div`2)  100
slideNegative = slider (cell_long`div`2) (-100)
slide0 = slider (cell_long`div`2) 0

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
                           x | isSlideUp x -> (slideNegative, Just RIGHT)
                             | isSlideDown x -> (slidePositive, Just LEFT)
                             | otherwise -> (rcx, Nothing)
        (rcy', ydir) = maybeToListTuple $
                         case rcy of 
                           y | isSlideUp y -> (slideNegative, Just DOWN)
                             | isSlideDown y -> (slidePositive, Just UP)
                             | otherwise -> (rcy, Nothing)


cell_long :: Num a => a
cell_long = 40 

data ActionCommand = Nuetral | Walk Direct | Stop Direct deriving (Eq, Show, Ord)
data CharaAction = Stopping | Walking Direct deriving (Eq, Show, Ord)


data CharaEnty = CharaEnty 
               { _cellEnty :: CellEnty
               , _fourside :: Map.Map Direct [F.Bitmap]
               }

data CharaState = CharaState 
                { _hp :: Int
                , _direct :: Direct
                , _charaAction :: CharaAction
                , _cellState :: CellState
                }


me_enty :: CharaEnty
me_enty =  CharaEnty (CellEnty True True ) 
                     (Map.fromList [(UP   , [_back0_png,_back1_png,_back2_png,_back3_png,_back4_png]),
                                    (DOWN , [_front0_png,_front1_png,_front2_png,_front3_png,_front4_png]),
                                    (LEFT , [_left0_png,_left1_png,_left2_png,_left3_png,_left4_png]),
                                    (RIGHT, [_right0_png,_right1_png,_right2_png,_right3_png,_right4_png])])
me_state :: CharaState
me_state = CharaState 100 DOWN Stopping (CellState (Cell(5,5)) center 0) 

me = (me_enty, me_state)

class FieldObj a where
    clip :: a -> F.Bitmap
    actOn :: a -> ActionCommand -> a
    effect :: a -> ActionCommand -> [(Cell,ActionCommand)]


type CellObj = (CellEnty, CellState)
type CharaObj = (CharaEnty,CharaState)

frameLoop = 1500


data CellEnty = CellEnty 
              { _movable :: Bool 
              , _block :: Bool 
              }

data CellState = CellState 
               { _cell :: Cell
               , _pos :: RCoord
               , _elapsedFrames :: Int
               }

data FieldMap = FieldMap 
              { _mobj :: [(CellEnty,CellState)] -- (Enty, 初期State)
              , _mchr :: [(CharaEnty,CharaState)] -- (Enty, 初期State)
              , _backpict :: Map.Map (SizedBlock25x25,SizedBlock25x25) F.Bitmap
              , _mapsize :: Size25x25
              }

class FieldMapI a where
    mapObjects :: a -> [(CellEnty, CellState)]
    mapSize :: a -> SizeTuple

field = Map.fromList [(Cell(5,5),
                      FieldMap [(CellEnty True True, (CellState (Cell(1,1)) center 0))
                               ,(CellEnty True True, (CellState (Cell(2,2)) center 0))
                               ] [] (Map.fromList [((SizedBlock25x25  (Cell(0,0)) ,SizedBlock25x25 (Cell(0,10)))
                                                    , _maptips_ami_png),
                                                   ((SizedBlock25x25  (Cell(5,0)) ,SizedBlock25x25 (Cell(25,25)))
                                                    , _maptips_grass_png)
                                                  ]
                                     ) (Size25x25)
                      ),
                      (Cell(5,6),
                      FieldMap [] []
                                    (Map.fromList [((SizedBlock25x25  (Cell(0,0)) ,SizedBlock25x25 (Cell(20,25)))
                                                    , _maptips_ami_png)
                                                  ]
                                     ) (Size25x25)
                      )
                      ]

makeLenses ''CellEnty
makeLenses ''CellState
makeLenses ''CharaEnty
makeLenses ''CharaState
makeLenses ''FieldMap

instance FieldObj CharaObj where
    clip ((CharaEnty (CellEnty _ _) fside),
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
    actOn (charaEnty, charaState) Nuetral 
          = (charaEnty, charaState&cellState.elapsedFrames.~elapsed') 
          where
              elapsed = charaState^.cellState^.elapsedFrames
              elapsed' = if  elapsed > frameLoop then 0 else elapsed + 1
    actOn (charaEnty, CharaState chp cdir action (CellState cell pos elapsed)) 
          (Walk cmd_dir)
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

    actOn (charaEnty, charaState) (Stop dir)
                = (charaEnty, stoppedState) 
                    where stoppedState = charaState&charaAction.~Stopping
                                                   &cellState.elapsedFrames.~0
                                                   &direct.~dir

main :: IO (Maybe a)
main = F.runGame F.Windowed (F.Box (F.V2 0 0) (F.V2 defaultWidth defaultHeight)) $ do
    font <- F.embedIO $ F.loadFont "VL-Gothic-Regular.ttf"
    let ?font = font
    mainLoop me_enty me_state

drawBackPict :: (Enum a, Monad m, Ord a, F.Picture2D m, SizedBlock a) =>
                 F.Vec2 -> Map.Map (a, a) F.Bitmap -> (F.Vec2 -> F.Vec2) -> m ()
drawBackPict origin bp trans =
    forM_ (Map.keys bp) 
                  (\crange -> let b = fromJust (Map.lookup crange bp) :: F.Bitmap
                              in forM [(fst crange)..(snd crange)] 
                                   $ \rc -> let transVal = trans $ picPos origin cell_long (cellValue rc) center
                                            in F.translate transVal (F.bitmap b)
                  )

mainLoop :: CharaEnty -> CharaState -> F.Game a
mainLoop me_enty me_state = do
    let initial_origin = F.V2 0 0
        thick = 2
        whole_rect = (0,0,defaultWidth, defaultHeight)
        initial_mapIndex = Cell(5,5)
        currentMap = fromJust $ Map.lookup initial_mapIndex field

    -- io_field    <- F.embedIO $ newIORef currentMap 
    -- io_me_state <- F.embedIO $ newIORef me_state
    -- io_mapIndex <- F.embedIO $ newIORef initial_mapIndex
    -- io_origin   <- F.embedIO $ newIORef initial_origin 
    (directionKey, directionKeySink) <- F.embedIO $ E.external (Just DOWN)

    network <- F.embedIO $ E.start $ mdo
        player   <- E.transfer2 (me_enty, me_state) (\dk f p -> movePlayer dk p f) directionKey field' :: (E.SignalGen (E.Signal Character))
            --  1 p directKeySink f -> p
        field    <- E.transfer currentMap (\p f -> f) player 
            -- 1 f p -> f
        field'   <- E.delay currentMap field
            -- sank
        viewport <- E.transfer2 initial_origin (\p f v -> moveView f p v ) player field :: (E.SignalGen (E.Signal F.Vec2))
            -- 1 v p f -> v
        return $ renderOwn <$> player <*> viewport <*> field :: (E.SignalGen (E.Signal (F.Frame ()))) 
            -- p v f
    F.foreverFrame $ do
        current_inp <- filterM F.keyPress $ Map.keys key_map 
        F.embedIO $ do
            directionKeySink $ readInput current_inp 
            network 
        -- F.color F.blue
        --     $ fillCell (F.V2 0 0) cell_long $ Cell (5,5)
---
---        current_field <- F.embedIO $ readIORef io_field
---        mapIndex      <- F.embedIO $ readIORef io_mapIndex
---        origin        <- F.embedIO $ readIORef io_origin
---        mapM_ (\d -> let inx = adjacentCell mapIndex d
---                     in  case Map.lookup inx field of
---                         Nothing -> return ()
---                         Just m -> drawBackPict origin (m^.backpict) $ transMap (cellSize (m^.mapsize)) d
---              ) allDirection
---
---        let cix     = zipWith (curry Cell) [0 .. mr] [0 .. mc] :: [Cell]
---            (mr,mc) = cellSize $ current_field^.mapsize
---            bp      = current_field^.backpict
---            me_cell = me_state^.cellState^.cell
---
---        mapM_ id [
---            drawBackPict origin bp id
---            -- ,
---            --F.color F.red 
---            --    $ ownCell (me_enty, io_me_state) io_origin io_field
---            --,
---            -- F.color F.yellow 
---            --     $ F.thickness 3 
---            --     $ renderGrids whole_rect cell_long
---            -- ,
---            -- F.color F.black 
---            --     $ F.translate (F.V2 150 150) 
---            --     $ F.text ?font 30 $ show me_cell
---            -- ,
---            -- F.color F.black 
---            --     $ F.translate (F.V2 40 40) 
---            --     $ F.text ?font 40 $ show origin 
---            ]
    where
        _transMap ::  Double -> Direct -> F.Vec2 -> F.Vec2
        _transMap y UP    = (-) $ F.V2 0 (cell_long*y)
        _transMap y DOWN  = (+) $ F.V2 0 (cell_long*y)
        _transMap x LEFT  = (-) $ F.V2 (cell_long*x) 0
        _transMap x RIGHT = (+) $ F.V2 (cell_long*x) 0
        transMap :: SizeTuple -> Direct -> F.Vec2 -> F.Vec2
        transMap st dir = let F.V2 sx sy = coordVec st 
                          in case dir of
                             UP    -> _transMap sy dir
                             DOWN  -> _transMap sy dir
                             LEFT  -> _transMap sx dir
                             RIGHT -> _transMap sx dir

readInput :: [F.Key] -> Maybe Direct
readInput keys = 
    listToMaybe $ map (\k -> fromJust $ Map.lookup k key_map) keys

renderOwn :: Character -> F.Vec2 -> FieldMap -> F.Frame ()
renderOwn (me, state) vp (FieldMap fobj_ary fchars bp msize) = 
    let obj_cells = map ((^.cell).snd) fobj_ary
        c  = state^.cellState^.cell
        p  = state^.cellState^.pos :: RCoord
        abpos = picPos vp cell_long c p
        in  F.translate abpos 
            $ F.bitmap $ clip (me, state)


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

type Character = (CharaEnty, CharaState)

    -- F.translate (F.V2 240 150) $ F.text ?font 50 (show abpos)
    -- F.translate (F.V2 240 250) $ F.text ?font 50 (show c)



ownCell :: (?font :: F.Font) => (CharaEnty, IORef CharaState) ->
                                IORef F.Vec2 -> IORef FieldMap -> F.Frame ()
ownCell (me, me_state) io_origin io_field = do
    (FieldMap fobj_ary fchars bp msize) <- F.embedIO $ readIORef io_field
    origin <- F.embedIO $ readIORef io_origin
    me_chara_state <- F.embedIO $ readIORef me_state
    let me_cellstate = me_chara_state^.cellState
        me_cell    = me_cellstate^.cell

    current_inp <- filterM F.keyPress $ Map.keys key_map 
    let inp_directs :: Maybe Direct
        inp_directs = listToMaybe $ map (\k -> fromJust $ Map.lookup k key_map) current_inp
        obj_cells   = map ((^.cell).snd) fobj_ary
        blocked_dir = adjacentDirections obj_cells me_cell

        cmd =  case inp_directs of 
                   Nothing -> Nuetral
                   Just dir | dir `elem` blocked_dir -> Stop dir
                            | otherwise -> Walk dir

        (me',me_state') = actOn (me, me_chara_state) cmd
        me_cellState'   = me_state'^.cellState
        me_pos          = me_cellState'^.pos
        me_cell'        = me_cellState'^.cell
        me_abpos        = picPos origin cell_long me_cell' me_pos
        origin' :: F.Vec2
        origin' =  (case me_abpos of 
             F.V2 px py | px <= (defaultWidth/3)    -> (+) $ F.V2 4 0 
                        | px >= (defaultWidth*2/3)  -> flip (-) $ F.V2 4 0
                        | py <= (defaultHeight/3)   -> (+) $ F.V2 0 4
                        | py >= (defaultHeight*2/3) -> flip (-) $ F.V2 0 4
                        | otherwise -> id
                   ) origin

    F.embedIO $ writeIORef io_origin origin'
    F.embedIO $ writeIORef me_state me_state'
    ainp <- F.keyPress F.KeyA
    when ainp $ fillCells origin' cell_long $ peripheralCells me_cell 1

    F.color F.yellow $ fillCells origin' cell_long obj_cells
    F.translate me_abpos 
        $ F.bitmap $ clip (me', me_state')
    F.translate (F.V2 240 150) $ F.text ?font 50 (show me_abpos)
    F.translate (F.V2 240 250) $ F.text ?font 50 (show me_cell')

movePlayer :: Maybe Direct -> Character -> FieldMap -> Character
movePlayer dir (me, state) (FieldMap fobj_ary fchars bp msize) = 
    let obj_cells = map ((^.cell).snd) fobj_ary
        me_cellState'   = me_state'^.cellState
        me_cell         = me_cellState'^.cell
        blocked_dir = adjacentDirections obj_cells me_cell
        cmd =  case dir of 
               Nothing -> Nuetral
               Just dir | dir `elem` blocked_dir -> Stop dir
                        | otherwise -> Walk dir
        (me',me_state') = actOn (me, state) cmd
    in (me', me_state')

moveView :: FieldMap -> Character -> F.Vec2 -> F.Vec2
moveView (FieldMap fobj_ary fchars bp msize) (me, state) vp =
    let c = state^.cellState^.cell :: Cell
        p  = state^.cellState^.pos :: RCoord
        abpos = picPos vp cell_long c p
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

renderStripeV :: Rect -> Double -> F.Frame ()
renderStripeV (x0, y0, w, h) interval  = zipWithM_ (\x y -> F.line [x,y])
            [F.V2 (x0 + p)  y0      | p <- [0,interval .. w]] 
            [F.V2 (x0 + p) (y0 + h) | p <- [0,interval ..]] 

renderStripeH :: Rect -> Double -> F.Frame ()
renderStripeH (x0, y0, w, h) interval = zipWithM_ (\x y -> F.line [x,y])
            [F.V2 x0       (y0 + p) | p <- [0,interval .. h]] 
            [F.V2 (x0 + w) (y0 + p) | p <- [0,interval ..]] 

renderStripeV' (x0, y0, w, h) interval = zipWithM_ (\x y -> F.line [x,y])
            [F.V2 (diff (x0 + p))  y0 | p <- [0,interval .. w]] 
            [F.V2 (x0 + p) (y0 + h) | p <- [0,interval ..]] 
        where
            diff x = let c = x0 + w / 2
                         p = c - x
                     in  x + p * 0.3 

renderCellOutline :: F.Picture2D p => F.Vec2 -> Double -> Direct -> Cell -> p ()
renderCellOutline origin long dir c = F.line $ map (\edge -> cornerPoint origin long edge c)  $ directLineEdge dir

picPos :: F.Vec2 -> Double -> Cell -> RCoord -> F.Vec2
picPos o l c rc = cornerPoint o l UpperLeft c + getRCoord rc

fillCell :: F.Picture2D p => F.Vec2 -> Double -> Cell -> p () 
fillCell origin long c = F.polygon $ rectCell origin long c 

-- tCell :: F.Affine p => F.Vec2 -> Double -> Cell -> Direct -> p a -> p a
-- tCell origin long c dir = F.translate $ (/4).sum $ rectCell origin long $ adjacentCell c dir

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


-- FRP
--

-- type Time = Float
-- data UserAction
-- 
-- newtype Behavior a = Behavior (([Maybe UserAction], [Time]) -> [a])
-- newtype Event a  = Event (([Maybe UserAction], [Time]) -> [Maybe a])
-- 
-- -- Behavior (([Maybe UserAction], [Time]) -> [Time]
-- time :: Behavior Time
-- time = Behavior (\(_, ts) -> ts)
-- 
-- const :: a -> Behavior a
-- const x = Behavior (\_ -> repeat x)

-- Applicative
-- m (a -> b) -> m a -> m b
-- ($*) :: Behavior (a -> b) -> Behavior a -> Behavior b
-- Behavior ff $* Behavior fb = Behavior (\uts -> zipWith ($) (ff uts) (fb uts))
-- 
-- lift0 :: a -> Behavior a
-- lift0 = const
-- 
-- lift1 :: (a -> b) -> (Behavior a -> Behavior b)
-- lift1 f b1 = lift0 f $* b1
-- 
-- lift2 :: (a -> b -> c) -> (Behavior a -> Behavior b -> Behavior c)
-- lift2 f b1 b2 = lift1 f b1 $* b2
-- 
-- lift3 :: (a -> b -> c -> d) -> (Behavior a -> Behavior b -> Behavior c -> Behavior d)
-- lift3 f b1 b2 b3 = lift2 f b1 b2 $* b3

--memoB :: Behavior a -> Behavior a
--memoB (Behavior fb) = Behavior (memo1 fb)


-- untilB :: Behavior a -> Event (Behavior a) -> Behavior a
-- Behavior fb `untilB` Event fe = 

-- switch :: Behavior a -> Event (Behavior a) -> Behavior a

