module Lib where

import Control.Arrow (returnA, (>>>), Arrow(second, arr, first))
import Control.Category ((>>>), Category(..))
import Prelude hiding (id, (.))
import Data.Char (toLower)

newtype Circuito a b = Porta { valuta :: a -> b }
type Porta = Circuito

instance Category Circuito where
    id = Porta id
    (Porta f) . (Porta g) = Porta (f . g)

instance Arrow Circuito where
    arr f = Porta f
    first (Porta f) = Porta (mapPrm f)
        where mapPrm g (a, b) = (g a, b)

nandGate :: Porta (Bool, Bool) Bool
nandGate = arr (\(x, y) -> not (x && y))

notGate :: Porta Bool Bool
notGate = arr (\x -> (x, x)) >>> nandGate

andGate :: Porta (Bool, Bool) Bool
andGate = nandGate >>> notGate

orGate :: Porta (Bool, Bool) Bool
orGate = first notGate >>> second notGate >>> nandGate

norGate :: Porta (Bool, Bool) Bool
norGate = orGate >>> notGate

xorGate :: Porta (Bool, Bool) Bool
xorGate = proc (a, b) -> do
    c   <- nandGate -< (a, b)
    o'  <- nandGate -< (c, a)
    o'' <- nandGate -< (c, b)
    nandGate -< (o', o'')

xnorGate :: Porta (Bool, Bool) Bool
xnorGate = xorGate >>> notGate


------- SETTE SEGMENTI

aSeg :: Circuito (Bool, Bool, Bool, Bool) Bool
aSeg = proc (a, b, c, d) -> do
    aN <- notGate -< a
    bN <- notGate -< b
    cN <- notGate -< c
    dN <- notGate -< d

    alpha <- nor4 -< (a,  b,  c,  dN)
    beta  <- nor4 -< (a,  bN, c,  d)
    gamma <- nor4 -< (aN, b,  cN, dN)
    delta <- nor4 -< (aN, bN, c,  dN)

    nor4 -< (alpha, beta, gamma, delta)

    where
        nor4 = proc (a, b, c, d) -> do
            ichi <- orGate -< (a, b)
            ni   <- orGate -< (c, d)

            norGate -< (ichi, ni)

bSeg :: Circuito (Bool, Bool, Bool, Bool) Bool
bSeg = proc (a, b, c, d) -> do
    aN <- notGate -< a
    bN <- notGate -< b
    cN <- notGate -< c
    dN <- notGate -< d

    alpha   <- nor4 -< (a,  bN, c,  dN)
    beta    <- nor4 -< (a,  bN, cN, d)
    gamma   <- nor4 -< (aN, b,  cN, dN)
    delta   <- nor4 -< (aN, bN, c,  d)
    epsilon <- nor4 -< (aN, bN, cN, d)
    zeta    <- nor4 -< (aN, bN, cN, dN)

    nor6 -< (alpha, beta, gamma, delta, epsilon, zeta)

    where
        nor4 = proc (a, b, c, d) -> do
            ichi <- orGate -< (a, b)
            ni   <- orGate -< (c, d)

            norGate -< (ichi, ni)

        nor6 = proc (a, b, c, d, e, f) -> do
            ichi <- orGate -< (a, b)
            ni   <- orGate -< (c, d)
            san  <- orGate -< (e, f)
            shi  <- orGate -< (ichi, ni)

            norGate -< (san, shi)

cSeg :: Circuito (Bool, Bool, Bool, Bool) Bool
cSeg = proc (a, b, c, d) -> do
    aN <- notGate -< a
    bN <- notGate -< b
    cN <- notGate -< c
    dN <- notGate -< d

    alpha <- nor4 -< (a,  b,  cN, d)
    beta  <- nor4 -< (aN, bN, c,  d)
    gamma <- nor4 -< (aN, bN, cN, d)
    delta <- nor4 -< (aN, bN, cN, dN)

    nor4 -< (alpha, beta, gamma, delta)

    where
        nor4 = proc (a, b, c, d) -> do
            ichi <- orGate -< (a, b)
            ni   <- orGate -< (c, d)

            norGate -< (ichi, ni)

dSeg :: Circuito (Bool, Bool, Bool, Bool) Bool
dSeg = proc (a, b, c, d) -> do
    aN <- notGate -< a
    bN <- notGate -< b
    cN <- notGate -< c
    dN <- notGate -< d

    alpha   <- nor4 -< (a,  b,  c,  dN)
    beta    <- nor4 -< (a,  bN, c,  d)
    gamma   <- nor4 -< (a,  bN, cN, dN)
    delta   <- nor4 -< (aN, b,  cN, d)
    epsilon <- nor4 -< (aN, bN, cN, dN)

    nor5 -< (alpha, beta, gamma, delta, epsilon)

    where
        nor4 = proc (a, b, c, d) -> do
            ichi <- orGate -< (a, b)
            ni   <- orGate -< (c, d)

            norGate -< (ichi, ni)

        nor5 = proc (a, b, c, d, e) -> do
            ichi <- orGate -< (a, b)
            ni   <- orGate -< (c, d)
            san  <- orGate -< (ichi, e)

            norGate -< (ni, san)

eSeg :: Circuito (Bool, Bool, Bool, Bool) Bool
eSeg = proc (a, b, c, d) -> do
    aN <- notGate -< a
    bN <- notGate -< b
    cN <- notGate -< c
    dN <- notGate -< d

    alpha   <- nor4 -< (a,  b,  c,  dN)
    beta    <- nor4 -< (a,  b,  cN, dN)
    gamma   <- nor4 -< (a,  bN, c,  d)
    delta   <- nor4 -< (a,  bN, c,  dN)
    epsilon <- nor4 -< (a,  bN, cN, dN)
    zeta    <- nor4 -< (aN, b,  c,  dN)

    nor6 -< (alpha, beta, gamma, delta, epsilon, zeta)

    where
        nor4 = proc (a, b, c, d) -> do
            ichi <- orGate -< (a, b)
            ni   <- orGate -< (c, d)

            norGate -< (ichi, ni)

        nor6 = proc (a, b, c, d, e, f) -> do
            ichi <- orGate -< (a, b)
            ni   <- orGate -< (c, d)
            san  <- orGate -< (e, f)
            shi  <- orGate -< (ichi, ni)

            norGate -< (san, shi)

fSeg :: Circuito (Bool, Bool, Bool, Bool) Bool
fSeg = proc (a, b, c, d) -> do
    aN <- notGate -< a
    bN <- notGate -< b
    cN <- notGate -< c
    dN <- notGate -< d

    alpha   <- nor4 -< (a,  b,  c,  dN)
    beta    <- nor4 -< (a,  b,  cN, d)
    gamma   <- nor4 -< (a,  b,  cN, dN)
    delta   <- nor4 -< (a,  bN, cN, dN)
    epsilon <- nor4 -< (aN, bN, c,  dN)

    nor5 -< (alpha, beta, gamma, delta, epsilon)

    where
        nor4 = proc (a, b, c, d) -> do
            ichi <- orGate -< (a, b)
            ni   <- orGate -< (c, d)

            norGate -< (ichi, ni)

        nor5 = proc (a, b, c, d, e) -> do
            ichi <- orGate -< (a, b)
            ni   <- orGate -< (c, d)
            san  <- orGate -< (ichi, e)

            norGate -< (ni, san)

gSeg :: Circuito (Bool, Bool, Bool, Bool) Bool
gSeg = proc (a, b, c, d) -> do
    aN <- notGate -< a
    bN <- notGate -< b
    cN <- notGate -< c
    dN <- notGate -< d

    alpha <- nor4 -< (a,  b,  c,  d)
    beta  <- nor4 -< (a,  b,  c,  dN)
    gamma <- nor4 -< (a,  bN, cN, dN)
    delta <- nor4 -< (aN, bN, c,  d)

    nor4 -< (alpha, beta, gamma, delta)

    where
        nor4 = proc (a, b, c, d) -> do
            ichi <- orGate -< (a, b)
            ni   <- orGate -< (c, d)

            norGate -< (ichi, ni)

display :: Circuito (Bool, Bool, Bool, Bool) (Bool, Bool, Bool, Bool, Bool, Bool, Bool)
display = proc x -> do
    a <- aSeg -< x
    b <- bSeg -< x
    c <- cSeg -< x
    d <- dSeg -< x
    e <- eSeg -< x
    f <- fSeg -< x
    g <- gSeg -< x

    returnA -< (a, b, c, d, e, f, g)

stampaDisplay :: (Bool, Bool, Bool, Bool) -> IO ()
stampaDisplay x = do
    let (a, b, c, d, e, f, g) = valuta display x

    if a then putStrLn " __ " else putStrLn "    "
    if f then putStr   "|"    else putStr   " "
    if g then putStr   "__"   else putStr   "  "
    if b then putStrLn "|"    else putStrLn " "
    if e then putStr   "|"    else putStr   " "
    if d then putStr   "__"   else putStr   "  "
    if c then putStrLn "|"    else putStrLn " "

    putStrLn ""

stampaDisplay' :: Char -> IO ()
stampaDisplay' x
    | x' == '0' = stampaDisplay (False, False, False, False)
    | x' == '1' = stampaDisplay (False, False, False, True)
    | x' == '2' = stampaDisplay (False, False, True,  False)
    | x' == '3' = stampaDisplay (False, False, True,  True)
    | x' == '4' = stampaDisplay (False, True,  False, False)
    | x' == '5' = stampaDisplay (False, True,  False, True)
    | x' == '6' = stampaDisplay (False, True,  True,  False)
    | x' == '7' = stampaDisplay (False, True,  True,  True)
    | x' == '8' = stampaDisplay (True,  False, False, False)
    | x' == '9' = stampaDisplay (True,  False, False, True)
    | x' == 'a' = stampaDisplay (True,  False, True,  False)
    | x' == 'b' = stampaDisplay (True,  False, True,  True)
    | x' == 'c' = stampaDisplay (True,  True,  False, False)
    | x' == 'd' = stampaDisplay (True,  True,  False, True)
    | x' == 'e' = stampaDisplay (True,  True,  True,  False)
    | x' == 'f' = stampaDisplay (True,  True,  True,  True)
    | otherwise = pikachu
    where
        x' = toLower x
        pikachu = do
            putStrLn ""
            putStrLn ",*************,,*/(((((//,,*(#%%%%%%%%%%%%%%%#(*,,,****************************************************,*/(((((((((/((((////****/((##%%%%%%"
            putStrLn ",*************,,//((((((//,,*(%%%%%%%%%%%%%%%%%##/*****************************************************,,*/(///(//////****//((##%%%%%%%%%%%"
            putStrLn ",************,,*/(((((((//***/#%%%%%%%%%%%%%%%%%%%#(/***************************************************,*//////////*//((#%%%%%%%%%%%%%%%%%"
            putStrLn ",***********,,*////////////***/##%%%%%%%%%%%%%%%%%%%##(*,***********************************************,,*////////(###%%%%%%%%%%%%%%%%%%%%"
            putStrLn ",**********,,,*/*******//////**/(#%%%%%%%%%%%%%%%%%%%%%#(/**********************************************,,,***/(##%%%%%%%%%%%%%%%%%%%%%%%%%"
            putStrLn ",*********,,,,*************///***/(#%%%%%%%%%%%%%%%%%%%%%%#(/***********************************,****,****/((#%%%%%%%%%%%%%%%%%%%%%%%%%%%%#"
            putStrLn ",*********,,,***************//****/(##%%%%%%%%%%%%%%%%%%%%%%##//**************//////////////////////((#####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#("
            putStrLn ",********,,,,***********************/(#%%%%%%%%%%%%%%%%%%%%%%%##################%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%##(/"
            putStrLn ",*******,..,***********************,,*/##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%###((//"
            putStrLn ",*******,.,,***********************,,,,*(#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%##(//**//"
            putStrLn ",******,.,,,************************,,,,*/(#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#(//*******"
            putStrLn ",*****,,,,,********,***,,,,,,,,,,,,*,,,,,,*/(######%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%##(/**********"
            putStrLn ",*****,..,*******,,,,,,,,,,,,,,,,,,,,,,*,,,,*///((#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%###(/************"
            putStrLn ",*****,,,*******,,,,,*,,,,,,,,,,,,,,,,,****,,,*/(#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#######(//**************"
            putStrLn ",****,.,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,**,,,/(%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#((//******************"
            putStrLn ",***,..,,,,,,,,,,,,,,,,,,,,,,,,,,,,,..,,,,,,,*(#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#(/*******************"
            putStrLn ",**,,.,,,,,,,,,,,,,,,,,,,,,,,,,,.......,,,,,,/#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#####%%%%%%%%%%%%%%%%#(/******************"
            putStrLn ",**,..,,,,,,,,,,,,,,,,,,,,,,,,,......,,,*,,,*(#%%%%%%%%##(((/(##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%##(((/*/((#%%%%%%%%%%%%%%#(/*****************"
            putStrLn ",*,..,,,,,,,,,,,,,,,,,,,,,,,,,,,.....,,**,,*/#%%%%%%%##((((*,**/#%%%%%%%%%%%%%%%%%%%%%%%%%%%%##((##/,,,*(#%%%%%%%%%%%%%%#(*****************"
            putStrLn ".*,.,,,**,,,,,,,,,,,,,,,,,,,,,,,,,,*****,,,/(%%%%%%%%#(//(#/,..*/#%%%%%%%%%%%%%%%%%%%%%%%%%%%#(//(#/,..,/(#%%%%%%%%%%%%%%#/*****///////////"
            putStrLn ".,..,,,,,,,,,,,,,,,,,,,,,,,,,,*,,*******,,,(#%%%%%%%%#(*,,,....,/#%%%%%%%%%%%%%%%%%%%%%%%%%%%#(*,,,....,/(#%%%%%%%%%%%%%%#(*,**////////////"
            putStrLn ".,..,,,,,,,,,...........,,,,,,*,********,,*(#%%%%%%%%%#(/*,,...,/#%%%%%%%%%%%%%%%%%%%%%%%%%%%%#(/*,,..,*/##%%%%%%%%%%%%%%%#(***////////////"
            putStrLn "...,,,,,,,................,,*,**********,,/#%%%%%%%%%%%%#((////((#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%##((///(#%%%%%%%%%%%%%%%%%%(/**////////////"
            putStrLn " ..,,,,,,.................,,,**********,,*(#%%%%%%%%%%%%%%%%%%#%%%%%%%%#((///((#%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%#/**////////////"
            putStrLn ".,,,,,,,,.................,,***********,,/(####%%%%%%%%%%%%%%%%%%%%%%%%#(/*,,,*(#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#(/*////////////"
            putStrLn ".,***,,,,,,..............,,,**********,..,***//((##%%%%%%%%%%%%%%%%%%%%%%%##((##%%%%%%%%%%%%%%%%%%%%%%%%%##(((((((((###%%%%%#/**///////////"
            putStrLn ".*****,,,,,,,,,,,,,,,,,,,*************,..,*******/(#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%##///*//////((#%%%%%#(**///////////"
            putStrLn ".****************/******/***////*****,.,*///////**/#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#(////////////(#%%%%%#/**//////////"
            putStrLn ".***********************/////*******,..,*//////////(#%%%%%%%%%%%%%%%%%%%%##########%%%%%%%%%%%%%%%%%%%%#(///////////*/(#%%%%%#(***/////////"
            putStrLn ".************************///********,..,*//////////#%%%%%%%%%%%%%%%%%%#(//*****///(((##%%%%%%%%%%%%%%%%#(///////////**/##%%%%##/***////////"
            putStrLn ".***********************************,.,,***///////(#%%%%%%%%%%%%%%%%#(/*,,,*//((((////(#%%%%%%%%%%%%%%%#((////////////(#%%%%%%#(*********//"
            putStrLn ",***********,,,*,,*,,**************,,,*//******//(#%%%%%%%%%%%%%%%%%#(*,,*/(((#####(((((#%%%%%%%%%%%%%%%##///////////(#%%%%%%%%#(***///////"
            putStrLn ",*************,,**,,,************,,,,,/(##((((####%%%%%%%%%%%%%%%%%%%(/**/(((#((((#((//(#%%%%%%%%%%%%%%%%%#(((((((((##%%%%%%%%%%#/**///////"
            putStrLn ",******************************,,,,,,,*(#%#%%%%%%%%%%%%%%%%%%%%%%%%%%#(**/((#(#(((#((//(#%%%%%%%%%%%%%%%%%%%%%%%#%#%%%%%%%%%%%%%#(**///////"
            putStrLn ",*************,**************,****,,,,,/(#%%%%%%%%%%%%%%%%%%%%%%%%%%%%#(/*/((((#((((///(#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%(/*///////"
            putStrLn ",*************************************,*/#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%##(////////////(#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#/**/////*"
            putStrLn ",******////****///////////////////////***/#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%####(((((((###%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#(********"
            putStrLn ".,*,****///////////////////////////////***/#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#(/*******"
            putStrLn ".,,,,*****//////////////////////////*******(#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%##(*******"
            putStrLn ".,,,,,,***********/////////////////********/(#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%(*******"
            putStrLn ""