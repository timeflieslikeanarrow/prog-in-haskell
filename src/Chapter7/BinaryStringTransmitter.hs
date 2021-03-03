module Chapter7.BinaryStringTransmitter where

import Data.Char

import Chapter7.Base (unfold, count)

type Bit = Int

bin2int :: [Bit] -> Int
--bin2int bits = sum [w*b | (w, b) <- zip weights bits]
--                where weights = iterate (*2) 1

bin2int = foldr (\x y -> x + 2 * y) 0

int2bin :: Int -> [Bit]
--int2bin 0 = []
--int2bin n = n `mod` 2 : int2bin (n `div` 2)
int2bin = unfold (== 0) (`mod` 2) (`div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
--chop8 []    = []
--chop8 bits  = (take 8 bits) : chop8 (drop 8 bits)
chop8 = unfold (== []) (take 8) (drop 8) -- Exercise 6

decode :: [Bit] -> String
decode =  map (chr . bin2int) . chop8

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

--Exercise 7

encode' :: String -> [Bit]
encode' = concat . map (extendParityBit . make8 . int2bin . ord)

chop9 :: [Bit] -> [[Bit]]
chop9 = unfold (== []) (take 9) (drop 9)

decode' :: [Bit] -> String
decode' =  map (chr . bin2int . checkPartybitAndRestore) . chop9

transmit' :: String -> String
transmit' = decode' . channel' . encode'

channel' :: [Bit] -> [Bit]
channel' (b:bs) = b:bs --bs

onebitmod2 :: [Bit] -> Int
onebitmod2 bits = (count 1 bits) `mod` 2

extendParityBit :: [Bit] -> [Bit]
extendParityBit bs = (onebitmod2 bs) : bs

checkPartybitAndRestore :: [Bit] -> [Bit]
checkPartybitAndRestore (b:bs) = if (onebitmod2 bs == b) 
                                 then bs 
                                 else error "bits corrupted"