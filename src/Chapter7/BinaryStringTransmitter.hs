module Chapter7.BinaryStringTransmitter where

import Data.Char

import Chapter7.Base

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