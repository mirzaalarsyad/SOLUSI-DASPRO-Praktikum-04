module FilterList where

-- NIM / Nama : 16518189 / M Mirza Fathan Al Arsyad
-- Tanggal    : 5 Maret 2019
-- Topik      : Aspek fungsi sebagai parameter fungsi
-- Deskripsi  : Filter List

-- DEFINISI DAN SPESIFIKASI LIST
{- type List of Int: [ ] atau [e o List] atau [List o e]  
   Definisi type List of Int
   Basis: List of Int kosong adalah list of Int 
   Rekurens: 
   List tidak kosong dibuat dengan menambahkan sebuah elemen bertype Int di awal 
   sebuah list atau
   dibuat dengan menambahkan sebuah elemen bertype Int di akhir sebuah list -}

-- DEFINISI DAN SPESIFIKASI KONSTRUKTOR
konso :: Int -> [Int] -> [Int]
{- konso e li menghasilkan sebuah list of integer dari e (sebuah integer) dan li 
   (list of integer), dengan e sebagai elemen pertama: e o li -> li' -}
-- REALISASI
konso e li = [e] ++ li

konsDot :: [Int] -> Int -> [Int]
{- konsDot li e menghasilkan sebuah list of integer dari li (list of integer) dan 
   e (sebuah integer), dengan e sebagai elemen terakhir: li o e -> li' -}
-- REALISASI
konsDot li e = li ++ [e]

-- DEFINISI DAN SPESIFIKASI SELEKTOR
-- head :: [Int] -> Int
-- head l menghasilkan elemen pertama list l, l tidak kosong

-- tail :: [Int] -> [Int]
-- tail l menghasilkan list tanpa elemen pertama list l, l tidak kosong

-- last :: [Int] -> Int
-- last l menghasilkan elemen terakhir list l, l tidak kosong

-- init :: [Int] -> [Int]
-- init l menghasilkan list tanpa elemen terakhir list l, l tidak kosong

-- DEFINISI DAN SPESIFIKASI PREDIKAT
isEmpty :: [Int] -> Bool
-- isEmpty l  true jika list of integer l kosong
-- REALISASI
isEmpty l = null l

isOneElmt :: [Int] -> Bool
-- isOneElmt l true jika list of integer l hanya mempunyai satu elemen
-- REALISASI
isOneElmt l = (length l) == 1 


-- FILTER LIST                  filterList(l,f)

-- DEFINISI DAN SPESIFIKASI
filterList :: [Int] -> (Int -> Bool) -> [Int]
-- filter list dengan fungsi boolean
isPos :: Int -> Bool
-- isPos (x) true jika x > 0
isNeg :: Int -> Bool
-- isNeg (x) true jika x < 0
isEqual10 :: Int -> Bool
-- isEqual10(x) true jika x = 10  
isKabisat :: Int -> Bool
-- menghasilkan list baru yang hanya berisi elemen list yang masuk kategori tahun kabisat

-- REALISASI
isPos x = x > 0
isNeg x = x < 0
isEqual10 x = x == 10
isKabisat x | mod x 100 == 0 = (mod x 400 == 0)
            | otherwise = if (mod x 4 == 0) then True else False
filterList l f =
    if isEmpty l then [] --basis
    else if (f (head l)) then (konso (head l) (filterList (tail l) f)) --rekurens
         else (filterList (tail l) f)
