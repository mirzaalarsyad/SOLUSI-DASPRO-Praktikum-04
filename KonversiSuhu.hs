module KonversiSuhu where

-- NIM / Nama : 16518189 / M Mirza Fathan Al Arsyad
-- Tanggal    : 5 Maret 2019
-- Topik      : Aspek fungsi sebagai parameter fungsi
-- Deskripsi  : Konversi Suhu

-- DEFINISI DAN SPESIFIKASI
konversiSuhu :: Float -> (Float -> Float) -> Float
{- KonversiSuhu(t,f) dengan T merepresentasikan suhu dalam  derajat tertentu dan fungsi f yang merupakan
fungsi konversi suhu, menghasilkan konversi t ke suhu pada derajat tertentu berdasarkan fungsi f. -}
konversiCtoK :: Float -> Float
-- menghasilkan konversi t dalam derajat Celcius ke derajat Kelvin dengan rumus: t + 273.15
konversiRtoC :: Float -> Float
-- konversi dari reamur ke celcius

--REALISASI
konversiCtoK t = t + 273.15
konversiRtoC t = 1.25 * t
konversiSuhu t f = (f t)