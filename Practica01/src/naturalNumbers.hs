data Nat = Zero
         | D Nat
         | O Nat deriving Show
         
-- toNat. Obtiene la representaci ́on en n ́umeros Nat de un n ́umero entero.
toNat :: Int -> Nat
toNat 0 = Zero
toNat a = if a `mod` 2 == 0
          then D (toNat (a `div` 2))
          else O (toNat ((a-1) `div` 2))    

-- succ. Obtiene el sucesor de un n ́umero Nat.
succN :: Nat -> Nat
succN Zero = O Zero
succN (D x) = x
succN (O x) = x