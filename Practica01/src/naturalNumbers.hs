data Nat = Zero | D Nat | O Nat deriving Show
         
-- toNat. Obtiene la representaci ́on en n ́umeros Nat de un n ́umero entero.
toNat :: Int -> Nat
toNat 0 = Zero
toNat a = if a `mod` 2 == 0
          then D (toNat (a `div` 2))
          else O (toNat ((a-1) `div` 2))    

-- succ. Obtiene el sucesor de un n ́umero Nat.
succN :: Nat -> Nat
succN Zero = O Zero
succN (D Zero) = O Zero
succN (O Zero) = D (O Zero)
succN (O (O x)) = D (succN (O x))
succN (O (D x)) = D (O x)
succN (D (O x)) = O (O x)
succN (D (D x)) = O (D x)

-- pred. Obtiene el predecesor de un n ́umero Nat.
predN :: Nat -> Nat
predN Zero = error "Zero is the smallest natural"
predN (D Zero) = error "Zero is the smallest natural"
predN (O Zero) = Zero
predN (D (D x)) = O (O (predN x))
predN (D (O x)) = O (D x)
predN (O (D x)) = D (D x)
predN (O (O x)) = D (O x)