data ListS a = NilS | Snoc ( ListS a ) a deriving Show

-- 1. headS. Obtiene el primer elemento de la lista.
headS :: ListS a -> a
headS NilS = error "Empty list"
headS (Snoc NilS a) = a
headS (Snoc x _) = headS x 

-- 2. tailS. Obtiene la lista sin el primer elemento.
tailS :: ListS a -> ListS a
tailS NilS = error "Empty list"
tailS (Snoc NilS a) = NilS
tailS (Snoc x a) = Snoc (tailS x) a

-- 3. initS. Obtiene la lista sin el ultimo elemento.
initS :: ListS a -> ListS a
initS NilS = error "Empty list"
initS (Snoc x _) = x 

-- 4. lastS. Obtiene el ultimo elemento de la lista.
lastS :: ListS a -> a
lastS NilS = error "Empty list"
lastS (Snoc _ a) = a

-- 5. nthElementS. Obtiene el n- esimo elemento de la lista. TODO:
nthElementS :: Int -> ListS a -> a
nthElementS _ NilS = error "Invalid index"
nthElementS 0 (Snoc _ a) = a
nthElementS n (Snoc x _) = if n < 0 
                           then error "Invalid index"
                           else nthElementS (n-1) x

-- 6. deleteNthElementS. Elimina el n- ́esimo elemento de la lista. TODO:
deleteNthElementS :: Int -> ListS a -> ListS a
deleteNthElementS _ x = error "Not implemented"

-- 7. addFirstS. Obtiene la lista donde el primer elemento es el elemento dado.
addFirstS :: a -> ListS a -> ListS a
addFirstS a NilS = Snoc NilS a
addFirstS a (Snoc x b) = Snoc (addFirstS a x) b 

-- 8. addLastS. Obtiene la lista donde el ultimo elemento es el elemento dado.
addLastS :: a -> ListS a -> ListS a
addLastS a x = Snoc x a

-- 9. reverseS. Obtiene la reversa de la lista. TODO:
reverseS :: ListS a -> ListS a
reverseS NilS = NilS
reverseS (Snoc NilS a) = (Snoc NilS a)
reverseS (Snoc x a) = addFirstS a (reverseS x)

-- 10. appendS. Obtiene la concatenacion de dos listas.
appendS :: ListS a -> ListS a -> ListS a
appendS x NilS = x
appendS x l = appendS (Snoc x (headS l)) (tailS l)

-- 11. takeS. Obtiene la lista con los primeros n elementos.
takeS :: Int -> ListS a -> ListS a
takeS _ NilS = NilS
takeS 0 x = NilS
takeS _ (Snoc NilS a) = Snoc NilS a
takeS n (Snoc x a) = if n < 0
                     then error "Invalid index"
                     else addFirstS (headS x) (takeS (n-1) (tailS (Snoc x a)))
