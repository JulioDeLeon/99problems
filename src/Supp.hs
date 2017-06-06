module Supp ( myLast
              , myLastButOne
              , elementAt
              , myLength
              , myReverse
              , isPalindrome
              , myCompress
              , myPack
              , myEncode
              , myEncode'
              , myDecode
              , myEncodeDirect ) where

-- 1. last element in list (without last')
myLast :: [a] -> a
myLast [] = error "Empty List"
myLast [x] = x
myLast (h:tail) = myLast tail

-- 2. Find the last but One element, second to last
myLastButOne :: [a] -> a
myLastButOne [] = error "Empty List"
myLastButOne [a] = error "List too short"
myLastButOne [m,n] = m
myLastButOne (x:xs) = myLastButOne xs

-- 3. Find the kth element in a list, handles infinite list
elementAt :: Int -> [a] -> a
elementAt _ [] = error "Empty List or index out of range"
elementAt 0 (x:_) = x
elementAt i (x:xs) = elementAt (i-1) xs

-- 4. Find the number of elements of a list, what about infinite?
myLength :: [a] -> Int
myLength [] = 0
myLength [a] = 1
myLength (x:xs) = 1 + myLength xs

-- 5. Reverse a list (without reverse)
myReverse :: [a] -> [a]
myReverse lst = foldr (\ elem acc -> acc ++ [elem]) [] lst


-- 6. Find out if a list is a palindrome
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs =
  let
    ys = reverse xs
    len = (length xs) `div` 2
    zipped = zip (take len xs) (take len ys)
  in
    foldr (\x acc -> (fst x == snd x)) True zipped

-- 7. Flatten a nested list structure
data NestedList a = Elem a | List [NestedList a]

myFlatten :: NestedList a -> [a]
myFlatten (Elem a) = [a]
myFlatten (List lst) = myFlatten (head lst) ++ myFlatten (List $ tail lst)

-- 8. Eliminate consecutive duplicates of list elements
myCompress :: Eq a => [a] -> [a]
myCompress [] = []
myCompress lst = foldr (\elem acc -> if (head acc) == elem then acc else elem:acc) [last lst] lst

-- 9. Pack consecutive elements in list
myPack :: Eq a => [a] -> [[a]]
myPack [] = []
myPack lst = foldr (\x acc ->
                      if length acc == 0 then
                        [[x]]
                      else
                        let
                          pElem = last $ head acc
                        in
                          if pElem == x then
                            (x:(head acc)):(tail acc)
                          else
                            [[x]] ++ acc) [] lst

-- 10. Encode a list, run-length. Count and pair consecutive elements in list
myEncode :: Eq a => [a] -> [(Int, a)]
myEncode [] = []
myEncode lst = foldr (\x acc -> (length x, head x):acc) [] $ myPack lst

-- 11. Modify encode to not put singular items in tuples
data Dup a = Sing a | Dub (Int, a) deriving (Show)
myEncode' :: Eq a => [a] -> [Dup a]
myEncode' = foldr (\x  acc ->
                     if length x == 1 then
                       (Sing (head x)):acc
                     else
                       (Dub (length x, head x)):acc) [] . myPack

-- 12. Decode the encode list from 11
myDecode :: Eq a => [Dup a] -> [a]
myDecode lst = foldl f [] lst
  where f acc (Sing x) = acc ++ [x]
        f acc (Dub (n, x)) = acc ++ (take n $ repeat x)

-- 13. Encode direct (dont create sublist when encoding the list), Dont use myPack with creates sublist underneath
myEncodeDirect :: Eq a => [a] -> [Dup a]
myEncodeDirect lst = foldl f [] lst
  where
    eval (Sing a) b = if a == b then (Dub (2, a)) else (Sing b)
    eval (Dub (n, a)) b = if a == b then (Dub (n+1, a)) else (Sing b)
    same (Sing a) b = a == b
    same (Dub (n, a)) b = a == b
    f acc x =
      if length acc == 0 then
        [(Sing x)]
      else
        let
          l = last acc
          s = same l x
        in
          if s then
            (init acc) ++ [(eval l x)]
          else
            acc ++ [(eval l x)]
