{-# LANGUAGE ScopedTypeVariables #-}  -- Включаем некоторые расширения компилятора.
import Test.Tasty
import Test.Tasty.HUnit
import Data.Proxy
import Map
import qualified Data.Map.Strict as SMap
import MapInstance
import NaiveList(NaiveList)  -- Импортируем только тип NaiveList, но не его конструкторы Nil/Cons, чтобы не путались с конструкторами NaiveTree.
import NaiveTree

main :: IO ()
main = defaultMain testMap

{-|
  Генерирует группу тестов для конкретной реализации 'Map'
  с определённым именем.

  Мы хотим писать тесты один раз для всех возможных реализаций 'Map'.
  В чистом Haskell нам может помочь параметрический полиморфизм,
  но для этого нужно, чтобы в сигнатуре функции присутствовал
  тип из класса 'Map', который мы хотим протестировать.

  Специально для этих целей существует обёртка 'Data.Proxy', он
  позволяет передавать в функции даже типы высшего порядка.
-}
mapTests :: Map m => String -> Proxy m -> TestTree
mapTests name (_ :: Proxy m) =
    -- Чтобы можно было связать типовую переменную m здесь и в let ниже, нужно расширение ScopedTypeVariables.
    testGroup name [
        testGroup "Test fromList and toAscList" [
            testCase "fromList works on an empty map" $
                let map = fromList[] :: m Int String in
                Map.null map @?= True,

            testCase "fromList works on a non-empty map" $
                let map = fromList [(2, "a"), (1, "b"), (3, "c"), (1, "x")] :: m Int String in
                ( Map.size map     == 3        &&
                  Map.lookup 1 map == Just "x" &&
                  Map.lookup 2 map == Just "a" &&
                  Map.lookup 3 map == Just "c" ) @?= True,

            testCase "toAscList . fromList sorts list" $
                let map = fromList [(2, "a"), (1, "b"), (3, "c"), (1, "x")] :: m Int String in
                toAscList map @?= [(1, "x"), (2, "a"), (3, "c")]
        ],

        testGroup "Test insert" [
            testCase "insert works on an empty map" $
                let map  = empty :: m Int String in
                let map' = Map.insert 1 "One" map in
                Map.lookup 1 map' @?= Just "One",

            testCase "insert replaces value" $
                let map  = singleton 1 "One" :: m Int String in
                let map' = Map.insert 1 "New One" map in
                Map.lookup 1 map' @?= Just "New One"
        ],

        testGroup "Test insertWith" [
            testCase "insertWith works on an empty map" $
                let map  = empty :: m Int String in
                let map' = Map.insertWith (const $ const "wrong") 1 "One" map in
                Map.lookup 1 map' @?= Just "One",               

            testCase "insertWith replaces value" $
                let map  = singleton 1 "One" :: m Int String in
                let map' = Map.insertWith (++) 1 "New " map in
                Map.lookup 1 map' @?= Just "New One"
        ],

        testGroup "Test insertWithKey" [
            testCase "insertWithKey works on empty map" $
                let map  = empty :: m Int String in
                let map' = Map.insertWithKey (\key new_value old_value -> show key ++ new_value ++ old_value) 1 "One" map in
                Map.lookup 1 map' @?= Just "One",

            testCase "insertWithKey replaces value" $
                let map  = singleton 1 "One" :: m Int String in
                let map' = Map.insertWithKey (\key new_value old_value -> show key ++ new_value ++ old_value) 1 "New " map in
                Map.lookup 1 map' @?= Just "1New One"
        ],

        testGroup "Test delete" [
            testCase "delete works on an empty map" $
                let map  = empty :: m Int String in
                let map' = Map.delete 1 map in
                Map.null map' @?= True,

            testCase "delete works correctly if key exists" $
                let map  = Map.fromList [(1, "One"), (2, "Two")] :: m Int String in
                let map' = Map.delete 1 map in
                ( Map.size map'     == 1       &&
                  Map.lookup 1 map' == Nothing &&
                  Map.lookup 2 map' == Just "Two" ) @?= True,

            testCase "delete works correctly if key does not exist" $
                let map  = Map.fromList [(1, "One"), (2, "Two")] :: m Int String in
                let map' = Map.delete 3 map in
                ( Map.size map'     == 2          &&
                  Map.lookup 3 map' == Nothing    &&
                  Map.lookup 1 map' == Just "One" &&
                  Map.lookup 2 map' == Just "Two" ) @?= True
        ],

        testGroup "Test adjust" [
            testCase "adjust works on an empty map" $
                let map  = empty :: m Int String in
                let map' = Map.adjust ("New " ++) 1 map in
                Map.null map' @?= True,

            testCase "adjust works correctly if key exists" $
                let map  = singleton 1 "One" :: m Int String in
                let map' = Map.adjust ("New " ++) 1 map in
                ( Map.size map'     == 1 &&
                  Map.lookup 1 map' == Just "New One" ) @?= True,

            testCase "adjust works correctly if key does not exist" $
                let map  = singleton 1 "One" :: m Int String in
                let map' = Map.adjust ("New " ++) 2 map in
                ( Map.size map'     == 1       &&
                  Map.lookup 2 map' == Nothing &&
                  Map.lookup 1 map' == Just "One" ) @?= True
        ],

        testGroup "Test adjustWithKey" [
            testCase "adjustWithKey works on an empty map" $
                let map  = empty :: m Int String in
                let map' = Map.adjustWithKey (\key x -> show key ++ "New " ++ x) 1 map in
                Map.null map' @?= True,

            testCase "adjustWithKey works correctly if key exists" $
                let map  = singleton 1 "One" :: m Int String in
                let map' = Map.adjustWithKey (\key x -> show key ++ "New " ++ x) 1 map in
                ( Map.size map'     == 1 &&
                  Map.lookup 1 map' == Just "1New One" ) @?= True,

            testCase "adjustWithKey works correctly if key does not exist" $
                let map  = singleton 1 "One" :: m Int String in
                let map' = Map.adjustWithKey (\key x -> show key ++ "New " ++ x) 2 map in
                ( Map.size map'     == 1       &&
                  Map.lookup 2 map' == Nothing &&
                  Map.lookup 1 map' == Just "One" ) @?= True  
        ],

        testGroup "Test update" [
            testCase "update works on an empty map" $
                let map  = empty :: m Int String in
                let map' = Map.update (\x -> if x == "One" then Just "New One" else Nothing) 1 map in
                Map.null map' @?= True,

            testCase "update works correctly if key exists" $
                let map  = singleton 1 "One" :: m Int String in
                let map' = Map.update (\x -> if x == "One" then Just "New One" else Nothing) 1 map in
                ( Map.size map'     == 1 &&
                  Map.lookup 1 map' == Just "New One") @?= True,

            testCase "update works correctly if key does not exist" $
                let map  = singleton 1 "One" :: m Int String in
                let map' = Map.update (\x -> if x == "One" then Just "New One" else Nothing) 2 map in
                ( Map.size map'     == 1       &&
                  Map.lookup 2 map' == Nothing &&
                  Map.lookup 1 map' == Just "One" ) @?= True,

            testCase "update works correctly if function returns Nothing" $
                let map  = singleton 1 "not One" :: m Int String in
                let map' = Map.update (\x -> if x == "One" then Just "New One" else Nothing) 1 map in
                ( Map.size map'     == 0 &&
                  Map.lookup 1 map' == Nothing ) @?= True
        ],

        testGroup "Test updateWithKey" [
            testCase "updateWithKey works on an empty map" $
                let map  = empty :: m Int String in
                let map' = Map.updateWithKey (\k x -> if x == "One" then Just (show k ++ "New One") else Nothing) 1 map in
                Map.null map' @?= True,

            testCase "updateWithKey works correctly if the key exists" $
                let map  = singleton 1 "One" :: m Int String in
                let map' = Map.updateWithKey (\k x -> if x == "One" then Just (show k ++ "New One") else Nothing) 1 map in
                ( Map.size map'     == 1 &&
                  Map.lookup 1 map' == Just "1New One" ) @?= True,

            testCase "updateWithKey works correctly if key does not exist" $
                let map  = singleton 1 "One" :: m Int String in
                let map' = Map.updateWithKey (\k x -> if x == "One" then Just (show k ++ "New One") else Nothing) 2 map in
                ( Map.size map'     == 1       &&
                  Map.lookup 2 map' == Nothing &&
                  Map.lookup 1 map' == Just "One" ) @?= True,

            testCase "updateWithKey works correctly if function returns Nothing" $
                let map  = singleton 1 "not One" :: m Int String in
                let map' = Map.updateWithKey (\k x -> if x == "One" then Just (show k ++ "New One") else Nothing) 1 map in
                ( Map.size map'     == 0 &&
                  Map.lookup 1 map' == Nothing ) @?= True
        ],

        testGroup "Test alter and lookup" [
            testCase "alter insertion works on an empty map" $
                let map  = empty :: m Int String in
                let map' = Map.alter (const $ Just "One") 1 map in
                Map.lookup 1 map' @?= Just "One",

            testCase "alter insertion works on a singleton map" $
                let map  = singleton 2 "Two" :: m Int String in
                let map' = Map.alter (const $ Just "One") 1 map in
                ( Map.lookup 2 map' == Just "Two" &&
                  Map.lookup 1 map' == Just "One" ) @?= True,

            testCase "alter works on a singleton map" $
                let map  = singleton 1 "One" :: m Int String in
                let map' = Map.alter (const $ Just "One") 1 map in
                Map.lookup 1 map' @?= Just "One",

            testCase "alter deletion works correctly if element does exists" $
                let map  = singleton 1 "One" :: m Int String in
                let map' = Map.alter (const Nothing) 1 map in
                Map.lookup 1 map' @?= Nothing,

            testCase "alter deletion works correctly if element does not exist" $
                let map  = singleton 1 "One" :: m Int String in
                let map' = Map.alter (const Nothing) 2 map in
                ( Map.lookup 2 map' == Nothing &&
                  Map.lookup 1 map' == Just "One" ) @?= True

        ],

        testGroup "Test member" [
            testCase "member works on an empty map" $
                let map = empty :: m Int String in
                Map.member 1 map @?= False,

            testCase "member works correctly if key exists" $
                let map = singleton 1 "One" :: m Int String in
                Map.member 1 map @?= True,

            testCase "member works correctly if key does not exist" $
                let map = singleton 1 "One" :: m Int String in
                Map.member 2 map @?= False
        ],

        testGroup "Test notMember" [
            testCase "notMember works on an empty map" $
                let map = empty :: m Int String in
                Map.notMember 1 map @?= True,

            testCase "notMember works correctly if key exists" $
                let map = singleton 1 "One" :: m Int String in
                Map.notMember 1 map @?= False,

            testCase "notMember works correctly if key does not exist" $
                let map = singleton 1 "One" :: m Int String in
                Map.notMember 2 map @?= True
        ],

        testGroup "Test empty and singleton" [
            testCase "empty returns an empty map" $
                let map = empty :: m Int String in
                Map.null map @?= True,

            testCase "singleton returns a singleton map" $
                let map = singleton 1 "One" :: m Int String in
                Map.size map @?= 1
        ]
    ]

testNaiveTree :: TestTree
testNaiveTree = testGroup "Test NaiveTree" [
        testGroup "merge" [
            testCase "merge empty" $
                merge Nil Nil @?= (Nil :: NaiveTree () ())
            ,
            testCase "merge two nodes" $
                -- Ваша реализация может выдавать другое дерево, соответствующее
                -- последовательности 1, 2.
                merge (Node 1 "a" Nil Nil) (Node 2 "b" Nil Nil)
                    @?= Node 1 "a" Nil (Node 2 "b" Nil Nil)
        ]
    ]

testMap :: TestTree
testMap = testGroup "Testing implementations of trees"
    [
        mapTests "Data.Map.Strict" (Proxy :: Proxy SMap.Map),
        mapTests "NaiveList" (Proxy :: Proxy NaiveList),
        mapTests "NaiveTree" (Proxy :: Proxy NaiveTree),
        testNaiveTree
    ]
