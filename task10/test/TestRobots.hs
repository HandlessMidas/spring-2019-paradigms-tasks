import Test.Tasty
import Test.Tasty.HUnit

import Robots

main :: IO ()
main = defaultMain testsRobots

testsRobots :: TestTree
testsRobots = let
        walter = robot "Walter" 50 50
        cortana = robot "Cortana" 100 20
        r2d2 = robot "R2D2" 10 500
        jarvis = robot "Jarvis" 100 250
        ultron = robot "Ultron" 500 1000
        t1000 = robot "T1000" 400 1500
    in testGroup "Unit tests for Robots task"
        [ testCase "Test for getName" $
            getName walter @?= "Walter"
        , testCase "Test for printRobot" $
            printRobot walter @?= "Walter, attack: 50, health: 50"
        , testCase "Test for getAttack" $
            getAttack r2d2 @?= 10 
        , testCase "Test for getHealth" $
            getHealth ultron @?= 1000 
        , testCase "Test for setName" $
            setName "walter" walter @?= robot "walter" 50 50
        , testCase "Test for setAttack" $
            setAttack 55 walter @?= robot "Walter" 55 50
        , testCase "Test for setHealth" $
            setHealth 55 walter @?= robot "Walter" 50 55
        , testCase "Test damage with no fight" $
            damage ultron 10 @?= robot "Ultron" 500 990
        , testCase "Test isAlive" $
            isAlive r2d2 @?= True 
        , testCase "Test fight with death" $
            fight ultron jarvis @?= robot "Jarvis" 100 (250 - 500)
        , testCase "Test fight without death" $
            fight cortana r2d2 @?= robot "R2D2" 10 (500 - 100)
        , testCase "Test threeRoundFight attacker wins" $
            threeRoundFight t1000 ultron @?= robot "T1000" 400 1000
        , testCase "Test threeRoundFight defender wins" $
            threeRoundFight cortana ultron  @?= robot "Ultron" 500 900
        ]
