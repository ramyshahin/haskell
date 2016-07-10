-- Rock Paper Scissors 
import System.IO

data Move = Rock | Paper | Scissors 
    deriving (Show, Read, Eq)

data Outcome = Win | Lose | Tie
    deriving (Show)
    
parseMove :: String -> Maybe Move

parseMove mv0 = 
    case reads mv0 of
        [(mv,str)] -> if (ok str) then Just mv else Nothing
        _          -> Nothing
    where ok = all (`elem` " \n\r\t")
    
play :: Move -> Move -> Outcome

play Rock Scissors  = Win
play Scissors Paper = Win
play Paper Rock     = Win

play mv0 mv1 = if (mv0 == mv1) then Tie else Lose

-- computer versus user game (argument is computer's move)
computerVsUser :: Move -> IO ()

computerVsUser computerMove = 
    do
        putStrLn "Please enter one of [Rock,Paper,Scissors]"
        userMoveTxt <- getLine
        let mv0 = (parseMove userMoveTxt)
        case mv0 of
            Just mv -> putStrLn $ "You " ++ show (play mv computerMove)
            Nothing -> computerVsUser computerMove
        
    