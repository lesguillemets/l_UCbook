{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Data.List
import Control.Monad
import Control.Applicative
import Data.Maybe

data Rule s a = Rule { _state :: s, _char :: a, _nextState :: s }

instance (Show s, Show a) => Show (Rule s a) where
    show (Rule _s _c _n) = intercalate " -> " [show _s, show _c, show _n]

applies :: (Eq s, Eq a) => Rule s a -> s -> a -> Bool
applies (Rule st ch _) s' c' = st == s' && ch == c'

type DFARuleBook s a = [Rule s a]

nextState :: (Eq s, Eq a) => DFARuleBook s a -> s -> a -> Maybe s
nextState rb s a = _nextState <$> find (\r -> (r `applies`) s a) rb

data DFA s a = DFA {
             _currentState :: s,
             _acceptStates :: [s],
             _ruleBook :: DFARuleBook s a
             }

accepting :: Eq s => DFA s a -> Bool
accepting dfa = _currentState dfa `elem` _acceptStates dfa

-- TODO : Monadic
readChar :: (Eq s, Eq a) => DFA s a -> a -> Maybe (DFA s a)
readChar a@(DFA cs _ rb) c = let
    state' = nextState rb cs c
    in
        case state' of
            Nothing -> Nothing
            Just s -> Just $ a{ _currentState = s }

readString :: (Eq s, Eq a) => DFA s a -> [a] -> Maybe (DFA s a)
readString = foldM readChar

type IntState = Int
type Alph = Char

sampleRules :: DFARuleBook Int Char
sampleRules = map (uncurry3 Rule) [
    (1,'a',2),
    (1,'b',1),
    (2,'a',2),
    (2,'b',3),
    (3,'a',3),
    (3,'b',3)
    ]
sampleMachine :: DFA Int Char
sampleMachine = DFA 1 [3] sampleRules

main = do
    print $ nextState sampleRules 1 'a'
    print $ nextState sampleRules 1 'b'
    print $ nextState sampleRules 2 'b'
    print $ accepting sampleMachine
    let (m' :: DFA Int Char) = fromJust $  sampleMachine `readChar` 'b'
    print $ accepting m'
    print . accepting . fromJust . (`readChar` 'b') $ sampleMachine
    print . accepting . fromJust . (`readString` "baaab") $ sampleMachine
    

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c) = f a b c
