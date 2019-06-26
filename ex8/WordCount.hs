-- Compile with: ghc --make WordCount.hs

module Main
where
import System.Environment

{--showln  = (++ "\n") . show

wc_l inp  = showln(length(lines inp))
wc_w inp = showln(length(words inp))
wc_c inp = showln(length(inp))--}

main :: IO ()
main = do
    args <- getArgs
    putStrLn "To be done..."--

{--main = interact wc
    where wc input =  wc_l input ++ wc_w input ++ wc_c input--}

-- We couldn't figure out a way to apply with args,hence we used the help of interact function.