
import System.Environment
import Data.List 

showln  = (++ "\n") . show

wc_l inp  = showln(length(lines inp))
wc_w inp = showln(length(words inp))
wc_c inp = showln(length(inp))


main :: IO ()
main = interact wc
    where wc input =  wc_l input ++ wc_w input ++ wc_c input



