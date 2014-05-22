{-
	This Chat client relays messages to everything on the network using
	multicast UDP packets.
-}

import System.IO				-- For handles
import Network.Socket			-- For sockets
import Network.Mutlicast		-- For multicasting
import Control.Concurrent		-- For threads

main :: IO ()
main = do
	putStrLn "Starting multicast chat!"
