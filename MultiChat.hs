{-
	This Chat client relays messages to everything on the network using
	broadcast UDP packets.
-}

import System.IO				-- For handles
import System.Environment		-- For getArgs
import Network.Socket			-- For sockets
import Control.Concurrent		-- For threads

-- Global vars for configuration
port 		= "1234"			-- Port to send / receive broadcasts
packetSize	= 1024				-- Maximum size of UDP packets
-- Note: Global config is hardcoded to ensure all clients can talk to eachother

main :: IO ()
main = do
	args <- getArgs
	if length args == 3
	then
		do
		let hostname = args !! 0		-- Host to listen on
		let destination = args !! 1		-- Broadcast address to send to
		let username = args !! 2		-- Username to send
		-- Convert destination string to an address
		(broadcast:_) <- getAddrInfo Nothing (Just destination) (Just port)
		(listen:_) <- getAddrInfo Nothing (Just hostname) (Just port)
		forkIO (networkToClient listen)			-- Read from network
		clientToNetwork broadcast username		-- Write to network
	else
		putStrLn "Usage: MultiChat <Listen IP> <Broadcast address> <Username>"

-- Returns a UDP socket connected to the broadcast address
getBroadcastSock :: AddrInfo -> IO Socket
getBroadcastSock addr = do
	s <- socket (addrFamily addr) Datagram defaultProtocol	-- Make new socket
	setSocketOption s Broadcast 1							-- Enable broadcast
	connect s (addrAddress addr)							-- Connect broadcast
	return s

-- Reads from the client and broadcasts messages to network
clientToNetwork :: AddrInfo -> String -> IO ()
clientToNetwork addr username = do
	sock <- getBroadcastSock addr
	msg <- getLine
	send sock ("<" ++ username ++ "> " ++ msg)
	close sock
	clientToNetwork addr username

-- Reads from network and prints messages to client
networkToClient :: AddrInfo -> IO ()
networkToClient addr = do
	let portno = (read port :: Integer) -- Make port an Integer
	s <- socket (addrFamily addr) Datagram defaultProtocol	-- Make socket
	bindSocket s (SockAddrInet (fromIntegral portno) iNADDR_ANY)
	listenLoop s
	where listenLoop sock = do
		(msg, _, addr) <- recvFrom sock packetSize -- Read a packet from network
		putStrLn ("[" ++ (getSockAddr addr) ++ "] " ++ msg)
		listenLoop sock

-- Gets the IP a socket is connected to
getSockAddr :: SockAddr -> String
getSockAddr addr = splitComma (show addr)
	where splitComma (x:xs)
		| (length xs == 0) = ""
		| ([x] == ":") = ""
		| otherwise = [x] ++ (splitComma xs)
