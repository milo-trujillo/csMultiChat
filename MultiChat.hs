{-
	This Chat client relays messages to everything on the network using
	broadcast UDP packets.
-}

import System.IO				-- For handles
import Network.Socket			-- For sockets
import Control.Concurrent		-- For threads

-- Global vars for configuration
port 		= "1234"			-- Port to send / receive broadcasts
destination = "10.0.0.255" 		-- Host to send on
hostname	= "10.0.0.1"		-- Host to listen on
packetSize	= 1024				-- Maximum size of UDP packets

main :: IO ()
main = do
	-- Convert destination string to an address
	(broadcast:_) <- getAddrInfo Nothing (Just destination) (Just port)
	(listen:_) <- getAddrInfo Nothing (Just hostname) (Just port)
	forkIO (networkToClient listen)	-- Listen for incoming connections
	clientToNetwork broadcast		-- Write to network

-- Returns a UDP socket connected to the broadcast address
getBroadcastSock :: AddrInfo -> IO Socket
getBroadcastSock addr = do
	s <- socket (addrFamily addr) Datagram defaultProtocol	-- Make new socket
	setSocketOption s Broadcast 1							-- Enable broadcasting
	connect s (addrAddress addr)							-- Connect to broadcast
	return s

-- Reads from the client and broadcasts messages to network
clientToNetwork :: AddrInfo -> IO ()
clientToNetwork addr = do
	sock <- getBroadcastSock addr
	msg <- getLine
	send sock msg
	close sock
	clientToNetwork addr

-- Reads from network and prints messages to client
networkToClient :: AddrInfo -> IO ()
networkToClient addr = do
	let portno = (read port :: Integer) -- Make port an Integer
	s <- socket (addrFamily addr) Datagram defaultProtocol	-- Make new socket
	bindSocket s (SockAddrInet (fromIntegral portno) iNADDR_ANY)
	listenLoop s
	where listenLoop sock = do
		(msg, _, addr) <- recvFrom sock packetSize -- Read a packet from network
		putStrLn msg
		listenLoop sock
