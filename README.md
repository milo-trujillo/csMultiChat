csMultiChat
==========

Overview
--------
This is a server-less chat client, written in Haskell. It uses the broadcast address to signal every machine on the network over UDP, thus negating the need for a server. Haskell was chosen for its great success with concurrent IO and networking in the csChatServ project.

### Dependencies

This project requires Network.Socket, System.IO, and Control.Concurrent. It also requires GHC and a Unix system.
