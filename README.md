csMultiChat
==========

Overview
--------
This is a server-less chat client, written in Haskell. It uses multicast packets to signal every machine on the network over UDP, thus negating the need for a server. Haskell was chosen for its great success with concurrent IO in the csChatServ project.

### Dependencies

This project requires Network.Socket and Network.Multicast, neither of which come with ghc.
