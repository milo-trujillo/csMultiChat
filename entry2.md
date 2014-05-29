Journal Entry 2
===============

### Overall

Config is now read from command line arguments, and the IP of the client is displayed next to all received messages. First version of the client is basically done.

### Config

The IP address, broadcast address, and username for the client are all sent as command line arguments. Ideally I'd like to just give the name of a wifi card and have the client figure out the appropriate addresses, but I can't find an appropriate library for Haskell. Converting the arguments to IPs and ports was trivial after the last project.

### IP addresses

After some testing I realized conversations were very difficult to follow if two clients had the same username, or if someone repeatedly changes their username. I now prepend the IP address a message came from to the message itself, hopefully improving conversation quality.
