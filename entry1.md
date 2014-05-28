Journal Entry 1
===============

### Overall

A basic prototype for the chat program is up and running. Configuration is hard-coded in, and the UI is far from elegant, but messages are successfully passed between machines on the same network without a central server. Aside from picking up more Haskell this project is making me learn a lot more about networking, which I've broken into several sections below:

### Multicast

Originally I planned on doing this project with multicast packets, but I've now learned enough the see the folly of my ways. Multicast packets are designed to send a single packet to several hosts. This is great if you have the addresses of every participant in the chat. It is less useful when we want to communicate with the entire network. Which brings us to...

### Broadcast packets

This program is designed for a wifi network, which means any packet on the network will hit every wifi card in range, before ever touching a router. Unless in promiscuous mode, wifi cards are configured to ignore all packets not addressed to their IP, *or* the broadcast address. Therefore we can address packets to the broadcast address (10.0.0.255, 10.0.255.255, or somesuch, depending on the netmask), and every wifi card will read them. Better yet, the router will even bounce the packets back to hit anyone on a different AP or ethernet.

### UDP

The chat program is stateless, containing no central server and no list of other chat clients. Therefore the problem is best modeled by UDP, a stateless datagram, than a full TCP connection. That meant that I had to learn to use UDP, which is a bit of a strange beast. For sending it looks like I need to make a new UDP socket and connect for each packet I send. This is a lot more overhead for sending, but means we have no constantly open connections. Receiving is the opposite. Rather than binding to a listen socket and forking to receive each new client we bind and listen and handle clients all on one socket. Each packet arrives as a tuple of (message, _, address), and we fork to handle the individual message then.

### Todo

Most important is adding command line argument support, so the IP and broadcast addresses can be supplied at runtime rather than compile time. After that I'd like to add a better interface so user input isn't interrupted by incoming messages. The GUI libraries in Haskell aren't super mature, so I'm tempted to use curses instead.

### Bugs

Not all messages arrive. This isn't actually a bug, but a result of using UDP on a heavily congested network like the campus student wifi.
