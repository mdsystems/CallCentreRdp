# CallCentreRdp

A small project group for some small applications that make use of RDP virtual channels to provide a link between a user's desktop and a call centre application running on an RDP server. All source code compiles with Delphi 10.2.

This set of small applications was written to support a call centre interface that ran via Microsoft Terminal Services (it was actually ThinStuff's XP/VS server, which provides similar facilities at a much lower cost). The applications make use of a DLL that is loaded by terminal services when an RDP connection is made and a small application runs on the user's desktop. This interacts using virtual channels.

The messaging interface is relatively simple. It has been relatively well tested but was never used in production, due to changing client requirements.

If anyone finds this repository and wants to use the code, feel free. If changes are made, please do create pull requests and we will attempt to maintain the suite of programs in case they are of any use to other developers.
