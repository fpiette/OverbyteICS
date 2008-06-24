{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  TWSocket class encapsulate the Windows Socket paradigm
Creation:     April 1996
Version:      6.08
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1996-2007 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@overbyte.be>
              SSL implementation includes code written by Arno Garrels,
              Berlin, Germany, contact: <arno.garrels@gmx.de>

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.

History:
If not otherwise noted, changes are by Francois Piette
Jul 18, 1996  Move all low level socket to winsock to be Delphi 2.x compatible
Sep 18, 1996  Use structured exception for handling errors
Sep 19, 1996  Check csDestroying before invoking event handler
Nov 04, 1996  Better error handling
Jan 31, 1997  Changed property assignation for Addr, Port and Proto
              Added notification handler
Feb 14, 1997  Corrected bug in property assignation for Addr, Port and Proto
Mar 26, 1997  Make UDP protocol work correctly
              Enable UDP broadcasting by using addr 255.255.255.255
Apr 1, 1997   Added class function when independent of any open socket
              Moved InitData as global
              Added ReceivedFrom function
              Added ResolveHost function
Jul 22, 1997  Adapted to Delphi 3 which has a modified winsock.accept
Aug 13, 1997  'sin' member made public
Aug 24, 1997  Create the only help
              Makes writing HSocket the same as calling Dup.
Sep 5, 1997   Version 2.01, added WinsockInfo function
Sep 21, 1997  Version 2.02, make it really thread safe
                            created global WSocketVersion
Sep 25, 1997  Version 2.04, port to C++Builder
Sep 27, 1997  Version 2.05. All class methods converted to global
              procedure or function because C++Builder do not like
              class method very much.
              Old class method              New global function
              ----------------              -------------------
              WinsockInfo                   WinsockInfo
              SocketErrorDesc               WSocketErrorDesc
              GetHostByAddr                 WSocketGetHostByAddr
              GetHostByName                 WSocketGetHostByName
              ResolveHost                   WSocketResolveHost
              HostName                      LocalHostName
Oct 02, 1997  V2.06 Added a check in destructor to avoid calling WSACleanup at
              design time which crashes the excellent Eagle Software CDK.
Oct 16, 1997  V2.07 Added PortNum property with numeric value for Port.
              Added RcvdCount property to return the number of
              characters received in the buffer but not read yet. Do not
              confuse with ReadCount which returns the number of chars
              already received.
              Added a check for FWait assignation in front of ReadLine
              Prefixed each TSocketState value by 'ws' to avoid name conflict.
              Moved FHSocket member to private section because the property
              HSocket does the right job.
              Added a check for state closed when changing Port, Proto and Addr.
Oct 22, 1997  V2.08 Added Flush method (asked by john@nexnix.co.uk) and
              FlushTimeout property (default to 60 seconds).
Oct 22, 1997  V2.09 Added SendFlags property to enable sending in or out of
              band data (normal or urgent, see RFC-1122)
Oct 28, 1997  V2.10 Added an OnLineTooLong event and code to handle the case
              where ReadLine has been called and the buffer overflowed (line
              long)
Oct 29, 1997  V2.11 Added DnsLookup functionnality (DnsLookup method, DnsResult
              property and DnsLookupDone event).
              Calling the connect method with a hostname work well except that
              it could block for a long period (ie: 2 minutes) if DNS do not
              respond. Calling the connect method with a numeric IP address will
              never block. So you can call DnsLookup to start hostname
              resolution in the background, after some time you evenutually
              receive the OnDnsLookupDone event. The copy the DnsResult property
              to the Addr property and call connect.
Oct 30, 1997  V2.12 added a check in DnsLookup to handel numeric IP which do
              not require any lookup. The numeric IP is treated immediately
              and immediately trigger the DnsLookupDone event.
              I modified the code to be compatible with Delphi 1.
Oct 31, 1997  V2.13 added CancelDnsLookup procedure.
Nov 09, 1997  V2.14 add LocalIPList function to get the list of local IP
              addresses (you have two IP addresses when connected to a LAN
              and an ISP).
Nov 11, 1997  V2.15 Made TCustomWSocket with virtual functions. This will
              allow to easily descend a new component from TCustomWSocket.
              Make ReadLine stop when the connection is broken.
Nov 12, 1997  V2.16 Corrected bug (Justin Yunke <yunke@productivity.org>)
              in LocalIPList: phe should be checked for nil.
Nov 18, 1997  Added ReceiveStr function (Suggested by FLDKNHA@danisco.com)
Nov 30, 1997  V2.18 Added a call to OnDnsLookupDone when canceling.
Dec 04, 1997  V2.19 Added LocalPort property and SessionConnected event
              for UDP socket.
              V2.20 Modified MessageLoop and ProcessMessages to process not
              only the socket messages, but all messages (necessary if the
              thread has several TWSocket for example).
Dec 09, 1997  V2.21 Corrected a minor bug in ReceiveStr. Detected by
              david@e.co.za (David Butler).
Dec 10, 1997  V2.22 Corrected a minor bug in Send which now correctly
              returns the number of bytes sent. Detected by
              james.huggins@blockbuster.com
Dec 16, 1997  V2.23 Corrected a bug which prevented the receiving of datagram
              from a UDP socket.
              Thank to Mark Melvin (melvin@misrg.ml.org) for pointing it.
Dec 20, 1997  V2.24 Added the PeekData function as suggested by Matt Rose
              mcrose@avproinc.com
Dec 26, 1997  V2.25 Added the Text property as suggested by Daniel P. Stasinski
              <dse@pacific.net>. Made GetXPort work even when listening as
              suggested by is81024@cis.nctu.edu.tw.
Jan 10, 1998  V2.26 Check for null hostname in DNSLookup
              Added DnsResultList with all IP addresses returned form DNS
Jan 13, 1998  V2.27 a Added MultiThreaaded property to tell the component that
              it is working in a thread and should take care of it (call
              internal ProcessMessages in place of Application.ProcessMessages,
              and do not use the WaitCtrl object).
Jan 15, 1998  V2.28 WMAsyncSelect revisited to work properly with NT winsock 2.
Feb 10, 1998  V2.29 Added an OnError event. If not assigned, then the component
              raise an exception when the error occurs.
Feb 14, 1998  V2.30 Published Text property
Feb 16, 1998  V2.31 Added virtual methods to trigger events
              Renamed all event handler variable to begin with FOn
Feb 26, 1998  V2.32 Added procedure PutDataInSendBuffer and PutStringInSendBuffer
              Using PutDataInSendBuffer you can place data in the send buffer
              without actualy trying to send it. This allows to place several
              (probably small) data chunk before the component attempt to send
              it. This prevent small packet to be sent. You can call
              Send(nil, 0) to force the component to begin to send data.
              If the buffer was not empty, PutDataInSendBuffer will just queue
              data to the buffer. This data will be sent in sequence.
Mar 02, 1998  V2.33 Changed the error check with WSAstartup as pointed out by
              Donald Strenczewilk (dstrenz@servtech.com)
Mar 06, 1998  V2.34 Added a runtime property to change the buffer size.
Mar 27, 1998  V2.35 Adapted for C++Builder 3
Apr 08, 1998  V2.36 Made SetDefaultValue virtual
Apr 13, 1998  V2.37 Reset FDnsLookupHandle to 0 after a failed call to
              WSACancelAsyncRequest
Apr 22, 1998  V2.38 Published AllSent property to let outside know if our
              buffer has some data unsent.
Apr 28, 1998  V2.39 Added LingerOnOff and LingerTimeout. Default values are
              wsLingerOn and timeout = 0 to behave by default as before.
              This value is setup just before Connect. Call SetLingerOption to
              set the linger option on the fly (the connection must be
              established to set the option). See winsock.closesocket on line
              help (winsock.hlp or win32.hlp) for a dsicussion of this option
              usage.
May 06, 1998  V2.40 Added a workaround for Trumpet winsock inet_addr bug.
              Thanks to Andrej Cuckov <andrej@cuckov.com> for his code.
May 18, 1998  V2.41 Jan Tomasek <xtomasej@feld.cvut.cz> found that Trumpet
              Winsock (Win 3.11) has some bugs and suggested a workaround in
              TryToSend procedure. This workaround makes TWSocket blocking in
              some cases. A new property enables the workaround. See code.
Jun 01, 1998  V2.42 In finalization section, check for not assigned IPList.
Jun 15, 1998  V2.43 Added code to finalization section to unload winsock if
              still loaded at that point (this happend if no socket where
              created but WinsockInfo called). Suggested by Daniel Fazekas
              <fdsoft@dns.gyor-ph.hu>
Jun 27, 1998  V2.44 Added checks for valid arguments in SetPort, SetProto
              and SetAddr. Deferred address resolution until Connect or Listen.
Jul 08, 1998  V2.45 Adadpted for Delphi 4
Jul 20, 1998  V2.46 Added SetWindowLong(FWindowHandle, 0, 0) in the destructor
              and a check for TWSocket class in XSocketWindowProc.
              Added virtual method RealSend.
Jul 23, 1998  V2.47 Added a TriggerSessionClosed from TryToSend in case of
              send error. This was called before, but with a nul error argument.
              Now it correctly gives the error number.
              Added a trashcan to receive data if no OnDataAvailable event
              handler is installed. Just receive the data and throw it away.
              Added reverse dns lookup asynchronous code (IP -> HostName).
              Thanks to Daniel Fazekas <fdsoft@dns.gyor-ph.hu> for his code.
Jul 30, 1998  V2.48 Changed local variable "error" by FLastError in SocketError
              to make it available from the OnError handler. Thanks to
              dana@medical-info.com for finding this bug.
              In Abort procedure, deleted all buffered data because it was send
              the next time the socket is opened !
              Added CancelDnsLookup in Abort procedure.
Aug 28, 1998  V2.49 Made InternalClose and ReceiveStr virtual
Sep 01, 1998  V2.50 Ignore CancelDnsLookup exception during destroy
Sep 29, 1998  V2.51 In InternalClose, protect AssignDefaultValue with
              try/except because SessionClosed event handler may have destroyed
              the component.
Oct 11, 1998  V2.52 Changed Shutdown(2) to Shutdown(1) in Internal Close to
              prevent data lost on send. You may have to call Shutdown(2) in
              your own code before calling Close to have the same behaviour as
              before.
              Changed argument type for ASyncReceive and passed 0 from FD_CLOSE
              message handler.
Oct 28, 1998  V2.53 Made WSocketLoadWinsock and WSocketUnloadWinsock public.
Nov 11, 1998  V2.54 Added OnDisplay event for debugging purpose
Nov 16, 1998  V2.55 Ignore WSANOTINITIALIZED error calling CloseSocket. This
              occurs when using TWSocket from a DLL and the finalization
              section is called before destroying TWSocket components (this is
              a program logic error).
              Made some properties and methods protected instead of private.
              Made some methods virtual.
              Added an Error argument to InternalClose.
              Added DoRecv virtual function.
              Added WSocketResolvePort
              Added WSocketResolveProto
              Deferred port and protocol resolution until really needed
              Transformed Listen to procedure (in case of failure Listen
              always calls SocketError which triggers an exception or the
              OnError event).
Nov 22, 1998  V3.00 Skipped from V2.55 to V3.00. Socks support is major update!
              Added SOCKS5 (RFC-1928) support for TCP connection and
              simple usercode passwword authentication.
              Consider the socks code as beta !
              New properties: SocksServer, SocksPort, SocksUsercode,
              SocksPassword, FSocksAuthentication. New events: OnSocksError,
              OnSocksConnected, OnSocksAuthState.
              I used WinGate 2.1d to test my code. Unfortunately WinGate do
              not correctly handle user authentication, so the code here is
              just untested...
Dec 05, 1998  V3.10 Removed ReadLine feature using TWait component.
              Added new TCustomLineWSocket and TCustomSyncWSocket.
              Those modifications implies that the ReadLine functionnality is
              slightly changed. Notably, the end of line marker is now
              configurable and remains in the received line unless a timeout
              occurs or the buffer is too small.
Dec 10, 1998  V3.11 Added missing code to resolve port in the Listen method.
Dec 12, 1998  V3.12 Added write method for LocalPort property. Thanks to
              Jan Tomasek <xtomasej@feld.cvut.cz> for his code.
              Added background exception handling.
              Fixed a bug in TCustomLineWSocket.TriggerDataAvailable which was
              not calling the inherited function when it actually should.
              Added a check on multithreaded in WaitForClose to call the
              correct ProcessMessages procedure.
              Added SOCKS4 support (only tcp connect is supported).
Dec 28, 1998  V3.13 Changed WSocketResolveHost to check for invalid numeric
              IP addresses whitout trying to use them as hostnames.
Dec 30, 1998  V3.14 Changed SetPort to SetRemotePort to solve the SetPort
              syndrome with BCB. Also chnaged GetPort to be consistant.
Jan 12, 1999  V3.15 Introduced DoRecvFrom virtual function. This correct a bug
              introduced in V3.14 related to UDP and RecvFrom.
Jan 23, 1999  V3.16 Changed FRcvdFlag computation in DoRecv and DoRecvFrom
              because it caused problems with HTTP component and large blocks.
              Removed modification by Jan Tomasek in TriggerDataAvailable
Jan 30, 1999  V3.17 Added WSocketResolveIp function.
              Checked for tcp protocol before setting linger off in abort.
              Moved a lot of variables from private to protected sections.
              Removed check for Assigned(FOnDataSent) in WMASyncSelect.
Feb 03, 1999  V3.18 Removed useless units in the uses clause.
Feb 14, 1999  V4.00 Jump to next major version number because lots of
              fundamental changes have been done. See below.

              Use runtime dynamic link with winsock. All winsock functions
              used by TWSocket are linked at runtime instead of loadtime. This
              allows programs to run without winsock installed, provided program
              doesn't try to use TWSocket or winsock function without first
              checking for winsock installation.
              Removed WSocketLoadWinsock and all use to DllStarted because it
              is no longer necessary because winsock is automatically loaded
              and initialized with the first call to a winsock function.

              Added MessagePump to centralize call to the message pump.
              It is a virtual procedure so that you can override it to
              cutomize your message pump. Also changed slightly ProcessMessages
              to closely match what is done in the forms unit.

              Removed old stuff related to WaitCtrl (was already excluded from
              compilation using a conditional directive).

              Added NOFORMS conditional compilation to exclude the Forms unit
              from wsocket. This will reduce exe or dll size by 100 to 150KB.
              To use this feature, you have to add NOFORMS in your project
              options in the "defines" edit box in the "directory/conditional"
              tab. Then you must add a message pump to your application and
              call it from TWSocket.OnMessagePump event handler. TWSocket really
              need a message pump in order to receive messages from winsock.
              Depending on how your application is built, you can use either
              TWSocket.MessageLoop or TWSocket.ProcessMessages to quickly build
              a working message pump. Or you may build your own custom message
              pump taylored to your needs. Your message pump must set
              TWSocket.Terminated property to TRUE when your application
              terminates or you may experience long delays when closing your
              application.
              You may use NOFORMS setting even if you use the forms unit (GUI
              application). Simply call Application.ProcessMessages in the
              OnMessagePump event handler.
              OnMessagePump event is not visible in the object inspector. You
              must assign it at run-time before using the component and after
              having created it (in a GUI application you can do that in the
              FormCreate event, in a console application, you can do it right
              after TWSocket.Create call).
Feb 17, 1999  V4.01 Added LineEcho and LineEdit features.
Feb 27, 1999  V4.02 Added TCustomLineWSocket.GetRcvdCount to make RcvdCount
              property and ReceiveStr work in line mode.
Mar 01, 1999  V4.03 Added conditional compile for BCB4. Thanks to James
              Legg <jlegg@iname.com>.
Mar 14, 1999  V4.04 Corrected a bug: wsocket hangup when there was no
              OnDataAvailable handler and line mode was on.
Apr 21, 1999  V4.05 Added H+ (long strings) and X+ (extended syntax)
              compilation options
May 07, 1999  V4.06 Added WSAECONNABORTED to valid error codes in TryToSend.
Jul 21, 1999  V4.07 Added GetPeerPort method, PeerPort and PeerAddr propertied
              as suggested by J. Punter <JPunter@login-bv.com>.
Aug 20, 1999  V4.05 Changed conditional compilation so that default is same
              as latest compiler (currently Delphi 4, Bcb 4). Should be ok for
              Delphi 5.
              Added LocalAddr property as suggested by Rod Pickering
              <fuzzylogic123@yahoo.com>. LocalAddr default to '0.0.0.0' and is
              intended to be used by a client when connecting to a server, to
              select a local interface for multihomed computer. Note that to
              select an interface for a server, use Addr property before
              listening.
              LocalAddr has to be an IP address in dotted form. Valid values are
              '0.0.0.0' for any interface, '127.0.0.1' for localhost or any
              value returned by LocalIPList.
              Replaced loadtime import for ntohs and getpeername by runtime
              load.
              Revised check for dotted numeric IP address in WSocketResolveHost
              to allow correct handling of hostnames beginning by a digit.
              Added OnSendData event. Triggered each time data has been sent
              to winsock. Do not confuse with OnDataSent which is triggered
              when TWSocket internal buffer is emptyed. This event has been
              suggested by Paul Gertzen" <pgertzen@livetechnology.com> to
              easyly implement progress bar.
              Corrected WSocketGetHostByAddr to make it dynamically link to
              winsock.
Sep 5, 1999   V4.09 Added CloseDelayed method.
              Make sure that TriggerSessionClosed is called from WMASyncSelect
              and InternalClose, even if there is no OnSessionClosed event
              handler assigned. This is required to make derived components
              work correctly.
              Created message WM_TRIGGER_EXCEPTION to help checking background
              exception handling (OnBgException event).
              Corrected bug for Delphi 1 and ReallocMem.
Oct 02, 1999  V4.10 Added Release method.
Oct 16, 1999  V4.11 Corrected a bug in TCustomLineWSocket.DoRecv: need to move
              data in front of buffer instead of changing buffer pointer which
              will crash the whole thing at free time.
Oct 23, 1999  V4.12 Made WSocketIsDottedIP a public function
Nov 12, 1999  V4.13 removed 3 calls to TriggerSocksAuthState because it was
                    called twice. By A. Burlakov <alex@helexis.com>.
Jan 24, 1999  V4.14 Call Receive instead of DoRecv from ReceiveStr to be sure
              to set LastError correctly. Thanks to Farkas Balazs
              <megasys@www.iridium.hu>
              Suppressed FDllName and used winsocket constant directly. I had
              troubles with some DLL code and string handling at program
              termination.
Apr 09, 2000 V4.15 Added error number when resolving proto and port
Apr 29, 2000 V4.16 Added WSocketForceLoadWinsock and
             WSocketCancelForceLoadWinsock. Thanks to Steve Williams.
             Created variable FSelectEvent to store current async event mask.
             Added ComponentOptions property with currently only one options
             wsoNoReceiveLoop which disable a receive loop in AsyncReceive.
             This loop breaking was suggested by Davie <smatters@smatters.com>
             to lower resource usage with really fast LAN and large transfers.
             By default, this option is disabled so there is no change needed
             in current code.
May 20, 2000 V4.17 Made TSocket = u_int (same def as in winsock.pas)
             Moved bind after setting options.
             Thanks to Primoz Gabrijelcic <fab@siol.net>
Jul 15, 2000 V4.18 Alon Gingold <gingold@hiker.org.il> changed
             TCustomSocksWSocket calls to inherited triggers of
             TriggerSessionConnected and TriggerDataAvailable.
             Now, it calls the trigger directly. This solves the problem
             of descendent classes with overriden triggers, not being
             called when a REAL connection was established, and when real
             data starts coming in. Special care MUST be taken in such
             overridden triggers to ONLY call the inherited trigger AND
             IMMEDIATELY EXIT when FSocksState <> socksData to avoid loopback
Jul 22, 2000 V4.19 John Goodwin <john@jjgoodwin.com> found a failure in the
             logic for DnsLookup. He also implemented a workaround.
             See DnsLookup comments for explanation.
Aug 09, 2000 V4.20 Alon Gingold <gingold2@mrm-multicat.com> found a bug in
             SOCKS4 implementation where a nul byte was incorrectly added
             (it should be added only with SOCKS4A version, not straith
             SOCKS4).
Sep 17, 2000 V4.21 Eugene Mayevski <Mayevski@eldos.org> added TWndMethod for
             NOFORMS applications in other components.
Oct 15, 2000 V4.22 Added method GetXAddr which returns local IP address to
             which a socket has been bound. There was already a GetXPort.
             Thanks to Wilfried Mestdagh <wilfried_sonal@compuserve.com>
             and Steve Williams <stevewilliams@kromestudios.com>.
Nov 08, 2000 V4.23 Moved FSelectEvent from private to protected section.
Nov 11, 2000 V4.24 Added LineLimit property and OnLineLimitExceeded event.
             When using line mode, line length is checked as each data block is
             comming. If the length is greater than the limit, then the event
             is triggered. You have the opportunity to close the socket or
             change the limit to a higher value. Thus you can prevent a hacker
             from locking your system by sending unlimited line which otherwise
             would eat up all system resources.
             Changed line handling variables to LongInt
             Checked all length involved in StrPCopy calls.
Nov 26, 2000 V4.25 Do not trust GetRcvdCount. Always call Receive to check for
             incomming data (sometime NT4 will hang if we don't do that).
Jan 24, 2001 V4.26 Blaine R Southam <bsoutham@iname.com> fixed out of bound
             error in TCustomLineWSocket.TriggerDataAvailable
Feb 17, 2001 V4.27 Davie <smatters@smatters.com> fixed a bug causing byte lost
             when closing (related to wsoNoReceiveLoop option).
May 04, 2001 V4.28 Fixed W2K bug (winsock message ordering)
Jun 18, 2001 V4.29 Added AllocateHWnd and DeallocateHWnd from Forms unit to
             avoid warning from Delphi 6 in all other components.
Jul 08, 2001 V4.30 Fixed small bug related to NOFOMRS and V4.29
Jul 26, 2001 V4.31 Checked csDesigning in GetRcvdCount so that Delphi 6 does'nt
             crash when object inspector wants to display RcvdCount value.
             Added multicast capability and UDP ReuseAddr. Thanks to Mark
             G. Lewis <Lewis@erg.sri.com> for his code.
             Added TriggerSessionClosed to SocketError as suggested by Wilfried
             Mestdagh <wilfried_sonal@compuserve.com>
Jul 28, 2001 V4.32 New option wsoTcpNoDelay implemented. Code by Arnaldo Braun
             <abraun@th.com.br>
Jul 30, 2001 V4.33 Corrected at few glitches with Delphi 1
Sep 08, 2001 V4.34 Added ThreadAttach and related functions
Nov 27, 2001 V4.35 Added type definition for in_addr and Delphi 2 (Yes there are
             still some peoples who wants to use it. Don't ask me why !).
Dec 02, 2001 V4.36 david.brock2@btinternet.com found a bug in SOCKS4 where
             error check incorrectly checked "FRcvBuf[1] = #$90" instead of
             "FRcvBuf[1] <> #90". He also found a bug when receiving domain name
             where length of name was incorrectly copyed to the buffer.
Dec 23, 2001 V4.37 Removed bWrite, nMoreCnt, bMoreFlag and nMoreMax which where
             not more really used. Thanks to Al Kirk <akirk@pacific.net> for
             showing that.
Feb 24, 2002 V4.38 Wilfried Mestdagh <wilfried@mestdagh.biz> added ThreadDetach
             and a property editor for LineEnd. XSocketDeallocateHWnd made a
             function.
             I created a new unit WSocketE.pas to put Wilfried's property
             editor so that it works correctly with Delphi 6.
Apr 24, 2002 V4.39 Removed OnLineTooLong event which was not used anywhere.
             Use OnLineLimitExceeded event if you used this event.
             Thanks to Alex Kook <cookis@mail.ru> for finding this one.
Apr 27, 2002 V4.40 Added procedure WSocketUnregisterClass to be able to
             unregister hidden window. This is necessary when TWSocket is
             used within a DLL which is unloaded and reloaded by applications,
             specially when running with Windows-XP. Thanks to Jean-Michel Aliu
             <jmaliu@jmasoftware.com> who provided a test case.
Jun 02, 2002 V4.41 allow SOCK_RAW in Connect method for any protocol which is
             not TCP or UDP. Thanks to Holger Lembke <holger@hlembke.de>.
Jun 04, 2002 V4.42 Do not call Listen for SOCK_RAW.
             Thanks to Holger Lembke <holger@hlembke.de>.
Jun 08, 2002 V4.43 Add a dummy Register procedure for BCB1.
             Thanks to Marc-Alexander Prowe <listen@mohajer.de>.
Jul 07, 2002 V4.44 Added code in Connect method to check if socket still opened
             after OnChangeState event. If not, trigger an error WSAINVAL.
Sep 16, 2002 V4.45 Exposed RcvdPtr and RcvdCnt readonly properties.
Sep 17, 2002 V4.46 Used InterlockedIncrement/InterlockedDecrement to Inc/Dec
             socket count safely when TWSocket is used within a thread. This
             was proposed by Matthew Meadows <matthew.meadows@inquisite.com>
Sep 28, 2002 V4.47 Changed DnsLookup so that a hostname is checked for dotted
             IP addresse and resolve it numerically. Thanks to Bogdan Calin
             <soul4blade@yahoo.com> who found this bug. Alos loaded the result
             list with the address to be consistant with real lookup result.
Nov 17, 2002 V4.48 Roland Klabunde <roland.klabunde@gmx.net> found a bug in
             multicast code: listening on a specific interface was ignored.
             He fixed Listen and Connect.
Nov 27, 2002 V4.49 Added ListenBacklog property, default to 5.
Dec 17, 2002 V4.50 Moved code to virtual function to permit SSL implementation.
Jan 19, 2003 V5.00 First pre-release for ICS-SSL. New major version number
             V5.01 Gabi Slonto <buffne01@gmx.net> found a bug in DnsLookup
             when hostname was actulally a dotted IP address.
Mar 18, 2003 V5.02 Fixed WSocketIsDottedIP: reordering of boolean expressions
             involaving a string. Thanks to Ian Baker <ibaker@codecutters.org>
Apr 30, 2003 V5.03 Replaced all calls to setsockopt by calls to
             WSocket_setsockopt to avoid statically linked winsock DLL.
             Thanks to Piotr Dalek <enigmatical@interia.pl>.
             Also replaced inet_addr by WSocket_inet_addr.
Aug 27, 2003 V5.04 Marco van de Voort <marcov@stack.nl> added FreePascal (FPC)
             conditional compilation. Please contact him for any FPC support
             question.
Aug 28, 2003 V5.05 Fixed a multithreading issue related to windows class
             registration. Now using a critical section around the code.
             Thanks to Bogdan Ureche <bureche@omnivex.com> for his precious help.
Aug 31, 2003 V5.06 Added warning about deprecated procedures Synchronize,
             WaitUntilReady and ReadLine. Do not use them in new applications.
Sep 03, 2003 V5.07 Bogdan Ureche <bureche@omnivex.com> added a critical section
             to avoid problem when winsock.dll is unloaded by a thread while
             another thread is still using some TWSocket.
Sep 15, 2003 V5.08 Fixed finalization section to no free critical section if
             a TWSocket is still existing. This happend for example when a
             TWSocket is on a form and Halt is called from FormCreate event.
             Changed SendStr argument to const.
Nov 09, 2003 V5.09 Added manifest constants for Shutdown
             Added TCustomLineWSocket.SendLine method.
Jan 16, 2004 V5.10 Added "const" in front of all method using strings.
Jan 17, 2004 V5.11 Modified TriggerDataAvailable so that when in LineMode, we
             check if a line is still in the buffer of already received data.
             Also updated WMTriggerDataAvailable to avoid infinite loops.
             Introduced FLineFound to flag when a line has been found.
             See "OLD_20040117" to find this code.
Jan 21, 2004 V5.12 Checked null string in PutStringInSendBuffer and null
             pointer in PutDataInSendBuffer.
Jan 26, 2004 V5.13 Conditional compilation for BCB for constants for Shutdown.
             Reordered uses clause for FPC compatibility.
             Fixed TCustomLineWSocket.TriggerDataAvailable to deliver data
             already received while in line mode but after component user
             turned line mode off in the middle of the way. This could occur
             for example in a HTTP application where line mode is used to
             receive HTTP header line and turned off when last header line is
             found. At that point, if posted data (HTTP document) was completely
             in the same packet as the last header line, that data was not
             delivered until the next packet comes, which could never occur !
Mar 20, 2004 V5.14 Added partial support for RAW socket.
             To use RAW sockets, set Proto to 'raw_ip', 'raw_icmp', ...
             Set Port to '0' or whatever value is useful for the protocol.
             When using IP protocol, you can add option wsoSIO_RCVALL so that
             your program receive ALL datagrams when you listen on a given
             interface (You can't use 0.0.0.0).
             Do not use Connect with RAW socket. Always use Listen and then
             use SendTo to send datagrams use the socket.
             Added ReqVerHigh and ReqVerLow properties to be able to select
             which winsock version you want to load. Default to 1.1 but need
             2.2 for RAW sockets to be used.
Mar 24, 2004 V5.15 Changed WSocket_Synchronized_ResolveProto to hard code
             protocol number for tcp, udp and raw.
Apr 17, 2004 V6.00 New major release started. Move all platform and runtime
             dependencies to separate units. New base component for handling
             component with window handle.
Jun 20, 2004 V 5.16 John Mulvey <john@mulvey.eurobell.co.uk> fixed error message
             in GetPeerAddr which incorrectly reported an error about
             GetPeerName.
May 23, 2005 V5.17 PutDataInSendBuffer set bAllSent to false.
Jun 03, 2005 V5.18 Added SocketSndBufSize property which gives the size of
             winsock internal send buffer. When using TCP, you must make sure
             you never use a BufSize equal or greater than this value or
             you'll experience bad performances. See description in MSDN
             http://support.microsoft.com/default.aspx?scid=kb;en-us;823764
             Default value for BufSize is 1460 and SocketSndBufSize is 8192 so
             there is no problem when not changing those values.
Jun 18, 2005 V5.19 Made TCustomSocksWSocket.Connect accept 'tcp' as well as '6'
             for protocol. By Piotr "Hellrayzer" Dalek.
             Renamed event OnDisplay to OnDebugDisplay.
Sept 4, 2005 V5.20 added BufferedByteCount property used to ensure winsock has sent
             data, currently used in TFtpCli to check a put has finished correctly
             Thanks to Tobias Giesen <tobias@tgtools.de> for the fix
Dec 27, 2005 V6.00a Updated new release with change done in the old release.
Dec 31, 2005 V6.00b added new debug and logging event and log levels, replacing
             conditional debug code with optional code to avoid rebuilding apps.
             Works in combination with new component TIcsLogger.
             This is controlled by the new LogOptions property:
               loDestEvent - write to OnIcsLogEvent (called from higher level protocols)
               loDestFile - write to file debug_out.myprog.txt
               loDestOutDebug - write to OutputDebugString (shown in Debugger Event Log window)
               loAddStamp - time stamp each log line (accurate only to about 18ms)
               loWsockErr - log wsocket errors
               loWsockInfo - log wsocket general information
               loWsockDump - log wsocket data (not implemented yet)
               loSslErr - log SSL errors
               loSslInfo - log SSL general information
               loSslDump - log SSL packets and data
               loProtSpecErr - log protocol specific error
               loProtSpecInfo - log protocol specific general information
               loProtSpecDump - log protocol specific data and packets
Jan 22, 2006 V6.00c Added some KeepAlive stuff (seems winsock is bugged and
             doesn't care any setting done !).
Jan 28, 2006 V6.00d Gerhard Rattinger fixed SetKeepAliveOption for Delphi 3
Mar 09, 2006 V6.00e Arno made properties to select keepalive parameters.
             He also fixed ReverseDnsLookup to return a list of
             host names (aliases) instead of just the first entry. Added func.
             ReverseDnsLookupSync.
Apr 27, 2006 V6.00f Roger Tinembart <tinembart@brain.ch> added a critical section
             around the list of sendbuffers (FBufHandler) to avoid problems when
             the data is placed in the sendbuffer (for example with SendStr)
             by a different thread than the one that is effectively sending the
             data with TryToSend
June 11, 2006 V6.01 Use new TIcsBufferHandler.
Aug 06, 2006 V6.02 Angus added GetWinsockErr to give alpha and numeric winsock
             errors and improved many other error messages,
             and fixed FReadCount for 64-bit downloads
             added some EXTERNALSYM for BCB compatiblity
Aug 18, 2006 V6.03 Fixed a bug in ASyncReceive(). This bug caused data loss.
Oct 28, 2006 V6.04 Added setter for SocketSndBufSize and SocketRcvBufSize
Dec 22, 2006 V6.05 Oliver Grahl fixed SendLine to properly count LineEnd characters.
Jan 18, 2007 V6.06 Fixed constructor and DeleteBufferedData to behave correctly
             when an exception occur in AllocateSocketHWnd.
Mar 23, 2007 V6.07 Removed FD_CONNECT from dup().
Apr 04, 2007 V6.08 Arno Garrels updated SetKeepAliveOption
Mar 10, 2008 V6.09 Francois Piette & Arno Garrels made some changes to
                   prepare code for Unicode
                   WSocket_gethostname conversion from String to AnsiString
                   WSocketGetProc and WSocket2GetProc use AnsiString
                   GetAliasList simplified and use AnsiString


About multithreading and event-driven:
    TWSocket is a pure asynchronous component. It is non-blocking and
    event-driven. It means that when you request an operation such as connect,
    the component start the operation your requested and give control back
    immediately while performing the operation in the background automatically.
    When the operation is done, an event is triggered (such as
    OnSessionConnected if you called Connect).

    This asynchronous non-blocking behaviour is very high performance but a
    little bit difficult to start with. For example, you can't call Connect and
    immediately call SendStr the line below. If you try, you'll have an
    exception triggered saying you are not connected. Calling connect will start
    connection process but will return long before connection is established.
    Calling SendStr at the next line will not work because the socket is not
    connected yet. To make it works the right way, you have to put your SendStr
    in the OnSessionConnected event.

    The asynchronous operation allows you to do several TCP/IP I/O
    simultaneously. Just use as many component as you need. Each one will
    operate independently of the other without blocking each other ! So you
    basically don't need multi-threading with TWSocket, unless YOUR processing
    is lengthy and blocking.

    If you have to use multithreading, you have two possibilities:
    1) Create your TWSocket from your thread's Execute method
    2) Attach a TWSocket to a given thread using ThreadAttach.
    In both cases, you must set MultiThreaded property to TRUE.
    If you don't use one of those methods, you'll end up with a false
    multithreaded program: all events will be processed by the main tread !
    For both methods to work, you MUST have a message loop withing your thread.
    Delphi create a message loop automatically for the main thread (it's in
    the Forms unit), but does NOT create one in a thread ! For your convenience,
    TWSocket has his own MessageLoop procedure. You can use it from your thread.

    Sample program MtSrv uses first method while ThrdSrv uses second method.
    Sample program TcpSrv is much the same as ThrdSrv but doesn't use any
    thread. You'll see that it is able to server a lot of simultaneous clients
    as well and it is much simpler.


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsWSocket;

{$B-}           { Enable partial boolean evaluation   }
{$T-}           { Untyped pointers                    }
{$X+}           { Enable extended syntax              }
{$I OverbyteIcsDefs.inc}
{$IFDEF DELPHI6_UP}
    {$WARN SYMBOL_PLATFORM   OFF}
    {$WARN SYMBOL_LIBRARY    OFF}
    {$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}
{$IFDEF COMPILER2_UP}{ Not for Delphi 1                    }
    {$H+}            { Use long strings                    }
    {$J+}            { Allow typed constant to be modified }
{$ENDIF}
{$IFDEF BCB3_UP}
    {$ObjExportAll On}
{$ENDIF}

{ If NO_ADV_MT is defined, then there is less multithread code compiled.    }
{$IFDEF DELPHI1}
    {$DEFINE NO_ADV_MT}
{$ENDIF}

{$IFDEF WIN32}
    {$DEFINE VCL}
{$ENDIF}

interface

uses
{ You must define USE_SSL so that SSL code is included in the component.    }
{ To be able to compile the component, you must have the SSL related files  }
{ which are _NOT_ freeware. See http://www.overbyte.be for details.         }
{$IFDEF USE_SSL}
  OverbyteIcsSSLEAY, OverbyteIcsLIBEAY, Contnrs, Masks, { Masks added AG 06/20/07 }  
{$ENDIF}
{$IFDEF CLR}
  System.ComponentModel,
  System.Text,
  System.Runtime.InteropServices,
  {$IFDEF VCL}
  Borland.Vcl.Classes,
  {$ENDIF}
{$ENDIF}
{$IFNDEF NO_DEBUG_LOG}
  OverbyteIcsLogger,
{$ENDIF}
  OverbyteIcsTypes,      OverbyteIcsLibrary,
  OverbyteIcsWndControl, OverbyteIcsWSockBuf,
  OverbyteIcsWinsock;

const
  WSocketVersion            = 608;
  CopyRight    : String     = ' TWSocket (c) 1996-2007 Francois Piette V6.08 ';
  WSA_WSOCKET_TIMEOUT       = 12001;
{$IFNDEF BCB}
  { Manifest constants for Shutdown }
  SD_RECEIVE                = 0;
  SD_SEND                   = 1;  { Use this one for graceful close }
  SD_BOTH                   = 2;
{$ENDIF}
{$IFDEF WIN32}
  winsocket  = 'wsock32.dll';      { 32 bits TCP/IP system DLL }
  winsocket2 = 'ws2_32.dll';       { 32 bits TCP/IP system DLL version 2}
{$ELSE}
  winsocket = 'winsock.dll';      { 16 bits TCP/IP system DLL }
{$ENDIF}

type

  TWndMethod         = procedure(var Message: TMessage) of object;
  ESocketException   = class(Exception);
  TBgExceptionEvent  = procedure (Sender : TObject;
                                  E : Exception;
                                  var CanClose : Boolean) of object;

  TSocketState       = (wsInvalidState,
                        wsOpened,     wsBound,
                        wsConnecting, wsSocksConnected, wsConnected,
                        wsAccepting,  wsListening,
                        wsClosed);
  TSocketSendFlags   = (wsSendNormal, wsSendUrgent);
  TSocketLingerOnOff = (wsLingerOff, wsLingerOn, wsLingerNoSet);
  TSocketKeepAliveOnOff = (wsKeepAliveOff, wsKeepAliveOnCustom,
                           wsKeepAliveOnSystem);
{$IFDEF CLR}
  TSockAddr          = OverbyteIcsWinSock.TSockAddr;  
{$ENDIF}
{$IFDEF WIN32}
  TSockAddr          = OverbyteIcsWinsock.TSockAddr;
    ip_mreq = record
        imr_multiaddr : in_addr;
        imr_interface : in_addr;
    end;
{$ENDIF}

  TDataAvailable     = procedure (Sender: TObject; ErrCode: Word) of object;
  TDataSent          = procedure (Sender: TObject; ErrCode: Word) of object;
  TSendData          = procedure (Sender: TObject; BytesSent: Integer) of object;
  TSessionClosed     = procedure (Sender: TObject; ErrCode: Word) of object;
  TSessionAvailable  = procedure (Sender: TObject; ErrCode: Word) of object;
  TSessionConnected  = procedure (Sender: TObject; ErrCode: Word) of object;
  TDnsLookupDone     = procedure (Sender: TObject; ErrCode: Word) of object;
  TChangeState       = procedure (Sender: TObject;
                                 OldState, NewState : TSocketState) of object;
  TDebugDisplay      = procedure (Sender: TObject; var Msg : String) of object;
  TWSocketSyncNextProc = procedure of object;
{$IFDEF CLR}
  [Flags]
  TWSocketOptions      = (wsoNone          = 0,
                          wsoNoReceiveLoop = 1,
                          wsoTcpNoDelay    = 2,
                          wsoSIO_RCVALL    = 4);
  TWSocketOption       = TWSocketOptions;
{$ENDIF}
{$IFDEF WIN32}
  TWSocketOption       = (wsoNoReceiveLoop, wsoTcpNoDelay, wsoSIO_RCVALL);
  TWSocketOptions      = set of TWSocketOption;
{$ENDIF}
{$IFDEF DELPHI4_UP}
  { TSocket type definition has been removed starting from Delphi 4 }
  //TSocket = u_int;
{$ENDIF}
  TTcpKeepAlive = packed record
    OnOff             : u_long;
    KeepAliveTime     : u_long;
    KeepAliveInterval : u_long;
  end;

type  { <== Required to make D7 code explorer happy, AG 05/24/2007 }

  TCustomWSocket = class(TIcsWndControl)
  private
    FDnsResult          : String;
    FDnsResultList      : TStrings;
    FSendFlags          : Integer;
    FLastError          : Integer;
    //FWindowHandle       : HWND;
  {$IFDEF CLR}
    FDnsLookupBuffer    : TBytes;
    FName               : String;
  {$ENDIF}
  {$IFDEF WIN32}
    FDnsLookupBuffer    : array [0..MAXGETHOSTSTRUCT] of char;
  {$ENDIF}
    FDnsLookupCheckMsg  : Boolean;
    FDnsLookupTempMsg   : TMessage;
    FHandle             : HWND;
  {$IFDEF CLR}
    FDnsLookupGCH       : GCHandle;
    FDnsLookupIntPtr    : IntPtr;
  {$ENDIF}
  {$IFDEF VER80}
    FTrumpetCompability : Boolean;
  {$ENDIF}
  protected
    FHSocket            : TSocket;
    FASocket            : TSocket;               { Accepted socket }
    FMsg_WM_ASYNCSELECT            : UINT;
    FMsg_WM_ASYNCGETHOSTBYNAME     : UINT;
    FMsg_WM_ASYNCGETHOSTBYADDR     : UINT;
    FMsg_WM_CLOSE_DELAYED          : UINT;
    //FMsg_WM_WSOCKET_RELEASE      : UINT;
    FMsg_WM_TRIGGER_EXCEPTION      : UINT;
    FMsg_WM_TRIGGER_DATA_AVAILABLE : UINT;
    FAddrStr            : String;
    FAddrResolved       : Boolean;
    FAddrFormat         : Integer;
    FAddrAssigned       : Boolean;
    FProto              : Integer;
    FProtoAssigned      : Boolean;
    FProtoResolved      : Boolean;
    FLocalPortResolved  : Boolean;
    FProtoStr           : String;
    FPortStr            : String;
    FPortAssigned       : Boolean;
    FPortResolved       : Boolean;
    FPortNum            : Integer;
    FLocalPortStr       : String;
    FLocalPortNum       : Integer;
    FLocalAddr          : String;     { IP address for local interface to use }
    FType               : Integer;
    FBufHandler         : TIcsBufferHandler;
    FLingerOnOff        : TSocketLingerOnOff;
    FLingerTimeout      : Integer;              { In seconds, 0 = disabled }
    FKeepAliveOnOff     : TSocketKeepAliveOnOff;
    FKeepAliveTime      : Integer;              { In milliseconds }
    FKeepAliveInterval  : Integer;              { In milliseconds }
    FListenBacklog      : Integer;
    ReadLineCount       : Integer;
    bAllSent            : Boolean;
    FReadCount          : Int64;   { V5.26 }



    FPaused             : Boolean;
    FCloseInvoked       : Boolean;
    FBufferedByteCount  : LongInt;   { V5.20 how man xmit bytes unsent        }
    FFlushTimeout       : Integer;   { This property is not used anymore      }
    FMultiThreaded      : Boolean;
    FDnsLookupHandle    : THandle;
    { More info about multicast can be found at:                              }
    {    http://ntrg.cs.tcd.ie/undergrad/4ba2/multicast/antony/               }
    {    http://www.tldp.org/HOWTO/Multicast-HOWTO-6.html                     }
    FMultiCast          : Boolean;
    { Multicast addresses consists of a range of addresses from 224.0.0.0 to  }
    { 239.255.255.255. However, the multicast addresses from 224.0.0.0 to     }
    { 224.0.0.255 are reserved for multicast routing information; Application }
    { programs should use multicast addresses outside this range.             }
    FMultiCastAddrStr   : String;
    FMultiCastIpTTL     : Integer;
    FReuseAddr          : Boolean;
    FComponentOptions   : TWSocketOptions;
    FState              : TSocketState;
    FRcvdFlag           : Boolean;
    FSelectEvent        : LongInt;
    FSelectMessage      : WORD;
{$IFDEF CLR}
    FRecvStrBuf         : TBytes;
{$ENDIF}
    FOnSessionAvailable : TSessionAvailable;
    FOnSessionConnected : TSessionConnected;
    FOnSessionClosed    : TSessionClosed;
    FOnChangeState      : TChangeState;
    FOnDataAvailable    : TDataAvailable;
    FOnDataSent         : TDataSent;
    FOnSendData         : TSendData;
    { FOnLineTooLong      : TNotifyEvent; }
    FOnDnsLookupDone    : TDnsLookupDone;
    FOnError            : TNotifyEvent;
    FOnBgException      : TBgExceptionEvent;
    FOnDebugDisplay     : TDebugDisplay;       { 18/06/05 }
    FOnMessagePump      : TNotifyEvent;
    //FThreadId           : THandle;
    FSocketSndBufSize   : Integer;  { Winsock internal socket send buffer size }
    FSocketRcvBufSize   : Integer;  { Winsock internal socket Recv buffer size }
{$IFNDEF NO_DEBUG_LOG}
    FIcsLogger          : TIcsLogger;                                           { V5.21 }
    procedure   SetIcsLogger(const Value : TIcsLogger); virtual;                { V5.21 }
    procedure   DebugLog(LogOption : TLogOption; const Msg : String); virtual;  { V5.21 }
    function    CheckLogOptions(const LogOption: TLogOption): Boolean; virtual; { V5.21 }
{$ENDIF}    
    procedure   WndProc(var MsgRec: TMessage); override;
    function    MsgHandlersCount: Integer; override;
    procedure   AllocateMsgHandlers; override;
    procedure   FreeMsgHandlers; override;
    procedure   AllocateSocketHWnd; virtual;
    procedure   DeallocateSocketHWnd; virtual;
    procedure   SocketError(sockfunc: String);
    procedure   WMASyncSelect(var msg: TMessage);
    procedure   WMAsyncGetHostByName(var msg: TMessage);
    procedure   WMAsyncGetHostByAddr(var msg: TMessage);
    procedure   WMCloseDelayed(var msg: TMessage);
    //procedure WMRelease(var msg: TMessage);
    procedure   ChangeState(NewState : TSocketState);
    procedure   TryToSend; virtual;
    procedure   ASyncReceive(Error : Word; MySocketOptions : TWSocketOptions);
    procedure   AssignDefaultValue; virtual;
    procedure   InternalClose(bShut : Boolean; Error : Word); virtual;
    procedure   InternalAbort(ErrCode : Word); virtual;
{$IFDEF WIN32}
    procedure   Notification(AComponent: TComponent; operation: TOperation); override;
{$ENDIF}
    procedure   SetSendFlags(newValue : TSocketSendFlags);
    function    GetSendFlags : TSocketSendFlags;
    procedure   SetAddr(InAddr : String);
    function    GetAddr : String;
    procedure   SetRemotePort(sPort : String); virtual;
    function    GetRemotePort : String;
    procedure   SetLocalAddr(sLocalAddr : String);
    procedure   SetLocalPort(sLocalPort : String);
    procedure   SetProto(sProto : String); virtual;
    function    GetProto : String;
    function    GetRcvdCount : LongInt; virtual;
    procedure   SetBufSize(Value : Integer); virtual;
    function    GetBufSize: Integer; virtual;
    procedure   SetSocketRcvBufSize(BufSize : Integer); virtual;
    procedure   SetSocketSndBufSize(BufSize : Integer); virtual;
    procedure   BindSocket; virtual;
    procedure   SendText(Str : String);
    function    RealSend(var Data : TWSocketData; Len : Integer) : Integer; virtual;
//  procedure   RaiseExceptionFmt(const Fmt : String; args : array of const); virtual;
    procedure   RaiseException(const Msg : String); virtual;
    procedure   HandleBackGroundException(E: Exception); override;
    function    GetReqVerLow: BYTE;
    procedure   SetReqVerLow(const Value: BYTE);
    function    GetReqVerHigh: BYTE;
    procedure   SetReqVerHigh(const Value: BYTE);
    procedure   TriggerDebugDisplay(Msg : String); { 18/06/05 }
    procedure   TriggerSendData(BytesSent : Integer);
    function    TriggerDataAvailable(Error : Word) : Boolean; virtual;
    procedure   TriggerSessionAvailable(Error : Word); virtual;
    procedure   TriggerSessionConnectedSpecial(Error : Word); virtual;
    procedure   TriggerSessionConnected(Error : Word); virtual;
    procedure   TriggerSessionClosed(Error : Word); virtual;
    procedure   TriggerDataSent(Error : Word); virtual;
    procedure   TriggerChangeState(OldState, NewState : TSocketState); virtual;
    procedure   TriggerDNSLookupDone(Error : Word); virtual;
    procedure   TriggerError; virtual;
    function    DoRecv(var Buffer : TWSocketData;
                       BufferSize : Integer;
                       Flags      : Integer) : Integer; virtual;
    function    DoRecvFrom(FHSocket    : TSocket;
                           var Buffer  : TWSocketData;
                           BufferSize  : Integer;
                           Flags       : Integer;
                           var From    : TSockAddr;
                           var FromLen : Integer) : Integer; virtual;
    procedure Do_FD_CONNECT(var msg: TMessage); virtual;
    procedure Do_FD_READ(var msg: TMessage); virtual;
    procedure Do_FD_WRITE(var msg: TMessage); virtual;
    procedure Do_FD_ACCEPT(var msg: TMessage); virtual;
    procedure Do_FD_CLOSE(var msg: TMessage); virtual;
    procedure DupConnected; virtual;
  public
    sin         : TSockAddrIn;
{$IFDEF CLR}
    constructor Create{$IFDEF VCL}(AOwner : TComponent){$ENDIF}; override;
{$ENDIF}
{$IFDEF WIN32}
    constructor Create(AOwner: TComponent); override;
{$ENDIF}
    destructor  Destroy; override;
    procedure   Connect; virtual;
    procedure   Close; virtual;
    procedure   CloseDelayed; virtual;
    //procedure Release; override;    { Release is handled in TIcsWndControl }
    procedure   Abort; virtual;
    procedure   Flush; virtual;
    procedure   WaitForClose; virtual;
    procedure   Listen; virtual;
    function    Accept: TSocket; virtual;
    function    Receive(Buffer : TWSocketData; BufferSize: Integer) : Integer;  {overload; }virtual;
//{$IFDEF WIN32}
//    function    Receive(Buffer : TBytes; BufferSize: Integer) : Integer;  overload; virtual;
//{$ENDIF}
    function    ReceiveStr : String; virtual;
    function    ReceiveFrom(Buffer      : TWSocketData;
                            BufferSize  : Integer;
                            var From    : TSockAddr;
                            var FromLen : Integer) : Integer; virtual;
    function    PeekData(Buffer : TWSocketData; BufferSize: Integer) : Integer;
    function    Send({$IFDEF CLR} const {$ENDIF} Data : TWSocketData; Len : Integer) : Integer; overload; virtual;
    function    Send(DataByte : Byte) : Integer; overload; virtual;
    function    SendTo(Dest       : TSockAddr;
                       DestLen    : Integer;
                       const Data : TWSocketData;
                       Len        : Integer) : Integer; virtual;
    function    SendStr(const Str : AnsiString) : Integer; {$IFDEF COMPILER12_UP} overload; {$ENDIF} virtual;
{$IFDEF COMPILER12_UP}
    function    SendStr(const Str : UnicodeString) : Integer; overload; virtual;
{$ENDIF}
    procedure   DnsLookup(const AHostName : AnsiString); virtual;
    procedure   ReverseDnsLookup(const HostAddr: String); virtual;
    procedure   ReverseDnsLookupSync(const HostAddr: String); virtual;  {AG 03/03/06}
    procedure   CancelDnsLookup; virtual;
    function    GetPeerAddr: String; virtual;
    function    GetPeerPort: String; virtual;
    function    GetPeerName(var Name : TSockAddrIn; NameLen : Integer) : Integer; virtual;
    function    GetXPort: String; virtual;
    function    GetXAddr: String; virtual;
    function    TimerIsSet(var tvp : TTimeVal) : Boolean; virtual;
    procedure   TimerClear(var tvp : TTimeVal); virtual;
    function    TimerCmp(var tvp : TTimeVal; var uvp : TTimeVal; IsEqual : Boolean) : Boolean; virtual;
    function    GetSockName(var saddr : TSockAddrIn; var saddrlen : Integer) : Integer; virtual;
    procedure   SetLingerOption;
    procedure   SetKeepAliveOption;
    procedure   Dup(NewHSocket : TSocket); virtual;
    procedure   Shutdown(How : Integer); virtual;
    procedure   Pause; virtual;
    procedure   Resume; virtual;
    procedure   PutDataInSendBuffer(Data : TWSocketData; Len : Integer); virtual;
    procedure   PutStringInSendBuffer(const Str : AnsiString); {$IFDEF COMPILER12_UP} overload; {$ENDIF}
{$IFDEF COMPILER12_UP}
    procedure   PutStringInSendBuffer(const Str : UnicodeString); overload;     
{$ENDIF}    
    procedure   DeleteBufferedData;
{$IFDEF COMPILER2_UP}
    procedure   ThreadAttach; override;
    procedure   ThreadDetach; override;
{$ENDIF}
{$IFDEF NOFORMS}
    property    Terminated         : Boolean        read  FTerminated
                                                    write FTerminated;
    property    OnMessagePump      : TNotifyEvent   read  FOnMessagePump
                                                    write FOnMessagePump;
{$ENDIF}
    property BufferedByteCount: LongInt             read FBufferedByteCount;  { V5.20 }
  protected
  {$IFDEF CLR}
    property Name : String                          read  FName
                                                    write FName;
  {$ENDIF}
{$IFNDEF NO_DEBUG_LOG}
    property IcsLogger : TIcsLogger                 read  FIcsLogger          { V5.21 }
                                                    write SetIcsLogger;       { V5.21 }
{$ENDIF}
    property PortNum : Integer                      read  FPortNum;
    property FWindowHandle : HWND                   read  FHandle;
    property HSocket : TSocket                      read  FHSocket
                                                    write Dup;

    property Addr : String                          read  GetAddr
                                                    write SetAddr;
    property Port : String                          read  GetRemotePort
                                                    write SetRemotePort;
    property LocalPort : String                     read  FLocalPortStr
                                                    write SetLocalPort;
    property LocalAddr : String                     read  FLocalAddr
                                                    write SetLocalAddr;
    property Proto : String                         read  GetProto
                                                    write SetProto;
    property MultiThreaded   : Boolean              read  FMultiThreaded
                                                    write FMultiThreaded;
    property MultiCast       : Boolean              read  FMultiCast
                                                    write FMultiCast;
    property MultiCastAddrStr: String               read  FMultiCastAddrStr
                                                    write FMultiCastAddrStr;
    property MultiCastIpTTL  : Integer              read  FMultiCastIpTTL
                                                    write FMultiCastIpTTL;
    property ReuseAddr       : Boolean              read  FReuseAddr
                                                    write FReuseAddr;
    property PeerAddr : String                      read  GetPeerAddr;
    property PeerPort : String                      read  GetPeerPort;
    property DnsResult : String                     read  FDnsResult;
    property DnsResultList : TStrings               read  FDnsResultList;
    property State : TSocketState                   read  FState;
    property AllSent   : Boolean                    read  bAllSent;
    property ReadCount : Int64   { V5.26 }          read  FReadCount;
    property RcvdCount : LongInt                    read  GetRcvdCount;
    property LastError : Integer                    read  FLastError
                                                    write FLastError;  { V5.20 }
    property ComponentOptions : TWSocketOptions     read  FComponentOptions
                                                    write FComponentOptions;
    property BufSize          : Integer             read  GetBufSize
                                                    write SetBufSize;
    property SocketRcvBufSize : Integer             read  FSocketRcvBufSize
                                                    write SetSocketRcvBufSize;
    property SocketSndBufSize : Integer             read  FSocketSndBufSize
                                                    write SetSocketSndBufSize;
    property ListenBacklog    : Integer             read  FListenBacklog
                                                    write FListenBacklog;
    property ReqVerLow       : BYTE                 read  GetReqVerLow
                                                    write SetReqVerLow;
    property ReqVerHigh      : BYTE                 read  GetReqVerHigh
                                                    write SetReqVerHigh;
    property OnDataAvailable : TDataAvailable       read  FOnDataAvailable
                                                    write FOnDataAvailable;
    property OnDataSent      : TDataSent            read  FOnDataSent
                                                    write FOnDataSent;
    property OnSendData      : TSendData            read  FOnSendData
                                                    write FOnSendData;
    property OnSessionClosed : TSessionClosed       read  FOnSessionClosed
                                                    write FOnSessionClosed;
    property OnSessionAvailable : TSessionAvailable read  FOnSessionAvailable
                                                    write FOnSessionAvailable;
    property OnSessionConnected : TSessionConnected read  FOnSessionConnected
                                                    write FOnSessionConnected;
    property OnChangeState      : TChangeState      read  FOnChangeState
                                                    write FOnChangeState;
  { property OnLineTooLong      : TNotifyEvent      read  FOnLineTooLong
                                                    write FOnLineTooLong; }
    property OnDnsLookupDone    : TDnsLookupDone    read  FOnDnsLookupDone
                                                    write FOnDnsLookupDone;
    property OnError            : TNotifyEvent      read  FOnError
                                                    write FOnError;
    property OnBgException      : TBgExceptionEvent read  FOnBgException
                                                    write FOnBgException;
    { FlushTimeout property is not used anymore }
    property FlushTimeout : Integer                 read  FFlushTimeOut
                                                    write FFlushTimeout;
    property SendFlags : TSocketSendFlags           read  GetSendFlags
                                                    write SetSendFlags;
    property Text: String                           read  ReceiveStr
                                                    write SendText;
    property LingerOnOff   : TSocketLingerOnOff     read  FLingerOnOff
                                                    write FLingerOnOff;
    property LingerTimeout : Integer                read  FLingerTimeout
                                                    write FLingerTimeout;
    property KeepAliveOnOff: TSocketKeepAliveOnOff  read  FKeepAliveOnOff
                                                    write FKeepAliveOnOff;
    property KeepAliveTime : Integer                read  FKeepAliveTime
                                                    write FKeepAliveTime;
    property KeepAliveInterval : Integer            read  FKeepAliveInterval
                                                    write FKeepAliveInterval;
{$IFDEF DELPHI1}
    property TrumpetCompability : Boolean           read  FTrumpetCompability
                                                    write FTrumpetCompability;
{$ENDIF}
    property OnDebugDisplay : TDebugDisplay         read  FOnDebugDisplay
                                                    write FOnDebugDisplay;
  end;

  TSocksState          = (socksData, socksNegociateMethods, socksAuthenticate, socksConnect);
  TSocksAuthentication = (socksNoAuthentication, socksAuthenticateUsercode);
  TSocksAuthState      = (socksAuthStart, socksAuthSuccess, socksAuthFailure, socksAuthNotRequired);
  TSocksAuthStateEvent = procedure(Sender : TObject; AuthState : TSocksAuthState) of object;
  TSocksErrorEvent     = procedure(Sender : TObject; Error : Integer; Msg : String) of Object;

  TCustomSocksWSocket = class(TCustomWSocket)
  protected
      FSocksState          : TSocksState;
      FSocksServer         : String;
      FSocksLevel          : String;
      FSocksPort           : String;
      FSocksPortAssigned   : Boolean;
      FSocksServerAssigned : Boolean;
      FSocksUsercode       : String;
      FSocksPassword       : String;
      FSocksAuthentication : TSocksAuthentication;
      FSocksAuthNumber     : char;
      FBoundAddr           : String;
      FBoundPort           : String;
  {$IFDEF CLR}
      FRcvBuf              : TBytes;
  {$ENDIF}
  {$IFDEF WIN32}
      FRcvBuf              : array [0..127] of Byte;
  {$ENDIF}
      FRcvCnt              : Integer;
      FSocksRcvdCnt        : Integer;
      FSocksRcvdPtr        : Integer;
      FOnSocksError        : TSocksErrorEvent;
      FOnSocksConnected    : TSessionConnected;
      FOnSocksAuthState    : TSocksAuthStateEvent;
      procedure   AssignDefaultValue; override;
      procedure   TriggerSessionConnectedSpecial(Error : Word); override;
      procedure   TriggerSocksConnected(Error : Word); virtual;
      procedure   TriggerSessionClosed(Error : Word); override;
      function    TriggerDataAvailable(Error : Word) : Boolean; override;
      procedure   SetSocksPort(sPort : String); virtual;
      procedure   SetSocksServer(sServer : String); virtual;
      procedure   TriggerSocksError(Error : Integer; Msg : String); virtual;
      procedure   TriggerSocksAuthState(AuthState : TSocksAuthState);
      function    GetRcvdCount : LongInt; override;
      procedure   SetSocksLevel(newValue : String);
      function    DoRecv(var Buffer : TWSocketData;
                         BufferSize : Integer;
                         Flags      : Integer) : Integer; override;
      procedure   SocksDoConnect;
      procedure   SocksDoAuthenticate;
      procedure   DataAvailableError(ErrCode : Integer; Msg : String);
  public
      constructor Create{$IFDEF VCL}(AOwner : TComponent){$ENDIF}; override;
      procedure   Connect; override;
      procedure   Listen; override;
  protected
      property SocksServer   : String               read  FSocksServer
                                                    write SetSocksServer;
      property SocksLevel    : String               read  FSocksLevel
                                                    write SetSocksLevel;
      property SocksPort     : String               read  FSocksPort
                                                    write SetSocksPort;
      property SocksUsercode : String               read  FSocksUsercode
                                                    write FSocksUsercode;
      property SocksPassword : String               read  FSocksPassword
                                                    write FSocksPassword;
      property SocksAuthentication : TSocksAuthentication
                                                    read  FSocksAuthentication
                                                    write FSocksAuthentication;
      property OnSocksError  : TSocksErrorEvent     read  FOnSocksError
                                                    write FOnSocksError;
      property OnSocksConnected : TSessionConnected read  FOnSocksConnected
                                                    write FOnSocksConnected;
      property OnSocksAuthState : TSocksAuthStateEvent
                                                    read  FOnSocksAuthState
                                                    write FOnSocksAuthState;
  end;

{ You must define USE_SSL so that SSL code is included in the component.    }
{ To be able to compile the component, you must have the SSL related files  }
{ which are _NOT_ freeware. See http://www.overbyte.be for details.         }
{$IFDEF USE_SSL}
{$I OverbyteIcsWSocketIntfSsl.inc}
{$ENDIF}

  TLineLimitEvent = procedure (Sender        : TObject;
                               RcvdLength    : LongInt;
                               var ClearData : Boolean) of object;
                               
{ You must define USE_SSL so that SSL code is included in the component.    }
{ To be able to compile the component, you must have the SSL related files  }
{ which are _NOT_ freeware. See http://www.overbyte.be for details.         }

{$IFDEF USE_SSL}              // Makes the IDE happy
  TBaseParentWSocket = TCustomSslWSocket;
{$ELSE}
  TBaseParentWSocket = TCustomSocksWSocket;
{$ENDIF}
(*
{$IFDEF USE_SSL}
  TCustomLineWSocket = class (TCustomSslWSocket)
{$ELSE}
  TCustomLineWSocket = class (TCustomSocksWSocket)
{$ENDIF}
*)
  TCustomLineWSocket = class (TBaseParentWSocket)
  protected
      FRcvdPtr             : TWSocketData;
      FRcvBufSize          : LongInt;
      FRcvdCnt             : LongInt;
      {$IFDEF CLR}
      FLocalBuf            : TBytes;
      {$ENDIF}
      FLineEnd             : AnsiString;  
      FLineMode            : Boolean;
      FLineLength          : Integer;    { When a line is available  }
      FLineLimit           : LongInt;    { Max line length we accept }
      FLineReceivedFlag    : Boolean;
      FLineFound           : Boolean;
      FLineClearData       : Boolean;
      FLineEcho            : Boolean;    { Echo received data    }
      FLineEdit            : Boolean;    { Edit received data    }
      FTimeout             : LongInt;    { Given in milliseconds }
      FTimeStop            : LongInt;    { Milliseconds          }
      FOnLineLimitExceeded : TLineLimitEvent;
      procedure   WndProc(var MsgRec: TMessage); override;
      procedure   WMTriggerDataAvailable(var msg: TMessage);
      function    TriggerDataAvailable(ErrCode : Word) : Boolean; override;
      procedure   TriggerSessionClosed(Error : Word); override;
      procedure   TriggerLineLimitExceeded(Cnt: Integer;
                                           var ClearData : Boolean); virtual;
      procedure   SetLineMode(newValue : Boolean); virtual;
      procedure   EditLine(var Len : Integer); virtual;
      function    GetRcvdCount : LongInt; override;
      function    DoRecv(var Buffer : TWSocketData;
                         BufferSize : Integer;
                         Flags      : Integer) : Integer; override;
  public
      constructor Create{$IFDEF VCL}(AOwner : TComponent){$ENDIF}; override;
      destructor  Destroy; override;
      function    SendLine(const Str : String) : Integer; virtual;
      property    LineLength : Integer      read  FLineLength;
      property    RcvdPtr    : TWSocketData read  FRcvdPtr;
      property    RcvdCnt    : LongInt      read  FRcvdCnt;
  published
      property LineMode : Boolean           read  FLineMode
                                            write SetLineMode;
      property LineLimit : LongInt          read  FLineLimit
                                            write FLineLimit;
      property LineEnd  : AnsiString        read  FLineEnd       
                                            write FLineEnd;
      property LineEcho : Boolean           read  FLineEcho
                                            write FLineEcho;
      property LineEdit : Boolean           read  FLineEdit
                                            write FLineEdit;
      property OnLineLimitExceeded : TLineLimitEvent
                                            read  FOnLineLimitExceeded
                                            write FOnLineLimitExceeded;
  end;

  { DEPRECATED: DO NOT USE Synchronize, WaitUntilReady, ReadLine procedure }
  { for a new application.                                                 }
  TCustomSyncWSocket = class(TCustomLineWSocket)
  protected
{$IFDEF CLR}
      FLinePointer : TBytes;
{$ENDIF}
{$IFDEF WIN32}
      FLinePointer : ^String;
{$ENDIF}
      function    Synchronize(Proc         : TWSocketSyncNextProc;
                              var DoneFlag : Boolean) : Integer; virtual;
      function    WaitUntilReady(var DoneFlag : Boolean) : Integer; virtual;
      procedure   InternalDataAvailable(Sender: TObject; Error: Word);
  public
      procedure   ReadLine(Timeout : Integer; var Buffer : String);
  end;

{$IFDEF CLR}
//  [DesignTimeVisibleAttribute(TRUE)]
{$ENDIF}
  TWSocket = class(TCustomSyncWSocket)
  public
    property PortNum;
    property Handle;
    property HSocket;
    property BufSize;
    property Text;
    property AllSent;
  {$IFDEF DELPHI1}
    property TrumpetCompability;
  {$ENDIF}
    property PeerAddr;
    property PeerPort;
    property State;
    property DnsResult;
    property DnsResultList;
    property ReadCount;
    property RcvdCount;
    property SocketRcvBufSize;     {AG 03/10/07}
    property SocketSndBufSize;     {AG 03/10/07}
    property OnDebugDisplay;
  published
    property Addr;
    property Port;
    property Proto;
    property LocalAddr;
    property LocalPort;
    property MultiThreaded;
    property MultiCast;
    property MultiCastAddrStr;
    property MultiCastIpTTL;
    property FlushTimeout;
    property SendFlags;
    property LingerOnOff;
    property LingerTimeout;
    property KeepAliveOnOff;
    property KeepAliveTime;
    property KeepAliveInterval;
    property SocksLevel;
    property SocksServer;
    property SocksPort;
    property SocksUsercode;
    property SocksPassword;
    property SocksAuthentication;
    property LastError;
    property ReuseAddr;
    property ComponentOptions;
    property ListenBacklog;
    property ReqVerLow;
    property ReqVerHigh;
    property OnDataAvailable;
    property OnDataSent;
    property OnSendData;
    property OnSessionClosed;
    property OnSessionAvailable;
    property OnSessionConnected;
    property OnSocksConnected;
    property OnChangeState;
    { property OnLineTooLong; }
    property OnDnsLookupDone;
    property OnError;
    property OnBgException;
    property OnSocksError;
    property OnSocksAuthState;
{$IFNDEF NO_DEBUG_LOG}
    property IcsLogger;                       { V5.21 }
{$ENDIF}
  end;

  TSocksWSocket = class(TWSocket)
  end;

{ You must define USE_SSL so that SSL code is included in the component.    }
{ To be able to compile the component, you must have the SSL related files  }
{ which are _NOT_ freeware. See http://www.overbyte.be for details.         }
{$IFDEF USE_SSL}
  TSslWSocket = class(TWSocket)
  public
      property  SslVersion;
      property  SslCipher;
      property  SslTotalBits;
      property  SslSecretBits;
      property  X509Class;
      //property  SslEstablished;
      property  SslState;
  published
{$IFNDEF NO_DEBUG_LOG}
      property IcsLogger;                      { V5.21 }
{$ENDIF}
      property  SslContext;
      property  SslEnable;
      property  SslAcceptableHosts;
      property  SslMode;
      property  OnSslVerifyPeer;
      property  OnSslHandshakeDone;
      property  OnSslCliGetSession;
      property  OnSslCliNewSession;
      property  OnSslSvrNewSession;
      property  OnSslSvrGetSession;
      property  OnSslSetSessionIDContext;
      property  OnSslShutDownComplete;
      property  OnSslCliCertRequest;
  end;
{$ENDIF}

function  HasOption(OptSet : TWSocketOptions; Opt : TWSocketOption) : Boolean;
function  RemoveOption(OptSet : TWSocketOptions; Opt : TWSocketOption) : TWSocketOptions;
function  AddOptions(Opts: array of TWSocketOption): TWSocketOptions;
function  WinsockInfo : TWSADATA;
function  WSocketErrorDesc(ErrCode: Integer) : String;
function  GetWinsockErr(ErrCode: Integer) : String;
function  GetWindowsErr(ErrCode: Integer): String;
{$IFDEF CLR}
function  WSocketGetHostByAddr(const Addr : String) : IntPtr;
function  WSocketGetHostByName(const Name : String) : IntPtr;
{$ENDIF}
{$IFDEF WIN32}
function  WSocketGetHostByAddr(Addr : String) : PHostEnt;
function  WSocketGetHostByName(Name : String) : PHostEnt;
{$ENDIF}
function  LocalHostName : String;
function  LocalIPList : TStrings;
function  WSocketResolveIp(IpAddr : String) : String;
function  WSocketResolveHost(InAddr : String) : TInAddr;
function  WSocketResolvePort(Port : String; Proto : String) : Word;
function  WSocketResolveProto(sProto : String) : Integer;
procedure WSocketForceLoadWinsock;
procedure WSocketCancelForceLoadWinsock;
procedure WSocketUnloadWinsock;
function  WSocketIsDottedIP(const S : String) : Boolean;
{ function  WSocketLoadWinsock : Boolean; 14/02/99 }

{$IFDEF DELPHI1}
type
    DWORD = LongInt;
    TWSAStartup            = function (wVersionRequired: word;
                                       var WSData: TWSAData): Integer;
    TWSACleanup            = function : Integer;
    TWSASetLastError       = procedure (iError: Integer);
    TWSAGetLastError       = function : Integer;
    TWSACancelAsyncRequest = function (hAsyncTaskHandle: THandle): Integer;
    TWSAAsyncGetHostByName = function (HWindow: HWND;
                                       wMsg: u_int;
                                       name, buf: PChar;
                                       buflen: Integer): THandle;
    TWSAAsyncGetHostByAddr = function (HWindow: HWND;
                                       wMsg: u_int; addr: PChar;
                                       len, Struct: Integer;
                                       buf: PChar;
                                       buflen: Integer): THandle;
    TWSAAsyncSelect        = function (s: TSocket;
                                      HWindow: HWND;
                                      wMsg: u_int;
                                      lEvent: Longint): Integer;
    TGetServByName         = function (name, proto: PChar): PServEnt;
    TGetProtoByName        = function (name: PChar): PProtoEnt;
    TGetHostByName         = function (name: PChar): PHostEnt;
    TGetHostByAddr         = function (addr: Pointer; len, Struct: Integer): PHostEnt;
    TGetHostName           = function (name: PChar; len: Integer): Integer;
    TOpenSocket            = function (af, Struct, protocol: Integer): TSocket;
    TShutdown              = function (s: TSocket; how: Integer): Integer;
    TSetSockOpt            = function (s: TSocket; level, optname: Integer;
                                       optval: PChar;
                                       optlen: Integer): Integer; 
    TGetSockOpt            = function (s: TSocket; level, optname: Integer; optval: PChar; var optlen: Integer): Integer; 
    TSendTo                = function (s: TSocket; var Buf;
                                       len, flags: Integer;
                                       var addrto: TSockAddr;
                                       tolen: Integer): Integer; 
    TSend                  = function (s: TSocket; var Buf;
                                       len, flags: Integer): Integer;
    TRecv                  = function (s: TSocket;
                                       var Buf;
                                       len, flags: Integer): Integer; 
    TRecvFrom              = function (s: TSocket;
                                       var Buf; len, flags: Integer;
                                       var from: TSockAddr;
                                       var fromlen: Integer): Integer;
    Tntohs                 = function (netshort: u_short): u_short;
    Tntohl                 = function (netlong: u_long): u_long;
    TListen                = function (s: TSocket; backlog: Integer): Integer;
    TIoctlSocket           = function (s: TSocket; cmd: DWORD;
                                       var arg: u_long): Integer;
    TInet_ntoa             = function (inaddr: TInAddr): PChar;
    TInet_addr             = function (cp: PChar): u_long;
    Thtons                 = function (hostshort: u_short): u_short;
    Thtonl                 = function (hostlong: u_long): u_long;
    TGetSockName           = function (s: TSocket; var name: TSockAddr;
                                       var namelen: Integer): Integer;
    TGetPeerName           = function (s: TSocket; var name: TSockAddr;
                                       var namelen: Integer): Integer;
    TConnect               = function (s: TSocket; var name: TSockAddr;
                                       namelen: Integer): Integer;
    TCloseSocket           = function (s: TSocket): Integer;
    TBind                  = function (s: TSocket; var addr: TSockAddr;
                                       namelen: Integer): Integer;
    TAccept                = function (s: TSocket; var addr: TSockAddr;
                                       var addrlen: Integer): TSocket;
{$ELSE}
{$IFDEF WIN32}   // DotNET doesn't support dynamic winsock loading
type
    TWSAStartup            = function (wVersionRequired: word;
                                       var WSData: TWSAData): Integer; stdcall;
    TWSACleanup            = function : Integer; stdcall;
    TWSASetLastError       = procedure (iError: Integer); stdcall;
    TWSAGetLastError       = function : Integer; stdcall;
    TWSACancelAsyncRequest = function (hAsyncTaskHandle: THandle): Integer; stdcall;
    TWSAAsyncGetHostByName = function (HWindow: HWND;
                                       wMsg: u_int;
                                       name, buf: PAnsiChar;
                                       buflen: Integer): THandle; stdcall;
    TWSAAsyncGetHostByAddr = function (HWindow: HWND;
                                       wMsg: u_int; addr: PChar;
                                       len, Struct: Integer;
                                       buf: PChar;
                                       buflen: Integer): THandle; stdcall;
    TWSAAsyncSelect        = function (s: TSocket;
                                       HWindow: HWND;
                                       wMsg: u_int;
                                       lEvent: Longint): Integer; stdcall;
    TGetServByName         = function (name, proto: PChar): PServEnt; stdcall;
    TGetProtoByName        = function (name: PChar): PProtoEnt; stdcall;
    TGetHostByName         = function (name: PChar): PHostEnt; stdcall;
    TGetHostByAddr         = function (addr: Pointer; len, Struct: Integer): PHostEnt; stdcall;
    TGetHostName           = function (name: PChar; len: Integer): Integer; stdcall;
    TOpenSocket            = function (af, Struct, protocol: Integer): TSocket; stdcall;
    TShutdown              = function (s: TSocket; how: Integer): Integer; stdcall;
    TSetSockOpt            = function (s: TSocket; level, optname: Integer;
                                       optval: PChar;
                                       optlen: Integer): Integer; stdcall;
    TGetSockOpt            = function (s: TSocket; level, optname: Integer;
                                       optval: PChar;
                                       var optlen: Integer): Integer; stdcall;
    TSendTo                = function (s: TSocket; var Buf;
                                       len, flags: Integer;
                                       var addrto: TSockAddr;
                                       tolen: Integer): Integer; stdcall;
    TSend                  = function (s: TSocket; var Buf;
                                       len, flags: Integer): Integer; stdcall;
    TRecv                  = function (s: TSocket;
                                       var Buf;
                                       len, flags: Integer): Integer; stdcall;
    TRecvFrom              = function (s: TSocket;
                                       var Buf; len, flags: Integer;
                                       var from: TSockAddr;
                                       var fromlen: Integer): Integer; stdcall;
    Tntohs                 = function (netshort: u_short): u_short; stdcall;
    Tntohl                 = function (netlong: u_long): u_long; stdcall;
    TListen                = function (s: TSocket;
                                       backlog: Integer): Integer; stdcall;
    TIoctlSocket           = function (s: TSocket; cmd: DWORD;
                                       var arg: u_long): Integer; stdcall;
    TWSAIoctl              = function (s                 : TSocket;
                                       IoControlCode     : DWORD;
                                       InBuffer          : Pointer;
                                       InBufferSize      : DWORD;
                                       OutBuffer         : Pointer;
                                       OutBufferSize     : DWORD;
                                       var BytesReturned : DWORD;
                                       Overlapped        : POverlapped;
                                       CompletionRoutine : FARPROC): Integer; stdcall;
    TInet_ntoa             = function (inaddr: TInAddr): PAnsiChar; stdcall; 
    TInet_addr             = function (cp: PAnsiChar): u_long; stdcall;  
    Thtons                 = function (hostshort: u_short): u_short; stdcall;
    Thtonl                 = function (hostlong: u_long): u_long; stdcall;
    TGetSockName           = function (s: TSocket; var name: TSockAddr;
                                       var namelen: Integer): Integer; stdcall;
    TGetPeerName           = function (s: TSocket; var name: TSockAddr;
                                       var namelen: Integer): Integer; stdcall;
    TConnect               = function (s: TSocket; var name: TSockAddr;
                                       namelen: Integer): Integer; stdcall;
    TCloseSocket           = function (s: TSocket): Integer; stdcall;
    TBind                  = function (s: TSocket; var addr: TSockAddr;
                                       namelen: Integer): Integer; stdcall;
{$IFDEF VER90} { Delphi 2 has a special definition}
    TAccept                = function (s: TSocket; var addr: TSockAddr;
                                       var addrlen: Integer): TSocket; stdcall;
{$ELSE}
    TAccept                = function (s: TSocket; addr: PSockAddr;
                                       addrlen: PInteger): TSocket; stdcall;
{$ENDIF}
{$ENDIF}
{$ENDIF}

{$IFDEF WIN32}  // DotNET doesn't support dynamic winsock loading
var
   FWSAStartup            : TWSAStartup;
   FWSACleanup            : TWSACleanup;
   FWSASetLastError       : TWSASetLastError;
   FWSAGetLastError       : TWSAGetLastError;
   FWSACancelAsyncRequest : TWSACancelAsyncRequest;
   FWSAAsyncGetHostByName : TWSAAsyncGetHostByName;
   FWSAAsyncGetHostByAddr : TWSAAsyncGetHostByAddr;
   FWSAAsyncSelect        : TWSAAsyncSelect;
   FGetServByName         : TGetServByName;
   FGetProtoByName        : TGetProtoByName;
   FGetHostByName         : TGetHostByName;
   FGetHostByAddr         : TGetHostByAddr;
   FGetHostName           : TGetHostName;
   FOpenSocket            : TOpenSocket;
   FShutdown              : TShutdown;
   FSetSockOpt            : TSetSockOpt;
   FGetSockOpt            : TGetSockOpt;
   FSendTo                : TSendTo;
   FSend                  : TSend;
   FRecv                  : TRecv;
   FRecvFrom              : TRecvFrom;
   Fntohs                 : Tntohs;
   Fntohl                 : Tntohl;
   FListen                : TListen;
   FIoctlSocket           : TIoctlSocket;
{$IFDEF COMPILER2_UP}
   FWSAIoctl              : TWSAIoctl;
{$ENDIF}
   FInet_ntoa             : TInet_ntoa;
   FInet_addr             : TInet_addr;
   Fhtons                 : Thtons;
   Fhtonl                 : Thtonl;
   FGetSockName           : TGetSockName;
   FGetPeerName           : TGetPeerName;
   FConnect               : TConnect;
   FCloseSocket           : TCloseSocket;
   FBind                  : TBind;
   FAccept                : TAccept;

function WSocketGetProc(const ProcName : AnsiString) : Pointer;
{$IFDEF COMPILER2_UP}
function WSocket2GetProc(const ProcName : AnsiString) : Pointer;
{$ENDIF}
{$ENDIF}

function WSocket_WSAStartup(wVersionRequired: word;
                           var WSData: TWSAData): Integer;
function WSocket_WSACleanup : Integer;
procedure WSocket_WSASetLastError(iError: Integer);
function WSocket_WSAGetLastError: Integer;
function WSocket_WSACancelAsyncRequest(hAsyncTaskHandle: THandle): Integer;
{$IFDEF CLR}
function  WSocket_WSAAsyncGetHostByName(HWindow: HWND; wMsg: u_int;
                                        const name : String; buf: IntPtr;
                                        buflen: Integer): THandle;
function  WSocket_WSAAsyncGetHostByAddr(HWindow: HWND;
                                        wMsg: u_int; var addr: u_long;
                                        len, Struct: Integer;
                                        buf: IntPtr;
                                        buflen: Integer): THandle;
{$ENDIF}
{$IFDEF WIN32}
function WSocket_WSAAsyncGetHostByName(HWindow: HWND; wMsg: u_int;
                                      name, buf: PAnsiChar;
                                      buflen: Integer): THandle;
function WSocket_WSAAsyncGetHostByAddr(HWindow: HWND;
                                      wMsg: u_int; addr: PChar;
                                      len, Struct: Integer;
                                      buf: PChar;
                                      buflen: Integer): THandle;
{$ENDIF}
function WSocket_WSAAsyncSelect(s: TSocket; HWindow: HWND; wMsg: u_int; lEvent: Longint): Integer;
function WSocket_recv(s: TSocket;
                      var Buf: TWSocketData; len, flags: Integer): Integer;
function WSocket_recvfrom(s: TSocket;
                         var Buf: TWSocketData; len, flags: Integer;
                         var from: TSockAddr;
                         var fromlen: Integer): Integer;
{$IFDEF CLR}
function  WSocket_getservbyname(const name, proto: String): IntPtr;
function  WSocket_getprotobyname(const name: String): IntPtr;
function  WSocket_gethostbyname(const name: String): IntPtr;
function  WSocket_gethostbyaddr(var addr: u_long; len, Struct: Integer): IntPtr;
{$ENDIF}
{$IFDEF WIN32}
function WSocket_getservbyname(name, proto: PChar): PServEnt;
function WSocket_getprotobyname(name: PChar): PProtoEnt;
function WSocket_gethostbyname(name: PChar): PHostEnt;
function WSocket_gethostbyaddr(addr: Pointer; len, Struct: Integer): PHostEnt;
{$ENDIF}
function WSocket_gethostname(out name: String): Integer;
function WSocket_socket(af, Struct, protocol: Integer): TSocket;
function WSocket_shutdown(s: TSocket; how: Integer): Integer;
{$IFDEF CLR}
function  WSocket_getsockopt(s: TSocket; level, optname: Integer;
                             var optval: Integer;
                             var optlen: Integer): Integer; overload;
function  WSocket_getsockopt(s: TSocket; level, optname: Integer;
                             var optval: ip_mreq;
                             var optlen: Integer): Integer; overload;
function  WSocket_getsockopt(s: TSocket; level, optname: Integer;
                             var optval: TInAddr;
                             var optlen: Integer): Integer; overload;
function  WSocket_getsockopt(s: TSocket; level, optname: Integer;
                             var optval: TLinger;
                             var optlen: Integer): Integer; overload;
{$ENDIF}
{$IFDEF WIN32}
function WSocket_setsockopt(s: TSocket; level, optname: Integer; optval: PChar;
                            optlen: Integer): Integer; overload;
function WSocket_setsockopt(s: TSocket; level, optname: Integer; var optval: TLinger;
                            optlen: Integer): Integer; overload;
function WSocket_getsockopt(s: TSocket; level, optname: Integer; optval: PChar;
                            var optlen: Integer): Integer;
{$ENDIF}
function WSocket_sendto(s: TSocket; var Buf : TWSocketData; len, flags: Integer;
                        var addrto: TSockAddr;
                        tolen: Integer): Integer;
function WSocket_send(s: TSocket; var Buf : TWSocketData; len, flags: Integer): Integer;
function WSocket_ntohs(netshort: u_short): u_short;
function WSocket_ntohl(netlong: u_long): u_long;
function WSocket_listen(s: TSocket; backlog: Integer): Integer;
function WSocket_ioctlsocket(s: TSocket; cmd: DWORD; var arg: u_long): Integer;
{$IFNDEF VER80}
{$IFDEF WIN32}
function WSocket_WSAIoctl(s                 : TSocket; IoControlCode : DWORD;
                          InBuffer          : Pointer; InBufferSize  : DWORD;
                          OutBuffer         : Pointer; OutBufferSize : DWORD;
                          var BytesReturned : DWORD; Overlapped      : POverlapped;
                          CompletionRoutine : FARPROC): Integer;
{$ENDIF}
{$ENDIF}
function WSocket_inet_ntoa(inaddr: TInAddr): String;
function WSocket_inet_addr(const cp: AnsiString): u_long;  
function WSocket_htons(hostshort: u_short): u_short;
function WSocket_htonl(hostlong: u_long): u_long;
function WSocket_getsockname(s: TSocket; var name: TSockAddr;
                             var namelen: Integer): Integer;
function WSocket_getpeername(s: TSocket; var name: TSockAddr;
                             var namelen: Integer): Integer;
function WSocket_connect(s: TSocket; var name: TSockAddr;
                         namelen: Integer): Integer;
function WSocket_closesocket(s: TSocket): Integer;
function WSocket_bind(s: TSocket; var addr: TSockAddr; namelen: Integer): Integer;
{$IFDEF DELPHI1}
function WSocket_accept(s: TSocket; var addr: TSockAddr; var addrlen: Integer): TSocket;
{$ELSE}
{$IFDEF VER90}
function WSocket_accept(s: TSocket; var addr: TSockAddr; var addrlen: Integer): TSocket;
{$ELSE}
{$IFDEF CLR}
function WSocket_accept(s: TSocket; var addr: TSockAddr; var addrlen: Integer): TSocket;
{$ENDIF}
{$IFDEF WIN32}
function WSocket_accept(s: TSocket; addr: PSockAddr; addrlen: PInteger): TSocket;
{$ENDIF}
{$ENDIF}
{$ENDIF}

const
    WSocketGCount   : Integer = 0;
    WSocketGForced  : boolean = FALSE;
    GReqVerLow      : BYTE    = 1;
    GReqVerHigh     : BYTE    = 1;

{$EXTERNALSYM IOC_UNIX}
    IOC_UNIX             = $00000000;       { Do not use this in Windows     }
{   IOCPARM_MASK         = $000007F7;  }    { Parameters must be < 128 bytes }
 {$EXTERNALSYM IOC_WS2}
    IOC_WS2              = $08000000;
 {$EXTERNALSYM IOC_PROTOCOL}
    IOC_PROTOCOL         = $10000000;
{   IOC_VOID             = $20000000;  }    { No parameters                  }
{   IOC_OUT              = $40000000;  }    { Copy out parameters            }
{$EXTERNALSYM IOC_IN}
    IOC_IN               = $80000000;       { Copy in parameters             }
{   IOC_INOUT            = (IOC_IN or IOC_OUT); }
{$EXTERNALSYM IOC_VENDOR}
    IOC_VENDOR           = $18000000;
    SIO_RCVALL           = IOC_IN or IOC_VENDOR or 1;
    SIO_RCVALL_MCAST     = IOC_IN or IOC_VENDOR or 2;
    SIO_RCVALL_IGMPMCAST = IOC_IN or IOC_VENDOR or 3;
    SIO_KEEPALIVE_VALS   = IOC_IN or IOC_VENDOR or 4;
    SIO_ABSORB_RTRALERT  = IOC_IN or IOC_VENDOR or 5;
    SIO_UCAST_IF         = IOC_IN or IOC_VENDOR or 6;
    SIO_LIMIT_BROADCASTS = IOC_IN or IOC_VENDOR or 7;
    SIO_INDEX_BIND       = IOC_IN or IOC_VENDOR or 8;
    SIO_INDEX_MCASTIF    = IOC_IN or IOC_VENDOR or 9;
    SIO_INDEX_ADD_MCAST  = IOC_IN or IOC_VENDOR or 10;
    SIO_INDEX_DEL_MCAST  = IOC_IN or IOC_VENDOR or 11;

{$IFDEF DELPHI1}
{ Delphi 1 doesn't like missing register procedure in a unit }
procedure Register;
{$ENDIF}
{$IFDEF VER90}
{ Delphi 2 doesn't like missing register procedure in a unit }
procedure Register;
{$ENDIF}
{$IFDEF VER93}
{ BCB 1 doesn't like missing register procedure in a unit }
procedure Register;
{$ENDIF}

{$IFDEF CLR}
//procedure Register;
{$ENDIF}
{$IFNDEF NO_DEBUG_LOG}
var
    __DataSocket : TCustomWSocket;
{$ENDIF}

implementation

{ R 'OverbyteIcsWSocket.TWSocket.bmp'}

const
    FDllHandle     : THandle  = 0;
    FDll2Handle    : THandle  = 0;
    socksNoError              = 20000;
    socksProtocolError        = 20001;
    socksVersionError         = 20002;
    socksAuthMethodError      = 20003;
    socksGeneralFailure       = 20004;
    socksConnectionNotAllowed = 20005;
    socksNetworkUnreachable   = 20006;
    socksHostUnreachable      = 20007;
    socksConnectionRefused    = 20008;
    socksTtlExpired           = 20009;
    socksUnknownCommand       = 20010;
    socksUnknownAddressType   = 20011;
    socksUnassignedError      = 20012;
    socksInternalError        = 20013;
    socksDataReceiveError     = 20014;
    socksAuthenticationFailed = 20015;
    socksRejectedOrFailed     = 20016;
    socksHostResolutionFailed = 20017;

{$IFDEF DELPHI1}
    IP_DEFAULT_MULTICAST_TTL  = 1;
    IP_MULTICAST_IF           = 2;
    IP_MULTICAST_TTL          = 3;
    IP_MULTICAST_LOOP         = 4;
    IP_ADD_MEMBERSHIP         = 5;
    IP_DROP_MEMBERSHIP        = 6;
type
    in_addr = TInAddr;
{$ENDIF}
{$IFNDEF DELPHI1}
{$IFNDEF COMPILER4_UP}
type
    in_addr = TInAddr;
{$ENDIF}
{$ENDIF}

var
    GInitData         : TWSADATA;
    IPList            : TStrings;
{$IFDEF CLR}
    GWSAStartupCalled : Boolean = FALSE;
{$ENDIF}
{$IFDEF COMPILER2_UP}
    GClassCritSect    : TRTLCriticalSection;
    GWSockCritSect    : TRTLCriticalSection;
//  V6.01 moved GSendBufCritSect to OverbyteIcsWSocket.pas
//  GSendBufCritSect  : TRTLCriticalSection;                 { v6.00f }
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF DELPHI1}
{ Delphi 1 miss the SetLength procedure. So we rewrite it. }
procedure SetLength(var S: String; NewLength: Integer);
begin
    S[0] := chr(NewLength);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF DELPHI1}
{ Delphi 1 doesn't like missing register procedure in a unit so we provide  }
{ an empty procedure                                                        }
procedure Register;
begin
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF VER90}
{ Delphi 2 doesn't like missing register procedure in a unit so we provide  }
{ an empty procedure                                                        }
procedure Register;
begin
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF VER93}
{ BCB 1 doesn't like missing register procedure in a unit so we provide     }
{ an empty procedure                                                        }
procedure Register;
begin
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function atoi(const Value : String) : Integer;
var
    i : Integer;
begin
    Result := 0;
    i := 1;
    while (i <= Length(Value)) and (Value[i] = ' ') do
        i := i + 1;
    while (i <= Length(Value)) and (Value[i] >= '0') and (Value[i] <= '9')do begin
        Result := Result * 10 + ord(Value[i]) - ord('0');
        i := i + 1;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsDigit(Ch : Char) : Boolean;
begin
    Result := (ch >= '0') and (ch <= '9');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Check for a valid numeric dotted IP address such as 192.161.65.25         }
{ Accept leading and trailing spaces.                                       }
function WSocketIsDottedIP(const S : String) : Boolean;
var
    I          : Integer;
    DotCount   : Integer;
    NumVal     : Integer;
begin
    Result     := FALSE;
    DotCount   := 0;
    NumVal     := 0;
    I          := 1;

    { Skip leading spaces }
    while (I <= Length(S)) and (S[I] = ' ') do
        Inc(I);
    { Can't begin with a dot }
    if (I <= Length(S)) and (S[I] = '.') then
        Exit;
    { Scan full string }
    while I <= Length(S) do begin
        if S[I] = '.' then begin
            Inc(DotCount);
            if (DotCount > 3) or (NumVal > 255) then
                Exit;
            NumVal := 0;
            { A dot must be followed by a digit }
            if (I >= Length(S)) or (not (AnsiChar(S[I + 1]) in ['0'..'9'])) then
                Exit;
        end
        else if AnsiChar(S[I]) in ['0'..'9'] then
            NumVal := NumVal * 10 + Ord(S[I]) - Ord('0')
        else begin
            { Not a digit nor a dot. Accept spaces until end of string }
            while (I <= Length(S)) and (S[I] = ' ') do
                Inc(I);
            if I <= Length(S) then
                Exit;  { Not a space, do not accept }
            break;     { Only spaces, accept        }
        end;
        Inc(I);
    end;
    { We must have exactly 3 dots }
    if (DotCount <> 3) or (NumVal > 255) then
        Exit;
    Result := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF DELPHI1}
function TrimRight(Str : String) : String;
var
    i : Integer;
begin
    i := Length(Str);
    while (i > 0) and (Str[i] in [' ', #9]) do
        i := i - 1;
    Result := Copy(Str, 1, i);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TrimLeft(Str : String) : String;
var
    i : Integer;
begin
    if Str[1] <> ' ' then
        Result := Str
    else begin
        i := 1;
        while (i <= Length(Str)) and (Str[i] = ' ') do
            i := i + 1;
        Result := Copy(Str, i, Length(Str) - i + 1);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Trim(Str : String) : String;
begin
    Result := TrimLeft(TrimRight(Str));
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.RaiseException(const Msg : String);
begin
    if Assigned(FOnError) then
        TriggerError                 { Should be modified to pass Msg ! }
    else
        raise ESocketException.Create(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
//procedure TCustomWSocket.RaiseExceptionFmt(const Fmt : String; args : array of const);
//begin
//    if Assigned(FOnError) then
//        TriggerError
//    else
//        raise ESocketException.CreateFmt(Fmt, args);
//end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF WIN32}
function WSocket_Synchronized_WSAStartup(
    wVersionRequired: word;
    var WSData: TWSAData): Integer;
begin
    if @FWSAStartup = nil then
        @FWSAStartup := WSocketGetProc('WSAStartup');
    Result := FWSAStartup(wVersionRequired, WSData);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_WSACleanup : Integer;
begin
    if @FWSACleanup = nil then
        @FWSACleanup := WSocketGetProc('WSACleanup');
    Result := FWSACleanup;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure WSocket_Synchronized_WSASetLastError(iError: Integer);
begin
    if @FWSASetLastError = nil then
        @FWSASetLastError := WSocketGetProc('WSASetLastError');
    FWSASetLastError(iError);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_WSAGetLastError: Integer;
begin
    if @FWSAGetLastError = nil then
        @FWSAGetLastError := WSocketGetProc('WSAGetLastError');
    Result := FWSAGetLastError;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_WSACancelAsyncRequest(hAsyncTaskHandle: THandle): Integer;
begin
    if @FWSACancelAsyncRequest = nil then
        @FWSACancelAsyncRequest := WSocketGetProc('WSACancelAsyncRequest');
    Result := FWSACancelAsyncRequest(hAsyncTaskHandle);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_WSAAsyncGetHostByName(
    HWindow: HWND; wMsg: u_int;
    name, buf: PAnsiChar;
    buflen: Integer): THandle;
begin
    if @FWSAAsyncGetHostByName = nil then
        @FWSAAsyncGetHostByName := WSocketGetProc('WSAAsyncGetHostByName');
    Result := FWSAAsyncGetHostByName(HWindow, wMsg, name, buf, buflen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_WSAAsyncGetHostByAddr(
    HWindow: HWND;
    wMsg: u_int; addr: PChar;
    len, Struct: Integer;
    buf: PChar;
    buflen: Integer): THandle;
begin
    if @FWSAAsyncGetHostByAddr = nil then
        @FWSAAsyncGetHostByAddr := WSocketGetProc('WSAAsyncGetHostByAddr');
    Result := FWSAAsyncGetHostByAddr(HWindow, wMsg, addr, len, struct, buf, buflen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_WSAAsyncSelect(
    s: TSocket;
    HWindow: HWND;
    wMsg: u_int;
    lEvent: Longint): Integer;
begin
    if @FWSAAsyncSelect = nil then
        @FWSAAsyncSelect := WSocketGetProc('WSAAsyncSelect');
    Result := FWSAAsyncSelect(s, HWindow, wMsg, lEvent);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_getservbyname(name, proto: PChar): PServEnt;
begin
    if @Fgetservbyname = nil then
        @Fgetservbyname := WSocketGetProc('getservbyname');
    Result := Fgetservbyname(name, proto);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_getprotobyname(const Name: String): PProtoEnt;
begin
    if @Fgetprotobyname = nil then
        @Fgetprotobyname := WSocketGetProc('getprotobyname');
    Result := Fgetprotobyname(PChar(Name));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_gethostbyname(name: PChar): PHostEnt;
begin
    if @Fgethostbyname = nil then
        @Fgethostbyname := WSocketGetProc('gethostbyname');
    Result := Fgethostbyname(name);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_gethostbyaddr(addr: Pointer; len, Struct: Integer): PHostEnt;
begin
    if @Fgethostbyaddr = nil then
        @Fgethostbyaddr := WSocketGetProc('gethostbyaddr');
    Result := Fgethostbyaddr(addr, len, Struct);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_gethostname(name: PChar; len: Integer): Integer;
begin
    if @Fgethostname = nil then
        @Fgethostname := WSocketGetProc('gethostname');
    Result := Fgethostname(name, len);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_socket(af, Struct, protocol: Integer): TSocket;
begin
    if @FOpenSocket= nil then
        @FOpenSocket := WSocketGetProc('socket');
    Result := FOpenSocket(af, Struct, protocol);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_shutdown(s: TSocket; how: Integer): Integer;
begin
    if @FShutdown = nil then
        @FShutdown := WSocketGetProc('shutdown');
    Result := FShutdown(s, how);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_setsockopt(s: TSocket; level, optname: Integer; optval: PChar;
                            optlen: Integer): Integer; overload;
begin
    if @FSetSockOpt = nil then
        @FSetSockOpt := WSocketGetProc('setsockopt');
    Result := FSetSockOpt(s, level, optname, optval, optlen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_setsockopt(s: TSocket; level, optname: Integer; var optval: TLinger;
                            optlen: Integer): Integer; overload;
begin
    if @FSetSockOpt = nil then
        @FSetSockOpt := WSocketGetProc('setsockopt');
    Result := FSetSockOpt(s, level, optname, @optval, optlen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_setsockopt(s: TSocket; level, optname: Integer; var optval: ip_mreq;
                            optlen: Integer): Integer; overload;
begin
    if @FSetSockOpt = nil then
        @FSetSockOpt := WSocketGetProc('setsockopt');
    Result := FSetSockOpt(s, level, optname, @optval, optlen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_setsockopt(s: TSocket; level, optname: Integer; var optval: Integer;
                            optlen: Integer): Integer; overload;
begin
    if @FSetSockOpt = nil then
        @FSetSockOpt := WSocketGetProc('setsockopt');
    Result := FSetSockOpt(s, level, optname, @optval, optlen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_setsockopt(s: TSocket; level, optname: Integer; var optval: TInAddr;
                            optlen: Integer): Integer; overload;
begin
    if @FSetSockOpt = nil then
        @FSetSockOpt := WSocketGetProc('setsockopt');
    Result := FSetSockOpt(s, level, optname, @optval, optlen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_getsockopt(
    s: TSocket; level, optname: Integer;
    optval: PChar; var optlen: Integer): Integer;
begin
    if @FGetSockOpt = nil then
        @FGetSockOpt := WSocketGetProc('getsockopt');
    Result := FGetSockOpt(s, level, optname, optval, optlen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_sendto(
    s          : TSocket;
    const Buf  : TWSocketData;
    len, flags : Integer;
    var addrto : TSockAddr;
    tolen      : Integer): Integer;
begin
    if @FSendTo = nil then
        @FSendTo := WSocketGetProc('sendto');
    Result := FSendTo(s, Buf^, len, flags, addrto, tolen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_send(s: TSocket; var Buf : TWSocketData; len, flags: Integer): Integer;
begin
    if @FSend = nil then
        @FSend := WSocketGetProc('send');
    Result := FSend(s, Buf^, len, flags);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_ntohs(netshort: u_short): u_short;
begin
    if @Fntohs = nil then
        @Fntohs := WSocketGetProc('ntohs');
    Result := Fntohs(netshort);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_ntohl(netlong: u_long): u_long;
begin
    if @Fntohl = nil then
        @Fntohl := WSocketGetProc('ntohl');
    Result := Fntohl(netlong);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_listen(s: TSocket; backlog: Integer): Integer;
begin
    if @FListen = nil then
        @FListen := WSocketGetProc('listen');
    Result := FListen(s, backlog);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_ioctlsocket(s: TSocket; cmd: DWORD; var arg: u_long): Integer;
begin
    if @FIoctlSocket = nil then
        @FIoctlSocket := WSocketGetProc('ioctlsocket');
    Result := FIoctlSocket(s, cmd, arg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF COMPILER2_UP}
function WSocket_Synchronized_WSAIoctl(
    s                 : TSocket; IoControlCode : DWORD;
    InBuffer          : Pointer; InBufferSize  : DWORD;
    OutBuffer         : Pointer; OutBufferSize : DWORD;
    var BytesReturned : DWORD; Overlapped      : POverlapped;
    CompletionRoutine : FARPROC): Integer;
begin
    if @FWSAIoctl = nil then
        @FWSAIoctl := WSocket2GetProc('WSAIoctl');
    Result := FWSAIoctl(s, IoControlCode, InBuffer, InBufferSize, OutBuffer,
                        OutBufferSize, BytesReturned, Overlapped, CompletionRoutine);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_inet_ntoa(inaddr: TInAddr): PAnsiChar; 
begin
    if @FInet_ntoa = nil then
        @FInet_ntoa := WSocketGetProc('inet_ntoa');
    Result := FInet_ntoa(inaddr);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_inet_addr(const cp: AnsiString): u_long; 
begin
    if @FInet_addr = nil then
        @FInet_addr := WSocketGetProc('inet_addr');
    Result := FInet_addr(PAnsiChar(cp)); 
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_htons(hostshort: u_short): u_short;
begin
    if @Fhtons = nil then
        @Fhtons := WSocketGetProc('htons');
    Result := Fhtons(hostshort);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_htonl(hostlong: u_long): u_long;
begin
    if @Fhtonl = nil then
        @Fhtonl := WSocketGetProc('htonl');
    Result := Fhtonl(hostlong);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_getsockname(
    s           : TSocket;
    var name    : TSockAddr;
    var namelen : Integer): Integer;
begin
    if @FGetSockName = nil then
        @FGetSockName := WSocketGetProc('getsockname');
    Result := FGetSockName(s, name, namelen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_getpeername(
    s           : TSocket;
    var name    : TSockAddr;
    var namelen : Integer): Integer;
begin
    if @FGetPeerName = nil then
        @FGetPeerName := WSocketGetProc('getpeername');
    Result := FGetPeerName(s, name, namelen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_connect(
    s        : TSocket;
    var name : TSockAddr;
    namelen  : Integer): Integer;
begin
    if @FConnect= nil then
        @FConnect := WSocketGetProc('connect');
    Result := FConnect(s, name, namelen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_closesocket(s: TSocket): Integer;
begin
    if @FCloseSocket = nil then
        @FCloseSocket := WSocketGetProc('closesocket');
    Result := FCloseSocket(s);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_bind(
    s: TSocket;
    var addr: TSockAddr;
    namelen: Integer): Integer;
begin
    if @FBind = nil then
        @FBind := WSocketGetProc('bind');
    Result := FBind(s, addr, namelen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_accept(
    s: TSocket;
{$IFDEF DELPHI1} { Delphi 1 }
    var addr: TSockAddr;
    var addrlen: Integer): TSocket;
{$ELSE}
{$IFDEF VER90} { Delphi 2 }
    var addr: TSockAddr;
    var addrlen: Integer): TSocket;
{$ELSE}{ Delphi 3/4/5, Bcb 1/3/4 }
    addr: PSockAddr;
    addrlen: PInteger): TSocket;
{$ENDIF}
{$ENDIF}
begin
    if @FAccept = nil then
        @FAccept := WSocketGetProc('accept');
    Result := FAccept(s, addr, addrlen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_recv(s: TSocket; var Buf: TWSocketData; len, flags: Integer): Integer;
begin
    if @FRecv= nil then
        @FRecv := WSocketGetProc('recv');
    Result := FRecv(s, Buf^, len, flags);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_recvfrom(
    s: TSocket;
    var Buf: TWSocketData; len, flags: Integer;
    var from: TSockAddr;
    var fromlen: Integer): Integer;
begin
    if @FRecvFrom = nil then
        @FRecvFrom := WSocketGetProc('recvfrom');
    Result := FRecvFrom(s, Buf^, len, flags, from, fromlen);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Winsock is dynamically loaded and unloaded when needed. In some cases     }
{ you may find winsock being loaded and unloaded very often in your app     }
{ This happend for example when you dynamically create a TWSocket and       }
{ destroy a TWSocket when there is no "permanant" TWSocket (that is a       }
{ TWSocket dropped on a persitant form). It is the very inefficiant.        }
{ Calling WSocketForceLoadWinsock will increament the reference count so    }
{ that winsock will not be unloaded when the last TWSocket is destroyed.    }
procedure WSocketForceLoadWinsock;
begin
{$IFDEF WIN32}
{$IFDEF COMPILER2_UP}
    EnterCriticalSection(GWSockCritSect);
    try
{$ENDIF}
        if not WSocketGForced then begin
            WSocketGForced := TRUE;
            Inc(WSocketGCount);
            WSocketGetProc('');
        end;
{$IFDEF COMPILER2_UP}
    finally
        LeaveCriticalSection(GWSockCritSect);
    end;
{$ENDIF}
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Cancel the operation done with WSocketForceLoadWinsock.                   }
procedure WSocketCancelForceLoadWinsock;
begin
{$IFDEF WIN32}
{$IFDEF COMPILER2_UP}
    EnterCriticalSection(GWSockCritSect);
    try
{$ENDIF}
        if WSocketGForced then begin
            WSocketGForced := FALSE;
            Dec(WSocketGCount);
            if WSocketGCount <= 0 then
                WSocketUnloadWinsock;
        end;
{$IFDEF COMPILER2_UP}
    finally
        LeaveCriticalSection(GWSockCritSect);
    end;
{$ENDIF}
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure WSocketUnloadWinsock;
begin
{$IFDEF WIN32}
{$IFDEF NEVER}   { 14/02/99 }
    if DllStarted then begin
        DllStarted := FALSE;
        WSocket_WSACleanup;
    end;
{$ENDIF}
{$IFDEF COMPILER2_UP}
    EnterCriticalSection(GWSockCritSect);
    try
{$ENDIF}
        if (FDllHandle <> 0) and (WSocketGCount = 0) then begin
            WSocket_Synchronized_WSACleanup;
{$IFDEF COMPILER2_UP}
            if FDll2Handle <> 0 then begin
                FreeLibrary(FDll2Handle);
                FDll2Handle        := 0;
                FWSAIoctl          := nil;
            end;
{$ENDIF}
            FreeLibrary(FDllHandle);
            FDllHandle             := 0;
            FWSAStartup            := nil;
            FWSACleanup            := nil;
            FWSASetLastError       := nil;
            FWSAGetLastError       := nil;
            FWSACancelAsyncRequest := nil;
            FWSAAsyncGetHostByName := nil;
            FWSAAsyncGetHostByAddr := nil;
            FWSAAsyncSelect        := nil;
            FGetServByName         := nil;
            FGetProtoByName        := nil;
            FGetHostByName         := nil;
            FGetHostByAddr         := nil;
            FGetHostName           := nil;
            FOpenSocket            := nil;
            FShutdown              := nil;
            FSetSockOpt            := nil;
            FGetSockOpt            := nil;
            FSendTo                := nil;
            FSend                  := nil;
            FRecv                  := nil;
            FRecvFrom              := nil;
            Fntohs                 := nil;
            Fntohl                 := nil;
            FListen                := nil;
            FIoctlSocket           := nil;
{$IFDEF COMPILER2_UP}
            FWSAIoctl              := nil;
{$ENDIF}
            FInet_ntoa             := nil;
            FInet_addr             := nil;
            Fhtons                 := nil;
            Fhtonl                 := nil;
            FGetSockName           := nil;
            FGetPeerName           := nil;
            FConnect               := nil;
            FCloseSocket           := nil;
            FBind                  := nil;
            FAccept                := nil;
        end;
        WSocketGForced := FALSE;
{$IFDEF COMPILER2_UP}
    finally
        LeaveCriticalSection(GWSockCritSect);
    end;
{$ENDIF}
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF WIN32}
function WSocketGetProc(const ProcName : AnsiString) : Pointer;
{$IFDEF DELPHI1}
var
    Error     : THandle;
    Buf       : String;
    LastError : LongInt;
begin
    if FDllHandle = 0 then begin
       { Delphi 1 strings are not nul terminated }
        Buf := winsocket + #0;
        FDllHandle := LoadLibrary(@Buf[1]);
        if FDllHandle < HINSTANCE_ERROR then begin
            Error      := FDllHandle;
            FDllHandle := 0;
        {    raise ESocketException.Create('Unable to load ' + winsocket +
                                          ' Error #' + IntToStr(Error));  }
            raise ESocketException.Create('Unable to load ' + winsocket +
                                     ' - ' + GetWindowsErr (Error)); { V5.26 }
        end;
{$IFDEF DELPHI1}
        { Delphi 1 support only Winsock 1.01 }
        LastError := WSocket_WSAStartup($101, GInitData);
{$ELSE}
        LastError := WSocket_WSAStartup(MAKEWORD(GReqVerLow, GReqVerHigh) { $202 $101}, GInitData);
{$ENDIF}
        if LastError <> 0 then begin
          {  raise ESocketException.CreateFmt('%s: WSAStartup error #%d',
                                             [winsocket, LastError]);  }
            raise ESocketException.Create('Winsock startup error ' + winsocket +
                                  ' - ' + GetWindowsErr (LastError)); { V5.26 }
        end;
    end;
    if Length(ProcName) = 0 then
        Result := nil
    else begin
        { Delphi 1 strings are not nul terminated }
        Buf := ProcName + #0;
        Result := GetProcAddress(FDllHandle, @Buf[1]);
        if Result = nil then
            raise ESocketException.Create('Procedure ' + ProcName +
               ' not found in ' + winsocket + ' - ' + GetWindowsErr (GetLastError)); { V5.26 }
    end;
end;
{$ELSE}
var
    LastError : LongInt;
begin
    { Prevents compiler warning "Return value might be undefined"  }
    Result := nil;

    EnterCriticalSection(GWSockCritSect);
    try
        if FDllHandle = 0 then begin
            FDllHandle := LoadLibrary(@winsocket[1]);
            if FDllHandle = 0 then
             {   raise ESocketException.Create('Unable to load ' + winsocket +
                                              ' Error #' + IntToStr(GetLastError));}
                raise ESocketException.Create('Unable to load ' + winsocket +
                              ' - ' + GetWindowsErr (GetLastError)); { V5.26 }
            LastError := WSocket_Synchronized_WSAStartup(MAKEWORD(GReqVerLow, GReqVerHigh) { $202 $101}, GInitData);
            if LastError <> 0 then begin
              {  raise ESocketException.CreateFmt('%s: WSAStartup error #%d',
                                                 [winsocket, LastError]);  }
                raise ESocketException.Create('Winsock startup error ' + winsocket +
                                     ' - ' + GetWindowsErr (LastError)); { V5.26 }
            end;
        end;
        if Length(ProcName) = 0 then
            Result := nil
        else begin
            Result := GetProcAddress(FDllHandle, @ProcName[1]);
            if Result = nil then
                raise ESocketException.Create('Procedure ' + ProcName +
                                              ' not found in ' + winsocket +
                                   ' - ' + GetWindowsErr (GetLastError)); { V5.26 }
        end;
    finally
        LeaveCriticalSection(GWSockCritSect);
    end;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF COMPILER2_UP}
function WSocket2GetProc(const ProcName : AnsiString) : Pointer;
begin
    { Prevents compiler warning "Return value might be undefined"  }
    Result := nil;

    EnterCriticalSection(GWSockCritSect);
    try
        if FDll2Handle = 0 then begin
            { Be sure to have main winsock.dll loaded }
            if FDllHandle = 0 then
                WSocketGetProc('');
            FDll2Handle := LoadLibrary(@winsocket2[1]);
            if FDll2Handle = 0 then
              {  raise ESocketException.Create('Unable to load ' + winsocket2 +
                                              ' Error #' + IntToStr(GetLastError));  }
                raise ESocketException.Create('Unable to load ' + winsocket2 +
                              ' - ' + GetWindowsErr (GetLastError)); { V5.26 }
        end;
        if Length(ProcName) = 0 then
            Result := nil
        else begin
            Result := GetProcAddress(FDll2Handle, @ProcName[1]);
            if Result = nil then
                raise ESocketException.Create('Procedure ' + ProcName +
                                              ' not found in ' + winsocket2 +
                                ' - ' + GetWindowsErr (GetLastError)); { V5.26 }
        end;
    finally
        LeaveCriticalSection(GWSockCritSect);
    end;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WinsockInfo : TWSADATA;
begin
{    LoadWinsock(winsocket); 14/02/99 }
    { Load winsock and initialize it as needed }
{$IFDEF COMPILER2_UP}
    EnterCriticalSection(GWSockCritSect);
    try
{$ENDIF}
        WSocketGetProc('');
        Result := GInitData;
        { If no socket created, then unload winsock immediately }
        if WSocketGCount <= 0 then
            WSocketUnloadWinsock;
{$IFDEF COMPILER2_UP}
    finally
        LeaveCriticalSection(GWSockCritSect);
    end;
{$ENDIF}
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF CLR}
function WSocket_Synchronized_WSAStartup(
    wVersionRequired : Word;
    out WSData       : TWSAData): Integer;
begin
    if GWSAStartupCalled then
        Result := 0
    else begin
        Result := OverByteIcsWinsock.WSAStartup(wVersionRequired, WSData);
        GWSAStartupCalled := TRUE;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_WSACleanup : Integer;
begin
    Result := OverByteIcsWinsock.WSACleanup;
    GWSAStartupCalled := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure WSocket_Synchronized_WSASetLastError(ErrCode: Integer);
begin
    if not GWSAStartupCalled then
        WSocket_Synchronized_WSAStartup(MAKEWORD(GReqVerLow, GReqVerHigh), GInitData);
    OverByteIcsWinsock.WsaSetLastError(ErrCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_WSAGetLastError: Integer;
begin
    if not GWSAStartupCalled then
        WSocket_Synchronized_WSAStartup(MAKEWORD(GReqVerLow, GReqVerHigh), GInitData);
    Result := OverByteIcsWinsock.WSAGetLastError;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_WSACancelAsyncRequest(hAsyncTaskHandle: THandle): Integer;
begin
    if not GWSAStartupCalled then
        WSocket_Synchronized_WSAStartup(MAKEWORD(GReqVerLow, GReqVerHigh), GInitData);
    Result := OverByteIcsWinsock.WSACancelAsyncRequest(hAsyncTaskHandle);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_WSAAsyncGetHostByName(
    HWindow: HWND; wMsg: u_int;
    const name : String; buf: IntPtr;
    buflen: Integer): THandle;
begin
    if not GWSAStartupCalled then
        WSocket_Synchronized_WSAStartup(MAKEWORD(GReqVerLow, GReqVerHigh), GInitData);
    Result := OverByteIcsWinsock.WSAAsyncGetHostByName(
                  HWindow, wMsg, name, buf, buflen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_WSAAsyncGetHostByAddr(
    HWindow: HWND;
    wMsg: u_int; var addr: u_long;
    len, Struct: Integer;
    buf: IntPtr;
    buflen: Integer): THandle;
begin
    if not GWSAStartupCalled then
        WSocket_Synchronized_WSAStartup(MAKEWORD(GReqVerLow, GReqVerHigh), GInitData);
    Result := OverByteIcsWinsock.WSAAsyncGetHostByAddr(
                   HWindow, wMsg, addr, len, struct, buf, buflen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_WSAAsyncSelect(
    S       : TSocket;
    HWindow : HWND;
    wMsg    : u_int;
    lEvent  : Longint): Integer;
begin
    if not GWSAStartupCalled then
        WSocket_Synchronized_WSAStartup(MAKEWORD(GReqVerLow, GReqVerHigh), GInitData);
    Result := OverByteIcsWinsock.WSAAsyncSelect(S, HWindow, wMsg, lEvent);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_getservbyname(const Name, Proto: String): IntPtr;
begin
    if not GWSAStartupCalled then
        WSocket_Synchronized_WSAStartup(MAKEWORD(GReqVerLow, GReqVerHigh), GInitData);
    Result := OverByteIcsWinsock.getservbyname(Name, Proto);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_getprotobyname(const name: String): IntPtr;
begin
    if not GWSAStartupCalled then
        WSocket_Synchronized_WSAStartup(MAKEWORD(GReqVerLow, GReqVerHigh), GInitData);
    Result := OverByteIcsWinsock.getprotobyname(name);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_gethostbyname(const Name: String): IntPtr;
begin
    if not GWSAStartupCalled then
        WSocket_Synchronized_WSAStartup(MAKEWORD(GReqVerLow, GReqVerHigh), GInitData);
    Result := OverByteIcsWinsock.gethostbyname(Name);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_gethostbyaddr(
    var addr: u_long; len, Struct: Integer): IntPtr;
begin
    if not GWSAStartupCalled then
        WSocket_Synchronized_WSAStartup(MAKEWORD(GReqVerLow, GReqVerHigh), GInitData);
    Result := OverByteIcsWinsock.gethostbyaddr(addr, len, Struct);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_gethostname(out Name: String): Integer;
var
    SB: System.Text.StringBuilder;
begin
    if not GWSAStartupCalled then
        WSocket_Synchronized_WSAStartup(MAKEWORD(GReqVerLow, GReqVerHigh), GInitData);
    SB     := System.Text.StringBuilder.Create(256);
    Result := OverByteIcsWinsock.gethostname(SB, SB.Capacity);
    if Result <> 0 then
        Name := ''
    else
        Name := SB.ToString;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_socket(af, Struct, protocol: Integer): TSocket;
begin
    if not GWSAStartupCalled then
        WSocket_Synchronized_WSAStartup(MAKEWORD(GReqVerLow, GReqVerHigh), GInitData);
    Result := OverByteIcsWinsock.socket(af, Struct, protocol);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_shutdown(s: TSocket; how: Integer): Integer;
begin
    if not GWSAStartupCalled then
        WSocket_Synchronized_WSAStartup(MAKEWORD(GReqVerLow, GReqVerHigh), GInitData);
    Result := OverByteIcsWinsock.shutdown(s, how);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_setsockopt(
    s              : TSocket;
    level, optname : Integer;
    var optval     : Integer;
    optlen         : Integer): Integer; overload;
begin
    if not GWSAStartupCalled then
        WSocket_Synchronized_WSAStartup(MAKEWORD(GReqVerLow, GReqVerHigh), GInitData);
    Result := OverByteIcsWinsock.setsockopt_integer(
                  S, Level, OptName, OptVal, 4);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_setsockopt(
    s              : TSocket;
    level, optname : Integer;
    var optval     : ip_mreq;
    optlen         : Integer): Integer; overload;
begin
    if not GWSAStartupCalled then
        WSocket_Synchronized_WSAStartup(MAKEWORD(GReqVerLow, GReqVerHigh), GInitData);
    Result := OverByteIcsWinsock.setsockopt_ip_mreq(
                  S, Level, OptName, OptVal, 8);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_setsockopt(
    s              : TSocket;
    level, optname : Integer;
    var optval     : TInAddr;
    optlen         : Integer): Integer; overload;
begin
    if not GWSAStartupCalled then
        WSocket_Synchronized_WSAStartup(MAKEWORD(GReqVerLow, GReqVerHigh), GInitData);
    Result := OverByteIcsWinsock.setsockopt_tinaddr(
                  S, Level, OptName, OptVal, 4);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_setsockopt(
    s              : TSocket;
    level, optname : Integer;
    var optval     : TLinger;
    optlen         : Integer): Integer; overload;
begin
    if not GWSAStartupCalled then
        WSocket_Synchronized_WSAStartup(MAKEWORD(GReqVerLow, GReqVerHigh), GInitData);
    Result := OverByteIcsWinsock.setsockopt_tlinger(
                  S, Level, OptName, OptVal, 4);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_getsockopt(
    s: TSocket; level, optname: Integer;
    var optval: Integer; var optlen: Integer): Integer; overload;
begin
    if not GWSAStartupCalled then
        WSocket_Synchronized_WSAStartup(MAKEWORD(GReqVerLow, GReqVerHigh), GInitData);
    Result := OverByteIcsWinsock.getsockopt_integer(
                  s, level, optname, optval, optlen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_getsockopt(
    s: TSocket; level, optname: Integer;
    var optval: ip_mreq; var optlen: Integer): Integer; overload;
begin
    if not GWSAStartupCalled then
        WSocket_Synchronized_WSAStartup(MAKEWORD(GReqVerLow, GReqVerHigh), GInitData);
    Result := OverByteIcsWinsock.getsockopt_ip_mreq(
                  s, level, optname, optval, optlen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_getsockopt(
    s: TSocket; level, optname: Integer;
    var optval: TInAddr; var optlen: Integer): Integer; overload;
begin
    if not GWSAStartupCalled then
        WSocket_Synchronized_WSAStartup(MAKEWORD(GReqVerLow, GReqVerHigh), GInitData);
    Result := OverByteIcsWinsock.getsockopt_tinaddr(
                  s, level, optname, optval, optlen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_getsockopt(
    s: TSocket; level, optname: Integer;
    var optval: TLinger; var optlen: Integer): Integer; overload;
begin
    if not GWSAStartupCalled then
        WSocket_Synchronized_WSAStartup(MAKEWORD(GReqVerLow, GReqVerHigh), GInitData);
    Result := OverByteIcsWinsock.getsockopt_tlinger(
                  s, level, optname, optval, optlen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_sendto(
    s          : TSocket;
    const Buf  : TBytes;
    len, flags : Integer;
    var addrto : TSockAddr;
    tolen      : Integer): Integer;
begin
    if not GWSAStartupCalled then
        WSocket_Synchronized_WSAStartup(MAKEWORD(GReqVerLow, GReqVerHigh), GInitData);
    Result := OverByteIcsWinsock.sendto(
                  s, Buf, len, flags, addrto, SizeOfTSockAddr);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_send(
    s: TSocket;
    const Buf : TBytes;
    len, flags: Integer): Integer;
begin
    if not GWSAStartupCalled then
        WSocket_Synchronized_WSAStartup(MAKEWORD(GReqVerLow, GReqVerHigh), GInitData);
    Result := OverByteIcsWinsock.send(s, Buf, len, flags);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_ntohs(netshort: u_short): u_short;
begin
    if not GWSAStartupCalled then
        WSocket_Synchronized_WSAStartup(MAKEWORD(GReqVerLow, GReqVerHigh), GInitData);
    Result := OverByteIcsWinsock.ntohs(netshort);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_ntohl(netlong: u_long): u_long;
begin
    if not GWSAStartupCalled then
        WSocket_Synchronized_WSAStartup(MAKEWORD(GReqVerLow, GReqVerHigh), GInitData);
    Result := OverByteIcsWinsock.ntohl(netlong);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_listen(s: TSocket; backlog: Integer): Integer;
begin
    if not GWSAStartupCalled then
        WSocket_Synchronized_WSAStartup(MAKEWORD(GReqVerLow, GReqVerHigh), GInitData);
    Result := OverByteIcsWinsock.listen(s, backlog);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_ioctlsocket(s: TSocket; cmd: DWORD; var arg: u_long): Integer;
begin
    if not GWSAStartupCalled then
        WSocket_Synchronized_WSAStartup(MAKEWORD(GReqVerLow, GReqVerHigh), GInitData);
    Result := OverByteIcsWinsock.ioctlsocket(s, cmd, arg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_inet_ntoa(inaddr: TInAddr): String;
begin
    if not GWSAStartupCalled then
        WSocket_Synchronized_WSAStartup(MAKEWORD(GReqVerLow, GReqVerHigh), GInitData);
    Result := OverByteIcsWinsock.inet_ntoa(inaddr);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_inet_addr(const cp: String): u_long;
begin
    if not GWSAStartupCalled then
        WSocket_Synchronized_WSAStartup(MAKEWORD(GReqVerLow, GReqVerHigh), GInitData);
    Result := OverByteIcsWinsock.inet_addr(cp);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_htons(hostshort: u_short): u_short;
begin
    if not GWSAStartupCalled then
        WSocket_Synchronized_WSAStartup(MAKEWORD(GReqVerLow, GReqVerHigh), GInitData);
    Result := OverByteIcsWinsock.htons(hostshort);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_htonl(hostlong: u_long): u_long;
begin
    if not GWSAStartupCalled then
        WSocket_Synchronized_WSAStartup(MAKEWORD(GReqVerLow, GReqVerHigh), GInitData);
    Result := OverByteIcsWinsock.htonl(hostlong);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_getsockname(
    S           : TSocket;
    var Name    : TSockAddr;
    var NameLen : Integer): Integer;
var
    APINameLen : Integer;
begin
    APINameLen := SizeOfTSockAddr;
    if not GWSAStartupCalled then
        WSocket_Synchronized_WSAStartup(MAKEWORD(GReqVerLow, GReqVerHigh), GInitData);
    Result     := OverByteIcsWinsock.getsockname(S, Name, APINameLen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_getpeername(
    S           : TSocket;
    out Name    : TSockAddr;
    var NameLen : Integer): Integer;
var
    APINameLen : Integer;
begin
    APINameLen := SizeOfTSockAddr;
    if not GWSAStartupCalled then
        WSocket_Synchronized_WSAStartup(MAKEWORD(GReqVerLow, GReqVerHigh), GInitData);
    Result     := OverByteIcsWinsock.getpeername(S, Name, APINameLen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_connect(
    S        : TSocket;
    var Name : TSockAddr;
    NameLen  : Integer): Integer;
begin
    if not GWSAStartupCalled then
        WSocket_Synchronized_WSAStartup(MAKEWORD(GReqVerLow, GReqVerHigh), GInitData);
    Result := OverByteIcsWinsock.connect(S, Name, SizeOfTSockAddr);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_closesocket(s: TSocket): Integer;
begin
    if not GWSAStartupCalled then
        WSocket_Synchronized_WSAStartup(MAKEWORD(GReqVerLow, GReqVerHigh), GInitData);
    Result := OverByteIcsWinsock.closesocket(s);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_bind(
    S        : TSocket;
    var Addr : TSockAddr;
    NameLen  : Integer): Integer;
begin
    if not GWSAStartupCalled then
        WSocket_Synchronized_WSAStartup(MAKEWORD(GReqVerLow, GReqVerHigh), GInitData);
    Result := OverByteIcsWinsock.bind(S, Addr, SizeOfTSockAddr);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_accept(
    s           : TSocket;
    var addr    : TSockAddr;
    var addrlen : Integer): TSocket;
begin
    if not GWSAStartupCalled then
        WSocket_Synchronized_WSAStartup(MAKEWORD(GReqVerLow, GReqVerHigh), GInitData);
    addrlen := SizeOfTSockAddr;
    Result := OverByteIcsWinsock.accept(s, addr, addrlen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_recv(
    S          : TSocket;
    out Buf    : TBytes;
    Len, Flags : Integer): Integer;
begin
    if not GWSAStartupCalled then
        WSocket_Synchronized_WSAStartup(MAKEWORD(GReqVerLow, GReqVerHigh), GInitData);
    Result := OverByteIcsWinsock.recv(s, Buf, len, flags);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_recvfrom(
    S           : TSocket;
    out Buf     : TBytes;
    Len, Flags  : Integer;
    var From    : TSockAddr;
    var FromLen : Integer): Integer;
var
    APIFromLen : Integer;
begin
    if not GWSAStartupCalled then
        WSocket_Synchronized_WSAStartup(MAKEWORD(GReqVerLow, GReqVerHigh), GInitData);
    APIFromLen := SizeOfTSockAddr;
    Result     := OverByteIcsWinsock.recvfrom(
                      S, Buf, Len, Flags, From, APIFromLen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WinsockInfo : TWSADATA;
begin
    if not GWSAStartupCalled then
        WSocket_Synchronized_WSAStartup(MAKEWORD(GReqVerLow, GReqVerHigh), GInitData);
    Result := GInitData;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF NO_ADV_MT}
procedure SafeIncrementCount;
begin
    EnterCriticalSection(GWSockCritSect);
    Inc(WSocketGCount);
    LeaveCriticalSection(GWSockCritSect);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure SafeDecrementCount;
begin
    EnterCriticalSection(GWSockCritSect);
    Dec(WSocketGCount);
    LeaveCriticalSection(GWSockCritSect);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_WSAStartup(
    wVersionRequired : WORD;
    var WSData       : TWSAData): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_WSAStartup(wVersionRequired, WSData);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_WSACleanup : Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_WSACleanup;
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure WSocket_WSASetLastError(iError: Integer);
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        WSocket_Synchronized_WSASetLastError(iError);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_WSAGetLastError: Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_WSAGetLastError;
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_WSACancelAsyncRequest(hAsyncTaskHandle: THandle): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_WSACancelAsyncRequest(hAsyncTaskHandle);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF CLR}
function WSocket_WSAAsyncGetHostByName(
    HWindow: HWND; wMsg: u_int;
    const name : String; buf: IntPtr;
    buflen: Integer): THandle;
begin
    Result := OverByteIcsWinsock.WSAAsyncGetHostByName(
                  HWindow, wMsg, name, buf, buflen);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF CLR}
function WSocket_WSAAsyncGetHostByAddr(
    HWindow: HWND;
    wMsg: u_int; var addr: u_long;
    len, Struct: Integer;
    buf: IntPtr;
    buflen: Integer): THandle;
begin
    Result := OverByteIcsWinsock.WSAAsyncGetHostByAddr(
                   HWindow, wMsg, addr, len, struct, buf, buflen);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF WIN32}
function WSocket_WSAAsyncGetHostByName(
    HWindow: HWND; wMsg: u_int;
    name, buf: PAnsiChar;
    buflen: Integer): THandle;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_WSAAsyncGetHostByName(
                      HWindow, wMsg, name, buf, buflen);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF WIN32}
function WSocket_WSAAsyncGetHostByAddr(
    HWindow: HWND;
    wMsg: u_int; addr: PChar;
    len, Struct: Integer;
    buf: PChar;
    buflen: Integer): THandle;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_WSAAsyncGetHostByAddr(
                       HWindow, wMsg, addr, len, struct, buf, buflen);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_WSAAsyncSelect(
    s: TSocket;
    HWindow: HWND;
    wMsg: u_int;
    lEvent: Longint): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_WSAAsyncSelect(
                      s, HWindow, wMsg, lEvent);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF CLR}
function WSocket_getservbyname(const Name, Proto: String): IntPtr;
begin
    Result := OverByteIcsWinsock.getservbyname(Name, Proto);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_getprotobyname(const name: String): IntPtr;
begin
    Result := OverByteIcsWinsock.getprotobyname(name);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_gethostbyname(const Name: String): IntPtr;
begin
    Result := OverByteIcsWinsock.gethostbyname(Name);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_gethostbyaddr(
    var addr: u_long; len, Struct: Integer): IntPtr;
begin
    Result := OverByteIcsWinsock.gethostbyaddr(addr, len, Struct);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_gethostname(out Name: String): Integer;
var
    SB: System.Text.StringBuilder;
begin
    SB     := System.Text.StringBuilder.Create(256);
    Result := OverByteIcsWinsock.gethostname(SB, SB.Capacity);
    if Result <> 0 then
        Name := ''
    else
        Name := SB.ToString;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$ENDIF}
{$IFDEF WIN32}
function WSocket_getservbyname(name, proto: PChar): PServEnt;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_getservbyname(name, proto);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_getprotobyname(name: PChar): PProtoEnt;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_getprotobyname(name);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_gethostbyname(name: PChar): PHostEnt;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_gethostbyname(name);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_gethostbyaddr(addr: Pointer; len, Struct: Integer): PHostEnt;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_gethostbyaddr(addr, len, Struct);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_gethostname(out name: String): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        SetLength(Name, 256);
        Result := WSocket_Synchronized_gethostname(PChar(name), 256);
        if Result >= 0 then
            // Unicode will convert on the fly
            SetLength(Name, StrLen(PAnsiChar(AnsiString(Name)))) // Unicode change
        else
            SetLength(Name, 0);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_socket(af, Struct, protocol: Integer): TSocket;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_socket(af, Struct, protocol);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_shutdown(s: TSocket; how: Integer): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_Shutdown(s, how);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF CLR}
function WSocket_setsockopt(
    s              : TSocket;
    level, optname : Integer;
    var optval     : Integer;
    optlen         : Integer): Integer; overload;
begin
    Result := OverByteIcsWinsock.setsockopt_integer(
                  S, Level, OptName, OptVal, 4);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_setsockopt(
    s              : TSocket;
    level, optname : Integer;
    var optval     : ip_mreq;
    optlen         : Integer): Integer; overload;
begin
    Result := OverByteIcsWinsock.setsockopt_ip_mreq(
                  S, Level, OptName, OptVal, 8);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_setsockopt(
    s              : TSocket;
    level, optname : Integer;
    var optval     : TInAddr;
    optlen         : Integer): Integer; overload;
begin
    Result := OverByteIcsWinsock.setsockopt_tinaddr(
                  S, Level, OptName, OptVal, 4);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_setsockopt(
    s              : TSocket;
    level, optname : Integer;
    var optval     : TLinger;
    optlen         : Integer): Integer; overload;
begin
    Result := OverByteIcsWinsock.setsockopt_tlinger(
                  S, Level, OptName, OptVal, 4);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF WIN32}
function WSocket_setsockopt(s: TSocket; level, optname: Integer; optval: PChar;
                            optlen: Integer): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_setsockopt(s, level, optname, optval, optlen);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_setsockopt(s: TSocket; level, optname: Integer; var optval: TLinger;
                            optlen: Integer): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_setsockopt(s, level, optname, optval, optlen);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF CLR}
function WSocket_getsockopt(
    s: TSocket; level, optname: Integer;
    var optval: Integer; var optlen: Integer): Integer;
begin
    Result := OverByteIcsWinsock.getsockopt_integer(
                  s, level, optname, optval, optlen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_getsockopt(
    s: TSocket; level, optname: Integer;
    var optval: ip_mreq; var optlen: Integer): Integer;
begin
    Result := OverByteIcsWinsock.getsockopt_ip_mreq(
                  s, level, optname, optval, optlen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_getsockopt(
    s: TSocket; level, optname: Integer;
    var optval: TInAddr; var optlen: Integer): Integer;
begin
    Result := OverByteIcsWinsock.getsockopt_tinaddr(
                  s, level, optname, optval, optlen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_getsockopt(
    s: TSocket; level, optname: Integer;
    var optval: TLinger; var optlen: Integer): Integer;
begin
    Result := OverByteIcsWinsock.getsockopt_tlinger(
                  s, level, optname, optval, optlen);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF WIN32}
function WSocket_getsockopt(
    s: TSocket; level, optname: Integer;
    optval: PChar; var optlen: Integer): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_getsockopt(s, level, optname, optval, optlen);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_sendto(
    s          : TSocket;
    var Buf    : TWSocketData;
    len, flags : Integer;
    var addrto : TSockAddr;
    tolen      : Integer): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_sendto(s, Buf, len, flags, addrto, tolen);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_send(s: TSocket; var Buf : TWSocketData; len, flags: Integer): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_send(s, Buf, len, flags);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_ntohs(netshort: u_short): u_short;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_ntohs(netshort);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_ntohl(netlong: u_long): u_long;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_ntohl(netlong);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_listen(s: TSocket; backlog: Integer): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_listen(s, backlog);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_ioctlsocket(s: TSocket; cmd: DWORD; var arg: u_long): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_ioctlsocket(s, cmd, arg);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF WIN32}
{$IFDEF COMPILER2_UP}
function WSocket_WSAIoctl(
    s                 : TSocket; IoControlCode : DWORD;
    InBuffer          : Pointer; InBufferSize  : DWORD;
    OutBuffer         : Pointer; OutBufferSize : DWORD;
    var BytesReturned : DWORD; Overlapped      : POverlapped;
    CompletionRoutine : FARPROC): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_WSAIoctl(
                      s, IoControlCode, InBuffer, InBufferSize, OutBuffer,
                      OutBufferSize, BytesReturned, Overlapped, CompletionRoutine);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;
{$ENDIF}
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF CLR}
function WSocket_inet_ntoa(inaddr: TInAddr): String;
begin
    Result := OverByteIcsWinsock.inet_ntoa(inaddr);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF WIN32}
function WSocket_inet_ntoa(inaddr: TInAddr): String;   
var
    Temp : PAnsiChar;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Temp := WSocket_Synchronized_inet_ntoa(inaddr);
        if Temp = nil then
            Result := ''
        else
            Result := Temp;
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_inet_addr(const cp: AnsiString): u_long;  
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_inet_addr(cp);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_htons(hostshort: u_short): u_short;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_htons(hostshort);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_htonl(hostlong: u_long): u_long;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_htonl(hostlong);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_getsockname(
    s           : TSocket;
    var name    : TSockAddr;
    var namelen : Integer): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_getsockname(s, name, namelen);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_getpeername(
    s           : TSocket;
    var name    : TSockAddr;
    var namelen : Integer): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_getpeername(s, name, namelen);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_connect(
    s        : TSocket;
    var name : TSockAddr;
    namelen  : Integer): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_connect(s, name, namelen);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_closesocket(s: TSocket): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_closesocket(s);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_bind(
    s: TSocket;
    var addr: TSockAddr;
    namelen: Integer): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_bind(s, addr, namelen);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_accept(
    s: TSocket;
{$IFDEF DELPHI1} { Delphi 1 }
    var addr: TSockAddr;
    var addrlen: Integer): TSocket;
{$ELSE}
{$IFDEF VER90} { Delphi 2 }
    var addr: TSockAddr;
    var addrlen: Integer): TSocket;
{$ELSE}{ Delphi 3/4/5, Bcb 1/3/4 }
{$IFDEF CLR}
    var addr: TSockAddr;
    var addrlen: Integer): TSocket;
{$ELSE}
    addr: PSockAddr;
    addrlen: PInteger): TSocket;
{$ENDIF}
{$ENDIF}
{$ENDIF}
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_accept(s, addr, addrlen);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_recv(s: TSocket; var Buf : TWSocketData; len, flags: Integer): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_recv(s, Buf, len, flags);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_recvfrom(
    s: TSocket;
    var Buf: TWSocketData; len, flags: Integer;
    var from: TSockAddr;
    var fromlen: Integer): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_recvfrom(s, Buf, len, flags, from, fromlen);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF WIN32}
procedure TCustomWSocket.Notification(AComponent: TComponent; operation: TOperation);
begin
    inherited Notification(AComponent, operation);
    if operation = opRemove then begin
    {$IFNDEF NO_DEBUG_LOG}
        if AComponent = FIcsLogger then                               { V5.21 }
            FIcsLogger := nil;                                        { V5.21 }
    {$ENDIF}                                                          { V5.21 }
    end;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.AssignDefaultValue;
begin
    //FillChar(sin, Sizeof(sin), 0);
    sin.sin_family      := AF_INET;
    sin.sin_port        := 0;
    sin.sin_addr.S_addr := 0;
    FAddrFormat         := PF_INET;

    FPortAssigned       := FALSE;
    FAddrAssigned       := FALSE;
    FAddrResolved       := FALSE;
    FPortResolved       := FALSE;
    FProtoResolved      := FALSE;
    FLocalPortResolved  := FALSE;

    FProtoAssigned      := TRUE;
    FProto              := IPPROTO_TCP;
    FProtoStr           := 'tcp';
    FType               := SOCK_STREAM;
    FLocalPortStr       := '0';
    FLocalAddr          := '0.0.0.0';

    FLingerOnOff        := wsLingerOn;
    FLingerTimeout      := 0;
    FHSocket            := INVALID_SOCKET;
    FSelectEvent        := 0;
    FState              := wsClosed;
    bAllSent            := TRUE;
    FPaused             := FALSE;
    FReadCount          := 0;
    FCloseInvoked       := FALSE;
    FFlushTimeout       := 60;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ All exceptions *MUST* be handled. If an exception is not handled, the     }
{ application will be shut down !                                           }
procedure TCustomWSocket.HandleBackGroundException(E: Exception);
var
    CanAbort : Boolean;
begin
    CanAbort := TRUE;
    { First call the error event handler, if any }
    if Assigned(FOnBgException) then begin
        try
            FOnBgException(Self, E, CanAbort);
        except
        end;
    end;
    { Then abort the socket }
    if CanAbort then begin
        try
            Abort;
        except
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This procedure handle all messages for TWSocket. All exceptions must be   }
{ handled or the application will be shutted down !                         }
{ If WndProc is overriden in descendent components, then the same exception }
{ handling *MUST* be setup because descendent component code is executed    }
{ before the base class code.                                               }
procedure TCustomWSocket.WndProc(var MsgRec: TMessage);
begin
    try
        with MsgRec do begin
            if Msg = FMsg_WM_ASYNCSELECT then
                WMASyncSelect(MsgRec)
            else if Msg = FMsg_WM_ASYNCGETHOSTBYNAME then
                WMAsyncGetHostByName(MsgRec)
            else if Msg = FMsg_WM_ASYNCGETHOSTBYADDR then
                WMAsyncGetHostByAddr(MsgRec)
            else if Msg = FMsg_WM_CLOSE_DELAYED then
                WMCloseDelayed(MsgRec)
//            else if Msg = FMsg_WM_WSOCKET_RELEASE then
//                WMRelease(MsgRec)
            else if Msg = FMsg_WM_TRIGGER_EXCEPTION then
                { This is useful to check for background exceptions            }
                { In your application, use following code to test your handler }
                { PostMessage(WSocket1.Handle, WM_TRIGGER_EXCEPTION, 0, 0);    }
                raise ESocketException.Create('Test exception in WSocket')
            else
                inherited WndProc(MsgRec);
                //Result := DefWindowProc(Handle, Msg, wParam, LParam);
        end;
    except
        on E:Exception do
            HandleBackGroundException(E);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.MsgHandlersCount : Integer;
begin
    Result := 6 + inherited MsgHandlersCount;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.AllocateMsgHandlers;
begin
    inherited AllocateMsgHandlers;
    FMsg_WM_ASYNCSELECT            := FWndHandler.AllocateMsgHandler(Self);
    FMsg_WM_ASYNCGETHOSTBYNAME     := FWndHandler.AllocateMsgHandler(Self);
    FMsg_WM_ASYNCGETHOSTBYADDR     := FWndHandler.AllocateMsgHandler(Self);
    FMsg_WM_CLOSE_DELAYED          := FWndHandler.AllocateMsgHandler(Self);
    FMsg_WM_TRIGGER_EXCEPTION      := FWndHandler.AllocateMsgHandler(Self);
    FMsg_WM_TRIGGER_DATA_AVAILABLE := FWndHandler.AllocateMsgHandler(Self);
//  FMsg_WM_WSOCKET_RELEASE        := FWndHandler.AllocateMsgHandler(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.FreeMsgHandlers;
begin
    if Assigned(FWndHandler) then begin
        FWndHandler.UnregisterMessage(FMsg_WM_ASYNCSELECT);
        FWndHandler.UnregisterMessage(FMsg_WM_ASYNCGETHOSTBYNAME);
        FWndHandler.UnregisterMessage(FMsg_WM_ASYNCGETHOSTBYADDR);
        FWndHandler.UnregisterMessage(FMsg_WM_CLOSE_DELAYED);
        FWndHandler.UnregisterMessage(FMsg_WM_TRIGGER_EXCEPTION);
        FWndHandler.UnregisterMessage(FMsg_WM_TRIGGER_DATA_AVAILABLE);
    end;
    inherited FreeMsgHandlers;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.AllocateSocketHWnd;
begin
    inherited AllocateHWnd;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.DeallocateSocketHWnd;
begin
    inherited DeallocateHWnd;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF COMPILER2_UP}
procedure TCustomWSocket.ThreadAttach;
begin
    inherited ThreadAttach;
    if FHSocket <> INVALID_SOCKET then
        WSocket_Synchronized_WSAASyncSelect(FHSocket, Handle,
                                            FMsg_WM_ASYNCSELECT, FSelectEvent);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.ThreadDetach;
begin
    if (GetCurrentThreadID = DWORD(FThreadID)) and (FHSocket <> INVALID_SOCKET) then
        WSocket_Synchronized_WSAASyncSelect(FHSocket, Handle, 0, 0);
    inherited ThreadDetach;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TCustomWSocket.Create{$IFDEF VCL}(AOwner: TComponent){$ENDIF};
begin
    inherited Create{$IFDEF VCL}(AOwner){$ENDIF};

    FHSocket            := INVALID_SOCKET;           { FP: 18/01/2007 }
    AllocateSocketHWnd;
    FBufHandler         := TIcsBufferHandler.Create(Self);
    FBufHandler.BufSize := 1460; {1514;}             { Default buffer size }
    FDnsResultList      := TStringList.Create;
    FMultiCastIpTTL     := IP_DEFAULT_MULTICAST_TTL;
    ListenBacklog       := 5;
    FBufferedByteCount  := 0;  { V5.20 }
    FMultiCastAddrStr   := '';
    FAddrStr            := '';
    FPortStr            := '';
    AssignDefaultValue;

{$IFDEF COMPILER2_UP}
    EnterCriticalSection(GWSockCritSect);
    try
{$ENDIF}
        Inc(WSocketGCount);
{$IFDEF COMPILER2_UP}
    finally
        LeaveCriticalSection(GWSockCritSect);
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TCustomWSocket.Destroy;
begin
    try
        CancelDnsLookup;             { Cancel any pending dns lookup      }
    except
        { Ignore any exception here }
    end;

    if FState <> wsClosed then       { Close the socket if not yet closed }
        Close;

{$IFDEF COMPILER2_UP}
    EnterCriticalSection(GWSockCritSect);
    try
{$ENDIF}
        Dec(WSocketGCount);
        if WSocketGCount <= 0 then begin
            WSocketUnloadWinsock;
{           WSocketGCount := 0;  // it is set to 0 in WSocketUnloadWinsock }
        end;
{$IFDEF COMPILER2_UP}
    finally
        LeaveCriticalSection(GWSockCritSect);
    end;
{$ENDIF}

    if Assigned(FBufHandler) then begin
        FBufHandler.Free;
        FBufHandler := nil;
    end;
    if Assigned(FDnsResultList) then begin
        FDnsResultList.Free;
        FDnsResultList := nil;
    end;

    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetReqVerLow: BYTE;
begin
    Result := GReqVerLow;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetReqVerLow(const Value: BYTE);
begin
    if GReqVerLow <> Value then begin
        if FDllHandle <> 0 then
            SocketError('SetReqVerLow: WinSock version can''t be changed now')
        else
            GReqVerLow := Value;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetReqVerHigh: BYTE;
begin
    Result := GReqVerHigh;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetReqVerHigh(const Value: BYTE);
begin
    if GReqVerHigh <> Value then begin
        if FDllHandle <> 0 then
            SocketError('SetReqVerHigh: WinSock version can''t be changed now')
        else
            GReqVerHigh := Value;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Dup(NewHSocket : TSocket);
var
    iStatus : Integer;
    optlen  : Integer;
begin
{$IFDEF CLR}
    if DesignMode then begin
        FHsocket := NewHSocket;
        Exit;
    end;
{$ENDIF}
    if (NewHSocket = 0) or (NewHSocket = INVALID_SOCKET) then begin
        WSocket_Synchronized_WSASetLastError(WSAEINVAL);
        SocketError('Dup');
        Exit;
    end;

    if FState <> wsClosed then begin
        iStatus := WSocket_Synchronized_closesocket(FHSocket);
        FHSocket := INVALID_SOCKET;
        if iStatus <> 0 then begin
            SocketError('Dup (closesocket)');
            Exit;
        end;

        ChangeState(wsClosed);
    end;
    FHsocket := NewHSocket;

    { Get winsock send buffer size }
    optlen  := SizeOf(FSocketSndBufSize);
{$IFDEF CLR}
    iStatus := WSocket_getsockopt(FHSocket, SOL_SOCKET, SO_SNDBUF,
                                  FSocketSndBufSize, optlen);
{$ELSE}
    iStatus := WSocket_getsockopt(FHSocket, SOL_SOCKET, SO_SNDBUF,
                                  PChar(@FSocketSndBufSize), optlen);
{$ENDIF}

    if iStatus <> 0 then begin
        SocketError('getsockopt(SO_SNDBUF)');
        Exit;
    end;

    { Get winsock receive buffer size }
    optlen  := SizeOf(FSocketRcvBufSize);
{$IFDEF CLR}
    iStatus := WSocket_getsockopt(FHSocket, SOL_SOCKET, SO_RCVBUF,
                                  FSocketRcvBufSize, optlen);
{$ELSE}
    iStatus := WSocket_getsockopt(FHSocket, SOL_SOCKET, SO_RCVBUF,
                                  PChar(@FSocketRcvBufSize), optlen);
{$ENDIF}

    if iStatus <> 0 then begin
        SocketError('getsockopt(SO_RCVBUF)');
        Exit;
    end;

    SetLingerOption;
    SetKeepAliveOption;  // AG { 05/23/07)

    { FD_CONNECT is not needed for dup(): The socket is already connected }
    FSelectEvent := FD_READ or FD_WRITE or FD_CLOSE { or FD_CONNECT };
    iStatus      := WSocket_Synchronized_WSAASyncSelect(FHSocket, Handle,
                                                        FMsg_WM_ASYNCSELECT,
                                                        FSelectEvent);
    if iStatus <> 0 then begin
        SocketError('WSAAsyncSelect');
        Exit;
    end;
    DupConnected;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.DupConnected;
begin
    ChangeState(wsConnected);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetBufSize(Value : Integer);
begin
    FBufHandler.BufSize := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetBufSize: Integer;
begin
    Result := FBufHandler.BufSize;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Get the number of char received and waiting to be read                    }
function TCustomWSocket.GetRcvdCount : LongInt;
var
    Temp : u_long;
begin
{$IFDEF CLR}
    if DesignMode then begin
{$ENDIF}
{$IFDEF WIN32}
    if csDesigning in ComponentState then begin
{$ENDIF}
        Result := -1;
        Exit;
    end;
    if WSocket_Synchronized_ioctlsocket(FHSocket, FIONREAD, Temp) = SOCKET_ERROR then begin
        Result := -1;
        SocketError('ioctlSocket');
        Exit;
    end;
    Result := LongInt(Temp);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.ChangeState(NewState : TSocketState);
var
    OldState : TSocketState;
begin
    OldState := FState;
    FState   := NewState;
    if OldState <> NewState then       { 20030226 }
        TriggerChangeState(OldState, NewState);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ DoRecv is a simple wrapper around winsock recv function to make it        }
{ a virtual function.                                                       }
function TCustomWSocket.DoRecv(
    var Buffer : TWSocketData;
    BufferSize : Integer;
    Flags      : Integer) : Integer;
begin
{ MoulinCnt := (MoulinCnt + 1) and 3; }
{ Write('R', Moulin[MoulinCnt], #13); }
    Result := WSocket_Synchronized_recv(FHSocket, Buffer, BufferSize, Flags);
{   FRcvdFlag := (Result > 0);}
    { If we received the requested size, we may need to receive more }
    FRcvdFlag := (Result >= BufferSize);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ The socket is non-blocking, so this routine will only receive as much     }
{ data as it is available.                                                  }
function TCustomWSocket.Receive(Buffer : TWSocketData; BufferSize: Integer) : Integer;
begin
    Result := DoRecv(Buffer, BufferSize, 0);
    if Result < 0 then
        FLastError := WSocket_Synchronized_WSAGetLastError
    else
        { Here we should check for overflows ! It is well possible to }
        { receive more than 2GB during a single session.              }
        { Or we could use an Int64 variable...                        }
        FReadCount := FReadCount + Result;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Receive as much data as possible into a string                            }
{ You should avoid this function and use Receive. Using string will be      }
{ much slower because data will be copied several times.                    }
{ ReceiveStr will *NOT* wait for a line to be received. It just read        }
{ already received characters and return them as a string.                  }
function TCustomWSocket.ReceiveStr : String;
var
    lCount : LongInt;
{$IFDEF COMPILER12_UP}
    TempS : AnsiString;
{$ENDIF}
begin
    lCount := GetRcvdCount;

    if lCount < 0 then begin  { GetRcvdCount returned an error }
        SetLength(Result, 0);
        Exit;
    end;

    if lCount = 0 then        { GetRcvdCount say nothing, will try anyway }
        LCount := 255;        { some reasonable arbitrary value           }

{$IFDEF DELPHI1}
    { Delphi 1 strings are limited }
    if lCount > High(Result) then
        lCount := High(Result);
{$ENDIF}

{$IFDEF CLR}
    if Length(FRecvStrBuf) < 1460 then
        SetLength(FRecvStrBuf, 1460);
    if Length(FRecvStrBuf) < lCount then
    SetLength(FRecvStrBuf, lCount);
    lCount := Receive(FRecvStrBuf, Length(FRecvStrBuf));
    if lCount <= 0 then
        Result := ''
    else begin
        Result := System.Text.Encoding.Default.GetString(FRecvStrBuf, 0, lCount);
//        SetLength(Result, lCount);
//        while lCount > 0 do begin
//            Dec(lCount);
//            Result[lCount + 1] := Char(FRecvStrBuf[lCount]);
//        end;
    end;
{$ENDIF}
{$IFDEF WIN32}
{$IFDEF COMPILER12_UP}
    SetLength(TempS, lCount);
    lCount := Receive(@TempS[1], lCount);
    if lCount > 0 then begin
        SetLength(TempS, lCount);
        Result := String(TempS);
    end
    else
        SetLength(Result, 0);
{$ELSE}
    SetLength(Result, lCount);
    lCount := Receive(@Result[1], lCount);
    if lCount > 0 then
        SetLength(Result, lCount)
    else
        SetLength(Result, 0);
{$ENDIF}
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.DoRecvFrom(
    FHSocket    : TSocket;
    var Buffer  : TWSocketData;
    BufferSize  : Integer;
    Flags       : Integer;
    var From    : TSockAddr;
    var FromLen : Integer) : Integer;
begin
    Result := WSocket_Synchronized_recvfrom(FHSocket, Buffer, BufferSize,
                                            Flags, From, FromLen);
{   FRcvdFlag := (Result > 0); }
    FRcvdFlag := (Result >= BufferSize);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.ReceiveFrom(
    Buffer      : TWSocketData;
    BufferSize  : Integer;
    var From    : TSockAddr;
    var FromLen : Integer) : Integer;
begin
    Result := DoRecvFrom(FHSocket, Buffer, BufferSize, 0, From, FromLen);
    if Result < 0 then
        FLastError := WSocket_Synchronized_WSAGetLastError
    else
        FReadCount := FReadCount + Result;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.PeekData(Buffer : TWSocketData; BufferSize: Integer) : Integer;
begin
    Result := DoRecv(Buffer, BufferSize, MSG_PEEK);
    if Result < 0 then
        FLastError := WSocket_Synchronized_WSAGetLastError;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{
function SearchChar(Data : PChar; Len : Integer; Ch : Char) : PChar;
begin
    while Len > 0 do begin
        Len := Len - 1;
        if Data^ = Ch then begin
            Result := Data;
            exit;
        end;
        Data := Data + 1;
    end;
    Result := nil;
end;
}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This function should be used with UDP only. Use Send for TCP.             }
function TCustomWSocket.SendTo(
    Dest       : TSockAddr;
    DestLen    : Integer;
    const Data : TWSocketData;
    Len        : Integer) : Integer;
begin
    Result := WSocket_Synchronized_SendTo(FHSocket, Data, Len, FSendFlags,
                                          TSockAddr(Dest), DestLen);
    if Result > 0 then begin
        TriggerSendData(Result);
        { Post FD_WRITE message to have OnDataSent event triggered }
        if bAllSent and (FType = SOCK_DGRAM) then
            PostMessage(Handle,
                        FMsg_WM_ASYNCSELECT,
                        FHSocket,
                        MakeLong(FD_WRITE, 0));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.RealSend(var Data : TWSocketData; Len : Integer) : Integer;
begin
{ MoulinCnt := (MoulinCnt + 1) and 3; }
{ Write('S', Moulin[MoulinCnt], #13); }
    if FType = SOCK_DGRAM then
        Result := WSocket_Synchronized_SendTo(FHSocket, Data, Len, FSendFlags,
                                              TSockAddr(sin), SizeOf(sin))
    else
        Result := WSocket_Synchronized_Send(FHSocket, Data, Len, FSendFlags);
    if Result > 0 then
        TriggerSendData(Result);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.TryToSend;
var
    Len       : Integer;
    Count     : Integer;
    Data      : TWSocketData;
    LastError : Integer;
begin
    FBufHandler.Lock;
    try
        if (FHSocket = INVALID_SOCKET) or (FBufHandler.IsEmpty) then begin
            bAllSent := TRUE;
            Exit;
        end;

        while TRUE do begin
            Len := FBufHandler.Peek(Data);
            if Len <= 0 then begin
                // Buffer is empty, every thing has been sent
                bAllSent := TRUE;
                break;
            end;
            Count := RealSend(Data, Len);
            if Count > 0 then begin
                Dec(FBufferedByteCount, Count);
                if FBufferedByteCount < 0 then
                    FBufferedByteCount := 0;
            end;
            if Count = 0 then
                break;  // Closed by remote
            if Count = SOCKET_ERROR then begin
                LastError := WSocket_Synchronized_WSAGetLastError;
                if (LastError = WSAECONNRESET) or (LastError = WSAENOTSOCK) or
                   (LastError = WSAENOTCONN)   or (LastError = WSAEINVAL)   or
                   (LastError = WSAECONNABORTED)     { 07/05/99 }
                then begin
{$IFNDEF NO_DEBUG_LOG}
                    if CheckLogOptions(loWsockErr) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
                            DebugLog(loWsockErr, Name +
                                     ' Winsock Send failed - ' +
                                     GetWinsockErr(LastError));
{$ENDIF}
                    FCloseInvoked := TRUE;           { 23/07/98 }
                    Close;
                    TriggerSessionClosed(LastError); { 23/07/98 }
                end
                else if LastError <> WSAEWOULDBLOCK then begin
                    SocketError('TryToSend failed');
                    break;
                end;
                break;
            end;
            FBufHandler.Remove(Count);
            if Count < Len then
                break; // Could not write as much as we wanted. Stop sending
        end;
    finally
        FBufHandler.UnLock;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.PutStringInSendBuffer(const Str : AnsiString); 
{$IFDEF CLR}
var
    Data : TBytes;
    I    : Integer;
begin
    SetLength(Data, Length(Str));
    for I := 1 to Length(Str) do
        Data[I - 1] := Ord(Str[I]);
    PutDataInSendBuffer(Data, Length(Str));
{$ENDIF}
{$IFDEF WIN32}
begin
    if Str <> '' then
        PutDataInSendBuffer(@Str[1], Length(Str));
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF COMPILER12_UP}                                              
procedure TCustomWSocket.PutStringInSendBuffer(const Str : UnicodeString);
begin
    PutStringInSendBuffer(AnsiString(Str));
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.PutDataInSendBuffer(
    Data : TWSocketData;
    Len  : Integer);
begin
    if (Len <= 0) or (Data = nil) then
        Exit;

    FBufHandler.Lock;
    try
        FBufHandler.Write(Data, Len);
        Inc(FBufferedByteCount, Len);
        bAllSent := FALSE;
    finally
        FBufHandler.UnLock;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Return -1 if error, else return number of byte written                    }
function TCustomWSocket.Send({$IFDEF CLR} const {$ENDIF} Data : TWSocketData; Len : Integer) : Integer;
begin
    if (FState <> wsConnected) and (FState <> wsSocksConnected) then begin
        WSocket_Synchronized_WSASetLastError(WSAENOTCONN);
        SocketError('Send');
        Result := -1;
        Exit;
    end;

    bAllSent := FALSE;
    if Len <= 0 then
        Result := 0
    else begin
        Result   := Len;
        PutDataInSendBuffer(Data, Len);
    end;

    if bAllSent then
        Exit;

    TryToSend;

    if bAllSent then begin
        { We post a message to fire the FD_WRITE message wich in turn will }
        { fire the OnDataSent event. We cannot fire the event ourself      }
        { because the event handler will eventually call send again.       }
        { Sending the message prevent recursive call and stack overflow.   }
        { The PostMessage function posts (places) a message in a window's  }
        { message queue and then returns without waiting for the           }
        { corresponding window to process the message. The message will be }
        { seen and routed by Delphi a litle later, when we will be out of  }
        { the send function.                                               }
        PostMessage(Handle,
                    FMsg_WM_ASYNCSELECT,
                    FHSocket,
                    MakeLong(FD_WRITE, 0));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.Send(DataByte : Byte) : Integer;
{$IFDEF CLR}
var
    Buf : TBytes;
begin
    SetLength(Buf, 1);
    Buf[1] := DataByte;
    Result := Send(Buf, 1);
    SetLength(Buf, 0);
end;
{$ENDIF}
{$IFDEF WIN32}
begin
    Result := Send(@DataByte, 1);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Return -1 if error, else return number of ansi-converted elements written }
{$IFDEF COMPILER12_UP}
function TCustomWSocket.SendStr(const Str : UnicodeString) : Integer;
begin
    Result := SendStr(AnsiString(Str))
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Return -1 if error, else return number of byte written                    }
function TCustomWSocket.SendStr(const Str : AnsiString) : Integer;
begin
    if Length(Str) > 0 then
        Result := Send({$IFDEF CLR}
                       System.Text.Encoding.Default.GetBytes(Str),
                       {$ENDIF}
                       {$IFDEF WIN32}
                       PAnsiChar(Str),
                       {$ENDIF}
                       Length(Str))
    else
        Result := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SendText(Str : String);
begin
    SendStr(Str);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function HasOption(
    OptSet : TWSocketOptions;
    Opt    : TWSocketOption): Boolean;
begin
{$IFDEF CLR}
    Result := (Ord(OptSet) and Ord(Opt)) <> 0;
{$ENDIF}
{$IFDEF WIN32}
    Result := Opt in OptSet;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function AddOptions(Opts: array of TWSocketOption): TWSocketOptions;
var
    I : Integer;
begin
{$IFDEF CLR}
    Result := wsoNone;
    for I := Low(Opts) to High(Opts) do
        Result := TWSocketOptions(Integer(Result) + Integer(Opts[I]));
{$ENDIF}
{$IFDEF WIN32}
    Result := [];
    for I := Low(Opts) to High(Opts) do
        Result := Result + [Opts[I]];
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  RemoveOption(
    OptSet : TWSocketOptions;
    Opt    : TWSocketOption) : TWSocketOptions;
begin
{$IFDEF CLR}
    Result := TWSocketOptions(Ord(OptSet) and (not Ord(Opt)));
{$ENDIF}
{$IFDEF WIN32}
    Result := OptSet - [Opt];
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.ASyncReceive(
    Error           : Word;
    MySocketOptions : TWSocketOptions);
var
    bMore        : Boolean;
    lCount       : {$IFDEF FPC} LongWord; {$ELSE} u_long; {$ENDIF}
{$IFDEF WIN32}
    TrashCanBuf  : array [0..1023] of char;
{$ENDIF}
    TrashCan     : TWSocketData;
    TrashCanSize : Integer;
begin
    bMore := TRUE;
    while bMore do begin
        FLastError := 0;

        try
            if not TriggerDataAvailable(Error) then begin
                { Nothing wants to receive, we will receive and throw away  23/07/98 }
                {$IFDEF CLR}
                TrashCanSize := 1024;
                SetLength(TrashCan, TrashCanSize);
                {$ENDIF}
                {$IFDEF WIN32}
                TrashCanSize := SizeOf(TrashCan);
                TrashCan     := @TrashCanBuf;
                {$ENDIF}
                if DoRecv(TrashCan, TrashCanSize, 0) = SOCKET_ERROR then begin
                    FLastError := WSocket_Synchronized_WSAGetLastError;
                    if FLastError = WSAEWOULDBLOCK then begin
                        FLastError := 0;
                        break;
                    end;
                end;
            end;

            { DLR Honor the socket options being passed as parameters }
            if HasOption({FComponentOptions}MySocketOptions, wsoNoReceiveLoop) then  { V6.03 }
                break;

            if FLastError <> 0 then begin
                bMore := FALSE;
                { -1 value is not a true error but is used to break the loop }
                if FLastError = -1 then
                    FLastError := 0;
            end
            { Check if we have something new arrived, if yes, process it }
            else if WSocket_Synchronized_ioctlsocket(FHSocket, FIONREAD,
                                                     lCount) = SOCKET_ERROR then begin
                FLastError := WSocket_Synchronized_WSAGetLastError;
                bMore      := FALSE;
            end
            else if lCount = 0 then
                bMore := FALSE;
        except
            bMore := FALSE;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Do_FD_CONNECT(var msg: TMessage);
begin
    if FState <> wsConnected then begin
        ChangeState(wsConnected);
        TriggerSessionConnectedSpecial(HiWord(msg.LParam));
        if (HiWord(msg.LParam) <> 0) and (FState <> wsClosed) then
            Close;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Do_FD_READ(var msg: TMessage);
begin
    if FState <> wsConnected then begin
      ChangeState(wsConnected);
      TriggerSessionConnectedSpecial(HiWord(msg.LParam));
    end;
    ASyncReceive(HiWord(msg.LParam), FComponentOptions);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Do_FD_WRITE(var msg: TMessage);
begin
    TryToSend;
{ If you wants to test background exception, uncomment the next 2 lines. }
{   if bAllSent then                                                }
{       raise Exception.Create('Test TWSocket exception');          }
    if bAllSent then
        TriggerDataSent(HiWord(msg.LParam));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Do_FD_CLOSE(var msg: TMessage);
begin
    { In some strange situations I found that we receive a FD_CLOSE  }
    { during the connection phase, breaking the connection early !   }
    { This occurs for example after a failed FTP transfert Probably  }
    { something related to bugged winsock. Doesn't hurt with good    }
    { winsock. So let the code there !                               }
    if (FState <> wsConnecting) and (FHSocket <> INVALID_SOCKET) then begin
        { Check if we have something arrived, if yes, process it     }
        { DLR, since we are closing MAKE SURE WE LOOP in the receive }
        { function to get ALL remaining data                         }
        ASyncReceive(0, RemoveOption(FComponentOptions, wsoNoReceiveLoop));

        if not FCloseInvoked then begin
            FCloseInvoked := TRUE;
            TriggerSessionClosed(HiWord(msg.LParam));
        end;

        if FState <> wsClosed then
            Close;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Do_FD_ACCEPT(var msg: TMessage);
begin
    TriggerSessionAvailable(HiWord(msg.LParam));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF NO_DEBUG_LOG}
function WinsockMsgToString(var msg: TMessage) : String;
begin
    Result := '';
    if (msg.lParam and FD_CONNECT) <> 0 then
        Result := Result + ' FD_CONNECT';
    if (msg.lParam and FD_READ) <> 0 then
        Result := Result + ' FD_READ';
    if (msg.lParam and FD_WRITE) <> 0 then
        Result := Result + ' FD_WRITE';
    if (msg.lParam and FD_CLOSE) <> 0 then
        Result := Result + ' FD_CLOSE';
    if (msg.lParam and FD_ACCEPT) <> 0 then
        Result := Result + ' FD_ACCEPT';
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.WMASyncSelect(var msg: TMessage);
var
    Check   : Word;
    ParamLo : Word;
const
    TTTCount : Integer = 0;
begin
{TriggerDisplay('AsyncSelect ' + IntToStr(msg.wParam) + ', ' + IntToStr(msg.LParamLo));}
    { Verify that the socket handle is ours handle }
    if msg.wParam <> FHSocket then
        Exit;

    if FPaused then
        exit;

    ParamLo := LoWord(msg.lParam);

    Check := ParamLo and FD_CONNECT;
    if Check <> 0 then begin
        FSelectMessage := FD_CONNECT;
        Do_FD_CONNECT(msg);
    end;

    Check := ParamLo and FD_READ;
    if Check <> 0 then begin
        FSelectMessage := FD_READ;
        Do_FD_READ(msg);
    end;

    Check := ParamLo and FD_WRITE;
    if Check <> 0 then begin
        FSelectMessage := FD_WRITE;
        Do_FD_WRITE(msg);
    end;

    Check := ParamLo and FD_ACCEPT;
    if Check <> 0 then begin
        FSelectMessage := FD_ACCEPT;
        Do_FD_ACCEPT(msg);
    end;

    Check := ParamLo and FD_CLOSE;
    if Check <> 0 then begin
        FSelectMessage := FD_CLOSE;
        {WriteLn('FD_CLOSE ', FHSocket);}
        Do_FD_CLOSE(msg);
    end;
    FSelectMessage := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF CLR}
procedure GetIPList(HostEntry : THostEnt; ToList : TStrings);
var
    AddrList   : IntPtr;
    AddrItem   : IntPtr;
    Addr       : Integer;
    I          : Integer;
begin
    ToList.Clear;
    I := 0;
    AddrList := Marshal.ReadIntPtr(HostEntry.h_addr_list);
    while TRUE do begin
        AddrItem  := Marshal.ReadIntPtr(HostEntry.h_addr_list, I);
        if AddrItem = IntPtr.Zero then
            break;
        Addr := Marshal.ReadInt32(AddrItem);
        ToList.Add(IntToStr((Addr and $FF)) + '.' +
                   IntToStr((Addr shr 8) and $FF) + '.' +
                   IntToStr((Addr shr 16) and $FF) + '.' +
                   IntToStr((Addr shr 24) and $FF));
        Inc(I, 4);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure GetAliasList(HostEntry : THostEnt; ToList : TStrings);
var
    AddrItem : IntPtr;
    I        : Integer;
begin
    I := 0;
    while TRUE do begin
        AddrItem  := Marshal.ReadIntPtr(HostEntry.h_aliases, I);
        if AddrItem = IntPtr.Zero then
            break;
        ToList.Add(Marshal.PtrToStringAnsi(AddrItem));
        Inc(I);
    end;
end;
{$ENDIF}
{$IFDEF WIN32}
procedure GetIPList(phe  : PHostEnt; ToList : TStrings);
type
    TaPInAddr = array [0..255] of PInAddr;
    PaPInAddr = ^TaPInAddr;
var
    pptr : PaPInAddr;
    I    : Integer;
begin
    pptr := PaPInAddr(Phe^.h_addr_list);

    I := 0;
    while pptr^[I] <> nil do begin
        ToList.Add(WSocket_inet_ntoa(pptr^[I]^));
        Inc(I);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure GetAliasList(phe : PHostEnt; ToList : TStrings);
type
    TaPAnsiChar = array [0..255] of PAnsiChar;
    PaPAnsiChar = ^TaPAnsiChar;
var
    pptr : PaPAnsiChar;
    I    : Integer;
begin
    pptr := PaPAnsiChar(Phe^.h_aliases);
    I    := 0;
    while pptr^[I] <> nil do begin
        ToList.Add(pptr^[I]);
        Inc(I);
    end;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.WMAsyncGetHostByName(var msg: TMessage);
var
    ErrCode : Word;
{$IFDEF CLR}
    HostEntry : THostEnt;
{$ENDIF}
{$IFDEF WIN32}
    Phe     : Phostent;
{$ENDIF}
begin
    if FDnsLookupHandle = 0 then begin
        { We are still executing WSAAsyncGetHostByName and FDnsLookupHandle }
        { has not been assigned yet ! We should proceed later.              }
        FDnsLookupTempMsg  := msg;
        FDnsLookupCheckMsg := TRUE;
        Exit;
    end
    else if msg.wParam <> LongInt(FDnsLookupHandle) then
        Exit;

    FDnsLookupHandle := 0;
    ErrCode := HiWord(Msg.LParam);
{$IFDEF CLR}
    if ErrCode = 0 then begin
        HostEntry := THostEnt(Marshal.PtrToStructure(FDnsLookupIntPtr, TypeOf(THostEnt)));
        GetIpList(HostEntry, FDnsResultList);
        if FDnsResultList.Count > 0 then
            FDnsResult := FDnsResultList.Strings[0];
    end;
    FDnsLookupGCH.Free;
{$ENDIF}
{$IFDEF WIN32}
    if ErrCode = 0 then begin
        Phe := PHostent(@FDnsLookupBuffer);
        if phe <> nil then begin
            GetIpList(Phe, FDnsResultList);
            FDnsResult := FDnsResultList.Strings[0];
        end;
    end;
{$ENDIF}
    TriggerDnsLookupDone(ErrCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.WMAsyncGetHostByAddr(var msg: TMessage);
var
{$IFDEF CLR}
    HostEntry : THostEnt;
{$ENDIF}
{$IFDEF WIN32}
    Phe     : Phostent;
{$ENDIF}
    ErrCode : Word;
begin
    if msg.wParam <> LongInt(FDnsLookupHandle) then
        Exit;
    FDnsLookupHandle := 0;
    ErrCode          := HiWord(Msg.LParam);
    if ErrCode = 0 then begin
{$IFDEF CLR}
        if FDnsLookupIntPtr <> IntPtr.Zero then begin
            HostEntry  := THostEnt(Marshal.PtrToStructure(FDnsLookupIntPtr, TypeOf(THostEnt)));
            FDnsResult := Marshal.PtrToStringAnsi(HostEntry.h_name);
{$ENDIF}
{$IFDEF WIN32}
        Phe := PHostent(@FDnsLookupBuffer);
        if phe <> nil then begin
            SetLength(FDnsResult, StrLen(Phe^.h_name));
            StrCopy(@FDnsResult[1], Phe^.h_name);
{$ENDIF}
            FDnsResultList.Clear;
            FDnsResultList.Add(FDnsResult);
{$IFDEF CLR}
            GetAliasList(HostEntry, FDnsResultList);
{$ENDIF}
{$IFDEF WIN32}
            GetAliasList(Phe, FDnsResultList);  {AG 03/03/06}
{$ENDIF}
        end;
    end;
    TriggerDnsLookupDone(ErrCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetProto(sProto : String);
begin
    if FProtoAssigned and (sProto = FProtoStr) then
        Exit;

    if FState <> wsClosed then begin
        RaiseException('Cannot change Proto if not closed');
        Exit;
    end;

    FProtoStr := Trim(sProto);
    if Length(FProtoStr) = 0 then begin
        FProtoAssigned := FALSE;
        Exit;
    end;

    FProtoResolved := FALSE;
    FProtoAssigned := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetProto : String;
begin
    Result := FProtoStr;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetRemotePort(sPort : String);
begin
    if FPortAssigned and (FPortStr = sPort) then
        Exit;

    if FState <> wsClosed then begin
        RaiseException('Cannot change Port if not closed');
        Exit;
    end;

    FPortStr := Trim(sPort);
    if Length(FPortStr) = 0 then begin
        FPortAssigned := FALSE;
        Exit;
    end;

    FPortResolved := FALSE;
    FPortAssigned := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetRemotePort : String;
begin
    Result := FPortStr;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetLocalPort(sLocalPort : String);
begin
    if FState <> wsClosed then begin
        RaiseException('Cannot change LocalPort if not closed');
        Exit;
    end;

    FLocalPortStr      := sLocalPort;
    FLocalPortResolved := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetLocalAddr(sLocalAddr : String);
{var
    IPAddr  : TInAddr;}
begin
    if FState <> wsClosed then begin
        RaiseException('Cannot change LocalAddr if not closed');
        Exit;
    end;

    if Length(sLocalAddr) = 0 then
        sLocalAddr := '0.0.0.0';
{$IFDEF NEVER}
{$IFDEF DELPHI1}
    sLocalAddr := sLocalAddr + #0;
{$ENDIF}
    IPAddr.S_addr := WSocket_Synchronized_inet_addr(sLocalAddr);
    if IPAddr.S_addr = u_long(INADDR_NONE) then
        RaiseException('SetLocalAddr(): Invalid IP address');
    FLocalAddr := StrPas(WSocket_Synchronized_inet_ntoa(IPAddr));
{$ELSE}
    FLocalAddr := sLocalAddr;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetXPort: String;
var
    saddr    : TSockAddrIn;
    saddrlen : Integer;
    port     : Integer;
begin
    Result := 'error';
    if FState in [wsConnected, wsBound, wsListening] then begin
        saddrlen := sizeof(saddr);
        if WSocket_Synchronized_GetSockName(FHSocket, TSockAddr(saddr), saddrlen) = 0 then begin
            port     := WSocket_Synchronized_ntohs(saddr.sin_port);
            Result   := IntToStr(port);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetXAddr: String;
var
    saddr    : TSockAddrIn;
    saddrlen : Integer;
begin
    Result := 'error';
    if FState in [wsConnected, wsBound, wsListening] then begin
        saddrlen := sizeof(saddr);
        if WSocket_Synchronized_GetSockName(FHSocket, TSockAddr(saddr), saddrlen) = 0 then
            Result := {StrPas}(WSocket_Synchronized_inet_ntoa(saddr.sin_addr));
     end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetAddr(InAddr : String);
begin
    if FAddrAssigned and (FAddrStr = InAddr) then
        Exit;

    if FState <> wsClosed then begin
        RaiseException('Cannot change Addr if not closed');
        Exit;
    end;

    FAddrStr := Trim(InAddr);
    if Length(FAddrStr) = 0 then begin
        FAddrAssigned := FALSE;
        Exit;
    end;

    FAddrResolved       := FALSE;
    FAddrAssigned       := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_ResolveHost(InAddr : String) : TInAddr;
var
{$IFDEF CLR}
    Phe       : IntPtr;
    HostEntry : THostEnt;
    AddrList  : IntPtr;
    AddrItem  : IntPtr;
{$ENDIF}
{$IFDEF WIN32}
    Phe     : Phostent;
{$ENDIF}
    IPAddr  : u_long;
begin
    if InAddr = '' then
      {  raise ESocketException.Create('WSocketResolveHost: ''' + InAddr + ''' Invalid Hostname.'); }
        raise ESocketException.Create('Winsock Resolve Host: ''' + InAddr + ''' Invalid Hostname.');   { V5.26 }


    if WSocketIsDottedIP(InAddr) then begin
        { Address is a dotted numeric address like 192.161.124.32 }
        IPAddr := WSocket_Synchronized_inet_addr(InAddr);
{$IFDEF DELPHI1}
        { With Trumpet Winsock 2B and 30D (win 3.11), inet_addr returns faulty }
        { results for 0.0.0.0                                                  }
        if (IPAddr = INADDR_NONE) and (InAddr = '0.0.0.0') then begin
            Result.s_addr := 0;
            Exit;
        end;
{$ENDIF}
        if IPAddr = u_long(INADDR_NONE) then begin
            if InAddr = '255.255.255.255' then begin
                Result.s_addr := u_long(INADDR_BROADCAST);
                Exit;
            end;
       {     raise ESocketException.Create('WSocketResolveHost: ''' + InAddr + ''' Invalid IP address.');  }
            raise ESocketException.Create('Winsock Resolve Host: ''' + InAddr +
                                         ''' Invalid IP address.');   { V5.26 }
        end;
        Result.s_addr := IPAddr;
        Exit;
    end;

{$IFDEF CLR}
    Phe := OverByteIcsWinsock.GetHostByName(InAddr);
    if Phe = IntPtr.Zero then
        raise ESocketException.Create(
         { 'WSocketResolveHost: Cannot convert host address ''' + InAddr +
           ''', Error #' + IntToStr(WSAGetLastError)); }
           'Winsocket Resolve Host: Cannot convert host address ''' + InAddr +
           ''', Error #' + IntToStr(WSAGetLastError));
    HostEntry     := THostEnt(Marshal.PtrToStructure(Phe, TypeOf(THostEnt)));
    AddrList      := Marshal.ReadIntPtr(HostEntry.h_addr_list);
    AddrItem      := Marshal.ReadIntPtr(HostEntry.h_addr_list);
    Result.s_addr := Marshal.ReadInt32(AddrItem);
{$ENDIF}
{$IFDEF WIN32}
    { Address is a hostname }
    Phe := WSocket_Synchronized_GetHostByName(PChar(InAddr));
    if Phe = nil then
        raise ESocketException.Create(
                 'Winsock Resolve Host: Cannot convert host address ''' +
                 InAddr + ''' - ' +
                 GetWinsockErr(WSocket_Synchronized_WSAGetLastError));
    Result.s_addr := PInAddr(Phe^.h_addr_list^)^.s_addr;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocketResolveHost(InAddr : String) : TInAddr;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_ResolveHost(InAddr);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Convert port name or number to number in host order (ftp -> 21)           }
function WSocket_Synchronized_ResolvePort(Port : String; Proto : String) : WORD;
{$IFDEF CLR}
var
    Pse       : IntPtr;
    ServEntry : TServEnt;
begin
    if Port = '' then
      {  raise ESocketException.Create('WSocketResolvePort: Invalid Port.'); }
        raise ESocketException.Create('Winsock Resolve Port: Invalid Port.');    { V5.26 }


    if IsDigit(Port[1]) then
        Result := atoi(Port)
    else begin
        if not GWSAStartupCalled then
            WSocket_Synchronized_WSAStartup(MAKEWORD(GReqVerLow, GReqVerHigh), GInitData);
        if Proto = '' then
            Pse := OverByteIcsWinsock.GetServByName(Port, '')
        else
            Pse := OverByteIcsWinsock.GetServByName(Port, Proto);
        if Pse = IntPtr.Zero then
            raise ESocketException.Create(
                     'Winsock Resolve Port: Cannot convert port ''' +
                     Port + ''' - ' +
                     GetWinsockErr(OverByteIcsWinsock.WSAGetLastError)); { V5.26 }
        ServEntry := TServEnt(Marshal.PtrToStructure(Pse, TypeOf(TServEnt)));
        Result    := OverByteIcsWinsock.ntohs(ServEntry.s_port);
    end;
end;
{$ENDIF}
{$IFDEF WIN32}
var
    Pse      : Pservent;
begin
    if Port = '' then
      { raise ESocketException.Create('WSocketResolvePort: Invalid Port.'); }
        raise ESocketException.Create('Winsock Resolve Port: Invalid Port.');

    if Proto = '' then
        raise ESocketException.Create('Winsock Resolve Port: Invalid Proto.');

    if IsDigit(Port[1]) then
        Result := atoi(Port)
    else begin
        Pse := WSocket_Synchronized_GetServByName(PChar(Port), PChar(Proto));
        if Pse = nil then
            raise ESocketException.Create(
                      'Winsock Resolve Port: Cannot convert port ''' +
                      Port + ''' - ' +
                      GetWinsockErr(WSocket_Synchronized_WSAGetLastError)); { V5.26 }
        Result := WSocket_Synchronized_ntohs(Pse^.s_port);
    end;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Convert port name or number to number in host order (ftp -> 21)           }
function WSocketResolvePort(Port : String; Proto : String) : Word;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_ResolvePort(Port, Proto);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_ResolveProto(sProto : String) : Integer;
var
{$IFDEF CLR}
    Ppe        : IntPtr;
    ProtoEntry : TProtoEnt;
{$ENDIF}
{$IFDEF WIN32}
    Ppe     : Pprotoent;
{$ENDIF}
begin
    if sProto = '' then
      {  raise ESocketException.Create('WSocketResolveProto: Invalid Protocol.');  }
        raise ESocketException.Create('Winsock Resolve Proto: Invalid Protocol.');  { V5.26 }

    if IsDigit(sProto[1]) then
        Result := atoi(sProto)
    else begin
        sProto := LowerCase(Trim(sProto));
        if sProto = 'tcp' then
            Result := IPPROTO_TCP
        else if sProto = 'udp' then
            Result := IPPROTO_UDP
        else if sProto = 'raw' then
            Result := IPPROTO_RAW
        else begin
{$IFDEF CLR}
        ppe := OverByteIcsWinsock.getprotobyname(sProto);
        if Ppe = IntPtr.Zero then
            raise ESocketException.Create(
                      'Winsock Resolve Proto: Cannot convert protocol ''' +
                      sProto + '''- ' +
                      GetWinsockErr(OverByteIcsWinsock.WSAGetLastError)); { V5.26 }
        ProtoEntry := TProtoEnt(Marshal.PtrToStructure(Ppe, TypeOf(TProtoEnt)));
        Result     := ProtoEntry.p_proto;
{$ENDIF}
{$IFDEF WIN32}
            ppe := WSocket_Synchronized_getprotobyname(sProto);
            if Ppe = nil then
                raise ESocketException.Create(
                          'Winsock Resolve Proto: Cannot convert protocol ''' +
                          sProto + ''' - ' +
                          GetWinsockErr(WSocket_Synchronized_WSAGetLastError));    { V5.26 }
            Result := ppe^.p_proto;
{$ENDIF}
        end;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocketResolveProto(sProto : String) : Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_ResolveProto(sProto);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetAddr : String;
begin
    Result := FAddrStr;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetSockName(var saddr : TSockAddrIn; var saddrlen : Integer) : Integer;
begin
    Result := WSocket_Synchronized_GetSockName(FHSocket, TSockAddr(saddr), saddrlen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetPeerAddr: String;
var
    saddr    : TSockAddrIn;
    saddrlen : Integer;
begin
{$IFDEF CLR}
    if DesignMode then begin
        Result := '';
        Exit;
    end;
{$ENDIF}
    Result := 'error';
    if FState = wsConnected then begin
        saddrlen := sizeof(saddr);
        if WSocket_Synchronized_GetPeerName(FHSocket, TSockAddr(saddr), saddrlen) = 0 then
            Result := WSocket_Synchronized_inet_ntoa(saddr.sin_addr)
        else begin
            SocketError('GetPeerName');
            Exit;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetPeerPort: String;
var
    saddr    : TSockAddrIn;
    saddrlen : Integer;
begin
{$IFDEF CLR}
    if DesignMode then begin
        Result := '';
        Exit;
    end;
{$ENDIF}
    Result := 'error';
    if FState = wsConnected then begin
        saddrlen := sizeof(saddr);
        if WSocket_Synchronized_GetPeerName(FHSocket, TSockAddr(saddr), saddrlen) = 0 then
            Result := IntToStr(WSocket_Synchronized_ntohs(saddr.sin_port))
        else begin
            SocketError('GetPeerPort');
            Exit;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetPeerName(var Name : TSockAddrIn; NameLen : Integer) : Integer;
begin
{$IFDEF CLR}
    if DesignMode then begin
        Result := SOCKET_ERROR;
        Exit;
    end;
{$ENDIF}
    if FState = wsConnected then
        Result := WSocket_Synchronized_GetPeerName(FHSocket, TSockAddr(Name), NameLen)
    else
        Result := SOCKET_ERROR;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.CancelDnsLookup;
begin
    if FDnsLookupHandle = 0 then
        Exit;
    if WSocket_Synchronized_WSACancelAsyncRequest(FDnsLookupHandle) <> 0 then begin
        FDnsLookupHandle := 0;
        SocketError('WSACancelAsyncRequest');
        Exit;
    end;
    FDnsLookupHandle := 0;

{$IFDEF WIN32}
    if not (csDestroying in ComponentState) then
{$ENDIF}
        TriggerDnsLookupDone(WSAEINTR);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.DnsLookup(const AHostName : AnsiString);
var
    IPAddr   : TInAddr;
    HostName : AnsiString;
begin
    if AHostName = '' then begin
        RaiseException('DNS lookup: invalid host name.');
        TriggerDnsLookupDone(WSAEINVAL);
        Exit;
    end;

    { Cancel any pending lookup }
    if FDnsLookupHandle <> 0 then begin
        WSocket_Synchronized_WSACancelAsyncRequest(FDnsLookupHandle);
        FDnsLookupHandle := 0;
    end;

    FDnsResult := '';
    FDnsResultList.Clear;

{$IFDEF DELPHI1}
    { Delphi 1 do not automatically add a terminating nul char }
    HostName := AHostName + #0;
{$ELSE}
    HostName := AHostName;
{$ENDIF}
    if WSocketIsDottedIP(Hostname) then begin   { 28/09/2002 }
        IPAddr.S_addr := WSocket_Synchronized_inet_addr(HostName);
        if IPAddr.S_addr <> u_long(INADDR_NONE) then begin
            FDnsResult := WSocket_Synchronized_inet_ntoa(IPAddr);
            FDnsResultList.Add(FDnsResult);     { 28/09/2002 }{ 12/02/2003 }
            TriggerDnsLookupDone(0);
            Exit;
        end;
    end;

    if FWindowHandle = 0 then
        RaiseException('DnsLookup: Window not assigned');

    { John Goodwin found a case where winsock dispatch WM_ASYNCGETHOSTBYNAME }
    { message before returning from WSAAsyncGetHostByName call. Because of   }
    { that, FDnsLookupHandle is not yet assigned when execution comes in     }
    { WMAsyncGetHostByName. John use a flag to check this situation.         }
    FDnsLookupCheckMsg := FALSE;
{$IFDEF CLR}
    SetLength(FDnsLookupBuffer, MAXGETHOSTSTRUCT);
    FDnsLookupGCH    := GCHandle.Alloc(FDnsLookupBuffer, GCHandleType.Pinned);
    FDnsLookupIntPtr := FDnsLookupGCH.AddrOfPinnedObject;
    FDnsLookupHandle := WSocket_WSAAsyncGetHostByName(
                              FWindowHandle,
                              FMsg_WM_ASYNCGETHOSTBYNAME,
                              HostName,
                              FDnsLookupIntPtr,
                              MAXGETHOSTSTRUCT);
    if FDnsLookupHandle = 0 then begin
        FDnsLookupGCH.Free;
        RaiseException(HostName +
                       ': can''t start DNS lookup - ' +
                       GetWinsockErr(WSocket_WSAGetLastError));   { V5.26 }
        Exit;
    end;
{$ENDIF}
{$IFDEF WIN32}
    FDnsLookupHandle   := WSocket_Synchronized_WSAAsyncGetHostByName(
                              FWindowHandle,
                              FMsg_WM_ASYNCGETHOSTBYNAME,
                              @HostName[1],
                              @FDnsLookupBuffer,
                              SizeOf(FDnsLookupBuffer));
    if FDnsLookupHandle = 0 then begin
        RaiseException(HostName + ': can''t start DNS lookup - ' +
                       GetWinsockErr(WSocket_Synchronized_WSAGetLastError));  { V5.26 }
        Exit;
    end;
{$ENDIF}
    if FDnsLookupCheckMsg then begin
        FDnsLookupCheckMsg := FALSE;
        WMAsyncGetHostByName(FDnsLookupTempMsg);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.ReverseDnsLookup(const HostAddr: String);
var
    lAddr  : u_long;
begin
    if HostAddr = '' then begin
        RaiseException('Reverse DNS Lookup: Invalid host name.'); { V5.26 }
        TriggerDnsLookupDone(WSAEINVAL);
        Exit;
    end;
    { Cancel any pending lookup }
    if FDnsLookupHandle <> 0 then
        WSocket_Synchronized_WSACancelAsyncRequest(FDnsLookupHandle);

    FDnsResult := '';
    FDnsResultList.Clear;

    lAddr := WSocket_Synchronized_inet_addr(HostAddr);

    if FWindowHandle = 0 then
        RaiseException('Reverse DNS Lookup: Window not assigned');  { V5.26 }

{$IFDEF CLR}
    SetLength(FDnsLookupBuffer, MAXGETHOSTSTRUCT);
    FDnsLookupGCH    := GCHandle.Alloc(FDnsLookupBuffer, GCHandleType.Pinned);
    FDnsLookupIntPtr := FDnsLookupGCH.AddrOfPinnedObject;
    FDnsLookupHandle := WSocket_WSAAsyncGetHostByAddr(
                            FWindowHandle,
                            FMsg_WM_ASYNCGETHOSTBYADDR,
                            lAddr, 4, PF_INET,
                            FDnsLookupIntPtr,
                            MAXGETHOSTSTRUCT);
    if FDnsLookupHandle = 0 then begin
        FDnsLookupGCH.Free;
        RaiseException(HostAddr + ': can''t start DNS lookup - ' +
                       GetWinsockErr(WSocket_WSAGetLastError));   { V5.26 }
    end;
{$ENDIF}
{$IFDEF WIN32}
    FDnsLookupHandle := WSocket_Synchronized_WSAAsyncGetHostByAddr(
                            FWindowHandle,
                            FMsg_WM_ASYNCGETHOSTBYADDR,
                            PChar(@lAddr), 4, PF_INET,
                            @FDnsLookupBuffer,
                            SizeOf(FDnsLookupBuffer));
    if FDnsLookupHandle = 0 then
        RaiseException(HostAddr + ': can''t start reverse DNS lookup - ' +
                       GetWinsockErr(WSocket_Synchronized_WSAGetLastError));  { V5.26 }
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF CLR}
procedure TCustomWSocket.ReverseDnsLookupSync(const HostAddr: String);
var
    lAddr  : u_long;
    Phe    : IntPtr;
    HostEntry    : THostEnt;
begin
    if Length(HostAddr) = 0 then begin
        RaiseException('ReverseDnsLookup: Invalid host name.');
        TriggerDnsLookupDone(WSAEINVAL);
        Exit;
    end;
    // Cancel any pending lookup
    if FDnsLookupHandle <> 0 then
        WSocket_WSACancelAsyncRequest(FDnsLookupHandle);

    FDnsResult := '';
    FDnsResultList.Clear;

    lAddr := WSocket_Synchronized_inet_addr(HostAddr);
    Phe   := WSocket_Synchronized_gethostbyaddr(lAddr, 4, AF_INET);
    if Phe = IntPtr.Zero then
        TriggerDnsLookupDone(WSocket_Synchronized_WSAGetLastError)
    else begin
        HostEntry  := THostEnt(Marshal.PtrToStructure(Phe,
                                                        TypeOf(THostEnt)));
        FDnsResult := Marshal.PtrToStringAnsi(HostEntry.h_name);
        FDnsResultList.Add(FDnsResult);
        GetAliasList(HostEntry, FDnsResultList);
        TriggerDnsLookupDone(0);
    end;
end;
{$ENDIF}
{$IFDEF WIN32}
procedure TCustomWSocket.ReverseDnsLookupSync(const HostAddr: String); {AG 03/03/06}
var
    szAddr : array [0..256] of Char;
    lAddr  : u_long;
    Phe    : Phostent;
begin
    if (Length(HostAddr) = 0) or (Length(HostAddr) >= SizeOf(szAddr)) then begin
        RaiseException('Reverse DNS Lookup: Invalid host name.');   { V5.26 }
        TriggerDnsLookupDone(WSAEINVAL);
        Exit;
    end;
    { Cancel any pending lookup }
    if FDnsLookupHandle <> 0 then
        WSocket_Synchronized_WSACancelAsyncRequest(FDnsLookupHandle);

    FDnsResult := '';
    FDnsResultList.Clear;

    StrPCopy(szAddr, HostAddr); { Length already checked above }
    lAddr := WSocket_Synchronized_inet_addr(szAddr);

    Phe := WSocket_Synchronized_gethostbyaddr(PChar(@lAddr), 4, AF_INET);
    if Phe = nil then
        TriggerDnsLookupDone(WSocket_Synchronized_WSAGetLastError)
    else begin
        SetLength(FDnsResult, StrLen(Phe^.h_name));
        StrCopy(@FDnsResult[1], Phe^.h_name);
        FDnsResultList.Add(FDnsResult);
        GetAliasList(Phe, FDnsResultList);
        TriggerDnsLookupDone(0);
    end;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.BindSocket;
var
    SockName      : TSockAddr;
    SockNamelen   : Integer;
    LocalSockName : TSockAddrIn;
{$IFDEF CLR}
    I             : Integer;
begin
    for I := Low(LocalSockName.sin_zero) to High(LocalSockName.sin_zero) do
        LocalSockName.sin_zero[0] := #0;
{$ENDIF}
{$IFDEF WIN32}
begin
    FillChar(LocalSockName, Sizeof(LocalSockName), 0);
{$ENDIF}
    SockNamelen                   := SizeOf(LocalSockName);
    LocalSockName.sin_family      := AF_INET;
    LocalSockName.sin_port        := WSocket_Synchronized_htons(FLocalPortNum);
    LocalSockName.sin_addr.s_addr := WSocket_Synchronized_ResolveHost(FLocalAddr).s_addr;

    if WSocket_Synchronized_bind(HSocket, LocalSockName, SockNamelen) <> 0 then begin
        RaiseException('Bind socket failed - ' +
                       GetWinsockErr(WSocket_Synchronized_WSAGetLastError));  { V5.26 }
        Exit;
    end;
    SockNamelen := sizeof(SockName);
    if WSocket_Synchronized_getsockname(FHSocket, SockName, SockNamelen) <> 0 then begin
        RaiseException('Winsock get socket name failed - ' +
                       GetWinsockErr(WSocket_Synchronized_WSAGetLastError));  { V5.26 }
        Exit;
    end;
    FLocalPortNum := WSocket_Synchronized_ntohs(SockName.sin_port);
    FLocalPortStr := IntToStr(FLocalPortNum);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF CLR}
procedure TCustomWSocket.SetKeepAliveOption;
begin
    // Not implemented !
end;
{$ENDIF}
{$IFDEF WIN32}
procedure TCustomWSocket.SetKeepAliveOption;
var
    OptVal        : Integer;
    Status        : Integer;
    KeepAliveIn   : TTcpKeepAlive;
    KeepAliveOut  : TTcpKeepAlive;
{$IFDEF DELPHI3}
    BytesReturned : DWORD;
{$ELSE}
    BytesReturned : Cardinal;
{$ENDIF}
begin
    if FKeepAliveOnOff = wsKeepAliveOff then
        Exit;
    if FKeepAliveOnOff = wsKeepAliveOnSystem then begin
        OptVal := 1;
        Status := WSocket_Synchronized_setsockopt(FHSocket, SOL_SOCKET,
                                                  SO_KEEPALIVE, @OptVal,
                                                  SizeOf(OptVal));

        if Status <> 0 then
            SocketError('setsockopt(SO_KEEPALIVE)');
        Exit;
    end;
{$IFNDEF DELPHI1}
    FillChar(KeepAliveIn, SizeOf(KeepAliveIn), 0);
    FillChar(KeepAliveOut, SizeOf(KeepAliveOut), 0);
    BytesReturned := 0;

    KeepAliveIn.OnOff             := 1;
    KeepAliveIn.KeepAliveInterval := FKeepAliveInterval;
    KeepAliveIn.KeepAliveTime     := FKeepAliveTime;
    Status := WSocket_WSAIoctl(FHSocket,      SIO_KEEPALIVE_VALS,
                               @KeepAliveIn,  SizeOf(KeepAliveIn),
                               @KeepAliveOut, SizeOf(KeepAliveOut),
                               BytesReturned, nil, nil);
    if Status <> 0 then begin
        SocketError('WSocket_WSAIoctl(SIO_KEEPALIVE_VALS)');
        Exit;
    end;
{$ENDIF}
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetLingerOption;
var
    iStatus : Integer;
    li      : TLinger;
begin
    if FLingerOnOff = wsLingerNoSet then
        Exit;                            { Option set is disabled, ignore }

    if FHSocket = INVALID_SOCKET then begin
        RaiseException('Cannot set linger option at this time');
        Exit;
    end;

    li.l_onoff  := Ord(FLingerOnOff);    { 0/1 = disable/enable linger }
    li.l_linger := FLingerTimeout;       { timeout in seconds          }
    iStatus     := WSocket_Synchronized_setsockopt(FHSocket, SOL_SOCKET,
                                      SO_LINGER, li, SizeOf(li));

    if iStatus <> 0 then begin
        SocketError('setsockopt(SO_LINGER)');
        Exit;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Connect;
var
    iStatus : Integer;
    optval  : Integer;
    optlen  : Integer;
    lAddr   : TInAddr;
begin
    if (FHSocket <> INVALID_SOCKET) and (FState <> wsClosed) then begin
        RaiseException('Connect: Socket already in use');
        Exit;
    end;

    if  not FPortAssigned then begin
        RaiseException('Connect: No Port Specified');
        Exit;
    end;

    if not FAddrAssigned then begin
        RaiseException('Connect: No IP Address Specified');
        Exit;
    end;

    if not FProtoAssigned then begin
        RaiseException('Connect: No Protocol Specified');
        Exit;
    end;

    try
        if not FProtoResolved then begin
            { The next line will trigger an exception in case of failure }
            FProto := WSocket_Synchronized_ResolveProto(FProtoStr);
            case FProto of
            IPPROTO_UDP: FType := SOCK_DGRAM;
            IPPROTO_TCP: FType := SOCK_STREAM;
            IPPROTO_RAW: FType := SOCK_RAW;
            else
                         FType := SOCK_RAW;
            end;
            FProtoResolved := TRUE;
        end;

        if not FPortResolved then begin
            { The next line will trigger an exception in case of failure }
            FPortNum      := WSocket_Synchronized_ResolvePort(FPortStr, GetProto);
            sin.sin_port  := WSocket_Synchronized_htons(FPortNum);
            FPortResolved := TRUE;
        end;

        if not FLocalPortResolved then begin
            { The next line will trigger an exception in case of failure }
            FLocalPortNum      := WSocket_Synchronized_ResolvePort(FLocalPortStr, GetProto);
            FLocalPortResolved := TRUE;
        end;

        if not FAddrResolved then begin
            { The next line will trigger an exception in case of failure }
            sin.sin_addr.s_addr := WSocket_Synchronized_ResolveHost(FAddrStr).s_addr;
            FAddrResolved := TRUE;
        end;
    except
        on E:Exception do begin
            RaiseException('connect: ' + E.Message);
            Exit;
        end;
    end;

    { Remove any data from the internal output buffer }
    { (should already be empty !)                     }
    DeleteBufferedData;

    { Open the socket }
    FHSocket := WSocket_Synchronized_socket(FAddrFormat, FType, FProto);
    if FHSocket = INVALID_SOCKET then begin
        SocketError('Connect (socket)');
        Exit;
    end;

    { Get winsock send buffer size }
    optlen  := SizeOf(FSocketSndBufSize);
{$IFDEF CLR}
    iStatus := WSocket_getsockopt(FHSocket, SOL_SOCKET, SO_SNDBUF,
                                  FSocketSndBufSize, optlen);
{$ELSE}
    iStatus := WSocket_getsockopt(FHSocket, SOL_SOCKET, SO_SNDBUF,
                                  PChar(@FSocketSndBufSize), optlen);
{$ENDIF}
    if iStatus <> 0 then begin
        SocketError('getsockopt(SO_SNDBUF)');
        Exit;
    end;

    { Get winsock receive buffer size }
    optlen  := SizeOf(FSocketRcvBufSize);
{$IFDEF CLR}
    iStatus := WSocket_getsockopt(FHSocket, SOL_SOCKET, SO_RCVBUF,
                                  FSocketRcvBufSize, optlen);
{$ELSE}
    iStatus := WSocket_getsockopt(FHSocket, SOL_SOCKET, SO_RCVBUF,
                                  PChar(@FSocketRcvBufSize), optlen);
{$ENDIF}
    if iStatus <> 0 then begin
        SocketError('getsockopt(SO_RCVBUF)');
        Exit;
    end;

    { Trigger OnChangeState event }
    ChangeState(wsOpened);

    if FState <> wsOpened then begin  { 07/07/02 }
        { Socket has been closed in the OnChangeState event ! }
        WSocket_Synchronized_WSASetLastError(WSAEINVAL);
        SocketError('Connect (Invalid operation in OnChangeState)');
        Exit;
    end;

    if FType = SOCK_DGRAM then begin
        BindSocket;
        if FMultiCast then begin
            if FMultiCastIpTTL <> IP_DEFAULT_MULTICAST_TTL then begin
                optval  := FMultiCastIpTTL; { set time-to-live for multicast }
                iStatus := WSocket_Synchronized_SetSockOpt(FHSocket, IPPROTO_IP,
                                                           IP_MULTICAST_TTL,
                                                           optval,
                                                           SizeOf(optval));
                if iStatus <> 0 then begin
                        SocketError('setsockopt(IP_MULTICAST_TTL)');
                        Exit;
                end;
            end;
            if FLocalAddr <> '0.0.0.0' then begin                      { RK }
                laddr.s_addr := WSocket_Synchronized_ResolveHost(FLocalAddr).s_addr;
                iStatus      := WSocket_Synchronized_SetSockOpt(FHSocket, IPPROTO_IP,
                                                                IP_MULTICAST_IF,
                                                                laddr,
                                                                SizeOf(laddr));
                if iStatus <> 0 then begin
                    SocketError('setsockopt(IP_MULTICAST_IF)');
                    Exit;
                end;
            end;                                                       { /RK }
        end;

        if sin.sin_addr.S_addr = u_long(INADDR_BROADCAST) then begin
            OptVal  := 1;
            iStatus := WSocket_Synchronized_setsockopt(FHSocket, SOL_SOCKET, SO_BROADCAST,
                                                       OptVal, SizeOf(OptVal));
            if iStatus <> 0 then begin
                SocketError('setsockopt(SO_BROADCAST)');
                Exit;
            end;
        end;
    end
    else begin
        { Socket type is SOCK_STREAM }
        optval  := -1;
        iStatus := WSocket_Synchronized_setsockopt(FHSocket, SOL_SOCKET,
                                                   SO_REUSEADDR, optval, SizeOf(optval));

        if iStatus <> 0 then begin
            SocketError('setsockopt(SO_REUSEADDR)');
            Exit;
        end;

        if HasOption(FComponentOptions, wsoTcpNoDelay) then begin
            optval := -1; { true, 0=false }
            iStatus := WSocket_Synchronized_setsockopt(FHsocket, IPPROTO_TCP,
                                                       TCP_NODELAY, optval, SizeOf(optval));
            if iStatus <> 0 then begin
                SocketError('setsockopt(IPPROTO_TCP, TCP_NODELAY)');
                Exit;
            end;
        end;

        SetLingerOption;
        SetKeepAliveOption;

        if (FLocalPortNum <> 0) or (FLocalAddr <> '0.0.0.0') then
            BindSocket;
    end;

    FSelectEvent := FD_READ   or FD_WRITE or FD_CLOSE or
                    FD_ACCEPT or FD_CONNECT;
    iStatus       := WSocket_Synchronized_WSAASyncSelect(FHSocket, Handle,
                                                         FMsg_WM_ASYNCSELECT,
                                                         FSelectEvent);
    if iStatus <> 0 then begin
        SocketError('WSAAsyncSelect');
        Exit;
    end;

    if FType = SOCK_DGRAM then begin
        ChangeState(wsConnected);
        TriggerSessionConnectedSpecial(0);
    end
    else begin
{$IFNDEF NO_DEBUG_LOG}
       if CheckLogOptions(loWsockInfo) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
            DebugLog(loWsockInfo, 'TWSocket will connect to ' +
                  WSocket_Synchronized_inet_ntoa(sin.sin_addr) + ':' +
                  IntToStr(WSocket_Synchronized_ntohs(sin.sin_port)));
{$ENDIF}
        iStatus := WSocket_Synchronized_connect(FHSocket, TSockAddr(sin), sizeof(sin));
        if iStatus = 0 then
            ChangeState(wsConnecting)
        else begin
            iStatus := WSocket_Synchronized_WSAGetLastError;
            if iStatus = WSAEWOULDBLOCK then
                ChangeState(wsConnecting)
            else begin
                FLastError := WSocket_Synchronized_WSAGetLastError;
                SocketError('Connect');
                Exit;
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Listen;
var
    iStatus        : Integer;
    optval         : Integer;
    mreq           : ip_mreq;
{$IFDEF WIN32}
    dwBufferInLen  : DWORD;
    dwBufferOutLen : DWORD;
    dwDummy        : DWORD;
{$ENDIF}
begin
    if not FPortAssigned then begin
        WSocket_Synchronized_WSASetLastError(WSAEINVAL);
        SocketError('listen: port not assigned');
        Exit;
    end;

    if not FProtoAssigned then begin
        WSocket_Synchronized_WSASetLastError(WSAEINVAL);
        SocketError('listen: protocol not assigned');
        Exit;
    end;

    if not FAddrAssigned then begin
        WSocket_Synchronized_WSASetLastError(WSAEINVAL);
        SocketError('listen: address not assigned');
        Exit;
    end;

    try
        if not FProtoResolved then begin
            { The next line will trigger an exception in case of failure }
            if CompareText(Copy(FProtoStr, 1, 4), 'raw_') = 0 then begin
                FType  := SOCK_RAW;
                FProto := WSocket_Synchronized_ResolveProto(Copy(FProtoStr, 5, 10));
            end
            else begin
                FProto := WSocket_Synchronized_ResolveProto(FProtoStr);
                if FProto = IPPROTO_UDP then
                    FType := SOCK_DGRAM
                else
                    FType := SOCK_STREAM;
            end;
            FProtoResolved := TRUE;
        end;

        if not FPortResolved then begin
            { The next line will trigger an exception in case of failure }
            FPortNum      := WSocket_Synchronized_ResolvePort(FPortStr, GetProto);
            sin.sin_port  := WSocket_Synchronized_htons(FPortNum);
            FPortResolved := TRUE;
        end;

        if not FAddrResolved then begin
            { The next line will trigger an exception in case of failure }
            sin.sin_addr.s_addr := WSocket_Synchronized_ResolveHost(FAddrStr).s_addr;
            FAddrResolved       := TRUE;
        end;
    except
        on E:Exception do begin
            RaiseException('listen: ' + E.Message);
            Exit;
        end;
    end;

    { Remove any data from the internal output buffer }
    { (should already be empty !)                     }
    DeleteBufferedData;

    FHSocket := WSocket_Synchronized_socket(FAddrFormat, FType, FProto);

    if FHSocket = INVALID_SOCKET then begin
        SocketError('socket');
        exit;
    end;

    if FType = SOCK_DGRAM then begin
        if FReuseAddr then begin
        { Enable multiple tasks to listen on duplicate address and port }
            optval  := -1;
            iStatus := WSocket_Synchronized_SetSockOpt(FHSocket, SOL_SOCKET,
                                                       SO_REUSEADDR,
                                                       optval, SizeOf(optval));

            if iStatus <> 0 then begin
                SocketError('setsockopt(SO_REUSEADDR)');
                Close;
                Exit;
            end;
        end;
    end;

    iStatus := WSocket_Synchronized_bind(FHSocket, TSockAddr(sin), sizeof(sin));
    if iStatus = 0 then
        ChangeState(wsBound)
    else begin
        SocketError('Bind');
        Close;
        Exit;
    end;

    case FType of
{$IFDEF WIN32}
{$IFDEF COMPILER2_UP}
    SOCK_RAW :
        begin
            if HasOption(FComponentOptions, wsoSIO_RCVALL) then begin
                dwBufferInLen  := 1;
                dwBufferOutLen := 0;
                iStatus := WSocket_Synchronized_WSAIoctl(FHSocket, SIO_RCVALL,
                    @dwBufferInLen,  SizeOf(dwBufferInLen),
                    @dwBufferOutLen, SizeOf(dwBufferOutLen),
                    dwDummy, nil, nil);

                if iStatus = SOCKET_ERROR then begin
                    SocketError('WSAIoctl(SIO_RCVALL)');
                    Close;
                    Exit;
                end;
            end;
            ChangeState(wsListening);
            ChangeState(wsConnected);
            TriggerSessionConnectedSpecial(0);
        end;
{$ENDIF}
{$ENDIF}
    SOCK_DGRAM :
        begin
            if FMultiCast then begin
                 { Use setsockopt() to join a multicast group }
                 { mreq.imr_multiaddr.s_addr := WSocket_inet_addr('225.0.0.37');}
                 { mreq.imr_multiaddr.s_addr := sin.sin_addr.s_addr;}
                 { mreq.imr_multiaddr.s_addr := WSocket_inet_addr(FAddrStr);}
                 mreq.imr_multiaddr.s_addr := WSocket_Synchronized_inet_addr(FMultiCastAddrStr);
                 { mreq.imr_interface.s_addr := htonl(INADDR_ANY);} { RK}
                 mreq.imr_interface.s_addr := WSocket_Synchronized_ResolveHost(FAddrStr).s_addr;
                 iStatus := WSocket_Synchronized_SetSockOpt(FHSocket, IPPROTO_IP,
                                                            IP_ADD_MEMBERSHIP,
                                                            mreq, SizeOf(mreq));

                 if iStatus <> 0 then begin
                    SocketError('setsockopt(IP_ADD_MEMBERSHIP)');
                    Exit;
                 end;
            end;
            ChangeState(wsListening);
            ChangeState(wsConnected);
            TriggerSessionConnectedSpecial(0);
        end;
    SOCK_STREAM :
        begin
            iStatus := WSocket_Synchronized_listen(FHSocket, FListenBacklog);
            if iStatus = 0 then
                ChangeState(wsListening)
            else begin
                SocketError('Listen');
                Exit;
            end;
        end;
    else
        SocketError('Listen: unexpected protocol.');
        Exit;
    end;

    { FP:26/09/06 Are FD_READ and FD_WRITE really necessary ? Probably not ! }
    { Lodewijk Ellen reported a problem with W2K3SP1 triggering an AV in     }
    { accept. Keeping only FD_ACCEPT and FD_CLOSE solved the problem.        }
    { Anyway, a listening socket doesn't send nor receive any data so those  }
    { notification are useless.                                              }
    FSelectEvent := FD_ACCEPT or FD_CLOSE;
    if FType <> SOCK_STREAM then
        FSelectEvent := FSelectEvent or FD_READ or FD_WRITE;
    iStatus      := WSocket_Synchronized_WSAASyncSelect(FHSocket, Handle,
                                                        FMsg_WM_ASYNCSELECT,
                                                        FSelectEvent);
    if iStatus <> 0 then begin
        SocketError('WSAASyncSelect');
        exit;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.Accept: TSocket;
var
   len     : Integer;
begin
    if FState <> wsListening then begin
        WSocket_Synchronized_WSASetLastError(WSAEINVAL);
        SocketError('not a listening socket');
        Result := INVALID_SOCKET;
        Exit;
    end;

    len := sizeof(sin);
{$IFDEF DELPHI1} { Delphi 1 }
    FASocket := WSocket_Synchronized_accept(FHSocket, TSockAddr(sin), len);
{$ELSE}
{$IFDEF VER90} { Delphi 2}
    FASocket := WSocket_Synchronized_accept(FHSocket, TSockAddr(sin), len);
{$ELSE}
{$IFDEF CLR}
    FASocket := WSocket_Synchronized_accept(FHSocket, sin, len);
{$ELSE}
    { Delphi 3/4, Bcb 1/3/4 use pointers instead of var parameters }
    FASocket := WSocket_Synchronized_accept(FHSocket, @sin, @len);
{$ENDIF}
{$ENDIF}
{$ENDIF}

    if FASocket = INVALID_SOCKET then begin
        SocketError('Accept');
        Result := INVALID_SOCKET;
        Exit;
    end
    else
        Result := FASocket;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Pause;
begin
    FPaused := TRUE;
    WSocket_Synchronized_WSAASyncSelect(FHSocket, Handle, 0, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Resume;
begin
    FPaused := FALSE;
    WSocket_Synchronized_WSAASyncSelect(FHSocket, Handle,
                                        FMsg_WM_ASYNCSELECT, FSelectEvent);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Shutdown(How : Integer);
begin
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loWsockInfo) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
      DebugLog(loWsockInfo, {$IFNDEF CLR}IntToHex(Integer(Self), 8) + ' ' +{$ENDIF}
                      'TCustomWSocket.Shutdown ' + IntToStr(How) + ' ' +IntToStr(FHSocket));
{$ENDIF}
    if FHSocket <> INVALID_SOCKET then
        WSocket_Synchronized_shutdown(FHSocket, How);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.DeleteBufferedData;
begin
    if Assigned(FBufHandler) then begin
        FBufHandler.Lock;
        try
            FBufHandler.DeleteAllData;
            FBufferedByteCount := 0;
        finally
            FBufHandler.UnLock;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Abort;
begin
    InternalAbort(0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.InternalAbort(ErrCode : Word);
begin
    CancelDnsLookup;
    DeleteBufferedData;
    { Be sure to close as fast as possible (abortive close) }
    if (State = wsConnected) and (FProto = IPPROTO_TCP) then begin
        LingerOnOff := wsLingerOff;
        SetLingerOption;
    end;
    InternalClose(FALSE, ErrCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Close;
begin
    InternalClose(TRUE, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.CloseDelayed;
begin
    PostMessage(Handle, FMsg_WM_CLOSE_DELAYED, 0, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
//procedure TCustomWSocket.Release;
//begin
//    PostMessage(Handle, FMsg_WM_WSOCKET_RELEASE, 0, 0);
//end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.WMCloseDelayed(var msg: TMessage);
begin
    Close;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
//procedure TCustomWSocket.WMRelease(var msg: TMessage);
//begin
//    Destroy;
//end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Flush;
begin
    while (FHSocket <> INVALID_SOCKET) and     { No more socket   }
          (not bAllSent) do begin              { Nothing to send  }
            { Break; }
        TryToSend;
        MessagePump;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.InternalClose(bShut : Boolean; Error : Word);
var
    iStatus : Integer;
{    Buffer  : array [0..127] of Char; }
begin
    if FHSocket = INVALID_SOCKET then begin
        if FState <> wsClosed then begin
            ChangeState(wsClosed);
            AssignDefaultValue;
        end;
        Exit;
    end;

    if FState = wsClosed then
        Exit;

{ 11/10/98 called shutdown(1) instead of shutdown(2). This disables only    }
{ sends. Disabling receives as well produced data lost is some cases.       }
{ Manifest constants for Shutdown                                           }
{  SD_RECEIVE = 0;   disables receives                                      }
{  SD_SEND    = 1;   disables sends, *Use this one for graceful close*      }
{  SD_BOTH    = 2;   disables both sends and receives                       }

    if bShut then
        ShutDown(1);

    if FHSocket <> INVALID_SOCKET then begin
        repeat
            { Close the socket }
            iStatus := WSocket_Synchronized_closesocket(FHSocket);
            if iStatus <> 0 then begin
                FLastError := WSocket_Synchronized_WSAGetLastError;
                if FLastError <> WSAEWOULDBLOCK then begin
                    FHSocket := INVALID_SOCKET;
                    { Ignore the error occuring when winsock DLL not      }
                    { initialized (occurs when using TWSocket from a DLL) }
                    if FLastError = WSANOTINITIALISED then
                        break;
                    SocketError('Disconnect (closesocket)');
                    Exit;
                end;
                MessagePump;
            end;
        until iStatus = 0;
        FHSocket := INVALID_SOCKET;
    end;

    ChangeState(wsClosed);
    if {$IFDEF WIN32}(not (csDestroying in ComponentState)) and {$ENDIF}
       (not FCloseInvoked) {and Assigned(FOnSessionClosed)} then begin
        FCloseInvoked := TRUE;
        TriggerSessionClosed(Error);
    end;
    { 29/09/98 Protect AssignDefaultValue because SessionClosed event handler }
    { may have destroyed the component.                                       }
    try
        AssignDefaultValue;
    except
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.WaitForClose;
var
    lCount    : {$IFDEF FPC} LongWord; {$ELSE} u_long; {$ENDIF}
    Status    : Integer;
    DataBuf   : TWSocketData;
{$IFDEF WIN32}
    Ch        : Char;
{$ENDIF}
begin
    while (FHSocket <> INVALID_SOCKET) and (FState <> wsClosed) do begin
        MessagePump;

        if WSocket_Synchronized_ioctlsocket(FHSocket, FIONREAD, lCount) = SOCKET_ERROR then
            break;
        if lCount > 0 then
            TriggerDataAvailable(0);

{$IFDEF CLR}
        SetLength(DataBuf, 1);
{$ENDIF}
{$IFDEF WIN32}
        DataBuf := @Ch;
{$ENDIF}
        Status := DoRecv(DataBuf, 1, 0);
        if Status <= 0 then begin
            FLastError := WSocket_Synchronized_WSAGetLastError;
            if FLastError <> WSAEWOULDBLOCK then
                break;
        end;
        MessagePump;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF CLR}
function WSocketGetHostByAddr(const Addr : String) : IntPtr;
var
    lAddr        : u_long;
    LookupIntPtr : IntPtr;
begin
    if Length(Addr) = 0 then
        raise ESocketException.Create('Winsock Get Host Addr: Invalid address.');   { V5.26 }

    lAddr        := WSocket_inet_addr(Addr);
    LookupIntPtr := WSocket_gethostbyaddr(lAddr, 4, PF_INET);
    Result       := LookupIntPtr;
end;
{$ENDIF}
{$IFDEF WIN32}
function WSocketGetHostByAddr(Addr : String) : PHostEnt;
var
    szAddr : array[0..256] of char;
    lAddr  : u_long;
begin
    if (Length(Addr) = 0) or (Length(Addr) >= SizeOf(szAddr)) then
        raise ESocketException.Create('Winsock Get Host Addr: Invalid address.');   { V5.26 }

    StrPCopy(szAddr, Addr); { Length already checked above }
    lAddr  := WSocket_inet_addr(szAddr);
    Result := WSocket_gethostbyaddr(PChar(@lAddr), 4, PF_INET);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocketResolveIp(IpAddr : String) : String;
{$IFDEF CLR}
var
    HostEntry    : THostEnt;
    LookupIntPtr : IntPtr;
begin
    LookupIntPtr := WSocketGetHostByAddr(IpAddr);
    if LookupIntPtr = IntPtr.Zero then
        Result := ''
    else begin
        HostEntry    := THostEnt(Marshal.PtrToStructure(LookupIntPtr,
                                                        TypeOf(THostEnt)));
        Result    := Marshal.PtrToStringAnsi(HostEntry.h_name);
    end;
end;
{$ENDIF}
{$IFDEF WIN32}
var
    Phe : PHostEnt;
begin
    phe := WSocketGetHostByAddr(IpAddr);
    if Phe = nil then
        Result := ''
    else begin
        SetLength(Result, StrLen(Phe^.h_name));
        StrCopy(@Result[1], Phe^.h_name);
    end;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF CLR}
function WSocketGetHostByName(const Name : String) : IntPtr;
begin
    if Length(Name) = 0 then
        raise ESocketException.Create('Winsock Get Host Name: Invalid Hostname.');   { V5.26 }

    Result := WSocket_gethostbyname(Name);
end;
{$ENDIF}
{$IFDEF WIN32}
function WSocketGetHostByName(Name : String) : PHostEnt;
var
    szName : array[0..256] of char;
begin
    if (Length(Name) = 0) or (Length(Name) >= SizeOf(szName)) then
        raise ESocketException.Create('Winsock Get Host Name: Invalid Hostname.');   { V5.26 }

    StrPCopy(szName, Name);
    Result := WSocket_gethostbyname(szName);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function LocalIPList : TStrings;
{$IFDEF CLR}
var
    HostEntry    : THostEnt;
    LookupIntPtr : IntPtr;
begin
    IPList.Clear;
    Result := IPList;

    LookupIntPtr := WSocketGetHostByName(LocalHostName);
    if LookupIntPtr <> IntPtr.Zero then begin
        HostEntry := THostEnt(Marshal.PtrToStructure(LookupIntPtr,
                                                     TypeOf(THostEnt)));
        GetIpList(HostEntry, IPList);
    end;
end;
{$ENDIF}
{$IFDEF WIN32}
var
    phe  : PHostEnt;
begin
    IPList.Clear;
    Result := IPList;

    phe  := WSocketGetHostByName(LocalHostName);
    if phe <> nil then
        GetIpList(Phe, IPList);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function LocalHostName : String;
begin
    if WSocket_gethostname(Result) <> 0 then
        raise ESocketException.Create('Winsock Get Host Name failed');

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.TimerIsSet(var tvp : TTimeVal) : Boolean;
begin
    Result := (tvp.tv_sec <> 0) or (tvp.tv_usec <> 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.TimerCmp(var tvp : TTimeVal; var uvp : TTimeVal; IsEqual : Boolean) : Boolean;
begin
    Result := (tvp.tv_sec = uvp.tv_sec) and (tvp.tv_usec = uvp.tv_usec);
    if not IsEqual then
        Result := not Result;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.TimerClear(var tvp : TTimeVal);
begin
   tvp.tv_sec  := 0;
   tvp.tv_usec := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetSendFlags(newValue : TSocketSendFlags);
begin
    case newValue of
    wsSendNormal: FSendFlags := 0;
    wsSendUrgent: FSendFlags := MSG_OOB;
    else
        RaiseException('Invalid SendFlags');
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetSendFlags : TSocketSendFlags;
begin
    case FSendFlags of
    0       : Result := wsSendNormal;
    MSG_OOB : Result := wsSendUrgent;
    else
        RaiseException('Invalid internal SendFlags');
        Result := wsSendNormal;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.TriggerDebugDisplay(Msg : String);
begin
    if Assigned(FOnDebugDisplay) then
        FOnDebugDisplay(Self, Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.TriggerSendData(BytesSent : Integer);
begin
    if Assigned(FOnSendData) then
        FOnSendData(Self, BytesSent);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.TriggerSessionAvailable(Error : Word);
begin
    if Assigned(FOnSessionAvailable) then
        FOnSessionAvailable(Self, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.TriggerSessionConnectedSpecial(Error : Word);
begin
    TriggerSessionConnected(Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.TriggerSessionConnected(Error : Word);
begin
    if Assigned(FOnSessionConnected) then
        FOnSessionConnected(Self, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.TriggerSessionClosed(Error : Word);
begin
    if Assigned(FOnSessionClosed) then
        FOnSessionClosed(Self, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.TriggerDataAvailable(Error : Word) : Boolean;
begin
    Result := Assigned(FOnDataAvailable);
    if not Result then
        Exit;
{$IFDEF TOMASEK}                    { 23/01/99 }
    { Do not allow FD_READ messages, this will prevent reentering the }
    { OnDataAvailable event handler.                                  }
    FSelectEvent := FD_WRITE or FD_CLOSE or FD_CONNECT;
    WSocket_Synchronized_WSAASyncSelect(FHSocket, Handle, WM_ASYNCSELECT, FSelectEvent);
    try
        FRcvdFlag := TRUE;
        while Result and FRcvdFlag do begin
            { Trigger user code. This will normally call DoRecv which will }
            { update FRcvdFlag.                                            }
            { If user code is wrong, we'll loop forever !                  }
            FOnDataAvailable(Self, Error);
            Result := Assigned(FOnDataAvailable);
        end;
    finally
        { Allow all events now }
        FSelectEvent := FD_READ or FD_WRITE or FD_CLOSE or FD_CONNECT;
        WSocket_Synchronized_WSAASyncSelect(FHSocket, Handle, WM_ASYNCSELECT, FSelectEvent);
    end;
{$ELSE}                             { 23/01/99 }
    FOnDataAvailable(Self, Error);  { 23/01/99 }
{$ENDIF}                            { 23/01/99 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.TriggerDataSent(Error : Word);
begin
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loWsockDump) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
        DebugLog(loWsockDump, {$IFNDEF CLR}IntToHex(Integer(Self), 8) + ' ' +{$ENDIF}
                      'TriggerDataSent ' + IntToStr(FHSocket));
{$ENDIF}
    if Assigned(FOnDataSent) then
        FOnDataSent(Self, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.TriggerError;
begin
    if Assigned(FOnError) then
        FOnError(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.TriggerDNSLookupDone(Error : Word);
begin
    if Assigned(FOnDNSLookupDone) then
        FOnDNSLookupDone(Self, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.TriggerChangeState(OldState, NewState : TSocketState);
begin
    if Assigned(FOnChangeState) then
        FOnChangeState(Self, OldState, NewState);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SocketError(sockfunc: String);
var
    Error  : Integer;
    Line   : String;
begin
    Error := WSocket_Synchronized_WSAGetLastError;
{    Line  := 'Error '+ IntToStr(Error) + ' in function ' + sockfunc +
             #13#10 + WSocketErrorDesc(Error);  }
    Line  := WSocketErrorDesc(Error) + ' (#' + IntToStr(Error) +
                                         ' in ' + sockfunc + ')' ;   { V5.26 }

    if (Error = WSAECONNRESET) or
       (Error = WSAENOTCONN)   then begin
        WSocket_Synchronized_closesocket(FHSocket);
        FHSocket := INVALID_SOCKET;
        if FState <> wsClosed then
           TriggerSessionClosed(Error);
        ChangeState(wsClosed);
    end;

    FLastError := Error;
    RaiseException(Line);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *} { V5.21 }
{$IFNDEF NO_DEBUG_LOG}
function TCustomWSocket.CheckLogOptions(const LogOption: TLogOption): Boolean;
begin
    Result := Assigned(FIcsLogger) and (LogOption in FIcsLogger.LogOptions);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.DebugLog(LogOption: TLogOption; const Msg: String);  { V5.21 }
begin
    if Assigned(FIcsLogger) then
        FIcsLogger.DoDebugLog(Self, LogOption, Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *} { V5.21 }
procedure TCustomWSocket.SetIcsLogger(const Value: TIcsLogger);
begin
    FIcsLogger := Value;
    if Value <> nil then
        Value.FreeNotification(Self);
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetSocketRcvBufSize(BufSize : Integer);
var
    iStatus : Integer;
    optlen  : Integer;
begin
    optlen  := SizeOf(BufSize);
{$IFDEF CLR}
    iStatus := WSocket_setsockopt(FHSocket, SOL_SOCKET, SO_RCVBUF,
                                  BufSize, optlen);
{$ELSE}
    iStatus := WSocket_setsockopt(FHSocket, SOL_SOCKET, SO_RCVBUF,
                                  PChar(@BufSize), optlen);
{$ENDIF}
    if iStatus = 0 then
        FSocketSndBufSize := BufSize
    else
        SocketError('setsockopt(SO_RCVBUF)');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetSocketSndBufSize(BufSize : Integer);
var
    iStatus : Integer;
    optlen  : Integer;
begin
    optlen  := SizeOf(BufSize);
{$IFDEF CLR}
    iStatus := WSocket_setsockopt(FHSocket, SOL_SOCKET, SO_SNDBUF,
                                  BufSize, optlen);
{$ELSE}
    iStatus := WSocket_setsockopt(FHSocket, SOL_SOCKET, SO_SNDBUF,
                                  PChar(@BufSize), optlen);
{$ENDIF}
    if iStatus = 0 then
        FSocketSndBufSize := BufSize
    else
        SocketError('setsockopt(SO_SNDBUF)');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocketErrorDesc(ErrCode : Integer) : String;
begin
    case ErrCode of
    0:
      WSocketErrorDesc := 'No Error';
    WSAEINTR:
      WSocketErrorDesc := 'Interrupted system call';
    WSAEBADF:
      WSocketErrorDesc := 'Bad file number';
    WSAEACCES:
      WSocketErrorDesc := 'Permission denied';
    WSAEFAULT:
      WSocketErrorDesc := 'Bad address';
    WSAEINVAL:
      WSocketErrorDesc := 'Invalid argument';
    WSAEMFILE:
      WSocketErrorDesc := 'Too many open files';
    WSAEWOULDBLOCK:
      WSocketErrorDesc := 'Operation would block';
    WSAEINPROGRESS:
      WSocketErrorDesc := 'Operation now in progress';
    WSAEALREADY:
      WSocketErrorDesc := 'Operation already in progress';
    WSAENOTSOCK:
      WSocketErrorDesc := 'Socket operation on non-socket';
    WSAEDESTADDRREQ:
      WSocketErrorDesc := 'Destination address required';
    WSAEMSGSIZE:
      WSocketErrorDesc := 'Message too long';
    WSAEPROTOTYPE:
      WSocketErrorDesc := 'Protocol wrong type for socket';
    WSAENOPROTOOPT:
      WSocketErrorDesc := 'Protocol not available';
    WSAEPROTONOSUPPORT:
      WSocketErrorDesc := 'Protocol not supported';
    WSAESOCKTNOSUPPORT:
      WSocketErrorDesc := 'Socket type not supported';
    WSAEOPNOTSUPP:
      WSocketErrorDesc := 'Operation not supported on socket';
    WSAEPFNOSUPPORT:
      WSocketErrorDesc := 'Protocol family not supported';
    WSAEAFNOSUPPORT:
      WSocketErrorDesc := 'Address family not supported by protocol family';
    WSAEADDRINUSE:
      WSocketErrorDesc := 'Address already in use';
    WSAEADDRNOTAVAIL:
      WSocketErrorDesc := 'Address not available';
    WSAENETDOWN:
      WSocketErrorDesc := 'Network is down';
    WSAENETUNREACH:
      WSocketErrorDesc := 'Network is unreachable';
    WSAENETRESET:
      WSocketErrorDesc := 'Network dropped connection on reset';
    WSAECONNABORTED:
      WSocketErrorDesc := 'Connection aborted';
    WSAECONNRESET:
      WSocketErrorDesc := 'Connection reset by peer';
    WSAENOBUFS:
      WSocketErrorDesc := 'No buffer space available';
    WSAEISCONN:
      WSocketErrorDesc := 'Socket is already connected';
    WSAENOTCONN:
      WSocketErrorDesc := 'Socket is not connected';
    WSAESHUTDOWN:
      WSocketErrorDesc := 'Can''t send after socket shutdown';
    WSAETOOMANYREFS:
      WSocketErrorDesc := 'Too many references: can''t splice';
    WSAETIMEDOUT:
      WSocketErrorDesc := 'Connection timed out';
    WSAECONNREFUSED:
      WSocketErrorDesc := 'Connection refused';
    WSAELOOP:
      WSocketErrorDesc := 'Too many levels of symbolic links';
    WSAENAMETOOLONG:
      WSocketErrorDesc := 'File name too long';
    WSAEHOSTDOWN:
      WSocketErrorDesc := 'Host is down';
    WSAEHOSTUNREACH:
      WSocketErrorDesc := 'No route to host';
    WSAENOTEMPTY:
      WSocketErrorDesc := 'Directory not empty';
    WSAEPROCLIM:
      WSocketErrorDesc := 'Too many processes';
    WSAEUSERS:
      WSocketErrorDesc := 'Too many users';
    WSAEDQUOT:
      WSocketErrorDesc := 'Disc quota exceeded';
    WSAESTALE:
      WSocketErrorDesc := 'Stale NFS file handle';
    WSAEREMOTE:
      WSocketErrorDesc := 'Too many levels of remote in path';
    WSASYSNOTREADY:
      WSocketErrorDesc := 'Network sub-system is unusable';
    WSAVERNOTSUPPORTED:
      WSocketErrorDesc := 'WinSock DLL cannot support this application';
    WSANOTINITIALISED:
      WSocketErrorDesc := 'WinSock not initialized';
    WSAHOST_NOT_FOUND:
      WSocketErrorDesc := 'Host not found';
    WSATRY_AGAIN:
      WSocketErrorDesc := 'Non-authoritative host not found';
    WSANO_RECOVERY:
      WSocketErrorDesc := 'Non-recoverable error';
    WSANO_DATA:
      WSocketErrorDesc := 'No Data';
    else
      WSocketErrorDesc := 'Not a WinSock error';
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetWinsockErr(ErrCode: Integer): String ;    { V5.26 }
begin
    Result := WSocketErrorDesc(ErrCode) + ' (#' + IntToStr(ErrCode) + ')' ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetWindowsErr(ErrCode: Integer): String ;    { V5.26 }
begin
    Result := SysErrorMessage(ErrCode) + ' (#' + IntToStr(ErrCode) + ')' ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

         X X        X X        X X       X      X      X X      X X X X
       X     X    X     X    X     X     X     X     X     X    X
       X          X     X    X           X   X       X          X
         X X      X     X    X           X X           X X        X X
             X    X     X    X           X   X             X          X
       X     X    X     X    X     X     X     X     X     X    X     X
         X X        X X        X X       X      X      X  X       X X

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TCustomSocksWSocket.Create{$IFDEF VCL}(AOwner: TComponent){$ENDIF};
begin
    inherited Create{$IFDEF VCL}(AOwner){$ENDIF};
    FSocksUsercode := '';
    FSocksPassword := '';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSocksWSocket.AssignDefaultValue;
begin
    inherited AssignDefaultValue;
    FSocksState          := socksData;
    FSocksServer         := '';
    FSocksPort           := '';
    FSocksLevel          := '5';
    FSocksRcvdCnt        := 0;
    FSocksPortAssigned   := FALSE;
    FSocksServerAssigned := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSocksWSocket.SetSocksLevel(newValue : String);
begin
    if State <> wsClosed then begin
        RaiseException('Can''t change socks level if not closed');
        Exit;
    end;
    if (newValue <> '4')  and (newValue <> '5') and
       (newValue <> '4A') and (newValue <> '4a') then begin
        RaiseException('Invalid socks level. Must be 4, 4A or 5.');
        Exit;
    end;
    FSocksLevel := UpperCase(newValue);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSocksWSocket.SetSocksPort(sPort : String);
begin
    if State <> wsClosed then begin
        RaiseException('Can''t change socks port if not closed');
        Exit;
    end;
    FSocksPort := Trim(sPort);
    if Length(FSocksPort) = 0 then begin
        FSocksPortAssigned := FALSE;
        Exit;
    end;
    FSocksPortAssigned := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSocksWSocket.SetSocksServer(sServer : String);
begin
    if State <> wsClosed then begin
        RaiseException('Can''t change socks server if not closed');
        Exit;
    end;
    FSocksServer := Trim(sServer);
    if Length(FSocksServer) = 0 then begin
        FSocksServerAssigned := FALSE;
        Exit;
    end;
    FSocksServerAssigned := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSocksWSocket.Listen;
begin
    { Check if we really wants to use socks server }
    if not FSocksServerAssigned then begin
        { No socks server assigned, Listen as usual }
        inherited Listen;
        Exit;
    end;
    RaiseException('Listening is not supported thru socks server');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSocksWSocket.Connect;
begin
    { Check if we really wants to use socks server }
    if not FSocksServerAssigned then begin
        { No socks server assigned, connect as usual }
        inherited Connect;
        Exit;
    end;

    if (LowerCase(FProtoStr) <> 'tcp') and (Trim(FProtoStr) <> '6') then begin
        RaiseException('TCP is the only protocol supported thru socks server'); { V5.26 }
        Exit;
    end;

    try
        if not FPortResolved then begin
            { The next line will trigger an exception in case of failure }
            sin.sin_port  := WSocket_Synchronized_htons(WSocket_Synchronized_ResolvePort(FSocksPort, FProtoStr));
            FPortResolved := TRUE;
        end;

        if not FAddrResolved then begin
            { The next line will trigger an exception in case of failure }
            sin.sin_addr.s_addr := WSocket_Synchronized_ResolveHost(FSocksServer).s_addr;
            FAddrResolved       := TRUE;
        end;
        { The next line will trigger an exception in case of failure }
        FPortNum := WSocket_Synchronized_ResolvePort(FPortStr, FProtoStr);
    except
        on E:Exception do begin
            RaiseException('Connect: ' + E.Message);  { V5.26 }
            Exit;
        end;
    end;

    FSocksState := socksNegociateMethods;
    FRcvCnt     := 0;
    inherited Connect;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{function BufToStr(Buf : PChar; Cnt : Integer) : String;
begin
    Result := '';
    while Cnt > 0 do begin
        if Buf^ in [#32..#126] then
            Result := Result + Buf^
        else
            Result := Result + '#' + Format('%2.2d', [ord(Buf^)]);
        Inc(Buf);
        Dec(Cnt);
    end;
end;}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSocksWSocket.TriggerSessionConnectedSpecial(Error : Word);
var
    Buf : {$IFDEF CLR}TBytes;{$ELSE}array [0..2] of char;{$ENDIF}
begin
    if FSocksState = socksNegociateMethods then begin
        {ChangeState(wsSocksConnected);}
        TriggerSocksConnected(Error);
        if Error <> 0 then begin
            inherited TriggerSessionConnectedSpecial(Error);
            Exit;
        end;
        if FSocksLevel[1] = '4' then
            SocksDoConnect
        else begin
            if FSocksAuthentication = socksNoAuthentication then
                FSocksAuthNumber := #$00        { No authentification }
            else
                FSocksAuthNumber := #$02;       { Usercode/Password   }

            {$IFDEF CLR}
            SetLength(Buf, 3);
            Buf[0] := $05;                      { Version number      }
            Buf[1] := $01;                      { Number of methods   }
            Buf[2] := Ord(FSocksAuthNumber);    { Method identifier   }
            Send(Buf, 3);
            {$ELSE}
            Buf[0] := #$05;                     { Version number      }
            Buf[1] := #$01;                     { Number of methods   }
            Buf[2] := FSocksAuthNumber;         { Method identifier   }
            Send(@Buf, 3);
            {$ENDIF}
        end;
    end
    else
        inherited TriggerSessionConnectedSpecial(Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSocksWSocket.TriggerSessionClosed(Error : Word);
begin
    if FSocksState = socksAuthenticate then
        TriggerSocksAuthState(socksAuthFailure);
    inherited TriggerSessionClosed(Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSocksWSocket.TriggerSocksConnected(Error : Word);
begin
    if Assigned(FOnSocksConnected) then
        FOnSocksConnected(Self, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSocksWSocket.TriggerSocksError(Error : Integer; Msg : String);
begin
    if Assigned(FOnSocksError) then
        FOnSocksError(Self, Error, Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSocksWSocket.TriggerSocksAuthState(AuthState : TSocksAuthState);
begin
    if Assigned(FOnSocksAuthState) then
        FOnSocksAuthState(Self, AuthState);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSocksWSocket.SocksDoAuthenticate;
{$IFDEF CLR}
var
    Buf     : TBytes;
    I, J    : Integer;
begin
    FSocksState := socksAuthenticate;
    TriggerSocksAuthState(socksAuthStart);
    SetLength(Buf, 128);
    I      := 0;
    Buf[I] := $01; {06/03/99}           { Socks version }
    Inc(I);
    Buf[I] := Length(FSocksUsercode);
    for J := 1 to Length(FSocksUsercode) do begin
        Inc(I);
        Buf[I] := Ord(FSocksUsercode[J]);
    end;
    Inc(I);
    Buf[I] := Length(FSocksPassword);
    for J := 1 to Length(FSocksPassword) do begin
        Inc(I);
        Buf[I] := Ord(FSocksPassword[J]);
    end;
    Inc(I);
    try
{TriggerDisplay('Send = ''' + BufToStr(Buf, I) + '''');}
        Send(Buf, I);
    except
    end;
end;
{$ENDIF}
{$IFDEF WIN32}
var
    Buf     : array [0..127] of char;
    I       : Integer;
begin
    FSocksState := socksAuthenticate;
    TriggerSocksAuthState(socksAuthStart);
    Buf[0] := #$01; {06/03/99}           { Socks version }
    I      := 1;
    Buf[I] := chr(Length(FSocksUsercode));
    Move(FSocksUsercode[1], Buf[I + 1], Length(FSocksUsercode));
    I := I + 1 + Length(FSocksUsercode);
    Buf[I] := chr(Length(FSocksPassword));
    Move(FSocksPassword[1], Buf[I + 1], Length(FSocksPassword));
    I := I + 1 + Length(FSocksPassword);
    try
{TriggerDisplay('Send = ''' + BufToStr(Buf, I) + '''');}
        Send(@Buf, I);
    except
    end;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSocksWSocket.SocksDoConnect;
{$IFDEF CLR}
var
    Buf     : TBytes;
    I, J    : Integer;
    ErrCode : Integer;
    IP      : u_long;
begin
    FSocksState := socksConnect;
    if FSocksLevel[1] = '4' then begin
        SetLength(Buf, 128);
        Buf[0] := 4;                                 { Version number  }
        Buf[1] := 1;                                 { Connect command }
        { Todo: Check the byte order ! }
        Buf[2] := (FPortNum shr 8) and 255;          { Port high byte  }
        Buf[3] := FPortNum and 255;                  { Port low byte   }
        if FSocksLevel = '4A' then begin
            { Conventional IP saying we can't convert the destination   }
            { host's domain name to find its IP address                 }
            { The destination must then follow the user ID              }
            Buf[4] := 0;
            Buf[5] := 0;
            Buf[6] := 0;
            Buf[7] := 1;
        end
        else begin
            { With original SOCKS4, we have to supply the dest address  }
            try
                IP     := WSocketResolveHost(FAddrStr).s_addr;
                { Todo: Check the byte order ! }
                Buf[4] := (IP shr 24) and 255;
                Buf[5] := (IP shr 16) and 255;
                Buf[6] := (IP shr  8) and 255;
                Buf[7] := IP and 255;
            except
                on E:Exception do begin
                     ErrCode := socksHostResolutionFailed;
                     TriggerSocksError(ErrCode, E.ClassName + ' ' + E.Message);
                     InternalClose(TRUE, ErrCode);
                     Exit;
                end;
            end;
        end;
        I := 8;
        if Length(FSocksUsercode) > 0 then begin
            { I'm not sure it has to be like that ! Should I also use the }
            { password or not ?                                           }
            for J := 1 to Length(FSocksUsercode) do begin
                Buf[I] := Ord(FSocksUsercode[J]);
                Inc(I);
            end;
        end;
        Buf[I] := 0;
        Inc(I);
        if FSocksLevel = '4A' then begin
            { We have to supply the destination host name                 }
            for J := 1 to Length(FAddrStr) do begin
                Buf[I] := Ord(FaddrStr[J]);
                Inc(I);
            end;
            Buf[I] := 0;   { Alon Gingold }
            Inc(I);        { Alon Gingold }
        end;
        { Buf[I] := #0;      Alon Gingold }
        { Inc(I);            Alon Gingold }
    end
    else begin
        SetLength(Buf, 128);
        Buf[0] := $05;            { Socks version }
        Buf[1] := $01;            { Connect command }
        Buf[2] := $00;            { Reserved, must be $00 }
        Buf[3] := $03;            { Address type is domain name }
        Buf[4] := Length(FAddrStr);
        { Should check buffer overflow }
        I := 5;
        for J := 1 to Length(FAddrStr) do begin
            Buf[I] := Ord(FaddrStr[J]);
            Inc(I);
        end;
        { Todo: Check the byte order ! }
        Buf[I] := (FPortNum shr 8) and 255;          { Port high byte  }
        Inc(I);
        Buf[I] := FPortNum and 255;                  { Port low byte   }
        Inc(I);
    end;

    try
{TriggerDisplay('Send = ''' + BufToStr(Buf, I + 2) + '''');}
//MessageBox(0, BufToStr(Buf, I), 'SocksDoConnect', MB_OK);
        Send(Buf, I);
    except
    end;
end;
{$ENDIF}
{$IFDEF WIN32}
type
    pu_long = ^u_long;
var
    Buf     : array [0..127] of char;
    I       : Integer;
    ErrCode : Integer;
begin
    FSocksState := socksConnect;
    if FSocksLevel[1] = '4' then begin
        Buf[0] := #4;                                 { Version number  }
        Buf[1] := #1;                                 { Connect command }
        PWORD(@Buf[2])^  := WSocket_Synchronized_ntohs(FPortNum);  { Dest port       }
        if FSocksLevel = '4A' then
            { Conventional IP saying we can't convert the destination   }
            { host's domain name to find its IP address                 }
            { The destination must then follow the user ID              }
            pu_long(@Buf[4])^ := WSocket_Synchronized_inet_addr('0.0.0.1')
        else begin
            { With original SOCKS4, we have to supply the dest address  }
            try
                pu_long(@Buf[4])^ := WSocket_Synchronized_ResolveHost(FAddrStr).s_addr;
            except
                on E:Exception do begin
                     ErrCode := socksHostResolutionFailed;
                     TriggerSocksError(ErrCode, E.ClassName + ' ' + E.Message);
                     InternalClose(TRUE, ErrCode);
                     Exit;
                end;
            end;
        end;
        I := 8;
        if Length(FSocksUsercode) > 0 then begin
            { I'm not sure it has to be like that ! Should I also use the }
            { password or not ?                                           }
            Move(FSocksUsercode[1], Buf[I], Length(FSocksUsercode));
            I := I + Length(FSocksUsercode);
        end;
        Buf[I] := #0;
        Inc(I);
        if FSocksLevel = '4A' then begin
            { We have to supply the destination host name                 }
            Move(FAddrStr[1], Buf[I], Length(FAddrStr));
            I := I + Length(FAddrStr);
            Buf[I] := #0;  { Alon Gingold }
            Inc(I);        { Alon Gingold }
        end;
        { Buf[I] := #0;      Alon Gingold }
        { Inc(I);            Alon Gingold }
    end
    else begin
        Buf[0] := #$05;            { Socks version }
        Buf[1] := #$01;            { Connect command }
        Buf[2] := #$00;            { Reserved, must be $00 }
        Buf[3] := #$03;            { Address type is domain name }
        Buf[4] := chr(Length(FAddrStr));
        { Should check buffer overflow }
        Move(FAddrStr[1], Buf[5], Length(FAddrStr));
        I := 5 + Length(FAddrStr);
        PWord(@Buf[I])^ := WSocket_Synchronized_htons(FPortNum);
        I := I + 2;
    end;

    try
{TriggerDisplay('Send = ''' + BufToStr(Buf, I + 2) + '''');}
        Send(@Buf, I);
    except
    end;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSocksWSocket.DataAvailableError(
    ErrCode : Integer;
    Msg     : String);
begin
{   TriggerSocksError(ErrCode, Msg); }
{   inherited TriggerSessionConnectedSpecial(ErrCode); }
{   InternalClose(TRUE, ErrCode); }
    TriggerSocksError(ErrCode, Msg);
    FSocksState := socksData;
    {**ALON** Added, so TriggerSessionConnectedSpecial will only call inherited}
    {inherited} TriggerSessionConnectedSpecial(ErrCode);
    {**ALON** removed 'inherited' now calls top level}
    InternalClose(TRUE, ErrCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomSocksWSocket.TriggerDataAvailable(Error : Word) : Boolean;
var
    Len     : Integer;
    I       : Integer;
    ErrCode : Word;
    ErrMsg  : String;
    InAddr  : TInAddr;
    AnsLen  : Integer;
{$IFDEF CLR}
    J       : Integer;
    Buf     : TBytes;
{$ENDIF}
begin
    if FSocksState = socksData then begin
        Result := inherited TriggerDataAvailable(Error);
        Exit;
    end;

    if Error <> 0 then begin
        DataAvailableError(Error, 'data receive error');
        Result := FALSE;
        Exit;
    end;

{$IFDEF CLR}
    if Length(FRcvBuf) <> 128 then
        SetLength(FRcvBuf, 128);
    if Length(Buf) <> 128 then
        SetLength(Buf, 128);
{$ENDIF}

    if FSocksState = socksNegociateMethods then begin
        Result := TRUE;
{$IFDEF CLR}
        Len := Receive(Buf, Length(Buf) - FRcvCnt - 1);
        if Len < 0 then
            Exit;
        for I := 0 to Len - 1 do begin
            FRcvBuf[FRcvCnt] := Buf[I];
            Inc(FRcvCnt);
        end;
{$ENDIF}
{$IFDEF WIN32}
        Len := Receive(@FRcvBuf[FRcvCnt], Sizeof(FRcvBuf) - FRcvCnt - 1);
        if Len < 0 then
            Exit;
        FRcvCnt := FRcvCnt + Len;
{$ENDIF}
{TriggerDisplay('socksNegociateMethods FrcvBuf = ''' + BufToStr(FRcvBuf, FRcvCnt) + '''');}
        if FSocksLevel[1] = '4' then begin
            { We should never comes here }
            DataAvailableError(socksProtocolError, 'TWSocket logic error');
            Exit;
        end
        else begin  { SOCKS5 }
            { We are waiting only two bytes }
            if FRcvCnt < 2 then
                Exit;
{            if FRcvCnt <> 2 then begin  06/03/99}
{                DataAvailableError(socksProtocolError, 'too much data availaible');}
{                Exit;                                                              }
{            end;                                                                   }
            FRcvCnt := 0; { Clear receive counter }
            if FRcvBuf[0] <> $05 then begin
                DataAvailableError(socksVersionError, 'socks version error');
                Exit;
            end;
            if FRcvBuf[1] = $00 then begin
                { No authentication required }
                if FSocksAuthNumber <> #$00 then
                    { We asked for authentification, so complains... }
                    TriggerSocksAuthState(socksAuthNotRequired);
            end
            else if FRcvBuf[1] = $02 then begin
                { Usercode/Password authentication required }
                SocksDoAuthenticate;
                Exit;
            end
            else begin
                DataAvailableError(socksAuthMethodError, 'authentification method not acceptable');
                Exit;
            end;
            SocksDoConnect;
        end;
    end
    else if FSocksState = socksConnect then begin
        Result := TRUE;
{TriggerDisplay('socksConnect FrcvBuf = ''' + BufToStr(FRcvBuf, FRcvCnt) + '''');}
        if FSocksLevel[1] = '4' then begin
            { We wants at most 8 characters }
{$IFDEF CLR}
            Len := Receive(Buf, 8 - FRcvCnt);
            if Len < 0 then
                Exit;
            for I := 0 to Len - 1 do begin
                FRcvBuf[FRcvCnt] := Buf[I];
                Inc(FRcvCnt);
            end;
{$ENDIF}
{$IFDEF WIN32}
            Len := Receive(@FRcvBuf[FRcvCnt], 8 - FRcvCnt);
            if Len < 0 then
                Exit;
            FRcvCnt := FRcvCnt + Len;
{$ENDIF}
            { We are waiting for 8 bytes }
            if FRcvCnt < 8 then
                Exit;
            FRcvCnt := 0; { Clear receive counter }
            if FRcvBuf[0] <> 0 then begin
                DataAvailableError(socksVersionError, 'socks version error');
                Exit;
            end;
            if FRcvBuf[1] <> 90 then begin  { david.brock }
                case FRcvBuf[1] of
                91: ErrCode := socksRejectedOrFailed;
                92: ErrCode := socksConnectionRefused;
                93: ErrCode := socksAuthenticationFailed;
                else
                   ErrCode := socksUnassignedError;
                end;
                case ErrCode of
                socksRejectedOrFailed :
                    ErrMsg := 'request rejected or failed';
                socksConnectionRefused :
                    ErrMsg := 'connection refused';
                socksAuthenticationFailed :
                    ErrMsg := 'authentification failed';
                else
                    ErrMsg := 'unassigned error #' + IntToStr(Ord(FRcvBuf[1]));
                end;
                DataAvailableError(ErrCode, ErrMsg);
                Exit;
            end;
            FSocksState := socksData;
{           inherited TriggerSessionConnectedSpecial(0); }
{           Result := inherited TriggerDataAvailable(0); }
            {inherited} TriggerSessionConnectedSpecial(0);
            {**ALON** removed 'inherited' now calls top level}
            Result := {inherited} TriggerDataAvailable(0);
            {**ALON** removed 'inherited' now calls top level}
        end
        else begin { SOCKS5 }
            {$IFDEF CLR}
            Len := Receive(Buf, Length(Buf) - FRcvCnt - 1);
            if Len < 0 then
                Exit;
            for I := 0 to Len - 1 do begin
                FRcvBuf[FRcvCnt] := Buf[I];
                Inc(FRcvCnt);
            end;
            {$ENDIF}
            {$IFDEF WIN32}
            Len := Receive(@FRcvBuf[FRcvCnt], Sizeof(FRcvBuf) - FRcvCnt - 1);
            if Len < 0 then
                Exit;
            FRcvCnt := FRcvCnt + Len;
            {$ENDIF}
            if FRcvCnt >= 1 then begin
                { First byte is version, we expect version 5 }
                if FRcvBuf[0] <> $05 then begin
                    DataAvailableError(socksVersionError, 'socks version error');
                    Exit;
                end;
            end;
            if FRcvCnt >= 2 then begin
                if FRcvBuf[1] <> $00 then begin
                    case FRcvBuf[1] of
                    1: ErrCode := socksGeneralFailure;
                    2: ErrCode := socksConnectionNotAllowed;
                    3: ErrCode := socksNetworkUnreachable;
                    4: ErrCode := socksHostUnreachable;
                    5: ErrCode := socksConnectionRefused;
                    6: ErrCode := socksTtlExpired;
                    7: ErrCode := socksUnknownCommand;
                    8: ErrCode := socksUnknownAddressType;
                    else
                       ErrCode := socksUnassignedError;
                    end;
                    case ErrCode of
                    socksGeneralFailure :
                        ErrMsg := 'general SOCKS server failure';
                    socksConnectionNotAllowed :
                        ErrMsg := 'connection not allowed by ruleset';
                    socksNetworkUnreachable :
                        ErrMsg := 'network unreachable';
                    socksHostUnreachable :
                        ErrMsg := 'host unreachable';
                    socksConnectionRefused :
                        ErrMsg := 'connection refused';
                    socksTtlExpired :
                        ErrMsg := 'time to live expired';
                    socksUnknownCommand :
                        ErrMsg := 'command not supported';
                    socksUnknownAddressType :
                        ErrMsg := 'address type not supported';
                    else
                        ErrMsg := 'unassigned error #' + IntToStr(Ord(FRcvBuf[1]));
                    end;
                    DataAvailableError(ErrCode, ErrMsg);
                    Exit;
                end;
            end;
            if FRcvCnt < 5 then
                Exit;

            { We have enough data to learn the answer length }
            if FRcvBuf[3] = $01 then
                AnsLen := 10                     { IP V4 address }
            else if FRcvBuf[3] = $03 then
                AnsLen := 7 + Ord(FRcvBuf[4])    { Domain name   }
            else
                AnsLen := 5;                     { Other unsupported }

            if FRcvCnt < AnsLen then
                Exit;

            if FRcvBuf[3] = $01 then begin
                { IP V4 address }
                //Move(FRcvBuf[4], InAddr, 4);
                InAddr.S_addr := FRcvBuf[4] or
                                 (FRcvBuf[5] shl 8) or
                                 (FRcvBuf[6] shl 16) or
                                 (FRcvBuf[7] shl 24);
                FBoundAddr := WSocket_Synchronized_inet_ntoa(InAddr);
                I := 4 + 4;
            end
            else if FRcvBuf[3] = $03 then begin
                { Domain name }
                SetLength(FBoundAddr, Ord(FRcvBuf[4]));
                {$IFDEF CLR}
                for J := 1 to Ord(FRcvBuf[4]) do
                    FBoundAddr[J] := Char(FRcvBuf[4 + J]);
                {$ENDIF}
                {$IFDEF WIN32}
                Move(FRcvBuf[5], FBoundAddr[1], Length(FBoundAddr)); { david.brock }
                {$ENDIF}
                I := 4 + Ord(FRcvBuf[4]) + 1;
            end
            else begin
                { Unsupported address type }
                DataAvailableError(socksUnknownAddressType, 'address type not supported');
                Exit;
            end;

            FBoundPort  := IntToStr(WSocket_Synchronized_ntohs(
                                        FRcvBuf[I] or (FRcvBuf[I + 1] shl 8)));
            I           := I + 2;
            FSocksState := socksData;
{           inherited TriggerSessionConnectedSpecial(0); }
{ if IsConsole then WriteLn('SOCKS5 NEGOCIATION OK');}
            {inherited} TriggerSessionConnectedSpecial(0);
            {**ALON** removed 'inherited' now calls top level}
            FSocksRcvdCnt := FRcvCnt - I;
            if FSocksRcvdCnt < 0 then
                FSocksRcvdCnt := 0
            else
                FSocksRcvdPtr := I; //@FRcvBuf[I];
{           Result := inherited TriggerDataAvailable(0);}
            Result := {inherited} TriggerDataAvailable(0);
            {**ALON** removed 'inherited' now calls top level}
        end;
    end
    else if FSocksState = socksAuthenticate then begin
        Result := TRUE;
        {$IFDEF CLR}
        Len := Receive(Buf, Length(Buf) - FRcvCnt - 1);
        if Len < 0 then
            Exit;
        for I := 0 to Len - 1 do begin
            FRcvBuf[FRcvCnt] := Buf[I];
            Inc(FRcvCnt);
        end;
        {$ENDIF}
        {$IFDEF WIN32}
        Len := Receive(@FRcvBuf[FRcvCnt], Sizeof(FRcvBuf) - FRcvCnt - 1);
        if Len < 0 then
            Exit;
        FRcvCnt := FRcvCnt + Len;
        {$ENDIF}
{TriggerDisplay('socksAuthenticate FrcvBuf = ''' + BufToStr(FRcvBuf, FRcvCnt) + '''');}
        if FRcvCnt >= 1 then begin
            { First byte is version, we expect version 5 }
            if FRcvBuf[0] <> $01 then begin { 06/03/99 }
{                TriggerSocksAuthState(socksAuthFailure); Burlakov 12/11/99 }
                DataAvailableError(socksVersionError, 'socks version error');
                Exit;
            end;
        end;
        if FRcvCnt = 2 then begin
            { Second byte is status }
            if FRcvBuf[1] <> $00 then begin
{                TriggerSocksAuthState(socksAuthFailure); Burlakov 12/11/99 }
                DataAvailableError(socksAuthenticationFailed, 'socks authentication failed');
                Exit;
            end;
        end
        else if FRcvCnt > 2 then begin
{            TriggerSocksAuthState(socksAuthFailure); Burlakov 12/11/99 }
            DataAvailableError(socksProtocolError, 'too much data availaible');
            Exit;
        end;
        FRcvCnt := 0; { 06/03/99 }
        TriggerSocksAuthState(socksAuthSuccess);
        SocksDoConnect;
    end
    else begin
        { We should never comes here ! }
        DataAvailableError(socksInternalError, 'internal error');
        Result := FALSE;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomSocksWSocket.GetRcvdCount : LongInt;
begin
    if FSocksRcvdCnt <= 0 then
        Result := inherited GetRcvdCount
    else
        Result := FSocksRcvdCnt;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomSocksWSocket.DoRecv(
    var Buffer : TWSocketData;
    BufferSize : Integer;
    Flags      : Integer) : Integer;
{$IFDEF CLR}
var
    I : Integer;
{$ENDIF}
begin
    if FSocksRcvdCnt <= 0 then begin
        Result := inherited DoRecv(Buffer, BufferSize, Flags);
        Exit;
    end;
    { We already have received data into our internal buffer }
    if FSocksRcvdCnt <= BufferSize then begin
        { User buffer is greater than received data, copy all and clear }
        {$IFDEF CLR}
        for I := 0 to FSocksRcvdCnt - 1 do
            Buffer[I] := FRcvBuf[FSocksRcvdPtr + I];
        {$ENDIF}
        {$IFDEF WIN32}
        Move(FRcvBuf[FSocksRcvdPtr], Buffer, FSocksRcvdCnt);
        {$ENDIF}
        Result        := FSocksRcvdCnt;
        FSocksRcvdCnt := 0;
        Exit;
    end;
    { User buffer is smaller, copy as much as possible }
    {$IFDEF CLR}
    for I := 0 to BufferSize - 1 do
        Buffer[I] := FRcvBuf[FSocksRcvdPtr + I];
    {$ENDIF}
    {$IFDEF WIN32}
    Move(FRcvBuf[FSocksRcvdPtr], Buffer, BufferSize);
    {$ENDIF}
    Result        := BufferSize;
    FSocksRcvdPtr := FSocksRcvdPtr + BufferSize;
    FSocksRcvdCnt := FSocksRcvdCnt - BufferSize;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

              X          X     X       X      X X X X
              X          X     X X     X      X
              X          X     X   X   X      X
              X          X     X     X X      X X X
              X          X     X       X      X
              X          X     X       X      X
              X X X X    X     X       X      X X X X

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TCustomLineWSocket.Create{$IFDEF VCL}(AOwner: TComponent){$ENDIF};
begin
    inherited Create{$IFDEF VCL}(AOwner){$ENDIF};
    FLineEnd   := #13#10;
    FLineMode  := FALSE;
    FLineLimit := 65536;  { Arbitrary line limit }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TCustomLineWSocket.Destroy;
begin
    if FRcvdPtr <> nil then begin
        {$IFDEF CLR}
        SetLength(FRcvdPtr, 0);
        {$ENDIF}
        {$IFDEF WIN32}
        FreeMem(FRcvdPtr, FRcvBufSize);
        FRcvdPtr     := nil;
        {$ENDIF}
        FRcvBufSize := 0;
    end;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomLineWSocket.WndProc(var MsgRec: TMessage);
begin
    with MsgRec do begin
        if Msg = FMsg_WM_TRIGGER_DATA_AVAILABLE then begin
            { We *MUST* handle all exception to avoid application shutdown }
            try
                WMTriggerDataAvailable(MsgRec)
            except
                on E:Exception do
                    HandleBackGroundException(E);
            end;
        end
        else
            inherited WndProc(MsgRec);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomLineWSocket.WMTriggerDataAvailable(var msg: TMessage);
var
    Count : Integer;
begin
{$IFDEF OLD_20040117}
    while FRcvdCnt > 0 do
        TriggerDataAvailable(0);
{$ELSE}
    Count := 0;
    while FRcvdCnt > 0 do begin
        Inc(Count);
        FLineFound := FALSE;
        TriggerDataAvailable(0);
        if (FRcvdCnt <= 0) or
           (FLineMode and (Count > 3) and (not FLineFound)) then
            Break;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomLineWSocket.SetLineMode(newValue : Boolean);
begin
    if FLineMode = newValue then
        Exit;
    FLineMode := newValue;
    if (FRcvdCnt > 0) or (FLineLength > 0) then
        PostMessage(Handle, FMsg_WM_TRIGGER_DATA_AVAILABLE, 0, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Return -1 if error, else return number of byte written                    }
function TCustomLineWSocket.SendLine(const Str : String) : Integer;
begin
    Result := Length(Str);
    if Result > 0 then begin
        PutStringInSendBuffer(Str);
        SendStr(LineEnd);
        Inc(Result, Length(LineEnd));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomLineWSocket.GetRcvdCount : LongInt;
begin
    if not FLineMode then
        Result := inherited GetRcvdCount
    else
        Result := FLineLength;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomLineWSocket.DoRecv(
    var Buffer : TWSocketData;
    BufferSize : Integer;
    Flags      : Integer) : Integer;
{$IFDEF CLR}
var
    I : Integer;
{$ENDIF}
begin
    if FLineMode and (FLineLength > 0) then begin
        { We are in line mode an a line is received }
        if FLineLength <= BufferSize then begin
            { User buffer is greater than received data, copy all and clear }
            {$IFDEF CLR}
            System.Buffer.BlockCopy(FRcvdPtr, 0, Buffer, 0, FLineLength);
            //for I := 0 to FLineLength - 1 do
            //    Buffer[I] := FRcvdPtr[I];
            {$ENDIF}
            {$IFDEF WIN32}
            Move(FRcvdPtr^, Buffer^, FLineLength);
            {$ENDIF}
            Result      := FLineLength;
            FLineLength := 0;
            Exit;
        end;
        { User buffer is smaller, copy as much as possible }
        {$IFDEF CLR}
        for I := 0 to BufferSize - 1 do
            Buffer[I] := FRcvdPtr[I];
        { Move the end of line to beginning of buffer to be read the next time }
        for I := 0 to FLineLength - BufferSize - 1 do
            FRcvdPtr[I] := FRcvdPtr[BufferSize + I];
        {$ENDIF}
        {$IFDEF WIN32}
        Move(FRcvdPtr^, Buffer^, BufferSize);
        { Move the end of line to beginning of buffer to be read the next time }
        Move(PAnsiChar(FRcvdPtr)[BufferSize], FRcvdPtr^, FLineLength - BufferSize);
        {$ENDIF}
        Result      := BufferSize;
        FLineLength := FLineLength - BufferSize;
        Exit;
    end;

    if FLineMode or (FRcvdCnt <= 0) then begin
        { There is nothing in our internal buffer }
        Result := inherited DoRecv(Buffer, BufferSize, Flags);
        Exit;
    end;

    { We already have received data into our internal buffer }
    if FRcvdCnt <= BufferSize then begin
        { User buffer is greater than received data, copy all and clear }
        {$IFDEF CLR}
        for I := 0 to FRcvdCnt - 1 do
            Buffer[I] := FRcvdPtr[I];
        {$ENDIF}
        {$IFDEF WIN32}
        Move(FRcvdPtr^, Buffer^, FRcvdCnt);
        {$ENDIF}
        Result   := FRcvdCnt;
        FRcvdCnt := 0;
        Exit;
    end;
    {$IFDEF CLR}
    { User buffer is smaller, copy as much as possible }
    for I := 0 to BufferSize - 1 do
        Buffer[I] := FRcvdPtr[I];
    { Then move remaining data to front of buffer  16/10/99 }
    for I := 0 to FRcvdCnt - BufferSize do
        FRcvdPtr[I] := FRcvdPtr[BufferSize + I];
    {$ENDIF}
    {$IFDEF WIN32}
    { User buffer is smaller, copy as much as possible }
    Move(FRcvdPtr^, Buffer^, BufferSize);
    { Then move remaining data to front og buffer  16/10/99 }
    Move(PAnsiChar(FRcvdPtr)[BufferSize], FRcvdPtr^, FRcvdCnt - BufferSize + 1);  
    {$ENDIF}
    Result   := BufferSize;
    FRcvdCnt := FRcvdCnt - BufferSize;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Edit received data. Handle TAB and BACKSPACE characters.                  }
{ A data packet has been received into FRcvPtr buffer, starting from        }
{ FRcvdCnt offset. Packet size if passed as the Len argument.               }
procedure TCustomLineWSocket.EditLine(var Len : Integer);
{$IFDEF CLR}
var
    Buf     : TBytes;
    BufSize : LongInt;
    I       : LongInt;
    J       : LongInt;
    K       : Integer;
    Edited  : Boolean;
    NewCnt  : LongInt;
    NewSize : LongInt;
const
    BackString : String = #8 + ' ' + #8;
begin
    BufSize := 0;
    try
        Edited := FALSE;
        I      := FRcvdCnt;
        J      := FRcvdCnt;
        NewCnt := FRcvdCnt;
        { Loop to process all received char }
        while I < (FRcvdCnt + Len) do begin
            if FRcvdPtr[I] = 8 then begin   { BACKSPACE character }
                if FLineEcho and (J > 0) then
                    SendStr(BackString);
                if not Edited then begin
                    { Not edited yet, so we allocate a buffer to store }
                    { edited data and we remember we edited data.      }
                    Edited := TRUE;
                    { Compute buffer size as a multiple of 256 bytes   }
                    BufSize := ((FRcvdCnt + Len + 256) shr 8) shl 8;
                    SetLength(Buf, BufSize);
                    { Copy data already processed }
                    for K := 0 to I - 1 do
                        Buf[K] := FRcvdPtr[K];
                end;
                if J > 0 then begin
                    Dec(J);
                    if J < NewCnt then
                        NewCnt := J;
                end;
                Inc(I);
            end
            else if FRcvdPtr[I] = 9 then begin  { TAB character }
                if not Edited then begin
                    { Not edited yet, so we allocate a buffer to store }
                    { edited data and we remember we edited data.      }
                    Edited := TRUE;
                    { Compute buffer size as a multiple of 256 bytes   }
                    BufSize := ((FRcvdCnt + Len + 256) shr 8) shl 8;
                    SetLength(Buf, BufSize);
                    { Copy data already processed }
                    for K := 0 to I - 1 do
                        Buf[K] := FRcvdPtr[K];
                end;
                repeat
                    if FLineEcho then
                        SendStr(' ');
                    Buf[J] := Ord(' ');
                    Inc(J);
                until (J and 7) = 0;
                Inc(I);
            end
            else begin
                if FLineEcho then
                    Send(FRcvdPtr[I]);
                if Edited then begin
                    if J >= BufSize then begin
                        { Need to allocate more buffer space }
                        NewSize := BufSize + 256;
                        SetLength(Buf, NewSize);
                        BufSize := NewSize;
                    end;
                    Buf[J] := FRcvdPtr[I];
                end;
                Inc(I);
                Inc(J);
            end;
        end;
        if Edited then begin
            if J >= FRcvBufSize then begin
                { Current buffer is too small, allocate larger }
                NewSize := J + 1;
                SetLength(FRcvdPtr, NewSize);
                FRcvBufSize := NewSize;
            end;
            { Move edited data back to original buffer }
            for K := 0 to J - 1 do
                FRcvdPtr[K] := Buf[K];
            FRcvdPtr[J] := 0;
            FRcvdCnt    := NewCnt;
            Len         := J - FRcvdCnt;
        end;
    finally
        if BufSize > 0 then
            SetLength(Buf, BufSize);
    end;
end;
{$ENDIF}
{$IFDEF WIN32}
var
    Buf     : PChar;
    BufSize : LongInt;
    I       : LongInt;
    J       : LongInt;
    Edited  : Boolean;
    NewCnt  : LongInt;
    NewSize : LongInt;
const
    BackString : String = #8 + ' ' + #8;
begin
    BufSize := 0;
    try
        Edited := FALSE;
        I      := FRcvdCnt;
        J      := FRcvdCnt;
        NewCnt := FRcvdCnt;
        { Loop to process all received char }
        while I < (FRcvdCnt + Len) do begin
            if PChar(FRcvdPtr)[I] = #8 then begin   { BACKSPACE character }
                if FLineEcho and (J > 0) then
                    SendStr(BackString);
                if not Edited then begin
                    { Not edited yet, so we allocate a buffer to store }
                    { edited data and we remember we edited data.      }
                    Edited := TRUE;
                    { Computer buffer size as a multiple of 256 bytes  }
                    BufSize := ((FRcvdCnt + Len + 256) shr 8) shl 8;
                    GetMem(Buf, BufSize);
                    { Copy data already processed }
                    Move(FRcvdPtr^, Buf^, I);
                end;
                if J > 0 then begin
                    Dec(J);
                    if J < NewCnt then
                        NewCnt := J;
                end;
                Inc(I);
            end
            else if PChar(FRcvdPtr)[I] = #9 then begin  { TAB character }
                if not Edited then begin
                    { Not edited yet, so we allocate a buffer to store }
                    { edited data and we remember we edited data.      }
                    Edited := TRUE;
                    { Computer buffer size as a multiple of 256 bytes  }
                    BufSize := ((FRcvdCnt + Len + 256) shr 8) shl 8;
                    GetMem(Buf, BufSize);
                    { Copy data already processed }
                    Move(FRcvdPtr^, Buf^, I);
                end;
                repeat
                    if FLineEcho then
                        SendStr(' ');
                    Buf[J] := ' ';
                    Inc(J);
                until (J and 7) = 0;
                Inc(I);
            end
            else begin
                if FLineEcho then
                    Send(@PChar(FRcvdPtr)[I], 1);
                if Edited then begin
                    if J >= BufSize then begin
                        { Need to allocate more buffer space }
                        NewSize := BufSize + 256;
                        {$IFDEF DELPHI1}
                        Buf := ReallocMem(Buf, BufSize, NewSize);
                        {$ELSE}
                        ReallocMem(Buf, NewSize);
                        {$ENDIF}
                        BufSize := NewSize;
                    end;
                    Buf[J] := PChar(FRcvdPtr)[I];
                end;
                Inc(I);
                Inc(J);
            end;
        end;
        if Edited then begin
            if J >= FRcvBufSize then begin
                { Current buffer is too small, allocate larger }
                NewSize := J + 1;
                {$IFDEF DELPHI1}
                FRcvdPtr := ReallocMem(FRcvdPtr, FRcvBufSize, NewSize);
                {$ELSE}
                ReallocMem(FRcvdPtr, NewSize);
                {$ENDIF}
                FRcvBufSize := NewSize;
            end;
            { Move edited data back to original buffer }
            Move(Buf^, FRcvdPtr^, J);
            PChar(FRcvdPtr)[J] := #0;
            FRcvdCnt := NewCnt;
            Len      := J - FRcvdCnt;
        end;
    finally
        if BufSize > 0 then
            FreeMem(Buf, BufSize);
    end;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomLineWSocket.TriggerLineLimitExceeded(
    Cnt           : Integer;
    var ClearData : Boolean);
begin
    if Assigned(FOnLineLimitExceeded) then
        FOnLineLimitExceeded(Self, Cnt, ClearData);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomLineWSocket.TriggerDataAvailable(ErrCode : Word) : Boolean;
{$IFDEF CLR}
var
    Cnt        : Integer;
    Len        : Integer;
    NewSize    : LongInt;
    SearchFrom : LongInt;
    I, K       : LongInt;
    Found      : Boolean;
begin
{  if (not FLineMode) or (Length(FLineEnd) = 0) then begin }
    if (not FLineMode) or (Length(FLineEnd) = 0) or (FSocksState <> socksData)
    {**ALON** added check so, if data is received while still handshaking }
    { with the socks server, we ask the TCustomSocksWSocket to handle it  }
    then begin
        { We are not in line mode }
        Result := inherited TriggerDataAvailable(ErrCode);
        Exit;
    end;

    { We are in line mode. We receive data ourself }

    Result := TRUE;
    Cnt    := inherited GetRcvdCount;
    { if Cnt <= 0 then }
    {    Exit;         }
    if Cnt < 0 then
        Exit;
    if Cnt = 0 then
        Cnt := 255;

    if (FRcvdCnt + Cnt + 1) > FRcvBufSize then begin
        { Current buffer is too small, allocate larger }
        NewSize := FRcvdCnt + Cnt + 1;
        SetLength(FRcvdPtr, NewSize);
        FRcvBufSize := NewSize;
    end;

    if Length(FLocalBuf) < Cnt then
        SetLength(FLocalBuf, ((Cnt + 256) shr 8) shl 8);

    Len := Receive(FLocalBuf, Cnt);
    if Len <= 0 then
        Exit;
    for I := 0 to Len - 1 do
         FRcvdPtr[FRcvdCnt + I] := FLocalBuf[I];
    FRcvdPtr[FRcvdCnt + Len] := 0;
    if FLineEdit then
        EditLine(Len)
    else if FLineEcho then
        Send(FLocalBuf, Len);
    SearchFrom := FRcvdCnt - Length(FLineEnd);
    if SearchFrom < 0 then
        SearchFrom := 0;
    FRcvdCnt := FRcvdCnt + Len;
    while FLineMode do begin
        Found := FALSE;
        I := SearchFrom;
        while I < (FRcvdCnt - Length(FLineEnd) + 1) do begin
            if FRcvdPtr[I] = Ord(FLineEnd[1]) then begin
                Found := TRUE;
                for K := 2 to Length(FLineEnd) do begin
                    Found := (FRcvdPtr[I + K - 1] = Ord(FLineEnd[K]));
                    if not Found then
                        break;
                end;
                if Found then
                    break;    { Found the end of line marker }
            end;
            Inc(I);
        end;
        if not Found then begin
            if ((FLineLimit > 0) and (FRcvdCnt > FLineLimit)) then begin
                FLineClearData := TRUE;
                TriggerLineLimitExceeded(FRcvdCnt, FLineClearData);
                if FLineClearData then begin
                    FLineLength        := 0;
                    FRcvdCnt           := 0;
                    FLineClearData     := FALSE;
                end;
            end;
            break;
        end;
        FLineLength       := I + Length(FLineEnd);
        FLineReceivedFlag := TRUE;
        FLineFound        := TRUE;
        { We received a complete line. We need to signal it to application }
        { The application may not have a large buffer so we may need       }
        { several events to read the entire line. In the meanwhile, the    }
        { application may turn line mode off.                              }
        while FLineMode and (FLineLength > 0) do begin
            if not inherited TriggerDataAvailable(0) then
                { There is no handler installed }
                FLineLength := 0;
        end;
        { Move remaining data in front of buffer }
        if FLineLength > 0 then begin
            { Line mode was turned off in the middle of a line read. }
            { We preserve unread line and other received data.       }
            for K := 0 to FRcvdCnt - I - 1 do
                FRcvdPtr[FLineLength + K] := FRcvdPtr[I + K];
            FRcvdCnt := FRcvdCnt - I + FLineLength;
        end
        else begin
            for K := 0 to FRcvdCnt - I - Length(FLineEnd) - 1 do
                 FRcvdPtr[K] := FRcvdPtr[I + Length(FLineEnd) + K];
            FRcvdCnt := FRcvdCnt - I - Length(FLineEnd);
        end;
        if FRcvdCnt >= 0 then
            FRcvdPtr[FRcvdCnt] := 0;
        SearchFrom := 0;
        { It is possible the user has turned line mode to off. If data is }
        { still available in the receive buffer, we will deliver it.      }
        while (not FLineMode) and (FRcvdCnt > 0) do            { 26/01/04 }
            inherited TriggerDataAvailable(0);                 { 26/01/04 }
    end;
end;
{$ENDIF}
{$IFDEF WIN32}
var
    Cnt        : Integer;
    Len        : Integer;
    NewSize    : LongInt;
    SearchFrom : LongInt;
    I          : LongInt;
    Found      : Boolean;
begin
{  if (not FLineMode) or (Length(FLineEnd) = 0) then begin }
    if (not FLineMode) or (Length(FLineEnd) = 0) or (FSocksState <> socksData)
    {**ALON** added check so, if data is received while still handshaking }
    { with the socks server, we ask the TCustomSocksWSocket to handle it  }
    then begin
        { We are not in line mode }
        Result := inherited TriggerDataAvailable(ErrCode);
        Exit;
    end;

    { We are in line mode. We receive data ourself }

    Result := TRUE;
    Cnt    := inherited GetRcvdCount;
    { if Cnt <= 0 then }
    {    Exit;         }
    if Cnt < 0 then
        Exit;
    if Cnt = 0 then
        Cnt := 255;

    if (FRcvdCnt + Cnt + 1) > FRcvBufSize then begin
        { Current buffer is too small, allocate larger }
        NewSize := FRcvdCnt + Cnt + 1;
        {$IFDEF DELPHI1}
        FRcvdPtr := ReallocMem(FRcvdPtr, FRcvBufSize, NewSize);
        {$ELSE}
        ReallocMem(FRcvdPtr, NewSize);
        {$ENDIF}
        FRcvBufSize := NewSize;
    end;

    Len := Receive(IncPtr(FRcvdPtr, FRcvdCnt), Cnt);
{$IFDEF OLD_20040117}
    if Len <= 0 then
        Exit;
    FRcvdPtr[FRcvdCnt + Len] := #0;
{$ELSE}
    if Len <= 0 then begin
        if FRcvdCnt <= 0 then
            Exit;
        Len := 0;
    end;
{$ENDIF}

    if Len > 0 then begin
        if FLineEdit then
            EditLine(Len)
        else if FLineEcho then
            Send(IncPtr(FRcvdPtr, FRcvdCnt), Len);
    end;

    SearchFrom := FRcvdCnt - Length(FLineEnd);
    if SearchFrom < 0 then
        SearchFrom := 0;
    FRcvdCnt := FRcvdCnt + Len;
    while FLineMode do begin
        Found := FALSE;
        I := SearchFrom;
        while I < (FRcvdCnt - Length(FLineEnd) + 1) do begin
            if PAnsiChar(FRcvdPtr)[I] = FLineEnd[1] then begin     
                Found := (StrLComp(@(PAnsiChar(FRcvdPtr)[I]), @FLineEnd[1], Length(FLineEnd)) = 0);
                if Found then
                    break;    { Found the end of line marker }
            end;
            Inc(I);
        end;
        if not Found then begin
            if ((FLineLimit > 0) and (FRcvdCnt > FLineLimit)) then begin
                FLineClearData := TRUE;
                TriggerLineLimitExceeded(FRcvdCnt, FLineClearData);
                if FLineClearData then begin
                    FLineLength        := 0;
                    FRcvdCnt           := 0;
                    FLineClearData     := FALSE;
                end;
            end;
            break;
        end;
        FLineLength       := I + Length(FLineEnd);
        FLineReceivedFlag := TRUE;
        FLineFound        := TRUE;
        { We received a complete line. We need to signal it to application }
        { The application may not have a large buffer so we may need       }
        { several events to read the entire line. In the meanwhile, the    }
        { application may turn line mode off.                              }
        while FLineMode and (FLineLength > 0) do begin
            if not inherited TriggerDataAvailable(0) then
                { There is no handler installed }
                FLineLength := 0;
        end;
        { Move remaining data in front of buffer }
        if FLineLength > 0 then begin
            { Line mode was turned off in the middle of a line read. }
            { We preserve unread line and other received data.       }
            Move(PAnsiChar(FRcvdPtr)[I], PChar(FRcvdPtr)[FLineLength],  
                 FRcvdCnt - I);
            FRcvdCnt := FRcvdCnt - I + FLineLength;
        end
        else begin
            Move(PAnsiChar(FRcvdPtr)[I + Length(FLineEnd)], PAnsiChar(FRcvdPtr)[0],
                 FRcvdCnt - I - Length(FLineEnd));
            FRcvdCnt := FRcvdCnt - I - Length(FLineEnd);
        end;
        if FRcvdCnt >= 0 then
            PAnsiChar(FRcvdPtr)[FRcvdCnt] := #0;
        SearchFrom := 0;
        { It is possible the user has turned line mode to off. If data is }
        { still available in the receive buffer, we will deliver it.      }
        while (not FLineMode) and (FRcvdCnt > 0) do            { 26/01/04 }
            inherited TriggerDataAvailable(0);                 { 26/01/04 }
    end;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomLineWSocket.TriggerSessionClosed(Error : Word);
begin
    FLineReceivedFlag := TRUE;
    if FRcvdPtr <> nil then begin
        if FLineMode and (FRcvdCnt > 0) and (not FLineClearData) then begin
            FLineLength       := FRcvdCnt;
            while FLineMode and (FLineLength > 0) do
                inherited TriggerDataAvailable(0);
        end;
        {$IFDEF CLR}
        SetLength(FRcvdPtr, FRcvBufSize);
        {$ENDIF}
        {$IFDEF WIN32}
        FreeMem(FRcvdPtr, FRcvBufSize);
        FRcvdPtr    := nil;
        {$ENDIF}
        FRcvBufSize := 0;
        FRcvdCnt    := 0;
    end;
    inherited TriggerSessionClosed(Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

                 X X      X     X    X       X     X X X
               X     X      X   X    X X     X   X      X
               X              X X    X   X   X   X
                 X X            X    X     X X   X
                     X          X    X       X   X
               X     X    X     X    X       X   X      X
                 X X        X X      X       X     X X X

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSyncWSocket.InternalDataAvailable(
    Sender : TObject;
    Error  : Word);
var
    Len : Integer;
begin
    {$IFDEF CLR}
    SetLength(FLinePointer, FLineLength);
    Len := Receive(FLinePointer, FLineLength);
    if Len <= 0 then
        Len := 0;
    SetLength(FLinePointer, Len);
    {$ENDIF}
    {$IFDEF WIN32}
    SetLength(FLinePointer^, FLineLength);
    Len := Receive(@FLinePointer^[1], FLineLength);
    if Len <= 0 then
        FLinePointer^ := ''
    else
        SetLength(FLinePointer^, Len);
    {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomSyncWSocket.WaitUntilReady(var DoneFlag : Boolean) : Integer;
begin
    Result := 0;           { Suppose success }
    FTimeStop := Integer(GetTickCount) + FTimeout;
    while TRUE do begin
        if DoneFlag then begin
            Result := 0;
            break;
        end;

        if ((FTimeout > 0) and (Integer(GetTickCount) > FTimeStop)) or
{$IFDEF WIN32}
{$IFNDEF NOFORMS}
           Application.Terminated or
{$ENDIF}
{$ENDIF}
           FTerminated then begin
            { Application is terminated or timeout occured }
            Result := WSA_WSOCKET_TIMEOUT;
            break;
        end;
        MessagePump;
{$IFDEF COMPILER2_UP}
        { Do not use 100% CPU, but slow down transfert on high speed LAN }
        Sleep(0);
{$ENDIF}
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ DEPRECATED: DO NOT USE Synchronize procedure for a new application.       }
{ Instead, use pure event-driven design.                                    }
function TCustomSyncWSocket.Synchronize(
    Proc : TWSocketSyncNextProc;
    var DoneFlag : Boolean) : Integer;
begin
    DoneFlag := FALSE;
    if Assigned(Proc) then
        Proc;
    Result := WaitUntilReady(DoneFlag);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ DEPRECATED: DO NOT USE ReadLine procedure for a new application.          }
{ Instead, use pure event-driven design using OnDataAvailable event.        }
procedure TCustomSyncWSocket.ReadLine(
    Timeout    : Integer;  { seconds if positive, milli-seconds if negative }
    var Buffer : String);
var
    OldDataAvailable : TDataAvailable;
    OldLineMode      : Boolean;
    Status           : Integer;
{$IFDEF CLR}
    I                : Integer;
{$ENDIF}
begin
    Buffer            := '';
    if FState <> wsConnected then begin
        RaiseException('ReadLine failed: not connected');
        Exit;
    end;

    { Positive timeout means seconds. Negative means milli-seconds }
    { Null means 60 seconds.                                       }
    if TimeOut = 0 then
        FTimeOut      := 60000
    else if TimeOut > 0 then
        FTimeOut      := Timeout * 1000
    else
        FTimeOut      := -Timeout;

    FLineReceivedFlag := FALSE;
    {$IFDEF WIN32}
    FLinePointer      := @Buffer;
    {$ENDIF}
    { Save existing OnDataAvailable handler and install our own }
    OldDataAvailable  := FOnDataAvailable;
    FOnDataAvailable  := InternalDataAvailable;
    { Save existing line mode and turn it on }
    OldLineMode       := FLineMode;
    FLineMode         := TRUE;
    try
        Status := Synchronize(nil, FLineReceivedFlag);
        if Status = WSA_WSOCKET_TIMEOUT then begin
             { Sender didn't send line end within allowed time. Get all }
             { data available so far.                                   }
             if FRcvdCnt > 0 then begin
                 SetLength(Buffer, FRcvdCnt);
                 {$IFDEF CLR}
                 for I := 0 to FRcvdCnt - 1 do
                     Buffer[I + 1] := Char(FRcvdPtr[I]);
                 {$ENDIF}
                 {$IFDEF WIN32}
                 Move(FRcvdPtr^, Buffer[1], FRcvdCnt);
                 {$ENDIF}
                 FRcvdCnt := 0;
             {$IFDEF CLR}
             end
             else begin
                 SetLength(Buffer, Length(FLinePointer));
                 for I := 0 to Length(FLinePointer) - 1 do
                     Buffer[I + 1] := Char(FLinePointer[I]);
             {$ENDIF}
             end;
        end;
        { Should I raise an exception to tell the application that       }
        { some error occured ?                                           }
    finally
        FOnDataAvailable := OldDataAvailable;
        FLineMode        := OldLineMode;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ You must define USE_SSL so that SSL code is included in the component.    }
{ To be able to compile the component, you must have the SSL related files  }
{ which are _NOT_ freeware. See http://www.overbyte.be for details.         }
{$IFDEF USE_SSL}
{$I OverbyteIcsWSocketImplSsl.inc}
{$ENDIF}

{$IFDEF DELPHI1}
begin
    IPList := TStringList.Create;
    {
      Delphi 1 has no finalization. When your application terminates, you
      should add a call to WSocketUnloadWinsock to unload winsock from memory.
      It is done automatically for you when the last TWSocket component is
      destroyed but if you do any winsock call after that, you must call
      WSocketUnloadWinsock yourself. It is safe to call WSocketUnloadWinsock
      even if it has already been done.
    }
{$ELSE}
initialization
    IPList         := TStringList.Create;
    InitializeCriticalSection(GClassCritSect);
    InitializeCriticalSection(GWSockCritSect);
{$IFDEF USE_SSL}
    InitializeCriticalSection(SslCritSect);
    {$IFNDEF NO_ADV_MT}
        InitializeCriticalSection(LockPwdCB);
        InitializeCriticalSection(LockVerifyCB);
        InitializeCriticalSection(LockInfoCB);
        InitializeCriticalSection(LockRemSessCB);
        InitializeCriticalSection(LockNewSessCB);
        InitializeCriticalSection(LockGetSessCB);
        InitializeCriticalSection(LockClientCertCB);
    {$ENDIF}
{$ENDIF}

finalization
    if Assigned(IPList) then begin
        IPList.Free;
        IPList := nil;
    end;
    if WSocketGCount <= 0 then begin    { FP 15/09/03 }
        DeleteCriticalSection(GClassCritSect);
        WSocketUnloadWinsock;
        DeleteCriticalSection(GWSockCritSect);
    end;
{$IFDEF USE_SSL}
    {$IFNDEF NO_ADV_MT}
        DeleteCriticalSection(LockPwdCB);
        DeleteCriticalSection(LockVerifyCB);
        DeleteCriticalSection(LockInfoCB);
        DeleteCriticalSection(LockRemSessCB);
        DeleteCriticalSection(LockNewSessCB);
        DeleteCriticalSection(LockGetSessCB);
        DeleteCriticalSection(LockClientCertCB);
    {$ENDIF}
    DeleteCriticalSection(SslCritSect);
{$ENDIF}

{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.


