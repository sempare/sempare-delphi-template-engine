unit ServerConst1;

interface

resourcestring
  sPortInUse = '- Error: Port %s already in use';
  sPortSet = '- Port set to %s';
  sServerRunning = '- The Server is already running';
  sStartingServer = '- Starting HTTP Server on port %d';
  sStoppingServer = '- Stopping Server';
  sServerStopped = '- Server Stopped';
  sServerNotRunning = '- The Server is not running';
  sInvalidCommand = '- Error: Invalid Command';
  sIndyVersion = '- Indy Version: ';
  sActive = '- Active: ';
  sPort = '- Port: ';
  sSessionID = '- Session ID CookieName: ';
  sCommands = 'Enter a Command: ' + slineBreak +
    '   - "start" to start the server'+ slineBreak +
    '   - "stop" to stop the server'+ slineBreak +
    '   - "set port" to change the default port'+ slineBreak +
    '   - "status" for Server status'+ slineBreak +
    '   - "help" to show commands'+ slineBreak +
    '   - "exit" to close the application';
  sConnect = 'Connect your browser to http://localhost:%d';
  sWelcome = 'This is the Sempare Template Engine WebBroker Demo';
  sVersion = 'This is using the Sempare Template Engine v%s';

const
  cArrow = '->';
  cCommandStart = 'start';
  cCommandStop = 'stop';
  cCommandStatus = 'status';
  cCommandHelp = 'help';
  cCommandSetPort = 'set port';
  cCommandExit = 'exit';

implementation

end.
