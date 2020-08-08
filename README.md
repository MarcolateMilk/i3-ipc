# i3-ipc

Haskell library for communicating with i3 via IPC. The main focus here is ease of use and a clear, well-formed structure.

## Usage

```haskell
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

import Network.I3IPC

main :: IO ()
main = do

  -- Connect to i3's IPC socket
  con <- connection

  -- A few example messages
  commands   <- runCommand con "reload"
  workspaces <- getWorkspaces con
  version    <- getVersion con

  print commands
  print workspaces
  print version

  -- Example event listening
  listen [ ET'Window, ET'Mode, ET'Binding ] $ \ case
    WindowEvent  info -> print $ ie'container info
    ModeEvent    info -> print $ me'change info
    BindingEvent info -> print $ be'change info
```
