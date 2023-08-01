# xmonad-log

xmonad-log-multiscreen is a DBus monitoring solution that can easily be used to display
xmonad in a statusbar like [polybar](https://github.com/jaagr/polybar),
[lemonbar](https://github.com/LemonBoy/bar) and similar.

This fork allows you to add a custom ID (by passing it as the first argument) that allows you to use marshallPP and screen independent pretty printing. This allows you to have a bar on each screen and will show the correct focus on each screen.

There is an example configuration provided below which will show you how to use the screen index as the log id. The log id supports any string that supports characters of a valid path.

## Installation

xmonad-log is written in Go with one dependency:
[dbus](https://github.com/godbus/dbus). [Binary
packages](https://github.com/xintron/xmonad-log/releases) are available.

### Building from source

This package has been tested with Go 1.7 and above.

To build from source:
 1. Clone this repository into `$GOPATH/src/github.com/xintron/xmonad-log`.
 2. Build it within the directory with `go build`.

This should leave a `xmonad-log` binary in the directory. Move this to an
appropriate directory in your `$PATH`.

## Configure xmonad

To configure xmonad to send log events over DBus the haskell
[dbus](http://hackage.haskell.org/package/dbus) package is required. Once
installed the following can be added to your `.xmonad/xmonad.hs` configuration
to add DBus support.


View [this
xmonad-config](https://github.com/xintron/configs/blob/22a33b41587c180172392f80318883921c543053/.xmonad/lib/Config.hs#L199)
for a fully working polybar example using statusbar coloring.

Both xmonad examples below do the same exact thing however the second one might make it easier to map the xmonad logging output to something that is more easy to identify, ex: display id.
Note: According to dbus specification, paths can only contain numbers, letters, underscores, and the / character, so you cannot enter the log id as "DP-0" but rather "DP_0". If you have a script that spawns your bar based on the display, you must have the script replace the '-' with '_' from your xrandr query. 

Screen index id as xmonad-log log id config
```haskell
import XMonad
import XMonad.Hooks.DynamicLog

import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8

main :: IO ()
main = do
    dbus <- D.connectSession
    -- Request access to the DBus name
    D.requestName dbus (D.busName_ "org.xmonad.Log")
        [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

    -- Example of setting up logging for the first two screens
    xmonad $ def { logHook = dynamicLogWithPP (myLogHook dbus 0) <+> dynamicLogWithPP (myLogHook dbus 1) }

-- Override the PP values as you would otherwise, adding colors etc depending
-- on  the statusbar used
myLogHook :: D.Client -> Int -> PP
myLogHook dbus id = marshallPP (S id) $ def { ppOutput = dbusOutput dbus id }

-- Emit a DBus signal on log updates
dbusOutput :: D.Client -> Int -> String -> IO ()
dbusOutput dbus id str = do
    let signal = (D.signal objectPath interfaceName memberName) {
            D.signalBody = [D.toVariant $ UTF8.decodeString str]
        }
    D.emit dbus signal
  where
    objectPath = D.objectPath_ ("/org/xmonad/Log_" ++ show id)
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName = D.memberName_ "Update"
```

Custom log id as xmonad-log log id config
```haskell
import XMonad
import XMonad.Hooks.DynamicLog

import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8

main :: IO ()
main = do
    dbus <- D.connectSession
    -- Request access to the DBus name
    D.requestName dbus (D.busName_ "org.xmonad.Log")
        [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

    -- Example of setting up logging for the first two screens
    xmonad $ def { logHook = dynamicLogWithPP (myLogHook dbus 0 "DP-2") <+> dynamicLogWithPP (myLogHook dbus 1 "DP-0") }

-- Override the PP values as you would otherwise, adding colors etc depending
-- on  the statusbar used
myLogHook :: D.Client -> Int -> String -> PP
myLogHook dbus id log_id = marshallPP (S id) $ def { ppOutput = dbusOutput dbus log_id }

-- Emit a DBus signal on log updates
dbusOutput :: D.Client -> String -> String -> IO ()
dbusOutput dbus log_id str = do
    let signal = (D.signal objectPath interfaceName memberName) {
            D.signalBody = [D.toVariant $ UTF8.decodeString str]
        }
    D.emit dbus signal
  where
    objectPath = D.objectPath_ ("/org/xmonad/Log_" ++ log_id)
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName = D.memberName_ "Update"
```
