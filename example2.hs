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
    xmonad $ def { logHook = dynamicLogWithPP (myLogHook dbus 0 "DP_2") <+> dynamicLogWithPP (myLogHook dbus 1 "DP_0") }

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