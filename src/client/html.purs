module DeanList.Client.Html where

import VirtualDOM.VTree (VTree(..), vnode)

div :: forall a. { | a } -> [VTree] -> VTree
div = vnode "div"

h1 :: forall a. { | a } -> [VTree] -> VTree
h1 = vnode "h1"

a :: forall a. { | a } -> [VTree] -> VTree
a = vnode "a"

pureUnit :: String -> [VTree] -> VTree
pureUnit ratio = div {className: "pure-u-1 pure-u-lg-" ++ ratio}
