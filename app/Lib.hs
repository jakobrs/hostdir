module Lib where

import Data.Streaming.Network.Internal

ppHostPreference :: HostPreference -> String
ppHostPreference HostAny      = "*"
ppHostPreference HostIPv4     = "*4"
ppHostPreference HostIPv4Only = "!4"
ppHostPreference HostIPv6     = "*6"
ppHostPreference HostIPv6Only = "!6"
ppHostPreference (Host str)   = str
