module Streaming.Network.Client.Telnet.Cmd where

import Data.Word (Word8)

data Cmd
  = SE | NOP | DM | BRK | IP | AO | AYT | EC | EL | GA | SB
  | DO | DONT | WILL | WONT
  | IAC
  | Misc Word8
  deriving (Eq, Show)

cmdToCode :: Cmd -> Word8
cmdToCode SE       = 240
cmdToCode NOP      = 241
cmdToCode DM       = 242
cmdToCode BRK      = 243
cmdToCode IP       = 244
cmdToCode AO       = 245
cmdToCode AYT      = 246
cmdToCode EC       = 247
cmdToCode EL       = 248
cmdToCode GA       = 249
cmdToCode SB       = 250
cmdToCode DO       = 251
cmdToCode DONT     = 252
cmdToCode WILL     = 253
cmdToCode WONT     = 254
cmdToCode IAC      = 255
cmdToCode (Misc w) = w  

codeToCmd :: Word8 -> Cmd
codeToCmd 240 = SE
codeToCmd 241 = NOP
codeToCmd 242 = DM
codeToCmd 243 = BRK
codeToCmd 244 = IP
codeToCmd 245 = AO
codeToCmd 246 = AYT
codeToCmd 247 = EC
codeToCmd 248 = EL
codeToCmd 249 = GA
codeToCmd 250 = SB
codeToCmd 251 = DO
codeToCmd 252 = DONT
codeToCmd 253 = WILL
codeToCmd 254 = WONT
codeToCmd 255 = IAC
codeToCmd w   = Misc w
