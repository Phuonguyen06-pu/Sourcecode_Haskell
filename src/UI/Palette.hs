module UI.Palette where

import Graphics.Gloss (Color, makeColorI)

navy, panel, cyanNeon, pinkNeon, goldNeon, muted, bright :: Color
navy        = makeColorI 8 12 20 255
panel       = makeColorI 18 29 45 255
cyanNeon    = makeColorI 0 229 255 255
pinkNeon    = makeColorI 255 45 149 255
goldNeon    = makeColorI 255 213 78 255
muted       = makeColorI 114 145 166 255
bright      = makeColorI 227 244 255 255

-- Màu hai xe (P1 hồng, P2 xanh)
p1Color, p2Color :: Color
p1Color = makeColorI 244 63 94 255
p2Color = makeColorI 56 189 248 255

-- Màu vật cản và sàn đặc biệt
brickColor, brickEdge, steelFill, steelEdge :: Color
brickColor = makeColorI 71 85 105 255
brickEdge  = makeColorI 100 116 139 255
steelFill  = makeColorI 30 41 59 255
steelEdge  = makeColorI 203 213 225 255

mudColor, mudEdge, trapColor, trapEdge, springColor, barrelColor, barrelCore, teleColor :: Color
mudColor    = makeColorI 120 53 15 255
mudEdge     = makeColorI 146 64 14 255
trapColor   = makeColorI 168 85 247 255
trapEdge    = makeColorI 233 213 255 255
springColor = makeColorI 234 179 8 255
barrelColor = makeColorI 239 68 68 255
barrelCore  = makeColorI 254 240 138 255
teleColor   = makeColorI 56 189 248 255
