module Style where

import Oak.Css

textSize :: String
textSize = "16px"

container :: Array StyleAttribute
container =
  [ display "flex"
  ]


section :: Array StyleAttribute
section =
  [ flex "1"
  , maxWidth "15vw"
  , margin "20px"
  ]


big :: Array StyleAttribute
big =
  [ fontSize textSize
  ]


list :: Array StyleAttribute
list =
  [ listStyleType "none"
  , margin "0"
  , padding "4px"
  ]


checkbox :: Array StyleAttribute
checkbox =
  let size = "16px"
  in
  [ width size
  , height size
  ]
