module Style where

import Oak.Css



container :: Array StyleAttribute
container =
  [ display "flex"
  ]


section :: Array StyleAttribute
section =
  [ StyleAttribute "flex" "1"
  ]


big :: Array StyleAttribute
big =
  [ fontSize "24pt"
  ]

list :: Array StyleAttribute
list =
  [ listStyleType "none"
  , margin "0"
  , padding "4px"
  ]
