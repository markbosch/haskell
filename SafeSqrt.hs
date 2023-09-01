safeSqrt :: Either String Double -> Either String Double
safeSqrt (Left str) = Left str
safeSqrt (Right x) = if x < 0
                     then Left "Error"
                     else Right $ sqrt x