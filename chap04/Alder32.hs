import Data.Bits (shiftL, (.&.), (.|.))
import Data.Char (ord)

base = 65521

adler32 xs = helper 1 0 xs
  where
    helper a b (x : xs) =
      let a' = (a + (ord x .&. 0xff)) `mod` base
          b' = (a' + b) `mod` base
       in helper a' b' xs
    helper a b _ = (b `shiftL` 16) .|. a

