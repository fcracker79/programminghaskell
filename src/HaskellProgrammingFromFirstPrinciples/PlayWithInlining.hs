module HaskellProgrammingFromFirstPrinciples.PlayWithInlining where
import Debug.Trace(trace)
import GHC.IO (unsafePerformIO)

{-# INLINE inlined #-}
{-# NOINLINE notinlined #-}
{-# INLINE unsafeInlined #-}
{-# NOINLINE unsafeNotinlined #-}

inlined :: IO ()
inlined = trace "executing inlined" $ print "inline"

notinlined :: IO()
notinlined = trace "executing notinlined" print "not inlined"

unsafeInlined :: ()
unsafeInlined = trace "executing UNSAFE inlined" $ unsafePerformIO $ print "UNSAFE inline"

unsafeNotinlined :: ()
unsafeNotinlined = trace "executing UNSAFE notinlined" unsafePerformIO $ print "UNSAFE not inlined"

main :: IO ()
main = do
    inlined
    inlined
    notinlined
    notinlined
    let u1 = unsafeInlined
    let u2 = unsafeInlined
    let u3 = unsafeNotinlined
    let u4 = unsafeNotinlined
    print u1
    print u2
    print u3
    print u4
    return ()
