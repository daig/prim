--------------------------------------------------------------------
-- | Description : Runtime Exceptions
--
-- In addThe @throw@ family of functions are meant to 
--
-- See:
--
-- - A semantics for imprecise exceptions, by Simon Peyton Jones, Alastair Reid, Tony Hoare, Simon Marlow, Fergus Henderson, in PLDI'99.
-- - Asynchronous exceptions in Haskell, by Simon Marlow, Simon Peyton Jones, Andy Moran and John Reppy, in PLDI'01.
-- - An Extensible Dynamically-Typed Hierarchy of Exceptions, by Simon Marlow, in Haskell '06.
--------------------------------------------------------------------
{-# language GHCForeignImportPrim,UnliftedFFITypes #-}
module IO.Exception (E, module IO.Exception, module X) where
import Prelude hiding (catch#)
import GHC.Prim qualified as GHC
import GHC.Maybe
import IO.Exception.Mask qualified as Mask
import IO.Exception.Some
import GHC.Err as X (error,errorWithoutStackTrace,undefined)


-- | Catches /all/ types of exceptions by unsafely coercing it to @e@.
-- May crash if the type doesn't match.
--
--  Async exceptions are masked automatically during
--  the execution of an exception handler.
catch# ∷ ∀ {r} (a ∷ T r) e. IO a → (e → IO a) → IO a
catch# = GHC.catch#; {-# inline catch# #-}

-- | This is the simplest of the exception-catching functions.  It
-- takes a single argument, runs it, and if an exception is raised
-- the \"handler\" is executed, with the value of the exception passed as an
-- argument.  Otherwise, the result is returned as normal.  For example:
--
-- >   catch (readFile f)
-- >         (\e -> do let err = show (e :: IOException)
-- >                   hPutStr stderr ("Warning: Couldn't open " ++ f ++ ": " ++ err)
-- >                   return "")
--
-- Note that we have to give a type signature to @e@, or the program
-- will not typecheck as the type is ambiguous. While it is possible
-- to catch exceptions of any type, see the section \"Catching all
-- exceptions\" (in "Control.Exception") for an explanation of the problems with doing so.
--
-- For catching exceptions in pure (non-'IO') expressions, see the
-- function 'evaluate'.
--
-- Note that due to Haskell\'s unspecified evaluation order, an
-- expression may throw one of several possible exceptions: consider
-- the expression @(error \"urk\") + (1 \`div\` 0)@.  Does
-- the expression throw
-- @ErrorCall \"urk\"@, or @DivideByZero@?
--
-- The answer is \"it might throw either\"; the choice is
-- non-deterministic. If you are catching any type of exception then you
-- might catch either. If you are calling @catch@ with type
-- @IO Int -> (ArithException -> IO Int) -> IO Int@ then the handler may
-- get run with @DivideByZero@ as an argument, or an @ErrorCall \"urk\"@
-- exception may be propagated further up. If you call it again, you
-- might get the opposite behaviour. This is ok, because 'catch' is an
-- 'IO' computation.
--
--  Async exceptions are masked automatically during
--  the execution of an exception handler.

catch ∷ E e ⇒ IO a → (e → IO a) → IO a
catch io k = catch# io \case
  some@(fromException → e') → case e' of Just e  → k e
                                         Nothing → throw some

-- | Raising values other than type @SomeException@ leads to segfault if uncaught by 'catch#'
throw## ∷ ∀ {r} (a ∷ T r) e. e → a
throw## = Prelude.raise#; {-# inline throw## #-}
-- | Raising values other than type @SomeException@ leads to segfault if uncaught by 'catch#'
throw# ∷ ∀ {r} (a ∷ T r) e. e → IO a
throw# = GHC.raiseIO#; {-# inline throw# #-}

-- | A variant of 'throw' that can only be used within the 'IO' monad.
--
-- Although 'throw' has a type that is an instance of the type of 'throw', the
-- two functions are subtly different:
--
-- > throw e   `seq` ()  ===> throw e
-- > throw e `seq` ()  ===> ()
--
-- The first example will cause the exception @e@ to be raised,
-- whereas the second one won\'t.  In fact, 'throw' will only cause
-- an exception to be raised when it is used within the 'IO' monad.
--
-- The 'throw' variant should be used in preference to 'throw' to
-- raise an exception within the 'IO' monad because it guarantees
-- ordering with respect to other operations, whereas 'throw'
-- does not. We say that 'throw' throws *precise* exceptions and
-- 'throw', 'error', etc. all throw *imprecise* exceptions.
-- For example
--
-- > throw e + error "boom" ===> error "boom"
-- > throw e + error "boom" ===> throw e
--
-- are both valid reductions and the compiler may pick any (loop, even), whereas
--
-- > throw e >> error "boom" ===> throw e
--
-- will always throw @e@ when executed.
--
-- See also the
-- [GHC wiki page on precise exceptions](https://gitlab.haskell.org/ghc/ghc/-/wikis/exceptions/precise-exceptions)
-- for a more technical introduction to how GHC optimises around precise vs.
throw ∷ ∀ {r} (a ∷ T r) e. E e ⇒ e → IO a
throw (toException → e) = GHC.raiseIO# e; {-# inline throw #-}

-- | Evaluate the argument to weak head normal form.
-- Always produces a valid 'IO' action which throws on /execution/
-- iff the value 'throw##'s on /evaluation/.
-- In contrast 'seq' will throw an exception.
--
-- The practical implication of this difference is that due to the /imprecise exceptions/ semantics,
--
-- > (throw "foo") >> throw "bar"
--
-- If you are forcing a lazy value for efficiency reasons only and do not
-- care about exceptions, you may use 'seq'
--
-- see https://gitlab.haskell.org/ghc/ghc/-/blob/fc644b1a643128041cfec25db84e417851e28bab/compiler/GHC/Core/Opt/ConstantFold.hs#L1198
eval ∷ a → IO a
eval = seq#; {-# inline eval #-}


-- | An unrecoverable error. Display the string and exit.
foreign import prim "stg_paniczh" error# ∷ ∀ {r} {rr} (x ∷ T r) (ret ∷ T rr). S C1 → IO ret


-- | Executes an IO computation with asynchronous
-- exceptions /masked/.  That is, any thread which attempts to raise
-- an exception in the current thread with 'Control.Exception.throwTo'
-- will be blocked until asynchronous exceptions are Mask.unmasked again.
--
-- The argument passed to 'mask' is a function that takes as its
-- argument another function, which can be used to restore the
-- prevailing masking state within the context of the masked
-- computation.  For example, a common way to use 'mask' is to protect
-- the acquisition of a resource:
--
-- > mask $ \restore -> do
-- >     x <- acquire
-- >     restore (do_something_with x) `onException` release
-- >     release
--
-- This code guarantees that @acquire@ is paired with @release@, by masking
-- asynchronous exceptions for the critical parts. (Rather than write
-- this code yourself, it would be better to use
-- 'Control.Exception.bracket' which abstracts the general pattern).
--
-- Note that the @restore@ action passed to the argument to 'mask'
-- does not necessarily Mask.unmask asynchronous exceptions, it just
-- restores the masking state to that of the enclosing context.  Thus
-- if asynchronous exceptions are already masked, 'mask' cannot be used
-- to Mask.unmask exceptions again.  This is so that if you call a library function
-- with exceptions masked, you can be sure that the library call will not be
-- able to Mask.unmask exceptions again.  If you are writing library code and need
-- to use asynchronous exceptions, the only way is to create a new thread;
-- see 'Control.Concurrent.forkIOWithUnmask'.
--
-- Asynchronous exceptions may still be received while in the masked
-- state if the masked thread /blocks/ in certain ways; see
-- "Control.Exception#interruptible".
--
-- Threads created by 'Control.Concurrent.forkIO' inherit the
-- 'MaskingState' from the parent; that is, to start a thread in the
-- 'MaskedInterruptible' state,
-- use @mask_ $ forkIO ...@.  This is particularly useful if you need
-- to establish an exception handler in the forked thread before any
-- asynchronous exceptions are received.  To create a new thread in
-- an Mask.unmasked state use 'Control.Concurrent.forkIOWithUnmask'.
mask ∷ ∀ {rb} (b ∷ T rb). ((∀ {ra} (a ∷ T ra). IO a → IO a) → IO b) → IO b
mask k = \t → case Mask.state t of
  (# tt, s #) → case s of
         Mask.Unmasked → Mask.async (k Mask.unmask) tt
         Mask.Interruptible → k Mask.async tt
         Mask.Uninterruptible → k Mask.uninterruptible tt

-- | Like 'mask', but the masked computation is not interruptible (see
-- "Control.Exception#interruptible").  THIS SHOULD BE USED WITH
-- GREAT CARE, because if a thread executing in 'uninterruptibleMask'
-- blocks for any reason, then the thread (and possibly the program,
-- if this is the main thread) will be unresponsive and unkillable.
-- This function should only be necessary if you need to mask
-- exceptions around an interruptible operation, and you can guarantee
-- that the interruptible operation will only block for a short period
-- of time.
mask# ∷ ∀ {rb} (b ∷ T rb). ((∀ {ra} (a ∷ T ra). IO a → IO a) → IO b) → IO b
mask# k = \t → case Mask.state t of
  (# tt, s #) → case s of
    Mask.Unmasked → Mask.uninterruptible (k Mask.unmask) tt
    Mask.Interruptible → Mask.uninterruptible (k Mask.async) tt
    Mask.Uninterruptible → k Mask.uninterruptible tt

onException ∷ IO a → IO_ → IO a
onException io cleanup = catch io \ e t → throw @_ @SomeException e (cleanup t)

bracket ∷ IO a → (a → IO_) → (a → IO b) → IO b
bracket init cleanup io = mask \ k t → case init t of
  (# tt, a #) → case (k (io a) `onException` cleanup a) tt of
    (# ttt, b #) → (# cleanup a ttt, b #)

-- | Perferm a cleanup action after some @io@, even if @io@ throws an exception
finally ∷ IO a → IO_ → IO a
finally io cleanup = mask \ k t → case (k io `onException` cleanup) t of (# tt, a #) → (# cleanup tt, a #)


    
