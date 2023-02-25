--------------------------------------------------------------------
-- | Description : Escaping IO into pure values
--------------------------------------------------------------------
module IO.Unsafe (runRW#, module IO.Unsafe) where
import GHC.Magic

-- | Return the value computed by a state thread.
-- The @forall@ ensures that the internal state used by the 'ST'
-- computation is inaccessible to the rest of the program.
class RunST (a ∷ T ra) where
  runST ∷ (∀ s. ST s a) → a
{-|
This version of 'unIO#' is more efficient
because it omits the check that the IO is only being performed by a
single thread.  Hence, when you use 'unsafeDupablePerformIO',
there is a possibility that the IO action may be performed multiple
times (on a multiprocessor), and you should therefore ensure that
it gives the same results each time. It may even happen that one
of the duplicated IO actions is only run partially, and then interrupted
in the middle without an exception being raised. Therefore, functions
like 'Control.Exception.bracket' cannot be used safely within
'unsafeDupablePerformIO'. -}
  runIO## ∷ IO a → a
{-|
This is the \"back door\" into the 'IO' monad, allowing
'IO' computation to be performed at any time.  For
this to be safe, the 'IO' computation should be
free of side effects and independent of its environment.

If the I\/O computation wrapped in 'unsafePerformIO' performs side
effects, then the relative order in which those side effects take
place (relative to the main I\/O trunk, or other calls to
'unsafePerformIO') is indeterminate.  Furthermore, when using
'unsafePerformIO' to cause side-effects, you should take the following
precautions to ensure the side effects are performed as many times as
you expect them to be.  Note that these precautions are necessary for
GHC, but may not be sufficient, and other compilers may require
different precautions:

  * Use @{\-\# NOINLINE foo \#-\}@ as a pragma on any function @foo@
        that calls 'unsafePerformIO'.  If the call is inlined,
        the I\/O may be performed more than once.

  * Use the compiler flag @-fno-cse@ to prevent common sub-expression
        elimination being performed on the module, which might combine
        two side effects that were meant to be separate.  A good example
        is using multiple global variables (like @test@ in the example below).

  * Make sure that the either you switch off let-floating (@-fno-full-laziness@), or that the
        call to 'unsafePerformIO' cannot float outside a lambda.  For example,
        if you say:
        @
           f x = unsafePerformIO (newIORef [])
        @
        you may get only one reference cell shared between all calls to @f@.
        Better would be
        @
           f x = unsafePerformIO (newIORef [x])
        @
        because now it can't float outside the lambda.

It is less well known that
'unsafePerformIO' is not type safe.  For example:

>     test :: IORef [a]
>     test = unsafePerformIO $ newIORef []
>
>     main = do
>             writeIORef test [42]
>             bang <- readIORef test
>             print (bang :: [Char])

This program will core dump.  This problem with polymorphic references
is well known in the ML community, and does not arise with normal
monadic use of references.  There is no easy way to make it impossible
once you use 'unsafePerformIO'.  Indeed, it is
possible to write @coerce :: a -> b@ with the
help of 'unsafePerformIO'.  So be careful!

WARNING: If you're looking for "a way to get a 'String' from an 'IO String'",
then 'unsafePerformIO' is not the way to go.  Learn about do-notation and the
@<-@ syntax element before you proceed.
-}
  runIO# ∷ IO a → a
  {-|
'unsafeDupableInterleaveIO' allows an 'IO' computation to be deferred lazily.
When passed a value of type @IO a@, the 'IO' will only be performed
when the value of the @a@ is demanded.

The computation may be performed multiple times by different threads,
possibly at the same time. To ensure that the computation is performed
only once, use 'unsafeInterleaveIO' instead.
-}
  interleaveIO# :: IO a -> IO a
  interleaveIO## :: IO a -> IO a

instance RunST (a ∷ T_I) where 
  runST st = case runRW# st of (# _, a #) → a
  runIO## st = case runRW# st of (# _, a #) → a
  runIO# st = case runRW# (\t → st (noDuplicate t)) of (# _, a #) → a
  interleaveIO# io = \ s -> (# s, case io (noDuplicate# s) of {(# _, x #) -> x} #)
  interleaveIO## io = \ s -> (# s, case io s of {(# _, x #) -> x} #)


{- |
Note [unsafePerformIO and strictness]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this sub-expression (from tests/lib/should_run/memo002)

 unsafePerformIO (do { lockMemoTable
                     ; let r = f x
                     ; updateMemoTable x r
                     ; unlockMemoTable
                     ; return r })

It's super-important that the `let r = f x` is lazy. If the demand
analyser sees that `r` is sure to be demanded, it'll use call-by-value
for (f x), that will try to lock the already-locked table => deadlock.
See #19181 and #19413.

Now `r` doesn't look strict, because it's wrapped in a `return`.
But if we were to define unsafePerformIO like this
  unsafePerformIO (IO m) = case runRW# m of (# _, r #) -> r

then we'll push that `case` inside the arugment to runRW#, givign
  runRW# (\s -> case lockMemoTable s of s1 ->
                let r = f x in
                case updateMemoTable s1 of s2 ->
                case unlockMemoTable s2 of _ ->
                r)

And now that `let` really does look strict.  No good!

Solution: wrap the result of the unsafePerformIO in 'lazy', to conceal
it from the demand analyser:
  unsafePerformIO (IO m) = case runRW# m of (# _, r #) -> lazy r
                                                 ------>  ^^^^
See also Note [lazyId magic] in GHC.Types.Id.Make
-}
instance RunST (a ∷ ★) where 
  runST st = case runRW# st of (# _, a #) → a
  runIO## st = case runRW# st of (# _, a #) → lazy a
  runIO# st = case runRW# (\t → st (noDuplicate t)) of (# _, a #) → lazy a
  interleaveIO# = lifted_interleaveIO#
  interleaveIO## io = \ s -> (# s, case io s of {(# _, x #) -> x} #)



-- | Note [unsafeDupableInterleaveIO should not be inlined]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- We used to believe that INLINE on unsafeInterleaveIO was safe,
-- because the state from this IO thread is passed explicitly to the
-- interleaved IO, so it cannot be floated out and shared.
--
-- HOWEVER, if the compiler figures out that r is used strictly here,
-- then it will eliminate the thunk and the side effects in m will no
-- longer be shared in the way the programmer was probably expecting,
-- but can be performed many times.  In #5943, this broke our
-- definition of fixIO, which contains
--
--    ans <- unsafeInterleaveIO (takeMVar m)
--
-- after inlining, we lose the sharing of the takeMVar, so the second
-- time 'ans' was demanded we got a deadlock.  We could fix this with
-- a readMVar, but it seems wrong for unsafeInterleaveIO to sometimes
-- share and sometimes not (plus it probably breaks the noDuplicate).
-- So now, we do not inline unsafeDupableInterleaveIO.

lifted_interleaveIO# :: IO a -> IO a
lifted_interleaveIO# io = \ s -> let r = case io (noDuplicate# s) of {(# _, x #) -> x} in (# s, r #)
{-# noinline lifted_interleaveIO# #-}


{-|
Ensures that the suspensions under evaluation by the current thread
are unique; that is, the current thread is not evaluating anything
that is also under evaluation by another thread that has also executed
'noDuplicate'.

This operation is used in the definition of 'unsafePerformIO' to
prevent the IO action from being executed multiple times, which is usually
undesirable.
-}
noDuplicate ∷ IO_
noDuplicate = noDuplicate#; {-# inline noDuplicate #-}
