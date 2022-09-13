{-# language ForeignFunctionInterface, CApiFFI, UnliftedFFITypes, GHCForeignImportPrim #-}
module RTS.Trace (module RTS.Trace) where

-- | Emits an event via the RTS tracing framework.  The contents
--      of the event is the zero-terminated byte string passed as the first
--      argument.  The event will be emitted either to the @.eventlog@ file,
--      or to stderr, depending on the runtime RTS flags.
event ∷ S# → ST_ s
event = traceEvent#

-- | Emits an event via the RTS tracing framework.  The contents
--      of the event is the binary object passed as the first argument with
--      the the given length passed as the second argument. The event will be
--      emitted to the @.eventlog@ file.
binaryEvent ∷ P# → I {- ^ length -} → ST_ s
binaryEvent = traceBinaryEvent#

-- | Emits a marker event via the RTS tracing framework with the argument as contents.
--      The event will be emitted either to the @.eventlog@ file,
--      or to stderr, depending on the runtime RTS flags. 
marker# :: S# -> ST_ s
marker# = traceMarker#
