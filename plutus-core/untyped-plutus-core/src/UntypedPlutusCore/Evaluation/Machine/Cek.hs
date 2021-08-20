-- | The API to the CEK machine.

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module UntypedPlutusCore.Evaluation.Machine.Cek
    (
    -- * Running the machine
    runCek
    , runCekNoEmit
    , runCekNoEmit'
    , unsafeRunCekNoEmit
    , evaluateCek
    , evaluateCekNoEmit
    , evaluateCekNoEmit'
    , unsafeEvaluateCek
    , unsafeEvaluateCek'
    , unsafeEvaluateCekNoEmit
    , unsafeEvaluateCekNoEmit'
    , EvaluationResult(..)
    , extractEvaluationResult
    -- * Errors
    , CekUserError(..)
    , ErrorWithCause(..)
    , CekEvaluationException
    , EvaluationError(..)
    -- * Costing
    , ExBudgetCategory(..)
    , CekBudgetSpender(..)
    , ExBudgetMode(..)
    , StepKind(..)
    , CekExTally(..)
    , CountingSt (..)
    , TallyingSt (..)
    , RestrictingSt (..)
    , CekMachineCosts
    -- ** Costing modes
    , counting
    , tallying
    , restricting
    , restrictingEnormous
    , enormousBudget
    -- * Emitter modes
    , noEmitter
    , logEmitter
    , logWithTimeEmitter
    -- * Misc
    , CekValue(..)
    , readKnownCek
    , Hashable
    , PrettyUni
    )
where

import           PlutusPrelude

import           UntypedPlutusCore.Core
import           UntypedPlutusCore.DeBruijn
import           UntypedPlutusCore.Evaluation.Machine.Cek.CekMachineCosts
import           UntypedPlutusCore.Evaluation.Machine.Cek.EmitterMode
import           UntypedPlutusCore.Evaluation.Machine.Cek.ExBudgetMode
import           UntypedPlutusCore.Evaluation.Machine.Cek.Internal

import           PlutusCore.Constant
import           PlutusCore.Evaluation.Machine.ExMemory
import           PlutusCore.Evaluation.Machine.Exception
import           PlutusCore.Evaluation.Machine.MachineParameters
import           PlutusCore.Name
import           PlutusCore.Pretty

import           Control.Monad.Except
import           Data.Ix                                                  (Ix)
import           Data.Text                                                (Text)
import           PlutusCore.Quote
import           Universe

{- Note [CEK runners naming convention]
A function whose name ends in @NoEmit@ does not perform logging and so does not return any logs.
A function whose name starts with @unsafe@ throws exceptions instead of returning them purely.
A function from the @runCek@ family takes an 'ExBudgetMode' parameter and returns the final
'CekExBudgetState' (and possibly logs). Note that 'runCek' is defined in @...Cek.Internal@ for
reasons explained in Note [Compilation peculiarities].
A function from the @evaluateCek@ family does not return the final 'ExBudgetMode', nor does it
allow one to specify an 'ExBudgetMode'. I.e. such functions are only for fully evaluating programs
(and possibly returning logs). See also haddocks of 'enormousBudget'.
-}

-- | Evaluate a term using the CEK machine with logging disabled and keep track of costing.
runCekNoEmit
    :: ( uni `Everywhere` ExMemoryUsage, Ix fun, PrettyUni uni fun)
    => MachineParameters CekMachineCosts CekValue uni fun
    -> ExBudgetMode cost uni fun
    -> Term NamedDeBruijn uni fun ()
    -> (Either (CekEvaluationException uni fun) (Term NamedDeBruijn uni fun ()), cost)
runCekNoEmit params mode term =
    case runCek params mode noEmitter term of
        (errOrRes, cost', _) -> (errOrRes, cost')

-- | Evaluate a term using the CEK machine with logging disabled and keep track of costing.
runCekNoEmit'
    :: ( uni `Everywhere` ExMemoryUsage, Ix fun, PrettyUni uni fun, Monoid cost)
    => MachineParameters CekMachineCosts CekValue uni fun
    -> ExBudgetMode cost uni fun
    -> Term Name uni fun ()
    -> (Either (CekEvaluationException uni fun) (Term Name uni fun ()), cost)
runCekNoEmit' params mode term =
    case runExcept $ deBruijnTerm term of
        Left (e :: FreeVariableError) -> (error "no", mempty)
        Right dbt -> do
            let (errOrRes, cost', _) = runCek params mode noEmitter dbt
            case errOrRes of
                Left e -> (Left e, cost')
                Right rt -> case runQuote $ runExceptT $ unDeBruijnTerm rt of
                    Left (e :: FreeVariableError) -> (error "no", cost')
                    Right nt                      -> (Right nt, cost')


-- | Unsafely evaluate a term using the CEK machine with logging disabled and keep track of costing.
-- May throw a 'CekMachineException'.
unsafeRunCekNoEmit
    :: ( GShow uni, Typeable uni
       , Closed uni, uni `EverywhereAll` '[ExMemoryUsage, PrettyConst]
       , Ix fun, Pretty fun, Typeable fun
       )
    => MachineParameters CekMachineCosts CekValue uni fun
    -> ExBudgetMode cost uni fun
    -> Term NamedDeBruijn uni fun ()
    -> (EvaluationResult (Term NamedDeBruijn uni fun ()), cost)
unsafeRunCekNoEmit params mode =
    first unsafeExtractEvaluationResult . runCekNoEmit params mode

-- | Evaluate a term using the CEK machine with logging enabled.
evaluateCek
    :: ( uni `Everywhere` ExMemoryUsage, Ix fun, PrettyUni uni fun)
    => EmitterMode uni fun
    -> MachineParameters CekMachineCosts CekValue uni fun
    -> Term NamedDeBruijn uni fun ()
    -> (Either (CekEvaluationException uni fun) (Term NamedDeBruijn uni fun ()), [Text])
evaluateCek emitMode params term =
    case runCek params restrictingEnormous emitMode term of
        (errOrRes, _, logs) -> (errOrRes, logs)

-- | Evaluate a term using the CEK machine with logging enabled.
evaluateCek'
    :: ( uni `Everywhere` ExMemoryUsage, Ix fun, PrettyUni uni fun)
    => EmitterMode uni fun
    -> MachineParameters CekMachineCosts CekValue uni fun
    -> Term Name uni fun ()
    -> (Either (CekEvaluationException uni fun) (Term Name uni fun ()), [Text])
evaluateCek' emitMode params term =
    case runExcept $ deBruijnTerm term of
        Left (e :: FreeVariableError) -> (error "no", mempty)
        Right dbt -> do
            let (errOrRes, _, logs) = runCek params restrictingEnormous emitMode dbt
            case errOrRes of
                Left e -> (Left e, logs)
                Right rt -> case runQuote $ runExceptT $ unDeBruijnTerm rt of
                    Left (e :: FreeVariableError) -> (error "no", logs)
                    Right nt                      -> (Right nt, logs)

-- | Evaluate a term using the CEK machine with logging disabled.
evaluateCekNoEmit
    :: ( uni `Everywhere` ExMemoryUsage, Ix fun, PrettyUni uni fun)
    => MachineParameters CekMachineCosts CekValue uni fun
    -> Term NamedDeBruijn uni fun ()
    -> Either (CekEvaluationException uni fun) (Term NamedDeBruijn uni fun ())
evaluateCekNoEmit params = fst . runCekNoEmit params restrictingEnormous

-- | Evaluate a term using the CEK machine with logging disabled.
evaluateCekNoEmit'
    :: ( uni `Everywhere` ExMemoryUsage, Ix fun, PrettyUni uni fun)
    => MachineParameters CekMachineCosts CekValue uni fun
    -> Term Name uni fun ()
    -> Either (CekEvaluationException uni fun) (Term Name uni fun ())
evaluateCekNoEmit' params = fst . runCekNoEmit' params restrictingEnormous

-- | Evaluate a term using the CEK machine with logging enabled. May throw a 'CekMachineException'.
unsafeEvaluateCek
    :: ( GShow uni, Typeable uni
       , Closed uni, uni `EverywhereAll` '[ExMemoryUsage, PrettyConst]
       , Ix fun, Pretty fun, Typeable fun
       )
    => EmitterMode uni fun
    -> MachineParameters CekMachineCosts CekValue uni fun
    -> Term NamedDeBruijn uni fun ()
    -> (EvaluationResult (Term NamedDeBruijn uni fun ()), [Text])
unsafeEvaluateCek emitTime params = first unsafeExtractEvaluationResult . evaluateCek emitTime params

-- | Evaluate a term using the CEK machine with logging enabled. May throw a 'CekMachineException'.
unsafeEvaluateCek'
    :: ( GShow uni, Typeable uni
       , Closed uni, uni `EverywhereAll` '[ExMemoryUsage, PrettyConst]
       , Ix fun, Pretty fun, Typeable fun
       )
    => EmitterMode uni fun
    -> MachineParameters CekMachineCosts CekValue uni fun
    -> Term Name uni fun ()
    -> (EvaluationResult (Term Name uni fun ()), [Text])
unsafeEvaluateCek' emitTime params = first unsafeExtractEvaluationResult . evaluateCek' emitTime params

-- | Evaluate a term using the CEK machine with logging disabled. May throw a 'CekMachineException'.
unsafeEvaluateCekNoEmit
    :: ( GShow uni, Typeable uni
       , Closed uni, uni `EverywhereAll` '[ExMemoryUsage, PrettyConst]
       , Ix fun, Pretty fun, Typeable fun
       )
    => MachineParameters CekMachineCosts CekValue uni fun
    -> Term NamedDeBruijn uni fun ()
    -> EvaluationResult (Term NamedDeBruijn uni fun ())
unsafeEvaluateCekNoEmit params = unsafeExtractEvaluationResult . evaluateCekNoEmit params

-- | Evaluate a term using the CEK machine with logging disabled. May throw a 'CekMachineException'.
unsafeEvaluateCekNoEmit'
    :: ( GShow uni, Typeable uni
       , Closed uni, uni `EverywhereAll` '[ExMemoryUsage, PrettyConst]
       , Ix fun, Pretty fun, Typeable fun
       )
    => MachineParameters CekMachineCosts CekValue uni fun
    -> Term Name uni fun ()
    -> EvaluationResult (Term Name uni fun ())
unsafeEvaluateCekNoEmit' params = unsafeExtractEvaluationResult . evaluateCekNoEmit' params

-- | Unlift a value using the CEK machine.
readKnownCek
    :: ( uni `Everywhere` ExMemoryUsage
       , KnownType (Term NamedDeBruijn uni fun ()) a
       , Ix fun, PrettyUni uni fun
       )
    => MachineParameters CekMachineCosts CekValue uni fun
    -> Term NamedDeBruijn uni fun ()
    -> Either (CekEvaluationException uni fun) a
readKnownCek params = evaluateCekNoEmit params >=> readKnown
