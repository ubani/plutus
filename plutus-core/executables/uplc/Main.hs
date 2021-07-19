{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}

module Main (main) where

import           Common
import           Parsers
import qualified PlutusCore                               as PLC
import           PlutusCore.Evaluation.Machine.ExBudget   (ExBudget (..), ExRestrictingBudget (..))
import           PlutusCore.Evaluation.Machine.ExMemory   (ExCPU (..), ExMemory (..))

import           Data.Foldable                            (asum)
import           Data.Function                            ((&))
import           Data.Functor                             (void)
import qualified Data.IntMap                              as IntMap
import           Data.List                                (nub)
import           Data.List.Split                          (splitOn)

import qualified UntypedPlutusCore                        as UPLC
import qualified UntypedPlutusCore.Evaluation.Machine.Cek as Cek

import           Control.DeepSeq                          (NFData, rnf)
import           Options.Applicative
import           System.Exit                              (exitFailure, exitSuccess)
import           Text.Printf                              (printf)
import           Text.Read                                (readMaybe)

uplcHelpText :: String
uplcHelpText = helpText "Untyped Plutus Core"

uplcInfoCommand :: ParserInfo Command
uplcInfoCommand = plutus uplcHelpText

data BudgetMode  = Silent
                 | forall cost. (Eq cost, NFData cost, PrintBudgetState cost) =>
                     Verbose (Cek.ExBudgetMode cost PLC.DefaultUni PLC.DefaultFun)

data EvalOptions = EvalOptions Input Format PrintMode BudgetMode TimingMode CekModel

data AnalysisOptions = AnalysisOptions Input Format

---------------- Main commands -----------------

data Command = Apply     ApplyOptions
             | Convert   ConvertOptions
             | Print     PrintOptions
             | Example   ExampleOptions
             | Eval      EvalOptions
             | Analyse   AnalysisOptions

---------------- Option parsers ----------------

cekmodel :: Parser CekModel
cekmodel = flag Default Unit
           (  short '1'
           <> long "unit-cek-model"
           <> help "Use unit AST node costs for CEK cost model (tallying mode only)"
           )

evalOpts :: Parser EvalOptions
evalOpts =
    EvalOptions <$> input <*> inputformat <*> printmode <*> budgetmode <*> timingmode <*> cekmodel

analysisOptions :: Parser AnalysisOptions
analysisOptions =
    AnalysisOptions <$> input <*> inputformat

-- Reader for budget.  The --restricting option requires two integer arguments
-- and the easiest way to do this is to supply a colon-separated pair of
-- integers.
exbudgetReader :: ReadM ExBudget
exbudgetReader = do
  s <- str
  case splitOn ":" s of
    [a,b] -> case (readMaybe a, readMaybe b) of
               (Just cpu, Just mem) -> pure $ ExBudget (ExCPU cpu) (ExMemory mem)
               _                    -> readerError badfmt
    _     -> readerError badfmt
    where badfmt = "Invalid budget (expected eg 10000:50000)"

restrictingbudgetEnormous :: Parser BudgetMode
restrictingbudgetEnormous = flag' (Verbose Cek.restrictingEnormous)
                            (  long "restricting-enormous"
                            <> short 'r'
                            <> help "Run the machine in restricting mode with an enormous budget" )

restrictingbudget :: Parser BudgetMode
restrictingbudget = Verbose . Cek.restricting . ExRestrictingBudget
                    <$> option exbudgetReader
                            (  long "restricting"
                            <> short 'R'
                            <> metavar "ExCPU:ExMemory"
                            <> help "Run the machine in restricting mode with the given limits" )

countingbudget :: Parser BudgetMode
countingbudget = flag' (Verbose Cek.counting)
                 (  long "counting"
                 <> short 'c'
                 <> help "Run machine in counting mode and report results" )

tallyingbudget :: Parser BudgetMode
tallyingbudget = flag' (Verbose Cek.tallying)
                 (  long "tallying"
                 <> short 't'
                 <> help "Run machine in tallying mode and report results" )

budgetmode :: Parser BudgetMode
budgetmode = asum
    [ restrictingbudgetEnormous
    , restrictingbudget
    , countingbudget
    , tallyingbudget
    , pure Silent
    ]

plutus ::
  -- | The @helpText@
  String ->
  ParserInfo Command
plutus langHelpText =
    info
      (plutusOpts <**> helper)
      (fullDesc <> header "Untyped Plutus Core Tool" <> progDesc langHelpText)

plutusOpts :: Parser Command
plutusOpts = hsubparser (
       command "apply"
           (info (Apply <$> applyOpts)
            (progDesc $ "Given a list of input scripts f g1 g2 ... gn, output a script consisting of (... ((f g1) g2) ... gn); "
            ++ "for example, 'plc apply --if flat Validator.flat Datum.flat Redeemer.flat Context.flat --of flat -o Script.flat'"))
    <> command "print"
           (info (Print <$> printOpts)
            (progDesc "Parse a program then prettyprint it."))
    <> command "convert"
           (info (Convert <$> convertOpts)
            (progDesc "Convert a program between various formats"))
    <> command "example"
           (info (Example <$> exampleOpts)
            (progDesc $ "Show a program example. "
                     ++ "Usage: first request the list of available examples (optional step), "
                     ++ "then request a particular example by the name of a term. "
                     ++ "Note that evaluating a generated example may result in 'Failure'."))
    <> command "evaluate"
           (info (Eval <$> evalOpts)
            (progDesc "Evaluate an untyped Plutus Core program using the CEK machine."))
    <> command "analyse"
           (info (Analyse <$> analysisOptions)
            (progDesc $ "Perform some analysis of an untyped Plutus Core AST"))
  )


---------------- Script application ----------------

-- | Apply one script to a list of others.
runApply :: ApplyOptions -> IO ()
runApply (ApplyOptions inputfiles ifmt outp ofmt mode) = do
  scripts <- mapM ((getProgram ifmt ::  Input -> IO (UplcProg PLC.AlexPosn)) . FileInput) inputfiles
  let appliedScript =
        case map (\case p -> () <$ p) scripts of
          []          -> errorWithoutStackTrace "No input files"
          progAndargs -> foldl1 UPLC.applyProgram progAndargs
  writeProgram outp ofmt mode appliedScript

---------------- Evaluation ----------------

runEval :: EvalOptions -> IO ()
runEval (EvalOptions inp ifmt printMode budgetMode timingMode cekModel) = do
    prog <- getProgram ifmt inp
    let term = void . UPLC.toTerm $ prog
        !_ = rnf term
        cekparams = case cekModel of
                    Default -> PLC.defaultCekParameters  -- AST nodes are charged according to the default cost model
                    Unit    -> PLC.unitCekParameters     -- AST nodes are charged one unit each, so we can see how many times each node
                                                         -- type is encountered.  This is useful for calibrating the budgeting code.
    case budgetMode of
        Silent -> do
            let evaluate = Cek.evaluateCekNoEmit cekparams
            case timingMode of
                NoTiming -> evaluate term & handleEResult printMode
                Timing n -> timeEval n evaluate term >>= handleTimingResults term
        Verbose bm -> do
            let evaluate = Cek.runCekNoEmit cekparams bm
            case timingMode of
                NoTiming -> do
                        let (result, budget) = evaluate term
                        printBudgetState term cekModel budget
                        handleResultSilently result  -- We just want to see the budget information
                Timing n -> timeEval n evaluate term >>= handleTimingResultsWithBudget term
    where
        handleResultSilently =
            \case
                Right _  -> exitSuccess
                Left err -> print err >> exitFailure
        handleTimingResultsWithBudget term results =
            case nub results of
            [(Right _, budget)] -> do
                putStrLn ""
                printBudgetState term cekModel budget
                exitSuccess
            [(Left err,   budget)] -> do
                putStrLn ""
                print err
                printBudgetState term cekModel budget
                exitFailure
            _                                   -> error "Timing evaluations returned inconsistent results"

----------------- Print examples -----------------------
runUplcPrintExample ::
    ExampleOptions -> IO ()
runUplcPrintExample = runPrintExample getUplcExamples

---------------- Parse and print a UPLC source file ----------------

runPrint :: PrintOptions -> IO ()
runPrint (PrintOptions inp mode) =
    (parseInput inp :: IO (UplcProg PLC.AlexPosn)) >>= print . getPrintMethod mode

---------------- Conversions ----------------

-- | Convert between textual and FLAT representations.  This subsumes the
-- `print` command: for example, `plc convert -i prog.plc --typed --fmt Readable`
-- will read a typed plc file and print it in the Readable format.  Having
-- the separate `print` option may be more user-friendly though.
runConvert :: ConvertOptions -> IO ()
runConvert (ConvertOptions inp ifmt outp ofmt mode) = do
    program <- (getProgram ifmt inp :: IO (UplcProg PLC.AlexPosn))
    writeProgram outp ofmt mode program


---------------- AST Analysis ----------------

analyseBindings :: UPLC.Term UPLC.Name uni fun ann -> IO ()
analyseBindings term =
    let countFreqs tm freqs =
            case tm of
              UPLC.Var _ name      ->
                  let n = UPLC.unUnique $ UPLC.nameUnique name
                  in case IntMap.lookup n freqs of
                       Nothing -> error $ "Free variable: " ++ show name
                       Just k  -> IntMap.insert n (k+1) freqs
              UPLC.LamAbs _ name t ->
                  let n = UPLC.unUnique $ UPLC.nameUnique name
                  in case IntMap.lookup n freqs of
                       Nothing -> countFreqs t (IntMap.insert n 0 freqs)
                       Just _  -> error $ "Duplicate binding for " ++ show name
              UPLC.Apply _ t1 t2   -> countFreqs t2 (countFreqs t1 freqs)
              UPLC.Force _ t       -> countFreqs t freqs
              UPLC.Delay _ t       -> countFreqs t freqs
              UPLC.Constant {}     -> freqs
              UPLC.Builtin {}      -> freqs
              UPLC.Error _         -> freqs
        frequencies = countFreqs term IntMap.empty  -- How often is each variable accessed?
        incrCount k m = case IntMap.lookup k m of
                          Nothing -> IntMap.insert k (1::Int) m
                          Just n  -> IntMap.insert k (n+1) m
        counts = IntMap.toList $ IntMap.foldr incrCount IntMap.empty frequencies
        numVars = IntMap.size frequencies
        percentage n = (fromIntegral n / fromIntegral numVars) * 100.0 :: Double
    in do
      putStrLn $ "There are " ++ show numVars ++ " variables in total"
      putStrLn $ "This number of variables (left) are accessed this number of times (right): "
      mapM_ (\(a,b) -> printf "%-6d %4d %8.2f%%\n" b a (percentage b)) counts
      putStrLn ""
      let (_,c0) = counts !! 0
          (_,c1) = counts !! 1
          cn = numVars - c0 -c1
      putStrLn $ printf "# %d %d (%.1f%%) %d (%.1f%%) %d (%.1f%%)\n" numVars c0 (percentage c0) c1 (percentage c1) cn (percentage cn)

analyseDelays :: UPLC.Term UPLC.Name uni fun ann -> IO ()
analyseDelays term =
    let isValue = \case
                  UPLC.Var      {} -> True
                  UPLC.LamAbs   {} -> True
                  UPLC.Apply    {} -> False
                  UPLC.Force    {} -> False
                  UPLC.Delay    {} -> True
                  UPLC.Constant {} -> True
                  UPLC.Builtin  {} -> True
                  UPLC.Error    {} -> False
        doDelays tm (counts@(total1,values1)) =
            case tm of
              UPLC.Var _ _       -> counts
              UPLC.LamAbs _ _ t  -> doDelays t counts
              UPLC.Apply _ t1 t2 -> doDelays t2 (doDelays t1 counts)
              UPLC.Force _ t     -> doDelays t counts
              UPLC.Delay _ t     -> if isValue t then doDelays t (total1+1, values1+1) else doDelays t (total1+1, values1)
              UPLC.Constant {}   -> counts
              UPLC.Builtin {}    -> counts
              UPLC.Error _       -> counts
        (total, values) = doDelays term (0::Int,0::Int)
    in do
      putStrLn $ printf "Total = %d, values = %d, other = %d" total values (total-values)



runAnalysis :: AnalysisOptions -> IO ()
runAnalysis (AnalysisOptions inp ifmt) = do
  program <- (getProgram ifmt inp :: IO (UplcProg PLC.AlexPosn))
--  analyseBindings (UPLC.toTerm program)
  analyseDelays (UPLC.toTerm program)

main :: IO ()
main = do
    options <- customExecParser (prefs showHelpOnEmpty) uplcInfoCommand
    case options of
        Apply     opts -> runApply        opts
        Eval      opts -> runEval         opts
        Example   opts -> runUplcPrintExample opts
        Print     opts -> runPrint        opts
        Convert   opts -> runConvert      opts
        Analyse   opts -> runAnalysis     opts
