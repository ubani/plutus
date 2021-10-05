{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

{-| = ACTUS Analysis

Given an ACTUS contract cashflows can be projected.

-}

module Language.Marlowe.ACTUS.Analysis
  ( genProjectedCashflows )
where

import           Control.Applicative                                        ((<|>))
import qualified Data.List                                                  as L (groupBy)
import           Data.Maybe                                                 (fromMaybe, isNothing)
import           Data.Sort                                                  (sortOn)
import           Data.Time                                                  (Day)
import           Debug.Trace
import           Language.Marlowe.ACTUS.Definitions.BusinessEvents          (EventType (..), RiskFactors)
import           Language.Marlowe.ACTUS.Definitions.ContractState           (ContractState)
import           Language.Marlowe.ACTUS.Definitions.ContractTerms           (CT (..), ContractTerms (..))
import           Language.Marlowe.ACTUS.Definitions.Schedule                (CashFlow (..), ShiftedDay (..),
                                                                             calculationDay, paymentDay)
import           Language.Marlowe.ACTUS.Model.INIT.StateInitializationModel (initialize)
import           Language.Marlowe.ACTUS.Model.POF.Payoff                    (payoff)
import           Language.Marlowe.ACTUS.Model.SCHED.ContractSchedule        (maturity, schedule)
import           Language.Marlowe.ACTUS.Model.STF.StateTransition           (stateTransition)

-- |genProjectedCashflows generates a list of projected cashflows for
-- given contract terms together with the observed data
genProjectedCashflows :: (EventType -> Day -> RiskFactors) -> ContractTerms -> [CashFlow]
genProjectedCashflows getRiskFactors ct@ContractTerms {..} = fromMaybe [] $
  do
    st0 <- initialize ct
    return $
      let -- schedule
          scheduleEvent e = maybe [] (fmap (e,)) (schedule e ct)

          -- events
          eventTypes = [IED, MD, RR, RRF, IP, PR, PRF, IPCB, IPCI, PRD, TD, SC]

          events =
            let e = concatMap scheduleEvent eventTypes
             in trace (show $ filter filtersEvents . postProcessSchedule . sortOn (paymentDay . snd) $ e) filter filtersEvents . postProcessSchedule . sortOn (paymentDay . snd) $ e

          -- states
          applyStateTransition (states, terms@ContractTerms{ct_ARPRNXTj = arprnxt, ct_ARRATE = arrate}) (ev', date') =
            let (st, ev, date) = last (trace (show states) states)
                t = calculationDay date
                rf = getRiskFactors ev t
                terms' | elem ev' [PI, PR]  = terms {ct_ARPRNXTj = tail <$> arprnxt}
                       | elem ev' [RR, RRF] = terms {ct_ARRATE = tail <$> arrate}
                       | otherwise = terms

                states' = states ++ [(stateTransition ev rf ct st t, ev', date')]
             in (states', terms')

          states =
            let initialState = (st0, AD, ShiftedDay ct_SD ct_SD)
                (states', _)      = foldl applyStateTransition ([initialState], ct) (trace (show events) events)
             in filter filtersStates . tail $ states'

          -- payoff
          calculatePayoff (payoffs, terms@ContractTerms{ct_ARPRNXTj = arprnxt}) (st, ev, date) =
            let t = calculationDay date
                rf = getRiskFactors ev t
                terms' | elem ev [PI, PR] = terms {ct_ARPRNXTj = tail <$> arprnxt}
                       | otherwise = terms
                pof = payoff ev rf terms st t
             in (payoffs ++ [pof], terms')

          (payoffs, _) = foldl calculatePayoff ([], ct) states

          genCashflow ((_, ev, d), pff) =
            CashFlow
              { tick = 0,
                cashContractId = "0",
                cashParty = "party",
                cashCounterParty = "counterparty",
                cashPaymentDay = paymentDay d,
                cashCalculationDay = calculationDay d,
                cashEvent = ev,
                amount = pff,
                currency = "ada"
              }
       in sortOn cashPaymentDay $ genCashflow <$> zip states payoffs
  where
    filtersEvents :: (EventType, ShiftedDay) -> Bool
    filtersEvents (_, ShiftedDay {..}) = isNothing ct_TD || Just calculationDay <= ct_TD

    filtersStates :: (ContractState, EventType, ShiftedDay) -> Bool
    filtersStates (_, ev, ShiftedDay {..}) =
      case contractType of
        PAM -> isNothing ct_PRD || Just calculationDay >= ct_PRD
        LAM -> isNothing ct_PRD || ev == PRD || Just calculationDay > ct_PRD
        NAM -> isNothing ct_PRD || ev == PRD || Just calculationDay > ct_PRD
        ANN ->
          let b1 = isNothing ct_PRD || ev == PRD || Just calculationDay > ct_PRD
              b2 = let m = ct_MD <|> ct_AD <|> maturity ct in isNothing m || Just calculationDay <= m
           in b1 && b2
        LAX -> True

    postProcessSchedule :: [(EventType, ShiftedDay)] -> [(EventType, ShiftedDay)]
    postProcessSchedule =
      let trim = dropWhile (\(_, d) -> calculationDay d < ct_SD)

          priority :: (EventType, ShiftedDay) -> Int
          priority (event, _) = fromEnum event

          similarity (_, l) (_, r) = calculationDay l == calculationDay r
          regroup = L.groupBy similarity

          overwrite = map (sortOn priority) . regroup
       in concat . overwrite . trim
