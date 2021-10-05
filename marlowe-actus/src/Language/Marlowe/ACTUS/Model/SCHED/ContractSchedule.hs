{-# LANGUAGE RecordWildCards #-}

{-| = ACTUS contract schedules

The implementation is a transliteration of the ACTUS specification v1.1

-}

module Language.Marlowe.ACTUS.Model.SCHED.ContractSchedule
  ( schedule
  , maturity
  )
where

import           Control.Applicative                                      (Alternative ((<|>)))
import           Data.Ord                                                 (Down (..))
import           Data.Sort                                                (sortOn)
import           Data.Time                                                (Day)
import           Language.Marlowe.ACTUS.Definitions.BusinessEvents        (EventType (..))
import           Language.Marlowe.ACTUS.Definitions.ContractTerms         (ARINCDEC (..), CT (..), ContractTerms (..),
                                                                           Cycle (..), ScheduleConfig (..))
import           Language.Marlowe.ACTUS.Definitions.Schedule              (ShiftedDay (..))
import           Language.Marlowe.ACTUS.Model.SCHED.ContractScheduleModel
import           Language.Marlowe.ACTUS.Model.Utility.DateShift           (applyBDCWithCfg)
import           Language.Marlowe.ACTUS.Model.Utility.ScheduleGenerator   (applyEOMC,
                                                                           generateRecurrentScheduleWithCorrections,
                                                                           (<+>), (<->))
import           Language.Marlowe.ACTUS.Model.Utility.YearFraction        (yearFraction)
import           Language.Marlowe.ACTUS.Ops                               (YearFractionOps (_y))

schedule :: EventType -> ContractTerms -> Maybe [ShiftedDay]
schedule ev c = schedule' ev c { ct_MD = maturity c }
  where

    schedule' :: EventType -> ContractTerms -> Maybe [ShiftedDay]
    schedule' IED  ct@ContractTerms{ contractType = PAM } = _SCHED_IED_PAM ct
    schedule' MD   ct@ContractTerms{ contractType = PAM } = _SCHED_MD_PAM ct
    schedule' PP   ct@ContractTerms{ contractType = PAM } = _SCHED_PP_PAM ct
    schedule' PY   ct@ContractTerms{ contractType = PAM } = _SCHED_PY_PAM ct
    schedule' FP   ct@ContractTerms{ contractType = PAM } = _SCHED_FP_PAM ct
    schedule' PRD  ct@ContractTerms{ contractType = PAM } = _SCHED_PRD_PAM ct
    schedule' TD   ct@ContractTerms{ contractType = PAM } = _SCHED_TD_PAM ct
    schedule' IP   ct@ContractTerms{ contractType = PAM } = _SCHED_IP_PAM ct
    schedule' IPCI ct@ContractTerms{ contractType = PAM } = _SCHED_IPCI_PAM ct
    schedule' RR   ct@ContractTerms{ contractType = PAM } = _SCHED_RR_PAM ct
    schedule' RRF  ct@ContractTerms{ contractType = PAM } = _SCHED_RRF_PAM ct
    schedule' SC   ct@ContractTerms{ contractType = PAM } = _SCHED_SC_PAM ct

    schedule' IED  ct@ContractTerms{ contractType = LAM } = _SCHED_IED_PAM ct
    schedule' PR   ct@ContractTerms{ contractType = LAM } = _SCHED_PR_LAM ct
    schedule' MD   ct@ContractTerms{ contractType = LAM } = _SCHED_MD_LAM ct
    schedule' PP   ct@ContractTerms{ contractType = LAM } = _SCHED_PP_PAM ct
    schedule' PY   ct@ContractTerms{ contractType = LAM } = _SCHED_PY_PAM ct
    schedule' FP   ct@ContractTerms{ contractType = LAM } = _SCHED_FP_PAM ct
    schedule' PRD  ct@ContractTerms{ contractType = LAM } = _SCHED_PRD_PAM ct
    schedule' TD   ct@ContractTerms{ contractType = LAM } = _SCHED_TD_PAM ct
    schedule' IP   ct@ContractTerms{ contractType = LAM } = _SCHED_IP_PAM ct
    schedule' IPCI ct@ContractTerms{ contractType = LAM } = _SCHED_IPCI_PAM ct
    schedule' IPCB ct@ContractTerms{ contractType = LAM } = _SCHED_IPCB_LAM ct
    schedule' RR   ct@ContractTerms{ contractType = LAM } = _SCHED_RR_PAM ct
    schedule' RRF  ct@ContractTerms{ contractType = LAM } = _SCHED_RRF_PAM ct
    schedule' SC   ct@ContractTerms{ contractType = LAM } = _SCHED_SC_PAM ct

    schedule' IED  ct@ContractTerms{ contractType = NAM } = _SCHED_IED_PAM ct
    schedule' PR   ct@ContractTerms{ contractType = NAM } = _SCHED_PR_LAM ct
    schedule' MD   ct@ContractTerms{ contractType = NAM } = _SCHED_MD_PAM ct
    schedule' PP   ct@ContractTerms{ contractType = NAM } = _SCHED_PP_PAM ct
    schedule' PY   ct@ContractTerms{ contractType = NAM } = _SCHED_PY_PAM ct
    schedule' FP   ct@ContractTerms{ contractType = NAM } = _SCHED_FP_PAM ct
    schedule' PRD  ct@ContractTerms{ contractType = NAM } = _SCHED_PRD_PAM ct
    schedule' TD   ct@ContractTerms{ contractType = NAM } = _SCHED_TD_PAM ct
    schedule' IP   ct@ContractTerms{ contractType = NAM } = _SCHED_IP_NAM ct
    schedule' IPCI ct@ContractTerms{ contractType = NAM } = _SCHED_IPCI_NAM ct
    schedule' IPCB ct@ContractTerms{ contractType = NAM } = _SCHED_IPCB_LAM ct
    schedule' RR   ct@ContractTerms{ contractType = NAM } = _SCHED_RR_PAM ct
    schedule' RRF  ct@ContractTerms{ contractType = NAM } = _SCHED_RRF_PAM ct
    schedule' SC   ct@ContractTerms{ contractType = NAM } = _SCHED_SC_PAM ct

    schedule' IED  ct@ContractTerms{ contractType = ANN } = _SCHED_IED_PAM ct
    schedule' PR   ct@ContractTerms{ contractType = ANN } = _SCHED_PR_LAM ct
    schedule' MD   ct@ContractTerms{ contractType = ANN } = _SCHED_MD_PAM c { ct_MD = ct_MD c <|> ct_MD ct }
    schedule' PP   ct@ContractTerms{ contractType = ANN } = _SCHED_PP_PAM ct
    schedule' PY   ct@ContractTerms{ contractType = ANN } = _SCHED_PY_PAM ct
    schedule' FP   ct@ContractTerms{ contractType = ANN } = _SCHED_FP_PAM ct
    schedule' PRD  ct@ContractTerms{ contractType = ANN } = _SCHED_PRD_PAM ct
    schedule' TD   ct@ContractTerms{ contractType = ANN } = _SCHED_TD_PAM ct
    schedule' IP   ct@ContractTerms{ contractType = ANN } = _SCHED_IP_NAM c { ct_MD = ct_MD c <|> ct_MD ct }
    schedule' IPCI ct@ContractTerms{ contractType = ANN } = _SCHED_IPCI_PAM ct
    schedule' IPCB ct@ContractTerms{ contractType = ANN } = _SCHED_IPCB_LAM ct
    schedule' RR   ct@ContractTerms{ contractType = ANN } = _SCHED_RR_PAM ct
    schedule' RRF  ct@ContractTerms{ contractType = ANN } = _SCHED_RRF_PAM ct
    schedule' SC   ct@ContractTerms{ contractType = ANN } = _SCHED_SC_PAM ct
    schedule' PRF  ct@ContractTerms{ contractType = ANN } = _SCHED_PRF_ANN ct

    schedule' IED  ct@ContractTerms{ contractType = LAX } = _SCHED_IED_PAM ct
    schedule' MD   ct@ContractTerms{ contractType = LAX } = _SCHED_MD_PAM ct
    schedule' PR   ct@ContractTerms{ contractType = LAX } = _SCHED_PR_PI_LAX ct PR
    schedule' PI   ct@ContractTerms{ contractType = LAX } = _SCHED_PR_PI_LAX ct PI
    schedule' PP   ct@ContractTerms{ contractType = LAX } = _SCHED_PP_PAM ct
    schedule' PY   ct@ContractTerms{ contractType = LAX } = _SCHED_PY_PAM ct
    schedule' FP   ct@ContractTerms{ contractType = LAX } = _SCHED_FP_PAM ct
    schedule' PRD  ct@ContractTerms{ contractType = LAX } = _SCHED_PRD_PAM ct
    schedule' TD   ct@ContractTerms{ contractType = LAX } = _SCHED_TD_PAM ct
    schedule' IP   ct@ContractTerms{ contractType = LAX } = _SCHED_IP_LAX ct
    schedule' IPCI ct@ContractTerms{ contractType = LAX } = _SCHED_IPCI_PAM ct
    schedule' IPCB ct@ContractTerms{ contractType = LAX } = _SCHED_IPCB_LAM ct
    schedule' RR   ct@ContractTerms{ contractType = LAX } = _SCHED_RR_RRF_LAX ct RR
    schedule' RRF  ct@ContractTerms{ contractType = LAX } = _SCHED_RR_RRF_LAX ct RRF
    schedule' SC   ct@ContractTerms{ contractType = LAX } = _SCHED_SC_PAM ct

    schedule' _ _                                         = Nothing

maturity :: ContractTerms -> Maybe Day
maturity ContractTerms {contractType = PAM, ..} = ct_MD
maturity ContractTerms {contractType = LAM, ct_MD = md@(Just _)} = md
maturity
  ContractTerms
    { contractType = LAM,
      ct_MD = Nothing,
      ct_PRANX = Just principalRedemptionAnchor,
      ct_IPCL = Just interestPaymentCycle,
      ct_PRCL = Just principalRedemptionCycle,
      ct_PRNXT = Just nextPrincipalRedemptionPayment,
      ct_NT = Just notionalPrincipal,
      ct_SD = statusDate,
      scfg = scheduleConfig
    } =
    let (lastEvent, remainingPeriods)
          | principalRedemptionAnchor < statusDate =
            let previousEvents = generateRecurrentScheduleWithCorrections principalRedemptionAnchor principalRedemptionCycle statusDate scheduleConfig
                f1 = (\ShiftedDay {..} -> calculationDay > statusDate <-> interestPaymentCycle)
                f2 = (\ShiftedDay {..} -> calculationDay == statusDate)
                ShiftedDay {calculationDay = lastEventCalcDay} = head . filter f2 . filter f1 $ previousEvents
             in (lastEventCalcDay, notionalPrincipal / nextPrincipalRedemptionPayment)
          | otherwise = (principalRedemptionAnchor, notionalPrincipal / nextPrincipalRedemptionPayment - 1)
        m = lastEvent <+> (principalRedemptionCycle {n = n principalRedemptionCycle * round remainingPeriods :: Integer})
     in eomc scheduleConfig >>= \d -> return $ applyEOMC lastEvent principalRedemptionCycle d m
maturity ContractTerms {contractType = NAM, ct_MD = md@(Just _)} = md
maturity
  ContractTerms
    { contractType = NAM,
      ct_MD = Nothing,
      ct_PRANX = Just principalRedemptionAnchor,
      ct_PRNXT = Just nextPrincipalRedemptionPayment,
      ct_IED = Just initialExchangeDate,
      ct_PRCL = Just principalRedemptionCycle,
      ct_NT = Just notionalPrincipal,
      ct_IPNR = Just nominalInterestRate,
      ct_DCC = Just dayCountConvention,
      ct_SD = statusDate,
      scfg = scheduleConfig
    } =
    let lastEvent
          | principalRedemptionAnchor >= statusDate = principalRedemptionAnchor
          | initialExchangeDate <+> principalRedemptionCycle >= statusDate = initialExchangeDate <+> principalRedemptionCycle
          | otherwise =
            let previousEvents = generateRecurrentScheduleWithCorrections principalRedemptionAnchor principalRedemptionCycle statusDate scheduleConfig
                f = (\ShiftedDay {..} -> calculationDay == statusDate)
                ShiftedDay {calculationDay = lastEventCalcDay} = head . filter f $ previousEvents
             in lastEventCalcDay

        yLastEventPlusPRCL = yearFraction dayCountConvention lastEvent (lastEvent <+> principalRedemptionCycle) Nothing
        redemptionPerCycle = nextPrincipalRedemptionPayment - (yLastEventPlusPRCL * nominalInterestRate * notionalPrincipal)
        remainingPeriods = ceiling (notionalPrincipal / redemptionPerCycle) - 1
        m = lastEvent <+> principalRedemptionCycle {n = n principalRedemptionCycle * remainingPeriods}
     in eomc scheduleConfig >>= \d -> return $ applyEOMC lastEvent principalRedemptionCycle d m
maturity
  ContractTerms
    { contractType = ANN,
      ct_AD = Nothing,
      ct_MD = Nothing,
      ct_PRANX = Just principalRedemptionAnchor,
      ct_PRNXT = Just nextPrincipalRedemptionPayment,
      ct_IED = Just initialExchangeDate,
      ct_PRCL = Just principalRedemptionCycle,
      ct_NT = Just notionalPrincipal,
      ct_IPNR = Just nominalInterestRate,
      ct_DCC = Just dayCountConvention,
      ct_SD = t0,
      scfg = scheduleConfig
    } =
    let tplus = initialExchangeDate <+> principalRedemptionCycle
        lastEvent
          | principalRedemptionAnchor >= t0 = principalRedemptionAnchor
          | tplus >= t0 = tplus
          | otherwise =
            let previousEvents = generateRecurrentScheduleWithCorrections t0 principalRedemptionCycle principalRedemptionAnchor scheduleConfig
             in calculationDay . head . sortOn (Down . calculationDay) . filter (\ShiftedDay {..} -> calculationDay > t0) $ previousEvents
        timeFromLastEventPlusOneCycle = _y dayCountConvention lastEvent (lastEvent <+> principalRedemptionCycle) Nothing
        redemptionPerCycle = nextPrincipalRedemptionPayment - timeFromLastEventPlusOneCycle * nominalInterestRate * notionalPrincipal
        remainingPeriods = (ceiling (notionalPrincipal / redemptionPerCycle) - 1) :: Integer
    in Just . calculationDay . applyBDCWithCfg scheduleConfig $ lastEvent <+> principalRedemptionCycle { n = remainingPeriods }
maturity
  ContractTerms
    { contractType = ANN,
      ct_AD = ad@(Just _)
    } = ad
maturity
  ContractTerms
    { contractType = ANN,
      ct_AD = Nothing,
      ct_MD = md@(Just _)
    } = md
maturity
  ContractTerms
    { contractType = LAX,
      ct_MD = md@(Just _)
    } = md
maturity
  ContractTerms
    { contractType = LAX,
      ct_ARPRCLj = Nothing,
      ct_ARPRANXj = Just arrayCycleAnchorDateOfPrincipalRedemption
    } = Just $ last arrayCycleAnchorDateOfPrincipalRedemption
maturity
  ContractTerms
    { contractType = LAX,
      ct_ARPRCLj = Just arrayCycleOfPrincipalRedemption,
      ct_ARPRANXj = Just arrayCycleAnchorDateOfPrincipalRedemption,
      ct_ARPRNXTj = Just arrayNextPrincipalRedemptionPayment,
      ct_ARINCDEC = Just arrayIncreaseDecrease,
      ct_NT = Just notionalPrincipal,
      ct_SD = statusDate,
      scfg = scheduleConfig
    }
  | length arrayCycleOfPrincipalRedemption > 1 =
      let
        calculateMaturity :: Day -> Double -> Double -> Double -> [Day] -> [Cycle] -> [Double] -> [ARINCDEC] -> Bool -> Day
        calculateMaturity t s index noOfPrEvents (f:k:restARPRANX) (c:l:restARPRCL) (n:u:restARPRNXT) (i:d:restARINCDEC) continue =
          let
            prSched = _S (Just f) ((\c -> c { includeEndDay = False }) <$> Just c) (Just k) (Just scheduleConfig)
            incdecToDouble incdec
              | incdec == INC = 1.0
              | incdec == DEC = -1.0
            noOfPrEvents'
              | (fromIntegral (length prSched) * n * incdecToDouble i) + notionalPrincipal + s >= 0.0 = fromIntegral $ length prSched
              | otherwise = fromIntegral $ floor ((notionalPrincipal + s) / n)
            s' = noOfPrEvents' * incdecToDouble i * n
            (noOfPrEvents'', t', s'')
              | fromIntegral (length arrayCycleAnchorDateOfPrincipalRedemption - 2) == index =
                  let noOfPrEvents'' = abs (fromIntegral (ceiling ((s' + notionalPrincipal) / n)))
                      t' = foldl (\t' _ -> t' <+> l) k [0..noOfPrEvents'' - 1]
                      s'' = s' + (noOfPrEvents'' * incdecToDouble d * u)
                   in (noOfPrEvents'', t', s'')
              | otherwise = (noOfPrEvents', foldl (\t' _ -> t' <+> c) f [0..noOfPrEvents'], s')
         in
          if continue then
            calculateMaturity t' s'' (index + 1) noOfPrEvents' (k:restARPRANX) (l:restARPRCL) (u:restARPRNXT) (d:restARINCDEC) (s + notionalPrincipal > 0)
          else
            t
     in
      let t = calculateMaturity
                statusDate
                0.0
                0.0
                0.0
                arrayCycleAnchorDateOfPrincipalRedemption
                arrayCycleOfPrincipalRedemption
                arrayNextPrincipalRedemptionPayment
                arrayIncreaseDecrease
                True
       in Just $ calculationDay $ applyBDCWithCfg scheduleConfig t
  | otherwise =
      let noOfPrEvents = ceiling (notionalPrincipal / head arrayNextPrincipalRedemptionPayment)
          t = foldl (\t' _ -> t' <+> head arrayCycleOfPrincipalRedemption) (head arrayCycleAnchorDateOfPrincipalRedemption) [0..noOfPrEvents - 1]
       in Just $ calculationDay $ applyBDCWithCfg scheduleConfig t

maturity _ = Nothing
