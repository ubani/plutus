module Component.Wallets.View (render) where

import Prologue hiding (div)
import Component.Input as Input
import Component.Label as Label
import Component.WalletId as WalletId
import Component.Wallets.Types.Internal (Action(..), ComponentHTML, Modal(..), State)
import Contacts.Lenses (_walletNickname)
import Contacts.State (walletNicknameError)
import Contacts.Types (WalletDetails, WalletNicknameError, WalletLibrary)
import Contacts.View (walletIdTip)
import Css as Css
import Data.Lens (_1, _2, assign, view)
import Data.List as List
import Data.Map as Map
import Data.Maybe (isNothing)
import Data.Symbol (SProxy(..))
import Data.Tuple.Nested ((/\))
import Halogen (defaultEval, mkComponent, mkEval, raise)
import Halogen.Css (classNames)
import Halogen.Extra (mapSubmodule)
import Halogen.HTML (HTML, a, br_, button, div, div_, h2, hr, iframe, img, main, p, p_, section, slot, span_, text)
import Halogen.HTML.Events.Extra (onClick_)
import Halogen.HTML.Properties (disabled, href, src, title)
import Images (marloweRunLogo)
import InputField.State (dummyState, handleAction) as InputField
import InputField.State (validate)
import InputField.Types (Action, State) as InputField
import InputField.View (renderInput)
import Marlowe.PAB (PlutusAppId)
import Material.Icons (icon, icon_)
import Material.Icons as Icon

-------------------------------------------------------------------------------
-- Screen
-------------------------------------------------------------------------------
render :: forall m. State -> ComponentHTML m
render state =
  main
    [ classNames
        [ "h-full"
        , "overflow-x-hidden"
        , "grid"
        , "gap-8"
        , "grid-rows-1fr-auto-auto-1fr"
        , "lg:grid-rows-1fr-auto-1fr"
        , "lg:grid-cols-1fr-auto-auto-1fr"
        , "bg-background-shape"
        , "bg-right-top"
        , "bg-no-repeat"
        , "p-4"
        ]
    ]
    [ useWalletBox state
    , gettingStartedBox
    ]

useWalletBox :: forall m. State -> ComponentHTML m
useWalletBox { walletLibrary, walletDetails, searchKey } =
  let
    searchKeyOptions =
      { additionalCss: mempty
      , id_: "existingWallet"
      , placeholder: "Choose wallet or paste key"
      , readOnly: false
      , numberFormat: Nothing
      , valueOptions:
          List.toUnfoldable
            $ Map.values
            $ view _walletNickname
            <$> walletLibrary
      , after: Nothing
      , before: Nothing
      }
  in
    section
      [ classNames [ "row-start-2", "lg:col-start-2", "bg-white", "rounded-lg", "shadow-lg", "p-8", "lg:p-12", "max-w-sm", "mx-auto", "lg:max-w-none", "lg:w-welcome-box", "space-y-4" ] ]
      [ div [ classNames [ "p-2 pt-0" ] ]
          [ img
              [ classNames [ "mx-auto", "text-center" ]
              , src marloweRunLogo
              ]
          ]
      , p
          [ classNames [ "text-center" ] ]
          [ text "To begin using the Marlowe Run demo, generate a new demo wallet." ]
      , button
          [ classNames $ Css.primaryButton <> [ "w-full", "text-center" ]
          , onClick_ GenerateWallet
          ]
          [ text "Generate demo wallet" ]
      , a
          [ classNames [ "block", "text-purple", "text-center", "font-semibold" ]
          , onClick_ ExplainWalletGeneration
          ]
          [ text "Why do I need to do this?" ]
      , hr [ classNames [ "max-w-xs", "mx-auto" ] ]
      , p
          [ classNames [ "text-center" ] ]
          [ text "Or select an existing demo wallet from the list or paste in a demo wallet key." ]
      , SearchKeyAction <$> renderInput searchKeyOptions searchKey
      , div
          [ classNames [ "pt-2", "flex", "justify-between" ] ]
          [ a
              [ classNames [ "flex", "font-bold" ]
              , href "https://staging.marlowe-web.iohkdev.io"
              ]
              [ icon_ Icon.Previous
              , text "Back to home page"
              ]
          , a
              [ classNames [ "font-bold" ]
              -- FIXME: add link to documentation
              , href ""
              ]
              [ text "Docs" ]
          ]
      ]

gettingStartedBox :: forall m. ComponentHTML m
gettingStartedBox =
  section
    [ classNames [ "row-start-3", "lg:row-start-2", "lg:col-start-3", "max-w-sm", "mx-auto", "lg:max-w-none", "lg:w-welcome-box", "flex", "flex-col", "justify-center" ] ]
    [ a
        [ classNames [ "text-purple", "text-center", "lg:hidden" ]
        , onClick_ WatchGetStarted
        ]
        [ icon Icon.Play $ Css.bgBlueGradient <> [ "text-3xl", "text-white", "rounded-full" ]
        , br_
        , text "Watch our get started tutorial"
        ]
    , div
        [ classNames [ "hidden", "lg:block", "space-y-6" ] ]
        [ a
            [ classNames [ "block", "relative", "rounded-lg", "shadow-lg", "bg-get-started-thumbnail", "bg-cover", "w-full", "h-welcome-box" ]
            , onClick_ WatchGetStarted
            ]
            [ icon Icon.Play $ Css.bgBlueGradient <> [ "absolute", "bottom-4", "right-4", "text-3xl", "text-white", "rounded-full" ] ]
        , div_
            [ p
                [ classNames [ "font-semibold", "text-lg", "text-center" ] ]
                [ text "New to Marlowe Run?" ]
            , p
                [ classNames [ "text-lg", "text-center" ] ]
                [ text "Watch our get started tutorial" ]
            ]
        ]
    ]

-------------------------------------------------------------------------------
-- Modals
-------------------------------------------------------------------------------
welcomeModal :: forall m. Functor m => State -> ComponentHTML m
welcomeModal state@{ connecting, walletLibrary, modal } = case modal of
  Just (open /\ GetStartedHelp) -> layout Css.videoCard open getStartedHelp
  Just (open /\ GenerateWalletHelp) -> layout Css.card open generateWalletHelp
  Just (open /\ UseNewWallet details) ->
    layout Css.card open
      $ useNewWallet connecting details walletLibrary
  Just (open /\ UseWallet details) -> layout Css.card open $ useWallet connecting details
  Just (open /\ LocalWalletMissing) -> layout Css.card open localWalletMissing
  _ -> layout Css.card false []
  where
  layout classes open contents =
    div [ classNames $ Css.cardOverlay open ]
      [ div [ classNames $ classes open ] contents
      ]

getStartedHelp :: forall m. Array (ComponentHTML m)
getStartedHelp =
  [ a
      [ classNames [ "absolute", "-top-10", "right-0", "lg:-right-10" ]
      , onClick_ CloseModal
      ]
      [ icon Icon.Close [ "text-lg", "rounded-full", "bg-white", "p-2" ] ]
  , div
      [ classNames $ Css.embeddedVideoContainer <> [ "rounded", "overflow-hidden" ] ]
      [ iframe
          [ classNames Css.embeddedVideo
          , src "https://www.youtube.com/embed/PJLtKJJMH0U"
          , title "Get started video"
          ]
      ]
  ]

generateWalletHelp :: forall m. Array (ComponentHTML m)
generateWalletHelp =
  [ div
      [ classNames [ "p-5", "pb-6", "lg:pb-8", "space-y-4" ] ]
      [ h2
          [ classNames [ "font-semibold" ] ]
          [ text "Why generate a demo wallet?" ]
      , p_
          [ text "Demo wallets are used so you can play around with the app and all its incredible features without using your own tokens from your real wallet." ]
      , div
          [ classNames [ "flex" ] ]
          [ button
              [ classNames $ Css.primaryButton <> [ "flex-1" ]
              , onClick_ CloseModal
              ]
              [ text "Got it" ]
          ]
      ]
  ]

localWalletMissing :: forall m. Array (ComponentHTML m)
localWalletMissing =
  [ div
      [ classNames $ Css.card true ]
      [ a
          [ classNames [ "absolute", "top-4", "right-4" ]
          , onClick_ CloseModal
          ]
          [ icon_ Icon.Close ]
      , div [ classNames [ "flex", "font-semibold", "gap-2", "px-5", "py-4", "bg-gray" ] ]
          [ icon Icon.ErrorOutline []
          , span_ [ text "Wallet not found" ]
          ]
      , div
          [ classNames [ "p-5", "pb-6", "lg:pb-8", "space-y-4" ] ]
          [ p_
              [ text "A wallet that you have previously used is no longer available in our demo server. This is probably because the demo server has been updated. (Note that this demo is in continuous development, and data is not preserved between updates.) We recommend that you use the button below to clear your browser's cache for this site and start again." ]
          , div
              [ classNames [ "flex", "justify-center" ] ]
              [ button
                  [ classNames Css.primaryButton
                  , onClick_ ClearCache
                  ]
                  [ text "Clear Cache" ]
              ]
          ]
      ]
  ]

useWallet :: forall m. Boolean -> WalletDetails -> Array (ComponentHTML m)
useWallet chosen details@{ walletNickname } =
  [ a
      [ classNames [ "absolute", "top-4", "right-4" ]
      , onClick_ CloseModal
      ]
      [ icon_ Icon.Close ]
  , renderUseWalletModal
      ("Demo wallet " <> walletNickname)
      true
      ( Input.renderWithChildren
          Input.defaultInput { id = nicknameId, value = walletNickname }
          (\input -> [ input, nicknameLabel ])
      )
      chosen
      details
      CopyWalletId
      ConnectWallet
      CloseModal
  ]

useNewWallet ::
  forall m.
  Functor m =>
  Boolean ->
  WalletDetails ->
  WalletLibrary ->
  Array (ComponentHTML m)
useNewWallet chosen details walletLibrary =
  [ a
      [ classNames [ "absolute", "top-4", "right-4" ]
      , onClick_ CloseModal
      ]
      [ icon_ Icon.Close ]
  , slot useNewWalletSlot unit useNewWalletComponent (Tuple chosen details) Just
  ]
  where
  useNewWalletSlot :: SProxy "useNewWalletSlot"
  useNewWalletSlot = SProxy

  useNewWalletComponent =
    mkComponent
      { initialState:
          \input ->
            let
              state :: InputField.State WalletNicknameError
              state = InputField.dummyState
            in
              input
                $> state
                    { validator = walletNicknameError walletLibrary
                    }
      , render:
          \state ->
            let
              options =
                { additionalCss: mempty
                , id_: nicknameId
                , placeholder: "Give your wallet a nickname"
                , readOnly: false
                , numberFormat: Nothing
                , valueOptions: mempty
                , after: Nothing
                , before: Just nicknameLabel
                }

              inputState = snd state
            in
              renderUseWalletModal
                "Demo wallet generated"
                (isNothing $ validate inputState)
                (Edit <$> renderInput options inputState)
                (fst state)
                details
                Copy
                Choose
                Close
      , eval:
          mkEval
            defaultEval
              { handleAction =
                case _ of
                  Copy id -> raise $ CopyWalletId id
                  Choose id -> raise $ ConnectWallet id
                  Edit action -> mapSubmodule _2 Edit $ InputField.handleAction action
                  ReceiveChosen c -> assign _1 c
                  Close -> raise CloseModal
              , receive = Just <<< ReceiveChosen <<< fst
              }
      }

data UseNewWalletAction
  = Copy PlutusAppId
  | Choose WalletDetails
  | Edit (InputField.Action WalletNicknameError)
  | ReceiveChosen Boolean
  | Close

renderUseWalletModal ::
  forall p action.
  String ->
  Boolean ->
  HTML p action ->
  Boolean ->
  WalletDetails ->
  (PlutusAppId -> action) ->
  (WalletDetails -> action) ->
  action ->
  HTML p action
renderUseWalletModal title canConnect nicknameInput chosen details@{ companionAppId } copyId choose cancel =
  div [ classNames [ "p-5", "lg:p-6", "space-y-4" ] ]
    [ h2
        [ classNames [ "font-bold", "truncate", "w-11/12" ] ]
        [ text title ]
    , nicknameInput
    , div
        [ classNames [] ]
        [ copyId companionAppId
            <$ WalletId.render
                WalletId.defaultInput
                  { label = "Demo wallet ID"
                  , value = companionAppId
                  }
        , walletIdTip
        ]
    , div
        [ classNames [ "flex", "gap-4" ] ]
        [ button
            [ classNames $ Css.secondaryButton <> [ "flex-1" ]
            , onClick_ cancel
            ]
            [ text "Cancel" ]
        , button
            ( [ classNames $ Css.primaryButton <> [ "flex-1" ] ]
                <> if canConnect && not chosen then
                    [ onClick_ $ choose details ]
                  else
                    [ disabled true ]
            )
            [ text if chosen then "Connecting..." else "Connect Wallet" ]
        ]
    ]

nicknameId :: String
nicknameId = "walletNickname"

nicknameLabel :: forall w i. HTML w i
nicknameLabel =
  Label.render
    Label.defaultInput { for = nicknameId, text = "Wallet nickname" }
