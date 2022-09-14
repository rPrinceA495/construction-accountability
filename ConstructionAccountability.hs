import Control.Monad (void)
import Data.ByteString.Char8 qualified as C
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Ledger (Address, Datum (Datum), ScriptContext, Validator, Value)
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Constraints qualified as Constraints
import Ledger.Tx (ChainIndexTxOut (..))
import Ledger.Typed.Scripts qualified as Scripts
import Playground.Contract
import Plutus.Contract
import PlutusTx qualified
import PlutusTx.Prelude hiding (pure, (<$>))
import Prelude qualified as Haskell

------------------------------------------------------------

newtype LockDatum = LockDatum Bool deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

PlutusTx.makeLift ''LockDatum

newtype QualityAssuranceRedeemer = QualityAssuranceRedeemer Bool deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

PlutusTx.makeLift ''QualityAssuranceRedeemer

type ConstructionAccountabilitySchema =
        Endpoint "lock" LockParams
        .\/ Endpoint "assessment" AssessmentParams

data Project
instance Scripts.ValidatorTypes Project where
    type instance RedeemerType Project = QualityAssuranceRedeemer
    type instance DatumType Project = LockDatum

constructionAccountabilityInstance :: Scripts.TypedValidator Project
constructionAccountabilityInstance = Scripts.mkTypedValidator @Project
    $$(PlutusTx.compile [|| validateConformance ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator @LockDatum @QualityAssuranceRedeemer

-- | The validation function (Datum -> Redeemer -> ScriptContext -> Bool) * VALIDATOR SCRIPT * 
validateConformance :: LockDatum -> QualityAssuranceRedeemer -> ScriptContext -> Bool
validateConformance (LockDatum compliance) (QualityAssuranceRedeemer assessment) _ = compliance == assessment

-- | The validator script
complianceValidator :: Validator
complianceValidator = Scripts.validatorScript constructionAccountabilityInstance

-- | The address of the account (the hash of its validator script)
accountAddress :: Address
accountAddress = Ledger.scriptAddress complianceValidator

-- | Parameters for the "lock" endpoint
data LockParams = LockParams
    { compliance :: Bool
    , amount     :: Value
    }
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

--  | Parameters for the "assessment" endpoint
newtype AssessmentParams = AssessmentParams
    { projectAssessment :: Bool
    }
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

-- | The "lock" contract endpoint. See note [Contract endpoints]
lock :: AsContractError e => Promise () ConstructionAccountabilitySchema e ()
lock = endpoint @"lock" @LockParams $ \(LockParams compliance amount) -> do
    let tx         = Constraints.mustPayToTheScript (LockDatum compliance) amount
    void (submitTxConstraints constructionAccountabilityInstance tx)

-- | The "assessment" contract endpoint. See note [Contract endpoints]
assessment :: AsContractError e => Promise () ConstructionAccountabilitySchema e ()
assessment = endpoint @"assessment" @AssessmentParams $ \(AssessmentParams projectAssessment) -> do
    -- Wait for script to have a UTxO of a least 1 lovelace
    logInfo @Haskell.String "Waiting for script to have a UTxO of at least 1 lovelace"
    utxos <- fundsAtAddressGeq accountAddress (Ada.lovelaceValueOf 1)

    let redeemer = QualityAssuranceRedeemer projectAssessment
        tx       = collectFromScript utxos redeemer

    -- This is only for test purposes to have a possible failing transaction.
    -- In a real use-case, we would not submit the transaction if the assessment is
    -- false.
    logInfo @Haskell.String "Submitting transaction to assess project"
    void (submitTxConstraintsSpending constructionAccountabilityInstance utxos tx)

constructionAccountability :: AsContractError e => Contract () ConstructionAccountabilitySchema e ()
constructionAccountability = do
    logInfo @Haskell.String "Waiting for assessment or lock endpoint..."
    selectList [lock, assessment]

{- Note [Contract endpoints]

A contract endpoint is a function that uses the wallet API to interact with the
blockchain. We can look at contract endpoints from two different points of view.

1. Contract users

Contract endpoints are the visible interface of the contract. They provide a
UI (HTML form) for entering the parameters of the actions we may take as part
of the contract.

2. Contract authors

As contract authors we define endpoints as functions that return a value of
type 'MockWallet ()'. This type indicates that the function uses the wallet API
to produce and spend transaction outputs on the blockchain.

Endpoints can have any number of parameters: 'lock' has two
parameters, 'assessment' has one. For each endpoint we
include a call to 'mkFunction' at the end of the contract definition. This
causes the Haskell compiler to generate a schema for the endpoint. The Plutus
Playground then uses this schema to present an HTML form to the user where the
parameters can be entered.

-}

endpoints :: AsContractError e => Contract () ConstructionAccountabilitySchema e ()
endpoints = constructionAccountability

mkSchemaDefinitions ''ConstructionAccountabilitySchema

$(mkKnownCurrencies [])
