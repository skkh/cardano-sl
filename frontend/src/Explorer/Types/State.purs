module Explorer.Types.State where

import Data.Generic (class Generic, gShow)
import Data.Maybe (Maybe)
import Explorer.I18n.Lang (Language)
import Explorer.Routes (Route)
import Pos.Explorer.Web.ClientTypes (CAddressSummary, CBlockEntry, CBlockSummary, CTxEntry, CTxSummary, CTxBrief)
import Prelude (class Eq, class Ord, class Show)

-- Add all State types here to generate lenses from it

type State =
    { lang :: Language
    , route :: Route
    , socket :: SocketState
    , viewStates :: ViewStates
    , latestBlocks :: CBlockEntries
    , initialBlocksRequested :: Boolean
    , handleLatestBlocksSocketResult :: Boolean
    , initialTxsRequested :: Boolean
    , handleLatestTxsSocketResult :: Boolean
    , currentBlockSummary :: Maybe CBlockSummary
    , currentBlockTxs :: Maybe CTxBriefs
    , currentTxSummary :: Maybe CTxSummary
    , latestTransactions :: CTxEntries
    , currentAddressSummary :: Maybe CAddressSummary
    , selectedSearch :: Search
    , errors :: Errors
    , loading :: Boolean
    }

data Search
    = SearchAddress
    | SearchTx
    | SearchEpoch

derive instance gSearch :: Generic Search
instance showSearch :: Show Search where
    show = gShow
derive instance eqSearch :: Eq Search

type SocketState =
    { connected :: Boolean
    }

data DashboardAPICode = Curl | Node | JQuery
derive instance eqDashboardAPICode :: Eq DashboardAPICode
derive instance ordDashboardAPICode :: Ord DashboardAPICode

type CBlockEntries = Array CBlockEntry
type CTxEntries = Array CTxEntry
type CTxBriefs = Array CTxBrief

type Errors = Array String

type ViewStates =
    { dashboard :: DashboardViewState
    , addressDetail :: AddressDetailViewState
    , blockDetail :: BlockDetailViewState
    }

type DashboardViewState =
    { blocksExpanded :: Boolean
    , dashboardBlockPagination :: Int
    , transactionsExpanded :: Boolean
    , selectedApiCode :: DashboardAPICode
    , searchInput :: Boolean
    }

type BlockDetailViewState =
    { blockTxPagination :: Int
    }

type AddressDetailViewState =
    { addressTxPagination :: Int
    }

-- TODO (jk) CCurrency should be generated by purescript-bridge later
data CCurrency
    = ADA
    | BTC
    | USD
