port module GoogleCivic
    exposing
        ( OpenCivicBoundary
        , decodeOpenCivicBoundary
        )

import Json.Decode exposing (string, Decoder, andThen, succeed, fail)
import Json.Decode.Pipeline exposing (decode, required, resolve)
import List exposing (head, tail)


type alias OpenCivicBoundary =
    { kind : String
    , identifier : String
    }


decodeOpenCivicBoundary : Decoder OpenCivicBoundary
decodeOpenCivicBoundary =
    let
        toSplitBoundary : String -> Decoder OpenCivicBoundary
        toSplitBoundary boundaryString =
            let
                splitBoundary =
                    String.split ":" boundaryString
            in
                case splitBoundary of
                    [ kind, identifier ] ->
                        if List.any String.isEmpty splitBoundary then
                            fail "OpenCivic boundary has invalid format"
                        else
                            succeed (OpenCivicBoundary kind identifier)

                    _ ->
                        fail "OpenCivic boundary has invalid format"
    in
        string |> andThen toSplitBoundary


type alias OpenCivicDataId =
    { country : String
    , countryCode : String
    , types : List OpenCivicBoundary
    }


type alias Division =
    { name : String
    , alsoKnownAs : List (Maybe String)
    , officeIndices : List Int
    }


type OfficeLevels
    = International
    | Country
    | AdministrativeArea1
    | AdministrativeArea2
    | Regional
    | Locality
    | SubLocality1
    | SubLocality2
    | Special


type OfficeRoles
    = HeadOfState
    | HeadOfGovernment
    | DeputyHeadOfGovernment
    | GovernmentOfficer
    | ExecutiveCouncil
    | LegislatorUpperBody
    | LegislatorLowerBody
    | HighestCourtJudge
    | Judge
    | SchoolBoard
    | SpecialPurposeOfficer


type alias OfficeSource =
    { name : String
    , official : Bool
    }


type alias Office =
    { name : String
    , divisionId : OpenCivicDataId
    , levels : List OfficeLevels
    , roles : List OfficeRoles
    , sources : List OfficeSource
    }


type alias Address =
    { city : String
    , line1 : String
    , line2 : Maybe String
    , line3 : Maybe String
    , locationName : String
    , state : String
    , zipCode : String
    }


type MediaChannelType
    = Twitter
    | Facebook
    | GooglePlus
    | YouTube
    | Other String


type alias MediaChannel =
    { id : String
    , kind : String
    }


type alias Official =
    { addresses : List Address
    , channels : List MediaChannel
    , name : String
    , party : String
    , phones : List String
    , photoUrl : String
    , urls : List String
    }
