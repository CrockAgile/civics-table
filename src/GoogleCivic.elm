port module GoogleCivic
    exposing
        ( OpenCivicBoundary
        , stringToOpenCivicBoundary
        , OpenCivicDataId
        , decodeOpenCivicDataId
        )

import Json.Decode exposing (string, list, Decoder, succeed, fail, map)
import Json.Decode.Pipeline exposing (decode, required, resolve)
import List exposing (head, tail)
import Regex


type alias OpenCivicBoundary =
    { kind : String
    , identifier : String
    }


stringToOpenCivicBoundary : String -> Result String OpenCivicBoundary
stringToOpenCivicBoundary boundaryString =
    let
        splitBoundary =
            String.split ":" boundaryString
    in
        case splitBoundary of
            [ kind, identifier ] ->
                if List.any String.isEmpty splitBoundary then
                    Err ("OpenCivic boundary has empty field: " ++ boundaryString)
                else
                    Ok (OpenCivicBoundary kind identifier)

            _ ->
                Err ("OpenCivic boundary has invalid format: " ++ boundaryString)


type alias OpenCivicDataId =
    { countryCode : String
    , boundaries : List OpenCivicBoundary
    }


decodeOpenCivicDataId : Decoder OpenCivicDataId
decodeOpenCivicDataId =
    let
        -- ocd-division/country:us/state:or/circuit_court:4"
        decodeId idString =
            let
                idPrefix =
                    "ocd-division/country:"

                idPrefixLength =
                    String.length idPrefix

                idRegex =
                    Regex.regex ("^" ++ idPrefix ++ "\\w{2}(/\\w+:\\w+)*$")

                idMatch =
                    Regex.find (Regex.AtMost 1) idRegex idString
                        |> List.head
            in
                case idMatch of
                    Nothing ->
                        fail ("OpenCivic identifier has invalid format: " ++ idString)

                    Just match ->
                        let
                            countryCode =
                                String.slice idPrefixLength (idPrefixLength + 2) idString

                            boundariesResult =
                                idString
                                    |> String.slice (idPrefixLength + 3) (String.length idString)
                                    |> String.split "/"
                                    |> List.filter (not << String.isEmpty)
                                    |> List.map stringToOpenCivicBoundary
                                    |> List.foldr (Result.map2 (::)) (Ok [])
                        in
                            case boundariesResult of
                                Err error ->
                                    fail error

                                Ok boundaries ->
                                    succeed { countryCode = countryCode, boundaries = boundaries }
    in
        string |> Json.Decode.andThen decodeId


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
