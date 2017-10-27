module GoogleCivic exposing (..)


type alias OpenCivicDataType =
    { boundary : String
    , id : Int
    }


type alias OpenCivicDataId =
    { country : String
    , countryCode : String
    , types : List OpenCivicDataType
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
    , zip : String
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
