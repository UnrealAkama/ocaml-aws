type t =
  | AlreadyExists 
  | AuthFailure 
  | Blocked 
  | DependencyTimeout 
  | Disabled 
  | DryRunOperation 
  | IdempotentParameterMismatch 
  | IncompleteSignature 
  | InternalFailure 
  | InvalidAction 
  | InvalidAliasName 
  | InvalidArn 
  | InvalidCiphertext 
  | InvalidClientTokenId 
  | InvalidGrantToken 
  | InvalidKeyUsage 
  | InvalidMarker 
  | InvalidParameter 
  | InvalidParameterCombination 
  | InvalidParameterValue 
  | InvalidQueryParameter 
  | KMSInternal 
  | KeyUnavailable 
  | LimitExceeded 
  | MalformedPolicyDocument 
  | MalformedQueryString 
  | MissingAction 
  | MissingAuthenticationToken 
  | MissingParameter 
  | NotFound 
  | OptInRequired 
  | PendingVerification 
  | RequestExpired 
  | RequestLimitExceeded 
  | ServiceUnavailable 
  | Throttling 
  | UnauthorizedOperation 
  | UnknownParameter 
  | UnsupportedOperation 
  | UnsupportedProtocol 
  | ValidationError 
  | Uninhabited 
let common =
  [UnsupportedProtocol;
  UnknownParameter;
  UnauthorizedOperation;
  RequestLimitExceeded;
  PendingVerification;
  InvalidParameter;
  IdempotentParameterMismatch;
  DryRunOperation;
  Blocked;
  AuthFailure;
  ValidationError;
  Throttling;
  ServiceUnavailable;
  RequestExpired;
  OptInRequired;
  MissingParameter;
  MissingAuthenticationToken;
  MissingAction;
  MalformedQueryString;
  InvalidQueryParameter;
  InvalidParameterValue;
  InvalidParameterCombination;
  InvalidClientTokenId;
  InvalidAction;
  InternalFailure;
  IncompleteSignature]
let to_http_code e =
  match e with
  | AlreadyExists -> Some 400
  | AuthFailure -> None
  | Blocked -> None
  | DependencyTimeout -> Some 503
  | Disabled -> Some 409
  | DryRunOperation -> None
  | IdempotentParameterMismatch -> None
  | IncompleteSignature -> Some 400
  | InternalFailure -> Some 500
  | InvalidAction -> Some 400
  | InvalidAliasName -> Some 400
  | InvalidArn -> Some 400
  | InvalidCiphertext -> Some 400
  | InvalidClientTokenId -> Some 403
  | InvalidGrantToken -> Some 400
  | InvalidKeyUsage -> Some 400
  | InvalidMarker -> Some 400
  | InvalidParameter -> None
  | InvalidParameterCombination -> Some 400
  | InvalidParameterValue -> Some 400
  | InvalidQueryParameter -> Some 400
  | KMSInternal -> Some 500
  | KeyUnavailable -> Some 500
  | LimitExceeded -> Some 400
  | MalformedPolicyDocument -> Some 400
  | MalformedQueryString -> Some 404
  | MissingAction -> Some 400
  | MissingAuthenticationToken -> Some 403
  | MissingParameter -> Some 400
  | NotFound -> Some 404
  | OptInRequired -> Some 403
  | PendingVerification -> None
  | RequestExpired -> Some 400
  | RequestLimitExceeded -> None
  | ServiceUnavailable -> Some 503
  | Throttling -> Some 400
  | UnauthorizedOperation -> None
  | UnknownParameter -> None
  | UnsupportedOperation -> Some 400
  | UnsupportedProtocol -> None
  | ValidationError -> Some 400
  | Uninhabited -> None
let to_string e =
  match e with
  | AlreadyExists -> "AlreadyExists"
  | AuthFailure -> "AuthFailure"
  | Blocked -> "Blocked"
  | DependencyTimeout -> "DependencyTimeout"
  | Disabled -> "Disabled"
  | DryRunOperation -> "DryRunOperation"
  | IdempotentParameterMismatch -> "IdempotentParameterMismatch"
  | IncompleteSignature -> "IncompleteSignature"
  | InternalFailure -> "InternalFailure"
  | InvalidAction -> "InvalidAction"
  | InvalidAliasName -> "InvalidAliasName"
  | InvalidArn -> "InvalidArn"
  | InvalidCiphertext -> "InvalidCiphertext"
  | InvalidClientTokenId -> "InvalidClientTokenId"
  | InvalidGrantToken -> "InvalidGrantToken"
  | InvalidKeyUsage -> "InvalidKeyUsage"
  | InvalidMarker -> "InvalidMarker"
  | InvalidParameter -> "InvalidParameter"
  | InvalidParameterCombination -> "InvalidParameterCombination"
  | InvalidParameterValue -> "InvalidParameterValue"
  | InvalidQueryParameter -> "InvalidQueryParameter"
  | KMSInternal -> "KMSInternal"
  | KeyUnavailable -> "KeyUnavailable"
  | LimitExceeded -> "LimitExceeded"
  | MalformedPolicyDocument -> "MalformedPolicyDocument"
  | MalformedQueryString -> "MalformedQueryString"
  | MissingAction -> "MissingAction"
  | MissingAuthenticationToken -> "MissingAuthenticationToken"
  | MissingParameter -> "MissingParameter"
  | NotFound -> "NotFound"
  | OptInRequired -> "OptInRequired"
  | PendingVerification -> "PendingVerification"
  | RequestExpired -> "RequestExpired"
  | RequestLimitExceeded -> "RequestLimitExceeded"
  | ServiceUnavailable -> "ServiceUnavailable"
  | Throttling -> "Throttling"
  | UnauthorizedOperation -> "UnauthorizedOperation"
  | UnknownParameter -> "UnknownParameter"
  | UnsupportedOperation -> "UnsupportedOperation"
  | UnsupportedProtocol -> "UnsupportedProtocol"
  | ValidationError -> "ValidationError"
  | Uninhabited -> "Uninhabited"
let of_string e =
  match e with
  | "AlreadyExists" -> Some AlreadyExists
  | "AuthFailure" -> Some AuthFailure
  | "Blocked" -> Some Blocked
  | "DependencyTimeout" -> Some DependencyTimeout
  | "Disabled" -> Some Disabled
  | "DryRunOperation" -> Some DryRunOperation
  | "IdempotentParameterMismatch" -> Some IdempotentParameterMismatch
  | "IncompleteSignature" -> Some IncompleteSignature
  | "InternalFailure" -> Some InternalFailure
  | "InvalidAction" -> Some InvalidAction
  | "InvalidAliasName" -> Some InvalidAliasName
  | "InvalidArn" -> Some InvalidArn
  | "InvalidCiphertext" -> Some InvalidCiphertext
  | "InvalidClientTokenId" -> Some InvalidClientTokenId
  | "InvalidGrantToken" -> Some InvalidGrantToken
  | "InvalidKeyUsage" -> Some InvalidKeyUsage
  | "InvalidMarker" -> Some InvalidMarker
  | "InvalidParameter" -> Some InvalidParameter
  | "InvalidParameterCombination" -> Some InvalidParameterCombination
  | "InvalidParameterValue" -> Some InvalidParameterValue
  | "InvalidQueryParameter" -> Some InvalidQueryParameter
  | "KMSInternal" -> Some KMSInternal
  | "KeyUnavailable" -> Some KeyUnavailable
  | "LimitExceeded" -> Some LimitExceeded
  | "MalformedPolicyDocument" -> Some MalformedPolicyDocument
  | "MalformedQueryString" -> Some MalformedQueryString
  | "MissingAction" -> Some MissingAction
  | "MissingAuthenticationToken" -> Some MissingAuthenticationToken
  | "MissingParameter" -> Some MissingParameter
  | "NotFound" -> Some NotFound
  | "OptInRequired" -> Some OptInRequired
  | "PendingVerification" -> Some PendingVerification
  | "RequestExpired" -> Some RequestExpired
  | "RequestLimitExceeded" -> Some RequestLimitExceeded
  | "ServiceUnavailable" -> Some ServiceUnavailable
  | "Throttling" -> Some Throttling
  | "UnauthorizedOperation" -> Some UnauthorizedOperation
  | "UnknownParameter" -> Some UnknownParameter
  | "UnsupportedOperation" -> Some UnsupportedOperation
  | "UnsupportedProtocol" -> Some UnsupportedProtocol
  | "ValidationError" -> Some ValidationError
  | "Uninhabited" -> Some Uninhabited
  | _ -> None