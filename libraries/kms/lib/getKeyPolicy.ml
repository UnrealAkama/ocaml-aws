open Types
open Aws
type input = GetKeyPolicyRequest.t
type output = GetKeyPolicyResponse.t
type error = Errors_internal.t
let service = "kms"
let to_http service region req =
  let uri =
    Uri.add_query_params
      (Uri.of_string
         (Aws.Util.of_option_exn (Endpoints.url_of service region)))
      (List.append
         [("Version", ["2014-11-01"]); ("Action", ["GetKeyPolicy"])]
         (Util.drop_empty
            (Uri.query_of_encoded
               (Query.render (GetKeyPolicyRequest.to_query req))))) in
  (`POST, uri, [])
let of_http body =
  try
    let xml = Ezxmlm.from_string body in
    let resp = Xml.member "GetKeyPolicyResponse" (snd xml) in
    try
      Util.or_error (Util.option_bind resp GetKeyPolicyResponse.parse)
        (let open Error in
           BadResponse
             {
               body;
               message = "Could not find well formed GetKeyPolicyResponse."
             })
    with
    | Xml.RequiredFieldMissing msg ->
        let open Error in
          `Error
            (BadResponse
               {
                 body;
                 message =
                   ("Error parsing GetKeyPolicyResponse - missing field in body or children: "
                      ^ msg)
               })
  with
  | Failure msg ->
      `Error
        (let open Error in
           BadResponse { body; message = ("Error parsing xml: " ^ msg) })
let parse_error code err =
  let errors =
    [Errors_internal.KMSInternal;
    Errors_internal.DependencyTimeout;
    Errors_internal.InvalidArn;
    Errors_internal.NotFound] @ Errors_internal.common in
  match Errors_internal.of_string err with
  | Some var ->
      if
        (List.mem var errors) &&
          ((match Errors_internal.to_http_code var with
            | Some var -> var = code
            | None -> true))
      then Some var
      else None
  | None -> None