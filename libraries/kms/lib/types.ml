open Aws
open Aws.BaseTypes
open CalendarLib
type calendar = Calendar.t
module EncryptionContextType =
  struct
    type t = (String.t, String.t) Hashtbl.t
    let make elems () = elems
    let parse xml = None
    let to_query v =
      Query.to_query_hashtbl String.to_string String.to_query v
    let to_json v =
      `Assoc
        (Hashtbl.fold
           (fun k ->
              fun v ->
                fun acc -> ((String.to_string k), (String.to_json v)) :: acc)
           v [])
    let of_json j = Json.to_hashtbl String.of_string String.of_json j
  end
module GrantOperation =
  struct
    type t =
      | Decrypt 
      | Encrypt 
      | GenerateDataKey 
      | GenerateDataKeyWithoutPlaintext 
      | ReEncryptFrom 
      | ReEncryptTo 
      | CreateGrant 
      | RetireGrant 
    let str_to_t =
      [("RetireGrant", RetireGrant);
      ("CreateGrant", CreateGrant);
      ("ReEncryptTo", ReEncryptTo);
      ("ReEncryptFrom", ReEncryptFrom);
      ("GenerateDataKeyWithoutPlaintext", GenerateDataKeyWithoutPlaintext);
      ("GenerateDataKey", GenerateDataKey);
      ("Encrypt", Encrypt);
      ("Decrypt", Decrypt)]
    let t_to_str =
      [(RetireGrant, "RetireGrant");
      (CreateGrant, "CreateGrant");
      (ReEncryptTo, "ReEncryptTo");
      (ReEncryptFrom, "ReEncryptFrom");
      (GenerateDataKeyWithoutPlaintext, "GenerateDataKeyWithoutPlaintext");
      (GenerateDataKey, "GenerateDataKey");
      (Encrypt, "Encrypt");
      (Decrypt, "Decrypt")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module GrantConstraints =
  struct
    type t =
      {
      encryption_context_subset: EncryptionContextType.t option ;
      encryption_context_equals: EncryptionContextType.t option }
    let make ?encryption_context_subset  ?encryption_context_equals  () =
      { encryption_context_subset; encryption_context_equals }
    let parse xml =
      Some
        {
          encryption_context_subset =
            (Util.option_bind (Xml.member "EncryptionContextSubset" xml)
               EncryptionContextType.parse);
          encryption_context_equals =
            (Util.option_bind (Xml.member "EncryptionContextEquals" xml)
               EncryptionContextType.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.encryption_context_equals
              (fun f ->
                 Query.Pair
                   ("EncryptionContextEquals",
                     (EncryptionContextType.to_query f)));
           Util.option_map v.encryption_context_subset
             (fun f ->
                Query.Pair
                  ("EncryptionContextSubset",
                    (EncryptionContextType.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.encryption_context_equals
              (fun f ->
                 ("encryption_context_equals",
                   (EncryptionContextType.to_json f)));
           Util.option_map v.encryption_context_subset
             (fun f ->
                ("encryption_context_subset",
                  (EncryptionContextType.to_json f)))])
    let of_json j =
      {
        encryption_context_subset =
          (Util.option_map (Json.lookup j "encryption_context_subset")
             EncryptionContextType.of_json);
        encryption_context_equals =
          (Util.option_map (Json.lookup j "encryption_context_equals")
             EncryptionContextType.of_json)
      }
  end
module GrantOperationList =
  struct
    type t = GrantOperation.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map GrantOperation.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list GrantOperation.to_query v
    let to_json v = `List (List.map GrantOperation.to_json v)
    let of_json j = Json.to_list GrantOperation.of_json j
  end
module KeyListEntry =
  struct
    type t = {
      key_id: String.t option ;
      key_arn: String.t option }
    let make ?key_id  ?key_arn  () = { key_id; key_arn }
    let parse xml =
      Some
        {
          key_id = (Util.option_bind (Xml.member "KeyId" xml) String.parse);
          key_arn = (Util.option_bind (Xml.member "KeyArn" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.key_arn
              (fun f -> Query.Pair ("KeyArn", (String.to_query f)));
           Util.option_map v.key_id
             (fun f -> Query.Pair ("KeyId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.key_arn
              (fun f -> ("key_arn", (String.to_json f)));
           Util.option_map v.key_id (fun f -> ("key_id", (String.to_json f)))])
    let of_json j =
      {
        key_id = (Util.option_map (Json.lookup j "key_id") String.of_json);
        key_arn = (Util.option_map (Json.lookup j "key_arn") String.of_json)
      }
  end
module KeyUsageType =
  struct
    type t =
      | ENCRYPT_DECRYPT 
    let str_to_t = [("ENCRYPT_DECRYPT", ENCRYPT_DECRYPT)]
    let t_to_str = [(ENCRYPT_DECRYPT, "ENCRYPT_DECRYPT")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module GrantListEntry =
  struct
    type t =
      {
      grant_id: String.t option ;
      grantee_principal: String.t option ;
      retiring_principal: String.t option ;
      issuing_account: String.t option ;
      operations: GrantOperationList.t ;
      constraints: GrantConstraints.t option }
    let make ?grant_id  ?grantee_principal  ?retiring_principal 
      ?issuing_account  ?(operations= [])  ?constraints  () =
      {
        grant_id;
        grantee_principal;
        retiring_principal;
        issuing_account;
        operations;
        constraints
      }
    let parse xml =
      Some
        {
          grant_id =
            (Util.option_bind (Xml.member "GrantId" xml) String.parse);
          grantee_principal =
            (Util.option_bind (Xml.member "GranteePrincipal" xml)
               String.parse);
          retiring_principal =
            (Util.option_bind (Xml.member "RetiringPrincipal" xml)
               String.parse);
          issuing_account =
            (Util.option_bind (Xml.member "IssuingAccount" xml) String.parse);
          operations =
            (Util.of_option []
               (Util.option_bind (Xml.member "Operations" xml)
                  GrantOperationList.parse));
          constraints =
            (Util.option_bind (Xml.member "Constraints" xml)
               GrantConstraints.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.constraints
              (fun f ->
                 Query.Pair ("Constraints", (GrantConstraints.to_query f)));
           Some
             (Query.Pair
                ("Operations.member",
                  (GrantOperationList.to_query v.operations)));
           Util.option_map v.issuing_account
             (fun f -> Query.Pair ("IssuingAccount", (String.to_query f)));
           Util.option_map v.retiring_principal
             (fun f -> Query.Pair ("RetiringPrincipal", (String.to_query f)));
           Util.option_map v.grantee_principal
             (fun f -> Query.Pair ("GranteePrincipal", (String.to_query f)));
           Util.option_map v.grant_id
             (fun f -> Query.Pair ("GrantId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.constraints
              (fun f -> ("constraints", (GrantConstraints.to_json f)));
           Some ("operations", (GrantOperationList.to_json v.operations));
           Util.option_map v.issuing_account
             (fun f -> ("issuing_account", (String.to_json f)));
           Util.option_map v.retiring_principal
             (fun f -> ("retiring_principal", (String.to_json f)));
           Util.option_map v.grantee_principal
             (fun f -> ("grantee_principal", (String.to_json f)));
           Util.option_map v.grant_id
             (fun f -> ("grant_id", (String.to_json f)))])
    let of_json j =
      {
        grant_id =
          (Util.option_map (Json.lookup j "grant_id") String.of_json);
        grantee_principal =
          (Util.option_map (Json.lookup j "grantee_principal") String.of_json);
        retiring_principal =
          (Util.option_map (Json.lookup j "retiring_principal")
             String.of_json);
        issuing_account =
          (Util.option_map (Json.lookup j "issuing_account") String.of_json);
        operations =
          (GrantOperationList.of_json
             (Util.of_option_exn (Json.lookup j "operations")));
        constraints =
          (Util.option_map (Json.lookup j "constraints")
             GrantConstraints.of_json)
      }
  end
module AliasListEntry =
  struct
    type t =
      {
      alias_name: String.t option ;
      alias_arn: String.t option ;
      target_key_id: String.t option }
    let make ?alias_name  ?alias_arn  ?target_key_id  () =
      { alias_name; alias_arn; target_key_id }
    let parse xml =
      Some
        {
          alias_name =
            (Util.option_bind (Xml.member "AliasName" xml) String.parse);
          alias_arn =
            (Util.option_bind (Xml.member "AliasArn" xml) String.parse);
          target_key_id =
            (Util.option_bind (Xml.member "TargetKeyId" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.target_key_id
              (fun f -> Query.Pair ("TargetKeyId", (String.to_query f)));
           Util.option_map v.alias_arn
             (fun f -> Query.Pair ("AliasArn", (String.to_query f)));
           Util.option_map v.alias_name
             (fun f -> Query.Pair ("AliasName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.target_key_id
              (fun f -> ("target_key_id", (String.to_json f)));
           Util.option_map v.alias_arn
             (fun f -> ("alias_arn", (String.to_json f)));
           Util.option_map v.alias_name
             (fun f -> ("alias_name", (String.to_json f)))])
    let of_json j =
      {
        alias_name =
          (Util.option_map (Json.lookup j "alias_name") String.of_json);
        alias_arn =
          (Util.option_map (Json.lookup j "alias_arn") String.of_json);
        target_key_id =
          (Util.option_map (Json.lookup j "target_key_id") String.of_json)
      }
  end
module KeyList =
  struct
    type t = KeyListEntry.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map KeyListEntry.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list KeyListEntry.to_query v
    let to_json v = `List (List.map KeyListEntry.to_json v)
    let of_json j = Json.to_list KeyListEntry.of_json j
  end
module KeyMetadata =
  struct
    type t =
      {
      a_w_s_account_id: String.t option ;
      key_id: String.t ;
      arn: String.t option ;
      creation_date: DateTime.t option ;
      enabled: Boolean.t option ;
      description: String.t option ;
      key_usage: KeyUsageType.t option }
    let make ?a_w_s_account_id  ~key_id  ?arn  ?creation_date  ?enabled 
      ?description  ?key_usage  () =
      {
        a_w_s_account_id;
        key_id;
        arn;
        creation_date;
        enabled;
        description;
        key_usage
      }
    let parse xml =
      Some
        {
          a_w_s_account_id =
            (Util.option_bind (Xml.member "AWSAccountId" xml) String.parse);
          key_id =
            (Xml.required "KeyId"
               (Util.option_bind (Xml.member "KeyId" xml) String.parse));
          arn = (Util.option_bind (Xml.member "Arn" xml) String.parse);
          creation_date =
            (Util.option_bind (Xml.member "CreationDate" xml) DateTime.parse);
          enabled =
            (Util.option_bind (Xml.member "Enabled" xml) Boolean.parse);
          description =
            (Util.option_bind (Xml.member "Description" xml) String.parse);
          key_usage =
            (Util.option_bind (Xml.member "KeyUsage" xml) KeyUsageType.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.key_usage
              (fun f -> Query.Pair ("KeyUsage", (KeyUsageType.to_query f)));
           Util.option_map v.description
             (fun f -> Query.Pair ("Description", (String.to_query f)));
           Util.option_map v.enabled
             (fun f -> Query.Pair ("Enabled", (Boolean.to_query f)));
           Util.option_map v.creation_date
             (fun f -> Query.Pair ("CreationDate", (DateTime.to_query f)));
           Util.option_map v.arn
             (fun f -> Query.Pair ("Arn", (String.to_query f)));
           Some (Query.Pair ("KeyId", (String.to_query v.key_id)));
           Util.option_map v.a_w_s_account_id
             (fun f -> Query.Pair ("AWSAccountId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.key_usage
              (fun f -> ("key_usage", (KeyUsageType.to_json f)));
           Util.option_map v.description
             (fun f -> ("description", (String.to_json f)));
           Util.option_map v.enabled
             (fun f -> ("enabled", (Boolean.to_json f)));
           Util.option_map v.creation_date
             (fun f -> ("creation_date", (DateTime.to_json f)));
           Util.option_map v.arn (fun f -> ("arn", (String.to_json f)));
           Some ("key_id", (String.to_json v.key_id));
           Util.option_map v.a_w_s_account_id
             (fun f -> ("a_w_s_account_id", (String.to_json f)))])
    let of_json j =
      {
        a_w_s_account_id =
          (Util.option_map (Json.lookup j "a_w_s_account_id") String.of_json);
        key_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "key_id")));
        arn = (Util.option_map (Json.lookup j "arn") String.of_json);
        creation_date =
          (Util.option_map (Json.lookup j "creation_date") DateTime.of_json);
        enabled = (Util.option_map (Json.lookup j "enabled") Boolean.of_json);
        description =
          (Util.option_map (Json.lookup j "description") String.of_json);
        key_usage =
          (Util.option_map (Json.lookup j "key_usage") KeyUsageType.of_json)
      }
  end
module GrantTokenList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module DataKeySpec =
  struct
    type t =
      | AES_256 
      | AES_128 
    let str_to_t = [("AES_128", AES_128); ("AES_256", AES_256)]
    let t_to_str = [(AES_128, "AES_128"); (AES_256, "AES_256")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module PolicyNameList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module GrantList =
  struct
    type t = GrantListEntry.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map GrantListEntry.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list GrantListEntry.to_query v
    let to_json v = `List (List.map GrantListEntry.to_json v)
    let of_json j = Json.to_list GrantListEntry.of_json j
  end
module AliasList =
  struct
    type t = AliasListEntry.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map AliasListEntry.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list AliasListEntry.to_query v
    let to_json v = `List (List.map AliasListEntry.to_json v)
    let of_json j = Json.to_list AliasListEntry.of_json j
  end
module GetKeyPolicyResponse =
  struct
    type t = {
      policy: String.t option }
    let make ?policy  () = { policy }
    let parse xml =
      Some
        { policy = (Util.option_bind (Xml.member "Policy" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.policy
              (fun f -> Query.Pair ("Policy", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.policy
              (fun f -> ("policy", (String.to_json f)))])
    let of_json j =
      { policy = (Util.option_map (Json.lookup j "policy") String.of_json) }
  end
module InvalidCiphertextException =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module ListKeysResponse =
  struct
    type t =
      {
      keys: KeyList.t ;
      next_marker: String.t option ;
      truncated: Boolean.t option }
    let make ?(keys= [])  ?next_marker  ?truncated  () =
      { keys; next_marker; truncated }
    let parse xml =
      Some
        {
          keys =
            (Util.of_option []
               (Util.option_bind (Xml.member "Keys" xml) KeyList.parse));
          next_marker =
            (Util.option_bind (Xml.member "NextMarker" xml) String.parse);
          truncated =
            (Util.option_bind (Xml.member "Truncated" xml) Boolean.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.truncated
              (fun f -> Query.Pair ("Truncated", (Boolean.to_query f)));
           Util.option_map v.next_marker
             (fun f -> Query.Pair ("NextMarker", (String.to_query f)));
           Some (Query.Pair ("Keys.member", (KeyList.to_query v.keys)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.truncated
              (fun f -> ("truncated", (Boolean.to_json f)));
           Util.option_map v.next_marker
             (fun f -> ("next_marker", (String.to_json f)));
           Some ("keys", (KeyList.to_json v.keys))])
    let of_json j =
      {
        keys = (KeyList.of_json (Util.of_option_exn (Json.lookup j "keys")));
        next_marker =
          (Util.option_map (Json.lookup j "next_marker") String.of_json);
        truncated =
          (Util.option_map (Json.lookup j "truncated") Boolean.of_json)
      }
  end
module DescribeKeyResponse =
  struct
    type t = {
      key_metadata: KeyMetadata.t option }
    let make ?key_metadata  () = { key_metadata }
    let parse xml =
      Some
        {
          key_metadata =
            (Util.option_bind (Xml.member "KeyMetadata" xml)
               KeyMetadata.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.key_metadata
              (fun f -> Query.Pair ("KeyMetadata", (KeyMetadata.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.key_metadata
              (fun f -> ("key_metadata", (KeyMetadata.to_json f)))])
    let of_json j =
      {
        key_metadata =
          (Util.option_map (Json.lookup j "key_metadata") KeyMetadata.of_json)
      }
  end
module EncryptRequest =
  struct
    type t =
      {
      key_id: String.t ;
      plaintext: Blob.t ;
      encryption_context: EncryptionContextType.t option ;
      grant_tokens: GrantTokenList.t }
    let make ~key_id  ~plaintext  ?encryption_context  ?(grant_tokens= []) 
      () = { key_id; plaintext; encryption_context; grant_tokens }
    let parse xml =
      Some
        {
          key_id =
            (Xml.required "KeyId"
               (Util.option_bind (Xml.member "KeyId" xml) String.parse));
          plaintext =
            (Xml.required "Plaintext"
               (Util.option_bind (Xml.member "Plaintext" xml) Blob.parse));
          encryption_context =
            (Util.option_bind (Xml.member "EncryptionContext" xml)
               EncryptionContextType.parse);
          grant_tokens =
            (Util.of_option []
               (Util.option_bind (Xml.member "GrantTokens" xml)
                  GrantTokenList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("GrantTokens.member",
                   (GrantTokenList.to_query v.grant_tokens)));
           Util.option_map v.encryption_context
             (fun f ->
                Query.Pair
                  ("EncryptionContext", (EncryptionContextType.to_query f)));
           Some (Query.Pair ("Plaintext", (Blob.to_query v.plaintext)));
           Some (Query.Pair ("KeyId", (String.to_query v.key_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("grant_tokens", (GrantTokenList.to_json v.grant_tokens));
           Util.option_map v.encryption_context
             (fun f ->
                ("encryption_context", (EncryptionContextType.to_json f)));
           Some ("plaintext", (Blob.to_json v.plaintext));
           Some ("key_id", (String.to_json v.key_id))])
    let of_json j =
      {
        key_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "key_id")));
        plaintext =
          (Blob.of_json (Util.of_option_exn (Json.lookup j "plaintext")));
        encryption_context =
          (Util.option_map (Json.lookup j "encryption_context")
             EncryptionContextType.of_json);
        grant_tokens =
          (GrantTokenList.of_json
             (Util.of_option_exn (Json.lookup j "grant_tokens")))
      }
  end
module RetireGrantRequest =
  struct
    type t =
      {
      grant_token: String.t option ;
      key_id: String.t option ;
      grant_id: String.t option }
    let make ?grant_token  ?key_id  ?grant_id  () =
      { grant_token; key_id; grant_id }
    let parse xml =
      Some
        {
          grant_token =
            (Util.option_bind (Xml.member "GrantToken" xml) String.parse);
          key_id = (Util.option_bind (Xml.member "KeyId" xml) String.parse);
          grant_id =
            (Util.option_bind (Xml.member "GrantId" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.grant_id
              (fun f -> Query.Pair ("GrantId", (String.to_query f)));
           Util.option_map v.key_id
             (fun f -> Query.Pair ("KeyId", (String.to_query f)));
           Util.option_map v.grant_token
             (fun f -> Query.Pair ("GrantToken", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.grant_id
              (fun f -> ("grant_id", (String.to_json f)));
           Util.option_map v.key_id (fun f -> ("key_id", (String.to_json f)));
           Util.option_map v.grant_token
             (fun f -> ("grant_token", (String.to_json f)))])
    let of_json j =
      {
        grant_token =
          (Util.option_map (Json.lookup j "grant_token") String.of_json);
        key_id = (Util.option_map (Json.lookup j "key_id") String.of_json);
        grant_id =
          (Util.option_map (Json.lookup j "grant_id") String.of_json)
      }
  end
module GenerateRandomResponse =
  struct
    type t = {
      plaintext: Blob.t option }
    let make ?plaintext  () = { plaintext }
    let parse xml =
      Some
        {
          plaintext =
            (Util.option_bind (Xml.member "Plaintext" xml) Blob.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.plaintext
              (fun f -> Query.Pair ("Plaintext", (Blob.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.plaintext
              (fun f -> ("plaintext", (Blob.to_json f)))])
    let of_json j =
      {
        plaintext =
          (Util.option_map (Json.lookup j "plaintext") Blob.of_json)
      }
  end
module GenerateDataKeyRequest =
  struct
    type t =
      {
      key_id: String.t ;
      encryption_context: EncryptionContextType.t option ;
      number_of_bytes: Integer.t option ;
      key_spec: DataKeySpec.t option ;
      grant_tokens: GrantTokenList.t }
    let make ~key_id  ?encryption_context  ?number_of_bytes  ?key_spec 
      ?(grant_tokens= [])  () =
      { key_id; encryption_context; number_of_bytes; key_spec; grant_tokens }
    let parse xml =
      Some
        {
          key_id =
            (Xml.required "KeyId"
               (Util.option_bind (Xml.member "KeyId" xml) String.parse));
          encryption_context =
            (Util.option_bind (Xml.member "EncryptionContext" xml)
               EncryptionContextType.parse);
          number_of_bytes =
            (Util.option_bind (Xml.member "NumberOfBytes" xml) Integer.parse);
          key_spec =
            (Util.option_bind (Xml.member "KeySpec" xml) DataKeySpec.parse);
          grant_tokens =
            (Util.of_option []
               (Util.option_bind (Xml.member "GrantTokens" xml)
                  GrantTokenList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("GrantTokens.member",
                   (GrantTokenList.to_query v.grant_tokens)));
           Util.option_map v.key_spec
             (fun f -> Query.Pair ("KeySpec", (DataKeySpec.to_query f)));
           Util.option_map v.number_of_bytes
             (fun f -> Query.Pair ("NumberOfBytes", (Integer.to_query f)));
           Util.option_map v.encryption_context
             (fun f ->
                Query.Pair
                  ("EncryptionContext", (EncryptionContextType.to_query f)));
           Some (Query.Pair ("KeyId", (String.to_query v.key_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("grant_tokens", (GrantTokenList.to_json v.grant_tokens));
           Util.option_map v.key_spec
             (fun f -> ("key_spec", (DataKeySpec.to_json f)));
           Util.option_map v.number_of_bytes
             (fun f -> ("number_of_bytes", (Integer.to_json f)));
           Util.option_map v.encryption_context
             (fun f ->
                ("encryption_context", (EncryptionContextType.to_json f)));
           Some ("key_id", (String.to_json v.key_id))])
    let of_json j =
      {
        key_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "key_id")));
        encryption_context =
          (Util.option_map (Json.lookup j "encryption_context")
             EncryptionContextType.of_json);
        number_of_bytes =
          (Util.option_map (Json.lookup j "number_of_bytes") Integer.of_json);
        key_spec =
          (Util.option_map (Json.lookup j "key_spec") DataKeySpec.of_json);
        grant_tokens =
          (GrantTokenList.of_json
             (Util.of_option_exn (Json.lookup j "grant_tokens")))
      }
  end
module EncryptResponse =
  struct
    type t = {
      ciphertext_blob: Blob.t option ;
      key_id: String.t option }
    let make ?ciphertext_blob  ?key_id  () = { ciphertext_blob; key_id }
    let parse xml =
      Some
        {
          ciphertext_blob =
            (Util.option_bind (Xml.member "CiphertextBlob" xml) Blob.parse);
          key_id = (Util.option_bind (Xml.member "KeyId" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.key_id
              (fun f -> Query.Pair ("KeyId", (String.to_query f)));
           Util.option_map v.ciphertext_blob
             (fun f -> Query.Pair ("CiphertextBlob", (Blob.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.key_id
              (fun f -> ("key_id", (String.to_json f)));
           Util.option_map v.ciphertext_blob
             (fun f -> ("ciphertext_blob", (Blob.to_json f)))])
    let of_json j =
      {
        ciphertext_blob =
          (Util.option_map (Json.lookup j "ciphertext_blob") Blob.of_json);
        key_id = (Util.option_map (Json.lookup j "key_id") String.of_json)
      }
  end
module GenerateRandomRequest =
  struct
    type t = {
      number_of_bytes: Integer.t option }
    let make ?number_of_bytes  () = { number_of_bytes }
    let parse xml =
      Some
        {
          number_of_bytes =
            (Util.option_bind (Xml.member "NumberOfBytes" xml) Integer.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.number_of_bytes
              (fun f -> Query.Pair ("NumberOfBytes", (Integer.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.number_of_bytes
              (fun f -> ("number_of_bytes", (Integer.to_json f)))])
    let of_json j =
      {
        number_of_bytes =
          (Util.option_map (Json.lookup j "number_of_bytes") Integer.of_json)
      }
  end
module DependencyTimeoutException =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module InvalidArnException =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module EnableKeyRotationRequest =
  struct
    type t = {
      key_id: String.t }
    let make ~key_id  () = { key_id }
    let parse xml =
      Some
        {
          key_id =
            (Xml.required "KeyId"
               (Util.option_bind (Xml.member "KeyId" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("KeyId", (String.to_query v.key_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt [Some ("key_id", (String.to_json v.key_id))])
    let of_json j =
      {
        key_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "key_id")))
      }
  end
module KeyUnavailableException =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module MalformedPolicyDocumentException =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module GetKeyRotationStatusRequest =
  struct
    type t = {
      key_id: String.t }
    let make ~key_id  () = { key_id }
    let parse xml =
      Some
        {
          key_id =
            (Xml.required "KeyId"
               (Util.option_bind (Xml.member "KeyId" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("KeyId", (String.to_query v.key_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt [Some ("key_id", (String.to_json v.key_id))])
    let of_json j =
      {
        key_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "key_id")))
      }
  end
module ReEncryptRequest =
  struct
    type t =
      {
      ciphertext_blob: Blob.t ;
      source_encryption_context: EncryptionContextType.t option ;
      destination_key_id: String.t ;
      destination_encryption_context: EncryptionContextType.t option ;
      grant_tokens: GrantTokenList.t }
    let make ~ciphertext_blob  ?source_encryption_context 
      ~destination_key_id  ?destination_encryption_context  ?(grant_tokens=
      [])  () =
      {
        ciphertext_blob;
        source_encryption_context;
        destination_key_id;
        destination_encryption_context;
        grant_tokens
      }
    let parse xml =
      Some
        {
          ciphertext_blob =
            (Xml.required "CiphertextBlob"
               (Util.option_bind (Xml.member "CiphertextBlob" xml) Blob.parse));
          source_encryption_context =
            (Util.option_bind (Xml.member "SourceEncryptionContext" xml)
               EncryptionContextType.parse);
          destination_key_id =
            (Xml.required "DestinationKeyId"
               (Util.option_bind (Xml.member "DestinationKeyId" xml)
                  String.parse));
          destination_encryption_context =
            (Util.option_bind (Xml.member "DestinationEncryptionContext" xml)
               EncryptionContextType.parse);
          grant_tokens =
            (Util.of_option []
               (Util.option_bind (Xml.member "GrantTokens" xml)
                  GrantTokenList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("GrantTokens.member",
                   (GrantTokenList.to_query v.grant_tokens)));
           Util.option_map v.destination_encryption_context
             (fun f ->
                Query.Pair
                  ("DestinationEncryptionContext",
                    (EncryptionContextType.to_query f)));
           Some
             (Query.Pair
                ("DestinationKeyId", (String.to_query v.destination_key_id)));
           Util.option_map v.source_encryption_context
             (fun f ->
                Query.Pair
                  ("SourceEncryptionContext",
                    (EncryptionContextType.to_query f)));
           Some
             (Query.Pair
                ("CiphertextBlob", (Blob.to_query v.ciphertext_blob)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("grant_tokens", (GrantTokenList.to_json v.grant_tokens));
           Util.option_map v.destination_encryption_context
             (fun f ->
                ("destination_encryption_context",
                  (EncryptionContextType.to_json f)));
           Some ("destination_key_id", (String.to_json v.destination_key_id));
           Util.option_map v.source_encryption_context
             (fun f ->
                ("source_encryption_context",
                  (EncryptionContextType.to_json f)));
           Some ("ciphertext_blob", (Blob.to_json v.ciphertext_blob))])
    let of_json j =
      {
        ciphertext_blob =
          (Blob.of_json
             (Util.of_option_exn (Json.lookup j "ciphertext_blob")));
        source_encryption_context =
          (Util.option_map (Json.lookup j "source_encryption_context")
             EncryptionContextType.of_json);
        destination_key_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "destination_key_id")));
        destination_encryption_context =
          (Util.option_map (Json.lookup j "destination_encryption_context")
             EncryptionContextType.of_json);
        grant_tokens =
          (GrantTokenList.of_json
             (Util.of_option_exn (Json.lookup j "grant_tokens")))
      }
  end
module InvalidMarkerException =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module EnableKeyRequest =
  struct
    type t = {
      key_id: String.t }
    let make ~key_id  () = { key_id }
    let parse xml =
      Some
        {
          key_id =
            (Xml.required "KeyId"
               (Util.option_bind (Xml.member "KeyId" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("KeyId", (String.to_query v.key_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt [Some ("key_id", (String.to_json v.key_id))])
    let of_json j =
      {
        key_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "key_id")))
      }
  end
module DecryptResponse =
  struct
    type t = {
      key_id: String.t option ;
      plaintext: Blob.t option }
    let make ?key_id  ?plaintext  () = { key_id; plaintext }
    let parse xml =
      Some
        {
          key_id = (Util.option_bind (Xml.member "KeyId" xml) String.parse);
          plaintext =
            (Util.option_bind (Xml.member "Plaintext" xml) Blob.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.plaintext
              (fun f -> Query.Pair ("Plaintext", (Blob.to_query f)));
           Util.option_map v.key_id
             (fun f -> Query.Pair ("KeyId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.plaintext
              (fun f -> ("plaintext", (Blob.to_json f)));
           Util.option_map v.key_id (fun f -> ("key_id", (String.to_json f)))])
    let of_json j =
      {
        key_id = (Util.option_map (Json.lookup j "key_id") String.of_json);
        plaintext =
          (Util.option_map (Json.lookup j "plaintext") Blob.of_json)
      }
  end
module KMSInternalException =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module AlreadyExistsException =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module UpdateAliasRequest =
  struct
    type t = {
      alias_name: String.t ;
      target_key_id: String.t }
    let make ~alias_name  ~target_key_id  () = { alias_name; target_key_id }
    let parse xml =
      Some
        {
          alias_name =
            (Xml.required "AliasName"
               (Util.option_bind (Xml.member "AliasName" xml) String.parse));
          target_key_id =
            (Xml.required "TargetKeyId"
               (Util.option_bind (Xml.member "TargetKeyId" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("TargetKeyId", (String.to_query v.target_key_id)));
           Some (Query.Pair ("AliasName", (String.to_query v.alias_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("target_key_id", (String.to_json v.target_key_id));
           Some ("alias_name", (String.to_json v.alias_name))])
    let of_json j =
      {
        alias_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "alias_name")));
        target_key_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "target_key_id")))
      }
  end
module InvalidGrantTokenException =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module ListKeyPoliciesResponse =
  struct
    type t =
      {
      policy_names: PolicyNameList.t ;
      next_marker: String.t option ;
      truncated: Boolean.t option }
    let make ?(policy_names= [])  ?next_marker  ?truncated  () =
      { policy_names; next_marker; truncated }
    let parse xml =
      Some
        {
          policy_names =
            (Util.of_option []
               (Util.option_bind (Xml.member "PolicyNames" xml)
                  PolicyNameList.parse));
          next_marker =
            (Util.option_bind (Xml.member "NextMarker" xml) String.parse);
          truncated =
            (Util.option_bind (Xml.member "Truncated" xml) Boolean.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.truncated
              (fun f -> Query.Pair ("Truncated", (Boolean.to_query f)));
           Util.option_map v.next_marker
             (fun f -> Query.Pair ("NextMarker", (String.to_query f)));
           Some
             (Query.Pair
                ("PolicyNames.member",
                  (PolicyNameList.to_query v.policy_names)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.truncated
              (fun f -> ("truncated", (Boolean.to_json f)));
           Util.option_map v.next_marker
             (fun f -> ("next_marker", (String.to_json f)));
           Some ("policy_names", (PolicyNameList.to_json v.policy_names))])
    let of_json j =
      {
        policy_names =
          (PolicyNameList.of_json
             (Util.of_option_exn (Json.lookup j "policy_names")));
        next_marker =
          (Util.option_map (Json.lookup j "next_marker") String.of_json);
        truncated =
          (Util.option_map (Json.lookup j "truncated") Boolean.of_json)
      }
  end
module ListGrantsResponse =
  struct
    type t =
      {
      grants: GrantList.t ;
      next_marker: String.t option ;
      truncated: Boolean.t option }
    let make ?(grants= [])  ?next_marker  ?truncated  () =
      { grants; next_marker; truncated }
    let parse xml =
      Some
        {
          grants =
            (Util.of_option []
               (Util.option_bind (Xml.member "Grants" xml) GrantList.parse));
          next_marker =
            (Util.option_bind (Xml.member "NextMarker" xml) String.parse);
          truncated =
            (Util.option_bind (Xml.member "Truncated" xml) Boolean.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.truncated
              (fun f -> Query.Pair ("Truncated", (Boolean.to_query f)));
           Util.option_map v.next_marker
             (fun f -> Query.Pair ("NextMarker", (String.to_query f)));
           Some (Query.Pair ("Grants.member", (GrantList.to_query v.grants)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.truncated
              (fun f -> ("truncated", (Boolean.to_json f)));
           Util.option_map v.next_marker
             (fun f -> ("next_marker", (String.to_json f)));
           Some ("grants", (GrantList.to_json v.grants))])
    let of_json j =
      {
        grants =
          (GrantList.of_json (Util.of_option_exn (Json.lookup j "grants")));
        next_marker =
          (Util.option_map (Json.lookup j "next_marker") String.of_json);
        truncated =
          (Util.option_map (Json.lookup j "truncated") Boolean.of_json)
      }
  end
module ListAliasesRequest =
  struct
    type t = {
      limit: Integer.t option ;
      marker: String.t option }
    let make ?limit  ?marker  () = { limit; marker }
    let parse xml =
      Some
        {
          limit = (Util.option_bind (Xml.member "Limit" xml) Integer.parse);
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.limit
             (fun f -> Query.Pair ("Limit", (Integer.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> ("marker", (String.to_json f)));
           Util.option_map v.limit (fun f -> ("limit", (Integer.to_json f)))])
    let of_json j =
      {
        limit = (Util.option_map (Json.lookup j "limit") Integer.of_json);
        marker = (Util.option_map (Json.lookup j "marker") String.of_json)
      }
  end
module ListKeyPoliciesRequest =
  struct
    type t =
      {
      key_id: String.t ;
      limit: Integer.t option ;
      marker: String.t option }
    let make ~key_id  ?limit  ?marker  () = { key_id; limit; marker }
    let parse xml =
      Some
        {
          key_id =
            (Xml.required "KeyId"
               (Util.option_bind (Xml.member "KeyId" xml) String.parse));
          limit = (Util.option_bind (Xml.member "Limit" xml) Integer.parse);
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.limit
             (fun f -> Query.Pair ("Limit", (Integer.to_query f)));
           Some (Query.Pair ("KeyId", (String.to_query v.key_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> ("marker", (String.to_json f)));
           Util.option_map v.limit (fun f -> ("limit", (Integer.to_json f)));
           Some ("key_id", (String.to_json v.key_id))])
    let of_json j =
      {
        key_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "key_id")));
        limit = (Util.option_map (Json.lookup j "limit") Integer.of_json);
        marker = (Util.option_map (Json.lookup j "marker") String.of_json)
      }
  end
module NotFoundException =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module CreateGrantResponse =
  struct
    type t = {
      grant_token: String.t option ;
      grant_id: String.t option }
    let make ?grant_token  ?grant_id  () = { grant_token; grant_id }
    let parse xml =
      Some
        {
          grant_token =
            (Util.option_bind (Xml.member "GrantToken" xml) String.parse);
          grant_id =
            (Util.option_bind (Xml.member "GrantId" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.grant_id
              (fun f -> Query.Pair ("GrantId", (String.to_query f)));
           Util.option_map v.grant_token
             (fun f -> Query.Pair ("GrantToken", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.grant_id
              (fun f -> ("grant_id", (String.to_json f)));
           Util.option_map v.grant_token
             (fun f -> ("grant_token", (String.to_json f)))])
    let of_json j =
      {
        grant_token =
          (Util.option_map (Json.lookup j "grant_token") String.of_json);
        grant_id =
          (Util.option_map (Json.lookup j "grant_id") String.of_json)
      }
  end
module DeleteAliasRequest =
  struct
    type t = {
      alias_name: String.t }
    let make ~alias_name  () = { alias_name }
    let parse xml =
      Some
        {
          alias_name =
            (Xml.required "AliasName"
               (Util.option_bind (Xml.member "AliasName" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("AliasName", (String.to_query v.alias_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("alias_name", (String.to_json v.alias_name))])
    let of_json j =
      {
        alias_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "alias_name")))
      }
  end
module PutKeyPolicyRequest =
  struct
    type t = {
      key_id: String.t ;
      policy_name: String.t ;
      policy: String.t }
    let make ~key_id  ~policy_name  ~policy  () =
      { key_id; policy_name; policy }
    let parse xml =
      Some
        {
          key_id =
            (Xml.required "KeyId"
               (Util.option_bind (Xml.member "KeyId" xml) String.parse));
          policy_name =
            (Xml.required "PolicyName"
               (Util.option_bind (Xml.member "PolicyName" xml) String.parse));
          policy =
            (Xml.required "Policy"
               (Util.option_bind (Xml.member "Policy" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Policy", (String.to_query v.policy)));
           Some (Query.Pair ("PolicyName", (String.to_query v.policy_name)));
           Some (Query.Pair ("KeyId", (String.to_query v.key_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("policy", (String.to_json v.policy));
           Some ("policy_name", (String.to_json v.policy_name));
           Some ("key_id", (String.to_json v.key_id))])
    let of_json j =
      {
        key_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "key_id")));
        policy_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "policy_name")));
        policy =
          (String.of_json (Util.of_option_exn (Json.lookup j "policy")))
      }
  end
module DecryptRequest =
  struct
    type t =
      {
      ciphertext_blob: Blob.t ;
      encryption_context: EncryptionContextType.t option ;
      grant_tokens: GrantTokenList.t }
    let make ~ciphertext_blob  ?encryption_context  ?(grant_tokens= [])  () =
      { ciphertext_blob; encryption_context; grant_tokens }
    let parse xml =
      Some
        {
          ciphertext_blob =
            (Xml.required "CiphertextBlob"
               (Util.option_bind (Xml.member "CiphertextBlob" xml) Blob.parse));
          encryption_context =
            (Util.option_bind (Xml.member "EncryptionContext" xml)
               EncryptionContextType.parse);
          grant_tokens =
            (Util.of_option []
               (Util.option_bind (Xml.member "GrantTokens" xml)
                  GrantTokenList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("GrantTokens.member",
                   (GrantTokenList.to_query v.grant_tokens)));
           Util.option_map v.encryption_context
             (fun f ->
                Query.Pair
                  ("EncryptionContext", (EncryptionContextType.to_query f)));
           Some
             (Query.Pair
                ("CiphertextBlob", (Blob.to_query v.ciphertext_blob)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("grant_tokens", (GrantTokenList.to_json v.grant_tokens));
           Util.option_map v.encryption_context
             (fun f ->
                ("encryption_context", (EncryptionContextType.to_json f)));
           Some ("ciphertext_blob", (Blob.to_json v.ciphertext_blob))])
    let of_json j =
      {
        ciphertext_blob =
          (Blob.of_json
             (Util.of_option_exn (Json.lookup j "ciphertext_blob")));
        encryption_context =
          (Util.option_map (Json.lookup j "encryption_context")
             EncryptionContextType.of_json);
        grant_tokens =
          (GrantTokenList.of_json
             (Util.of_option_exn (Json.lookup j "grant_tokens")))
      }
  end
module DisabledException =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module GetKeyPolicyRequest =
  struct
    type t = {
      key_id: String.t ;
      policy_name: String.t }
    let make ~key_id  ~policy_name  () = { key_id; policy_name }
    let parse xml =
      Some
        {
          key_id =
            (Xml.required "KeyId"
               (Util.option_bind (Xml.member "KeyId" xml) String.parse));
          policy_name =
            (Xml.required "PolicyName"
               (Util.option_bind (Xml.member "PolicyName" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("PolicyName", (String.to_query v.policy_name)));
           Some (Query.Pair ("KeyId", (String.to_query v.key_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("policy_name", (String.to_json v.policy_name));
           Some ("key_id", (String.to_json v.key_id))])
    let of_json j =
      {
        key_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "key_id")));
        policy_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "policy_name")))
      }
  end
module ListKeysRequest =
  struct
    type t = {
      limit: Integer.t option ;
      marker: String.t option }
    let make ?limit  ?marker  () = { limit; marker }
    let parse xml =
      Some
        {
          limit = (Util.option_bind (Xml.member "Limit" xml) Integer.parse);
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.limit
             (fun f -> Query.Pair ("Limit", (Integer.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> ("marker", (String.to_json f)));
           Util.option_map v.limit (fun f -> ("limit", (Integer.to_json f)))])
    let of_json j =
      {
        limit = (Util.option_map (Json.lookup j "limit") Integer.of_json);
        marker = (Util.option_map (Json.lookup j "marker") String.of_json)
      }
  end
module GenerateDataKeyResponse =
  struct
    type t = {
      ciphertext_blob: Blob.t ;
      plaintext: Blob.t ;
      key_id: String.t }
    let make ~ciphertext_blob  ~plaintext  ~key_id  () =
      { ciphertext_blob; plaintext; key_id }
    let parse xml =
      Some
        {
          ciphertext_blob =
            (Xml.required "CiphertextBlob"
               (Util.option_bind (Xml.member "CiphertextBlob" xml) Blob.parse));
          plaintext =
            (Xml.required "Plaintext"
               (Util.option_bind (Xml.member "Plaintext" xml) Blob.parse));
          key_id =
            (Xml.required "KeyId"
               (Util.option_bind (Xml.member "KeyId" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("KeyId", (String.to_query v.key_id)));
           Some (Query.Pair ("Plaintext", (Blob.to_query v.plaintext)));
           Some
             (Query.Pair
                ("CiphertextBlob", (Blob.to_query v.ciphertext_blob)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("key_id", (String.to_json v.key_id));
           Some ("plaintext", (Blob.to_json v.plaintext));
           Some ("ciphertext_blob", (Blob.to_json v.ciphertext_blob))])
    let of_json j =
      {
        ciphertext_blob =
          (Blob.of_json
             (Util.of_option_exn (Json.lookup j "ciphertext_blob")));
        plaintext =
          (Blob.of_json (Util.of_option_exn (Json.lookup j "plaintext")));
        key_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "key_id")))
      }
  end
module InvalidKeyUsageException =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module CreateGrantRequest =
  struct
    type t =
      {
      key_id: String.t ;
      grantee_principal: String.t ;
      retiring_principal: String.t option ;
      operations: GrantOperationList.t ;
      constraints: GrantConstraints.t option ;
      grant_tokens: GrantTokenList.t }
    let make ~key_id  ~grantee_principal  ?retiring_principal  ?(operations=
      [])  ?constraints  ?(grant_tokens= [])  () =
      {
        key_id;
        grantee_principal;
        retiring_principal;
        operations;
        constraints;
        grant_tokens
      }
    let parse xml =
      Some
        {
          key_id =
            (Xml.required "KeyId"
               (Util.option_bind (Xml.member "KeyId" xml) String.parse));
          grantee_principal =
            (Xml.required "GranteePrincipal"
               (Util.option_bind (Xml.member "GranteePrincipal" xml)
                  String.parse));
          retiring_principal =
            (Util.option_bind (Xml.member "RetiringPrincipal" xml)
               String.parse);
          operations =
            (Util.of_option []
               (Util.option_bind (Xml.member "Operations" xml)
                  GrantOperationList.parse));
          constraints =
            (Util.option_bind (Xml.member "Constraints" xml)
               GrantConstraints.parse);
          grant_tokens =
            (Util.of_option []
               (Util.option_bind (Xml.member "GrantTokens" xml)
                  GrantTokenList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("GrantTokens.member",
                   (GrantTokenList.to_query v.grant_tokens)));
           Util.option_map v.constraints
             (fun f ->
                Query.Pair ("Constraints", (GrantConstraints.to_query f)));
           Some
             (Query.Pair
                ("Operations.member",
                  (GrantOperationList.to_query v.operations)));
           Util.option_map v.retiring_principal
             (fun f -> Query.Pair ("RetiringPrincipal", (String.to_query f)));
           Some
             (Query.Pair
                ("GranteePrincipal", (String.to_query v.grantee_principal)));
           Some (Query.Pair ("KeyId", (String.to_query v.key_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("grant_tokens", (GrantTokenList.to_json v.grant_tokens));
           Util.option_map v.constraints
             (fun f -> ("constraints", (GrantConstraints.to_json f)));
           Some ("operations", (GrantOperationList.to_json v.operations));
           Util.option_map v.retiring_principal
             (fun f -> ("retiring_principal", (String.to_json f)));
           Some ("grantee_principal", (String.to_json v.grantee_principal));
           Some ("key_id", (String.to_json v.key_id))])
    let of_json j =
      {
        key_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "key_id")));
        grantee_principal =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "grantee_principal")));
        retiring_principal =
          (Util.option_map (Json.lookup j "retiring_principal")
             String.of_json);
        operations =
          (GrantOperationList.of_json
             (Util.of_option_exn (Json.lookup j "operations")));
        constraints =
          (Util.option_map (Json.lookup j "constraints")
             GrantConstraints.of_json);
        grant_tokens =
          (GrantTokenList.of_json
             (Util.of_option_exn (Json.lookup j "grant_tokens")))
      }
  end
module GenerateDataKeyWithoutPlaintextRequest =
  struct
    type t =
      {
      key_id: String.t ;
      encryption_context: EncryptionContextType.t option ;
      key_spec: DataKeySpec.t option ;
      number_of_bytes: Integer.t option ;
      grant_tokens: GrantTokenList.t }
    let make ~key_id  ?encryption_context  ?key_spec  ?number_of_bytes 
      ?(grant_tokens= [])  () =
      { key_id; encryption_context; key_spec; number_of_bytes; grant_tokens }
    let parse xml =
      Some
        {
          key_id =
            (Xml.required "KeyId"
               (Util.option_bind (Xml.member "KeyId" xml) String.parse));
          encryption_context =
            (Util.option_bind (Xml.member "EncryptionContext" xml)
               EncryptionContextType.parse);
          key_spec =
            (Util.option_bind (Xml.member "KeySpec" xml) DataKeySpec.parse);
          number_of_bytes =
            (Util.option_bind (Xml.member "NumberOfBytes" xml) Integer.parse);
          grant_tokens =
            (Util.of_option []
               (Util.option_bind (Xml.member "GrantTokens" xml)
                  GrantTokenList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("GrantTokens.member",
                   (GrantTokenList.to_query v.grant_tokens)));
           Util.option_map v.number_of_bytes
             (fun f -> Query.Pair ("NumberOfBytes", (Integer.to_query f)));
           Util.option_map v.key_spec
             (fun f -> Query.Pair ("KeySpec", (DataKeySpec.to_query f)));
           Util.option_map v.encryption_context
             (fun f ->
                Query.Pair
                  ("EncryptionContext", (EncryptionContextType.to_query f)));
           Some (Query.Pair ("KeyId", (String.to_query v.key_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("grant_tokens", (GrantTokenList.to_json v.grant_tokens));
           Util.option_map v.number_of_bytes
             (fun f -> ("number_of_bytes", (Integer.to_json f)));
           Util.option_map v.key_spec
             (fun f -> ("key_spec", (DataKeySpec.to_json f)));
           Util.option_map v.encryption_context
             (fun f ->
                ("encryption_context", (EncryptionContextType.to_json f)));
           Some ("key_id", (String.to_json v.key_id))])
    let of_json j =
      {
        key_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "key_id")));
        encryption_context =
          (Util.option_map (Json.lookup j "encryption_context")
             EncryptionContextType.of_json);
        key_spec =
          (Util.option_map (Json.lookup j "key_spec") DataKeySpec.of_json);
        number_of_bytes =
          (Util.option_map (Json.lookup j "number_of_bytes") Integer.of_json);
        grant_tokens =
          (GrantTokenList.of_json
             (Util.of_option_exn (Json.lookup j "grant_tokens")))
      }
  end
module UnsupportedOperationException =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module CreateAliasRequest =
  struct
    type t = {
      alias_name: String.t ;
      target_key_id: String.t }
    let make ~alias_name  ~target_key_id  () = { alias_name; target_key_id }
    let parse xml =
      Some
        {
          alias_name =
            (Xml.required "AliasName"
               (Util.option_bind (Xml.member "AliasName" xml) String.parse));
          target_key_id =
            (Xml.required "TargetKeyId"
               (Util.option_bind (Xml.member "TargetKeyId" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("TargetKeyId", (String.to_query v.target_key_id)));
           Some (Query.Pair ("AliasName", (String.to_query v.alias_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("target_key_id", (String.to_json v.target_key_id));
           Some ("alias_name", (String.to_json v.alias_name))])
    let of_json j =
      {
        alias_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "alias_name")));
        target_key_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "target_key_id")))
      }
  end
module ListGrantsRequest =
  struct
    type t =
      {
      key_id: String.t ;
      limit: Integer.t option ;
      marker: String.t option }
    let make ~key_id  ?limit  ?marker  () = { key_id; limit; marker }
    let parse xml =
      Some
        {
          key_id =
            (Xml.required "KeyId"
               (Util.option_bind (Xml.member "KeyId" xml) String.parse));
          limit = (Util.option_bind (Xml.member "Limit" xml) Integer.parse);
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.limit
             (fun f -> Query.Pair ("Limit", (Integer.to_query f)));
           Some (Query.Pair ("KeyId", (String.to_query v.key_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> ("marker", (String.to_json f)));
           Util.option_map v.limit (fun f -> ("limit", (Integer.to_json f)));
           Some ("key_id", (String.to_json v.key_id))])
    let of_json j =
      {
        key_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "key_id")));
        limit = (Util.option_map (Json.lookup j "limit") Integer.of_json);
        marker = (Util.option_map (Json.lookup j "marker") String.of_json)
      }
  end
module RevokeGrantRequest =
  struct
    type t = {
      key_id: String.t ;
      grant_id: String.t }
    let make ~key_id  ~grant_id  () = { key_id; grant_id }
    let parse xml =
      Some
        {
          key_id =
            (Xml.required "KeyId"
               (Util.option_bind (Xml.member "KeyId" xml) String.parse));
          grant_id =
            (Xml.required "GrantId"
               (Util.option_bind (Xml.member "GrantId" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("GrantId", (String.to_query v.grant_id)));
           Some (Query.Pair ("KeyId", (String.to_query v.key_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("grant_id", (String.to_json v.grant_id));
           Some ("key_id", (String.to_json v.key_id))])
    let of_json j =
      {
        key_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "key_id")));
        grant_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "grant_id")))
      }
  end
module GenerateDataKeyWithoutPlaintextResponse =
  struct
    type t = {
      ciphertext_blob: Blob.t option ;
      key_id: String.t option }
    let make ?ciphertext_blob  ?key_id  () = { ciphertext_blob; key_id }
    let parse xml =
      Some
        {
          ciphertext_blob =
            (Util.option_bind (Xml.member "CiphertextBlob" xml) Blob.parse);
          key_id = (Util.option_bind (Xml.member "KeyId" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.key_id
              (fun f -> Query.Pair ("KeyId", (String.to_query f)));
           Util.option_map v.ciphertext_blob
             (fun f -> Query.Pair ("CiphertextBlob", (Blob.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.key_id
              (fun f -> ("key_id", (String.to_json f)));
           Util.option_map v.ciphertext_blob
             (fun f -> ("ciphertext_blob", (Blob.to_json f)))])
    let of_json j =
      {
        ciphertext_blob =
          (Util.option_map (Json.lookup j "ciphertext_blob") Blob.of_json);
        key_id = (Util.option_map (Json.lookup j "key_id") String.of_json)
      }
  end
module LimitExceededException =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module GetKeyRotationStatusResponse =
  struct
    type t = {
      key_rotation_enabled: Boolean.t option }
    let make ?key_rotation_enabled  () = { key_rotation_enabled }
    let parse xml =
      Some
        {
          key_rotation_enabled =
            (Util.option_bind (Xml.member "KeyRotationEnabled" xml)
               Boolean.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.key_rotation_enabled
              (fun f ->
                 Query.Pair ("KeyRotationEnabled", (Boolean.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.key_rotation_enabled
              (fun f -> ("key_rotation_enabled", (Boolean.to_json f)))])
    let of_json j =
      {
        key_rotation_enabled =
          (Util.option_map (Json.lookup j "key_rotation_enabled")
             Boolean.of_json)
      }
  end
module ListAliasesResponse =
  struct
    type t =
      {
      aliases: AliasList.t ;
      next_marker: String.t option ;
      truncated: Boolean.t option }
    let make ?(aliases= [])  ?next_marker  ?truncated  () =
      { aliases; next_marker; truncated }
    let parse xml =
      Some
        {
          aliases =
            (Util.of_option []
               (Util.option_bind (Xml.member "Aliases" xml) AliasList.parse));
          next_marker =
            (Util.option_bind (Xml.member "NextMarker" xml) String.parse);
          truncated =
            (Util.option_bind (Xml.member "Truncated" xml) Boolean.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.truncated
              (fun f -> Query.Pair ("Truncated", (Boolean.to_query f)));
           Util.option_map v.next_marker
             (fun f -> Query.Pair ("NextMarker", (String.to_query f)));
           Some
             (Query.Pair ("Aliases.member", (AliasList.to_query v.aliases)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.truncated
              (fun f -> ("truncated", (Boolean.to_json f)));
           Util.option_map v.next_marker
             (fun f -> ("next_marker", (String.to_json f)));
           Some ("aliases", (AliasList.to_json v.aliases))])
    let of_json j =
      {
        aliases =
          (AliasList.of_json (Util.of_option_exn (Json.lookup j "aliases")));
        next_marker =
          (Util.option_map (Json.lookup j "next_marker") String.of_json);
        truncated =
          (Util.option_map (Json.lookup j "truncated") Boolean.of_json)
      }
  end
module UpdateKeyDescriptionRequest =
  struct
    type t = {
      key_id: String.t ;
      description: String.t }
    let make ~key_id  ~description  () = { key_id; description }
    let parse xml =
      Some
        {
          key_id =
            (Xml.required "KeyId"
               (Util.option_bind (Xml.member "KeyId" xml) String.parse));
          description =
            (Xml.required "Description"
               (Util.option_bind (Xml.member "Description" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("Description", (String.to_query v.description)));
           Some (Query.Pair ("KeyId", (String.to_query v.key_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("description", (String.to_json v.description));
           Some ("key_id", (String.to_json v.key_id))])
    let of_json j =
      {
        key_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "key_id")));
        description =
          (String.of_json (Util.of_option_exn (Json.lookup j "description")))
      }
  end
module CreateKeyRequest =
  struct
    type t =
      {
      policy: String.t option ;
      description: String.t option ;
      key_usage: KeyUsageType.t option }
    let make ?policy  ?description  ?key_usage  () =
      { policy; description; key_usage }
    let parse xml =
      Some
        {
          policy = (Util.option_bind (Xml.member "Policy" xml) String.parse);
          description =
            (Util.option_bind (Xml.member "Description" xml) String.parse);
          key_usage =
            (Util.option_bind (Xml.member "KeyUsage" xml) KeyUsageType.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.key_usage
              (fun f -> Query.Pair ("KeyUsage", (KeyUsageType.to_query f)));
           Util.option_map v.description
             (fun f -> Query.Pair ("Description", (String.to_query f)));
           Util.option_map v.policy
             (fun f -> Query.Pair ("Policy", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.key_usage
              (fun f -> ("key_usage", (KeyUsageType.to_json f)));
           Util.option_map v.description
             (fun f -> ("description", (String.to_json f)));
           Util.option_map v.policy (fun f -> ("policy", (String.to_json f)))])
    let of_json j =
      {
        policy = (Util.option_map (Json.lookup j "policy") String.of_json);
        description =
          (Util.option_map (Json.lookup j "description") String.of_json);
        key_usage =
          (Util.option_map (Json.lookup j "key_usage") KeyUsageType.of_json)
      }
  end
module InvalidAliasNameException =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module DescribeKeyRequest =
  struct
    type t = {
      key_id: String.t }
    let make ~key_id  () = { key_id }
    let parse xml =
      Some
        {
          key_id =
            (Xml.required "KeyId"
               (Util.option_bind (Xml.member "KeyId" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("KeyId", (String.to_query v.key_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt [Some ("key_id", (String.to_json v.key_id))])
    let of_json j =
      {
        key_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "key_id")))
      }
  end
module DisableKeyRequest =
  struct
    type t = {
      key_id: String.t }
    let make ~key_id  () = { key_id }
    let parse xml =
      Some
        {
          key_id =
            (Xml.required "KeyId"
               (Util.option_bind (Xml.member "KeyId" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("KeyId", (String.to_query v.key_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt [Some ("key_id", (String.to_json v.key_id))])
    let of_json j =
      {
        key_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "key_id")))
      }
  end
module ReEncryptResponse =
  struct
    type t =
      {
      ciphertext_blob: Blob.t option ;
      source_key_id: String.t option ;
      key_id: String.t option }
    let make ?ciphertext_blob  ?source_key_id  ?key_id  () =
      { ciphertext_blob; source_key_id; key_id }
    let parse xml =
      Some
        {
          ciphertext_blob =
            (Util.option_bind (Xml.member "CiphertextBlob" xml) Blob.parse);
          source_key_id =
            (Util.option_bind (Xml.member "SourceKeyId" xml) String.parse);
          key_id = (Util.option_bind (Xml.member "KeyId" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.key_id
              (fun f -> Query.Pair ("KeyId", (String.to_query f)));
           Util.option_map v.source_key_id
             (fun f -> Query.Pair ("SourceKeyId", (String.to_query f)));
           Util.option_map v.ciphertext_blob
             (fun f -> Query.Pair ("CiphertextBlob", (Blob.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.key_id
              (fun f -> ("key_id", (String.to_json f)));
           Util.option_map v.source_key_id
             (fun f -> ("source_key_id", (String.to_json f)));
           Util.option_map v.ciphertext_blob
             (fun f -> ("ciphertext_blob", (Blob.to_json f)))])
    let of_json j =
      {
        ciphertext_blob =
          (Util.option_map (Json.lookup j "ciphertext_blob") Blob.of_json);
        source_key_id =
          (Util.option_map (Json.lookup j "source_key_id") String.of_json);
        key_id = (Util.option_map (Json.lookup j "key_id") String.of_json)
      }
  end
module DisableKeyRotationRequest =
  struct
    type t = {
      key_id: String.t }
    let make ~key_id  () = { key_id }
    let parse xml =
      Some
        {
          key_id =
            (Xml.required "KeyId"
               (Util.option_bind (Xml.member "KeyId" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("KeyId", (String.to_query v.key_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt [Some ("key_id", (String.to_json v.key_id))])
    let of_json j =
      {
        key_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "key_id")))
      }
  end
module CreateKeyResponse =
  struct
    type t = {
      key_metadata: KeyMetadata.t option }
    let make ?key_metadata  () = { key_metadata }
    let parse xml =
      Some
        {
          key_metadata =
            (Util.option_bind (Xml.member "KeyMetadata" xml)
               KeyMetadata.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.key_metadata
              (fun f -> Query.Pair ("KeyMetadata", (KeyMetadata.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.key_metadata
              (fun f -> ("key_metadata", (KeyMetadata.to_json f)))])
    let of_json j =
      {
        key_metadata =
          (Util.option_map (Json.lookup j "key_metadata") KeyMetadata.of_json)
      }
  end