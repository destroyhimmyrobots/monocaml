module OscMatcher : OscMatcher = struct
  let matcher sel param_matchers =
    let rec aux values matchers = match values, matchers with
      | v :: vs , m :: ms -> (try m v
                              with Match_failure _ -> raise OSCMessageParseError) :: aux vs ms
      | [], [] -> []
      | _ -> raise OSCMessageParseError
    in function { selector   = s'
                ; parameters = p' } when sel = s' -> aux p' param_matchers
              | _ -> raise OSCValueMarshallError

  (* TODO: int_list matcher *)
  let int sel message =
    match matcher sel [function OSCInt i -> OSCInt i] message
    with | [OSCInt i] -> i
         | _ -> raise OSCValueMarshallError
  let chr sel message =
    match matcher sel [function OSCInt8 c -> OSCInt8 c] message
    with | [OSCInt8 c] -> c
         | _ -> raise OSCValueMarshallError
  let str sel message =
    match matcher sel [function OSCString s -> OSCString s] message
    with | [OSCString s] -> s
         | _ -> raise OSCValueMarshallError
end