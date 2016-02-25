module OscMessageBuilder : OscMessageBuilder = struct
  let bool_list s bs = { selector = s; parameters = List.map (fun x -> OSCBool x  ) bs }
  let  chr_list s cs = { selector = s; parameters = List.map (fun x -> OSCInt8 x  ) cs }
  let  int_list s is = { selector = s; parameters = List.map (fun x -> OSCInt  x  ) is }
  let  str_list s ss = { selector = s; parameters = List.map (fun t -> OSCString t) ss }
  let bool s b = bool_list s [b]
  let  chr s c =  chr_list s [c]
  let  int s i =  int_list s [i]
  let  str s t =  str_list s [t]

  let concat_osc = String.concat " "
  let rec string_of_osc = function
    | OSCBool b -> if b then "1" else "0"
    | OSCInt i -> string_of_int i
    | OSCInt8 c -> Char.escaped c
    | OSCString s -> s
    | OSCSelectorValue (OSCSelector s) -> s
    | OSCList l -> concat_osc_list l
  and concat_osc_list w = concat_osc (List.map string_of_osc w)
  let build {selector = s; parameters = p} = concat_osc_list (OSCSelectorValue s :: p)
end