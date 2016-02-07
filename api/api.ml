type oscSelector = OSCSelector of string
type oscValue = OSCInt of int
              | OSCString of string
              | OSCPair of oscValue * oscValue
              | OSCTriple of oscValue * oscValue * oscValue
type 'a oscValue' = OSCValue' of 'a
                  | OSCPair' of oscValue' * oscValue'
                  | OSCTriple' of oscValue' * oscValue'
type oscMessage = { selector : oscSelector
                  ; parameters : oscValue list }
exception OSCMessageParseError

module SerialOsc : sig
  module MessageBuilder : sig
    val build : oscMessage -> string
  end

  module DaemonApi : sig
    val prefix : oscSelector
    module SendingApi : sig
      val list : string -> int -> oscMessage   (* List connected devices *)
      val notify : string -> int -> oscMessage (* Change listening address *)
    end
    module ReceivingApi : sig
      val device : oscMessage -> (string, string, int) (* Currently connected device *)
      val add : oscMessage -> string    (* Device connected *)
      val remove : oscMessage -> string (* Device disconnected *)
    end
  end

  module DeviceApi : sig
    module SysApi : sig
      val prefix : oscSelector
      module SendingApi : sig
        val port : int -> oscMessage      (* Set client listening port *)
        val host : string -> oscMessage   (* Set client listening post *)
        val prefix : string -> oscMessage (* Set device message filter *)
        val rotation : int -> oscMessage  (* Set device rotation 0 90 180 270 *)
      end
      module RecevingApi : sig
        val id : oscMessage -> string          (* device id  *)
        val port : oscMessage -> int           (* device listen port *)
        val host : oscMessage -> string        (* device listen host *)
        val size : oscMessage -> int, int      (* grid device size *)
        val prefix : oscMessage -> oscSelector (* application prefix *)
        val rotation : oscMessage -> int       (* grid device rotation *)
      end
    end
    module InfoApi : sig
      val prefix : oscSelector
      module SendingApi : sig
        val info : () -> oscMessage (* Receive device info messages at this application *)
        val info' : int -> oscMessage (* Receive device info messages at localhost:port *)
        val info'' : string -> int -> oscMessage (* Receive device info messages to host:port *)
      end
    end
  end

end = struct
  let with_prefix (OSCSelector a) w = OSCSelector (a ^ w)

  module MessageBuilder = struct
    let concat_osc = String.concat " "
    let concat_osc_list w = concat_osc (List.map string_of_osc w)
    and rec string_of_osc = function
      | OSCInt i -> string_of_int i
      | OSCString s -> s
      | OSCPair (a, w) -> concat_osc_list [a; w]
      | OSCTriple (a, w, v) -> concat_osc_list [a;  w;  v]
    let build {selector; parameters} = concat_osc selector (concat_osc_list parameters)
  end

  module DaemonApi = struct
    let prefix =  "/serialosc"
    let with_prefix = with_prefix (OSCSelector prefix)
    module SendingApi = struct
        let list   host port = OSCMessage ( with_prefix  "/list"
                                          , [OSCString host, OSCInt port])
        let notify host port = OSCMessage ( with_prefix "/notify"
                                          , [OSCString host, OSCInt port])
    end
    module ReceivingApi = struct
        let device = function
          | { parameter = OSCMessage (_, OSCTriple (OSCString id,
                                                    OSCString typ,
                                                    OSCInt port)) } -> (id, typ, port)
          | _ -> raise OSCMessageParseError
        let add = function
          | { parameter = OSCString id } -> id
          | _ -> raise OSCMessageParseError
        let remove = function
          | { parameter = OSCString id } -> id
          | _ -> raise OSCMessageParseError
    end
  end

  module DeviceApi = struct
    module SysApi = struct
      let prefix = "/sys"
      let with_prefix = with_prefix (OSCSelector prefix)
      module SendingApi = struct
        let port p = OSCMessage (with_prefix "/port", OSCInt p)
        let host h = OSCMessage (with_prefix "/host", OSCString h)
      end
    end
    module InfoApi = struct
      let prefix = "/info"
      let with_prefix = with_prefix (OSCSelector prefix) (* TODO: Same as /sys/info?  *)
      module SendingApi = struct
        let info = OSCMessage (with_prefix "", "") (* TODO: Does empty string work? Trailing spaces... *)
        let info' port = OSCMessage (with_prefix "", OSCInt port)
        let info'' host port -> OSCMessage (with_prefix "",
                                            OSCPair ( OSCString host
                                                    , OSCInt port ))
      end
      module ReceivingApi = struct
        let id       = function { parameter = OSCMessage (OSCInt i) } -> i
        let host     = function { parameter = OSCMessage (OSCInt h) } -> h
        let port     = function { parameter = OSCMessage (OSCInt p) } -> p
        let rotation = function { parameter = OSCMessage (OSCInt r) } -> r
        let prefix   = function { parameter = OSCMessage (OSCString pre) } -> pre
        let size     = function { parameter = OSCMessage [OOSCInt w, OSCInt h] } -> w, h
      end
    end
  end
end
