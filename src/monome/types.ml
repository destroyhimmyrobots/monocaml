type grid_edition = Grid40h        (* 2006-2007 Tiltmod Binary *)
                  | Grid40hSE      (* 2007	Tilt	Binary *)
                  | Grid40hKit     (* 2007-2010 	Binary *)
                  | GridSeries064  (* 2007-2010 Tilt	Binary *)
                  | GridSeries128  (* 2007-2010 No	Binary *)
                  | GridSeries256  (* 2007-2010 No	Binary *)
                  | GridMK8x8      (* 2010 *)
                  | GridMK8x16     (* 2010 *)
                  | GridMK16x16    (* 2010 *)
                  | GridGrey064    (* 2010-2012 Tilt	Binary	*)
                  | GridGrey128    (* 2010-2012	Tilt	Binary	*)
                  | Grid064        (* 2011-2012 Tilt	Quaternary-11/Sedenary-12 *)
                  | Grid128        (* 2011-2015 Tilt-12	Quaternary-11/Sedenary-12 *)
                  | Grid256        (* 2011-2012 Tilt	Quaternary-11/Sedenary-12 *)
                  | Grid512        (* 2010 *)
type grid_size = Grid064
               | Grid128
               | Grid256
               | Grid512
type grid_leds = Binary
               | Quaternary
               | Sedenary
type grid_tilt = Nulliary       (* 2007-2010, 2013-2015 *)
               | Uniaxis        (* 2006-2011 *)
               | Triaxis        (* 2012 only *)
type intensity = char (* LED brightness: 0x00 to 0x0F or '\000' to '\016' *)
type grid_state = intensity array array
type grid = { size  : grid_size
            ; leds  : grid_leds
            ; tilt  : grid_tilt
            ; state : grid_state }
type mask = char                    (* LED configuration: 8 bits. 0x00 to 0xFF or '\000' to '\255'. *)
type row  = char                    (* 0x00 to 0x0F or '\000' to '\016' *)
type col  = char                    (* 0x00 to 0x0F or '\000' to '\016' *)
type point = { row : row ; col : col }
type key_state = { key : point ; state : bool }
type offset   = Noll | Atta
type quadrant = I | II | III | IV
type tiltValue = Roll of int
               | Yaw of int
               | Pitch of int (* 8-bit integers *)
type tiltSensor = Sensor of int
type oscSelector = OSCSelector of string
type oscValue = OSCBool of bool
              | OSCInt of int
              | OSCInt8 of char
              | OSCString of string
              | OSCSelectorValue of oscSelector
              | OSCList of oscValue list
type oscMessage = { selector : oscSelector
                  ; parameters : oscValue list }
