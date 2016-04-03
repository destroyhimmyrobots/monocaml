(* API for Monome Interpreter *)
type grid = { size  : grid_size
            ; leds  : grid_leds
            ; tilt  : grid_tilt
            ; state : grid_state }
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
