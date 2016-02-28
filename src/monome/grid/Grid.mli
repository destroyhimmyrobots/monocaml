type grid_series = Grid40h        (* 200p6-2007 Tiltmod Binary *)
                 | Grid40hSE      (* 2007	Tilt	Binary *)
                 | Grid40hKit     (* 2007-2010 	Binary *)
                 | GridSeries064  (* 2007-2010 Tilt	Binary *)
                 | GridSeries128  (* 2007-2010 No	Binary *)
                 | GridSeries256  (* 2007-2010 No	Binary *)
                 | GridMK8x8      (* 2010 *)
                 | GridMK8x16     (* 2010 *)
                 | GridMK16x16    (* 2010 *)
                 | GridGrey064    (* 2010-2012 Tilt	Binary *)
                 | GridGrey128    (* 2010-2012	Tilt	Binary *)
                 | Grid064        (* 2011-2012 Tilt	Quaternary-11/Sedenary-12 *)
                 | Grid128        (* 2011-2015 Tilt-12	Quaternary-11/Sedenary-12 *)
                 | Grid256        (* 2011-2012 Tilt	Quaternary-11/Sedenary-12 *)
                 | Grid512        (* 2010 *)
type grid_size = Grid064 | Grid128 | Grid256 | Grid512
type grid_leds = Binary | Quaternary | Sedenary | LedUnknown
type grid_tilt = Nulliary       (* 2007-2010, 2013-2015 *)
               | Uniaxis        (* 2006-2011 *)
               | Triaxis        (* 2012 only *)
               | ModUniaxis
               | TiltUnknown
type grid_year = Y2006 | Y2007 | Y2008 | Y2009 | Y2010
               | Y2011 | Y2012 | Y2013 | Y2014 | Y2015
type grid_capability = { series : grid_series
                       ; year   : grid_year
                       ; leds   : grid_leds
                       ; tilt   : grid_tilt }
type grid_state = intensity array array
type intensity = char

module type TMonomeGrid =
  sig
    (* Perhaps use type a = FixedSizeArray16 for mask_rows, mask_cols? *)
    type t
    val rows : int
    val cols : int
    val mask_rows : int
    val mask_cols : int
    val cardinality : int
    val editions : grid_capability list
  end

module type MONOMEGRID =
  functor (M : TMonomeGrid) ->
  sig
    type t
    (* LED brightness: 0x00 to 0x0F or '\000' to '\016' *)
    (* Have the user select capabilities: *)
    (* List.hd (MonomeGrid256.editions ()) |> MonomeGrid256.make *)
    val editions : grid_capability list
    val make : grid_capability -> t
    val mask_col : intensity array
    val mask_row : intensity array
    val leds : t -> grid_leds
    val tilt : t -> grid_tilt
    val set_key : t -> int -> int -> intensity -> t
    val get_key : t -> int -> int -> intensity
  end
