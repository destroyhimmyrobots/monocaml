module MakeMonomeGrid : MONOMEGRID =
  functor (M : TMonomeGrid) ->
  struct
    type t = intensity array array * grid_leds * grid_tilt * grid_year * grid_series
    let editions = M.editions
    let make { leds = d ; series = s ; tilt = t ; year = y } =
      (Array.make_matrix M.rows M.cols '\000', d, t, y, s)
    let mask_col = Array.make M.mask_cols '\000'
    let mask_row = Array.make M.mask_rows '\000'
    let rows = M.rows
    let cols = M.cols
    let leds (_, d, _, _, _) = d
    let tilt (_, _, t, _, _) = t
    let set_key g r c i = match g with
      | (s, _, _, _, _) -> Array.set (Array.get s r) c i; g
    let get_key (s, _, _, _, _) r c   = Array.get (Array.get s r) c
  end

module MonomeGrid064 =
  MakeMonomeGrid(struct
                  type t = Grid064
                  let rows = 8
                  let cols = 8
                  let mask_rows = 1
                  let mask_cols = 1
                  let cardinality = 64
                  let editions = [ {series = Grid40h       ; year = Y2006; leds = Binary     ; tilt = ModUniaxis}
                                 ; {series = Grid40h       ; year = Y2007; leds = Binary     ; tilt = ModUniaxis}
                                 ; {series = Grid40hSE     ; year = Y2007; leds = Binary     ; tilt = Uniaxis}
                                 ; {series = Grid40hKit    ; year = Y2007; leds = Binary     ; tilt = TiltUnknown}
                                 ; {series = Grid40hKit    ; year = Y2008; leds = Binary     ; tilt = TiltUnknown}
                                 ; {series = Grid40hKit    ; year = Y2009; leds = Binary     ; tilt = TiltUnknown}
                                 ; {series = Grid40hKit    ; year = Y2010; leds = Binary     ; tilt = TiltUnknown}
                                 ; {series = GridSeries064 ; year = Y2007; leds = Binary     ; tilt = Uniaxis}
                                 ; {series = GridSeries064 ; year = Y2008; leds = Binary     ; tilt = Uniaxis}
                                 ; {series = GridSeries064 ; year = Y2009; leds = Binary     ; tilt = Uniaxis}
                                 ; {series = GridSeries064 ; year = Y2010; leds = Binary     ; tilt = Uniaxis}
                                 ; {series = GridMK8x8     ; year = Y2010; leds = LedUnknown ; tilt = TiltUnknown}
                                 ; {series = GridGrey064   ; year = Y2010; leds = Binary     ; tilt = Uniaxis}
                                 ; {series = GridGrey064   ; year = Y2011; leds = Binary     ; tilt = Uniaxis}
                                 ; {series = GridGrey064   ; year = Y2012; leds = Binary     ; tilt = Uniaxis}
                                 ; {series = Grid064       ; year = Y2011; leds = Quaternary ; tilt = Triaxis}
                                 ; {series = Grid064       ; year = Y2012; leds = Sedenary   ; tilt = Triaxis} ]
                end)

module MonomeGrid128 =
  MakeMonomeGrid(struct
                  type t = Grid128
                  let rows = 8
                  let cols = 16
                  let mask_rows = 1
                  let mask_cols = 2
                  let cardinality = 128
                  let editions = [ {series = GridSeries128; year = Y2007; leds = Binary;     tilt = Nulliary}
                                 ; {series = GridSeries128; year = Y2008; leds = Binary;     tilt = Nulliary}
                                 ; {series = GridSeries128; year = Y2009; leds = Binary;     tilt = Nulliary}
                                 ; {series = GridSeries128; year = Y2010; leds = Binary;     tilt = Nulliary}
                                 ; {series = GridMK8x16   ; year = Y2010; leds = LedUnknown; tilt = TiltUnknown}
                                 ; {series = GridGrey128  ; year = Y2010; leds = Binary;     tilt = Uniaxis} (* Tilt? *)
                                 ; {series = GridGrey128  ; year = Y2011; leds = Binary;     tilt = Uniaxis} (* Tilt? *)
                                 ; {series = GridGrey128  ; year = Y2012; leds = Binary;     tilt = Uniaxis} (* Tilt? *)
                                 ; {series = Grid128      ; year = Y2011; leds = Quaternary; tilt = Triaxis}
                                 ; {series = Grid128      ; year = Y2012; leds = Sedenary;   tilt = Triaxis}
                                 ; {series = Grid128      ; year = Y2013; leds = Sedenary;   tilt = Nulliary}
                                 ; {series = Grid128      ; year = Y2014; leds = Sedenary;   tilt = Nulliary}
                                 ; {series = Grid128      ; year = Y2015; leds = Sedenary;   tilt = Nulliary} ]
                end)

module MonomeGrid256 =
  MakeMonomeGrid(struct
                  type t = Grid256
                  let rows = 16
                  let cols = 16
                  let mask_rows = 2
                  let mask_cols = 2
                  let cardinality = 256
                  let editions = [ {series = GridSeries256; year = Y2007; leds = Binary;     tilt = Nulliary}
                                 ; {series = GridSeries256; year = Y2008; leds = Binary;     tilt = Nulliary}
                                 ; {series = GridSeries256; year = Y2009; leds = Binary;     tilt = Nulliary}
                                 ; {series = GridSeries256; year = Y2010; leds = Binary;     tilt = Nulliary}
                                 ; {series = GridMK16x16  ; year = Y2010; leds = LedUnknown; tilt = TiltUnknown}
                                 ; {series = Grid256      ; year = Y2011; leds = Quaternary; tilt = Triaxis}
                                 ; {series = Grid256      ; year = Y2012; leds = Sedenary;   tilt = Triaxis}
                                 ]
                end)

(* TODO: MonomeGrid512 implemented as 2x256 *)
