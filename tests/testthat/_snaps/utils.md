# map_chr

    Code
      map_chr(1:2, function(x) 1)
    Condition
      Error in `vapply()`:
      ! values must be type 'character',
       but FUN(X[[1]]) result is type 'double'

# map_int

    Code
      map_int(1:2, function(x) 1)
    Condition
      Error in `vapply()`:
      ! values must be type 'integer',
       but FUN(X[[1]]) result is type 'double'

# map_dbl

    Code
      map_dbl(1:2, function(x) "foo")
    Condition
      Error in `vapply()`:
      ! values must be type 'double',
       but FUN(X[[1]]) result is type 'character'

# map_lgl

    Code
      map_lgl(1:2, function(x) 1)
    Condition
      Error in `vapply()`:
      ! values must be type 'logical',
       but FUN(X[[1]]) result is type 'double'

# style_orange [plain]

    Code
      style_orange("orange")
    Output
      <cli_ansi_string>
      [1] orange

# style_orange [ansi]

    Code
      style_orange("orange")
    Output
      <cli_ansi_string>
      [1] [33morange[39m

# style_bg_grey [plain]

    Code
      style_bg_grey("grey background")
    Output
      <cli_ansi_string>
      [1] grey background

# style_bg_grey [ansi]

    Code
      style_bg_grey("grey background")
    Output
      <cli_ansi_string>
      [1] [40mgrey background[49m

# empty_data_frame

    Code
      empty_data_frame(10)
    Output
      # A data frame: 10 x 0
    Code
      empty_data_frame(0)
    Output
      # A data frame: 0 x 0

