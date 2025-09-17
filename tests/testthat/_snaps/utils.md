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

