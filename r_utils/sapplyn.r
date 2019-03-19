# allows the application of sapply to the arg_numth argument of the function f. The rest of the arguments will be listed in order
sapplyn = function(X, f, arg_num, ...) {
  new_fun = function(arg1, ...) {
    # extract ... into a list
    input_args = unlist(list(...))

    if(arg_num > length(input_args)) {
      # if arg_num is greater than or equal to the number of arguments apply as the last parameter
      output_args = as.list(unlist(list(input_args, arg1)))
      do.call(f, output_args)
    } else {
      # otherwise place parameter at the appropriate location within the parameter list
      output_args = list(input_args[1:arg_num-1], arg1, input_args[arg_num:length(input_args)])
      do.call(f, output_args)
    }
  }
  # if arg_num equals 1 this is equivalent to sapply
  # if less than 1 just proceed as if 1
  sapply(X, ifelse(arg_num<=1, f, new_fun), ...)
}