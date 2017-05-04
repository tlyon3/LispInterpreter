
# If you're getting errors about not being able to load this, you may
# need to add the current directory to the module load path:
#
push!(LOAD_PATH, ".")

#
# This is how I make sure it's reloaded when something changes:
# workspace(); reload("CI5"); using CI5;
#
# This is a helper function to run through a bunch of tests in a file:
# CI5.calcf( "./tests.txt" )
#

module HPInt

using Error
using Lexer
using Cairo
using Images
export parse, calc, NumVal, ClosureVal, analyze, MatrixVal

#
# ===================================================
#

abstract Environment
abstract OWL
abstract RetVal

type NumVal <: RetVal
  n::Real
end

type ClosureVal <: RetVal
    params::Array{Symbol}
    body::OWL
    env::Environment  # this is the environment at definition time!
end

type mtEnv <: Environment
end

type Binop <: OWL
  op::Function
  lhs::OWL
  rhs::OWL
end

type MultiBinop <: OWL
  op::Function
  args::Array{Any,1}
end

type CEnvironment <: Environment
  names::Array{Symbol}
  values::Array{RetVal}
  parent::Environment
end

type FunDef <: OWL
    formal_parameters::Array{Symbol}
    fun_body::OWL
end

type FunApp <: OWL
    fun_expr::OWL
    arg_expr::Array{OWL}
end

type Num <: OWL
    n::Real
end

type Neg <: OWL
  num::OWL
end

type If0 <: OWL
  condition::OWL
  zero_branch::OWL
  nonzero_branch::OWL
end

type With <: OWL
  binding_exprs::Array{Array{Any}}
  body::OWL
end

type Id <: OWL
  names::Symbol
end

type Collatz <: OWL
  n::Num
end

type And <: OWL
  arg_exprs::Array{Any, 1}
end

type MatrixVal <: RetVal
  n::Array{Float32,2}
end

type RenderText <: OWL
  words::AbstractString
  xpos::OWL #this should evaluate to a NumVal
  ypos::OWL #this should evaluate to a NumVal
end

type SimpleLoad <: OWL
  file::AbstractString
end

type SimpleSave <: OWL
  file::OWL #this should evaluate to a MatrixVal
  location::AbstractString
end

type Emboss <: OWL
  image::OWL #this should evaluate to a MatrixVal
end

type DropShadow <: OWL
  image::OWL #this should evaluate to a MatrixVal
end

type InnerShadow <: OWL
  image::OWL #this should evaluate to a MatrixVal
end

type Min <: OWL
  a::OWL #this should evaluate to a MatrixVal or NumVal
  b::OWL #this should evaluate to a MatrixVal or NumVal
end

type Max <: OWL
  a::OWL #this should evaluate to a MatrixVal or NumVal
  b::OWL #this should evaluate to a MatrixVal or NumVal
end
#
# ===================================================
#
type Assign <: OWL
  name::Symbol
  value::OWL
end

type Sequence <: OWL
  exprs::Array{OWL}
end

type Reference
  env::CEnvironment
  index::Int
end

#
# ===================================================
#


  function simple_load( img_path::AbstractString )
    im = Images.load( img_path );
    tmp = Images.separate(im);
    d = Images.data( tmp );
    d = d[:,:,1];  # just the r channel
    d = convert( Array{Float32,2}, d );
    return d
  end

  function simple_save( output::Array, img_path::AbstractString )
      output[ output .> 1.0 ] = 1.0
      output[ output .< 0.0 ] = 0.0
      tmpc = convert( Array{UInt32,2}, floor(output*255.0) )
      tmp_output =  tmpc + tmpc*256 + tmpc*65536 + 0xff000000
      c2 = CairoImageSurface( transpose(tmp_output), Cairo.FORMAT_ARGB32 )
      write_to_png( c2, img_path )
      return 42
  end

  function render_text( text_str::AbstractString, xpos, ypos )

    data = Matrix{UInt32}( 256, 256 );
    c = CairoImageSurface( data, Cairo.FORMAT_ARGB32 );
    cr = CairoContext( c );

    set_source_rgb( cr, 1., 1., 1. );
    rectangle( cr, 0., 0., 256., 256. );
    fill( cr );

    set_source_rgb( cr, 0, 0, 0 );
    select_font_face( cr, "Sans", Cairo.FONT_SLANT_NORMAL,
                      Cairo.FONT_WEIGHT_BOLD );
    set_font_size( cr, 90.0 );

    move_to( cr, xpos, ypos );
    show_text( cr, text_str );

    # tmp is an Array{UInt32,2}
    tmp = cr.surface.data;

    # grab just the blue channel, and convert the array to an array of floats
    tmp2 = convert( Array{Float32,2}, tmp & 0x000000ff ) / 255.0;
    tmp2 = convert( Array{Float32,2}, tmp2 );

    return tmp2
  end

  function emboss( img::Array )
    f = [ -2. -1. 0.
          -1.  1. 1.
           0.  1. 1. ];
    f = convert( Array{Float32,2}, f );

    es = conv2( f, img );
    es = es[1:256,1:256];
    return es
  end

  function drop_shadow( img::Array )
    foo = convert( Array{Float32,2}, gaussian2d(5.0,[25,25]) );
    foo = foo / maximum(foo);
    ds = conv2( foo, img );
    ds = ds[13:256+12,13:256+12];
    ds = ds / sum(foo);
    return ds
  end

  # assumes img is black-on-white
  function inner_shadow( img::Array )
    foo = convert( Array{Float32,2}, gaussian2d(5.0,[25,25]) );
    foo = foo / maximum(foo);
    is = conv2( foo, 1.0-img );
    is = is[8:251+12,8:251+12];
    is = is / sum(foo);
    is = max( is, img );
    return is
  end

  #----------------------------------
  function new_env(parent::CEnvironment)
    emptynames = []
    emptyvalues = []
    return CEnvironment(emptynames, emptyvalues, parent)
  end

  function add_to_env(env::CEnvironment, name::Symbol, value::RetVal)
      push!(env.values, value)
      push!(env.names, name)
  end

  function get_value(env::CEnvironment, name::Symbol)
    index = findfirst(env.names, name)
    if index == 0
      return get_value(env.parent, name)
    else
      return env.values[index]
    end
  end

  function get_value(reference::Reference)
    return reference.env.values[reference.index]
  end

  function get_value(env::mtEnv, name::Symbol)
    throw(LispError("Couldn't find variable: ", name))
  end

  function get_reference(env::CEnvironment, name::Symbol)
    index = findfirst(env.names, name)
    if index == 0
      return get_reference(env.parent, name)
    else
      return Reference(env, index)
    end
  end

  function get_env(env::mtEnv, name::Symbol)
    throw(LispError("Couldn't find variable: ", name))
  end

  function set_reference(ref::Reference, value::RetVal)
    ref.env.values[ref.index] = value
  end
  #----------------------------------
#
# ===================================================
#

function pp( ast::Num, depth::Int )
    print( ast.n )
end

function pp( ast::If0, depth::Int )
    print( "(if0 " )
    pp( ast.condition, depth+1 )
    print( " \n" )
    pp( ast.zero_branch, depth+1 )
    print( " \n" )
    pp( ast.nonzero_branch, depth+1 )
    print( ")" )
end

function pp( ast::Binop, depth::Int )
    print( "(+ " )
    pp( ast.lhs, depth+1 )
    print( " " )
    pp( ast.rhs, depth+1 )
    print( ")" )
end

function pp(ast::FunDef, depth::Int)
  print("(lambda (")
  for i in ast.formal_parameters
    print(i, " ")
  end
  print(") ")
  pp(ast.fun_body, depth+1)
  print(")")
end

function pp(ast::FunApp, depth::Int)
  print("(")
  pp(ast.fun_expr, depth+1)
  print(" ")
  for i in ast.arg_expr
    pp(i, depth)
    print(" ")
  end
  print(")")
end

function pp(ast::Id, depth::Int)
  print(ast.names)
end

function pp(num::Num, depth::Int)
  print(num.n)
end

function pp(ast::With, depth::Int)
  print( "(with (")
  for i in ast.binding_exprs
    print(" (")
    pp(i, depth+1)
    print(")")
  end
  print(") ")
  pp(ast.body, depth+1)
  print(")")
end

function pp(array::Array{Any, 1}, depth::Int)
  for i in array
    pp(i, depth+1)
    print(" ")
  end
end

function pp(symbol::Symbol, depth::Int)
  print(symbol)
end

function pp(ast::If0, depth::Int)
  print("(if0 ")
  pp(ast.condition, depth+1)
  print( "\n\t")
  pp(ast.zero_branch, depth+1)
  print("\n\t")
  pp(ast.nonzero_branch, depth+1)
  print(")")
end

function pp(ast::Binop, depth::Int)
  print("(")
  print(ast.op)
  print(" ")
  pp(ast.lhs, depth+1)
  print(" ")
  pp(ast.rhs, depth+1)
  print(")")
end

function pp(ast::And, depth::Int)
  print("(and ")
  for i in ast.arg_exprs
    pp(i, depth+1)
    print(" ")
  end
  print(")")
end

function pp(ast::MultiBinop, depth::Int)
  print("(")
  print(ast.op)
  for i in ast.args
    print(" ")
    pp(i, depth+1)
  end
  print(")")
end

#
# ===================================================
#


function parse( expr::Real )
  return Num( expr )
end

function parse( expr::Symbol )
  return Id( expr )
end

function parse( expr::Array{Any} )
  op_symbol = expr[1]

  if op_symbol == :+
      if size(expr, 1) > 3
        parsed_args = []
        for i in expr[2:end]
          push!(parsed_args, parse(i))
        end
        return MultiBinop(+, parsed_args)
      else
        lhs = parse( expr[2] )
        rhs = parse( expr[3] )
        return Binop(+, lhs, rhs )
    end

    elseif op_symbol == :-
      if length(expr) == 2
        num = parse( expr[2] )
        return Neg(num)
      elseif length(expr) == 3
        lhs = parse( expr[2] )
        rhs = parse( expr[3] )
        return Binop(-,lhs,rhs)
      else
        throw( LispError("Too many arguments!") )
      end

  elseif op_symbol == :*
    if length(expr) > 3
      throw( LispError("Too many arguments!"))
    end
    lhs = parse(expr[2])
    rhs = parse(expr[3])
    return Binop(*,lhs,rhs)

  elseif op_symbol == :/
    if length(expr) > 3
      throw( LispError("Too many arguments!"))
    end
    lhs = parse(expr[2])
    rhs = parse(expr[3])
    return Binop(/,lhs,rhs)

  elseif op_symbol == :collatz
    if length(expr) > 2
      throw( LispError("Too many arguments!"))
    end
    num = parse(expr[2])
    return Collatz(num)

  elseif op_symbol == :mod
    if length(expr) > 3
      throw( LispError("Too many arguments!"))
    end
    lhs = parse(expr[2])
    rhs = parse( expr[3] )
    return Binop(%, lhs, rhs)

  elseif op_symbol == :if0
      condition = parse( expr[2] )
      zero_branch = parse( expr[3] )
      nonzero_branch = parse( expr[4] )
      return If0( condition, zero_branch, nonzero_branch )

  elseif op_symbol == :and
    parsed_exprs = []
    for i in expr[2:end]
      push!(parsed_exprs, parse(i))
    end
    return And(parsed_exprs)


  elseif op_symbol == :with
    ids = []
      if typeof(expr[2]) != Array{Any,1}
        throw( LispError( "BindingExpressions must be an array" ) )
      else
        binding_exprs = expr[2]
        parsed_exprs = []
        #iterate through buinding_exprs and parse each expr
          for i in binding_exprs
            if typeof(i) != Array{Any, 1}
              throw( LispError( "Each subexpr must be an array" ) )
            elseif size(i,1) != 2
              throw( LispError( "Each subexpr must be size 2" ) )
            else
              # input is valid
              # process input
              parsed_ex = []
              push!(ids, i[1])
              push!(parsed_ex, i[1])
              push!(parsed_ex, parse(i[2]))
              push!(parsed_exprs, parsed_ex)
            end
          end
      body = parse( expr[3] )
      check_array = unique(ids)
      if size(check_array,1 ) != size(ids, 1)
        throw(LispError("Can't have multiple params with same id!"))
      end
      return_value = With(parsed_exprs, body)
      return return_value
    end

  elseif op_symbol == :lambda
    params = expr[2]
    check_parms = unique(params)
    # ensure param names are unique
    if size(check_parms,1 ) != size(params, 1)
      throw(LispError("Can't have multiple params with same id!"))
    end
    body = expr[3]
    parsedparms = []

    rval = FunDef(params, parse(body))
    return rval

#-----------------------------------

  elseif op_symbol == :set
    return Assign(expr[2], parse(expr[3]))

  elseif op_symbol == :sequence
    return Sequence(map(parse, expr[2:end]))

#-----------------------------------

  elseif op_symbol == :simple_load
    if size(expr,1) != 2
      throw( LispError("Argument exception!") )
    end
      return SimpleLoad(expr[2])

  elseif op_symbol == :simple_save
    if size(expr,1) != 3
      throw( LispError("Argument exception!") )
    end
      matrix = parse(expr[2])
      # if typeof(matrix) != MatrixVal
      #   throw(LispError("Value not a matrix!"))
      # else
        return SimpleSave(matrix, expr[3])

  elseif op_symbol == :render_text
    if size(expr,1) != 4
      throw( LispError("Argument exception!") )
    end
      xpos = parse(expr[3])
      ypos = parse(expr[4])
      # if (typeof(xpos) != NumVal) || (typeof(ypos) != NumVal)
      #   throw( LispError("Both values are not NumVals!") )
      # else
        return RenderText(expr[2], xpos, ypos)

  elseif op_symbol == :emboss
    if size(expr,1) != 2
      throw( LispError("Argument exception!") )
    end
      image = parse(expr[2])
      # if typeof(image) != MatrixVal
      #   throw(LispError("Value not a matrix!"))
      # else
        return Emboss(image)

  elseif op_symbol == :drop_shadow
    if size(expr,1) != 2
      throw( LispError("Argument exception!") )
    end
      image = parse(expr[2])
      # if typeof(image) != MatrixVal
      #   throw(LispError("Value not a matrix!"))
      # else
        return DropShadow(image)

  elseif op_symbol == :inner_shadow
    if size(expr,1) != 2
      throw( LispError("Argument exception!") )
    end
      image = parse(expr[2])
      # if typeof(image) != MatrixVal
      #   throw(LispError("Value not a matrix!"))
      # else
        return InnerShadow(image)

  elseif op_symbol == :min
    if size(expr,1) != 3
      throw( LispError("Argument exception!") )
    end
      matrix_a = parse(expr[2])
      matrix_b = parse(expr[3])
      # if typeof(matrix_a) != MatrixVal && typeof(matrix_a) != NumVal
      #   throw(LispError("Arguments must be either a MatrixVal or a NumVal!"))
      # elseif typeof(matrix_b) != MatrixVal && typeof(matrix_b) != NumVal
      #   throw(LispError("Arguments must be either a MatrixVal or a NumVal!"))
      # else
        return Min(matrix_a, matrix_b)

  elseif op_symbol == :max
    if size(expr,1) != 3
      throw( LispError("Argument exception!") )
    end
      matrix_a = parse(expr[2])
      matrix_b = parse(expr[3])
      # if typeof(matrix_a) != MatrixVal && typeof(matrix_a) != NumVal
      #   throw(LispError("Arguments must be either a MatrixVal or a NumVal!"))
      # elseif typeof(matrix_b) != MatrixVal && typeof(matrix_b) != NumVal
      #   throw(LispError("Arguments must be either a MatrixVal or a NumVal!"))
      # else
        return Max(matrix_a, matrix_b)


  else
    fundef = expr[1]
    fundefparsed = parse(fundef)
    params = expr[2:end]
    if size(fundefparsed.formal_parameters, 1) != size(params,1 )
      throw(LispError("Required and given params don't match!"))
    end
    parsedparms = []
    for i in params
      push!(parsedparms, parse(i))
    end

    return FunApp(fundefparsed, parsedparms)

  end
end

# the default case
function parse( expr::Any )
  throw( LispError("Invalid type $expr") )
end

function analyze(ast::OWL)
  throw( LispError( "Unknown node!"))
end

function analyze(ast::Num)
  return ast
end

function analyze(ast::Id)
  return ast
end

function analyze(ast::If0)
  cond = analyze(ast.condition)

  if typeof(cond) == Num
    if cond.n == 0
      return analyze(ast.zero_branch)
    else
      return analyze(ast.nonzero_branch)
    end
  else
    return If0(cond, analyze(ast.zero_branch), analyze(ast.nonzero_branch))
  end
end

function analyze(ast::FunApp)
  analyzed_args = []
  for i in ast.arg_expr
    push!(analyzed_args, analyze(i))
  end
  return FunApp(analyze(ast.fun_expr), analyzed_args)
end

function analyze(ast::FunDef)
  return FunDef(ast.formal_parameters, analyze(ast.fun_body))
end

function analyze(ast::Binop)
  lhs = analyze(ast.lhs)
  rhs = analyze(ast.rhs)

  if typeof(lhs) == Num && typeof(rhs) == Num
    return Num(ast.op(lhs.n, rhs.n))
  else
    return Binop(ast.op, lhs, rhs)
  end
end

function analyze(ast::MultiBinop)
  if size(ast.args, 1) == 2
    return Binop(ast.op, analyze(ast.args[1]), analyze(ast.args[2]))
  else
    lhs = analyze(ast.args[1])
    rhs = analyze(MultiBinop(ast.op, ast.args[2:end]))
    return Binop(ast.op, lhs, rhs)
  end
end

function analyze(ast::With)
  symbols = []
  analyzed_exprs = []
  for i in ast.binding_exprs
    push!(symbols, i[1])
    push!(analyzed_exprs, analyze(i[2]))
  end
  fun_def = FunDef(symbols, analyze(ast.body))
  return FunApp(fun_def, analyzed_exprs)
end

function analyze(ast::And)
    if size(ast.arg_exprs, 1) == 1
      return If0(ast.arg_exprs[1], Num(0), Num(1))
    else
      return If0(ast.arg_exprs[1], Num(0), analyze(And(ast.arg_exprs[2:end])))
    end
end

function analyze(owl::SimpleSave)
  matrix = analyze(owl.file)
  return SimpleSave(matrix, owl.location)
end

function analyze(owl::RenderText)
  xpos = analyze(owl.xpos)
  ypos = analyze(owl.ypos)
  return RenderText(owl.words, xpox, ypox)
end

function analyze(owl::Emboss)
  image = analyze(owl.image)
  return Emboss(image)
end

function analyze(owl::DropShadow)
  image = analyze(owl.image)
  return DropShadow(image)
end

function analyze(owl::InnerShadow)
  image = analyze(owl.image)
  return InnerShadow(image)
end

function analyze(owl::Min)
  a = analyze(owl.a)
  b = analyze(owl.b)
  return Min(a,b)
end

function analyze(owl::Max)
  a = analyze(owl.a)
  b = analyze(owl.b)
  return Max(a,b)
end

#
# ===================================================
#

# convenience function to make everything easier
function calc( expr::AbstractString )
  return calc( parse( Lexer.lex(expr) ) )
end

# evaluate a series of tests in a file
function calcf( fn::AbstractString )
  f = open( fn )

  cur_prog = ""
  for ln in eachline(f)
      ln = chomp( ln )
      if length(ln) == 0 && length(cur_prog) > 0
          println( "" )
          println( "--------- Evaluating ----------" )
          println( cur_prog )
          println( "---------- Returned -----------" )
          try
              println( calc( cur_prog ) )
          catch errobj
              println( ">> ERROR: lxd" )
              lxd = Lexer.lex( cur_prog )
              println( lxd )
              println( ">> ERROR: ast" )
              ast = parse( lxd )
              println( ast )
              println( ">> ERROR: rethrowing error" )
              throw( errobj )
          end
          println( "------------ done -------------" )
          println( "" )
          cur_prog = ""
      else
          cur_prog *= ln
      end
  end

  close( f )
end

# ===================================================

function calc( ast::OWL )
  return calc( ast, mtEnv() )
end

function calc( ae::Num, env::Environment )
  return NumVal( ae.n )
end

function calc(ae::Binop, env::Environment)
  left = @spawn calc(ae.lhs, env)
  right = @spawn calc(ae.rhs, env)
  right = fetch(right)
  if (ae.op == /) && right.n == 0
    throw(LispError("Can't divide by 0!"))
  end
  left = fetch(left)
  #both are MatrixVal
  if typeof(left) == MatrixVal && typeof(right) ==  MatrixVal
    if ae.op == *
      return MatrixVal(left.n .* right.n)
    elseif ae.op == /
      return MatrixVal(left.n ./ right.n)
    else
      return MatrixVal(ae.op(left.n, right.n))
    end

  #left is MatrixVal
  elseif typeof(left) == MatrixVal
    return MatrixVal(ae.op(left.n, right.n))

  #right is MatrixVal
  elseif  typeof(right) == MatrixVal
    return MatrixVal(ae.op(left.n, right.n))

  #both are NumVal
  else
    return NumVal(ae.op(left.n, right.n))
  end
end

function calc(ae::Neg, env::Environment)
  num = calc(ae.num)
  return NumVal((-(num.n)))
end

function calc( ae::If0, env::Environment )
  cond = calc( ae.condition, env )
  if cond.n == 0
    return calc( ae.zero_branch, env )
  else
    return calc( ae.nonzero_branch, env )
  end
end

function calc( ae::With, env::Environment )
  binding_exprs = ae.binding_exprs

  names = []
  # values = []
  # for i in binding_exprs
  #   push!(names, i[1])
  #   calcd = calc(i[2], env)
  #   push!(values, calcd)
  # end
  values = pmap(expr -> calc(expr[2], env), binding_exprs)
  for i in binding_exprs
    push!(names, i[1])
  end
  check_array = unique(names)
  if size(check_array,1 ) != size(names, 1)
    throw(LispError("Can't have multiple params with same id!"))
  end
  extended_env = CEnvironment(names, values, env)
  return calc(ae.body, extended_env)
end

function calc( ae::Id, env::Environment )
  if env == mtEnv()
    println(ae.names)
    throw( LispError( "WARGH! Couldn't find symbol!") )
  end
  index = findfirst(env.names, ae.names)
  if index == 0
    return calc(ae, env.parent)
  else
    return env.values[index]
  end
end

function calc(ae::MultiBinop, env::Environment)
  return calc(analyze(ae))
end

function calc( ae::FunDef, env::Environment )

    return ClosureVal( ae.formal_parameters, ae.fun_body, env )
end

function calc( ae::FunApp, env::Environment )
    fundef = ae.fun_expr
    argexpr = ae.arg_expr
    the_closure = @spawn calc(fundef, env)
    values = pmap(calc, argexpr)
    clo = fetch(the_closure)

    closureparams = clo.params
    new_env = CEnvironment(closureparams, values, the_closure.env)
    rval = calc(the_closure.body, new_env)
    return rval
end

function calc(expr::Collatz, env::Environment)
  return NumVal(collatz(expr.n.n))
end

function calc(owl::SimpleSave, env::Environment)
  file = calc(owl.file, env)
  if typeof(file) != MatrixVal
    throw(LispError("File is not a MatrixVal!"))
  end

  simple_save(file.n, owl.location)
end

function calc(owl::SimpleLoad, env::Environment)
  image = owl.file
  if typeof(image) != ASCIIString
    throw(LispError("File is not a string!"))
  end
  return MatrixVal(simple_load(image))
end

function calc(owl::RenderText, env::Environment)
  xpos = @spawn calc( owl.xpos, env)
  ypos = @spawn calc( owl.ypos, env)

  xpos = fetch(xpos)
  ypos = fetch(ypos)
  if typeof(xpos) != NumVal || typeof(ypos) != NumVal
    throw(LispError("Positions are not NumVals!"))
  end

  rval = render_text(owl.words, xpos.n, ypos.n)

  return MatrixVal( rval )
end

function calc(owl::Emboss, env::Environment)
  image = calc(owl.image, env)
  if typeof(image) != MatrixVal
    throw(LispError("Image is not a MatrixVal!"))
  end

  rval = emboss(image.n)
  return MatrixVal(rval)
end

function calc(owl::DropShadow, env::Environment)
  image = calc(owl.image, env)
  if typeof(image) != MatrixVal
    throw(LispError("Image is not a MatrixVal!"))
  end


  rval = drop_shadow(image.n)
  return MatrixVal(rval)
end

function calc(owl::InnerShadow, env::Environment)
  image = calc(owl.image, env)
  if typeof(image) != MatrixVal
    throw(LispError("Image is not a MatrixVal!"))
  end

  rval = inner_shadow(image.n)
  return MatrixVal(rval)
end

function calc(owl::Min, env::Environment)
  a = @spawn calc(owl.a, env)
  b = @spawn calc(owl.b, env)
  a = fetch(a)
  b = fetch(b)
  if typeof(a) != MatrixVal && typeof(a) != NumVal
    throw(LispError("Not a MatrixVal or NumVal!"))
  elseif typeof(b) != MatrixVal && typeof(b) != NumVal
    throw(LispError("Not a MatrixVal or a NumVal"))
  end
  rval = min(a.n, b.n)
  if isa(rval, Array)
    return MatrixVal(rval)
  else
    return NumVal(rval)
  end
end

function calc(owl::Max, env::Environment)
  a = @spawn calc(owl.a, env)
  b = @spawn calc(owl.b, env)
  a = fetch(a)
  b = fetch(b)
  if typeof(a) != MatrixVal && typeof(a) != NumVal
    throw(LispError("Not a MatrixVal or NumVal!"))
  elseif typeof(b) != MatrixVal && typeof(b) != NumVal
    throw(LispError("Not a MatrixVal or a NumVal"))
  end

  rval = max(a.n, b.n)

  if isa(rval, Array)
    return MatrixVal(rval)
  else
    return NumVal(rval)
  end
end

#
# ===================================================
#
function calc(expr::Assign, env::Environment)
  reference = get_reference(env, expr.name)
  value = calc(expr.value, env)
  set_reference(reference, value)
end

function calc(expr::Sequence, env::Environment)
  result = NumVal(0)
  for expr in expr.exprs
    result = calc(expr, env)
  end
  return result
end
#
# ===================================================
#

function collatz( n::Real )
  return collatz_helper( n, 0 )
end

function collatz_helper( n::Real, num_iters::Int )
  if n < 0
    throw(LispError("Collatz number is negative!"))
  end
  if n == 1
    return num_iters
  end
  if mod(n,2)==0
    return collatz_helper( n/2, num_iters+1 )
  else
    return collatz_helper( 3*n+1, num_iters+1 )
  end
end

end # module
