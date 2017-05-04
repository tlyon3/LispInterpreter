
# If you're getting errors about not being able to load this, you may
# need to add the current directory to the module load path:
#
 push!(LOAD_PATH, ".")

module ExtInt

using Error
using Lexer
export parse, calc, Num, Plus, Minus

#
# ===================================================
#

function calc(expr::AbstractString)
  return calc(parse(Lexer.lex(expr)))
end

# This is the abstract class "arithmetic expression"
abstract Environment
abstract RetVal
abstract OWL

type mtEnv <: Environment
end

type CEnvironment
  name::Symbol
  value::Real
  parent::Environment #this is the environment at definition!
end

type NumVal <: RetVal
  n::Real
end

type ClosureVal <: RetVal
  params::Array{Symbol}
  body::OWL
  env::Environment
end

type Num <: OWL
  n::Real
end

type Binop <: OWL
  op::Function
  lhs::OWL
  rhs::OWL
end

type Neg <: OWL
  num::OWL
end

type If0 <: OWL
  pred::OWL
  zeropath::OWL
  nonzeropath::OWL
end

type With <: OWL
  name::Symbol
  binding_expr::OWL
  body::OWL
end

type Id <: OWL
  name::Symbol
end

type FunDef <: OWL
  formal_parameters::Array{Symbol}
  fun_body::OWL
end

type FunApp <: OWL
  fun_expr::OWL
  arg_expr::OWL
end
#
# ===================================================
#

function parse( expr::Real )
  return Num( expr ) # return a "Num" type object, with the "n" member set to "expr"
end

function parse(sym::Symbol)
  return Id(sym)
end

function parse( expr::Array{Any} )
  # should be an array of length 3 - something like "(+ lhs rhs)"
  op_symbol = expr[1]

  if op_symbol == :+
    lhs = parse( expr[2] )
    rhs = parse( expr[3] )
    return Binop(+,lhs,rhs)
    #return Plus( lhs, rhs )

  elseif op_symbol == :-
    if length(expr) == 2
      num = parse( expr[2] )
      return Neg(num)
    else
      lhs = parse( expr[2] )
      rhs = parse( expr[3] )
      return Binop(-,lhs,rhs)
      #return Minus( lhs, rhs )
    end

  elseif op_symbol == :*
    lhs = parse(expr[2])
    rhs = parse(expr[3])
    return Binop(*,lhs,rhs)
    #return Mult(lhs, rhs)

  elseif op_symbol == :/
    lhs = parse(expr[2])
    rhs = parse(expr[3])
    return Binop(/,lhs,rhs)
    #return Div(lhs,rhs)

  elseif op_symbol == :mod
    lhs = parse(expr[2])
    rhs = parse( expr[3] )
    return Binop(%, lhs, rhs)
    #return Mod(lhs,rhs)

  elseif op_symbol == :if0
    cond = parse(expr[2])
    zeropath = parse(expr[3])
    nonzeropath = parse(expr[4])
    return If0(cond, zeropath, nonzeropath)

  elseif op_symbol == :with
    name = expr[2]
    bexpr = parse(expr[3])
    body = parse(expr[4])
    return With(name, bexpr, body)

  elseif op_symbol == :lambda
    return FunDef( expr[2], parse(expr[3]) )

    #else
    #return FunVal
  else
    return Id( op_symbol)
  end
end

# the default case
function parse( expr::Any )
  throw( LispError("Invalid type $expr") )
end

#
# ===================================================
#

# just a number - return it!
function calc( ae::Num, env::Environment)
  # return NumVal(ae.n)
  return ae.n
end

function calc(ae::Binop, env::Environment)
  left = calc(ae.lhs)
  right = calc(ae.rhs)
  if (ae.op == /) && right == 0
    throw(LispError("Can't divide by 0!"))
  end
  return NumVal(ae.op(left, right))
  return ae.op(left, right)
end

function calc(ae::If0,env::Environment)
    valueofcondition = calc(ae.cond)
    if valueofcondition == 0
      return calc(ae.zeropath, env)
    else
      return calc(ae.nonzeropath, env)
    end
end

function calc(ae::Neg, env::Environment)
  num = calc(ae.num)
  return NumVal((-num))
  #return -num
end

function calc( expr::OWL )
  return calc(expr, mtEnv())
end

function calc(expr::With, env::Environment)
  value = calc(expr.binding_expr, env)
  new_env = CEnvironment(expr.name, value, env)
  return calc(expr.body, new_env)
end

function calc(ae::Id, env::Environment)
  if env == mtEnv()
    throw( LispError("No such symbol!"))
  elseif env.name == ae.name
    return env.value
  else
    return calc(ae, env.parent)
  end
end

function calc(ae::FunDef, env::Environment)
  return ClosureVal( ae.formal_parameters, ae.fun_body, env)
end

function calc(ae::FunApp, env::Environment)
  closure = calc( ae.fun_expr, env )
  new_env = CEnvironment( closure.param, calc(ae.arg_expr), closure.env )
  return calc( closure, new_env )
end

end # module
