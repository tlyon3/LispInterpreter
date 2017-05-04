module Repl
push!(LOAD_PATH,pwd())

using Error
using Lexer
using Base

export repl


expressionRE = r"^(?s)\(.+\)"
parseRE = r"^parse\(.+\)$"
reloadRE = r"^reload$"
loadRE = r"^load\(.+\)$"
helpRE = r"^help$"
testRE = r"^test\([^;]+\;[^;]+\)$"
testFileRE = r"^testFile\([^;]+\;[^;]+\)$"




function loadMod(source)
  modn = source[1:end - 3]
  include(source)
  if isdefined(Main, symbol(modn))
    mod = getfield(Main, symbol(modn))
  else
    println("Couldn't import module")
    return Union{}
  end
  return mod;
end

function repl(source)
  mod = loadMod(source)
  if mod == Union{}
    return
  end
  println()
  println("                 " * faded("               __________   ___                 "))
  println("    Welcome to   " * faded("               |___ / ___/ / _ \\               "))
  println("      the 330    " * faded("                 |_ \\ |_ \\| | | |               "))
  println("       REPL      " * faded("                ___) |__) | |_| |               "))
  println("                 " * faded("               |____/____/ \\___/               "))
  println(faded("      type ^D to exit"))
  println(faded("      type help for options"))
  println()

  while !eof(STDIN)
    try
      src = readExpression()

      if ismatch(expressionRE, src)
        println(interpret(src,mod))
      elseif ismatch(parseRE, src)
          println(mod.parse(Lexer.lex(src[7:end-1])))
      elseif ismatch(reloadRE, src)
        loadMod(source)
      elseif ismatch(loadRE, src)
        nsource = src[6:end-1]
        nmod = loadMod(nsource)
        if nmod != Union{}
          mod = nmod
          source = nsource
        end
      elseif ismatch(testRE, src)
        exprs = split(src[6:end-1],';')
        println(exprs)
        compare(strip(exprs[1]), strip(exprs[2]), mod)
      elseif ismatch(testFileRE, src)
        exprs = split(src[10:end-1],';')
        testFile(strip(exprs[1]), strip(exprs[2]), mod)
      elseif ismatch(helpRE, src)
        printOptions()
      else
        println("Invalid option $src")
        continue
      end
    catch e
      if isa(e,Error.LispError)
        println(colored("!", "red"))
        println(faded(string(typeof(e)) * ":"))
        println(string(e))
      elseif isa(e, InterruptException)
        msg = "Interupted. " * faded("(Use ^D to exit)")
        println("\n" * colored("! ", "red") * msg)
      elseif isa(e, Exception)
        println(colored("! ", "red") * faded("The Julia is showing through..."))
        println(colored("  " * string(e), "red"))
        println(e)
      end
    end
  end
end

function testFile(source, answers, mod)
  try
    sourceFile = readall(open(source))
    answersFile = readall(open(answers))
    sourceArray = split(sourceFile,';')
    answersArray = split(answersFile, ';')
    if length(sourceArray) != length(answersArray)
      println(colored("File error", "red") * "There are a different number of answers then expr")
      return
    end
    for i in 1:(length(sourceArray) - 1)
      compare(strip(sourceArray[i]), strip(answersArray[i]), mod)
    end
  catch e
    println(string(e))
  end
end




isTestRegex = r"^error\:"
function compare(expr, against, mod)
  isError = false;
  testReg = r"."
  if ismatch(isTestRegex, against)
    isError = true;
    testReg = Regex(against[7:end])
  end

  try
    ans = ""
    if ismatch(parseRE, expr)
      ans = string(mod.parse(Lexer.lex(expr[7:end-1])))
    else
      ans = string(interpret(expr,mod))
    end

    ans = replace(ans, Regex(string(mod) * "\."), "")
    if ans == against
      println("Test passed")
    else
      println(colored("TestFailed", "red") * " $expr returned $ans, expected $against")
    end
  catch e
    if isError
      if ismatch(testReg, string(e))
        println("Test passed")
      else
        println(colored("TestFailed", "red") * " $expr threw wrong error, expected $testReg, got $(string(e))")
      end
    else
      println(colored("Test Failed", "red") * " $expr threw $(string(e)) expected $against")
    end
  end
end

function interpret(source,mod)
  mod.calc(mod.parse(Lexer.lex(source)))
end

function printOptions()
  println("Command \t--\tResult")
  println(colored("parse(expr)", "green") *
   "\t--\tWill only call parse on the given expression")
  println(colored("reload", "green") * "\t\t--\tHot reloads the current interpreter")
  println(colored("load(file.jl)", "green") * "\t--\tLoods the given file as the interpreter.")
  println(colored("help", "green") * "\t\t--\tShows this message.")
  println(colored("test(exp; res)", "green") * "\t--\tInterpretes exp, and tests it against res")
  println(colored("testFile(Expfile; AnsFile)", "green") * "\t--\tEach file is semi colon delimited runs each expression
  and tests it against the corresponding answers.")

end

function readExpression()
  exp = ""
  open_parens = 0
  while !eof(STDIN)
    line, parens = read_line(!isempty(strip(exp)) ? "->  " : "...  ")
    open_parens += parens
    exp *= line
    if !isempty(strip(exp)) && open_parens <= 0
      break
    end
  end
  return strip(exp)
end

function input(prompt::AbstractString="")
  print(prompt)
  chomp(readline())
end

## Return touple of user input line and number of unclosed parens
function read_line(prompt)
  line = input(colored(prompt, "grey", "bold"))
  return line, count(x -> x == '(', line) - count(x -> x == ')', line)
end

function colored(text, color, attr=nothing)
  attributes = Dict(
    "bold" => 1,
    "dark" => 2
  )
  colors = Dict(
    "grey" => 30,
    "red" => 31,
    "green" => 32,
    "yellow" => 33,
    "blue" => 34,
    "magenta" => 35,
    "cyan" => 36,
    "white" => 37,
    "reset" => 0
  )

  if "ANSI_COLORS_DISABLED" in keys(ENV)
    return text
  end

  color = @sprintf "\033[%dm" colors[color]
  attr  = (attr != nothing) ? (@sprintf "\033[%dm" attributes[attr]) : ""
  reset = @sprintf "\033[%dm" colors["reset"]

  return color * attr * text * reset
end

function faded(text)
  colored(text, "grey", "bold")
end

end # module
