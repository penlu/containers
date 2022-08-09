#!/usr/bin/env python
from z3 import *

f = open("traces/distinct.txt", "r")

def parse_cond(tokens):
  stack = []
  for i, token in enumerate(reversed(tokens)):
    if i % 10000 == 0:
      print("parsed %d tokens" % i)
    if token == "#and":
      op1 = stack.pop()
      op2 = stack.pop()
      stack.append(And(op1, op2))
    elif token == "#or":
      op1 = stack.pop()
      op2 = stack.pop()
      stack.append(Or(op1, op2))
    elif token == "#not":
      op = stack.pop()
      stack.append(Not(op))
    else:
      stack.append(Bool(token))
  assert(len(stack) == 1)
  return stack[0]

#def parse_cond(tokens):
#  if tokens[0] == "#and":
#    tokens, e1 = parse_cond(tokens[1:])
#    tokens, e2 = parse_cond(tokens)
#    return tokens, And(e1, e2)
#  elif tokens[0] == "#or":
#    tokens, e1 = parse_cond(tokens[1:])
#    tokens, e2 = parse_cond(tokens)
#    return tokens, Or(e1, e2)
#  elif tokens[0] == "#not":
#    tokens, e = parse_cond(tokens)
#    return tokens, Not(e)
#  else:
#    return tokens[1:], Bool(tokens[0])

def size_of_expr(expr):
  if is_and(expr):
    return sum(map(size_of_expr, expr.children())) + 1
  elif is_or(expr):
    return sum(map(size_of_expr, expr.children())) + 1
  elif is_not(expr):
    return sum(map(size_of_expr, expr.children())) + 1
  else:
    #print("ground term", expr)
    return 1

def expr_to_string(expr):
  if is_and(expr):
    children = expr.children()
    return "#and " * (len(children) - 1) + " ".join(map(expr_to_string, children))
  elif is_or(expr):
    children = expr.children()
    return "#or " * (len(children) - 1) + " ".join(map(expr_to_string, children))
  elif is_not(expr):
    children = expr.children()
    assert(len(children) == 1)
    return "#not " + expr_to_string(children[0])
  else:
    assert(is_const(expr))
    return expr.sexpr()

out = open("traces/simplified.txt", "w")

for line in f:
  tokens = line.split(" ")
  assert tokens[0] == "#cond"
  semic = tokens.index(';')
  print(semic)
  cond_tokens = tokens[1:semic]
  print("length")
  print(len(cond_tokens))
  expr = parse_cond(cond_tokens)

  print("parsed")
  #print(expr)
  print("size before simplification")
  print(size_of_expr(expr))
  simplified_expr = Repeat(AndThen(
      Tactic('ctx-solver-simplify'),
      #Tactic('unit-subsume-simplify'),
      Tactic('ctx-simplify'),
      Tactic('dom-simplify'),
      Tactic('simplify')
    ))(simplify(expr))
  simplified_expr = And(*simplified_expr[0])
  #print("simplified:")
  #print(simplified_expr)
  print("size after simplification")
  print(size_of_expr(simplified_expr))
  print("equivalence is proved:")
  prove(expr == simplified_expr)

  remainder = tokens[semic + 1:]
  out.write("#cond " + expr_to_string(simplified_expr) + " ; " + " ".join(remainder))
