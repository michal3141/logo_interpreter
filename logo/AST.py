# Abstract Syntax Tree Representation
class Prog:
    def __init__(self, functions, instructions):
        self.functions = functions
        self.instructions = instructions
        
class Func:
    def __init__(self, name, params, instructions):
        self.name = name
        self.params = params
        self.instructions = instructions
        # Initially empty - closure related dictionary
        self.closuredict = None
        
class Var:
    def __init__(self, name):
        self.name = name
           
class Make:
    def __init__(self, name, expr):
        self.name = name
        self.expr = expr
               
class Call:
    def __init__(self, name, args):
        self.name = name
        self.args = args
        
class Repeat:
    def __init__(self, times, instructions):
        self.times = times
        self.instructions = instructions
        
class IfElse:
    def __init__(self, condition, instr1, instr2):
        self.condition = condition
        self.instr1 = instr1
        self.instr2 = instr2
        
class For:
    def __init__(self, varname, start, stop, step, instructions):
        self.varname = varname
        self.start = start
        self.stop = stop
        self.step = step
        self.instructions = instructions
        
class Arithm:
    def __init__(self, operator, expr1, expr2):
        self.operator = operator
        self.expr1 = expr1
        self.expr2 = expr2

class Compar:
    def __init__(self, operator, expr1, expr2):
        self.operator = operator
        self.expr1 = expr1
        self.expr2 = expr2
        
class Const:
    def __init__(self, val, type):
        self.val = val
        self.type = type
        
class Closure:
    def __init__(self, name):
        self.name = name
        
