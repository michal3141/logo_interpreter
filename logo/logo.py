#!/usr/bin/python

# Simple Logo interpreter written in Python using turtle module and parser
# generator: PLY (Python Lex Yacc). See also: dokumentacja.pdf
# Authors: Michal Mrowczyk, Damian Kudas
# Feel free to report any possible bug to: mrowczyk@student.agh.edu.pl
# Convention: In comments we often use names: procedure, function, subroutine
# They all basically have the same notion in our program

import sys;
import ply.lex as lex;
import ply.yacc as yacc;
import turtle
import math
import random
import copy
import AST

# If 0 then normal, if 1 then interactive
mode = None

# Representing Abstract Syntax Tree (AST)
tree = None

# Type of variable: possibly integer or float
typ = None

# Built in looping variable in repeat statement (used by Logo interpreter)
repcount = -1

# Dictionary of pairs: varname --> varvalue (also called: environment)
# varname - name of variable
# varvalue - value of variable
vardict = {}

# Global flag depicting if there is syntax error in current parsing
error = False

# Global name representing information in which function interpreter is in
# given time
funkyname = "__main__"

# Denoting whether or not current function returns value (based on
# interpreter evaluations)
returning = False

def addToClass(cls):
    """
    Adds function to class: cls
    Used to inject eval functions into
    AST module classes
    """
    def decorator(func):
        setattr(cls,func.__name__,func)
        return func
    return decorator

class Interpreter:
    """
    Main class containing interpreter functionality
    It defines eval functions for AST module classes
    """
    @addToClass(AST.Prog)
    def eval(self):
        # Evaluating program is equivalent to sequential evaluation of
        # it's instructions
        for instruction in self.instructions:
            instruction.eval(self.functions)
    
    @addToClass(AST.Var)
    def eval(self, context):
        global vardict, mode
        
        # Checking if variable is defined during interpreter pass.
        # If not defined then actions are taken based on interpreting mode:
        # possibly interactive or interpreting file.
        if self.name in vardict.keys():
            return vardict[self.name]
        else:
            print("Undefined variable: " + self.name)
            if mode == 0:
                sys.exit(-1)
            return 0
          
    @addToClass(AST.Make)
    def eval(self, context):
        global vardict, funkyname, returning
        
        # Make statement's eval evaluates expression and assign it's value to
        # global variable dictionary vardict
        val = self.expr.eval(context)
        name = self.name.name
        vardict[name] = val
        
        # If make statement assigned value to variable with current function
        # name then updating returning flag
        if name == funkyname:
            returning = True
        
    @addToClass(AST.Call)
    def eval(self, context):
        global vardict, repcount, mode, error, returning, funkyname
        
        # Main point - calling procedure
        # There is an attempt to implement closures.
        # Admittedly it is not the clearest way to handle function call
        # so code is bit convoluted...
        
        # Protecting global vardict against local changes
        tmp = vardict.copy()
        
        # Protecting returning state of outer functions
        tmpreturning = returning
        
        # Protecting funkyname for outer function
        tmpfunkyname = funkyname
        
        # Setting returning to be False since no make "this_func_name
        # statement has been seen during this call
        returning = False
        
        # Setting this funkyname to name of this function
        funkyname = self.name
        
        # Result of calling this procedure / function
        result = None
        
        # Obtaining names of locally defined procedures / functions
        localprocs = []
        for proc in context:
            localprocs.append(proc.name)
            
        # Traversing vardict in order to find possible function names
        # Note that Python empowers us to treat functions like ordinary
        # variables in environment, so we use this approach heavily...
        for elem in vardict.keys():
            if isinstance(vardict[elem], AST.Func):
                localprocs.append(elem)
        
        # Evaluating arguments for procedure call.
        # This list will be passed into environment of called subroutine
        argvalues = []
        for arg in self.args:
            val = arg.eval(context)
            argvalues.append(val)
            
        # Checking if it is procedure defined locally (in Logo program):
        if self.name in localprocs:
            funcobject = None
            
            # Trying to obtain function from vardict based on function name
            # This is done mainly for closures purposes 
            for elem in vardict.keys():
                if elem == self.name and isinstance(vardict[elem], AST.Func):
                    funcobject = vardict[elem]

            # Obtaining function object from context
            # Note that those are the functions which are defined at the
            # beggining of the program or defined in interactive mode
            # using ' to func_name (args) end ' statement
            if funcobject == None:
                for func in context:
                    if func.name == self.name:
                        funcobject = func
                        
            # Counter for argvalues list elements.
            i = 0
            
            # Obtaining names of formal parameters for function we try to call
            names = [p.name for p in funcobject.params]
            
            # Checking if funcobject params are unique
            # Node that uniqueness checking is actually performed only when
            # function / procedure is called and not when definition happens!
            if len(names) != len(set(names)):
                print("Duplicated param name in function declaration: " + self.name)
                if mode == 0:
                    sys.exit(-1)
                else:
                    error = True
                              
            # Setting up environment for funcobject call
            # It actually contains consists of two steps
            # 1) Obtaining closure dictionary (if not None) and updating environment
            # 2) Taking values of arguments passed to function and updating environment
            # Please note that what is understood as closure dictionary params
            # may be overriden by arguments that are passed to function indirectly.
            cdict = funcobject.closuredict
            if cdict != None:
                for elem in cdict.keys():
                    vardict[elem] = cdict[elem]
            for p in funcobject.params:
                vardict[p.name] = argvalues[i]
                i += 1
                
            # Calling funcobject with argvalues as arguments:
            for instr in funcobject.instructions:
                 instr.eval(context)
                 
            # Setting up the result of the call (if function call returned value)
            # Please note that there is no way to check whether or not
            # one branch of if-else statement returns something and other one not
            # (at least using our approach). Also Python interpreter is not
            # capable of doing such checks. (At least Python 2)
            # Also there is a check that function's returned value was actually assigned
            # inside this function
            if (self.name in vardict.keys()) and returning:
                result = vardict[self.name]
            else:
                result = None
        # Checking for built in subroutines:
        else:
            # Stringifying argvalues for eval call.
            # Note that more elegant solution could require Python
            # join function to be employed
            strparams = "("
            for val in argvalues:
                strparams += str(val)
                strparams += ','
                
            # Removing trailing comma and replacing it with ')'
            strparams = strparams[0:len(strparams)-1] + ")"
            
            # Handling case when there are no args:
            if len(argvalues) == 0:
                strparams = "()"
            
            # Checking if it is turtle subroutine
            if self.name in dir(turtle):
                result = eval("turtle." + self.name + strparams)
            # Checking if it is math subroutine
            elif self.name in dir(math):
                result = eval("math." + self.name + strparams)
            # Checking if it is random subroutine              
            elif self.name in dir(random):
                result = eval("random." + self.name + strparams)
            # Checking if it is repcount built in subroutine
            elif self.name == 'repcount':
                if repcount < 0:
                    print("Repcount undefined")
                    if mode == 0:
                        sys.exit(-1)
                    else:
                        error = True
                result = repcount
            # If function was not recognized...
            else:
                print("Unrecognized function: " + self.name)
                if mode == 0:
                    sys.exit(-1)
                else:
                    error = True
                    
        # Recreating some globals in spirit of calling convention...
        vardict = tmp.copy()
        returning = tmpreturning
        funkyname = tmpfunkyname
        
        return result
    
    @addToClass(AST.Repeat)
    def eval(self, context):
        global vardict, repcount
        
        # Protecting vardict from local changes (useful when implementing scoping)
        tmp = vardict.copy()
        
        # Evaluating number of iterations...
        rang = self.times.eval(context)
        
        # ... And evaluating instructions that many times.
        for i in range(rang):
            repcount = i
            for ins in self.instructions:
                ins.eval(context)
        repcount = -1
        vardict = tmp.copy()
        
    @addToClass(AST.IfElse)
    def eval(self, context):
        global vardict
        
        # Protecting vardict from local changes (useful when implementing scoping)
        tmp = vardict.copy()
        
        # Evaluating condition and based on that evaluating different list of instructions
        cond = self.condition.eval(context)
        if cond:
            for i in self.instr1:
                i.eval(context)
        else:
            for i in self.instr2:
                i.eval(context)
                
        vardict = tmp.copy()
                
    @addToClass(AST.For)
    def eval(self, context):
        global vardict
        
        # Protecting vardict from local changes (useful when implementing scoping)
        tmp = vardict.copy()
        
        # Preparing start, stop and step for for statement
        start = self.start.eval(context)
        stop = self.stop.eval(context)
        step = self.step.eval(context)
        
        # Simulating Logo for loop using Python while loop
        i = start
        while i <= stop:
            vardict[self.varname] = i
            for ins in self.instructions:
                ins.eval(context)
            i += step
            
        vardict = tmp.copy()
        
    @addToClass(AST.Arithm)
    def eval(self, context):
        
        # Evaluating recursively arithmetic expression
        val1 = self.expr1.eval(context)
        val2 = self.expr2.eval(context)
        return eval(str(val1) + self.operator + str(val2))
        
    @addToClass(AST.Compar)
    def eval(self, context):
        
        # Performing comparison operation in Logo using Python
        # comparison capabilities. Note that there is special
        # treatment of equality checking
        val1 = self.expr1.eval(context)
        val2 = self.expr2.eval(context)
        if self.operator == "=":
            return eval(str(val1) + self.operator + "=" + str(val2))
        else:
            return eval(str(val1) + self.operator + str(val2))
        
    @addToClass(AST.Const)
    def eval(self, context):
        return self.val
    
    # One thing to reckon is that when creating closure one have
    # to remember current vardict state and save it under closuredict
    @addToClass(AST.Func)
    def eval(self, context):
        global vardict
        
        # Note that because there is really no special calling convention
        # treatment it is possible that closures assigned in inner function
        # (using closure mechanisms) will be visible in outer functions
        # It may feel buggy but it's intentional, that once we evaluated function
        # we keep it in global state the same way we deal with other user defined
        # functions.
        self.closuredict = vardict.copy()
        vardict[self.name] = self
      
    # When evaluating Closure object we return Func object for it
    @addToClass(AST.Closure)
    def eval(self, context):
        global vardict
        return vardict[self.name]
        
# Literals used by interpreter
literals = ':"[](),+-*/=<>'

# Tokens which are not literals
tokens = [ "WORD", "FLOAT", "INTEGER", 
         "NEQ", "LE", "GE" ];

# Keywords or reserved words in Logo language
reserved = {
    'ifelse' : 'IFELSE',
    'repeat' : 'REPEAT',
    'make' : 'MAKE',
    'for' : 'FOR',
    'to' : 'TO',
    'end' : 'END'
}
tokens += reserved.values()

# Ignoring tabs and spaces
t_ignore = ' \t'

# Ignoring comments which are created using ';' delimeter
t_ignore_COMMENT = r'\;.*'

# Handling new lines
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

# On scanner error
def t_error(t):
    global mode, error
    print ("Illegal character {0} at line {1}".format(t.value[0], t.lexer.lineno))
    if mode == 0:
        sys.exit(-1)
    else:
        error = True
    
# Lexems:
def t_WORD(t):
    r"[a-zA-Z_]\w*"
    if t.value in reserved:
        t.type = reserved[t.value]
    return t
    
def t_FLOAT(t):
    r"\d+(\.\d*)|\.\d+"
    global typ
    typ = t.type
    return t

def t_INTEGER(t):
    r"\d+"
    global typ
    typ = t.type
    return t

def t_LE(t):
    r"<="
    return t

def t_GE(t):
    r">="
    return t

def t_NEQ(t):
    r"!="
    return t

# Precedence definitions
precedence = (
   ("nonassoc", '<', '>', '=', 'NEQ', 'LE', 'GE'),
   ("left", '+', '-'),
   ("left", '*', '/') )

# Handling parser errors
def p_error(p):
    global mode, error
    if p != None:
        print("Syntax error at token: {0} in line: {1}".format(p.type, p.lineno))
    if mode == 0:
        sys.exit(-1)
    else:
        error = True

# Parsing stuff (consult chapter 2 in dokumentacja.pdf for more info about grammar)
def p_program(p):
    """program : instr_list
               | func_list
               | func_list instr_list"""
    global tree
    
    if tree == None:
        if len(p) == 2:
            if isinstance(p[1][0], AST.Func):
                tree = AST.Prog(p[1], [])
            else:    
                tree = AST.Prog([], p[1])
        else:
            tree = AST.Prog(p[1], p[2])
    else:
        if len(p) == 2:
            if isinstance(p[1][0], AST.Func):
                tree = AST.Prog(combine(tree.functions, p[1]), [])
            else:
                tree = AST.Prog(tree.functions, p[1])
        else:
            tree = AST.Prog(combine(tree.functions, p[1]), p[2])
    p[0] = tree    

def p_func_list(p):
    """func_list : func
                 | func_list func"""
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = p[1] + p[2]
        
def p_instr_list(p):
    """instr_list : instr
                  | instr_list instr"""
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = p[1] + p[2]
                  
def p_func(p):
    """func : TO WORD '(' params ')' extended_instr_list END"""
    p[0] = [AST.Func(p[2], p[4], p[6])]
    
def p_params(p):
    """params :
              | ref
              | params ref"""
    if len(p) == 1:
        p[0] = []
    elif len(p) == 2:
        if not isinstance(p[1], list):
            p[1] = [p[1]]
        p[0] = p[1]
    else:
        if not isinstance(p[1], list):
            p[1] = [p[1]]
        if not isinstance(p[2], list):
            p[2] = [p[2]]
        p[0] = p[1] + p[2]
              
def p_extended_instr_list(p):
    """extended_instr_list : instr
                           | func
                           | extended_instr_list instr
                           | extended_instr_list func"""
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = p[1] + p[2]
        
def p_ref(p):
    """ref : ':' WORD"""
    p[0] = AST.Var(p[2])
    
def p_instr(p):
    """instr : MAKE quoted expr
             | REPEAT expr '[' instr_list ']'
             | FOR '[' WORD expr expr expr ']' '[' instr_list ']'
             | FOR '[' WORD expr expr ']' '[' instr_list ']'
             | IFELSE condition '[' instr_list ']' '[' instr_list ']'
             | WORD '(' expr_list ')'"""
    if len(p) == 4:
        p[0] = [AST.Make(p[2], p[3])]
    elif len(p) == 5:
        p[0] = [AST.Call(p[1], p[3])]
    elif len(p) == 6:
        p[0] = [AST.Repeat(p[2], p[4])]
    elif len(p) == 9:
        p[0] = [AST.IfElse(p[2], p[4], p[7])]
    elif len(p) == 10:
        p[0] = [AST.For(p[3], p[4], p[5], AST.Const(1, "integer"), p[8])]
    else:
        p[0] = [AST.For(p[3], p[4], p[5], p[6], p[9])]
             
def p_expr_list(p):
    """expr_list :
                 | expr
                 | expr_list ',' expr"""
    if len(p) == 1:
        p[0] = []
    elif len(p) == 2:
        if not isinstance(p[1], list):
            p[1] = [p[1]]
        p[0] = p[1]
    else:
        if not isinstance(p[1], list):
            p[1] = [p[1]]
        if not isinstance(p[3], list):
            p[3] = [p[3]]
        p[0] = p[1] + p[3]
                 
def p_expr(p):
    """expr : const
            | name
            | ref
            | WORD '(' expr_list ')'
            | expr '+' expr
            | expr '-' expr
            | expr '*' expr
            | expr '/' expr
            | '(' expr ')'"""
    if len(p) == 2:
        p[0] = p[1]
    elif len(p) == 5:
        p[0] = AST.Call(p[1], p[3])
    elif p[1] == '(':
        p[0] = p[2]
    else:
        p[0] = AST.Arithm(p[2], p[1], p[3])

def p_condition(p):
    """condition : expr '=' expr
                 | expr '<' expr
                 | expr '>' expr
                 | expr NEQ expr
                 | expr GE expr
                 | expr LE expr"""
    p[0] = AST.Compar(p[2], p[1], p[3])
                 
def p_quoted(p):
    """quoted : '"' WORD"""
    p[0] = AST.Var(p[2])
    
def p_name(p):
    """name : WORD """
    p[0] = AST.Closure(p[1])
    
def p_const(p):
    """const : FLOAT
             | INTEGER"""
    global typ, mode, error
    if typ == 'FLOAT':
        p[0] = AST.Const(float(p[1]), "float")
    elif typ == 'INTEGER':
        p[0] = AST.Const(int(p[1]), "integer")
    else:
        print("Unrecognized type!")
        if mode == 0:
            sys.exit(-1)
        else:
            error = True

def combine(oldlist, newlist):
    """Combines oldlist and newlist of functions
       by replacing functions from oldlist which have the same name with
       functions from newlist"""
    result = []
    for f1 in oldlist:
        app = True
        for f2 in newlist:
            if f1.name == f2.name:
                app = False
        if app:
            result.append(f1)
            
    for f in newlist:
        result.append(f)
        
    return result

def treeinfo(tree):
    """
    Debugging function used to display something from constructed AST
    """
    print(tree)
    for f in tree.functions:
        print("Function: " + f.name)
        print("Params: ")
        for p in f.params:
            print(p.name)
        print("Instructions: ")
        for i in f.instructions:
            print(i)
            
    print("Module instructions: ")        
    for i in tree.instructions:
        print(i)
        
def main():
    global mode, error
    lexer = lex.lex()
    parser = yacc.yacc()
    
    # Normal mode: 
    if len(sys.argv) > 1:
        mode = 0
        file = open(sys.argv[1])
        text = file.read()
        parser.parse(text, lexer=lexer)
        # treeinfo(tree)
        # Evaluating or interpreting program
        tree.eval() 
        raw_input("Type anything to exit the program...")
    else:
        # Interactive mode:
        mode = 1
        while True:
            error = False
            text = raw_input("<< ")
            if text == "exit":
                break
            elif text == "help":
                print("Type exit to exit the program.")
                print("Type logo command to continue.")
            else:
                parser.parse(text, lexer=lexer)
                if not error: 
                    tree.eval()
                else:
                    print("Syntax error has occurred")
               
if __name__ == '__main__':
    main()
