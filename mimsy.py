#!/usr/bin/env python
## Copyright (c) 2009, Michael Patraw
## All rights reserved.
##
## Redistribution and use in source and binary forms, with or without
## modification, are permitted provided that the following conditions are met:
##     * Redistributions of source code must retain the above copyright
##       notice, this list of conditions and the following disclaimer.
##     * Redistributions in binary form must reproduce the above copyright
##       notice, this list of conditions and the following disclaimer in the
##       documentation and/or other materials provided with the distribution.
##     * The name of Michael Patraw may not be used to endorse or promote
##       products derived from this software without specific prior written
##       permission.
##
## THIS SOFTWARE IS PROVIDED BY Michael Patraw ''AS IS'' AND ANY
## EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
## WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
## DISCLAIMED. IN NO EVENT SHALL Michael Patraw BE LIABLE FOR ANY
## DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
## (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
## LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
## ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
## (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
## SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
##
#
# A python interpreter for the Mimsy Programming Language
#
import copy, sys, re

MEMORY_SIZE = 100

TOK_OP, TOK_STRING, TOK_NUMBER, TOK_NAME, TOK_LISTB, TOK_LISTE, \
TOK_MACROB, TOK_MACROE, TOK_SELECTB, TOK_SELECTE = range(10)

OP_SELECT, OP_SET, OP_GET, OP_LIT, OP_MANIP,        \
OP_NAME, OP_MACRO,                                  \
OP_MULT, OP_DIV, OP_MOD, OP_SUB, OP_PLUS, OP_AND,   \
OP_XOR, OP_OR, OP_NEG, OP_NOT, OP_CMP, OP_LEN,     \
OP_SAVE, OP_PLACEHOLD, OP_JMP, OP_JMPEQ, OP_CALL = range(24)

# Register constants.
REG_HAND, REG_CODE, REG_IP, REG_JMP, REG_FLAGS, REG_SELECT = range(MEMORY_SIZE, MEMORY_SIZE + 6)

# Flag constants
FLAG_EQ, FLAG_NE, FLAG_GT, FLAG_LT = range(4)

class Error(Exception):
    """
    """

class System(object):
    """
    Just holds the system variables
    """
    
    def __init__(self):
        self.memory = [None] * (MEMORY_SIZE + 7) # 7 For the registers.
        self.macros = {}
        self.memory[REG_HAND] = 0
        self.memory[REG_CODE] = []
        self.memory[REG_IP] = 0
        self.memory[REG_JMP] = []
        self.memory[REG_FLAGS] = [0, 0, 0, 0]
        self.memory[REG_SELECT] = []
        
        # Load default macros.
        
        def xioPut(mimsy):
            sys.stdout.write(chr(mimsy.memory[REG_HAND]))
        
        def xioGet(mimsy):
            i = sys.stdin.read(1)
            
            if len(i) == 0:
                mimsy.memory[REG_HAND] = -1
            else:
                mimsy.memory[REG_HAND] = ord(i)
            
            
        
        def xmimDebug(mimsy):
            print 'Memory: %d cells' % len(self.memory)
            print self.memory
            print
            print 'Macros'
            print self.macros
        
        self.macros["xioPut"] = xioPut
        self.macros["xioGet"] = xioGet
        self.macros["xmimDebug"] = xmimDebug
        self.macros["xmimNull"] = 0
        

def list_to_string(l):
    s = ""
    for i in l:
        s += chr(i[1])
    return s
    
def string_to_list(s):
    l = []
    for i in xrange(len(s)):
        l.append([OP_LIT, ord(s[i])])
    return l

def tokenize(string):
    """
    A token will just be a list of two elements. The first will
    be the type of token it is and the second will be it's value.
    
    Possible tokens:
    [TOK_OP, '*'] A TOK_OP token is a single operator.
    [TOK_STRING, 'string'] A TOK_STRING is just a string.
    [TOK_NUMBER, 403] Just a number.
    [TOK_NAME, 'name'] This would be a macro in mimsy.
    [TOK_LISTB, '['] A TOK_LISTB is the beginning of a list.
    [TOK_LISTE, ']'] A TOK_LISTE is the end of a list.
    [TOK_MACROB, '{'] A TOK_MACROB is the beginning of a macro defition.
    [TOK_MACROE, '}'] A TOK_MACROE is the end of a macro definition.
    [TOK_SELECTB, '('] Begin selection.
    [TOK_SELECTE, ')'] End selection.
    """
    
    rules = [
        (TOK_OP, re.compile(r'\<|\>|\,|\*|\/|\%|\-|\+|\&|\^|\||\~|\!|\=|\$|\@|\;|\:|\?|\`')),
        (TOK_STRING, re.compile(r'\".*\"')),
        (TOK_NUMBER, re.compile(r'\_?\d+\.*\d*')),
        (TOK_NAME, re.compile(r'[a-zA-Z]+')),
        (TOK_LISTB, re.compile(r'\[')),
        (TOK_LISTE, re.compile(r'\]')),
        (TOK_MACROB, re.compile(r'\{')),
        (TOK_MACROE, re.compile(r'\}')),
        (TOK_SELECTB, re.compile(r'\(')),
        (TOK_SELECTE, re.compile(r'\)'))
    ]
    whitespace = re.compile(r'[ ]+')
    
    tokens = []
    
    while string:
        match = False
        for tok, pattern in rules:
            m = whitespace.match(string)
            if m:
                string = string[m.end():]
                
            m = pattern.match(string)
            if m:
                match = True
                tokens.append([tok, m.group(0)])
                string = string[m.end():]
        if match == False:
            raise Error('Syntax error')

    return tokens

def analize(tokens):
    """
    This will check the validity of a mimsy token stream.
    """
    
    i = 0 # Position in token stream.
    
    def check_list(tokens, i):
        i += 1
        while i < len(tokens):
            if tokens[i][0] == TOK_LISTE:
                break
            elif tokens[i][0] == TOK_LISTB:
                i = check_list(tokens, i) - 1
            elif tokens[i][0] in (TOK_NUMBER, TOK_STRING, TOK_NAME):
                pass
            else:
                raise Error('Invalid list parameters!')
            i += 1
        else:
            raise Error('Stray [ found!')
        return i + 1
    
    def check_macro(tokens, i):
        i += 1
        arg = False
        
        if i >= len(tokens) or tokens[i][0] != TOK_NAME:
            raise Error('Macros require names!')
        
        i += 1
        while i < len(tokens):
            if tokens[i][0] == TOK_MACROE:
                break
            elif tokens[i][0] in (TOK_NUMBER, TOK_STRING):
                if arg == True:
                    raise Error('Too many macro parameters!')
                else:
                    arg = True
            elif tokens[i][0] == TOK_LISTB:
                if arg == True:
                    raise Error('Too many macro parameters!')
                else:
                    arg = True
                    i = check_list(tokens, i) - 1
            else:
                raise Error('Invalid macro parameters!')
            i += 1
        else:
            raise Error('Stray { found!')
        return i + 1
    
    def check_select(tokens, i):
        i += 1
        NUMBERNAME, COMMA, EITHER = range(3)
        expected = EITHER
        
        if tokens[i][1] in "$~@!*^?":
            if i+1 >= len(tokens):
                raise Error('Stray ( found!')
            if tokens[i+1][0] != TOK_SELECTE:
                raise Error('Stray ( found!')
            return i + 1
        elif tokens[i][1] == ',':
            if i+2 < len(tokens):
                if tokens[i+1][0] in (TOK_NAME, TOK_NUMBER) and \
                    tokens[i+2][0] == TOK_SELECTE:
                    return i + 2
            if i+1 < len(tokens):
                if tokens[i+1][0] == TOK_SELECTE:
                    return i + 1
            raise Error('Invalid select parameters!')
        
        
        while i < len(tokens):
            if tokens[i][0] == TOK_SELECTE:
                if expected == EITHER:
                    raise Error('No parameters!')
                if expected == NUMBERNAME:
                    raise Error('Select parameter; trailing comma')
                break
            elif tokens[i][0] in (TOK_NUMBER, TOK_NAME):
                if expected not in (NUMBERNAME, EITHER):
                    raise Error('Invalid select parameters!')
                expected = COMMA
            elif tokens[i][1] == ',':
                if expected not in (COMMA, EITHER):
                    raise Error('Invalid select parameters!')
                expected = NUMBERNAME
            else:
                raise Error('Invalid select parameters!')
            i += 1
        else:
            raise Error('Stray ( found!')
        return i
    
    while i < len(tokens):
        if tokens[i][0] == TOK_OP:
            pass
        elif tokens[i][0] == TOK_STRING:
            pass
        elif tokens[i][0] == TOK_NUMBER:
            pass
        elif tokens[i][0] == TOK_NAME:
            pass
        elif tokens[i][0] == TOK_LISTB:
            i = check_list(tokens, i)
        elif tokens[i][0] == TOK_LISTE:
            raise Error('%d: %s: Stray ] found!' % (i, tokens[i]))
        elif tokens[i][0] == TOK_MACROB:
            i = check_macro(tokens, i)
        elif tokens[i][0] == TOK_MACROE:
            raise Error('%d: %s: Stray } found!' % (i, tokens[i]))
        elif tokens[i][0] == TOK_SELECTB:
            i = check_select(tokens, i)
        elif tokens[i][0] == TOK_SELECTE:
            raise Error('%d: %s: Stray ) found!' % (i, tokens[i]))
        i += 1

def compil(tokens):
    """
    Compiles a stream of tokens into a stream of OP codes ready for
    execution. An opcode follows the same format as a token. The first
    element is the opcode type and the remaining opcodes are the arguments.
    
    For example, selecting a single index in memory would look like:
    [OP_SELECT, 5]
    
    Possible opcodes:
    
    OP_SELECT
        Arguments:
        Numbers representing indices, negative values work from the last. -1 is
        the last element.
        
        Numbers over the size of memory are registers. In Hand would be: 
        sys.memory[REG_HAND]
        
        No arguments specifies going back one level (,). A None value followed by a
        number is the walk specifier. (,5)
        
        If the argument is a single list, it selects the location In Hand.
        
    OP_LIT:
        Arguments: The element for the literal.
    
    OP_NAME
        Arguments: Just a name.
    
    OP_MACRO
        Arguments: A name followed by zero or one element.
    
    OP_MULT, OP_DIV, OP_MOD, OP_SUB, OP_PLUS, OP_AND,
    OP_XOR, OP_OR, OP_NEG, OP_NOT, OP_CMP, OP_LEN,
    OP_SAVE, OP_PLACEHOLD, OP_JMP, OP_JMPEQ, OP_CALL,
    OP_SET, OP_GET, OP_MANIP
        All of these require no arguments, they're just single element
        instructions.
    
    """
    program = []
    
    i = 0 # Position in token stream.
    
    def compile_op(tokens, i):
        if tokens[i][1] == '<':
            return [OP_SET], i
        elif tokens[i][1] == '>':
            return [OP_GET], i
        elif tokens[i][1] == ',':
            return [OP_MANIP], i
        elif tokens[i][1] == '*':
            return [OP_MULT], i
        elif tokens[i][1] == '/':
            return [OP_DIV], i
        elif tokens[i][1] == '%':
            return [OP_MOD], i
        elif tokens[i][1] == '-':
            return [OP_SUB], i
        elif tokens[i][1] == '+':
            return [OP_PLUS], i
        elif tokens[i][1] == '&':
            return [OP_AND], i
        elif tokens[i][1] == '^':
            return [OP_XOR], i
        elif tokens[i][1] == '|':
            return [OP_OR], i
        elif tokens[i][1] == '~':
            return [OP_NEG], i
        elif tokens[i][1] == '!':
            return [OP_NOT], i
        elif tokens[i][1] == '=':
            return [OP_CMP], i
        elif tokens[i][1] == '$':
            return [OP_LEN], i
        elif tokens[i][1] == '@':
            return [OP_SAVE], i
        elif tokens[i][1] == ';':
            return [OP_PLACEHOLD], i
        elif tokens[i][1] == ':':
            return [OP_JMP], i
        elif tokens[i][1] == '?':
            return [OP_JMPEQ], i
        elif tokens[i][1] == '`':
            return [OP_CALL], i
        else:
            raise Error('Critical error when compiling operators')
        
    def compile_literal(tokens, i):
        if tokens[i][0] == TOK_NUMBER:
            n = tokens[i][1]
            neg = 1
            ind = 0
            if n[0] == '_':
                neg = -1
                ind = 1
            
            try:
                n = int(n[ind:])
            except:
                try:
                    n = float(n[ind:])
                except:
                    raise Error('Failed to convert number')
            n *= neg
            
            return [OP_LIT, n], i
        elif tokens[i][0] == TOK_STRING:
            s = tokens[i][1]
            s = s[1:-1]
            return [OP_LIT, string_to_list(s)], i
        else:
            raise Error('Bug in compile_literal')
    
    def compile_name(tokens, i):
        return [OP_NAME, tokens[i][1]], i
    
    def compile_list(tokens, i):
        i += 1
        l = []
        while i < len(tokens):
            if tokens[i][0] == TOK_LISTE:
                return [OP_LIT, l], i
            elif tokens[i][0] == TOK_LISTB:
                nl, i = compile_list(tokens, i)
                l.append(nl)
            elif tokens[i][0] in (TOK_NUMBER, TOK_STRING):
                lit, i = compile_literal(tokens, i)
                l.append(lit)
            elif tokens[i][0] == TOK_NAME:
                name, i = compile_name(tokens, i)
                l.append(name)
            else:
                raise Error('Invalid list parameters!')
            i += 1
        raise Error('Failed to compile list')
    
    def compile_macro(tokens, i):
        i += 1
        op = [OP_MACRO]
        
        while i < len(tokens):
            if tokens[i][0] == TOK_MACROE:
                return op, i
            elif tokens[i][0] == TOK_NAME:
                name, i = compile_name(tokens, i)
                op.append(name)
            elif tokens[i][0] in (TOK_NUMBER, TOK_STRING):
                lit, i = compile_literal(tokens, i)
                op.append(lit)
            elif tokens[i][0] == TOK_LISTB:
                l, i = compile_list(tokens, i)
                op.append(l)
            i += 1
        raise Error('Failed to compile macro')
    
    def compile_select(tokens, i):
        i += 1
        NUMBERNAME, COMMA, EITHER = range(3)
        expected = EITHER
        literals = []
        
        if tokens[i][1] in "$~@!*^?":
            if tokens[i][1] == '$':
                return [OP_SELECT, [OP_LIT, []]], i
            if tokens[i][1] == '@':
                return [OP_SELECT, [OP_LIT, REG_HAND]], i
            if tokens[i][1] == '!':
                return [OP_SELECT, [OP_LIT, REG_CODE]], i
            if tokens[i][1] == '*':
                return [OP_SELECT, [OP_LIT, REG_IP]], i
            if tokens[i][1] == '^':
                return [OP_SELECT, [OP_LIT, REG_JMP]], i
            if tokens[i][1] == '?':
                return [OP_SELECT, [OP_LIT, REG_FLAGS]], i
            if tokens[i][1] == '~':
                return [OP_SELECT, [OP_LIT, REG_SELECT]], i
        elif tokens[i][1] == ',':
            if i+2 < len(tokens):
                if tokens[i+1][0] == TOK_NAME and tokens[i+2][0] == TOK_SELECTE:
                    name, tmp = compile_name(tokens, i + 1)
                    return [OP_SELECT, None, name], i + 2
                if tokens[i+1][0] == TOK_NUMBER and tokens[i+2][0] == TOK_SELECTE:
                    lit, tmp = compile_literal(tokens, i + 1)
                    return [OP_SELECT, None, lit], i + 2
            if i+1 < len(tokens):
                if tokens[i+1][0] == TOK_SELECTE:
                    return [OP_SELECT], i + 1
            raise Error('Invalid select parameters!')
        
        while i < len(tokens):
            if tokens[i][0] == TOK_SELECTE:
                break
            elif tokens[i][0] == TOK_NUMBER:
                lit, tmp = compile_literal(tokens, i)
                literals.append(lit)
            elif tokens[i][0] ==  TOK_NAME:
                name, tmp = compile_name(tokens, i)
                literals.append(name)
            i += 1
        op = [OP_SELECT]
        for l in literals:
            op.append(l)
        return op, i

    while i < len(tokens):
        if tokens[i][0] == TOK_OP:
            op, i = compile_op(tokens, i)
            program.append(op)
        elif tokens[i][0] in (TOK_NUMBER, TOK_STRING):
            op, i = compile_literal(tokens, i)
            program.append(op)
        elif tokens[i][0] == TOK_NAME:
            op, i = compile_name(tokens, i)
            program.append(op)
        elif tokens[i][0] == TOK_LISTB:
            op, i = compile_list(tokens, i)
            program.append(op)
        elif tokens[i][0] == TOK_MACROB:
            op, i = compile_macro(tokens, i)
            program.append(op)
        elif tokens[i][0] == TOK_SELECTB:
            op, i = compile_select(tokens, i)
            program.append(op)

        i += 1

    return program

def run(mimsy, code):
    """
    Runs a sequence of opcodes.
    """
    mimsy.memory[REG_CODE] = code

    def run_literal(lit):
        """
        Returns the equivalent python data for the literal.
        """
        
        if lit is None:
            return None

        elif lit[0] == OP_NAME:
            l = None
            if mimsy.macros.has_key(lit[1]):
                l = mimsy.macros[lit[1]]
            if not l:
                raise Error('Undefined macro')
            else:
                if callable(l):
                    raise Error('External function found as an argument')
                return run_literal(l)
        elif type(lit[1]) in (type(float()), type(int())):
            return lit[1]
        elif type(lit[1]) == type(list()):
            l = []
            for i in lit[1]:
                l.append(run_literal(i))
            return l
        
    """
    OP_SELECT
        Arguments:
        Numbers representing indices, negative values work from the last. -1 is
        the last element.
        
        Numbers over the size of memory are registers. In Hand would be: 
        sys.memory[REG_HAND]
        
        No arguments specifies going back one level (,). A None value followed by a
        number is the walk specifier. (,5)
        
        If the argument is a single list, it selects the location In Hand.
    """
    def op_select(mimsy, args):
        if args == []:
            mimsy.memory[REG_SELECT].pop()
        elif args[0] == []:
            if type(mimsy.memory[REG_HAND]) == type(list()):
                mimsy.memory[REG_SELECT] = copy.deepcopy(mimsy.memory[REG_HAND])
            else:
                raise Error('($) failed, In Hand not list.')
        elif args[0] == None:
            mimsy.memory[REG_SELECT].append(args[1])
        else:
            mimsy.memory[REG_SELECT] = []
            for arg in args:
                mimsy.memory[REG_SELECT].append(arg)
            
    
    def op_set(mimsy, args):
        sel = mimsy.memory
        indices = mimsy.memory[REG_SELECT]
        for i in indices[:-1]:
            sel = sel[i]
        try:
            sel[indices[-1]] = copy.deepcopy(mimsy.memory[REG_HAND])
        except TypeError:
            raise Error('Select out of bounds')
    
    def op_get(mimsy, args):
        sel = mimsy.memory
        indices = mimsy.memory[REG_SELECT]
        for i in indices[:-1]:
            sel = sel[i]
        try:
            mimsy.memory[REG_HAND] = copy.deepcopy(sel[indices[-1]])
        except TypeError:
            raise Error('Select out of bounds')
    
    def op_lit(mimsy, args):
        mimsy.memory[REG_HAND] = run_literal(args[0])
    
    def op_manip(mimsy, args):
        sel = mimsy.memory
        indices = mimsy.memory[REG_SELECT]
        for i in indices[:-1]:
            sel = sel[i]
        
        if type(mimsy.memory[REG_HAND]) == type(int()):
            if mimsy.memory[REG_HAND] == 0:
                del sel[indices[-1]]
            elif mimsy.memory[REG_HAND] <= 0:
                raise Error(', failed, cannot have negative integers')
            else:
                sel[indices[-1]] = [0] * mimsy.memory[REG_HAND]
        elif type(mimsy.memory[REG_HAND]) == type(list()):
            # List of size one in hand.
            if len(mimsy.memory[REG_HAND]) == 1:
                if type(sel[indices[-1]]) in (type(int()), type(float())):
                    sel[indices[-1]] = mimsy.memory[REG_HAND][0]
                else:
                    for i in range(len(sel[indices[-1]])):
                        sel[indices[-1]][i] = mimsy.memory[REG_HAND][0]
            # List of size two in hand.
            elif len(mimsy.memory[REG_HAND]) == 2:
                if type(sel[indices[-1]]) in (type(int()), type(float())):
                    raise Error('` failed, cannot sublist a non-list')
                else:
                    mimsy.memory[REG_HAND] = \
                        sel[indices[-1]][mimsy.memory[REG_HAND][0]:mimsy.memory[REG_HAND][1]]
            # List of size three in hand.
            elif len(mimsy.memory[REG_HAND]) == 3:
                if type(sel[indices[-1]]) in (type(int()), type(float())):
                    raise Error('` failed, cannot insert a list into a non-list')
                else:
                    hand0 = mimsy.memory[REG_HAND][0]
                    hand1 = mimsy.memory[REG_HAND][1]
                    hand2 = mimsy.memory[REG_HAND][2]
                    # Makes appending possible. TODO: Come up with a standard.
                    if hand1 > hand0:
                        sel[indices[-1]][hand0:] = [0] * hand2
                    else:
                        sel[indices[-1]][hand0:hand1] = [0] * hand2
        elif type(mimsy.memory[REG_HAND]) == type(float):
            raise Error(', failed, cannot have float in hand.')
    
    def op_name(mimsy, args):
        if mimsy.macros.has_key(args[0]):
            ele = mimsy.macros[args[0]]
            if callable(ele):
                ele(mimsy)
            else:
                mimsy.memory[REG_HAND] = copy.deepcopy(ele)
        else:
            raise Error('Undefined macro')
    
    def op_macro(mimsy, args):
        if len(args) == 1:
            mimsy.macros[args[0]] = copy.deepcopy(mimsy.memory[REG_HAND])
        elif len(args) == 2:
            mimsy.macros[args[0]] = copy.deepcopy(args[1])
    
    def op_mult(mimsy, args):
        sel = mimsy.memory
        indices = mimsy.memory[REG_SELECT]
        for i in indices[:-1]:
            sel = sel[i]
        
        if type(sel[indices[-1]]) == type(list()) or \
            mimsy.memory[REG_HAND] == type(list()):
            raise Error('Cannot multiply with lists.')
        else:
            mimsy.memory[REG_HAND] *= sel[indices[-1]]
    
    def op_div(mimsy, args):
        sel = mimsy.memory
        indices = mimsy.memory[REG_SELECT]
        for i in indices[:-1]:
            sel = sel[i]
        
        if type(sel[indices[-1]]) == type(list()) or \
            mimsy.memory[REG_HAND] == type(list()):
            raise Error('Cannot divide with lists.')
        else:
            mimsy.memory[REG_HAND] = sel[indices[-1]] / mimsy.memory[REG_HAND]
    
    def op_mod(mimsy, args):
        sel = mimsy.memory
        indices = mimsy.memory[REG_SELECT]
        for i in indices[:-1]:
            sel = sel[i]
        
        if type(sel[indices[-1]]) == type(list()) or \
            mimsy.memory[REG_HAND] == type(list()):
            raise Error('Cannot mod with lists.')
        else:
            res = sel[indices[-1]] / mimsy.memory[REG_HAND]
            rem = sel[indices[-1]] % mimsy.memory[REG_HAND]
            mimsy.memory[REG_HAND] = [res, rem]
    
    def op_sub(mimsy, args):
        sel = mimsy.memory
        indices = mimsy.memory[REG_SELECT]
        for i in indices[:-1]:
            sel = sel[i]
        
        if type(sel[indices[-1]]) == type(list()) or \
            mimsy.memory[REG_HAND] == type(list()):
            raise Error('Cannot subtract with lists.')
        else:
            mimsy.memory[REG_HAND] = sel[indices[-1]] - mimsy.memory[REG_HAND]
    
    def op_plus(mimsy, args):
        sel = mimsy.memory
        indices = mimsy.memory[REG_SELECT]
        for i in indices[:-1]:
            sel = sel[i]
        
        if type(sel[indices[-1]]) == type(list()) or \
            mimsy.memory[REG_HAND] == type(list()):
            raise Error('Cannot add with lists.')
        else:
            mimsy.memory[REG_HAND] += sel[indices[-1]]
    
    def op_and(mimsy, args):
        sel = mimsy.memory
        indices = mimsy.memory[REG_SELECT]
        for i in indices[:-1]:
            sel = sel[i]
        
        if type(sel[indices[-1]]) == type(list()) or \
            mimsy.memory[REG_HAND] == type(list()):
            raise Error('Cannot and with lists.')
        else:
            mimsy.memory[REG_HAND] = mimsy.memory[REG_HAND] & sel[indices[-1]]
    
    def op_xor(mimsy, args):
        sel = mimsy.memory
        indices = mimsy.memory[REG_SELECT]
        for i in indices[:-1]:
            sel = sel[i]
        
        if type(sel[indices[-1]]) == type(list()) or \
            mimsy.memory[REG_HAND] == type(list()):
            raise Error('Cannot xor with lists.')
        else:
            mimsy.memory[REG_HAND] = mimsy.memory[REG_HAND] ^ sel[indices[-1]]
    
    def op_or(mimsy, args):
        sel = mimsy.memory
        indices = mimsy.memory[REG_SELECT]
        for i in indices[:-1]:
            sel = sel[i]
        
        if type(sel[indices[-1]]) == type(list()) or \
            mimsy.memory[REG_HAND] == type(list()):
            raise Error('Cannot or with lists.')
        else:
            mimsy.memory[REG_HAND] = mimsy.memory[REG_HAND] | sel[indices[-1]]
    
    def op_neg(mimsy, args):
        sel = mimsy.memory
        indices = mimsy.memory[REG_SELECT]
        for i in indices[:-1]:
            sel = sel[i]
        
        if type(sel[indices[-1]]) == type(list()) or \
            mimsy.memory[REG_HAND] == type(list()):
            raise Error('Cannot add with lists.')
        else:
            mimsy.memory[REG_HAND] = ~mimsy.memory[REG_HAND]
    
    def op_not(mimsy, args):
        if mimsy.memory[REG_HAND] in (None, 0):
            mimsy.memory[REG_HAND] = 1
        else:
            mimsy.memory[REG_HAND] = 0
    
    def op_cmp(mimsy, args):
        sel = mimsy.memory
        indices = mimsy.memory[REG_SELECT]
        for i in indices[:-1]:
            sel = sel[i]
        
        if type(sel[indices[-1]]) == type(list()) or \
            mimsy.memory[REG_HAND] == type(list()):
            raise Error('Cannot compare with lists.')
        else:
            mimsy.memory[REG_FLAGS][FLAG_EQ] = int(mimsy.memory[REG_HAND] == sel[indices[-1]])
            mimsy.memory[REG_FLAGS][FLAG_NE] = int(mimsy.memory[REG_HAND] != sel[indices[-1]])
            mimsy.memory[REG_FLAGS][FLAG_LT] = int(sel[indices[-1]] < mimsy.memory[REG_HAND])
            mimsy.memory[REG_FLAGS][FLAG_GT] = int(sel[indices[-1]] > mimsy.memory[REG_HAND])
    
    def op_len(mimsy, args):
        if type(mimsy.memory[REG_HAND]) == type(list()):
            mimsy.memory[REG_HAND] = len(mimsy.memory[REG_HAND])
        else:
            mimsy.memory[REG_HAND] = -1
    
    def op_save(mimsy, args):
        ip = mimsy.memory[REG_IP]
        skip = mimsy.memory[REG_HAND] + 1
        while ip < len(mimsy.memory[REG_CODE]):
            if mimsy.memory[REG_CODE][ip][0] == OP_PLACEHOLD:
                skip -= 1
            
            if skip <= 0:
                break
            ip += 1
        else:
            # Didn't find a ;
            raise Error('@ failed, couldn\'t find a ;')
        mimsy.memory[REG_JMP].append(ip)
    
    def op_placehold(mimsy, args):
        pass
    
    def op_jmp(mimsy, args):
        mimsy.memory[REG_IP] = mimsy.memory[REG_JMP].pop()
    
    def op_jmpeq(mimsy, args):
        if mimsy.memory[REG_HAND] in (None, 0):
            mimsy.memory[REG_IP] = mimsy.memory[REG_JMP].pop()
        else:
            mimsy.memory[REG_JMP].pop()
    
    def op_call(mimsy, args):
        if type(mimsy.memory[REG_HAND]) == type(int()):
            mimsy.memory[REG_JMP].append(mimsy.memory[REG_HAND])
        else:
            raise Error('` Failed, tried to place a not integer on the JMP stack.')
        
    optable = {
        OP_SELECT: op_select,
        OP_SET: op_set,
        OP_GET: op_get,
        OP_LIT: op_lit,
        OP_MANIP: op_manip,
        OP_NAME: op_name,
        OP_MACRO: op_macro,
        OP_MULT: op_mult,
        OP_DIV: op_div,
        OP_MOD: op_mod,
        OP_SUB: op_sub,
        OP_PLUS: op_plus,
        OP_AND: op_and,
        OP_XOR: op_xor,
        OP_OR: op_or,
        OP_NEG: op_neg,
        OP_NOT: op_not,
        OP_CMP: op_cmp,
        OP_LEN: op_len,
        OP_SAVE: op_save,
        OP_PLACEHOLD: op_placehold,
        OP_JMP: op_jmp,
        OP_JMPEQ: op_jmpeq,
        OP_CALL: op_call
    }
    
    while mimsy.memory[REG_IP] < len(mimsy.memory[REG_CODE]):
        # Place any literal in hand.
        if mimsy.memory[REG_CODE][mimsy.memory[REG_IP]][0] == OP_LIT:
            mimsy.memory[REG_HAND] = \
                run_literal(mimsy.memory[REG_CODE][mimsy.memory[REG_IP]])
        else:
            args = []
            for lit in mimsy.memory[REG_CODE][mimsy.memory[REG_IP]][1:]:
                # TODO: Fix the rogue strings in the opcodes, they should be a
                #       lists of numbers.
                if type(lit) == type(str()):
                    args.append(lit)
                else:
                    # Fix for not running macro definition names.
                    if mimsy.memory[REG_CODE][mimsy.memory[REG_IP]][0] == OP_MACRO and \
                        lit[0] == OP_NAME:
                        args.append(lit[1])
                    else:
                        args.append(run_literal(lit))
            optable[mimsy.memory[REG_CODE][mimsy.memory[REG_IP]][0]](mimsy, args)
        mimsy.memory[REG_IP] += 1
    

if __name__ == '__main__':
    f = None
    fstr = ""
    mimsy = System()
    
    for arg in sys.argv[1:]:
        if arg[0] == '-':
            pass
        elif f is None:
            f = open(arg, 'r')
            if f is None:
                print 'Failed to open: %s' % (arg)
    
    if f is None:
        print 'No file specified...'
        sys.exit()
    
    for line in f:
        i = line.find("#")
        fstr += line[0:i] + ' '

    f.close()
    
    try:
        print 'Tokenizing...',
        tokens = tokenize(fstr)
        print 'Success!'
        print 'Analyzing...',
        analize(tokens)
        print 'Success!'
        print 'Compiling...', 
        program = compil(tokens)
        print 'Success!'
        print 'Running...'
        print
        run(mimsy, program)
        print
        print 'Success!'
    except Error as e:
        for arg in e.args:
            print arg,
