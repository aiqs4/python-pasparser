#
# Copyright (C) 2011 sascha.dewald@googlemail.com
#
# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 2 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 51
# Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
#


#-----------------------------------------------------------------
# pycparser: pas_lexer.py
#
# PasLexer class: lexer for the Pascal (Delphi) language
#
# Author: Sascha Dewald <sascha.dewald@googlemail.com>
# Copyright (C) 2011: Sascha Dewald <sascha.dewald@googlemail.com>
# 
# Based on cpyparser from Copyright (C) 2008-2010, Eli Bendersky
#-----------------------------------------------------------------

import re
import sys

import ply.lex
from ply.lex import TOKEN


class PasLexer(object):
    """ A lexer for the Pascal (Delhpi) language. After building it, set the
        input text with input(), and call token() to get new 
        tokens.
        
        The public attribute filename can be set to an initial
        filaneme, but the lexer will update it upon #line 
        directives.
    """
    def __init__(self,  error_func, 
                        type_lookup_func,
                        class_lookup_func,
                        const_lookup_func,
                        label_lookup_func,
                        unit_lookup_func):
      """ Create a new Lexer.
      
          error_func:
              An error function. Will be called with an error
              message, line and column as arguments, in case of 
              an error during lexing.
              
          type_lookup_func:
              A type lookup function. Given a string, it must
              return True IFF this string is a name of a type
              that was defined with a typedef earlier.
      """
      self.error_func = error_func #TODO: check!
      self.type_lookup_func     = type_lookup_func
      self.class_lookup_func  = class_lookup_func
      self.const_lookup_func     = const_lookup_func
      self.label_lookup_func     = label_lookup_func
      self.unit_lookup_func     = unit_lookup_func
      self.filename = ''
      
      #TODO: ???
      # Allow either "# line" or "# <num>" to support GCC's
      # cpp output
      #
      #self.line_pattern = re.compile('([ \t]*line\W)|([ \t]*\d+)')

    def build(self, **kwargs):
      """ Builds the lexer from the specification. Must be
          called after the lexer object is created. 
          
          This method exists separately, because the PLY
          manual warns against calling lex.lex inside
          __init__
      """
      self.lexer = ply.lex.lex(object=self, **kwargs)
      #self.lexer = ply.lex.lex(object=self, reflags=re.IGNORECASE, **kwargs) #TODO: INFO: changed to case-insensitive


    def reset_lineno(self):
      """ Resets the internal line number counter of the lexer.
      """
      self.lexer.lineno = 1

    def input(self, text):
      self.lexer.input(text)
    
    def token(self):
      #print '...token...'
      g = self.lexer.token()    
      return g

    ######################--   PRIVATE   --######################
    
    ##
    ## Internal auxiliary methods
    ##
    def _error(self, msg, token):
      location = self._make_tok_location(token)
      self.error_func(msg, location[0], location[1])
      self.lexer.skip(1)
    
    def _find_tok_column(self, token):
      i = token.lexpos
      while i > 0:
        if self.lexer.lexdata[i] == '\n': break
        i -= 1
      return (token.lexpos - i) + 1
    
    def _make_tok_location(self, token):
      return (token.lineno, self._find_tok_column(token))
    
    ##
    ## Reserved keywords
    ##
    #TODO
    #keywords = (
    #    'AUTO', 'BREAK', 'CASE', 'CHAR', 'CONST', 'CONTINUE', 
    #    'DEFAULT', 'DO', 'DOUBLE', 'ELSE', 'ENUM', 'EXTERN', 
    #    'FLOAT', 'FOR', 'GOTO', 'IF', 'INLINE', 'INT', 'LONG', 'REGISTER',
    #    'RESTRICT', 'RETURN', 'SHORT', 'SIGNED', 'SIZEOF', 'STATIC', 'STRUCT',
    #    'SWITCH', 'TYPEDEF', 'UNION', 'UNSIGNED', 'VOID', 
    #    'VOLATILE', 'WHILE', 
    #)
    keywords = ( #TODO: split into keywords, directive's, type's
        'ABSOLUTE', 'ABSTRACT', 'AND', #'ANSISTRING',
        'ARRAY', 'AS', #'ASSEMBLER', #TODO: assembler directive is only used for backward compatibility for delphi 1 
        'ASM', 'AT', 'AUTOMATED', 
        'BEGIN', 'BREAK', #'BOOLEAN', 'BYTE',
        'CASE', 'CDECL', #'CHAR',
        'CLASS', #'COMP', 
        'CONST', 'CONSTRUCTOR', 'CONTAINS', 'CONTINUE', #'CURRENCY', 
        'DEFAULT', 'DEPRECATED', 'DESTRUCTOR', 'DISPID', #'DISPOSE', #TODO: implement this
        #'DISPINTERFACE', #TODO: implement this 
        'DIV', 'DO', #'DOUBLE',
        'DOWNTO', 'DYNAMIC', 
        'ELSE', 'END', 'EXCEPT', 'EXIT', 
        'EXPORT', 'EXPORTS', 'EXTERNAL',
        'EXTENDED', 
        #'FALSE', 
        'FAR', 'FILE', 'FINALIZATION', 'FINALLY', 'FOR', 'FORWARD',
        'FUNCTION',
        'GOTO', 
        'HIGH', #TODO: i added low and high, because this is a special kind of functions
        'IF', 'IMPLEMENTATION', 
        'IMPLEMENTS', 'IN', 'INHERITED', #'INT64', 'INTEGER', 
        'INDEX', #'INLINE', 
        'INTERFACE',  
        'INITIALIZATION', 'INLINE', 'IS',
        'LABEL', 'LIBRARY', 'LOCAL', #'LONGINT', 'LONGWORD',
        'LOW',  #TODO: i added low and high, because this is a special kind of functions
        'MESSAGE', 'MOD',
        'NAME', 'NEAR', #'NEW', 
        'NIL', 'NODEFAULT', 'NOT',
        'OBJECT', 'OF', #'OLEVARIANT',
        'ON', 'OR', 'OVERLOAD', 'OVERRIDE', 'OUT',
        'PACKAGE', 'PACKED', 'PLATFORM', 'PASCAL', #'PCHAR', 'POINTER', 
        'PRIVATE', 'PROCEDURE', 'PROGRAM', 'PROPERTY', 
        'PROTECTED', 'PUBLIC', 'PUBLISHED',     
        'RAISE', 'READ', 'READONLY', #'REAL', 'REAL48',
        'RECORD', 'REGISTER', 'REINTRODUCE', 'REPEAT', 'REQUIRES', 
        'RESIDENT', 'RESOURCESTRING',
        'SAFECALL', 'SELF', 'SET', 'SHL', 'SHR', #'SHORTINT', 
        #'SINGLE',
        'SIZEOF', 
        #'SMALLINT',  
        'STDCALL', 'STORED', #'STRING', #'STRINGRESOURCE', 
        'THEN', #'THREADVAR', 
        'TO', #'TRUE', 
        'TRY', 'TYPE', 
        'UNIT', 'UNTIL', 'USES',  
        'VAR', 'VARARGS', #'VARIANT',
        'VIRTUAL',
        'WHILE', #'WIDECHAR', 'WIDESTRING',
        'WITH', #'WORD',
        'WRITE', 'WRITELN', #TODO: add writeLN because inside of writeln there exists some special formatings 
        'WRITEONLY',
        'XOR', 
    )

    types = ('ANSISTRING',
             'BOOLEAN', 'BYTE',
             'CHAR', 'COMP', 'CURRENCY',
             'DOUBLE', 'DWORD',
             'INT64', 'INTEGER',
             'LONGINT', 'LONGWORD', 
             'OLEVARIANT',
             'PCHAR', 'POINTER',
             'REAL', 'REAL48',
             'SHORTINT', 'SINGLE', 'SMALLINT', 'STRING',
             'VARIANT',
             'WIDECHAR', 'WIDESTRING', 'WORD', 
        )

    #TODO: hack to temporary allow the refactoring!
    keywords += types

    keyword_map = {}
    for r in keywords:
      keyword_map[r.lower()] = r

    type_map = {}
    for t in types:
      type_map[r.lower()] = r
      #keyword_map[r.lower()] = r

    ##
    ## All the tokens recognized by the lexer
    ##
    tokens = keywords + (
        # Identifiers
        'ID', 
        
        # Type identifiers (identifiers previously defined as 
        # types with typedef)
        'TYPE_ID', #'CLASS_ID', 
        'VAR_ID', 'CONST_ID', #TODO: this are new
        'LABEL_ID', 'UNIT_ID',
        
        'DOTDOT', #TODO: i moved dotdot to here, because it needs be parsed befor float!
        
        # constants 
        'INT_CONST_DEC', 'INT_CONST_OCT', 'INT_CONST_HEX',
        'FLOAT_CONST', 
        'CHAR_CONST', #TODO: char_const not implemented, and no warnings!!!
        #'WCHAR_CONST',l #TODO
        
        # String literals
        'STRING_LITERAL', #TODO: add GUID-support, for Com-Interfaces!!!!!!
        #'WSTRING_LITERAL', #TODO

        # Operators 
        'PLUS', 'MINUS', 'TIMES', 'DIVIDE', 

        'LT', 'LE', 'GT', 'GE', 'EQ', 'NE',
        
        # Assignment
        'ASSIGNMENT', 
       
        # Delimeters 
        'LPAREN', 'RPAREN',         # ( )
        'LBRACKET', 'RBRACKET',     # [ ]
        #'LBRACE', 'RBRACE',         # { } #TODO: handled by multiline comment #TODO: compiler-directiven 
  
        #'COMMENT_START',
        #'COMMENT_END',
        #'COMPILER_DIRECTIVE_START', 
        
        'COMMA', 'PERIOD',          # . ,       
        'SEMICOLON', 'COLON',            # ; :

#        'DOLLAR', #TODO: did i need the dollar-symbol?
        'CIRCUMFLEX',             # ^ 
        'ATSIGN',                # @

        

        #TODO #TODO #TODO #TODO #TODO
        #"**"    return(STARSTAR);
        #"->"   |    
        #"^"    return(UPARROW);

        # pre-processor 
        #'PPHASH',      # '#' #TODO

        # comments #TODO: removed, because parser can't handle this now
        #'COMMENT_SINGLE',
        #'COMMENT_MULTI',
        
        #'USES', #TODO: this line is a hack to stop after uses
        #'INTERFACE',  #TODO: hack to stop parsing after uses
        #'IMPLEMENTATION', #TODO: remove implementation, hack to stop parsing after implementation
    )

    ##
    ## Regexes for use in tokens
    ##
    ##

    identifier = r'[a-zA-Z_][0-9a-zA-Z_]*'

    decimal_constant = '[0-9]+' #TODO: i think, integer could also written with positive Exponent
    hex_constant = '(\$[0-9a-fA-F]+|[0-9]+[0-9a-fA-F]*[hH])' #'\$[0-9a-fA-F]+' #TODO: check if this works correct
    string_literal = r"(\'[^']+\')|(\'\')|(\#[0-9]+)|(\#\$[0-9a-fA-F]+)" #TODO: this could be optimized

    exponent_part = r"([eE][-+]?[0-9]+)"
    fractional_constant = r"([0-9]+\.(?=[^\.])[0-9]*)" #r"([0-9]+\.[0-9]*)|([0-9]+\.)"
    floating_constant = '(((('+fractional_constant+')'+exponent_part+'?)|([0-9]+'+exponent_part+'))[FfLl]?)'
    
    semicolon            = r';'

    dotdot = r'\.\.'

    eof = r'\z'

    ##
    ## Lexer states
    ##
    #TODO: Handle Comments!!!
    states = (
        # multicomment: multiline comment
        # 
        ('multicomment', 'exclusive'),
        ('multicommentCurlyBrace', 'exclusive'),
    #    ('multicomment_parens', 'exclusive'), #TODO: removed ppline state
        
    )

    def t_multicomment(self, t): #TODO: find a way, to handle compiler-directives!
      r'\(\*'        
      t.lexer.begin('multicomment')

    def t_multicomment_text(self, t):
      r'.+(?=\*\))'
      pass #TODO: just ignore!
   
    def t_multicomment_end(self, t):
      r'\*\)'
      t.lexer.begin('INITIAL')

    def t_multicomment_NEWLINE(self, t):
      r'\n+'
      t.lexer.lineno += t.value.count("\n")        

    t_multicomment_ignore = '\t ' #TODO: there exists other whitespace's too
    #t_multicomment_ignore = r'(?!*)).*' #TODO: move this to a function
    
    def t_multicomment_error(self, t):
      msg = 'Illegal character %s' % repr(t.value[0])
      self._error(msg, t)



    def t_multicommentCurlyBrace(self, t): #TODO: find a way, to handle compiler-directives!
      r'\{'        
      t.lexer.begin('multicommentCurlyBrace')
   
    def t_multicommentCurlyBrace_end(self, t):
      r'\}'
      t.lexer.begin('INITIAL')

    def t_multicommentCurlyBrace_NEWLINE(self, t):
      r'\n+'
      t.lexer.lineno += t.value.count("\n")        

    t_multicommentCurlyBrace_ignore = " \t" #TODO: there exists other whitespace's too
    #t_multicommentCurlyBrace_ignore = r"[^\}]?" #TODO: move this to a function

    def t_multicommentCurlyBrace_text(self, t):
      r'[^}]'
      pass #TODO: just ignore!

    def t_multicommentCurlyBrace_error(self, t):
      msg = 'Illegal character %s' % repr(t.value[0])
      self._error(msg, t)
       
    ##
    ## Rules for the normal state
    ##
    t_ignore = ' \t'

    @TOKEN(eof)
    def t_EOF(self, t):
      print "EOF"

    # Newlines
    def t_NEWLINE(self, t):
      r'\n+'
      t.lexer.lineno += t.value.count("\n")

    # Operators
    t_PLUS              = r'\+'
    t_MINUS             = r'-'
    t_TIMES             = r'\*'
    t_DIVIDE            = r'/'

    t_LT                = r'<'
    t_GT                = r'>'
    t_LE                = r'<='
    t_GE                = r'>='
    t_EQ                = r'=' #r'==' #TODO
    t_NE                = '<>' #r'!=' #TODO

    # Assignment operators
    t_ASSIGNMENT         = r':='

    # Delimeters
    t_LPAREN            = r'\('
    t_RPAREN            = r'\)'
    t_LBRACKET          = r'\['
    t_RBRACKET          = r'\]'    

    t_COMMA             = r','
    t_PERIOD            = r'\.'
    #t_SEMICOLON         = r';' #TODO: i removed SEMICOLON, because of uses hack!
    t_COLON             = r':'
    #t_DOLLAR            = r'\$' #TODO
    t_CIRCUMFLEX        = r'\^'

    t_ignore_COMMENT_SINGLE     = r'//.*' #TODO: move this down, as function!!! like multi comment
                            
    t_ATSIGN            = r'\@'
    t_STRING_LITERAL    = string_literal
    
    @TOKEN(dotdot) #TODO: i moved dotdot to here, because it have to be parsed before float_const 
    def t_DOTDOT(self, t):
        return t
    
    @TOKEN(semicolon)
    def t_SEMICOLON(self, t):
        return t
    
    # The following floating and integer constants are defined as 
    # functions to impose a strict order (otherwise, decimal
    # is placed before the others because its regex is longer,
    # and this is bad)
    #
    @TOKEN(floating_constant)
    def t_FLOAT_CONST(self, t):
      return t

    @TOKEN(hex_constant)
    def t_INT_CONST_HEX(self, t):
      return t

    @TOKEN(decimal_constant)
    def t_INT_CONST_DEC(self, t):
      return t

    @TOKEN(identifier)
    def t_ID(self, t):
      t.type = self.keyword_map.get(t.value.lower(), "ID") #TODO: is this correct? (lower)

      if t.type == 'ID':
        if self.type_lookup_func(t.value):
          t.type = "TYPE_ID"
        elif self.class_lookup_func(t.value):
          t.type = "CLASS_ID"            
        elif self.const_lookup_func(t.value):
          t.type = "CONST_ID"
        elif self.label_lookup_func(t.value):
          t.type = "LABEL_ID"
        elif self.unit_lookup_func(t.value):
          t.type = "UNIT_ID"
          
      return t
    
    def t_error(self, t):
      msg = 'Illegal character %s' % repr(t.value[0])
      self._error(msg, t)

if __name__ == "__main__":
  from portability import printme, printnewline
  
  if len(sys.argv) > 1:
    filename = sys.argv[1]
  else:
    filename = 'examples/data/zp_simple.pas'

  text = open(filename, 'rU').read()
  
  #~ text = '"'+r"""ka \p ka"""+'"'
  #text = r"""
  #546
  #    #line 66 "kwas\df.h" 
  #    id 4
  #    # 5 
  #    dsf
  #"""
  
  def errfoo(msg, a, b):
    printme(msg)
    sys.exit()
  
  def typelookup(namd):
    return False
  
  def constlookup(namd):
    return False
      
  def labellookup(namd):
    return False
  
  def unitlookup(namd):
    return False
  
  def classlookup(namd):
    return False
  
  paslex = PasLexer(errfoo, 
                    typelookup,
                    classlookup,
                    constlookup,
                    labellookup,
                    unitlookup)
  paslex.build(reflags=re.IGNORECASE) #TODO: INFO: changed to case-insensitive
  #paslex.build()
  
  paslex.input(text)
  
  while 1:
    tok = paslex.token()
    if not tok: break
        
    #~ print type(tok)
    printme([tok.value, tok.type, tok.lineno, paslex.filename, tok.lexpos])
  printnewline()
        
        

