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


#!/usr/bin/env python
import os
import sys

from pyparsing import *

def debug(st,locn,toks):
    print lineno(locn,st), col(locn,st), toks

#ruledef = Optional(ZeroOrMore(Word(alphanums + '_') | '"' + Word(alphanums + '_') + '"' ))
#ruledefs = Group(ruledef) + ZeroOrMore(Group(Suppress('|') + ruledef))
#rule = Group(Word(alphanums + '_') + Suppress(oneOf([':', '::=', ':='])) + Group(ruledefs) + Suppress(';'))
#grammer = ZeroOrMore(rule) # + Forward()

# ========
# Keywords
# ========

kBEGIN = CaselessKeyword('BEGIN')
kEND = CaselessKeyword('END')
#INTERFACE
kIMPLEMENTATION = CaselessKeyword('IMPLEMENTATION')
kINITIALIZATION = CaselessKeyword('INITIALIZATION')
kFINALIZATION = CaselessKeyword('FINALIZATION')

kCONST = CaselessKeyword('CONST')
kTYPE = CaselessKeyword('TYPE')
kVAR = CaselessKeyword('VAR')

kPROCEDURE = CaselessKeyword('PROCEDURE')
kFUNCTION = CaselessKeyword('FUNCTION')
kMETHOD = kPROCEDURE | kFUNCTION
kRESOURCESTRING = CaselessKeyword('RESOURCESTRING')

# ======
# Tokens
# ======

# *** Consts ***

print 'TODO: something is wrong with the numbers!'
tNumberDec = Word(nums) #+ ~FollowedBy('.')
tNumberHex = Combine('$' + OneOrMore(hexnums)) | Combine(OneOrMore(hexnums) + CaselessLiteral('h'))
tNumberOrdinal = tNumberDec | tNumberHex
tFloat = Combine(((Optional(tNumberDec) + '.' + tNumberDec) | (tNumberDec + '.' + Optional(tNumberDec)) ) + Optional(CaselessLiteral('E')+Optional(oneOf('-+'))+tNumberDec))
tNumber = tFloat | tNumberOrdinal
tChar = '#' + tNumberOrdinal

tString = OneOrMore(QuotedString(quoteChar="'") | OneOrMore(tChar))

# *** Comments ***

tCommentSingle = '//' + restOfLine
tCommentMulti = QuotedString(quoteChar="{", multiline=True, endQuoteChar='}') | \
                QuotedString(quoteChar="(*", multiline=True, endQuoteChar='*)')
tComment = tCommentMulti | tCommentSingle

# *** Identifiers ***

identifier = Word(alphas, alphanums + '_')
identifier.setParseAction(debug)

identlist = identifier + ZeroOrMore(',' + identifier)

# *** Operatores ***

operators = oneOf(list('@&;.:=+-/^()<>[],'))

# ====
# Code
# ====

programblock = CaselessKeyword('BEGIN') + Suppress(SkipTo(kEND + FollowedBy('.')))
print 'TODO: implement recursion to END'


# =================
# Interface Section
# =================

consts = kCONST + Suppress(SkipTo((Literal(';') + FollowedBy(kCONST)) | \
                                  (Literal(';') + FollowedBy(kTYPE)) | \
                                  (Literal(';') + FollowedBy(kVAR)) | \
                                  (Literal(';') + FollowedBy(kMETHOD)) | \
                                  (Literal(';') + FollowedBy(kRESOURCESTRING)) | \
                                  (Literal(';') + FollowedBy(kIMPLEMENTATION))) + ';')
types = kTYPE + Suppress(SkipTo((Literal(';') + FollowedBy(kCONST)) | \
                                (Literal(';') + FollowedBy(kTYPE)) | \
                                (Literal(';') + FollowedBy(kVAR)) | \
                                (Literal(';') + FollowedBy(kMETHOD)) | \
                                (Literal(';') + FollowedBy(kRESOURCESTRING)) | \
                                (Literal(';') + FollowedBy(kIMPLEMENTATION))) + ';')
variables = kVAR + Suppress(SkipTo((Literal(';') + FollowedBy(kCONST)) | \
                                   (Literal(';') + FollowedBy(kTYPE)) | \
                                   (Literal(';') + FollowedBy(kVAR)) | \
                                   (Literal(';') + FollowedBy(kMETHOD)) | \
                                   (Literal(';') + FollowedBy(kRESOURCESTRING)) | \
                                   (Literal(';') + FollowedBy(kIMPLEMENTATION))) + ';')
methods = kMETHOD + Suppress(SkipTo((Literal(';') + FollowedBy(kCONST)) | \
                                    (Literal(';') + FollowedBy(kTYPE)) | \
                                    (Literal(';') + FollowedBy(kVAR)) | \
                                    (Literal(';') + FollowedBy(kMETHOD)) | \
                                    (Literal(';') + FollowedBy(kRESOURCESTRING)) | \
                                    (Literal(';') + FollowedBy(kIMPLEMENTATION))) + ';')
resourcestrings = kRESOURCESTRING + Suppress(SkipTo((Literal(';') + FollowedBy(kCONST)) | \
                                                    (Literal(';') + FollowedBy(kTYPE)) | \
                                                    (Literal(';') + FollowedBy(kVAR)) | \
                                                    (Literal(';') + FollowedBy(kMETHOD)) | \
                                                    (Literal(';') + FollowedBy(kRESOURCESTRING)) | \
                                                    (Literal(';') + FollowedBy(kIMPLEMENTATION))) + ';')


uses = Group(CaselessKeyword('USES') + identlist + ';')

# ==============
# Main Structure
# ==============

# *** Unit ***
print 'TODO: portabilitydirective - the number can be a const-expression !'
portabilitydirective =  CaselessKeyword('DEPRECATED') | \
                        CaselessKeyword('LIBRARY') | \
                        (Keyword('PLATFORM', caseless=True) + Optional("=" + tNumber))
portabilitydirective.setParseAction(debug)
unit_interface = Group(CaselessKeyword('INTERFACE') + Optional(uses) + ZeroOrMore(consts | types | variables | methods | resourcestrings) + Suppress(SkipTo(kIMPLEMENTATION)))
unit_implementation = Group((CaselessKeyword('IMPLEMENTATION') + Optional(uses)) + \
                      Suppress(SkipTo(kINITIALIZATION | \
                                      kFINALIZATION | \
                                      (kEND + FollowedBy('.')))))
unit_init_initialization = Group(CaselessKeyword('INITIALIZATION') + \
                           Suppress(SkipTo(kFINALIZATION | \
                                           (kEND + FollowedBy('.')))))
unit_init_finalization = Group(CaselessKeyword('FINALIZATION') + Suppress(SkipTo(kEND + FollowedBy('.'))))
unit_init = (Optional(unit_init_initialization) + Optional(unit_init_finalization)) | \
            programblock

unit_headline = Group(CaselessKeyword('UNIT') + identifier + Suppress(ZeroOrMore(portabilitydirective)) + ';')

unit = unit_headline + unit_interface + unit_implementation + unit_init

# *** Library ***
library = CaselessKeyword('LIBRARY') + identifier + ';' + Optional(uses) + Optional(programblock)

# *** Program ***
program = Optional(CaselessKeyword('PROGRAM') + identifier + Optional(identlist) + ';') + Optional(uses) + programblock
print 'TODO: is programblock optional here?'

# *** Package ***
packageRequires = CaselessKeyword('CONTAINS') + identlist + ';'
packageContains = CaselessKeyword('REQUIRES') + identlist + ';'

package = CaselessKeyword('PACKAGE') + identifier + ';' + Optional(packageRequires) + Optional(packageContains)

# ===========
# Entry Point 
# ===========
grammer = (unit | program | library | package) + kEND + '.'
grammer.ignore(tComment)

def main(filename):
    text = open(filename, 'rU').read()
    g = grammer.parseFile(filename)
    print g

if __name__ == '__main__':
    if len(sys.argv) != 2: #TODO: move to ... or remove
        raise SystemExit("Usage: python main.py <image file>")
    main(sys.argv[1])

