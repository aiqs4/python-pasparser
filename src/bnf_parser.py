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


#! /usr/bin/env python
import os
import sys

import pyparsing as pp
from pyparsing \
     import Word, alphanums, delimitedList, Literal, OneOrMore, ZeroOrMore, White, Or, oneOf, Group, Suppress, Forward, Optional, oneOf
from collections import namedtuple

class Grammer():
    def __init__(self):
        #dict.__init__(self)
        self.rules = dict([])
        self.ruleOrder = []
        self.terminals = []

        self.ruleUsage = dict([])
        self.globalTokenDict = dict([])
        print 'TODO: is globalTokenSet needed'

    def __repr__(self): #TODO: add builtin to insert commas
        s = '{'
        for r in self.rules.keys():
            if len(s) != 1:
                s += ', '
            s += "'" + r + "': " + self.rules[r].__repr__()
        return s + '}'

    def __len__(self):
        return len(self.rules)

    def __iter__(self):
        for ruleName in self.ruleOrder:
            yield self.rules[ruleName]

    def normalize(self):
        for rule in self.rules.values():
            rule.normalize()

    def update_rule_usage(self):
        for rule in self.rules.values():
            for ruleName in self.ruleOrder:            
                if rule.has_token(ruleName):
                    self.ruleUsage[ruleName].append(rule.name)

    def add_rule(self, rule):
        r = Rule(rule, self.globalTokenDict)
        self.rules[rule] = r
        self.ruleOrder.append(rule)
        self.ruleUsage[rule] = []
        return r

    def add_terminal(self, terminal):
        self.terminals.append(terminals)

#    def parse(self, grammer):
#        self.grammer = grammer

class Token():
    def __init__(self, name):
        self.name = name
        self.isEmpty = None

class Rule(Token):
    def __init__(self, name, globalTokenDict):
        Token.__init__(self, name)
        #self.name = name
        self.tokenlists = []
        self.globalTokenDict = globalTokenDict
        self.globalTokenDict[name] = self
        print 'TODO: error handling. handle duplicate rule-names'

    def __repr__(self):
        s = []
        for t in self.tokenlists: #TODO: use builtin function, to insert commas
            s.append(t.tokens)
        return s.__repr__()

    def __len__(self):
        return len(self.tokenlists)

    def __iter__(self):
        for t in self.tokenlists:
            yield t

    def max_tokens(self):
        return max([len(l) for l in self.tokenlists])

    def normalize(self):        
        self.tokenlists = sorted(self.tokenlists, key=len, reverse=True)
        self.isEmpty = self.max_tokens() == 0

    def add_rule_line(self, tokenlist):
        t = TokenList(self.globalTokenDict)
        for _t in tokenlist:
            t.add_token(_t)
        self.tokenlists.append(t)
        return t

    def has_token(self, token):
       for t in self.tokenlists:
           if token in t:           
               return True
       return False

class TokenList():
    def __init__(self, globalTokenDict):
        self.tokens = []
        self.globalTokenDict = globalTokenDict

    def add_token(self, token): #TODO: support caseless compare function
        t = Token(token)
        #print 'TODO: token class unused for now'
        self.tokens.append(token)
        if self.globalTokenDict and not self.globalTokenDict.has_key(token):
            self.globalTokenDict[token] = t

    def __len__(self):
        return len(self.tokens)

    def __repr__(self):
        return self.tokens.__repr__()

    def __iter__(self):
        for t in self.tokens:
            yield t
        

    

ruledef = Optional(ZeroOrMore(Word(alphanums + '_') | '"' + Word(alphanums + '_') + '"' ))
ruledefs = Group(ruledef) + ZeroOrMore(Group(Suppress('|') + ruledef))
rule = Group(Word(alphanums + '_') + Suppress(oneOf([':', '::=', ':='])) + Group(ruledefs) + Suppress(';'))
grammer = ZeroOrMore(rule) # + Forward()


def build_dict(g):
    grammer = Grammer()    
    for rule in g:
        #grammer[rule[0]] = []
        _rule = grammer.add_rule(rule[0])
        for d in rule[1]:
            _rule.add_rule_line(d)
            #grammer[rule[0]].append(d)
    return grammer

def main(filename):
    text = open(filename, 'rU').read()
    g = grammer.parseFile(filename)
    parsedGrammer = build_dict(g)
    parsedGrammer.normalize()
    parsedGrammer.update_rule_usage()
    for rule in parsedGrammer.rules:
        print rule, ':'
        for t in parsedGrammer.rules[rule].tokenlists:
            print t

    print parsedGrammer.ruleUsage

if __name__ == '__main__':
    if len(sys.argv) != 2: #TODO: move to ... or remove
        raise SystemExit("Usage: python main.py <image file>")
    main(sys.argv[1])
