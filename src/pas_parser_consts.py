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

import pas_ast

import pas_grammer as g
from pas_grammer import GRAMMER


# ==================================================================
@GRAMMER(g.const_section)
def const_section(self, p): #TODO: this and the below differs #TODO: in D7grammer from gold, there is resourcestring here also
    #"""
    #constsection : CONST constantdecl
    #             | constsection constantdecl
    #"""
    #p[0] = [p[2]] if len(p) == 3 else [p[3]]
    p[0] = pas_ast.ConstSection(p[2])

@GRAMMER(g.const_decl_list)
def const_decl_list(self, p): #TODO: this and the above differs
    p[0] = [p[1]] if len(p) == 2 else p[1] + [p[2]]

@GRAMMER(g.const_decl)
def const_decl(self, p): #TODO: this and the above differs #TODO: typeid or complete type?
    #
    #           | ident_unit PERIOD ident EQ const_expr_typed_elem portabilitydirectivelist_opt SEMICOLON
    #           | ident_unit PERIOD ident COLON type EQ const_expr_typed_elem portabilitydirectivelist_opt SEMICOLON 
    #TODO: i added ident for unit name #TODO: this must be wrong!!! why unit name here???
    #"""
    #constantdecl : ident EQ typedconstant portabilitydirective_opt SEMICOLON
    #             | ident COLON type EQ typedconstant portabilitydirective_opt SEMICOLON 
    #"""
    self._add_constdef_type(p[1])
    p[0] = pas_ast.ConstDecl(p[1]) #TODO: not all params are here stored

# ==================================================================


## =========================================================================
# ==================================================================
@GRAMMER(g.const_expr_ordinal_list)
def const_expr_ordinal_list(self, p): #TODO: i'm not happy with a universal constexpr, so i started this
    p[0] = pas_ast.SomeType('const_expr_ordinal_list')


@GRAMMER(g.const_expr_ordinal)
def const_expr_ordinal(self, p): #TODO: i'm not happy with a universal constexpr, so i started this
    #TODO: i changed ident to CONST_ID!
    #TODO: added sizeof... todo, refactor this, was only a quick hack for now!
    ###"""
    #TODO: i added LOW and HIGH Here... check if this is correct for all case's
    #TODO: i temporary removed LPAREN & RPAREN, because of conflicts with set of and enumerated-type #TODO: implement this in future
    p[0] = pas_ast.SomeType('const_expr_ordinal')

@GRAMMER(g.const_ordinal_number)
def const_ordinal_number(self, p): #TODO: check ident, could also be unit.ident... #TODO: hex/octal-numbers!
    #TODO: moved ident to expr
    #TODO: i temporary removed LPAREN & RPAREN, because of conflicts with set of and enumerated-type #TODO: implement this in future
    p[0] = pas_ast.SomeType('const_ordinal_number')

# ------------------------------------------------------------------
@GRAMMER(g.const_expr_generic)
def const_expr_generic(self, p): #TODO: this is not perfect, because strings couldn't be divide, multiply...
    p[0] = pas_ast.SomeType('const_expr_generic')

@GRAMMER(g.const_element_generic)
def const_element_generic(self, p): #TODO: ident could also in sub-elements #TODO: unit.ident is missing
    #TODO: i change ident to CONST_ID  
    #TODO: added set_const... there are other const-types inside typeconstant... needs refactoring!!!
    #TODO: i removed set_const, i don't know why it is here?! split this type in other parts!
    p[0] = pas_ast.SomeType('const_element_generic')

@GRAMMER(g.const_element_number)
def const_element_number(self, p): #TODO: this is not perfect, here could also be a ident 
    #TODO: added sizeof... todo, refactor this, was only a quick hack for now!
    ###"""
    p[0] = pas_ast.SomeType('const_element_number')

# ==================================================================

@GRAMMER(g.const_expr_typed_elem)
def const_expr_typed_elem(self, p): #TODO: set_const not in ref here #TODO: remove or replace constexpr here
    #TODO: i replaced constexpr with const_expr_generic... but, i feel this is not complety right now!!!
    #"""
    #typedconstant : constexpr
    #              | arrayconstant
    #              | recordconstant
    #              | set_const
    #"""
    p[0] = pas_ast.SomeType('const_expr_typed_elem')

@GRAMMER(g.const_expr_typed_elem_list)
def const_expr_typed_elem_list(self, p): #TODO: not in ref 
    p[0] = pas_ast.SomeType('const_expr_typed_elem_list')

@GRAMMER(g.const_expr_array)
def const_expr_array(self, p): #TODO: different #TODO: only used in typedconstant for now
    p[0] = pas_ast.SomeType('arrayconstant')

@GRAMMER(g.const_expr_record)
def const_expr_record(self, p): #TODO: different name and semi-colon #TODO: only used in typedconstant for now
    p[0] = pas_ast.SomeType('const_expr_record')

@GRAMMER(g.const_expr_record_field_list)
def const_expr_record_field_list(self, p):
    p[0] = pas_ast.SomeType('const_expr_record_field_list')

@GRAMMER(g.const_expr_record_field)
def const_expr_record_field(self, p): #TODO: different name 
    p[0] = pas_ast.SomeType('const_expr_record_field')

@GRAMMER(g.const_expr_set)
def const_expr_set(self, p): #TODO: this loops, the const_expr... for what??? feels like a bad design
    #TODO: where is this is used, because of LBRACKET and RBRACKET??
    p[0] = pas_ast.SomeType('const_expr_set')

@GRAMMER(g.const_expr_set_elem_list)
def const_expr_set_elem_list(self, p):
    p[0] = pas_ast.SomeType('const_expr_set_elem_list')

@GRAMMER(g.const_expr_set_elem)
def const_expr_set_elem(self, p): #TODO: should this not better a ordinal? #TODO: change to const_expr_generic
    #TODO: i added LPAREN RPAREN, but there are more place, where this is missing... because, the left const, couldn't start with LPAREN RPAREN
    #"""
    #set_const_elem : constexpr
    #               | constexpr DOTDOT constexpr
    #"""
    p[0] = pas_ast.SomeType('const_expr_set_elem')
