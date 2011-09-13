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
# ======================== Statements ==============================
# ==================================================================
@GRAMMER(g.statementlist)
def statementlist(self, p): 
    p[0] = None #[p[1]] if len(p) == 2 else p[1] + [p[3]]

@GRAMMER(g.statement)
def statement(self, p): #TODO: differs 
    p[0] = p[1] if len(p) == 2 else p[3] #TODO: labelid

#TODO: p_stmtlist missing
#TODO: p_simplestatement missing

#TODO: i inserted to fix a problem SEMICOLON???
@GRAMMER(g.stmt_unlabelled)
def stmt_unlabelled(self, p): #TODO: differs #TODO: in o-pas-ref its name is struct:stmt  #TODO: goto missing?? #TODO: i removed empty, i think this is wrong here
    #                | stmt_exit        
    #TODO:  i removed exit, because it could also a function or other thing... refactor if possible
    #TODO: break, continue could be the same as exit!
    #"""
    #unlabelled_stmt : empty 
    #    | general_proc_call
    #    | assignment
    #    | goto_stmt
    #    | compoundstmt
    #    | ifstmt
    #    | break_stmt
    #    | casestmt
    #    | continue_stmt
    #    | exit_stmt
    #    | repeatstmt
    #    | whilestmt
    #    | forstmt
    #    | with_stmt
    #    | tryexceptstmt
    #    | tryfinallystmt
    #    | raisestmt
    #    | assemblerstmt
    #"""        
    #| tryexceptstmt  /* purely Delphi stuff ! */
    p[0] = p[1]

@GRAMMER(g.stmt_compound)
def stmt_compound(self, p): #TODO: i think, compound could also be empty #TODO: could use on other places too
    #TODO: semicolon ist opt #TODO: check if semicolon_opt is true for all case's
    p[0] = pas_ast.CompoundStmt(p[2]) #TODO: rename to stmt_compound or so

@GRAMMER(g.stmt_compound_error)
def stmt_compound_error(self, p): #TODO: this is just a test #TODO: seems not to work!
    print "Syntax Error in compound statement"

@GRAMMER(g.stmt_break)
def stmt_break(self, p):
    p[0] = pas_ast.SomeType('stmt_break')

@GRAMMER(g.stmt_continue)
def stmt_continue(self, p):
    p[0] = pas_ast.SomeType('stmt_continue')

@GRAMMER(g.stmt_exit)
def stmt_exit(self, p): #TODO: i must remove this, because exit could also be an ident
    #TODO: i fake a func_call, with exit here... this is only a hack! implement this in a special way! 
    p[0] = pas_ast.SomeType('stmt_exit')

@GRAMMER(g.stmt_goto)
def stmt_goto(self, p): #TODO: switch back to label_ident, like the other ident's!!!
    #"""
    #stmt_goto : GOTO label_ident
    #"""
    p[0] = pas_ast.SomeType('stmt_goto')

# ------------------- Proc Call --------------------------------------------
@GRAMMER(g.stmt_proc_call_general)
def stmt_proc_call_general(self, p): #TODO: rename, this is a statement too! #TODO: if this is a proc_call, why assignment here? 
    p[0] = pas_ast.SomeType('stmt_proc_call_general')

@GRAMMER(g.inheritedpropertyassignment) #TODO: function calls and other inherited things could be missing!
def inheritedpropertyassignment(self, p):
    p[0] = pas_ast.SomeType('inheritedpropertyassignment')

@GRAMMER(g.stmt_proc_call)
def stmt_proc_call(self, p): #TODO: i removed period because it is inside of variable for now
    #"""
    #proc_call
    #    : variable  /* instead if proc_ident param_list */
    #    | variable PERIOD proc_call
    #    /* | variable PERIOD property ASSIGNMENT expression */
    #"""
    p[0] = pas_ast.SomeType('stmt_proc_call')

@GRAMMER(g.stmt_writeln)
def stmt_writeln(self, p):
    p[0] = pas_ast.SomeType('writeln');

@GRAMMER(g.stmt_writeln_elem_list)
def stmt_writeln_elem_list(self, p):
    p[0] = pas_ast.SomeType('stmt_writeln_elem_list')

@GRAMMER(g.stmt_writeln_elem)
def stmt_writeln_elem(self, p):
    #TODO: this rule is not really correct... just quickly written
    p[0] = pas_ast.SomeType('stmt_writeln_elem')

## =========================================================================
# ---------------------------- Asssignment ---------------------------------
## =========================================================================
@GRAMMER(g.stmt_assignment)
def stmt_assignment(self, p):
    p[0] = pas_ast.Assignment('assignment', p[1], p[3])

## =========================================================================
#   Conditional Statments
## =========================================================================
@GRAMMER(g.stmt_conditional)
def stmt_conditional(self, p):
    p[0] = p[1]

@GRAMMER(g.stmt_if)
def stmt_if(self, p):
    p[0] = pas_ast.SomeType('stmt_if')

@GRAMMER(g.stmt_case)
def stmt_case(self, p): #TODO: differs            
    p[0] = pas_ast.SomeType('stmt_case')

@GRAMMER(g.stmt_case_selector_list)
def stmt_case_selector_list(self, p): #TODO: i think here are multiple SEMIICOLONs possible, so i moved it
    p[0] = pas_ast.SomeType('stmt_case_selector_list')

@GRAMMER(g.stmt_case_selector)
def stmt_case_selector(self, p): #TODO: this seems not allow multiple colon-label-objects (also in recvariant)
    #"""
    #caseselector : empty
    #             | caselabellist COLON statement
    #"""        
    p[0] = pas_ast.SomeType('stmt_case_selector')

@GRAMMER(g.stmt_case_stmt)
def stmt_case_stmt(self, p): #TODO: this seems not allow multiple colon-label-objects (also in recvariant)
    #TODO: are here multiple statements possible, without begin end???
    p[0] = pas_ast.SomeType('stmt_case_stmt')

@GRAMMER(g.stmt_case_labellist)
def stmt_case_labellist(self, p):
    p[0] = pas_ast.SomeType('stmt_case_labellist')

@GRAMMER(g.stmt_case_label)
def stmt_case_label(self, p):
    #TODO: | CHAR_CONST instead of STRING_LITERAL
    #TODO: here are char also possible... is then a const_char, a ordinal???
    #TODO: i temporarly added CHAR_CONST here
    #TODO: does CHAR_CONST allow DOTDOT???
    #TODO: i changed constexpr to const_expr_ordinal
    #"""
    #caselabel : constexpr 
    #          | constexpr DOTDOT constexpr
    #"""
    p[0] = pas_ast.SomeType('caselabel')

@GRAMMER(g.stmt_case_else)
def stmt_case_else(self, p): #TODO: is this in o-pas-ref? #TODO: possible use a generic else
    #TODO: i changed statement_opt to statementlist_opt
    p[0] = pas_ast.SomeType('stmt_case_else')

## =========================================================================
# -------------------- Loop Statement --------------------------------------
## =========================================================================
@GRAMMER(g.stmt_loop)
def stmt_loop(self, p):
    p[0] = p[1]

# -------------------- Reapat Stmt -----------------------------------------
@GRAMMER(g.stmt_repeat)
def stmt_repeat(self, p): #TODO: why here stmt list? and below only stmt? TODO: differs with o-pas-ref
    p[0] = pas_ast.SomeType('stmt_repeat')

# --------------------- While Stmt -----------------------------------------
@GRAMMER(g.stmt_while)
def stmt_while(self, p):
    p[0] = pas_ast.SomeType('stmt_while')

# ------------------- For Statement ----------------------------------------
@GRAMMER(g.stmt_for)
def stmt_for(self, p):
    p[0] = pas_ast.SomeType('stmt_for')

@GRAMMER(g.stmt_for_todowntochoice)
def stmt_for_todowntochoice(self, p):
    p[0] = pas_ast.SomeType('stmt_for_todowntochoice')

# ------------------- With Statement ---------------------------------------
@GRAMMER(g.stmt_with)
def stmt_with(self, p): #TODO: in o-pas-ref this is identlist
    #TODO: refactor, the quick hack with AS stmt
    p[0] = pas_ast.SomeType('stmt_with')

# --------- Exception Handling ---------------------------------------------
@GRAMMER(g.stmt_tryexcept)
def stmt_tryexcept(self, p):
    p[0] = pas_ast.SomeType('stmt_tryexcept')

@GRAMMER(g.stmt_tryexcept_block)
def stmt_tryexcept_block(self, p):
    p[0] = pas_ast.SomeType('stmt_tryexcept_block')

@GRAMMER(g.stmt_tryexcept_else)
def stmt_tryexcept_else(self, p): #TODO: could this not be used on other place too?
    p[0] = pas_ast.SomeType('stmt_tryexcept_else')

@GRAMMER(g.stmt_tryexcept_onlist)
def stmt_tryexcept_onlist(self, p):
    p[0] = pas_ast.SomeType('stmt_tryexcept_onlist')

@GRAMMER(g.stmt_tryexcept_ondef)
def stmt_tryexcept_ondef(self, p): #TODO: i think type is too much, i changed to simpletype
    #TODO: the statement ist opt
    #"""
    #ondef : ON ident COLON type DO statement_opt SEMICOLON
    #      | ON type DO statement_opt SEMICOLON
    #"""
    p[0] = pas_ast.SomeType('stmt_tryexcept_ondef')

@GRAMMER(g.stmt_tryfinally)
def stmt_tryfinally(self, p):
    p[0] = pas_ast.SomeType('stmt_tryfinally') #TODO: is object right here???

@GRAMMER(g.stmt_raise)
def stmt_raise(self, p):                  
    #TODO: the right at?
    p[0] = pas_ast.SomeType('stmt_raise')
