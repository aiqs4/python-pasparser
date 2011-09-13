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

block = \
        """
        block : declsectionlist_opt exports_section_list_opt stmt_compound exports_section_list_opt
        """

const_decl = \
        """
        const_decl : ident EQ const_expr_typed_elem portabilitydirectivelist_opt SEMICOLON
                   | ident COLON type EQ const_expr_typed_elem portabilitydirectivelist_opt SEMICOLON
        """

const_decl_list = \
        """
        const_decl_list : const_decl
                        | const_decl_list const_decl
        """

const_element_generic = \
        """
        const_element_generic : const_element_number
                              | CONST_ID
                              | unit_ident PERIOD CONST_ID
                              | const_general_string
                              | NIL
                              | NOT const_element_generic
                              | typeid_castid LPAREN const_expr_generic RPAREN
        """

const_element_number = \
        """
        const_element_number : const_unsigned_number
                             | sign const_element_number
                             | LPAREN const_element_number RPAREN
                             | SIZEOF LPAREN const_expr_generic RPAREN
        """

const_expr_array = \
        """
        const_expr_array : LPAREN const_expr_typed_elem_list RPAREN
        """

const_expr_generic = \
        """
        const_expr_generic : const_element_generic
                           | LPAREN const_expr_generic RPAREN 
                           | const_expr_generic op_rel const_element_generic
                           | const_expr_generic op_add const_element_generic
                           | const_expr_generic op_mul const_element_generic
        """

const_expr_ordinal = \
        """
        const_expr_ordinal : const_ordinal_number
                           | const_expr_ordinal op_add const_ordinal_number
                           | const_expr_ordinal op_mul const_ordinal_number
                           | LOW LPAREN type_ordinal RPAREN
                           | HIGH LPAREN type_ordinal RPAREN
                           | CONST_ID
                           | SIZEOF LPAREN const_expr_generic RPAREN
        """

const_expr_ordinal_list = \
        """
        const_expr_ordinal_list : const_expr_ordinal
                                | const_expr_ordinal_list COMMA const_expr_ordinal
        """

const_expr_record = \
        """
        const_expr_record : LPAREN const_expr_record_field_list RPAREN
        """

const_expr_record_field = \
        """
        const_expr_record_field : ident COLON const_expr_typed_elem
        """

const_expr_record_field_list = \
        """
        const_expr_record_field_list : const_expr_record_field
                                     | const_expr_record_field_list SEMICOLON const_expr_record_field
        """

const_expr_set = \
        """
        const_expr_set : LBRACKET const_expr_set_elem_list_opt RBRACKET
        """

const_expr_set_elem = \
        """
        const_expr_set_elem : const_expr_generic
                            | const_expr_generic DOTDOT const_expr_generic
        """

const_expr_set_elem_list = \
        """
        const_expr_set_elem_list : const_expr_set_elem
                                 | const_expr_set_elem_list COMMA const_expr_set_elem
        """

const_expr_typed_elem = \
        """
        const_expr_typed_elem : const_expr_generic
                              | const_expr_array
                              | const_expr_record
                              | const_expr_set
        """

const_expr_typed_elem_list = \
        """
        const_expr_typed_elem_list : const_expr_typed_elem
                                   | const_expr_typed_elem_list COMMA const_expr_typed_elem
        """

const_general_string = \
        """
        const_general_string : STRING_LITERAL
                             | const_general_string STRING_LITERAL
                             | CHAR_CONST
        """

const_ordinal_number = \
        """
        const_ordinal_number : const_unsigned_integer
                             | sign const_ordinal_number
        """

const_section = \
        """
        const_section : CONST const_decl_list
        """

const_string_expr = \
        """
        const_string_expr : const_general_string
                          | const_string_expr PLUS const_general_string
        """

const_unsigned_integer = \
        """
        const_unsigned_integer : INT_CONST_DEC
                               | INT_CONST_HEX
                               | INT_CONST_OCT 
        """

const_unsigned_number = \
        """
        const_unsigned_number : const_unsigned_integer
                              | FLOAT_CONST
        """

declsection = \
        """
        declsection : label_section
                    | const_section
                    | type_section
                    | var_section
                    | proceduredeclsection
        """

declsectionlist = \
        """
        declsectionlist : declsection
                        | declsectionlist declsection
        """

directive = \
        """
        directive : CDECL
                  | REGISTER
                  | DYNAMIC
                  | VIRTUAL
                  | EXPORT
                  | external_statement

                  | NEAR
                  | FAR
                  
                  | INLINE
                  | MESSAGE const_expr_ordinal
                  | OVERRIDE
                  | OVERLOAD
                  | PASCAL
                  | REINTRODUCE
                  | SAFECALL
                  | STDCALL
                  | VARARGS
                  | LOCAL
                  | ABSTRACT
                  | DISPID const_ordinal_number
        """

directive_as_name = \
        """
        directive_as_name : ABSOLUTE
                          | ABSTRACT
                          | AT
                          | CDECL
                          | CONTAINS
                          | DEFAULT
                          | DYNAMIC
                          | EXPORT
                          | FAR
                          | HIGH
                          | INDEX
                          | LOCAL
                          | LOW
                          | MESSAGE
                          | NAME
                          | NEAR
                          | NODEFAULT
                          | OVERLOAD
                          | OVERRIDE
                          | PASCAL
                          | READ
                          | REGISTER
                          | REINTRODUCE
                          | SAFECALL
                          | STDCALL
                          | STORED
                          | VARARGS
                          | VIRTUAL
                          | WRITE
        """

empty = \
  """
  empty :
  """

exports_item = \
  """
  exports_item : ident exports_nameindex_opt
  """

exports_item_list = \
        """
        exports_item_list : exports_item
                          | exports_item_list COMMA exports_item
        """

exports_nameindex_opt = \
        """
        exports_nameindex_opt : empty
                              | NAME STRING_LITERAL
                              | INDEX const_expr_ordinal
                              | INDEX const_expr_ordinal NAME STRING_LITERAL
                              | RESIDENT 
        """

exports_section = \
        """
        exports_section : EXPORTS exports_item_list
        """

exports_section_list = \
        """
        exports_section_list : exports_section
                             | exports_section_list exports_section
        """

expr_factor = \
        """
        expr_factor : variable                  
                    | const_unsigned_number
                    | const_general_string
                    | NIL   
                    | LPAREN expression RPAREN 
                    | LPAREN expression RPAREN CIRCUMFLEX
                    | NOT expr_factor
                    | var_set_constructor
                    | variable_cast
                    | variable_cast CIRCUMFLEX
        """

expr_simple = \
        """
        expr_simple : expr_term
                    | sign expr_term
                    | ATSIGN expr_term
                    | expr_simple op_add expr_term
        """

expr_term = \
        """
        expr_term : expr_factor
                  | expr_term op_mul expr_factor
        """

expression = \
        """
        expression : expr_simple
                   | expr_simple op_rel expr_simple
                   | INHERITED expression
        """

exprlist = \
        """
        exprlist : expression 
                 | exprlist COMMA expression
        """

exprlist2 = \
        """
        exprlist2 : LPAREN exprlist RPAREN
        """

external_statement = \
      """
      external_statement : EXTERNAL
                         | EXTERNAL NAME STRING_LITERAL
                         | EXTERNAL INDEX const_expr_ordinal
                         | EXTERNAL INDEX const_expr_ordinal NAME STRING_LITERAL
      """

file_library = \
        """
        file_library : LIBRARY ident SEMICOLON section_programblock PERIOD
        """

file_package = \
        """
        file_package : PACKAGE ident SEMICOLON file_package_requires_opt file_package_contains_opt END PERIOD
        """

file_package_contains = \
        """
        file_package_contains : CONTAINS identlist SEMICOLON
        """

file_package_requires = \
        """
        file_package_requires : REQUIRES identlist SEMICOLON
        """

file_program = \
        """
        file_program : PROGRAM ident identlist_opt SEMICOLON section_programblock PERIOD  
                     | section_programblock PERIOD
        """

file_unit = \
        """
        file_unit : UNIT ident portabilitydirectivelist_opt SEMICOLON section_interface section_implementation file_unit_section_init PERIOD
        """

file_unit_section_init = \
        """
        file_unit_section_init : INITIALIZATION statementlist END
                               | INITIALIZATION statementlist FINALIZATION statementlist END
                               | FINALIZATION statementlist END
                               | BEGIN statementlist END
                               | END
        """

ident = \
        """
        ident : ID
              | SELF
              | directive_as_name
        """

identlist = \
        """
        identlist : ident
                  | identlist COMMA ident
        """

identlist_opt = \
        """
        identlist_opt : empty
                      | LPAREN identlist RPAREN
        """

inheritedpropertyassignment = \
        """
        inheritedpropertyassignment : variable PERIOD inheritedpropertyassignment
                                    | variable ASSIGNMENT expression
        """

label_ident = \
        """
        label_ident : INT_CONST_DEC
                    | ident
        """

label_ident_list = \
        """
        label_ident_list : label_ident
                         | label_ident_list COMMA label_ident
        """

label_section = \
        """
        label_section : LABEL label_ident_list SEMICOLON
        """

method = \
        """
        method : method_head_class_exported
        """

method_decl_constructor = \
        """
        method_decl_constructor : method_head_constructor portabilitydirectivelist2_opt method_proc_block_section SEMICOLON             
        """

method_decl_destructor = \
        """
        method_decl_destructor : method_head_destructor portabilitydirectivelist2_opt method_proc_block_section SEMICOLON
        """

method_decl_function = \
        """
        method_decl_function : method_head_function portabilitydirectivelist2_opt method_proc_block_section SEMICOLON
                             | method_head_function portabilitydirectivelist2_opt stmt_assembler
        """

method_decl_procedure = \
        """
        method_decl_procedure : method_head_procedure portabilitydirectivelist2_opt method_proc_block_section SEMICOLON
                              | method_head_procedure portabilitydirectivelist2_opt stmt_assembler
        """

method_head_class_exported = \
        """
        method_head_class_exported : method_head_exported
                                   | CLASS method_head_procedure
                                   | CLASS method_head_function
                                   | method_head_constructor
                                   | method_head_destructor
        """

method_head_constructor = \
        """
        method_head_constructor : CONSTRUCTOR qualified_identifier method_param_list_opt proc_directive_list_opt
        """

method_head_destructor = \
        """
        method_head_destructor : DESTRUCTOR qualified_identifier method_param_list_opt proc_directive_list_opt 
        """

method_head_exported = \
        """
        method_head_exported : method_head_procedure
                             | method_head_function
        """

method_head_function = \
        """
        method_head_function : FUNCTION qualified_identifier method_param_list_opt COLON type_simple proc_directive_list_opt
        """

method_head_procedure = \
        """
        method_head_procedure : PROCEDURE qualified_identifier method_param_list_opt proc_directive_list_opt
        """

method_param = \
        """
        method_param : VAR method_param_decl
                     | CONST method_param_decl
                     | OUT method_param_decl
                     | method_param_decl
                     | VAR identlist
                     | CONST identlist
                     | OUT identlist
        """

method_param_decl = \
        """
        method_param_decl : identlist COLON ARRAY OF type_simple
                          | identlist COLON ARRAY OF FILE           
                          | identlist COLON type_simple
                          | identlist COLON FILE
                          | identlist COLON type_simple EQ const_expr_generic  
        """

method_param_list = \
        """
        method_param_list : method_param
                          | method_param_list SEMICOLON method_param
        """

method_param_list_opt = \
        """
        method_param_list_opt : empty
                              | LPAREN RPAREN
                              | LPAREN method_param_list RPAREN
        """

method_proc_block_section = \
        """
        method_proc_block_section : block
        """

methodlist = \
        """
        methodlist : method
                   | methodlist method
        """

op_add = \
        """
        op_add : PLUS
               | MINUS
               | OR
               | XOR
        """

op_mul = \
        """
        op_mul : TIMES
               | DIVIDE
               | DIV
               | MOD
               | AND
               | SHL
               | SHR
        """

op_rel = \
        """
        op_rel  : EQ
                | GT
                | LT
                | LE
                | GE
                | NE
                | IN
                | IS
                | AS
        """

packed = \
        """
        packed : PACKED
        """

portabilitydirective = \
        """
        portabilitydirective : PLATFORM
                             | PLATFORM EQ const_expr_ordinal
                             | DEPRECATED
                             | LIBRARY
        """

portabilitydirectivelist = \
        """
        portabilitydirectivelist : portabilitydirective
                                 | portabilitydirectivelist portabilitydirective
        """

portabilitydirectivelist2 = \
        """
        portabilitydirectivelist2 : portabilitydirective 
                                  | portabilitydirectivelist2 SEMICOLON portabilitydirective
        """

proc_directive_list = \
        """
        proc_directive_list : SEMICOLON directive 
                            | proc_directive_list SEMICOLON directive 
        """

proceduredeclsection = \
        """
        proceduredeclsection : method_decl_procedure
                             | method_decl_function
                             | CLASS method_decl_function
                             | CLASS method_decl_procedure
                             | method_decl_destructor
                             | method_decl_constructor
                             | method_head_exported FORWARD SEMICOLON
                             | method_head_exported LBRACKET external_statement RBRACKET SEMICOLON
        """

prop_interface = \
        """
        prop_interface : prop_param_list COLON type_simple
                       | COLON type_simple
        """

prop_param_identlisttypeid = \
        """
        prop_param_identlisttypeid : identlist COLON type_simple
        """

prop_param_identlisttypeidlist = \
        """
        prop_param_identlisttypeidlist : empty
                                       | prop_param_identlisttypeid
                                       | CONST prop_param_identlisttypeid
                                       | prop_param_identlisttypeidlist SEMICOLON prop_param_identlisttypeid
                                       | prop_param_identlisttypeidlist SEMICOLON CONST prop_param_identlisttypeid
        """

prop_param_list = \
        """
        prop_param_list : LBRACKET prop_param_identlisttypeidlist RBRACKET
        """

prop_spec_default_opt = \
        """
        prop_spec_default_opt : empty
                              | DEFAULT const_expr_typed_elem
                              | NODEFAULT            
        """

prop_spec_implements_opt = \
        """
        prop_spec_implements_opt : empty
                                 | IMPLEMENTS ident 
                                 | DISPID const_ordinal_number
        """

prop_spec_index_opt = \
        """
        prop_spec_index_opt : empty
                            | INDEX const_expr_ordinal
        """

prop_spec_read_opt = \
        """
        prop_spec_read_opt : empty
                           | READ ident           
        """

prop_spec_stored_opt = \
        """
        prop_spec_stored_opt : empty
                             | STORED ident
        """

prop_spec_writ_opt = \
        """
        prop_spec_write_opt : empty
                            | WRITE ident
        """

prop_specifiers = \
        """
        prop_specifiers : prop_spec_index_opt prop_spec_read_opt prop_spec_write_opt prop_spec_stored_opt prop_spec_default_opt prop_spec_implements_opt
        """

qualified_identifier = \
        """
        qualified_identifier : ident
                             | qualified_identifier PERIOD ident
        """

resourcestring_decl = \
        """
        resourcestring_decl : ident EQ const_string_expr SEMICOLON
        """

resourcestring_decllist = \
        """
        resourcestring_decllist : resourcestring_decl
                                | resourcestring_decllist resourcestring_decl
        """

section_implementation = \
        """
        section_implementation : IMPLEMENTATION section_uses_opt declsectionlist_opt exports_section_list_opt
                               | IMPLEMENTATION section_uses_opt section_resourcestring declsectionlist_opt exports_section_list_opt
        """

section_interface = \
        """
        section_interface : INTERFACE section_uses_opt section_interface_decl_list_opt
        """

section_interface_decl = \
        """
        section_interface_decl : const_section
                               | type_section
                               | var_section
                               | method_head_exported
                               | section_resourcestring
        """

section_interface_decl_list = \
        """
        section_interface_decl_list : section_interface_decl
                                    | section_interface_decl_list section_interface_decl
        """

section_programblock = \
        """
        section_programblock : section_uses_opt block
        """

section_resourcestring = \
        """
        section_resourcestring : RESOURCESTRING resourcestring_decllist                     
        """

section_uses = \
        """
        section_uses : USES identlist SEMICOLON
        """

#  
# 
#

semicolon_opt = \
        """
        semicolon_opt : empty
                      | SEMICOLON
        """

sign = \
        """
        sign : PLUS 
             | MINUS
        """

# -------------
# Source / Goal
# -------------

source_file = \
        """
        source_file : file_program
                    | file_package
                    | file_library
                    | file_unit
        """

source_file_error = \
        """
        source_file : error
        """

# ----------
# Statements
# ----------

statement = \
        """
        statement : stmt_unlabelled
                  | label_ident COLON stmt_unlabelled
        """

statementlist = \
        """
        statementlist : statement_opt
                      | statementlist SEMICOLON statement_opt
        """

stmt_assembler = \
        """
        stmt_assembler : ASM stmt_assemblylanguage END
        """

stmt_assemblylanguage = \
        """
        stmt_assemblylanguage : empty
        """

stmt_assignment = \
        """
        stmt_assignment : variable ASSIGNMENT expression
        """

stmt_break = \
        """
        stmt_break : BREAK
        """

stmt_case = \
        """
        stmt_case : CASE expression OF stmt_case_selector_list stmt_case_else END
        """

stmt_case_else = \
        """
        stmt_case_else : empty
                       | ELSE statementlist
        """

stmt_case_label = \
        """
        stmt_case_label : const_expr_ordinal 
                        | const_expr_ordinal DOTDOT const_expr_ordinal
                        | const_general_string
                        | const_general_string DOTDOT const_general_string 
                        
        """

stmt_case_labellist = \
        """
        stmt_case_labellist : stmt_case_label
                            | stmt_case_labellist COMMA stmt_case_label
        """

stmt_case_selector = \
        """
        stmt_case_selector : stmt_case_labellist COLON stmt_case_stmt
        """

stmt_case_selector_list = \
        """
        stmt_case_selector_list : stmt_case_selector
                                | stmt_case_selector_list stmt_case_selector
        """

stmt_case_stmt = \
        """
        stmt_case_stmt : statement_opt SEMICOLON
        """

stmt_compound = \
        """
        stmt_compound : BEGIN statementlist END
        """

stmt_compound_error = \
        """
        stmt_compound : BEGIN error END
        """

stmt_conditional = \
        """
        stmt_conditional : stmt_if
                         | stmt_case
        """

stmt_continue = \
        """
        stmt_continue : CONTINUE
        """

stmt_exit = \
        """
        stmt_exit : EXIT
                  | EXIT LPAREN variable RPAREN
        """

stmt_for = \
        """
        stmt_for : FOR qualified_identifier ASSIGNMENT expression stmt_for_todowntochoice expression DO statement_opt
        """

stmt_for_todowntochoice = \
        """
        stmt_for_todowntochoice : TO
                                | DOWNTO
        """

stmt_goto = \
        """
        stmt_goto : GOTO LABEL_ID
        """

stmt_if = \
        """
        stmt_if : IF expression THEN statement_opt ELSE statement_opt
                | IF expression THEN statement_opt
        """

stmt_loop = \
        """
        stmt_loop : stmt_repeat
                  | stmt_while
                  | stmt_for
        """

stmt_proc_call = \
        """
        stmt_proc_call : variable
        """

stmt_proc_call_general = \
        """
        stmt_proc_call_general : INHERITED stmt_proc_call
                               | INHERITED inheritedpropertyassignment
                               | INHERITED
                               | stmt_proc_call
                               | stmt_writeln
        """

stmt_raise = \
        """
        stmt_raise : RAISE var_object
                   | RAISE var_object AT var_address
                   | RAISE AT var_address
                   | RAISE
        """

stmt_repeat = \
        """
        stmt_repeat : REPEAT statementlist UNTIL expression
        """

stmt_tryexcept = \
        """
        stmt_tryexcept : TRY statementlist EXCEPT stmt_tryexcept_block END
        """

stmt_tryexcept_block = \
        """
        stmt_tryexcept_block : stmt_tryexcept_onlist stmt_tryexcept_else
                             | stmt_tryexcept_onlist               
                             | statementlist
        """

stmt_tryexcept_else = \
        """
        stmt_tryexcept_else : ELSE statementlist
        """

stmt_tryexcept_ondef = \
        """
        stmt_tryexcept_ondef : ON ident COLON type_simple DO statement_opt SEMICOLON
                             | ON type_simple DO statement_opt SEMICOLON
        """

stmt_tryexcept_onlist = \
        """
        stmt_tryexcept_onlist : stmt_tryexcept_ondef
                              | stmt_tryexcept_onlist stmt_tryexcept_ondef
        """

stmt_tryfinally = \
        """
        stmt_tryfinally : TRY statementlist FINALLY statementlist END
        """

stmt_unlabelled = \
        """
        stmt_unlabelled : stmt_proc_call_general
                        | stmt_assignment
                        | stmt_goto
                        | stmt_compound
                        | stmt_conditional
                        | stmt_loop
                        | stmt_break
                        | stmt_continue
                        | stmt_exit
                        | stmt_with
                        | stmt_tryexcept
                        | stmt_tryfinally
                        | stmt_raise
                        | stmt_assembler
        """

stmt_while = \
        """
        stmt_while : WHILE expression DO statement_opt
        """

stmt_with = \
        """
        stmt_with : WITH variable_list DO statement_opt
                  | WITH variable AS type_simple DO statement_opt
        """

stmt_writeln = \
        """
        stmt_writeln : WRITELN
                     | WRITELN LPAREN stmt_writeln_elem_list RPAREN
        """

stmt_writeln_elem = \
        """
        stmt_writeln_elem : expression 
                           | expression COLON const_unsigned_number 
        """

stmt_writeln_elem_list = \
        """
        stmt_writeln_elem_list : stmt_writeln_elem 
                               | stmt_writeln_elem_list COMMA stmt_writeln_elem
        """

# -----
# Types
# -----

type = \
        """
        type : type_simple SEMICOLON
             | type_struc SEMICOLON
             | type_procedure SEMICOLON
             
             | typeid_variant SEMICOLON
             | type_classref SEMICOLON
        """

type_alias = \
      """
      type_alias : UNIT_ID PERIOD ident
                 | ident PERIOD ident
      """

type_array = \
        """
        type_array : ARRAY LBRACKET type_array_dimensions_list RBRACKET OF type SEMICOLON
                   | ARRAY OF type SEMICOLON
        """

type_array_dimensions = \
        """
        type_array_dimensions : const_expr_generic
                              | const_expr_generic DOTDOT const_expr_generic
                              | typeid_ordinal
        """

type_array_dimensions_list = \
        """
        type_array_dimensions_list : type_array_dimensions
                                   | type_array_dimensions_list COMMA type_array_dimensions
        """

type_basic = \
        """
        type_basic : type_ordinal
                   | typeid_real
                   | type_string
        """

type_class = \
        """
        type_class : type_class_header type_class_structure_decl END
                   | type_class_header END
                   | type_class_header
        """

type_class_component = \
        """
        type_class_component : type_class_fielddecl 
                             | type_class_methoddecl
                             | type_class_propertydecl 
                             | type_class_propertydecl DEFAULT SEMICOLON
        """

type_class_component_struct = \
        """
        type_class_component_struct : type_class_component
                                    | type_class_visibility_decl
        """

type_class_fielddecl = \
        """
        type_class_fielddecl : identlist COLON type SEMICOLON               
        """

type_class_header = \
        """
        type_class_header : CLASS type_class_inheritance
                          | CLASS
        """

type_class_inheritance = \
        """
        type_class_inheritance : LPAREN identlist RPAREN 
        """

type_class_methoddecl = \
        """
        type_class_methoddecl : method
        """

type_class_property = \
        """
        type_class_property : PROPERTY ident prop_interface_opt prop_specifiers SEMICOLON
        """

type_class_propertydecl = \
        """
        type_class_propertydecl : type_class_property
        """

type_class_structure_decl = \
        """
        type_class_structure_decl : type_class_component_struct 
                                  | type_class_component_struct type_class_structure_decl
        """

type_class_visibility_decl = \
        """
        type_class_visibility_decl : PUBLISHED 
                                   | PUBLIC 
                                   | PROTECTED 
                                   | PRIVATE 
                                   | AUTOMATED
        """

type_classref = \
        """
        type_classref : CLASS OF type_ident
        """

type_decl = \
        """
        type_decl : ident EQ type_keyword_opt type
                  | ident EQ type_keyword_opt type_restricted SEMICOLON
                  | TYPE_ID EQ type_keyword_opt type_class SEMICOLON
                  | ident EQ type_alias SEMICOLON
                  | TYPE_ID EQ type_alias SEMICOLON
                  | ident EQ ident SEMICOLON
        """

type_decl_list = \
        """
        type_decl_list : type_decl
                       | type_decl_list type_decl
        """

type_enumerated = \
        """
        type_enumerated : LPAREN type_enumerated_element_list RPAREN
        """

type_enumerated_element = \
        """
        type_enumerated_element : ident
                                | ident EQ const_unsigned_integer                          
        """

type_enumerated_element_list = \
        """
        type_enumerated_element_list : type_enumerated_element
                                     | type_enumerated_element_list COMMA type_enumerated_element                         
        """

type_file = \
        """
        type_file : FILE OF type SEMICOLON
        """

type_function_head = \
        """
        type_function_head : FUNCTION method_param_list_opt COLON type_simple 
        """

type_ident = \
        """
        type_ident : ID
                   | TYPE_ID
        """

type_interface = \
        """
        type_interface : INTERFACE type_interface_heritage_opt type_interface_com_guid_opt methodlist_opt type_interface_classpropertylist_opt END
        """

type_interface_classpropertylist = \
        """
        type_interface_classpropertylist : type_class_property
                                         | type_interface_classpropertylist type_class_property
        """

type_interface_com_guid = \
      """
      type_interface_com_guid : LBRACKET STRING_LITERAL RBRACKET
      """

type_interface_heritage = \
        """
        type_interface_heritage : LPAREN identlist RPAREN
                                | LPAREN type_list RPAREN
        """

type_keyword = \
        """
        type_keyword : TYPE
        """

type_list = \
        """
        type_list : TYPE_ID
                  | type_list TYPE_ID
        """

type_object = \
        """
        type_object : OBJECT type_object_heritage_opt type_object_fieldlist_opt methodlist_opt END
        """

type_object_fieldlist = \
        """
        type_object_fieldlist : type_object_identlisttypelist
                              | type_object_fieldlist type_object_identlisttypelist
        """

type_object_heritage = \
        """
        type_object_heritage : LPAREN ident RPAREN
        """

type_object_identlisttypelist = \
        """
        type_object_identlisttypelist : identlist COLON type SEMICOLON
        """

type_ordinal = \
        """
        type_ordinal : type_enumerated            
                     | type_subrange
                     | typeid_ordinal
        """

type_pointer = \
        """
        type_pointer : CIRCUMFLEX type_basic
                     | CIRCUMFLEX type_ident
                     | POINTER 
        """

type_procedure = \
        """
        type_procedure : type_procedure_head type_procedure_ofobject_opt proc_directive_list_opt
                       | type_function_head type_procedure_ofobject_opt proc_directive_list_opt
        """

type_procedure_head = \
        """
        type_procedure_head : PROCEDURE method_param_list_opt
        """

type_procedure_ofobject = \
        """
        type_procedure_ofobject : OF OBJECT
        """

type_rec = \
        """
        type_rec : RECORD type_rec_field_list_opt END
        """

type_rec_field_decl = \
        """
        type_rec_field_decl : identlist COLON type SEMICOLON portabilitydirectivelist_opt
        """

type_rec_field_decl_list = \
        """
        type_rec_field_decl_list : type_rec_field_decl
                                 | type_rec_variant_section
                                 | type_rec_field_decl_list SEMICOLON type_rec_variant_section
                                 | type_rec_field_decl_list SEMICOLON type_rec_field_decl
        """

type_rec_field_list = \
        """
        type_rec_field_list : type_rec_field_decl_list semicolon_opt
        """

type_rec_variant = \
        """
        type_rec_variant : const_expr_ordinal_list COLON LPAREN type_rec_field_list_opt RPAREN
        """

type_rec_variant_list = \
        """
        type_rec_variant_list : type_rec_variant
                              | type_rec_variant_list SEMICOLON type_rec_variant
        """

type_rec_variant_section = \
        """
        type_rec_variant_section : CASE ident COLON type_ordinal OF type_rec_variant_list SEMICOLON      
                                 | CASE typeid_ordinal OF type_rec_variant_list SEMICOLON
                                 | CASE ident OF type_rec_variant_list SEMICOLON
        """

type_restricted = \
        """
        type_restricted : type_object
                        | type_class
                        | type_interface
        """

type_section = \
        """
        type_section : TYPE type_decl_list
        """

type_set = \
        """
        type_set : SET OF type_ordinal
                 | SET OF type_ident
        """

type_simple = \
        """
        type_simple : type_basic
                    | type_pointer
                    | type_ident           
        """

type_string = \
        """
        type_string : STRING
                    | ANSISTRING
                    | WIDESTRING
        """

type_struc = \
        """
        type_struc : packed_opt type_array
                   | packed_opt type_set
                   | packed_opt type_file
                   | packed_opt type_rec
        """

type_subrange = \
        """
        type_subrange : const_expr_ordinal DOTDOT const_expr_ordinal
        """

typeid_castid = \
        """
        typeid_castid : ID
                      | TYPE_ID
                      | typeid_ordinal
                      | typeid_real
        """

typeid_ordinal = \
        """
        typeid_ordinal : SHORTINT
                       | SMALLINT
                       | INTEGER
                       | BYTE
                       | LONGINT
                       | INT64
                       | DWORD
                       | WORD
                       | BOOLEAN
                       | CHAR
                       | WIDECHAR
                       | LONGWORD
                       | PCHAR
        """

typeid_real = \
        """
        typeid_real : REAL48
                    | REAL
                    | SINGLE
                    | DOUBLE
                    | EXTENDED
                    | CURRENCY
                    | COMP
        """

typeid_variant = \
        """
        typeid_variant : VARIANT
                       | OLEVARIANT
        """

# ----
# Unit
# ----

unit_ident = \
        """
        unit_ident : UNIT_ID
        """

# ---------
# Variables
# ---------

var_address = \
        """
        var_address : const_expr_ordinal
        """

var_decl = \
        """
        var_decl : identlist COLON type SEMICOLON
                 | identlist COLON type var_decl_opt SEMICOLON
        """

var_decl_list = \
        """
        var_decl_list : var_decl
                      | var_decl_list var_decl
        """

var_decl_opt = \
        """
        var_decl_opt : portabilitydirectivelist
                     | ABSOLUTE variable  
                     | EQ const_expr_typed_elem              
        """

var_ident = \
        """
        var_ident : VAR_ID
        """

var_object = \
        """
        var_object : ID
                   | var_object PERIOD ID exprlist2_opt
        """

var_section = \
        """
        var_section : VAR var_decl_list
        """

var_set_constructor = \
        """
        var_set_constructor : LBRACKET RBRACKET
                            | LBRACKET var_set_elementlist RBRACKET
        """

var_set_element = \
        """
        var_set_element : expression
                        | expression DOTDOT expression
        """

var_set_elementlist = \
        """
        var_set_elementlist : var_set_element
                            | var_set_elementlist COMMA var_set_element
        """

variable = \
        """
        variable : variable LBRACKET exprlist RBRACKET    
                 | variable CIRCUMFLEX
                 | variable PERIOD variable
                 | ident
                 | STRING
                 | variable LPAREN exprlist RPAREN
        """

variable_cast = \
        """
        variable_cast : LOW LPAREN type_ordinal RPAREN
                      | HIGH LPAREN type_ordinal RPAREN
                      | LOW LPAREN ident RPAREN
                      | HIGH LPAREN ident RPAREN
                      | typeid_ordinal LPAREN expression RPAREN
                      | POINTER LPAREN expression RPAREN
                      | SIZEOF LPAREN expression RPAREN
        """

variable_list = \
   """
   variable_list : variable
                 | variable_list COMMA variable
   """





# This decorator function can be used to set the grammer rule expression ion on a function
# when its docstring might need to be set in an alternative way
# -----------------------------------------------------------------------------

def GRAMMER(r):
    def set_doc(f):
        if hasattr(r,"__call__"):
            f.__doc__ = r.__doc__
        else:
            f.__doc__ = r
        return f
    return set_doc

# Alternative spelling of the GRAMMER decorator
Grammer = GRAMMER

