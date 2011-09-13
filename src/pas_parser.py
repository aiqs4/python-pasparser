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
# based on pycparser
#
# PasParser class: Parser and AST builder for the Object-Pascal language
#
#-----------------------------------------------------------------
import re

import ply.yacc

#from . import pas_ast
#from .pas_lexer import PasLexer
#from .plyparser import PLYParser, Coord, ParseError

import pas_ast
from pas_lexer import PasLexer
from plyparser import PLYParser, Coord, ParseError

import pas_grammer as g
from pas_grammer import GRAMMER

import pas_parser_code as code
import pas_parser_consts as const
import pas_parser_types as typ

class PasParser(PLYParser):
    def __init__(
            self, 
            lex_optimize=True,
            lextab='pypasparser.lextab',
            yacc_optimize=True,
            yacctab='pypasparser.yacctab',
            yacc_debug=False,
            parse_uses_func=None
            ):
        """ Create a new PasParser.
        
            Some arguments for controlling the debug/optimization
            level of the parser are provided. The defaults are 
            tuned for release/performance mode. 
            The simple rules for using them are:
            *) When tweaking PasParser/PasLexer, set these to False
            *) When releasing a stable parser, set to True
            
            lex_optimize:
                Set to False when you're modifying the lexer.
                Otherwise, changes in the lexer won't be used, if
                some lextab.py file exists.
                When releasing with a stable lexer, set to True
                to save the re-generation of the lexer table on 
                each run.
            
            lextab:
                Points to the lex table that's used for optimized
                mode. Only if you're modifying the lexer and want
                some tests to avoid re-generating the table, make 
                this point to a local lex table file (that's been
                earlier generated with lex_optimize=True)
            
            yacc_optimize:
                Set to False when you're modifying the parser.
                Otherwise, changes in the parser won't be used, if
                some parsetab.py file exists.
                When releasing with a stable parser, set to True
                to save the re-generation of the parser table on 
                each run.
            
            yacctab:
                Points to the yacc table that's used for optimized
                mode. Only if you're modifying the parser, make 
                this point to a local yacc table file
                        
            yacc_debug:
                Generate a parser.out file that explains how yacc
                built the parsing table from the grammar.
        """
        self.clex = PasLexer(
            error_func=self._lex_error_func,
            type_lookup_func=self._lex_type_lookup_func,
            class_lookup_func=self._lex_class_lookup_func,
            const_lookup_func=self._lex_const_lookup_func,
            label_lookup_func=self._lex_label_lookup_func,
            unit_lookup_func=self._lex_unit_lookup_func,
            )
            
        self.clex.build(
            optimize=lex_optimize,
            lextab=lextab)
        self.tokens = self.clex.tokens
        
        #TODO: #TODO: #TODO:
        rules_with_opt = [
            #'exprlist',
            'type_interface_com_guid',
            'exprlist2',
            'type_interface_classpropertylist',
            'section_uses',
            'portabilitydirectivelist2',
            'portabilitydirectivelist',
            'proc_directive_list',
            
            'type_interface_heritage',
            'prop_interface',
            'type_rec_field_list',
            'file_package_requires', 
            'file_package_contains',
            'section_interface_decl_list',
            'declsectionlist',
            'exports_section_list',
            'const_expr_set_elem_list',
            
            'statement',
            #'statementlist',

            'type_object_heritage',
            'type_object_fieldlist',
            'methodlist',
            'packed', 
            #'rec_variantsection',
            #'constexprlist',
            'type_keyword',
            'type_procedure_ofobject',
        ]
        
        for rule in rules_with_opt:
            self._create_opt_rule(rule)
        
        self.cparser = ply.yacc.yacc(
            module=self, 
            start='source_file',
            debug=yacc_debug,
            optimize=yacc_optimize,
            tabmodule=yacctab)
        
        # A table of identifiers defined as typedef types during
        # parsing.
        #
        self.typedef_table = set([])
        self.classdef_table = set([])
        self.constdef_table = set([])
        self.labeldef_table = set([])
        self.unitdef_table = set([])
        
        
        self.__parse_uses_func = parse_uses_func
    
    def parse(self, text, filename='', debuglevel=0):
        """ Parses Pascal code and returns an AST.
        
            text:
                A string containing the Pascal source code
            
            filename:
                Name of the file being parsed (for meaningful
                error messages)
            
            debuglevel:
                Debug level to yacc
        """
        self.clex.filename = filename
        self.clex.reset_lineno()
        self.typedef_table = set([])
        return self.cparser.parse(text, lexer=self.clex, debug=debuglevel)
    #print t.__doc__
    
    ######################--   PRIVATE   --######################
    
    def _lex_error_func(self, msg, line, column):
        self._parse_error(msg, self._coord(line, column))
    
    def _lex_type_lookup_func(self, name):
        """ Looks up types that were previously defined. 
            Passed to the lexer for recognizing identifiers that
            are types.
        """
        return name in self.typedef_table
    
    def _lex_class_lookup_func(self, name):
        """ Looks up classes that were previously defined. 
            Passed to the lexer for recognizing identifiers that
            are classes.
        """
        return name in self.classdef_table
    
    def _lex_const_lookup_func(self, name):
        """ Looks up consts that were previously defined. 
            Passed to the lexer for recognizing identifiers that
            are consts.
        """
        return name in self.constdef_table
        
    def _lex_label_lookup_func(self, name):
        """ Looks up labels that were previously defined. 
            Passed to the lexer for recognizing identifiers that
            are labels.
        """
        return name in self.labeldef_table
        
    def _lex_unit_lookup_func(self, name):
        """ Looks up units that were previously defined. 
            Passed to the lexer for recognizing identifiers that
            are units.
        """
        return name in self.unitdef_table
    
    def _add_typedef_type(self, name):
        """ Adds names that were defined as new types.
        """
        self.typedef_table.add(name)
    
    def _add_constdef_type(self, name):
        """ Adds names that were defined as new consts.
        """
        self.constdef_table.add(name)
        
    def _add_labeldef_type(self, name):
        """ Adds names that were defined as new labels.
        """
        self.labeldef_table.add(name)
        
    def _add_unitdef_type(self, name):
        """ Adds names that were defined as new units.
        """
        self.unitdef_table.add(name)

    def _type_modify_decl(self, decl, modifier):
        """ Tacks a type modifier on a declarator, and returns
            the modified declarator.
            
            Note: the declarator and modifier may be modified
        """
        #~ print '****'
        #~ decl.show(offset=3)
        #~ modifier.show(offset=3)
        #~ print '****'
        
        modifier_head = modifier
        modifier_tail = modifier
        
        # The modifier may be a nested list. Reach its tail.
        #
        while modifier_tail.type: 
            modifier_tail = modifier_tail.type
        
        # If the decl is a basic type, just tack the modifier onto
        # it
        #
        if isinstance(decl, pas_ast.TypeDecl):
            modifier_tail.type = decl
            return modifier
        else:
            # Otherwise, the decl is a list of modifiers. Reach
            # its tail and splice the modifier onto the tail,
            # pointing to the underlying basic type.
            #
            decl_tail = decl
            
            while not isinstance(decl_tail.type, pas_ast.TypeDecl):
                decl_tail = decl_tail.type
            
            modifier_tail.type = decl_tail.type
            decl_tail.type = modifier_head
            return decl

    # Due to the order in which declarators are constructed,
    # they have to be fixed in order to look like a normal AST.
    # 
    # When a declaration arrives from syntax construction, it has
    # these problems:
    # * The innermost TypeDecl has no type (because the basic
    #   type is only known at the uppermost declaration level)
    # * The declaration has no variable name, since that is saved
    #   in the innermost TypeDecl
    # * The typename of the declaration is a list of type 
    #   specifiers, and not a node. Here, basic identifier types
    #   should be separated from more complex types like enums
    #   and structs.
    #
    # This method fixes these problem.
    #
    def _fix_decl_name_type(self, decl, typename):
        """ Fixes a declaration. Modifies decl.
        """
        # Reach the underlying basic type
        #
        type = decl
        while not isinstance(type, pas_ast.TypeDecl):
            type = type.type
        
        decl.name = type.declname
        type.quals = decl.quals
        
        # The typename is a list of types. If any type in this 
        # list isn't a simple string type, it must be the only
        # type in the list (it's illegal to declare "int enum .."
        # If all the types are basic, they're collected in the
        # IdentifierType holder.
        #
        for tn in typename:
            if not isinstance(tn, str):
                if len(typename) > 1:
                    self._parse_error(
                        "Invalid multiple types specified", tn.coord)
                else:
                    type.type = tn
                    return decl
        
        type.type = pas_ast.IdentifierType(typename)
        return decl
    
    def _add_declaration_specifier(self, declspec, newspec, kind):
        """ Declaration specifiers are represented by a dictionary
            with the entries:
            * qual: a list of type qualifiers
            * storage: a list of storage type qualifiers
            * type: a list of type specifiers
            * function: a list of function specifiers
            
            This method is given a declaration specifier, and a 
            new specifier of a given kind.
            Returns the declaration specifier, with the new 
            specifier incorporated.
        """
        spec = declspec or dict(qual=[], storage=[], type=[], function=[])
        spec[kind].append(newspec)
        return spec
    
    def _build_function_definition(self, decl, spec, param_decls, body):
        """ Builds a function definition.
        """
        declaration = pas_ast.Decl(
            name=None,
            quals=spec['qual'],
            storage=spec['storage'],
            funcspec=spec['function'],
            type=decl, 
            init=None, 
            bitsize=None, 
            coord=decl.coord)
        
        typename = spec['type']
        declaration = self._fix_decl_name_type(declaration, typename)
        return pas_ast.FuncDef(
            decl=declaration,
            param_decls=param_decls,
            body=body,
            coord=decl.coord)

    def _select_struct_union_class(self, token): #TODO:
        """ Given a token (either STRUCT or UNION), selects the
            appropriate AST class.
        """
        if token == 'struct':
            return pas_ast.Struct
        else:
            return pas_ast.Union

    ##
    ## Precedence and associativity of operators
    ##
    precedence = ( #TODO:
        ('right', 'SEMICOLON'),
        ('right', 'END', 'RPAREN', 'RBRACKET'), #TODO: , 
        
        ('left', 'ATSIGN', 'CIRCUMFLEX'), #TODO: CIRCUMFLEX
        ('right', 'NOT'),
        ('left', 'PERIOD'), #TODO: dot before AS #TODO: i changed dot to right
        ('left', 'TIMES', 'DIVIDE', 'DIV', 'MOD', 'AND', 'SHL', 'SHR'), 
        ('left', 'PLUS', 'MINUS', 'OR', 'XOR'),
        ('left', 'EQ', 'NE', 'GT', 'GE', 'LT', 'LE', 'IN', 'IS', 'AS'), #TODO: in is nonassoc???
        
        #('right', 'DEFAULT'),        
        #('right', 'ELSE') #TODO: fix a shift/reduce conflict
    )
    

    ##
    ## Grammar productions
    ## Implementation of the BNF defined in various Object Pascal Reference's
    ##
    @GRAMMER(g.source_file)
    def p_source_file(self, p):
        #p[0] = ('translation_unit', p[1])
        p[0] = pas_ast.FileAST(p[1])

    @GRAMMER(g.source_file_error)
    def p_source_file_error(self, p): #TODO: if error raises here, then ast.show() doesn't work!
        #p[0] = ('translation_unit', p[1])
        print "Generic error in file"
        

    @GRAMMER(g.file_program)
    def p_file_program(self, p): #TODO: for what is the identlist_opt #TODO: the second statement here, can't be right
        p[0] = pas_ast.SomeType('file_program')

    # ==================================================================

    @GRAMMER(g.file_unit)
    def p_file_unit(self, p):
        print '================== file_unit ==============='
        #p[0] = ('unit', (p[2], p[3]), p[5], p[6], p[7])
        p[0] = pas_ast.Unit(p[2], p[3], p[5], p[6], p[7])
        #p[0] = pas_ast.SomeType('unit')

    @GRAMMER(g.file_unit_section_init)
    def p_file_unit_section_init(self, p): #TODO: move this to a better place 
        print '================== file_unit_section_init ==============='
        #TODO: add finalization #TODO: added other states too
        p[0] = None if len(p) == 2 else pas_ast.SomeType('file_unit_section_init') #TODO: extend this 

    # ==================================================================
    @GRAMMER(g.file_package)
    def p_file_package(self, p):
        p[0] = pas_ast.SomeType('file_package')

    @GRAMMER(g.file_package_requires)
    def p_file_package_requires(self, p): #TODO: in '<file.pas>' does this missing here, like contains clause???
        p[0] = pas_ast.SomeType('file_package_requires')

    @GRAMMER(g.file_package_contains)
    def p_file_package_contains(self, p): #TODO: in '<file.pas>' is missing here
        p[0] = pas_ast.SomeType('file_package_contains')

    # ==================================================================
    @GRAMMER(g.file_library)
    def p_file_library(self, p):
        p[0] = pas_ast.SomeType('file_library')

    @GRAMMER(g.section_programblock)
    def p_section_programblock(self, p): #TODO: only in program and only in library??? really? 
        p[0] = pas_ast.SomeType('section_programblock')

    @GRAMMER(g.section_uses)
    def p_section_uses(self, p): 
        if self.__parse_uses_func: #TODO: make this private!
          self.__parse_uses_func(p[2])
        p[0] = pas_ast.UsesClause(p[2])
                
    # ==================================================================
    @GRAMMER(g.section_interface)    
    def p_section_interface(self, p):
        #print '============= section_interface ============='
        p[0] = pas_ast.InterfaceSection(p[2], p[3]) #('interfacesection')

    @GRAMMER(g.section_interface_decl_list)
    def p_section_interface_decl_list(self, p):
        p[0] = [p[1]] if len(p) == 2 else p[1] + [p[2]]

    @GRAMMER(g.section_interface_decl)
    def p_section_interface_decl(self, p): #TODO: labels???? #TODO: exportedheading, could be merged, with other functions/procedures
        #print '============= section_interface_decl ============='
        #TODO: method_head_exported, is this not a list???
        p[0] = p[1]

    @GRAMMER(g.section_implementation)
    def p_section_implementation(self, p):
        #TODO: this declsection is bad... so i added section_resourcestring as a HACK... refactor this!!! 
        #print '============= section_implementation ============='
        if len(p) == 5:
            p[0] = pas_ast.ImplementationSection(p[2], p[3], p[4])
        else:
            p[0] = pas_ast.ImplementationSection(p[2], p[4], p[5])

    # ==================================================================

    @GRAMMER(g.section_resourcestring)    
    def p_section_resourcestring(self, p): #TODO: this is here only in interfacesection, i think this can be on other place too
        p[0] = pas_ast.SomeType('section_resourcestring')

    @GRAMMER(g.resourcestring_decllist)
    def p_resourcestring_decllist(self, p):
        p[0] = [p[1]] if len(p) == 2 else p[1] + [p[2]]
        #p[0] = None

    @GRAMMER(g.resourcestring_decl)
    def p_resourcestring_decl(self, p): #TODO: i changed constexpr to string... should also allow PLUS string...
        #TODO: allow char_expressions
        #"""
        #constresourcedecl : ident EQ constexpr SEMICOLON
        #"""
        p[0] = p[1] + '=' + p[2] #TODO: generate a class for this

    # ==================================================================

    @GRAMMER(g.block)
    def p_block(self, p): #TODO: ??? #TODO: declsection is not a list... but opt is correct #TODO: why duplicate of eportsstmtlist???
        print '================== block ==============='
        p[0] = pas_ast.Block(p[1], p[2], p[3], p[4])

    # ==================================================================
    # ----------------- EXPORTS ------------------------------------------------
    @GRAMMER(g.exports_section)
    def p_exports_section(self, p): #TODO: this should also in interface-section's, says the o-pas-ref
        p[0] = pas_ast.SomeType('exports_section')

    @GRAMMER(g.exports_section_list)
    def p_exports_section_list(self, p): #TODO: is this really a complete list allowed??
        p[0] = [p[1]] if len(p) == 2 else p[1] + [p[2]]

    @GRAMMER(g.exports_item_list)
    def p_exports_item_list(self, p):
        p[0] = pas_ast.SomeType('exports_item_list')

    @GRAMMER(g.exports_item)
    def p_exports_item(self, p): #TODO: how should the (e)bnf implemented, from the object-pascal ref
        p[0] = pas_ast.SomeType('exports_item')

    @GRAMMER(g.exports_nameindex_opt) #TODO: why opt???
    def p_exports_nameindex_opt(self, p): #TODO: the types are not fully correct #TODO: i changed constexpr, to ordinal_const_expr
        #TODO: is this really a STRING_LITERAL??? in the name ... what abaut ''
        #TODO: i added resident, check how resident really works. 
        #TODO: can resident be combined with the other options???
        p[0] = pas_ast.SomeType('exports_nameindex_opt')
    # ==================================================================

    # ==================================================================
        #/* Declaration sections: */
    # ==================================================================


    @GRAMMER(g.declsectionlist)    
    def p_declsectionlist(self, p): #TODO: maybe, rewrite as list? only if it is better readable (i dont know what is faster)
        p[0] = [p[1]] if len(p) == 2 else p[1] + [p[2]]

    @GRAMMER(g.declsection)
    def p_declsection(self, p): #TODO: duplicate #TODO: seems only used inside from implementation, so rename this
        p[0] = p[1]
        
     # ==================================================================
    @GRAMMER(g.label_section)
    def p_label_section(self, p): #TODO: is this really a list???
        p[0] = pas_ast.SomeType('label_section')

    @GRAMMER(g.label_ident_list)
    def p_label_ident_list(self, p):
        p[0] = pas_ast.SomeType('label_ident_list')

    @GRAMMER(g.label_ident)
    def p_label_ident(self, p): #TODO: really int???
        #"""
        #labelid    : INT_CONST_DEC /* must be decimal integer in the range 0..9999 */        
        #    | ident
        #"""
        self._add_labeldef_type(p[1])
        p[0] = p[1]

    p_const_section = const.const_section
    p_const_decl_list = const.const_decl_list
    p_const_decl = const.const_decl
    p_const_expr_ordinal_list = const.const_expr_ordinal_list
    p_const_expr_ordinal = const.const_expr_ordinal
    p_const_ordinal_number = const.const_ordinal_number
    p_const_expr_generic = const.const_expr_generic
    p_const_element_generic = const.const_element_generic
    p_const_element_number = const.const_element_number
    p_const_expr_typed_elem = const.const_expr_typed_elem
    p_const_expr_typed_elem_list = const.const_expr_typed_elem_list
    p_const_expr_array = const.const_expr_array
    p_const_expr_record = const.const_expr_record
    p_const_expr_record_field_list = const.const_expr_record_field_list
    p_const_expr_record_field = const.const_expr_record_field
    p_const_expr_set = const.const_expr_set
    p_const_expr_set_elem_list = const.const_expr_set_elem_list
    p_const_expr_set_elem = const.const_expr_set_elem
    
    
    p_type_section = typ.type_section
    p_type_decl_list = typ.type_decl_list
    p_type_alias = typ.type_alias
    p_type_decl = typ.type_decl
    p_type_keyword = typ.type_keyword
    p_type = typ._type
    p_type_simple = typ.type_simple
    p_type_basic = typ.type_basic
    p_type_restricted = typ.type_restricted
    p_type_classref = typ.type_classref
    p_type_ordinal = typ.type_ordinal
    p_type_subrange = typ.type_subrange
    p_typeid_real = typ.typeid_real
    p_typeid_ordinal = typ.typeid_ordinal
    p_typerid_variant = typ.typeid_variant
    p_type_enumerated = typ.type_enumerated
    p_type_enumerated_element_list = typ.type_enumerated_element_list
    p_type_enumerated_element = typ.type_enumerated_element
    p_type_string = typ.type_string
    p_type_struc = typ.type_struc
    p_packed = typ.packed
    p_type_array = typ.type_array
    p_type_array_dimensions_list = typ.type_array_dimensions_list
    p_type_array_dimensions = typ.type_array_dimensions
    p_type_rec = typ.type_rec
    p_type_rec_field_list = typ.type_rec_field_list
    p_type_rec_field_decl_list = typ.type_rec_field_decl_list
    p_type_rec_field_decl = typ.type_rec_field_decl
    p_type_rec_variant_section = typ.type_rec_variant_section
    p_type_rec_variant_list = typ.type_rec_variant_list
    p_type_rec_variant = typ.type_rec_variant
    p_type_set = typ.type_set
    p_type_file = typ.type_file
    p_type_pointer = typ.type_pointer
    p_type_object = typ.type_object
    p_type_object_heritage = typ.type_object_heritage
    p_type_object_fieldlist = typ.type_object_fieldlist
    p_type_object_identlisttypelist = typ.type_object_identlisttypelist
    p_type_procedure = typ.type_procedure
    p_type_procedure_ofobject = typ.type_procedure_ofobject
    p_type_procedure_head = typ.type_procedure_head
    p_type_function_head = typ.type_function_head
    p_type_interface_com_guid = typ.type_interface_com_guid
    p_type_interface = typ.type_interface
    p_type_interface_classpropertylist = typ.type_interface_classpropertylist
    p_type_interface_heritage = typ.type_interface_heritage
    p_type_class = typ.type_class
    p_type_class_header = typ.type_class_header
    p_type_class_inheritance = typ.type_class_inheritance
    p_type_class_structure_decl = typ.type_class_structure_decl
    p_type_class_component_struct = typ.type_class_component_struct
    p_type_class_visibility_decl = typ.type_class_visibility_decl
    p_type_class_component = typ.type_class_component
    p_type_class_fielddecl = typ.type_class_fielddecl
    p_type_class_methoddecl = typ.type_class_methoddecl
    p_type_class_propertydecl = typ.type_class_propertydecl
    p_type_class_property = typ.type_class_property
    
    # ------------------------------------------------------------------
    @GRAMMER(g.semicolon_opt) #TODO: why opt TODO: remove this 
    def p_semicolon_opt(self, p): #TODO: make this opt #TODO: move this to a better place
        p[0] = pas_ast.SomeType('semicolon_opt')

    #def p_default_opt(self, p): #TODO: semicolon here?
    #    """
    #    default_opt : empty 
    #                | DEFAULT SEMICOLON
    #    """
    #    p[0] = pas_ast.SomeType('default_opt')
    # --------------------------------------------------------------------------

    @GRAMMER(g.prop_interface)
    def p_prop_interface(self, p): #TODO: i think type is too much here... i changed to simple type
        #"""
        #propertyinterface : propertyparameterlist COLON type
        #                  | COLON type
        #"""
        p[0] = pas_ast.SomeType('prop_interface')

    @GRAMMER(g.prop_param_list)
    def p_prop_param_list(self, p):
        p[0] = pas_ast.SomeType('prop_param_list')

    @GRAMMER(g.prop_param_identlisttypeidlist)
    def p_prop_param_identlisttypeidlist(self, p):
        p[0] = pas_ast.SomeType('prop_param_identlisttypeidlist')

    @GRAMMER(g.prop_param_identlisttypeid)
    def p_prop_param_identlisttypeid(self, p): #TODO: ordident is sure false here, i switched to simpletype
        p[0] = pas_ast.SomeType('prop_param_identlisttypeid')

    # --------------------- Property Specifier ---------------------------------
    @GRAMMER(g.prop_specifiers)
    def p_prop_specifiers(self, p): #TODO: is indexspec on the correct place? or are the elements here, on the right order?
        p[0] = pas_ast.SomeType('prop_specifiers')

    @GRAMMER(g.prop_spec_index_opt)
    def p_prop_spec_index_opt(self, p):
        #TODO: i changed constexpr to const_expr_ordinal
        p[0] = pas_ast.SomeType('prop_spec_index_opt')

    @GRAMMER(g.prop_spec_read_opt)
    def p_prop_spec_read_opt(self, p):               
        p[0] = pas_ast.SomeType('prop_spec_read_opt')

    @GRAMMER(g.prop_spec_writ_opt)
    def p_prop_spec_writ_opt(self, p):              
        p[0] = pas_ast.SomeType('prop_spec_write_opt')

    @GRAMMER(g.prop_spec_stored_opt)
    def p_prop_spec_stored_opt(self, p):              
        p[0] = pas_ast.SomeType('prop_spec_stored_opt')

    @GRAMMER(g.prop_spec_default_opt)
    def p_prop_spec_default_opt(self, p):
        #TODO: added simple default
        #TODO: i changed constexpr to const_expr_generic #TODO: now i changed this to typedconstant
        #"""
        #defaultspec_opt : empty
        #                | DEFAULT const_expr_generic
        #                | NODEFAULT            
        #"""
        p[0] = pas_ast.SomeType('prop_spec_default_opt')

    #def p_prop_spec_index_default_opt(self, p): #TODO: this is new, but i think this is wrong in interfaces
    #    """
    #    prop_spec_index_default_opt : empty
    #                                | SEMICOLON DEFAULT            
    #    """
    #    p[0] = pas_ast.SomeType('prop_spec_default_opt')

    @GRAMMER(g.prop_spec_implements_opt)
    def p_prop_spec_implements_opt(self, p):
        #TODO: implements for propertys??? or only for interface's? 
        #TODO: i added dispid here, quick & dirty... refactor... and dispid are only allow in automated section's for propertys
        #TODO: only number after DISPID ???? 
        p[0] = pas_ast.SomeType('prop_spec_implements_opt')


    #---------------------------------------------------------------------------
    @GRAMMER(g.var_section)
    def p_var_section(self, p): #TODO: better write a list?!!!
        #"""
        #varsection : VAR vardecl
        #           | varsection vardecl
        #"""
        p[0] = pas_ast.VarSection(p[2])

    @GRAMMER(g.var_decl_list)
    def p_var_decl_list(self, p):
        p[0] = [p[1]] if len(p) == 2 else p[1] + [p[2]]
              
        #/* VarDecl  On Windows -> IdentList ':' Type [(ABSOLUTE (Ident | ConstExpr)) | '=' ConstExpr]     [PortabilityDirective]  */
        #/* On Linux   -> IdentList ':' Type [ABSOLUTE (Ident) | '=' ConstExpr] [PortabilityDirective] */
    @GRAMMER(g.var_decl)
    def p_var_decl(self, p): #TODO: differs #TODO ... #TODO SEMICOLON??? #TODO: changed type to typeid
        #"""
        #vardecl : identlist COLON type vardecl_opt portabilitydirective_opt SEMICOLON
        #"""
        for ident in p[1]:
            p[0] = pas_ast.VarDecl(ident, p[3], None, None)#p[1], p[3], p[4], p[5])

    @GRAMMER(g.var_decl_opt) #TODO: why opt here???
    def p_var_decl_opt(self, p): #TODO: differs #TODO.... #TODO: const, differs? #TODO variable differs #TODO: make this a real opt
        p[0] = pas_ast.SomeType('var_decl_opt') #p[1] if len(p) == 2 else p[2]

    @GRAMMER(g.var_ident)
    def p_var_ident(self, p): #TODO: implemnt this on common places
        p[0] = p[1]

    @GRAMMER(g.type_list)
    def p_type_list(self, p):
        p[0] = [p[1]] if len(p) == 2 else p[1] + [p[2]]

    @GRAMMER(g.identlist)
    def p_identlist(self, p): #TODO: not here in o-pas-ref
        p[0] = [p[1]] if len(p) == 2 else p[1] + [p[3]]

    @GRAMMER(g.identlist_opt)
    def p_identlist_opt(self, p): #TODO: not here in o-pas-ref
        p[0] = pas_ast.SomeType('identlist_opt')

    #def p_identlistlist(self, p): #TODO: not here in o-pas-ref #TODO: this is big bullshit, or for what should this be???
    #    """
    #    identlistlist : identlist
    #                  | identlistlist identlist
    #    """
    #    p[0] = pas_ast.SomeType('identlistlist')

    @GRAMMER(g.variable_list)
    def p_variable_list(self, p): #TODO: only used here. why???
        p[0] = pas_ast.SomeType('variable_list')

    @GRAMMER(g.variable)
    def p_variable(self, p): #TODO: not here in o-pas-ref #TODO: if variable is used inside of proc_call, why string here?
        #TODO: i added | variable COLON const_unsigned_integer, because of writeLN format-strings
        #variable: 
        #    qualified_identifier  
        #    | variable LBRACKET exprlist RBRACKET        /* array component */
        #    | variable LBRACKET exprlist RBRACKET PERIOD variable
        #    | variable CIRCUMFLEX                /* pointer value */
        #    | variable CIRCUMFLEX PERIOD variable
        #    /* procedure or type cast with call behind */
        #    | variable LPAREN optexprlist RPAREN PERIOD variable
        #    | _STRING_ LPAREN optexprlist RPAREN PERIOD variable
        #    | _STRING_ LPAREN optexprlist RPAREN 
        #    /* simple procedure or type cast */
        #    | variable LPAREN optexprlist RPAREN
        #"""
        p[0] = pas_ast.SomeType('variable')

    @GRAMMER(g.variable_cast)
    def p_variable_cast(self, p):
        #TODO: added sizeof variable... is it possible to get sizeof function result???
        #TODO: sizeof type and sizeof variable, are in conflict with each other... this must be refactored
        #TODO: i added variable_cast... i think everything here, needs generic refactoring!!!
        #         | ident LPAREN typeid_ordinal RPAREN        
        #         | HIGH LPAREN type_ordinal RPAREN
        #         | LOW LPAREN type_ordinal RPAREN
        #TODO: i added LOW and HIGH as special kind of function-call
        #TODO: add ident LPAREN exprlist RPAREN, to handle cast and proc call's. but this is not perfect!!!s
        #TODO: i removed the part, which looks like a proc_call, but of course can a variable also the result of a function
        #
        #TODO: addedd sizeof #TODO: sizeof should also on other place too
        #TODO: POINTER is somehow a ordinal type, refactor!!
        #TODO: Low and High here, are a bad workaround!
        #TODO: i temporary added LOW and HIGH... but i'm not sure, in where LOW and HIGH should be defined
        p[0] = pas_ast.SomeType('variable_cast')

    # -------------------- Expression ------------------------------------------
    @GRAMMER(g.exprlist)    
    def p_exprlist(self, p):
        p[0] = pas_ast.SomeType('exprlist')

    @GRAMMER(g.exprlist2)
    def p_exprlist2(self, p): #TODO: not in o-pas-ref
        p[0] = pas_ast.SomeType('exprlist2')

    @GRAMMER(g.expression)
    def p_expression(self, p): #TODO: #TODO: the same bullshit as constexpr!!!!!!!!
        #TODO: hack to support inherited, but this is in many cases wrong!
        p[0] = pas_ast.SomeType('expression')

    @GRAMMER(g.expr_simple)
    def p_expr_simple(self, p): #TODO: i think this is not perfect, see expression #TODO: i added LPAREN/RPAREN
        #"""
        #simpleexpression : term
        #         | sign term
        #         | AT term     /* return address */
        #         | simpleexpression addop term
        #"""        
        p[0] = pas_ast.SomeType('expr_simple')

    # --------------------------------------------------------------------------
    @GRAMMER(g.expr_term)
    def p_expr_term(self, p):
        p[0] = pas_ast.SomeType('expr_term')

               
        #/* Parameterless function calls, and function calls looking like type
        #   casts are caught as variables. */
    @GRAMMER(g.expr_factor)
    def p_expr_factor(self, p): #TODO: TODO: TODO: differs need checks!!!! #TODO: i replaced variable with typeid #TODO: now, i replaced typeid with ident
        #TODO: added | variable_cast CIRCUMFLEX... this is only a quick hack... 
        #TODO: i changed const_unsigned_number to const_expr_generic
        #TODO: add proc_call, could also be a cast! 
        #"""
        #factor : typeid
        #       | typeid PERIOD typeid                     
        #       | unsigned_number
        #       | general_string_const
        #       | NIL   
        #       | LPAREN expression RPAREN 
        #       | LPAREN expression RPAREN CIRCUMFLEX
        #       | NOT factor
        #       | setconstructor     
        #"""
        #"""
        #factor : variable                     
        #       | unsigned_number
        #       | general_string_const
        #       | _NIL_   
        #       | LPAREN expression RPAREN 
        #       | LPAREN expression RPAREN CIRCUMFLEX
        #       /* | qualified_identifier LPAREN optexprlist RPAREN */ #TODO:
        #       | _NOT_ factor
        #       | setconstructor           
        #"""
        p[0] = pas_ast.SomeType('expr_factor')

    #def p_designator(self, p): #TODO TODO TODO missing!!!!

    @GRAMMER(g.var_set_constructor)
    def p_var_set_constructor(self, p):
        p[0] = pas_ast.SomeType('var_set_constructor')

    @GRAMMER(g.var_set_elementlist)
    def p_var_set_elementlist(self, p):
        p[0] = pas_ast.SomeType('var_set_elementlist')

    @GRAMMER(g.var_set_element)
    def p_var_set_element(self, p):
        p[0] = pas_ast.SomeType('var_set_element')

    ## =========================================================================              
    #   Operators
    ## =========================================================================
    @GRAMMER(g.op_rel)
    def p_op_rel(self, p): #TODO: equal is not in o-pas-ref
        p[0] = pas_ast.SomeType('op_rel')

    @GRAMMER(g.op_add)
    def p_op_add(self, p):
        p[0] = pas_ast.SomeType('op_add')

    @GRAMMER(g.op_mul)
    def p_op_mul(self, p):
        p[0] = pas_ast.SomeType('op_mul')

    ## =========================================================================
    ## =========================================================================
    ## =========================================================================

    @GRAMMER(g.method_head_exported)
    def p_method_head_exported(self, p): #TODO: directive or directives??? #TODO: why exported, rename !!!
        #"""
        #exportedheading : procedureheading SEMICOLON directives
        #        | procedureheading SEMICOLON 
        #        | functionheading SEMICOLON directives 
        #        | functionheading SEMICOLON 
        #"""        
        p[0] = pas_ast.SomeType('method_head_exported')

    @GRAMMER(g.method_head_class_exported)
    def p_method_head_class_exported(self, p): #TODO: this is not in the object-pascal ref... or on this place!!
        #TODO: this is base for methodlist!!! and methodlist also used in interface, and objects!!! this must be wrong
        p[0] = pas_ast.SomeType('method_head_class_exported')

    @GRAMMER(g.method_head_function)
    def p_method_head_function(self, p): #TODO: differs #TODO: i moved the SEMICOLON to directives, to avoid conflicts
        #/*                 | _FUNCTION_ qualified_identifier formalparameters_opt COLON _STRING_ */
        p[0] = pas_ast.FunctionHeading(p[2], p[3], p[5])

    @GRAMMER(g.method_head_procedure)
    def p_method_head_procedure(self, p): #TODO: in o-pas-ref this is ident, not qualified... #TODO: i moved the SEMICOLON to directives, to avoid conflicts
        p[0] = pas_ast.ProcedureHeading(p[2], p[3], p[4])

    @GRAMMER(g.method_head_constructor)
    def p_method_head_constructor(self, p): #TODO: i moved the SEMICOLON to directives, to avoid conflicts
        p[0] = pas_ast.SomeType('method_head_constructor')

    @GRAMMER(g.method_head_destructor)
    def p_method_head_destructor(self, p): #TODO: i moved the SEMICOLON to directives, to avoid conflicts
        p[0] = pas_ast.SomeType('method_head_destructor')

    @GRAMMER(g.proceduredeclsection)
    def p_proceduredeclsection(self, p): #TODO: is simpler, in o-pas-ref #TODO: i added method_head_exported, check if this is right for all cases
        #TODO: p[0] is not really correct
        #TODO: method_head shouldn't be here!
        #TODO: added BRACKETs, because, some free-pascal code, seems to have this. I didn't know if this works with delphi
        #TODO: added external_statement... found in free-pascal... not tested in delphi!
        p[0] = p[1] if len(p) == 2 else p[2] if len(p) == 3 else p[1] if len(p) == 6 else p[1] #TODO: could be better written.

    @GRAMMER(g.method_decl_procedure)
    def p_method_decl_procedure(self, p): #TODO: differs
        if len(p) == 5:
            p[0] = pas_ast.ProcedureDecl(p[1], p[2], p[3])
        else:
            p[0] = pas_ast.ProcedureDecl(p[1], p[2], None) #TODO: None is only a dummy

    @GRAMMER(g.method_decl_destructor)
    def p_method_decl_destructor(self, p):
        p[0] = pas_ast.DestructorDecl(p[1], p[2], p[3], None) #TODO: None is only a dummy

    @GRAMMER(g.method_decl_constructor)
    def p_method_decl_constructor(self, p):                   
        p[0] = pas_ast.ConstructorDecl(p[1], p[2], p[3], None) #TODO: None is only a dummy

    @GRAMMER(g.method_decl_function)
    def p_method_decl_function(self, p): #TODO: differs
        if len(p) == 5:
            p[0] = pas_ast.ProcedureDecl(p[1], p[2], p[3])
        else:
            p[0] = pas_ast.ProcedureDecl(p[1], p[2], None) #TODO: None is only a dummy

    @GRAMMER(g.method_proc_block_section)
    def p_method_proc_block_section(self, p): #TODO: removed external #TODO: removed forward #TODO: removed block
        #"""
        #proc_block : block
        #       | EXTERNAL
        #       | FORWARD
        #"""
        p[0] = p[1] #pas_ast.SomeType('proc_block') #TODO:

    # ==========================================================================
    @GRAMMER(g.method_param_list_opt) #TODO: why opt here???
    def p_method_param_list_opt(self, p): #TODO: make the opt with the list above                  
        p[0] = None if len(p) < 4 else p[2]

    @GRAMMER(g.method_param_list)
    def p_method_param_list(self, p):
        p[0] = [p[1]] if len(p) == 2 else p[1] + [p[3]]

    @GRAMMER(g.method_param)
    def p_method_param(self, p): #equal to o-pas-ref
        if len(p) == 2: 
            p[0] = pas_ast.FormalParam(p[1], None) #TODO: find a good way, to group var, const, out 
        else:
            p[0] = pas_ast.SomeType('VAR/CONST/OUT identlist/parameter') # FormalParam(p[2], None) #TODO: None is a dummy here

    @GRAMMER(g.method_param_decl)
    def p_method_param_decl(self, p): #TODO: differs #TODO: empty is wrong here!!!
        #TODO: i replaced constexpr with const_expr_generic
        #"""
        #parameter :  
        #      | identlist COLON _ARRAY_ _OF_ simpletype
        #      #/* | identlist COLON _ARRAY_ _OF_ _STRING_ */
        #      | identlist COLON _ARRAY_ _OF_ _FILE_           
        #      | identlist COLON  simpletype
        #      #/* | identlist COLON  _STRING_ */
        #      | identlist COLON  _FILE_
        #      | identlist COLON simpletype EQ constexpr  
        #      # /* identlist  a revoir */
        #"""
        p[0] = pas_ast.SomeType('method_param_decl')

    # ---------------- OBJECT --------------------------------------------------
    @GRAMMER(g.methodlist)
    def p_methodlist(self, p): #TODO: differs #INFO: also used inside of class
        p[0] = pas_ast.SomeType('methodlist')
                
        #/* method : methodheading SEMICOLON _VIRTUAL_ SEMICOLON
        #    |methodheading  SEMICOLON
        #    ; */

    @GRAMMER(g.method)
    def p_method(self, p):
        p[0] = pas_ast.SomeType('method')

    # --------------------------------------------------------------------------
    # --------------------------------------------------------------------------

    p_statementlist = code.statementlist
    p_statement = code.statement
    p_stmt_unlabelled = code.stmt_unlabelled
    p_stmt_compound = code.stmt_compound
    p_stmt_compound_error = code.stmt_compound_error
    p_stmt_break = code.stmt_break
    p_stmt_continue = code.stmt_continue
    p_stmt_exit = code.stmt_exit
    p_stmt_goto = code.stmt_goto
    p_stmt_proc_call_general = code.stmt_proc_call_general
    p_inheritedpropertyassignment = code.inheritedpropertyassignment
    p_stmt_proc_call = code.stmt_proc_call
    p_stmt_writeln = code.stmt_writeln
    p_stmt_writeln_elem_list = code.stmt_writeln_elem_list
    p_stmt_writeln_elem = code.stmt_writeln_elem
    p_stmt_assignment = code.stmt_assignment
    p_stmt_conditional = code.stmt_conditional
    p_stmt_if = code.stmt_if
    p_stmt_case = code.stmt_case
    p_stmt_case_selector_list = code.stmt_case_selector_list
    p_stmt_case_selector = code.stmt_case_selector
    p_stmt_case_stmt = code.stmt_case_stmt
    p_stmt_case_labellist = code.stmt_case_labellist
    p_stmt_case_label = code.stmt_case_label
    p_stmt_case_else = code.stmt_case_else
    p_stmt_loop = code.stmt_loop
    p_stmt_repeat = code.stmt_repeat
    p_stmt_while = code.stmt_while
    p_stmt_for = code.stmt_for
    p_stmt_for_todowntochoice = code.stmt_for_todowntochoice
    p_stmt_with = code.stmt_with
    p_stmt_tryexcept = code.stmt_tryexcept
    p_stmt_tryexcept_block = code.stmt_tryexcept_block
    p_stmt_tryexcept_else = code.stmt_tryexcept_else
    p_stmt_tryexcept_onlist = code.stmt_tryexcept_onlist
    p_stmt_tryexcept_ondef = code.stmt_tryexcept_ondef
    p_stmt_tryfinally = code.stmt_tryfinally
    p_stmt_raise = code.stmt_raise

    #--------------------------------------------------------------------------- 
    @GRAMMER(g.const_general_string)
    def p_const_general_string(self, p): #TODO: needs general refactoring
        p[0] = p[0] if len(p) == 2 else [p[0]] + [p[2]]

    @GRAMMER(g.const_string_expr)
    def p_const_string_expr(self, p): #TODO: move this const, to the other consts #TODO: use this in other const_exprs
        p[0] = p[0] if len(p) == 2 else [p[0]] + [p[3]]

    @GRAMMER(g.sign)
    def p_sign(self, p):
        p[0] = p[1]

    @GRAMMER(g.const_unsigned_integer)
    def p_const_unsigned_integer(self, p):
        p[0] = pas_ast.SomeType('const_unsigned_integer')

    @GRAMMER(g.const_unsigned_number)
    def p_const_unsigned_number(self, p):
        p[0] = pas_ast.SomeType('const_unsigned_number')

    @GRAMMER(g.ident)
    def p_ident(self, p): #TODO: check if SELF is ok here #TODO: use directive on the correct place #TODO: TYPEID is new
        #| directive_as_name
        #TODO: oh, directive is here, because it could also be used as an name!
        #TODO: for now SELF is here
        #"""
        #ident : ID
        #      | directive
        #"""        
        p[0] = p[1]

    @GRAMMER(g.unit_ident)
    def p_unit_ident(self, p):
        p[0] = p[1]

    @GRAMMER(g.type_ident)
    def p_type_ident(self, p): #TODO: TYPEID & ordident is here new #TODO: what is with other types???    
        #"""
        #typeid : ID
        #       | TYPEID
        #       | typeid_ordinal
        #       | typeid_real
        #"""
        p[0] = pas_ast.SomeType(p[1])

    @GRAMMER(g.typeid_castid)
    def p_typeid_castid(self, p): #TODO: this is just a helper, will be removed soon
        p[0] = pas_ast.SomeType('typeid_castid'.join(p[1]))
        
    @GRAMMER(g.var_object)
    def p_var_object(self, p):               
        p[0] = pas_ast.SomeType('var_object')

    @GRAMMER(g.var_address)
    def p_var_address(self, p):   
        #TODO: i changed constexpr to const_expr_ordinal   
        p[0] = pas_ast.SomeType('var_address')

    @GRAMMER(g.qualified_identifier)
    def p_qualified_identifier(self, p):
        p[0] = p[1] if len(p) == 2 else p[1] + '.' + p[3]

    ## =========================================================================
    #   Modules
    ## =========================================================================

    ## =========================================================================
    #   Declarations
    ## =========================================================================

    ## =========================================================================
    #   Consts
    ## =========================================================================

    ## =========================================================================
    #   Types
    ## =========================================================================

    ## =========================================================================
    #   Classes and Objects
    ## =========================================================================

    ## =========================================================================
    #   Vars
    ## =========================================================================

    ## =========================================================================
    #   Expressions
    ## =========================================================================

    ## =========================================================================
    #   Statements
    ## =========================================================================

    ## =========================================================================
    #   Routines
    ## =========================================================================

    ## =========================================================================
    #   Directives
    ## =========================================================================
    @GRAMMER(g.portabilitydirectivelist)
    def p_portabilitydirectivelist(self, p): #TODO: implement this
        #TODO: are SEMICOLON needed here, too?
        p[0] = pas_ast.SomeType('portabilitydirective')

    @GRAMMER(g.portabilitydirectivelist2)
    def p_portabilitydirectivelist2(self, p): #TODO: implement this
        p[0] = pas_ast.SomeType('portabilitydirective2')

    @GRAMMER(g.portabilitydirective)
    def p_portabilitydirective(self, p): #TODO: implement this
        #TODO: i changed constexpr to const_expr_ordinal
        p[0] = pas_ast.SomeType('portabilitydirective')

    @GRAMMER(g.directive_as_name)
    def p_directive_as_name(self, p): #TODO: check missing!!! #TODO check order #TODO: not using??? #TODO: renamed
        #TODO: implement this as error-handler, because of conflicts. until now, i didn't know a nicer way to do this!
        #TODO: i removed high, low, exit... this could only be parsed last... implement this in a special way!
        #    | ABSTRACT
        #    | ASSEMBLER
        #    | AT
        #    | CDECL
        #    | DEFAULT
        #    | DISPID
        #    | DYNAMIC
        #    | EXPORT
        #    | FAR
        #    | INDEX
        #    | LOCAL
        #    | MESSAGE
        #    | NAME
        #    | NEAR
        #    | NODEFAULT
        #    | OVERLOAD
        #    | OVERRIDE
        #    | PASCAL
        #    | READ
        #    | READONLY
        #    | REGISTER
        #    | REINTRODUCE
        #    | RESIDENT
        #    | SAFECALL
        #    | STDCALL
        #    | STORED
        #    | VARARGS
        #    | VIRTUAL
        #    | WRITE
        #    | WRITEONLY
        #"""
        p[0] = pas_ast.SomeType('')

    @GRAMMER(g.external_statement)
    def p_external_statement(self, p):
      #TODO: added external like exports, but could be merge together! #TODO: check if external allows index option!
      p[0] = pas_ast.SomeType('EXTERNAL') #TODO: implement!

    @GRAMMER(g.directive)
    def p_directive(self, p): #TODO: split this into simple parts
        #| FORWARD
        #TODO: i moved external out here!     
        #TODO: added inline, but unsure, if all methods has this!
        #TODO: removed forward, because FORWARD; are on other place... but now forward is missing on some places!
        #TODO: i added DISPID const_ordinal_number... this is needed for methods, inside automated section
        #TODO: i added empty
        #TODO: i changed constexpr to const_expr_ordinal
        p[0] = pas_ast.SomeType('directive')

    @GRAMMER(g.proc_directive_list)
    def p_proc_directive_list(self, p): #TODO: is it better to move SEMICOLON to directive (i mean remove here, and add empty to directive?) ... and is it possible to add multiple SEMICOLONS here?
        #TODO: not all proc_directive's are allowed anywhere!
        #TODO: i renamed directives to proc_directives
        p[0] = pas_ast.SomeType('proc_directive_list')
            
    ## =========================================================================
    #   Assembler Instruction
    ## =========================================================================
    @GRAMMER(g.stmt_assembler)
    def p_stmt_assembler(self, p): #TODO: move this down to assemblylanguage
        p[0] = pas_ast.SomeType('stmt_assembler')

    @GRAMMER(g.stmt_assemblylanguage)
    def p_stmt_assemblylanguage(self, p): #TODO: ..  
        #"""
        #assemblylanguage : STRING_LITERAL
        #"""        
        p[0] = pas_ast.SomeType('stmt_assemblylanguage')

    ## =========================================================================
    #   Empty Tag
    ## =========================================================================
    @GRAMMER(g.empty)
    def p_empty(self, p):
        p[0] = None

    def p_error(self, p):
        if p:
            self._parse_error(
                'before: %s' % p.value, 
                self._coord(p.lineno))
        else:
            self._parse_error('At end of input', '')




if __name__ == "__main__":
    import pprint
    import time
    from portability import printme
    
    t1 = time.time()
    parser = PasParser(lex_optimize=True, yacc_debug=True, yacc_optimize=False)
    printme(time.time() - t1)
    
    #buf = ''' 
    #    int (*k)(int);
    #'''
    buf = '''
        unit test;

        interface

        uses SysUtils;

        type
            TSomeEnum = (seOne, seTwo);

        implementation

        procedure test(a: integer);
        var
            x: Integer;
            t: Integer;
            z: Integer;
        var
            t: Boolean;
        begin
            x := 0;
        end;

        end.
    '''
    
    import sys
    
    ### TODO: overwrite buf
    if len(sys.argv) > 1:
        filename = sys.argv[1]
    else:
        filename = 'examples/data/zp_simple.pas'
        #filename = '../zp.pas'
        #filename = '../zp2.pas'
        #filename = '../zp2_copy.pas'
    
    buf = open(filename, 'rU').read()
    
    # set debuglevel to 2 for debugging
    t = parser.parse(buf, filename, debuglevel=0)
    print t
    t.show(showcoord=True)
