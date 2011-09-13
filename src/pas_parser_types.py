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










# ==========================================================================
@GRAMMER(g.type_section)
def type_section(self, p): #TODO: proof, is different against the ref
    p[0] = pas_ast.TypeSection(p[2]) #TODO: rewrite

@GRAMMER(g.type_decl_list)
def type_decl_list(self, p):
    p[0] = [p[1]] if len(p) == 2 else p[1] + [p[2]]

@GRAMMER(g.type_alias)
def type_alias(self, p): #TODO: i added type alias... but this could also affect other's ... move this to a more generic place!
  p[0] = p[2] #TODO:

@GRAMMER(g.type_decl)
def type_decl(self, p): #TODO: proof, is different against the ref #TODO: portabilitydirective could a list here
    #TODO: i moved some SEMICOLONs around
    #TODO: i added ident EQ ident #But this is too generic!!!
    #TODO: added type_alias... but i think this should be on a more generic place, because, this affect not only types!!!
    #TODO: added CLASSID, to support forward declartions #TODO: i must changed this to TYPE_ID #TODO: implement this as a well known parse error, or so
    #TODO: i removed portabilitydirective, didn't work in delphi7 (i think)
    #TODO: i added ID #TODO: here should also type.type be possible

    #"""
    #typedecl : ident EQ type2_opt type portabilitydirective_opt SEMICOLON
    #         | ident EQ type2_opt restrictedtype portabilitydirective_opt SEMICOLON
    #"""
    self._add_typedef_type(p[1])
    p[0] = pas_ast.TypeDecl(p[1]) #TODO: not all params are here stored

@GRAMMER(g.type_keyword)
def type_keyword(self, p): 
    #TODO: no result, because nobody care's if this is inside the code!
    p[0] = None #pas_ast.SomeType('type_keyword')
# ==================================================================


#def p_type_opt(self, p): #TODO: whats this????
#    """
#    type_opt : empty
#            | TYPE
#    """
#    p[0] = None









@GRAMMER(g.type)
def _type(self, p): #TODO: missing type's #TODO: i moved (and removed) portabilitydirective's here, ... but only works for record's 
    #TODO: i added typeid #TODO: for now i moved typeid to simpletype
    #TODO: pointertype now inside simpletype, for now - not for ever, i hope
    p[0] = p[1]

@GRAMMER(g.type_simple)
def type_simple(self, p): #TODO: stringtype not here #TODO: pointer is somehow, also a basic type
    p[0] = p[1] #pas_ast.SomeType('simpletype')

@GRAMMER(g.type_basic)
def type_basic(self, p): #TODO: stringtype not here
    p[0] = p[1] #pas_ast.SomeType('simpletype')

@GRAMMER(g.type_restricted)
def type_restricted(self, p): #TODO: i added classreftype 
    p[0] = p[1] #pas_ast.SomeType('type_restricted')

@GRAMMER(g.type_classref)
def type_classref(self, p): #TODO: ordident different, i switched to typeid, should be simpletype or something like that
    p[0] = pas_ast.SomeType('type_classref')

@GRAMMER(g.type_ordinal)        
def type_ordinal(self, p): #TODO: here is something wrong, mix from type-names and type-implementation!?
    p[0] = pas_ast.SomeType('type_ordinal')


@GRAMMER(g.type_subrange)
def type_subrange(self, p): #TODO: there is constexpr in o-pas-ref, instead simpleexpression INFO: i switched it now
    #TODO: i changed constexpr to const_expr_ordinal
    #"""
    #subrangetype : constexpr DOTDOT constexpr
    #"""
    p[0] = pas_ast.SomeType('type_subrange')

@GRAMMER(g.typeid_real)
def typeid_real(self, p):
    #"""
    #realtype : REAL48
    #     | SINGLE
    #     | DOUBLE
    #     | EXTENDED
    #     | CURRENCY
    #     | COMP
    #"""
    p[0] = pas_ast.SomeType('typeid_real')

@GRAMMER(g.typeid_ordinal)
def typeid_ordinal(self, p): #TODO: why here string? why qualified_identifier?
    #"""
    #ordident : SHORTINT
    #     | SMALLINT
    #     | INTEGER
    #     | BYTE
    #     | LONGINT
    #     | INT64
    #     | WORD
    #     | BOOLEAN
    #     | CHAR
    #     | WIDECHAR
    #     | LONGWORD
    #     | PCHAR
    #     | qualified_identifier 
    #     | STRING
    #"""
    p[0] = p[1]

@GRAMMER(g.typeid_variant)
def typeid_variant(self, p):
    p[0] = p[1] #pas_ast.SomeType('typeid_variant')

# --------------------------------------------------------------------------     
@GRAMMER(g.type_enumerated)
def type_enumerated(self, p):
    p[0] = pas_ast.SomeType('type_enumerated')

@GRAMMER(g.type_enumerated_element_list)
def type_enumerated_element_list(self, p):
    p[0] = pas_ast.SomeType('type_enumerated_element_list')

@GRAMMER(g.type_enumerated_element)
def type_enumerated_element(self, p): # TODO: different to object pascal ref, there it is constexpr
    #TODO: unsigned_integer, should be a const_expr_ordinal
    self._add_constdef_type(p[1]) #TODO: mark this enum const, somehow
    p[0] = p[1] if len(p) == 2 else p[1] #TODO:

# --------------------------------------------------------------------------
@GRAMMER(g.type_string)
def type_string(self, p): #TODO: i removed [ ] brackets, string feature
    #           | STRING LBRACKET constexpr RBRACKET 
    #"""
    #"""
    #stringtype : ANSISTRING
    #       | WIDESTRING
    #       | STRING LBRACKET constexpr RBRACKET 
    #"""
    p[0] = pas_ast.SomeType('type_string')

@GRAMMER(g.type_struc)
def type_struc(self, p): #TODO: packed_opt by arraytype #TODO: portabilitydirective could be a list #TODO: removed some portdirective
    #TODO: packed classes???
    #TODO: i removed optional packed at end, seems to not work in delphi 7
    #"""
    #structype : packed_opt arraytype
    #        | packed_opt settype
    #        | packed_opt filetype
    #        | packed_opt rectype packed_opt
    #"""        
    p[0] = pas_ast.SomeType('type_struc')

@GRAMMER(g.packed)
def packed(self, p):
    p[0] = pas_ast.SomeType('packed')

@GRAMMER(g.type_array)
def type_array(self, p): #TODO: i changed type to simpletype (type's need generic refactoring)
    #TODO: i changed constexpr to const_expr_ordinal
    #"""
    #arraytype : ARRAY LBRACKET constexpr RBRACKET OF type 
    #          | ARRAY LBRACKET constexpr DOTDOT constexpr RBRACKET OF type
    #          | ARRAY OF type
    #"""
    p[0] = pas_ast.SomeType('type_array')

@GRAMMER(g.type_array_dimensions_list)
def type_array_dimensions_list(self, p):
    p[0] = pas_ast.SomeType('type_array_dimensions_list')

@GRAMMER(g.type_array_dimensions)
def type_array_dimensions(self, p): #TODO: exists there other possibilities to ordident? #TODO: really complete const_expr_ordinal, or more specific?
    #TODO: i changed const_expr_ordinal to const_expr_generic. but const_expr_set are similar, perhaps it is possible to merge this!
    #TODO: const_expr_ordinal was right, because only ordinal numbers are allowed!!!! change this back, here and in set !!!
    #"""
    #arraytype_dimensions : const_expr_ordinal
    #                     | const_expr_ordinal DOTDOT const_expr_ordinal
    #                     | typeid_ordinal
    #"""
    p[0] = pas_ast.SomeType('type_array_dimensions')



# ------------------------------------------------------------------
@GRAMMER(g.type_rec)
def type_rec(self, p): #TODO: i added optional semicolon
    p[0] = pas_ast.SomeType('type_rec')

@GRAMMER(g.type_rec_field_list)
def type_rec_field_list(self, p): #TODO: is the SEMICOLON on the right place??? #TODO: colon_opt? 
    #"""
    #fieldlist : fielddecllist SEMICOLON variantsection_opt colon_opt
    #"""        
    p[0] = pas_ast.SomeType('type_rec_field_list')

@GRAMMER(g.type_rec_field_decl_list)
def type_rec_field_decl_list(self, p):
    p[0] = pas_ast.SomeType('type_rec_field_decl_list')

@GRAMMER(g.type_rec_field_decl)
def type_rec_field_decl(self, p): 
    p[0] = pas_ast.SomeType('type_rec_field_decl')

# --------------------------------------------------------------------------
@GRAMMER(g.type_rec_variant_section)
def type_rec_variant_section(self, p): #TODO: differs #TODO: i changed simpletype to ordident&ordinaltype #TODO: is the first and last SEMICOLON optional?
    #"""
    #variantsection : _CASE_ ident COLON simpletype _OF_ recvariantlist SEMICOLON             
    #           /* | _CASE_ ident COLON unitid PERIOD simpletype _OF_ recvariantlist SEMICOLON  */
    #           
    #           | _CASE_ simpletype _OF_ recvariantlist SEMICOLON
    #           /* | _CASE_ unitid PERIOD simpletype _OF_ recvariantlist SEMICOLON */
    #"""
    p[0] = pas_ast.SomeType('type_rec_variant_section')

@GRAMMER(g.type_rec_variant_list)
def type_rec_variant_list(self, p):
    p[0] = pas_ast.SomeType('type_rec_variant_list')

@GRAMMER(g.type_rec_variant)
def type_rec_variant(self, p): #TODO: why empty??? better is _opt!!! #TODO: are multiple constexprlist possible, like in case? #TODO: remove empty
    #TODO: i changed constepxrlist to const_expr_ordinal_list 
    #"""
    #recvariant : constexprlist COLON LPAREN rec_fieldlist_opt RPAREN
    #           | empty
    #"""
    p[0] = pas_ast.SomeType('type_rec_variant')

# ==================================================================
# ------------------------------------------------------------------
@GRAMMER(g.type_set)
def type_set(self, p): #TODO: add TYPEID
    p[0] = pas_ast.SomeType('settype')

@GRAMMER(g.type_file)
def type_file(self, p): #TODO: o-pas-ref has here typeid not type
    p[0] = pas_ast.SomeType('type_file')

@GRAMMER(g.type_pointer)
def type_pointer(self, p): #TODO: o-pas-ref has here typeid not type #TODO: and no pointer here #TODO: PointerType should inside of SimpleType!!!
    #TODO: changed to basictype.. #TODO: added typeid
    #TODO: i remove portabilitydirective, seems not to work in delphi 7
    #TODO: i think type is too much here, i changed to simpletype 

    #"""
    #pointertype : CIRCUMFLEX type portabilitydirective_opt
    #            | POINTER portabilitydirective_opt
    #"""
    p[0] = pas_ast.SomeType('type_pointer')

# --------------------------------------------------------------------------
@GRAMMER(g.type_object)
def type_object(self, p): #equal o-pas-ref #TODO: are object-elements really in this order???
    p[0] = pas_ast.SomeType('type_object')

@GRAMMER(g.type_object_heritage)
def type_object_heritage(self, p): #INFO: only used in object #TODO: i changed qualid to ident
    p[0] = pas_ast.SomeType('type_object_heritage')

#def p_qualid(self, p): #TODO: duplicate
#    """
#    qualid : ident
#    """
#    p[0] = pas_ast.SomeType('qualid')

@GRAMMER(g.type_object_fieldlist)
def type_object_fieldlist(self, p): #TODO: differs #INFO: only used in object
    p[0] = pas_ast.SomeType('type_object_fieldlist')

@GRAMMER(g.type_object_identlisttypelist)
def type_object_identlisttypelist(self, p): #TODO: this could possible used some where else??? #TODO: is this correct?
    p[0] = pas_ast.SomeType('type_object_identlisttypelist')


# ------------ Procedure Type ----------------------------------------------
@GRAMMER(g.type_procedure)
def type_procedure(self, p): #TODO: why for type?
    #TODO: type_procedure: add proc_directive_list... but i know not all directives are supported! divide the directives into logical groups!
    #TODO: type_procedure: hack to support stdcall/cdecl, TODO implement the others directives!!!
    p[0] = pas_ast.SomeType('type_procedure')    

@GRAMMER(g.type_procedure_ofobject)
def type_procedure_ofobject(self, p):
    p[0] = pas_ast.SomeType('type_procedure_ofobject')

@GRAMMER(g.type_procedure_head)
def type_procedure_head(self, p): #TODO: o-pas-ref?
    p[0] = pas_ast.SomeType('type_procedure_head')

@GRAMMER(g.type_function_head)
def type_function_head(self, p): #TODO: o-pas-ref
    p[0] = pas_ast.SomeType('type_function_head')

# --------------------------------------------------------------------------
@GRAMMER(g.type_interface_com_guid)
def type_interface_com_guid(self, p): #TODO: use guid's!
  p[0] = None #TODO: 

@GRAMMER(g.type_interface)
def type_interface(self, p): #TODO: for now classmethodlist is the same as methodlist... i simplify this
    #"""
    #interfacetype : INTERFACE interfaceheritage_opt classmethodlist_opt classpropertylist_opt END
    #"""
    p[0] = pas_ast.SomeType('type_interface')

@GRAMMER(g.type_interface_classpropertylist)
def type_interface_classpropertylist(self, p): #TODO: why only used in interfacetype???, rename if it is so!
    p[0] = pas_ast.SomeType('type_interface_classpropertylist')

#def p_classmethodlist_opt(self, p):
#    """
#    classmethodlist_opt : methodlist
#                        | empty
#    """
#    p[0] = pas_ast.SomeType('classmethodlist_opt')

@GRAMMER(g.type_interface_heritage)
def type_interface_heritage(self, p): #TODO: this should be a interface_id list!
    #TODO: only allow type_list, or better a interface_list!
    p[0] = pas_ast.SomeType('type_interface_heritage')

#def p_interfaceheritage_opt(self, p):
#    """
#    interfaceheritage_opt : empty
#                | interfaceheritage
#    """
#    p[0] = pas_ast.SomeType('')

#/* ===================================================================== */
#/* Object and Class Declaration */    
#/* ===================================================================== */
@GRAMMER(g.type_class)
def type_class(self, p): #TODO: differs
    p[0] = pas_ast.SomeType('type_class')

@GRAMMER(g.type_class_header)
def type_class_header(self, p):
    p[0] = pas_ast.SomeType('type_class_header')

@GRAMMER(g.type_class_inheritance)
def type_class_inheritance(self, p): #TODO: make opt
    #"""
    #inheritance_class:
    #           LPAREN identlist RPAREN  /* inheritance from class and interface(s) */
    #"""
    p[0] = pas_ast.SomeType('type_class_inheritance')

@GRAMMER(g.type_class_structure_decl)
def type_class_structure_decl(self, p):
    p[0] = pas_ast.SomeType('type_class_structure_decl')

@GRAMMER(g.type_class_component_struct)
def type_class_component_struct(self, p): #TODO: find a way to store the visibility level
    #"""
    #class_component_struct : visibility_decl class_component_list
    #                       | class_component_list
    #                       | visibility_decl
    #"""
    p[0] = pas_ast.SomeType('type_class_component_struct')

@GRAMMER(g.type_class_visibility_decl)
def type_class_visibility_decl(self, p): #TODO: what is automated
    p[0] = pas_ast.SomeType('type_class_visibility_decl')

#def p_class_component_list(self, p): #TODO: this is a list inside a list... bad  (I changed this)              
#    """
#    class_component_list : field_or_property_or_method_decl
#    """
#    #"""
#    #class_component_list : field_or_property_or_method_decl
#    #                     | class_component_list field_or_property_or_method_decl
#    #"""
#    p[0] = pas_ast.SomeType('class_component_list')

#    #/* The following rule is NOT entirely TRUE : DELPHI DOES NOT ALLOW */
#    #/* TO FREELY MIX Field and procedure declarations                  */
#def p_field_or_property_or_method_decl(self, p):
#    """
#    field_or_property_or_method_decl : classfielddecl 
#                                     | classmethoddecl
#                                     | classpropertydecl
#    """
#    p[0] = pas_ast.SomeType('field_or_property_or_method_decl')

@GRAMMER(g.type_class_component)
def type_class_component(self, p):
    #TODO: i moved DEFAULT here, this is surely not perfect
    p[0] = pas_ast.SomeType('type_class_component')

@GRAMMER(g.type_class_fielddecl)
def type_class_fielddecl(self, p):
    #                ; /* exemple word:TLabel ... word est un type*/
    p[0] = pas_ast.SomeType('type_class_fielddecl')

@GRAMMER(g.type_class_methoddecl)
def type_class_methoddecl(self, p):
    p[0] = pas_ast.SomeType('type_class_methoddecl')

@GRAMMER(g.type_class_propertydecl)
def type_class_propertydecl(self, p):
    p[0] = pas_ast.SomeType('type_class_propertydecl')

## =========================================================================
# ---------------------- Propertys Details ---------------------------------
## =========================================================================
@GRAMMER(g.type_class_property)
def type_class_property(self, p): #TODO: why default_opt??? this is not in the ref #TODO: before specifier there could be an index, see embacadero doc        #"""
    #classproperty : PROPERTY ident propertyinterface_opt propertyspecifiers SEMICOLON default_opt
    #"""
    #TODO: 
    p[0] = pas_ast.SomeType('type_class_property')
