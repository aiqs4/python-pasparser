#!/usr/bin/python
#
# main.py
# Copyright (C) Sascha Dewald 2011 <sascha.dewald@googlemail.com>
# 
# python-pasparser is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# python-pasparser is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License along
# with this program.  If not, see <http://www.gnu.org/licenses/>.


import sys
import os
from os.path import join, getsize

from pas_parser import PasParser

class ParseProject(object):
  searchunits = dict() #TODO: warning this are static members!!!
  projectfile = ''
  _fileextlist = {'.dpr': 'project',
                  '.pas': 'source', 
                  '.pp' : 'source', 
                  '.inc': 'inc'}
  parsefiles = dict() #TODO: is filename inside a project unique???
  
  def __init__(self, 
               projectfile, 
               searchpath = '.',
               compilerdirectives = '',
               debuglevel=0): #TODO: rename compilerdirectives, to something better!
    self._debuglevel=debuglevel
    self.projectfile = projectfile           
    self.parsed_units = dict()
    
    print '*** Generating Searchpath Units ***'
    self._generate_searchpath_unit_dict(searchpath)
    
    print '*** Scanning Compiler Directives *** (FIXME: not yet implemented)'
    
    #for typ in self.searchunits:
    #  print '=== ', typ, ' ==='
    #  for files in self.searchunits[typ]:
    #    print files, self.searchunits[typ][files]
    
  def __parse_uses_func(self, uses):
    for unit in [u.lower() for u in uses]:
      if not self.parsed_units.has_key(unit):
        already_parsed = False       
        found = False 
        if self.searchunits['pas'].has_key(unit): #TODO: remove pas & pp          
          found = True
          if not self.searchunits['pas'][unit].has_key('parse_started'):
            next = ('pp', unit, self.searchunits['pas'][unit]['files'][0])  
            self.searchunits['pas'][unit]['parse_started'] = True
          else:
            already_parsed = True
        elif self.searchunits['pp'].has_key(unit):
          found = True
          if not self.searchunits['pp'][unit].has_key('parse_started'):
            next = ('pp', unit, self.searchunits['pp'][unit]['files'][0])
            self.searchunits['pp'][unit]['parse_started'] = True
          else:
            already_parsed = True
        
        if found:
          if not already_parsed:
            self._parse_file2(next) #TODO: THIS WILL RECURSIVE SCAN NEXT, AND THE CURRENT PARSE WILL STAY ALIVE!!! NOT SO GOOD
          else:
            raise Exception('Recursive Uses Loop found.')
        else:
          raise Exception('Unit not found. (%s)' % unit)
      else:
        pass #Already parsed!
      
  def _list_directory_files(directory, fileextlist):
    filelist = []    
    for root, dirs, files in os.walk(directory):
      filelist += [os.path.join(root, f) for f in files if os.path.splitext(f)[1] in fileextlist]
    return filelist
      
  def _generate_searchpath_unit_dict(self, searchpath):
    self.searchunits.clear()
    self.searchunits['project'] = dict() #TODO: project, source, inc should be a enum (python didn't have enums, so we should use tuple's, for example! or name tuples)
    self.searchunits['source'] = dict()
    self.searchunits['inc'] = dict()
    self.searchunits['pas'] = dict()
    self.searchunits['pp'] = dict()
       
    for root, dirs, files in os.walk(searchpath):
      for f in files:
        fileelem = os.path.splitext(f)
        #fileelem[0] = fileelem[0].lower()
        unit = fileelem[0].lower()
        typ = fileelem[1][1:].lower()
        if typ in ['pas', 'pp']:# self._fileextlist: #TODO: remove pas & pp
          if self.searchunits[typ].has_key(unit):
            self.searchunits[typ][unit]['files'].append(os.path.join(root, f))
          else:
            self.searchunits[typ][unit] = {'files': [os.path.join(root, f)]}
      
  def _parse_file2(self, unit):
    typ, unit, path = unit
    print '************* Start parsing *********************'
    print 'Name: ', unit
    print 'Path: ', path
    ast = self._parse_file(path)
    self.searchunits[typ][unit]['parse_started'] = False
    self.parsed_units[unit] = ast

    if not os.path.exists('output'):
      os.mkdir('output')      
    with open('output/%s.ast' % unit, 'w') as f:
      ast.show(f)
    
  def _parse_file(self, 
                  filename): #TODO: this was initialy in __init__.py
    """ Parse a Pas/Dpr file using pypasparser.
    
        filename:
            Name of the file you want to parse.
        
        When successful, an AST is returned. ParseError can be 
        thrown if the file doesn't parse successfully. 
    """
    text = open(filename, 'rU').read()
    
    parser = PasParser(parse_uses_func=self.__parse_uses_func, lex_optimize=True, yacc_debug=True, yacc_optimize=False)
    #parser._parse_uses_func = self._add_parseunits

    #self.parsefiles[os.path.split(filename)[1]] = [filename, parser]
    #self.parsefiles[filename] = (parser, )
    
    return parser.parse(text, filename, debuglevel=self._debuglevel) #TODO: what is with the return value? should this not be stored, anywhere?
      
  def parse(self):
    """ Starts Parsing and generating Ast """
    print '*** Start Parsing and generating Ast ***'
    
    print '===================================='
    print 'TODO: make the asts global available. (for type lookup)'
    print '===================================='
    asts = []
    #1. Build ast-tree-tree, for global types, consts, vars...
    #1. First parse the first file
    #2. The first file, says which parse next

    print '==================================='
    print 'TODO: put the actual project into the parse-queue'
    print '==================================='
    asts += [self._parse_file(self.projectfile)] #TODO: what is with the return value? should this not be stored, anywhere?
    
    #print self.searchunits['source']
    
    
    if False:
      for unit in self.unitlist.itervalues():
        if unit in self.searchunits['source']:
          next = self.searchunits['source'][unit]
          asts += [self._parse_file(next[0], breakatuses = True)] #TODO: give true a name
        else:
          print '########## ERROR unit (' + unit + ') not found #####################'
    #print self.unitlist
    
    print '########### Parse Next Unit !!!!!!!!!!! ###########################'
    
    return ast
         
if __name__ == "__main__":
  if len(sys.argv) > 2:
    searchpath = sys.argv[2]
  else:
    searchpath = '.'
      
  if len(sys.argv) > 1:
    parseFile = sys.argv[1]
  else:
    parseFile = 'examples/data/zp_simple.pas'
  
  pp = ParseProject(parseFile, searchpath, debuglevel=2) #INFO: debuglevel = 2
  asts = pp.parse()
  
  for ast in asts:
    print '================================================================'
    ast.show()
  
  #pasFiles = pp.listDirectory(searchpath, ['.pas', '.pp', '.inc'])

  #print pasFiles
  
  #try:
  #    ast = pp.parse_file(pasFiles)
  #except:
  #    print '****************************************'
  #    print '********* ERROR ERROR ERROR ************'
  #    print '****************************************'
