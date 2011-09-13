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
 
# example basictreeviewtoggle.py
 
import pygtk
pygtk.require('2.0')
import gtk
import gobject
 
class BasicTreeViewToggleExample:
 
    # close the window and quit
    def delete_event(self, widget, event, data=None):
        gtk.main_quit()
        return False
 
    def column_toggled(self, cell, path, model):
        # get toggled iter, and value at column 0
        iter = model.get_iter((int(path),))
        val = model.get_value(iter, 0)
 
        # toggle the value
        val = not val
 
        # set new value
        model.set(iter, 0, val)
 
 
    def __init__(self):
        # create a new window
        self.window = gtk.Window(gtk.WINDOW_TOPLEVEL)
        self.window.set_title("Basic TreeView Toggle Example")
        self.window.set_size_request(200, 200)
        self.window.connect("delete_event", self.delete_event)
 
        # create a ListStore with two columns to use as the model
        self.model = gtk.ListStore(gobject.TYPE_BOOLEAN, str)
 
        # create some list items
        for item in range(5):
            self.model.append([False, 'item %i' % item])
 
        # create the TreeView using the model
        self.view = gtk.TreeView(self.model)
 
        # create a CellRendererText to render the data
        self.cellrenderer_text = gtk.CellRendererText()
        self.cellrenderer_toggle = gtk.CellRendererToggle()
        self.cellrenderer_toggle.connect('toggled', self.column_toggled, self.model)
 
        # create the TreeViewColumns to display the data
        self.tvcolumntext = gtk.TreeViewColumn('TreeViewColumn 1')
        self.tvcolumntoggle = gtk.TreeViewColumn('tog', self.cellrenderer_toggle, active=0)
 
        # add the TreeViewColumns to the TreeView
        self.view.append_column(self.tvcolumntoggle)
        self.view.append_column(self.tvcolumntext)
 
 
 
 
        # add the cell to the tvcolumn and allow it to expand
        self.tvcolumntoggle.pack_start(self.cellrenderer_toggle, False)
        self.tvcolumntext.pack_start(self.cellrenderer_text, True)
 
 
        # set the cell "text" attribute to column 0 - retrieve text
        # from that column in treestore
        self.tvcolumntext.add_attribute(self.cellrenderer_text, 'text', 1)
 
 
 
        # make it searchable
        self.view.set_search_column(1)
 
        # Allow sorting on the column
        self.tvcolumntext.set_sort_column_id(1)
 
        # Allow drag and drop reordering of rows
        self.view.set_reorderable(True)
 
        self.sw = gtk.ScrolledWindow()
        self.sw.add(self.view)
        self.window.add(self.sw)
 
        self.window.show_all()
 
def main():
    gtk.main()
 
if __name__ == "__main__":
    tvexample = BasicTreeViewToggleExample()
    main()
