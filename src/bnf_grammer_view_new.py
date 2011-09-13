
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
import sys
import pygtk
pygtk.require('2.0') #TODO: or higher
import gtk, gobject

import gtksourceview2

import bnf_view
import bnf_parser

class GrammerViewer:
    def __init__(self, filename=None):
        #if len(sys.argv) != 2: #TODO: move to ... or remove
        #raise SystemExit("Usage: python main.py <image file>")

        if filename:
            grammer = open(filename, 'Ur').read()
        else:
            grammer = ''

        self.grammer_changed = True
        self.setup_window(grammer)
        self.reload_grammer()

    def delete_event(self, widget, event, data=None):
        print "delete event occurred"
        return False

    def destroy(self, widget, data=None):
        gtk.main_quit()

    def reload_grammer(self):
        buffer = self.sourceview.get_buffer()
        text = buffer.get_text(buffer.get_start_iter(),
                               buffer.get_end_iter()) #TODO: refactor this function
        g = bnf_parser.grammer.parseString(text)
        parsedGrammer = bnf_parser.build_dict(g)
        parsedGrammer.normalize()
        parsedGrammer.update_rule_usage()
        self.grammerview.reinit(parsedGrammer)

        self.update_tree(parsedGrammer)
        
        self.grammer_changed = False #TODO: thread-safety #TODO: too late, to reset?
        print 'TODO: reload_grammer'

    def do_reload_grammer(self, infobar, response_id, user_data=None):
        #TODO: add timeout... bad idea to parse on every change
        print 'TODO: g_idle_add'
        print 'response_id', response_id
        if response_id == gtk.RESPONSE_OK:
            self.reload_grammer()
            self.infobar.destroy()
            del self.infobar

    #TODO: this function could be static
    def infobar_set_text_and_icon(self,
                                  infobar,
                                  icon_stock_id,
                                  primary_text,
                                  secondary_text=None):
	hbox_content = gtk.HBox(False, 8)

	image = gtk.image_new_from_stock(icon_stock_id, gtk.ICON_SIZE_DIALOG)
	hbox_content.pack_start(image, False, False, 0)
	image.set_alignment(0.5, 0)

	vbox = gtk.VBox(False, 6)
	hbox_content.pack_start(vbox, True, True, 0)

	primary_markup = "<b>%s</b>" % primary_text
	primary_label = gtk.Label(primary_markup)

	vbox.pack_start(primary_label, True, True, 0)
	primary_label.set_use_markup(True)
	primary_label.set_line_wrap(True)
	primary_label.set_alignment(0, 0.5)
	primary_label.set_flags(gtk.CAN_FOCUS)
	primary_label.set_selectable(True)

        if secondary_text:	
            secondary_markup = "<small>%s</small>" % secondary_text
            secondary_label = gtk.Label(secondary_markup)

            vbox.pack_start(secondary_label, True, True, 0)
            secondary_label.set_flags(gtk.CAN_FOCUS)
            secondary_label.set_use_markup(True)
            secondary_label.set_line_wrap(True)
            secondary_label.set_selectable(True)
            secondary_label.set_alignment(0, 0.5)                                               

	hbox_content.show_all()
        ca = infobar.get_content_area()
        ca.add(hbox_content)

    def on_grammer_changed(self, buffer, data=None):
        print 'TODO: show infobar'
        #self.reload_grammer()
        if not self.grammer_changed:
            self.grammer_changed = True
            self.infobar = gtk.InfoBar()
            self.infobar.add_button(gtk.STOCK_REFRESH, gtk.RESPONSE_OK)
            self.infobar.set_message_type(gtk.MESSAGE_WARNING) #FIXME: is this needed?
            self.infobar_set_text_and_icon(self.infobar,
                                           gtk.STOCK_REFRESH,
                                           'Grammer Changed',
                                           'TreeView &amp; GrammerView needs refresh')
            self.vboxGrammer.pack_start(self.infobar, False, False, 0)

            self.infobar.connect("response", self.do_reload_grammer)                
            self.vboxGrammer.show_all()

    def setup_toolbar(self, toolbar):
        t = gtk.ToolButton(gtk.STOCK_OPEN)
        toolbar.add(t)

        t = gtk.ToolButton(gtk.STOCK_SAVE)
        toolbar.add(t)

        t = gtk.ToolButton(gtk.STOCK_CLOSE)
        toolbar.add(t)        


    def update_tree(self, grammer):
        self.treestore.clear() #TODO: FIXME: check if this is the correct way to delete all items
        for rule in grammer:
            titer = self.treestore.append(None, (rule.name, True, True))
            for ru in grammer.ruleUsage[rule.name]:
                self.treestore.append(titer, (ru, True, False))
        print 'TODO: add childs'
        
    def do_on_cell_toggle(self, cell, path, model):
        model[path][1] = not model[path][1]
        if model[path][1]:
            self.grammerview.rules[model[path][0]].show()
        else:
            self.grammerview.rules[model[path][0]].hide()
                
        print "Toggle '%s' to: %s" % (model[path][0], model[path][1],)

    def on_tv_button_pressed(self, treeview, event, userData=None):
        print 'TODO: tv_button_pressed', event.button, event.type
        if (event.button == 3) and (event.type == gtk.gdk.BUTTON_PRESS):
            # Figure out which item they right clicked on
            path = treeview.get_path_at_pos(int(event.x), int(event.y))
            # Get the selection
            selection = treeview.get_selection()

            # Get the selected path(s)
            rows = selection.get_selected_rows()

            # If they didn't right click on a currently selected row, change the selection
            if path[0] not in rows[1]:
                selection.unselect_all()
                selection.select_path(path[0])

            if selection.count_selected_rows() > 1:
                #poppup multiple selection menu
                self.popupMenu.popup(None, None, None, gtk.gdk.RIGHTBUTTON, event.time)
            else:
                #popup single selection box
                self.popupMenu.popup(None, None, None, gtk.gdk.RIGHTBUTTON, event.time)

            return True
                                       

    def on_tv_selection_changed(self, selection, data=None):
        print "Selection changed ('TODO: obsolete)"
        #self.popupMenu.popup(None, None, None, gtk.gdk.RIGHTBUTTON, 0)

    def on_tv_row_activated(self, treeview, path, view_column, data=None):
        self.grammerview.my_set_focus(self.treestore[path][0])

    def on_invert_visibility(self, item, userData=None):
        print 'invert visibility', userData
        pass

    def on_show_all(self, item, userData=None):
        print 'Show All', userData
        pass

    def on_hide_all(self, item, userData=None):
        print 'Hide All', userData
        pass
        
    def create_treeview(self):
        self.treestore = gtk.TreeStore(str, gobject.TYPE_BOOLEAN, bool)

        # for parent in range(4):
        #     piter = self.treestore.append(None, ('parent %i' % parent, True, True))
        #     for child in range(3):
        #         self.treestore.append(piter, ['child %i of parent %i' % (child, parent), False, False
        #])
        treeview = gtk.TreeView(self.treestore)

        self.cellRendererText = gtk.CellRendererText()
        self.cellRendererToggle = gtk.CellRendererToggle()
        self.cellRendererToggle.set_property('activatable', True)
        self.cellRendererToggle.connect('toggled', self.do_on_cell_toggle, self.treestore)

        self.tvcolumnText = gtk.TreeViewColumn('Rule') #, text=0)
        self.tvcolumnToggle = gtk.TreeViewColumn('Visible')

        self.tvcolumnToggle.pack_start(self.cellRendererToggle, False)
        self.tvcolumnText.pack_start(self.cellRendererText, True)

        self.tvcolumnText.add_attribute(self.cellRendererText, 'text', 0)
        self.tvcolumnToggle.add_attribute(self.cellRendererToggle, 'active', 1)
        self.tvcolumnToggle.add_attribute(self.cellRendererToggle, 'visible', 2)

        treeview.append_column(self.tvcolumnText)
        treeview.append_column(self.tvcolumnToggle)
       
        treeview.set_search_column(0)
        self.tvcolumnText.set_sort_column_id(0)

        treeview.set_reorderable(False)

        treeview.connect("row-activated", self.on_tv_row_activated)
        treeview.connect("button-press-event", self.on_tv_button_pressed)
        print 'TODO: connect the popup-menu signal'

        self.tvSelection = treeview.get_selection()
        self.tvSelection.connect("changed", self.on_tv_selection_changed)


        def add_menu_item(label, callback):
            popupMenuItem = gtk.MenuItem(label)
            self.popupMenu.append(popupMenuItem)
            if callback:
                popupMenuItem.connect('activate', callback)

        self.popupMenu = gtk.Menu()
        add_menu_item("Invert Visibility", self.on_invert_visibility)
        add_menu_item("Show All", self.on_show_all)
        add_menu_item("Hide All", self.on_hide_all)
        #popupMenuItem = gtk.MenuItem("Invert Visibility")
        #self.popupMenu.append(popupMenuItem)
        
        self.popupMenu.show_all()
        
        return treeview

    def setup_menu(self):
        self.menuBar = gtk.MenuBar()
        fileMenu = gtk.Menu()

        openMenuItem = gtk.MenuItem("Open")
        saveMenuItem = gtk.MenuItem("Save")
        quitMenuItem = gtk.MenuItem("Quit")

        fileMenu.append(openMenuItem)
        fileMenu.append(saveMenuItem)
        fileMenu.append(gtk.SeparatorMenuItem())
        fileMenu.append(quitMenuItem)

        #TODO: callbacks

        fileMenuItem = gtk.MenuItem("File")
        fileMenuItem.set_submenu(fileMenu)
        self.menuBar.append(fileMenuItem)

        helpMenu = gtk.Menu()

        aboutMenuItem = gtk.MenuItem("About")
        helpMenu.append(aboutMenuItem)

        helpMenuItem = gtk.MenuItem("Help")
        helpMenuItem.set_submenu(helpMenu)

        helpMenuItem.set_right_justified(right_justified=True)
        self.menuBar.append(helpMenuItem)


        return self.menuBar

    def setup_window(self, grammer=''):
        # Create a new window
        self.window = gtk.Window(gtk.WINDOW_TOPLEVEL)
        
        self.window.connect("delete-event", self.delete_event)
        self.window.connect("destroy", self.destroy)
        
        # Sets the border width of the window.
        self.window.set_title('Grammer Viewer')
        #self.window.set_border_width(10)
        #self.window.set_size_request(200,100)
        self.window.set_default_size(640, 400)
        
        self.vbox = gtk.VBox() #False, 0
        #self.box1.pack_start(self.button1, True, True, 0)

        mainMenu = self.setup_menu()
        self.vbox.pack_start(mainMenu, False, False, 2)
        
        self.toolbar = gtk.Toolbar()
        self.toolbar.set_style(gtk.TOOLBAR_BOTH)

        self.setup_toolbar(self.toolbar)
        
        self.vbox.pack_start(self.toolbar, False, True, 0)
        self.hpaned = gtk.HPaned()
        self.notebook = gtk.Notebook()

        self.scrolledwindow = gtk.ScrolledWindow()
        self.scrolledwindow.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)       
        self.sourceview = gtk.TextView() #gtksourceview2.GtkSourceView()
        self.scrolledwindow.add(self.sourceview)

        label = gtk.Label('Grammer')
        self.notebook.append_page(self.scrolledwindow, tab_label=label)
        
        label = gtk.Label('Browser')
        self.treeview = self.create_treeview()

        sw = gtk.ScrolledWindow()
        sw.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)
        sw.add(self.treeview)
        
        self.notebook.append_page(sw, tab_label=label)

        self.hpaned.add(self.notebook)

        self.grammerview = bnf_view.GrammerView() #TODO:

        self.vboxGrammer = gtk.VBox(False, 0)
        
        self.vboxGrammer.pack_end(self.grammerview, True, True, 0)
        
        self.hpaned.add(self.vboxGrammer)

        buffer = self.sourceview.get_buffer()
        buffer.connect('changed', self.on_grammer_changed)       
        buffer.set_text(grammer)
        
        #self.table = gtk.Table(2, 2, False)

        self.vbox.add(self.hpaned)
        
        self.statusbar = gtk.Statusbar()
        self.vbox.pack_end(self.statusbar, False, True, 0)
        
        self.sourceview.set_size_request(300, 100)
        
        self.window.add(self.vbox)
        self.window.show_all()

    def main(self):
        gtk.main()
        
def main():
    filename = sys.argv[1] if len(sys.argv) == 2 else None
        
    gv = GrammerViewer(filename)
    gv.main()

if __name__ == '__main__':
    main()
