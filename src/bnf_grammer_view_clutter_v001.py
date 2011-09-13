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
pygtk.require('2.0')

import cluttergtk
import clutter

import gtk

class BNFView:
    def delete_event(self, widget, event, data=None):
        print "delete event occurred"
        return False

    def destroy(self, widget, data=None):
        gtk.main_quit()

    def on_button_clicked(self, button):
        print "Hello World"

    def on_stage_button_pressed(self, button, data=None):
        print 'Stage Button pressed'

    def __init__(self):
        if len(sys.argv) != 2: #TODO: move to ... or remove
            raise SystemExit("Usage: python main.py <image file>")

        # Create a new window
        self.window = gtk.Window(gtk.WINDOW_TOPLEVEL)
 
        self.window.connect("delete-event", self.delete_event)
        self.window.connect("destroy", self.destroy)

        # Sets the border width of the window.
        self.window.set_title('BNF Viewer')
        self.window.set_border_width(10)
        #self.window.set_size_request(200,100)
        self.window.set_default_size(640, 400)

        self.vbox = gtk.VBox() #False, 0
        #self.box1.pack_start(self.button1, True, True, 0)

        self.toolbar = gtk.Toolbar()
        self.toolbar.set_style(gtk.TOOLBAR_TEXT)
 
        self.toolbar.append_item('Rules', 'Show Rules', 'private text', None, self.on_button_clicked)
        self.toolbar.append_space()

        self.vbox.add(self.toolbar)

        self.table = gtk.Table(2, 2, False)

        self.clutter_widget = cluttergtk.Embed()
        self.clutter_widget.set_size_request(200, 200)

        self.table.attach(self.clutter_widget,
                     0, 1,
                     0, 1,
                     gtk.EXPAND | gtk.FILL,
                     gtk.EXPAND | gtk.FILL,
                     0, 0)

        self.vbox.add(self.table)# pack_end(self.clutter_widget, True, True, 0)
     
        self.stage_color = clutter.Color(0, 0, 0, 255) # Black
        self.actor_color = clutter.Color(255, 255, 255, 153)

        self.stage = self.clutter_widget.get_stage()
        self.stage.set_color(self.stage_color)
        self.stage.set_size(640, 480)

        # Create a viewport actor to be able to scroll actor. By passing NULL it
        # will create new GtkAdjustments
        self.viewport = cluttergtk.Viewport()
        self.stage.add(self.viewport)
        self.viewport.set_size(640, 480)


        # Load image from first command line argument and add it to viewport
        self.texture = clutter.Texture(sys.argv[1])
        self.viewport_group = clutter.Group()

        self.viewport_group.add(self.texture)
        self.viewport.add(self.viewport_group)
        
        
        self.texture.set_position(0, 0)
        

        # Create scrollbars and connect them to viewport
        self.h_adjustment, self.v_adjustment = self.viewport.get_adjustments()
        self.scrollbar = gtk.VScrollbar(self.v_adjustment)
        self.table.attach(self.scrollbar,
                          1, 2,
                          0, 1,
                          0, gtk.EXPAND | gtk.FILL,
                          0, 0)

        self.scrollbar = gtk.HScrollbar(self.h_adjustment)
        self.table.attach(self.scrollbar,
                          0, 1,
                          1, 2,
                          gtk.EXPAND | gtk.FILL, 0,
                          0, 0)        

        # Connect a signal handler to handle mouse clicks and key presses on the stage
        self.stage.connect("button-press-event", self.on_stage_button_pressed)

        # Add a group to the stage
        self.group = clutter.Group()
        self.group.set_position(40, 40)
        self.viewport_group.add(self.group)

        # Add a rectangle to the stage
        self.rect = clutter.Rectangle(self.actor_color)
        self.rect.set_size(100, 400)
        self.rect.set_position(20, 20)
        self.group.add(self.rect)
        #self.rect.show()

        # Add a label to the stage
        self.label = clutter.Text("Sans 12", "Some Text", self.actor_color)
        self.label.set_size(500, 500)
        self.label.set_position(20, 15)
        self.group.add(self.label)
        #label.show()


        # This packs the button into the window (which is a container).
        self.window.add(self.vbox)

        # Show the window and the button
        self.window.show_all()

    def main(self):
        gtk.main()

def main():
    bnfview = BNFView()
    bnfview.main()

if __name__ == '__main__':
    sys.exit(main())
