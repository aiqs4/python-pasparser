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
import pygtk
pygtk.require('2.0')
import gtk, gobject

class MessageArea(gtk.HBox):
    #__gsignals__ = {"expose-event": "override"} 
    __gsignals__ = {
        
        'response': (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE, (gobject.TYPE_INT, )),
        'close': (gobject.SIGNAL_RUN_LAST | gobject.SIGNAL_ACTION, gobject.TYPE_NONE, ()),
    }

    def __init__(self, *buttons):
        gtk.HBox.__init__(self)

        #TODO: response Signal...
        #TODO: close ...

        self.contents = None
        self.action_area = None
        self.changing_style = False                  

        print "TODO: signal & bindings... etc..."

        print "INFO: now come's the real init..."
        print 'TODO: fix this hack'
        self.main_hbox = gtk.HBox(False, 16)
        self.main_hbox.set_border_width(8) #FIXME: use style properties
        self.main_hbox.show() 

        self.action_area = gtk.VBox(True, 10) #FIXME: use style properties
        self.main_hbox.pack_end(self.action_area, False, True, 0)

        self.pack_start(self.main_hbox, True, True, 0)

        self.set_app_paintable(True)

        self.connect("expose-event", self.__paint_message_area, None)

	# Note that we connect to style-set on one of the internal
	# widgets, not on the message area itself, since gtk does
	# not deliver any further style-set signals for a widget on
	# which the style has been forced with gtk_widget_set_style()
        self.main_hbox.connect("style-set", self.__style_set, self)

        if buttons:
            self.add_buttons()

    def add_action_widget(self, child, response_id):
        if not gobject.type_is_a(child, gtk.Widget):
            print 'Not a Widget' #FIXME:
            return

        print '#TODO: add response data'
        print child

        if type(child) is gtk.Button:
            signal_id = gobject.signal_lookup('clicked', type(child))
        else:
            signal_id = gobject.signal_lookup('activate', type(child))
            
        if signal_id:
            print 'TODO: check if this is correct.'
            signal = gobject.signal_name(signal_id)
            child.connect(signal, MessageArea.__action_widget_activated, self) #FIXME:
        else:
            print "Only 'activatable' widgets can be packed into the action area of a MessageArea"

        if response_id != gtk.RESPONSE_HELP:
            self.action_area.pack_start(child, False, False, 0)
        else:
            self.action_area.pack_end(child, False, False, 0)
        
            
        
    def add_button(self, button_text, response_id):
        print 'TODO: create stock button'
        button = gtk.Button(button_text)
        button.set_flags(gtk.CAN_DEFAULT)
        self.add_action_widget(button, response_id)

        return button

    def add_stock_button_with_text(self, text, stock_id, response_id):
        print "TODO: some error handling"
        button = gtk.Button("text")
        #button.add_mnemonic_label("text")
        button.set_image(gtk.image_new_from_stock(stock_id, gtk.ICON_SIZE_BUTTON))
        button.set_flags(gtk.CAN_DEFAULT)

        self.add_action_widget(button, response_id)

    def add_buttons(self, *buttons):
        print 'TODO: not yet implemented.'

    def set_response_sensitive(self, response_id, setting):
        children = self.action_area.children()
        for child in children:
            widget = child.get_data()
            rd = self.__get_response_data(widget, False)

            if (rd and rd.response_id == response_id):
                widget.set_sensitive(setting)
                

    def set_default_response(self, response_id):
        children = self.action_area.children()

        for child in children:
            widget = child.get_data()
            rd = self.__get_response_data(widget, False)

            if (rd and rd.response_id == response_id):
                widget.grab_default()

    def response(self, response_id):
        print 'TODO: error handling'
        self.emit('response', response_id)

    @staticmethod
    def __get_response_data(widget, create):
        print 'TODO: this is a static method'
        ad = widget.get_data('message-area-response-data')

        if not ad and create:
            ad = int(0) #FIXME: is this the correct type?
            widget.set_data('message-area-response-data', ad)

        return ad

    #@staticmethod
    def __find_button(self, response_id):
        print 'TODO: check if this method should be static?'
        children = self.action_area.children

        for child in children:
            print #TODO: get response data
            rd = MessageArea.__get_response_data(child.get_data(), False)
            if (rd and rd.response_id == response_id):
                return child

        return None

    def __close(self): #TODO:
        if not self.__find_button(gtk.RESPONSE_CANCEL):
            print 'WRONG signal'
            return
        self.response(gtk.RESPONSE_CANCEL) #TODO:

    def __paint_message_area(self, widget, event, user_data=None):
    #def do_expose_event(self, event, user_data=None):
        self.style.paint_flat_box(widget.window,
                                  gtk.STATE_NORMAL,
                                  gtk.SHADOW_OUT,
                                  None,
                                  widget,
                                  "tooltip",
                                  widget.allocation.x + 1,
                                  widget.allocation.y + 1,
                                  widget.allocation.width - 2,
                                  widget.allocation.height - 2)

        gtk.HBox.do_expose_event(self, event) #TODO: this calls parent

        return False


    @staticmethod
    def __style_set(widget, prev_style, message_area):
        if message_area.changing_style:
            return

        # This is a hack needed to use the tooltip background color
	window = gtk.Window(gtk.WINDOW_POPUP)
	window.set_name("gtk-tooltip")
	window.ensure_style()
	style = window.get_style()

	message_area.changing_style = True
	message_area.set_style(style)
	message_area.changing_style = False

	window.destroy()

	message_area.queue_draw()

    @staticmethod
    def __get_response_for_widget(message_area, widget):
        rd = MessageArea.__get_response_data(widget, False)
        if not rd:
            return gtk.RESPONSE_NONE
        else:
            return rd.response_id

    @staticmethod
    def __action_widget_activated(widget, message_area):
        print 'TODO: check if the call is correct'
        print 'TODO: should this better non-static?'
        response_id = MessageArea.__get_response_for_widget(message_area, widget)
        message_area.response(response_id)


    def set_contents(self, contents):
        self.contents = contents
        self.main_hbox.pack_start(self.contents, True, True, 0)


print "TODO: this should be done as class init !"
print 'TODO: fixme'
#INFO: Register the Widget
gobject.type_register(MessageArea)
#gobject.signal_new("response", MessageArea, gobject.SIGNAL_RUN_LAST,
#                   gobject.TYPE_NONE, (gobject.TYPE_INT))

#gobject.signal_new("close", MessageArea,
#                   gobject.SIGNAL_RUN_LAST | gobject.SIGNAL_ACTION,
#                   gobject.TYPE_NONE, ())        


def test():

    def set_message_area_text_and_icon (message_area,
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
        ca = message_area.get_content_area()
        ca.add(hbox_content)
    
    window = gtk.Window()
    window.connect("delete-event", gtk.main_quit)

    if False:
        widget = MessageArea()
        widget.add_stock_button_with_text('hello', gtk.STOCK_REFRESH, gtk.RESPONSE_OK)

        widget.add_button (gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL)
    else:
        infobar = gtk.InfoBar()
        infobar.add_button('hello', gtk.RESPONSE_OK)
        infobar.add_button(gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL)

        widget = infobar
        infobar.set_message_type(gtk.MESSAGE_WARNING)

    set_message_area_text_and_icon(widget, gtk.STOCK_REFRESH, 'hello world', 'example')
    
    window.add(widget)
    window.show_all()
    gtk.main()
    

if __name__ == "__main__":
    test()
