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
import gtk, gobject, cairo

from math import pi
from collections import namedtuple

FontExtent = namedtuple('FontExtent',
                        ['Ascent',
                         'Descent',
                         'Height',
                         'MaxXAdvance',
                         'MaxYAdvance',                         
                         ]
                        )

TextExtent = namedtuple('TextExtent',
                        ['XBearing',
                         'YBearing',
                         'Width',
                         'Height',
                         'XAdvance',
                         'YAdvance']
                        )


# Create a GTK+ widget on which we will draw using Cairo
class Screen(gtk.DrawingArea):
    # Draw in response to an expose-event
    __gsignals__ = {"expose-event": "override"}
    #                "realize": "override" }    

    # Handle the expose-event by drawing
    def do_expose_event(self, event):
        width = event.area.width
        height = event.area.height

        # Create the cairo context
        cr = self.window.cairo_create()

        # Restrict Cairo to the exposed area; avoid extra work
        cr.rectangle(event.area.x, event.area.y,
                     width, height)
        cr.clip()

        self.draw(cr, *self.window.get_size())

    def draw(self, cr, width, height):
        # Fill the background with gray
        cr.set_source_rgb(0.5, 0.5, 0.5)
        cr.rectangle(0, 0, width, height)
        cr.fill()

    def draw_background(self, cr, width, height):
        # ==== Background ===
        cr.save()
        cr.set_source_rgb(0.5, 0.5, 0.5)
        cr.rectangle(0, 0, width, height)
        cr.fill()

        # draw a rectangle
        cr.set_source_rgb(1.0, 1.0, 1.0)
        cr.rectangle(10, 10, width - 20, height - 20)
        cr.fill()
        cr.restore()
        # =====================




class Shapes(Screen):
    def draw(self, cr, width, height):
        cr.save()

        self.draw_background(cr, width, height)

        # draw lines
        cr.set_source_rgb(0.0, 0.0, 0.8)
        cr.move_to(width / 3.0, height / 3.0)
        cr.rel_line_to(0, height / 6.0)
        cr.move_to(2 * width / 3.0, height / 3.0)
        cr.rel_line_to(0, height / 6.0)
        cr.stroke()

        # and a circle
        cr.set_source_rgb(1.0, 0.0, 0.0)
        radius = min(width, height)
        cr.arc(width / 2.0, height / 2.0, radius / 2.0 - 20, 0, 2 * pi)
        cr.stroke()
        cr.arc(width / 2.0, height / 2.0, radius / 3.0 - 10, pi / 3, 2 * pi / 3)
        cr.stroke()
        cr.restore()

class Text(Screen):
    def draw_text(self, cr, text, x, y, extent):
        cr.save()
        cr.move_to(x,y);
        cr.show_text(text);
        cr.restore()
        
        #/* draw helping lines */
        cr.save()
        cr.set_source_rgba(1, 0.2, 0.2, 0.6)
        cr.set_line_width(6.0)
        cr.arc(x, y, 10.0, 0, 2*pi)
        cr.fill()
        cr.move_to(x, y)
        cr.rel_line_to(0, extent.YBearing) # -extents.height)
        cr.rel_line_to(extent.Width, 0)
        cr.rel_line_to(extent.XBearing, -extent.YBearing)
        cr.stroke()
        cr.restore()

        #/* draw helping lines */
        cr.save()
        cr.set_source_rgba(0.2, 0.2, 1, 0.6)
        cr.set_line_width(6.0)
        cr.arc(x, y+extent.Height+extent.YBearing, 10.0, 0, 2*pi)
        cr.fill()
        cr.move_to(x, y+extent.Height+extent.YBearing)
        cr.rel_line_to(0, -extent.Height)
        cr.rel_line_to(extent.Width, 0)
        cr.rel_line_to(extent.XBearing, -extent.YBearing)
        cr.stroke()
        cr.restore()
    
    def draw(self, cr, width, height):
        cr.save()
        
        cr.select_font_face ("Sans",
                             cairo.FONT_SLANT_NORMAL,
                             cairo.FONT_WEIGHT_NORMAL)

        cr.set_font_size (50.0);
        

        utf8 = "cairo";
        extent = TextExtent(*cr.text_extents(utf8))
        print '1) ', extent
        x=25.0
        y=150.0

        self.draw_text(cr, utf8, x, y, extent)

        x += 24 + extent.Width + extent.XBearing
        utf8 = "Jellow World, Joguhrto"
        extent = TextExtent(*cr.text_extents(utf8))
        print '2) ', extent
        self.draw_text(cr, utf8, x, y, extent)

        font_extent = FontExtent(*cr.font_extents())
        print '3) ', font_extent
        print font_extent.Ascent + font_extent.Descent
        

        cr.restore()

class Misc(Screen):
    def draw(self, cr, width, height):
        cr.save()

        self.draw_background(cr, width, height)

        # Shape
        cr.move_to (100,200)
        cr.curve_to (100,100, 100,100, 200,100)
        cr.curve_to (200,200, 200,200, 100,200)
        cr.close_path()
        # Save the state to restore it later. That will NOT save the path
        cr.save ()

        pat = cairo.LinearGradient(100,200,200,100)
        pat.add_color_stop_rgba(0, 0,0,0,1)
        pat.add_color_stop_rgba (1, 1,0,0,1)
        cr.set_source(pat) #cr.Pattern = pat

        # Fill the path with pattern
        cr.fill_preserve ()

        #  We "undo the pattern setting here
        cr.restore()

        # Color for the stroke
        cr.set_source_rgb(0,0,0)
        cr.set_line_width(3)
        cr.stroke()

        # ==== Text =======
        cr.save()
        cr.set_source_rgb(0,0,0)
        cr.select_font_face("Georgia")#, cairo.CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_BOLD)
        cr.set_font_size(12)
        cr.move_to(6,7)
        cr.show_text("Hello World")
        cr.restore()
        # =================
                            
        cr.restore()


class Label(Screen):
    __gsignals__ = { "realize": "override" }    
    
    def __init__(self, text='Hello World'):
        super(Label, self).__init__()
        self.fontSize = 36
        self.text = text
        self.text = ";"
        self.lineSpacing = 4 #FIXME: what is the correct way, to calculate the cairo line-spacing?

    def _calc_metrics(self):
        print '_calc_metrics'
        cr = self.window.cairo_create()
        cr.set_font_size(self.fontSize)
        fontExtent = FontExtent(*cr.font_extents())
        
        self.tHeight = max(fontExtent.Height,
                           fontExtent.Ascent + fontExtent.Descent)
        textExtent = TextExtent(*cr.text_extents(self.text))        

        #TODO: refactor the following 
        self.radius = (self.tHeight + self.lineSpacing) / 2.0
        self.textHAdj = fontExtent.Descent
        self.textAdj = self.radius * 0.25
        self.circleAdj = self.radius * 0.25
        self.tWidth = textExtent.Width+textExtent.XBearing
        self.tWidth = self.tWidth - 2.0*self.circleAdj
        if self.tWidth <= self.circleAdj:
            print 'is smaller'
            self.circleAdj = 0 #self.tWidth / 2.0
            self.tWidth = 0
        
        self.circleBottom = pi * 0.5
        self.circleTop = pi * 1.5


    def do_realize(self):
        print 'do_realize'
        super(Label, self).do_realize(self)
        self._calc_metrics()

    def _draw_crosshair(self, cr, width, height):
        cr.save()
        cr.set_source_rgb(0.0, 0.0, 0.0)
        cr.set_line_width(1)
        cr.move_to(width / 2.0 + 1, height / 10.0)
        cr.rel_line_to(0, height * 0.8)
        cr.move_to(width / 10.0, height / 2.0)
        cr.rel_line_to(width * 0.8, 0)
        cr.stroke()
        cr.restore()

    def _draw_text(self, cr, x, y):
        cr.save()
        cr.set_font_size(self.fontSize)
        cr.set_source_rgb(0,0,0)
        cr.move_to(x, y)
        cr.show_text(self.text)
        cr.stroke()
        cr.restore()

    def draw(self, cr, width, height):
        cr.save()
        
        x = (width-self.tWidth) / 2.0
        yText = (height + self.radius) / 2.0 #- self.textHAdj
        yBorder = height / 2.0

        cr.set_line_width(2)
        cr.set_source_rgb(0.2, 0.2, 0.2)        

        self.draw_background(cr, width, height)
        #self._draw_crosshair(cr, width, height)
        self._draw_text(cr, x-self.textAdj, yText)

        #=== Border ===      
        # Right Half-Circle
        cr.arc(x + self.tWidth, yBorder,
               self.radius, self.circleTop, self.circleBottom)
        #cr.rel_line_to(-self.tWidth+2*self.textAdj, 0)
        # Left Half-Circle
        cr.arc(x + self.circleAdj, yBorder, self.radius, self.circleBottom, self.circleTop)
        cr.close_path()
        print x, self.tWidth, self.textAdj, self.circleAdj
        print x + self.tWidth, x + self.textAdj, self.radius
        #cr.rel_line_to(self.tWidth ,0)
        cr.stroke()
        #===================
       
        cr.restore()


class Transform(Screen):
    def draw(self, cr, width, height):
        self.draw_background(cr, width, height)

        # set up a transform so that (0,0) to (1,1)
        # maps to (20, 20) to (width - 40, height - 40)
        cr.translate(20, 20)
        cr.scale((width - 40) / 1.0, (height - 40) / 1.0)

        # draw lines
        cr.set_line_width(0.01)
        cr.set_source_rgb(0.0, 0.0, 0.8)
        cr.move_to(1 / 3.0, 1 / 3.0)
        cr.rel_line_to(0, 1 / 6.0)
        cr.move_to(2 / 3.0, 1 / 3.0)
        cr.rel_line_to(0, 1 / 6.0)
        cr.stroke()

        # and a circle
        cr.set_source_rgb(1.0, 0.0, 0.0)
        radius = 1
        cr.arc(0.5, 0.5, 0.5, 0, 2 * pi)
        cr.stroke()
        cr.arc(0.5, 0.5, 0.33, pi / 3, 2 * pi / 3)
        cr.stroke()


class BezierCurve(Screen):
    def draw(self, cr, width, height):
        self.draw_background(cr, width, height)
        cr.set_line_width(1.0)
        cr.set_source_rgb(0.0, 0.0, 0.8)

        # x=25.6
        # y=128.0
        # x1=102.4
        # y1=230.4
        # x2=153.6
        # y2=25.6
        # x3=230.4
        # y3=128.0

        x=20
        y=20
        x1=200 #x3
        y1=20  #y
        x2=200  #x
        y2=20 #y3
        x3=200
        y3=500

        cr.move_to (x, y);
        cr.curve_to (x1, y1, x2, y2, x3, y3);

        cr.set_line_width (10.0);
        cr.stroke ();

        cr.set_source_rgba(1.0, 0.2, 0.2, 0.6);
        cr.set_line_width (6.0)
        cr.move_to(x, y)
        cr.line_to(x1,y1)
        cr.move_to(x2,y2)
        cr.line_to(x3,y3);
        cr.stroke();





# GTK mumbo-jumbo to show the widget in a window and quit when it's closed
def run(Widget):
    window = gtk.Window()
    window.connect("delete-event", gtk.main_quit)
    widget = Widget()
    widget.show()
    window.add(widget)
    window.present()
    gtk.main()

 


if __name__ == "__main__":
    #run(Screen)
    #run(Shapes)
    #run(Transform)
    #run(Misc)
    #run(Label)
    #run(Text)
    run(BezierCurve)


