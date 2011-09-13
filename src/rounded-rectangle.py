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


import clutter
from clutter import cogl
 
class RoundedRectangle(clutter.Actor):
    """
    Custom actor used to draw a rectangle that can have rounded corners
    """
    __gtype_name__ = 'RoundedRectangle'
 
    def __init__(self, width, height, arc, step, 
        color=None, border_color=None, border_width=0):
        """
        Creates a new rounded rectangle
        """
        super(RoundedRectangle, self).__init__()
        self._width = width
        self._height = height
        self._arc = arc
        self._step = step
        if color:
            self._color = color
        else:
            self._color = clutter.color_from_string("#000")
        if border_color:
            self._border_color = border_color
        else:
            self._border_color = clutter.color_from_string("#000")
        self._border_width = border_width
 
    def do_paint(self):
 
        # Draw a rectangle for the clipping
        cogl.path_round_rectangle(0, 0, self._width, self._height, self._arc, self._step)
        cogl.path_close()
        # Start the clip
        cogl.clip_push_from_path()
 
        # set color to border color
        cogl.set_source_color(self._border_color)
        # draw the rectangle for the border which is the same size and the
        # object
        cogl.path_round_rectangle(0, 0, self._width, self._height, self._arc, self._step)
        cogl.path_close()
        # color the path usign the border color
        ##cogl.path_fill()
        #cogl.path_stroke()
        
        if True: #False:
            # set the color of the filled area
            cogl.set_source_color(self._color)
            # draw the content with is the same size minus the wirth of the border
            # finish the clip
            cogl.path_round_rectangle(self._border_width, self._border_width, 
                self._width - self._border_width, 
                self._height - self._border_width, self._arc, self._step)
            cogl.path_fill() 
        cogl.path_close()
 
        cogl.clip_pop()
 
    def do_pick(self, color):
        if self.should_pick_paint() == False:
            return

        #return
        cogl.path_round_rectangle(0, 0, self._width, self._height, self._arc, self._step)
        cogl.path_close()
        # Start the clip
        cogl.clip_push_from_path()
        # set color to border color
        cogl.set_source_color(color)
        # draw the rectangle for the border which is the same size and the
        # object
        cogl.path_round_rectangle(0, 0, self._width, self._height, self._arc, self._step)
        cogl.path_close()
        cogl.path_fill() 
        cogl.clip_pop()
 
    def get_color(self):
        return self._color
 
    def set_color(self, color):
        self._color = color
        self.queue_redraw()
 
    def get_border_width(self):
        return self._border_width
 
    def set_border_width(self, width):
        self._border_width = width
        self.queue_redraw()
 
    def get_border_color(color):
        return self._border_color
 
    def set_border_color(self, color):
        self._border_color = color
        self.queue_redraw()
 
stage = clutter.Stage()
stage.set_size(400, 400)
rect = RoundedRectangle(200, 200, 12, 0.5)
rect.set_color(clutter.color_from_string("#88C"))
rect.set_border_width(5)
rect.set_position(12, 12)
stage.add(rect)
#show everything in the stage
stage.show_all()
stage.connect("destroy",clutter.main_quit)
#main clutter loop
clutter.main()
