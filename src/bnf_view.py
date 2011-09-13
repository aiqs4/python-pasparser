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

class GrammerView(gtk.ScrolledWindow): #TODO: this is a custom master container widget
    def __init__(self, grammer=None):
        gtk.ScrolledWindow.__init__(self)
        self.set_policy(gtk.POLICY_NEVER, gtk.POLICY_AUTOMATIC)
        self.vbox = gtk.VBox()
        self.vbox.set_homogeneous(False)
        self.add_with_viewport(self.vbox)

        if grammer:
            self.reinit(grammer)
        
    # def do_on_expanded(self, expander, param_spec, user_data=None):
    #     if expander.get_expanded():
    #         expander.child.hide_all()
    #     else:
    #         expander.child.show_all()
            
    def _init(self):
        pass

    def my_set_focus(self, ruleName):        
        child = self.rules[ruleName].get_label_widget() #FIXME: better use expander?
        #self.set_focus_child(child) #INFO: this doesn't work, because our child couldn't get focus

        adj = self.get_vadjustment()
        alloc = child.get_allocation()        
        if alloc.y < adj.value or alloc.y > adj.value + adj.page_size:
            adj.set_value(min(alloc.y, adj.upper-adj.page_size))


    def reinit(self, new_grammer): #INFO: Full reload
        #self.tokenpos = dict([]) #INFO: links to all token-pos's
        self.grammer = new_grammer
        self.rules = dict([])
        
        print 'reinit: ', len(self.grammer)

        for child in self.vbox.get_children():
            self.vbox.remove(child)
            #child.destroy()
        
        for ruleName in self.grammer.ruleOrder:
            rule = self.grammer.rules[ruleName]
            ruleview = RuleView(self.grammer, rule)
            #ruleview.show() #FIXME: is this needed? (see realize)

            sw = gtk.ScrolledWindow()
            sw.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_NEVER)
            sw.add_with_viewport(ruleview)

            expander = gtk.Expander(rule.name)
            expander.set_expanded(True)
            expander.add(sw)            
            expander.get_settings().set_property('gtk-enable-animations', False)
            #expander.connect('notify::expanded', self.do_on_expanded)

            self.vbox.add(expander)

            self.rules[rule.name] = expander #TODO: put this to the RuleView...

        self.show_all()

class RuleView(gtk.DrawingArea): #TODO: this is a cairo widget
    __gsignals__ = {"expose-event": "override",
                    "realize": "override" }
    def __init__(self, grammer, rule):
        gtk.DrawingArea.__init__(self)
        #self.connect("expose_event", self.expose)
        self.rule = rule
        self.fontSize = 12
        self.lineSpacing = 4
        self.tokens = []
        self._padding = 4.5

        self.grammer = grammer
       
    def init_tokens(self, cr): #TODO: refactor into smaller parts
        xSpace = 8 + self.radius #TODO: remove const
        self.connectorCoords = []
        self.endConCoords = []

        print 'TODO: tokenWidth could be global calculated!'
        tokenWidth = [0] * self.rule.max_tokens()

        tokenDict = dict([])
        for tokenlist in self.rule:
            print 'TODO: use a token geometry tuple'
            for pos, token in enumerate(tokenlist):                
                if not self.rule.globalTokenDict[token].isEmpty:
                    if not tokenDict.has_key(token):
                        t = TokenView(self, token, cr, x=0, y=0)
                        tokenDict[token] = t #TODO: not needed to add the complete view here                

                    if tokenWidth[pos] < tokenDict[token].tWidth:
                        tokenWidth[pos] = tokenDict[token].tWidth


        tokenStartPos = []
        for i in range(self.rule.max_tokens()):
            #print sum(tokenWidth[:i]), i
            tokenStartPos.append(sum(tokenWidth[:i]) + i*(xSpace + 2*self.radius))

        print 'tokenStartPos', tokenStartPos


        x = self._padding
        y = self._padding
        
        t = TokenView(self, '>>', cr, x, y)
        self.tokens.append(t)
        #t.calc_metrics(cr)

        calcConnectorPos = lambda t, x, y: ((x, y + t.tHeight / 2.0),
                                            (x + t.tWidth + 2.0*t.radius, y + t.tHeight / 2.0))

        lastLeftConnector, lastRightConnector = calcConnectorPos(t, x, y)
        
        xStartPos = lastRightConnector[0] + xSpace
        
        yStep = self.tokens[0].tHeight * 2.0
        width = xStartPos + 2.0 * self._padding #TODO:
        height = yStep + 2.0 * self._padding
 
        #yStart = y + 1.0 + self.radius
        startCon = lastRightConnector

        for tokenlist in self.rule:
            x = xStartPos
            lastConPos = startCon
            for pos, token in enumerate(tokenlist):
                if self.rule.globalTokenDict[token].isEmpty:
                    token = ''

                x = tokenStartPos[pos] + xStartPos
                
                t = TokenView(self, token, cr, x, y)
                #t.calc_metrics(cr)
                lastLeftConnector, lastRightConnector = calcConnectorPos(t, x, y)

                
                self.tokens.append(t)
                
                x = lastRightConnector[0] + xSpace
                
                width = max(width, x)
                newConPos = lastLeftConnector #, y + 1.0 + self.radius)
                self.connectorCoords.append((lastConPos, newConPos))
                lastConPos = lastRightConnector #, y + 1.0 + self.radius)
            self.endConCoords.append(lastRightConnector)
            
            y += yStep
            height = max(height, y)

        t = TokenView(self, '><', cr, width, self._padding)
        lastLeftConnector, lastRightConnector = calcConnectorPos(t, width, self._padding)
        self.tokens.append(t)
        #t.calc_metrics(cr)
        x += t.tWidth + xSpace + 2.0 * self.radius
        width = max(width, x)

        for con in self.endConCoords:
            self.connectorCoords.append((con, lastLeftConnector))

        self.width = width
        self.height = height

    def do_realize(self):
        #gtk.DrawingArea.realize(self)
        super(RuleView, self).do_realize(self) #FIXME: what is the correct way to do this? (see comment above
        self._calc_metrics() #TODO: give cairo to this function...

        cr = self.window.cairo_create()
        self.init_tokens(cr)

        self.set_size_request(int(self.width), int(self.height))
    

    def _calc_metrics(self): #TODO: this could be done once on the parent widget!
        cr = self.window.cairo_create()
        cr.set_font_size(self.fontSize)
        fontExtent = FontExtent(*cr.font_extents())

        self.textHAdj = fontExtent.Descent
        
        self.tHeight = max(fontExtent.Height,
                           fontExtent.Ascent + fontExtent.Descent)

        self.radius = (self.tHeight + self.lineSpacing) / 2.0

    # Handle the expose-event by drawing
    def do_expose_event(self, event):
        width = event.area.width
        height = event.area.height
        # Create the cairo context
        cr = self.window.cairo_create()
        # Restrict Cairo to the exposed area; avoid extra work
        cr.rectangle(event.area.x, event.area.y, width, height)
        cr.clip()

        cr.set_font_size(self.fontSize) 
        
        self.draw(cr, *self.window.get_size())

    def draw(self, cr, width, height):
        #cr.save()
        self.draw_background(cr, width, height)
        self.draw_connectors(cr)
        #cr.restore()

    def draw_background(self, cr, width, height):
        cr.save()
        cr.set_source_rgb(0.2, 0.2, 0.2)
        cr.rectangle(0, 0, width, height)
        cr.fill()
        cr.restore()

        # draw tokens an so...
        for token in self.tokens:
            token.draw(cr, 0, 0)
        print 'TODO: set x & y'

    def draw_connectors(self, cr):
        if len(self.connectorCoords) > 0:            
            cr.save()        
            cr.set_source_rgb(0.8, 0.8, 0.8)
            cr.set_line_width(1.0)
            for con in self.connectorCoords:
                if con[0][1] == con[1][1]:
                    cr.move_to(con[0][0], con[0][1])
                    cr.line_to(con[1][0], con[1][1])
                elif con[0][1] < con[1][1]:
                    cr.move_to(con[0][0], con[0][1])

                    cr.curve_to(con[0][0], con[0][1],
                                con[0][0]+self.radius, con[0][1],
                                con[0][0]+self.radius, con[0][1]+self.radius)

                    cr.line_to(con[0][0]+self.radius,  con[1][1]-self.radius)
                    cr.curve_to(con[0][0]+self.radius, con[1][1]-self.radius,
                                con[0][0]+self.radius, con[1][1],
                                con[1][0], con[1][1])
                else:
                    cr.move_to(con[0][0], con[0][1])
                    cr.line_to(con[1][0]-2*self.radius, con[0][1])

                    cr.curve_to(con[1][0]-2*self.radius, con[0][1],
                                con[1][0]-self.radius, con[0][1],
                                con[1][0]-self.radius, con[0][1]-self.radius)
                    cr.line_to(con[1][0]-self.radius, con[0][1]-self.radius)
                    cr.line_to(con[1][0]-self.radius, con[1][1]+self.radius)
                    
                    cr.curve_to(con[1][0]-self.radius, con[1][1]+self.radius,
                                con[1][0]-self.radius, con[1][1],
                                con[1][0], con[1][1])                
                    
                cr.stroke()
            cr.restore()
        else:
            print 'No ConnectorCoords found!' #TODO:
       

class TokenView(): #TODO: this is a single cairo object
    def __init__(self, ruleview, text, cr, x, y):
        self.ruleview = ruleview
        self.x = x
        self.y = y
        #self.parent = parent
        self.text = text
        self.lineSpacing = self.ruleview.lineSpacing #TODO: value from parent

        self.calc_metrics(cr, x, y)
    
    def do_on_mouse_over(self):
        pass

    def do_on_click(self):
        pass

    def calc_metrics(self, cr, x, y):
        self.radius = self.ruleview.radius #TODO: value from parent
        self.tHeight = self.ruleview.tHeight #TODO: value from parent
        cr.set_font_size(self.ruleview.fontSize) #TODO: value from parent
        textExtent = TextExtent(*cr.text_extents(self.text))

        #TODO: refactor the following 
        self.textAdj = 0# self.radius * 0.25
        self.circleAdj = 0 #self.radius * 0.25
        self.tWidth = textExtent.Width + textExtent.XBearing
        if False:
            self.tWidth = self.tWidth - 2.0*self.circleAdj
            if self.tWidth <= self.circleAdj:
                print 'is smaller'
                self.circleAdj = 0 #self.tWidth / 2.0

        #self.leftConnector = (x, y + self.tHeight / 2.0)
        #self.rightConnector = (x + self.tWidth + 2.0*self.radius, y + self.tHeight / 2.0)

        self.circleBottom = pi * 0.5 
        self.circleTop = pi * 1.5

    def draw(self, cr, x, y):
        cr.save()
        cr.set_line_width(1.0)
        self.draw_border(cr, self.x, self.y)
        self.draw_text(cr, self.x, self.y)
        cr.restore()
        
    def draw_border(self, cr, x, y):
        cr.save()

        #yBorder = height / 2.0
        yBorder = y + self.radius 
        
        # Right Half-Circle
        cr.arc(x + self.tWidth + self.radius, yBorder,
               self.radius, self.circleTop, self.circleBottom)
        #cr.rel_line_to(-self.tWidth+2*self.textAdj, 0)
        # Left Half-Circle7
        cr.arc(x + self.circleAdj + self.radius,
               yBorder, self.radius,
               self.circleBottom, self.circleTop)
        cr.close_path()
        cr.stroke()
        cr.restore()

    def draw_text(self, cr, x, y):
        cr.save()

        cr.set_line_width(2.0)

        x = x - self.textAdj + self.radius
        y = y + self.radius * 1.5

        cr.set_font_size(self.ruleview.fontSize) #TODO: value from parent
        cr.set_source_rgb(0.8, 0.8, 0.8)
        cr.move_to(x, y)
        cr.show_text(self.text)
        cr.stroke()
        cr.restore()

#def StartEndToken(TokenView):
    

def main():
    pass

if __name__ == '__main__':
    main()
