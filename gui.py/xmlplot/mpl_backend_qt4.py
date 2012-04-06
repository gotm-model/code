from matplotlib.backend_bases import FigureCanvasBase, TimerBase
from matplotlib.backends.backend_agg import FigureCanvasAgg

from xmlstore.qt_compat import QtGui,QtCore

DEBUG = False

class TimerQT(TimerBase):
    '''
    Subclass of :class:`backend_bases.TimerBase` that uses Qt4 timer events.

    Attributes:
    * interval: The time between timer events in milliseconds. Default
        is 1000 ms.
    * single_shot: Boolean flag indicating whether this timer should
        operate as single shot (run once and then stop). Defaults to False.
    * callbacks: Stores list of (func, args) tuples that will be called
        upon timer events. This list can be manipulated directly, or the
        functions add_callback and remove_callback can be used.
    '''
    def __init__(self, *args, **kwargs):
        TimerBase.__init__(self, *args, **kwargs)

        # Create a new timer and connect the timeout() signal to the
        # _on_timer method.
        self._timer = QtCore.QTimer()
        QtCore.QObject.connect(self._timer, QtCore.SIGNAL('timeout()'),
            self._on_timer)

    def __del__(self):
        # Probably not necessary in practice, but is good behavior to disconnect
        TimerBase.__del__(self)
        QtCore.QObject.disconnect(self._timer , QtCore.SIGNAL('timeout()'),
            self._on_timer)

    def _timer_set_single_shot(self):
        self._timer.setSingleShot(self._single)

    def _timer_set_interval(self):
        self._timer.setInterval(self._interval)

    def _timer_start(self):
        self._timer.start()

    def _timer_stop(self):
        self._timer.stop()


class FigureCanvasQT( QtGui.QWidget, FigureCanvasBase ):
    keyvald = { QtCore.Qt.Key_Control : 'control',
                QtCore.Qt.Key_Shift : 'shift',
                QtCore.Qt.Key_Alt : 'alt',
                QtCore.Qt.Key_Return : 'enter'
               }
    # left 1, middle 2, right 3
    buttond = {1:1, 2:3, 4:2}
    def __init__( self, figure ):
        if DEBUG: print 'FigureCanvasQt: ', figure

        QtGui.QWidget.__init__( self )
        FigureCanvasBase.__init__( self, figure )
        self.figure = figure
        self.setMouseTracking( True )
        self._idle = True
        # hide until we can test and fix
        #self.startTimer(backend_IdleEvent.milliseconds)
        w,h = self.get_width_height()
        self.resize( w, h )

        QtCore.QObject.connect(self, QtCore.SIGNAL('destroyed()'),
            self.close_event)

    def __timerEvent(self, event):
        # hide until we can test and fix
        self.mpl_idle_event(event)

    def enterEvent(self, event):
        FigureCanvasBase.enter_notify_event(self, event)

    def leaveEvent(self, event):
        QtGui.QApplication.restoreOverrideCursor()
        FigureCanvasBase.leave_notify_event(self, event)

    def mousePressEvent( self, event ):
        x = event.pos().x()
        # flipy so y=0 is bottom of canvas
        y = self.figure.bbox.height - event.pos().y()
        button = self.buttond[event.button()]
        FigureCanvasBase.button_press_event( self, x, y, button )
        if DEBUG: print 'button pressed:', event.button()

    def mouseMoveEvent( self, event ):
        x = event.x()
        # flipy so y=0 is bottom of canvas
        y = self.figure.bbox.height - event.y()
        FigureCanvasBase.motion_notify_event( self, x, y )
        #if DEBUG: print 'mouse move'

    def mouseReleaseEvent( self, event ):
        x = event.x()
        # flipy so y=0 is bottom of canvas
        y = self.figure.bbox.height - event.y()
        button = self.buttond[event.button()]
        FigureCanvasBase.button_release_event( self, x, y, button )
        if DEBUG: print 'button released'

    def wheelEvent( self, event ):
        x = event.x()
        # flipy so y=0 is bottom of canvas
        y = self.figure.bbox.height - event.y()
        # from QWheelEvent::delta doc
        steps = event.delta()/120
        if (event.orientation() == QtCore.Qt.Vertical):
            FigureCanvasBase.scroll_event( self, x, y, steps)
            if DEBUG: print 'scroll event : delta = %i, steps = %i ' % (event.delta(),steps)

    def keyPressEvent( self, event ):
        key = self._get_key( event )
        if key is None:
            return
        FigureCanvasBase.key_press_event( self, key )
        if DEBUG: print 'key press', key

    def keyReleaseEvent( self, event ):
        key = self._get_key(event)
        if key is None:
            return
        FigureCanvasBase.key_release_event( self, key )
        if DEBUG: print 'key release', key

    def resizeEvent( self, event ):
        if DEBUG: print 'resize (%d x %d)' % (event.size().width(), event.size().height())
        w = event.size().width()
        h = event.size().height()
        if DEBUG: print "FigureCanvasQtAgg.resizeEvent(", w, ",", h, ")"
        dpival = self.figure.dpi
        winch = w/dpival
        hinch = h/dpival
        self.figure.set_size_inches( winch, hinch )
        self.draw()
        self.update()
        QtGui.QWidget.resizeEvent(self, event)

    def sizeHint( self ):
        w, h = self.get_width_height()
        return QtCore.QSize( w, h )

    def minumumSizeHint( self ):
        return QtCore.QSize( 10, 10 )

    def _get_key( self, event ):
        if event.isAutoRepeat():
            return None
        if event.key() < 256:
            key = str(event.text())
        elif event.key() in self.keyvald:
            key = self.keyvald[ event.key() ]
        else:
            key = None

        return key

    def new_timer(self, *args, **kwargs):
        """
        Creates a new backend-specific subclass of :class:`backend_bases.Timer`.
        This is useful for getting periodic events through the backend's native
        event loop. Implemented only for backends with GUIs.

        optional arguments:

        *interval*
          Timer interval in milliseconds
        *callbacks*
          Sequence of (func, args, kwargs) where func(*args, **kwargs) will
          be executed by the timer every *interval*.
        """
        return TimerQT(*args, **kwargs)

    def flush_events(self):
        QtGui.qApp.processEvents()

    def start_event_loop(self,timeout):
        FigureCanvasBase.start_event_loop_default(self,timeout)
    start_event_loop.__doc__=FigureCanvasBase.start_event_loop_default.__doc__

    def stop_event_loop(self):
        FigureCanvasBase.stop_event_loop_default(self)
    stop_event_loop.__doc__=FigureCanvasBase.stop_event_loop_default.__doc__

    def draw_idle(self):
        'update drawing area only if idle'
        d = self._idle
        self._idle = False
        def idle_draw(*args):
            self.draw()
            self._idle = True
        if d: QtCore.QTimer.singleShot(0, idle_draw)

class FigureCanvasQTAgg( FigureCanvasQT, FigureCanvasAgg ):
    """
    The canvas the figure renders into.  Calls the draw and print fig
    methods, creates the renderers, etc...

    Public attribute

      figure - A Figure instance
   """
    # JB: added "afterResize" signal
    afterResize = QtCore.Signal()

    def __init__( self, figure ):
        if DEBUG: print 'FigureCanvasQtAgg: ', figure
        FigureCanvasQT.__init__( self, figure )
        FigureCanvasAgg.__init__( self, figure )
        self.drawRect = False
        self.rect = []
        self.blitbox = None
        self.replot = True
        
        # JB: do NOT set QtCore.Qt.WA_OpaquePaintEvent because part of the figure is transparent.
        #self.setAttribute(QtCore.Qt.WA_OpaquePaintEvent)

        # JB: added "animating" flag.
        self.animating = False

    def drawRectangle( self, rect ):
        self.rect = rect
        self.drawRect = True
        self.repaint( )

    def paintEvent( self, e ):
        """
        Draw to the Agg backend and then copy the image to the qt.drawable.
        In Qt, all drawing should be done inside of here when a widget is
        shown onscreen.
        """

        #FigureCanvasQT.paintEvent( self, e )
        if DEBUG: print 'FigureCanvasQtAgg.paintEvent: ', self, \
           self.get_width_height()

        if self.replot:
            FigureCanvasAgg.draw(self)
            self.replot = False

        if self.blitbox is None:
            # matplotlib is in rgba byte order.  QImage wants to put the bytes
            # into argb format and is in a 4 byte unsigned int.  Little endian
            # system is LSB first and expects the bytes in reverse order
            # (bgra).
            if QtCore.QSysInfo.ByteOrder == QtCore.QSysInfo.LittleEndian:
                stringBuffer = self.renderer._renderer.tostring_bgra()
            else:
                stringBuffer = self.renderer._renderer.tostring_argb()

            qImage = QtGui.QImage(stringBuffer, self.renderer.width,
                                  self.renderer.height,
                                  QtGui.QImage.Format_ARGB32)
            p = QtGui.QPainter(self)
            p.drawPixmap(QtCore.QPoint(0, 0), QtGui.QPixmap.fromImage(qImage))

            # draw the zoom rectangle to the QPainter
            if self.drawRect:
                # JB draw dashed white line on solid black to get contrast
                p.setPen( QtGui.QPen( QtCore.Qt.black, 1, QtCore.Qt.SolidLine ) )
                p.drawRect( self.rect[0], self.rect[1], self.rect[2], self.rect[3] )
                
                p.setPen( QtGui.QPen( QtCore.Qt.white, 1, QtCore.Qt.DotLine ) )
                p.drawRect( self.rect[0], self.rect[1], self.rect[2], self.rect[3] )
            p.end()
        else:
            bbox = self.blitbox
            l, b, r, t = bbox.extents
            w = int(r) - int(l)
            h = int(t) - int(b)
            t = int(b) + h
            reg = self.copy_from_bbox(bbox)
            stringBuffer = reg.to_string_argb()
            qImage = QtGui.QImage(stringBuffer, w, h, QtGui.QImage.Format_ARGB32)
            pixmap = QtGui.QPixmap.fromImage(qImage)
            p = QtGui.QPainter( self )
            p.drawPixmap(QtCore.QPoint(l, self.renderer.height-t), pixmap)
            p.end()
            self.blitbox = None
        self.drawRect = False

    def draw( self ):
        """
        Draw the figure when xwindows is ready for the update
        """

        if DEBUG: print "FigureCanvasQtAgg.draw", self
        self.replot = True

        # JB: Part of the figure will be transparent (everything outside the axes region),
        # so here we explicitly clear the Agg canvas. If we do not do that, parts of the original
        # figure will "shine through" in the transparent regions.
        self.get_renderer().clear()
        
        # JB: either repaint or update based on state of "animating" flag
        #self.update()
        if self.animating:
            # Force an immediate repaint. If we'd call update instead, multiple updates might only result 
            # in a single (delayed) repaint, thus losing the animation effect.
            self.repaint()
        else:
            self.update()
        
    def blit(self, bbox=None):
        """
        Blit the region in bbox
        """
        self.blitbox = bbox
        l, b, w, h = bbox.bounds
        t = b + h
        self.repaint(l, self.renderer.height-t, w, h)

    def print_figure(self, *args, **kwargs):
        FigureCanvasAgg.print_figure(self, *args, **kwargs)
        self.draw()

    # JB: emit afterResize event after resizing.
    def resizeEvent( self, e ):
        FigureCanvasQT.resizeEvent( self, e )
        self.afterResize.emit()

