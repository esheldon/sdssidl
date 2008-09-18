
PRO mplot, x, y, $
           column = col, $
           row = row, $
           position = position, $
           mxMargin = mxMargin, $
           myMargin = myMargin, $
           square = square, $
           noPlot = noPlot, $
           xRange = xRange, $
           yRange = yRange, $
           charSize = charSize, $
           xCharSize = xCharSize, $
           yCharSize = yCharSize, $
           xTicks = xTicks, $
           yTicks = yTicks, $
           xTickV = xTickV, $
           yTickV = yTickV, $
           xTick_Get = xTickGet, $
           yTick_Get = yTickGet, $
           xTickName = xTickName, $
           yTickName = yTickName, $
           xTickFormat = xTickFormat, $
           yTickFormat = yTickFormat, $
           noTickLabels = noTickLabels, $
           forceXTickNames = forceXTickNames, $
           forceYTickNames = forceYTickNames, $
           forceTopTickNames = forceTopTickNames, $
           forceRightTickNames = forceRightTickNames, $
           showLeftTickName = showLeftTickName, $
           showBottomTickName = showBottomTickName, $
           showRightTickName = showRightTickName, $
           showTopTickName = showTopTickName, $
           rightTickNames = rightTickNames, $
           topTickNames = topTickNames, $
           xTitle = xTitle, $
           yTitle = yTitle, $
           xTitCharSize = xTitCharSize, $
           yTitCharSize = yTitCharSize, $
           xTitOffSet = xTitOffset, $
           yTitOffSet = yTitOffset, $
           mTitle = mTitle, $
           mxTitle = mxTitle, $
           myTitle = myTitle, $
           mTitOffset = mTitOffset, $
           mxTitOffset = mxTitOffset, $
           myTitOffset = myTitOffset, $
           mTitSize = mTitSize, $
           mxTitSize = mxTitSize, $
           myTitSize = myTitSize, $
           cTitle = cTitle, $
           cTitOffset = cTitOffset, $
           cTitSize = cTitSize, $
           rTitle = rTitle, $
           rTitOffset = rTitOffset, $
           rTitSize = rTitSize, $
           _extra = extra

;+
; NAME:
;   mplot
;
; PURPOSE:
;
;   Generate abutted Multiple PLOTs on a page
;
; CATEGORY:
;
;   Plotting
;
; CALLING SEQUENCE:
;
;   mplot, [x,] y
;
;   Note: be sure to set !p.multi to your appropriate values first.
;   !p.multi[1] is the number of columns, !p.multi[2] is the number of
;   rows, and !p.multi[0] is the number of plots remaining (<=0 to start
;   a new page from the top left).
; 
; INPUTS:
;
;   y
;
;     Ordinate data to be plotted.  This is passed to plot, so the same
;     floating point restrictions apply.
;
; OPTIONAL INPUTS:
;
;   x
;
;     A vector argument. If x is not specified, y is plotted as a function 
;     of point number (starting at zero). If both arguments are provided, 
;     y is plotted as a function of x.
;	
; KEYWORD PARAMETERS:
;
;   Keywords used by `plot' are accepted; however some have slightly different
;   effects (see below).  You will probably want to use the [x|y]range
;   keywords with [x|y]style=1 for each mplot call to insure that the axes
;   match across sub-plot boundaries.
;
;   charSize
;
;     Overall scaling for annotation character size.  See the description
;     of the graphics keyword in the IDL documentation for more information.
;
;   [x|y]CharSize
;
;     Character size used to annotate the axis of a sub-plot.  See the 
;     description of the graphics keyword in the IDL documentation for more 
;     information.
;
;   column
;
;     Column index for the current sub-plot.  The default is to step through 
;     the sub-plots left to right, then top to bottom, until the entire page 
;     has been filled.  The leftmost column is defined by column=1, and the
;     column index increases to the right up to !p.multi[1].  If this is set 
;     to a named variable which is undefined or zero on input, the current 
;     row will be returned in the variable; otherwise it overrides the default.
;
;   force[X|Y]TickNames
;
;     By default, tick labels are only written on the left axis of the sub-
;     plots in the leftmost column and the bottom axis of the sub-plots in
;     the bottommost row.  To force labels on sub-plots at an arbitrary
;     (column, row), set these keywords.  See also the RightTickNames and 
;     TopTickNames keywords to get tick labels on the right and top sides
;     of sub-plots.
;
;   force[Right|Top]TickNames
;
;     By default, when [right|top]TickNames is set, these tick labels are 
;     only drawn for plots in the [right|top]-most [column|row].  To force
;     these labels on sub-plots at an arbitrary (column, row), set these
;     keywords.
;
;   m[x|y]Margin
;
;     This keyword functions exactly like the [x|y]Margin graphics keyword,
;     but it applies to the entire plot rather than to each sub-plot.  Note
;     that if you are using this keyword for one mplot call, you very probably
;     need to explicitly specify the same values in ALL calls to mplot which 
;     produce sub-plots on the same page.  Otherwise, the sub-plot positions
;     will be calculated assuming different margin widths.  The value is a 
;     two-element vector specifying the margins on the left/bottom followed
;     by right/top sides of the plot, in units of character size.  Default is 
;     [10,3] for X, [4,2] for Y.  Unlike plot, mplot does no variation of 
;     margins to account for big tick names or labels on the top/right sides 
;     of the plot, so you may want to fiddle with this.
;
;   mTitle
;
;     Title for the entire page of plots.  Individual sub-plot titles can be 
;     generated with the usual `title' keyword.
;
;   m[x|y]Title
;
;     Titles for the axes of the entire page of plots.  Individual
;     sub-plot titles can be generated with the usuam [x|y]Title keywords
;
;   mTitOffset
;
;     Offset the title upwards by this amount, measured in character size
;     units.  Default is 0.  You may want to adjust this if you have large
;     labels on the top edge of the plot, since mplot does not adjust the
;     plot position to account for such details.  Default value is zero,
;     in which case the title is written 0.5 character heights above the top
;     bottom edge of the top margin.
;
;   m[x|y]TitOffset
;
;     Same as titOffset, except mxTitOffset is the downward displacement of
;     the X-axis title, and myTitOffset is the leftward displacement of the
;     Y-axis title.  Default values are zero, in which case the x-axis title
;     is placed one character height above the bottom of the region, and
;     the y-axis title is placed 6 character widths from the right edge
;     of the left margin.
;
;   mTitSize
;
;     Character size for the main plot title, in units of 1.25 times the
;     global character size.
;
;   m[x|y]TitSize
;
;     Character size for the global plot axes, in units of [x|y]CharSize
;     times the global character size.
;
;   noPlot
;
;     Set this keyword to calculate the sub-plot Position (returned in a
;     named variable attached to the keyword) and draw any request labels,
;     but not draw any axes or data and do not update !p.multi.  This is
;     useful if you want to draw plots of different sizes: call mplot with
;     /noPlot in order to compute the default normalised postion, then
;     twiddle the appropriate elements and call mplot with the position
;     keyword set to the new values.
;
;   noTickLabels
;
;     If set fills the tickname keyword with blank strings.
;
;   position
;
;     Normally this keyword should not be set to a non-zero value.  Don't go 
;     fiddling with it unless you're prepared to suffer the consequences.  It 
;     overrides the calculation of where a sub-plot belongs on the current 
;     page.  This is a four-element vector giving the coordinates 
;     [(x0,y0),(x1,y1)] of the lower left and upper right corners of the plot 
;     window, respectively.  See the description of the graphics keyword in 
;     the IDL documentation for more information.  If this is set no a named
;     variable which is undefined or zero on input, the computed position will
;     be returned in the variable.
;
;   [x|y]range
;
;     See documentation for `plot'.
;
;   rightTickNames
;
;     Set this keyword to add tick labels to the right side of the plot.
;     To force this to happen even when the sub-plot is not in the rightmost
;     column, you must also set forceYTicks.
;
;   row
;
;     Row index for the current sub-plot.  The default is to step through 
;     the sub-plots left to right, then top to bottom, until the entire page 
;     has been filled.  The bottommost row is defined by row=1, and the row
;     index increases upward up to !p.multi[2].  If this is set to a named
;     variable which is undefined or zero on input, the current row will be
;     returned in the variable; otherwise it overrides the default.
;
;   show[Left|Bottom]TickName
;
;     If this keyword is set, the lowest tick which IDL wants to label is
;     labeled.  This is set by default, unless the show[Right|Top]TickName 
;     keyword is set, in which case it is unset by default.  (All ticks at the 
;     edges of the combined plot are labeled.)
;
;   show[Right|Top]TickName
;
;     If this keyword is set, the highest tick which IDL wants to label is
;     labeled.  This is unset by default.  (All ticks at the edges of the
;     combined plot are labeled.)
;
;   square
;
;     If this keyword is set to 1, produce square (aspect ratio=1) subplots.
;     If its value is 2, produce an overall square figure regardless of
;     number of sub-plots.  Default is 0, fill the window as IDL usually
;     does.
;
;   [x|y]Tick_Get
;
;     Set to a named variable to return the tick values; see the IDL
;     graphics keyword documentation.
;
;   [x|y]TickFormat
;
;     String or function name to set tick label formats; see the IDL
;     graphics keyword documentation.
;
;   [x|y]TickName
;
;     A character array containing explicit names for the tick labels.  See
;     the IDL graphics keyword documentation for more information.
;
;   [x|y]Ticks
;    
;     Number of major tick intervals.  See the IDL graphics keyword 
;     documentation for more information.
;
;   [x|y]TickV
;
;     Values for the major ticks.  See the IDL graphics keyword documentation 
;     for more information.
;
;   [r|c]Title
;
;     Title for the current row or column.
;
;   [x|y]Title
;
;     Titles for the current sub-plot.
;
;   [r|c]TitOffset
;
;     Offset the row/column titles left/up.  Default is zero, in which case
;     the row title is five character widths from the right edge of the left
;     margin, and the col title is 0.75 character heights above the bottom
;     edge of the top margin.
;
;   [r|c]TitSize
;
;     Character size for the row/column titles, in units of the global
;     character size.
;
;   topTickNames
;
;     Set this keyword to add tick labels to the top side of the plot.
;     To force this behavior even when the sub-plot is not in the topmost row,
;     you must also set the forceXTicks keyword.
;
; OUTPUTS:
;
;   A single sub-plot of an abutted multi-plot.
;
; OPTIONAL OUTPUTS:
;
;   If the position, row, or column keywords are set to a named variable
;   which is undefined or zero on input, the mplot-computed values are returned
;   in the variables.
;
; COMMON BLOCKS:
;
;   None
;
; SIDE EFFECTS:
;
;   The call to plot decrements !p.multi[0] by 1.  Thus after plotting
;   !p.multi[1] x !p.multi[2] plots, you will have the screen erased
;   unless you use the /noerase keyword.
;
; RESTRICTIONS:
;
;   mplot is less intelligent than plot about figuring out in advance how
;   large some set of labels will be, then adjusting the plot window and
;   margins accordingly.  However, keywords are provided to make this
;   sort of adjustment relatively straightforward, if tedious.  Of course,
;   there is really no way to solve this problem without a single function
;   call for all sub-plots, because you don't know in general how big the 
;   annotations will be until you've made all the sub-plots.  To use both
;   row/column titles and large axis titles, you will need to fiddle with
;   the title offset keywords and/or the margins.
;
;   To produce plots of differing size, more than one call is required.
;   First call mplot with /noPlot and the position keyword set to an
;   undefined or zero variable in order to get the default position.  Then
;   twiddle with the appropriate elements of the Position vector and call
;   mplot with position set to these values.  I can't think of a good
;   general way to handle this with keywords.
;
;   There doesn't seem to be a nice way of adjusting the character size
;   for individual sub-plot titles (which can become microscopic when there
;   are many plots per page).  So you're better off using the row/column
;   titles.
;
;   Please contact me if you find missing features which would improve the 
;   code.
;
; PROCEDURE:
;
;   This is a wrapper for plot, which goes through some tomfoolery adjusting
;   the position keyword so that plots can be easily abutted against one
;   another.  This task is of course wholly trivial in SM but sadly lacking 
;   in IDL.
;
; EXAMPLE:
;
;   To make a series of 24 rather boring plots:
;
;     !p.multi = [0,6,4]
;     FOR i=1,24 DO $
;        mplot, [0,1], [0,1]
;
;   A more complicated example of 6 almost equally boring plots:
;
;     !p.multi = [0,2,3]
;     mplot, [0.001,1], [0,1], /xlog, /square, mymar=[3,3], $
;        mtitle="FOO", mxtitle="X-foo", mytitle="Y-foo", $
;        mtitoff=0.5, mxtitoff=-1, mytitoff=4, $
;        rtitle="Row 3", ctitle="Col 1"
;     mplot, [0.001,1], [0,1], /xlog, /square, mymar=[3,3], ctitle="Col 2"
;     mplot, [0.001,1], [0,1], /xlog, /square, mymar=[3,3], rtitle="Row 2"
;     mplot, [0.001,1], [0,1], /xlog, /square, mymar=[3,3]
;     mplot, [0.001,1], [0,1], /xlog, /square, mymar=[3,3], rtitle="Row 1"
;     mplot, [0.001,1], [0,1], /xlog, /square, mymar=[3,3]
;
;   There are many additional keywords for your amusement.
;
; MODIFICATION HISTORY:
;
;   1997/10/16  v1.0   jeb  jbaker@astro.berkeley.edu

;   1997/10/17  v1.0a  jeb  
;     Bug fixes
;     Added /noPlot
;     Modified title keywords for better compatibility with plot
;
;   1997/10/21  v1.1   jeb
;     Changed defaults so that system variables are recognized
;     Keywords for adjusting ticks are now handled as in plot
;     Simplified the show[Right|Left|Top|Bottom]TickName logic
;
;   1997/10/24  v1.1a  jeb
;     Fixed force[right/top]tickname bug
;
;   1998/02/03  v1.1b  jeb
;     Added [x|y]title keywords; when passed as _extra to both plot
;     and axis commands, these were producing titles in different
;     places!
;
;   1998/04/28  v1.2  jeb
;     xstyle/ystyle keyword support: this needs to be worked on.
;
;   1999/02/17  v1.3  jeb
;     Added [x|y]Tick_Get keywords as _extra sadly doesn't handle
;     undefined named variables.
;
;   1999/02/26  v1.4  jeb
;     Pass [x|y]range explicitly to plot; seems to be a problem with 
;     _extra!
;
;   1999/03/19  v1.5  jeb
;     Allow to be called with only one argument.  Added noTickNames.
;
;   1999/04/19  v1.6  jeb
;     Pass [x|y]TickFormat keywords explicitly to handle function
;     calls.  _extra sucks!
;
;   1999/04/19  v2.0 jeb
;     Major code clean up.
;-

on_error, 2

;;; Set the default X margin

IF keyword_set(mxMargin) THEN BEGIN
   IF n_elements(mxMargin) NE 2 THEN BEGIN 
      message, 'mxMargin must be a 2-element array'
   ENDIF
ENDIF ELSE BEGIN
   mxMargin = !x.margin
ENDELSE
IF n_elements(mxMargin) NE 2 THEN mxMargin = [10, 3]

;;; Set the default Y margin

IF keyword_set(myMargin) THEN BEGIN
   IF n_elements(myMargin) NE 2 THEN BEGIN 
      message, 'myMargin must be a 2-element array'
   ENDIF
ENDIF ELSE BEGIN
   myMargin = !y.margin
ENDELSE
IF n_elements(myMargin) NE 2 THEN mxMargin = [4, 2]

;;; Set default tick names

IF NOT keyword_set(xTickName) THEN xTickName = !x.tickname
IF NOT keyword_set(yTickName) THEN yTickName = !y.tickname
IF n_elements(xTickName) EQ 0 THEN xTickName = ''
IF n_elements(yTickName) EQ 0 THEN yTickName = ''

;;; Set default tick properties: ticks and tickv

IF NOT keyword_set(xTicks) THEN xTicks = !x.ticks
IF NOT keyword_set(yTicks) THEN yTicks = !y.ticks
IF NOT keyword_set(xTickV) THEN xTickV = !x.tickv
IF NOT keyword_set(yTickV) THEN yTickV = !y.tickv

;;; Set default character sizes

IF NOT keyword_set(charSize) THEN charSize = !p.CharSize
IF NOT keyword_set(xCharSize) THEN xCharSize = !x.CharSize
IF NOT keyword_set(yCharSize) THEN yCharSize = !y.CharSize
IF charSize EQ 0. THEN charSize = 1.
IF xCharSize EQ 0. THEN xCharSize = 1.
IF yCharSize EQ 0. THEN yCharSize = 1.
IF NOT keyword_set(xTitCharSize) THEN xTitCharSize = xCharSize
IF NOT keyword_set(yTitCharSize) THEN yTitCharSize = yCharSize

; Set default title properties

IF NOT keyword_set(xTitOffset) THEN xTitOffset = 5
IF NOT keyword_set(yTitOffset) THEN yTitOffset = 5
IF NOT keyword_set(mTitOffset) THEN mTitOffset = 0
IF NOT keyword_set(mXTitOffset) THEN mXTitOffset = 0
IF NOT keyword_set(mYTitOffset) THEN mYTitOffset = 0
IF NOT keyword_set(mTitSize) THEN mTitSize = 1
IF NOT keyword_set(mXTitSize) THEN mXTitSize = 1
IF NOT keyword_set(mYTitSize) THEN mYTitSize = 1
IF NOT keyword_set(cTitOffset) THEN cTitOffset = 0
IF NOT keyword_set(rTitOffset) THEN rTitOffset = 0
IF NOT keyword_set(cTitSize) THEN cTitSize = 1
IF NOT keyword_set(rTitSize) THEN rTitSize = 1

;;; By default, show the right-most tick labels

IF NOT keyword_set(showRightTickName) THEN BEGIN
   showRightTickName = 0
   IF n_elements(showLeftTickName) EQ 0 THEN showLeftTickName = 1
ENDIF ELSE BEGIN
    IF n_elements(showLeftTickName) EQ 0 THEN showLeftTickName = 0
ENDELSE

;;; By default, show the top-most tick labels

IF NOT keyword_set(showTopTickName) THEN BEGIN
   showTopTickName = 0
   IF n_elements(showBottomTickName) EQ 0 THEN showBottomTickName = 1
ENDIF ELSE BEGIN
   IF n_elements(showBottomTickName) EQ 0 THEN showBottomTickName = 0
ENDELSE

;;; Define range keywords to avoid errors later

IF NOT keyword_set(xrange) THEN xrange = [0, 0]
IF NOT keyword_set(yrange) THEN yrange = [0, 0]

;;; Set the total number of plots on page, at least 1

ncols = !p.multi[1]
nrows = !p.multi[2]
IF ncols LE 0 THEN ncols = 1
IF nrows LE 0 THEN nrows = 1
nplots = ncols * nrows

;;; Number of plots remaining

nrem = !p.multi[0]
IF nrem LE 0 THEN nrem = nplots

;;; (row, col) index of the next plot, where bottom left is (1, 1)

IF NOT keyword_set(row) THEN row = (nrem-1) / ncols + 1
IF NOT keyword_set(col) THEN BEGIN
    col = nrem MOD ncols
    IF col EQ 0 THEN col = ncols
    col = ncols - col + 1
ENDIF

;;; Character size in normalized coordinates

xCharSizeNorm = float(!d.x_ch_size) / float(!d.x_size)
yCharSizeNorm = float(!d.y_ch_size) / float(!d.y_size)

;;; Size of the current sub-plot in normalized coordinates

dx = (1. - XCharSizeNorm * total(MXMargin)) / float(ncols)
dy = (1. - YCharSizeNorm * total(MYMargin)) / float(nrows)

;;; Set the data window boundaries

xBot = (col - 1) * dx + xCharSizeNorm * mxMargin[0]
yBot = (row - 1) * dy + yCharSizeNorm * myMargin[0]
xTop = xBot + dx
yTop = yBot + dy
IF keyword_set(square) THEN BEGIN
   mplot_square_coords, xBot, yBot, type = square-1
   mplot_square_coords, xTop, yTop, type = square-1
ENDIF
IF NOT keyword_set(position) THEN position = [xBot, yBot, xTop, yTop]
;print, position
;;; Call the plot routine

IF NOT keyword_set(noPlot) THEN BEGIN

;;; Suppress tick labels for now

   oldXTickName = !x.tickname
   oldYTickName = !y.tickname
   !x.tickname = ' '
   !y.tickname = ' '
   
;;; Draw the plot, paying attention to the number of arguments
   
    IF n_elements(y) GT 0 THEN BEGIN 
      
        print,'hello world'
        ;print,xtickv
        ;print,ytickv
        ;if xtickv[0] eq 0 then delvarx, xtickv, xticks
        ;if ytickv[0] eq 0 then delvarx, ytickv, yticks
        if 1 then begin
            plot, x, y, xtick_get=xtickv, ytick_get=ytickv, position=position, _extra=extra
        endif else begin
            plot, x, y, $
                charSize = charSize, $
                position = position, $
                xCharSize = xCharSize, $
                yCharSize = yCharSize, $
                xTicks = xTicks, $
                yTicks = yTicks, $
                xTickV = xTickV, $
                yTickV = yTickV, $
                xTick_Get = xTickV, $
                yTick_Get = yTickV, $
                xRange = xRange, $
                yRange = yRange, $
                _extra = extra
        endelse

   ENDIF ELSE BEGIN
      
      plot, x, $
        charSize = charSize, $
        position = position, $
        xCharSize = xCharSize, $
        yCharSize = yCharSize, $
        xTicks = xTicks, $
        yTicks = yTicks, $
        xTickV = xTickV, $
        yTickV = yTickV, $
        xTick_Get = xTickV, $
        yTick_Get = yTickV, $
        xRange = xRange, $
        yRange = yRange, $
        _extra = extra
      
   ENDELSE 
   
   !x.tickname = xTickName
   !y.tickname = yTickName
   
   xtick0 = 0
   ytick0 = 0
   xtick1 = n_elements(xTickV) - 1
   ytick1 = n_elements(yTickV) - 1
   
   IF col NE 1 AND showLeftTickName EQ 0 THEN xtick0 = xtick0 + 1
   IF row NE 1 AND showBottomTickName EQ 0 THEN ytick0 = ytick0 + 1
   
   IF col NE ncols AND showRightTickName EQ 0 THEN xtick1 = xtick1 - 1
   IF row NE nrows AND showTopTickName EQ 0 THEN ytick1 = ytick1 - 1
   
   xTickV = xTickV[xtick0:xtick1]
   yTickV = yTickV[ytick0:ytick1]
   
   nxticks = n_elements(xTickV)
   nyticks = n_elements(yTickV)
   
   IF keyword_set(noTickLabels) THEN BEGIN
      !X.tickname = replicate(' ', nxticks+1)
      !Y.tickname = replicate(' ', nyticks+1)
   ENDIF
   
;;; Draw the bottom and left tick labels and titles
   
   IF row EQ 1 OR keyword_set(forceXTickNames) THEN BEGIN

      axis, $
        xAxis = 0, $
        xTickV = xTickV, $
        xTicks = nxticks-1, $
        charSize = charSize, $
        xCharSize = xCharSize, $
        xTickFormat = xTickFormat, $
        _extra = extra

      xTickName = " "
      FOR i=1, xTitOffSet-1 DO BEGIN
         xTickName = xTickName + " "
      ENDFOR 
      xTickName = replicate(xTickName, 2)
      axis, $
        xAxis = 0, $
        xTitle = xTitle, $
        charSize = xTitCharSize, $
        xTickFormat = '(a1)', $
        _extra = extra
;        xTicks = 1, $
;        xTickName = xTickName, $
       
  ENDIF 
   
   IF col EQ 1 OR keyword_set(forceYTickNames) THEN BEGIN 

      axis, $
        yAxis = 0, $
        yTickV = yTickV, $
        yTicks = nyticks-1, $
        charSize = charSize, $
        yCharSize = yCharSize, $
        yTickFormat = yTickFormat, $
        _extra = extra
      
      yTickName = " "
      FOR i=1, yTitOffSet-1 DO BEGIN
         yTickName = yTickName + " "
      ENDFOR 
      yTickName = replicate(yTickName, 2)

      axis, $
        yAxis = 0, $
        yTitle = yTitle, $
        charSize = yTitCharSize, $
        _extra = extra, $
        yTicks = 1, $
        yTickName = yTickName
;        yTickFormat = '(a1)', $

     ENDIF 
   
;;; Draw top and right tick labels
   
   IF keyword_set(topTickNames) THEN BEGIN
      IF row EQ nrows OR keyword_set(forceTopTickNames) THEN BEGIN
         axis, $
           xAxis = 1, $
           xTickV = xTickV, $
           xTicks = nxticks, $
           charSize = charSize, $
           xCharSize = xCharSize, $
           xTickFormat = xTickFormat, $
           _extra = extra
      ENDIF 
   ENDIF 
   
   IF keyword_set(rightTickNames) THEN BEGIN 
      IF col EQ ncols OR keyword_set(forceRightTickNames) THEN BEGIN
         axis, $
           yAxis = 1, $
           yTickV = yTickV, $
           yTicks = nyticks, $
           charSize = charSize, $
           yCharSize = yCharSize, $
           yTickFormat = yTickFormat, $
           _extra = extra
      ENDIF 
   ENDIF 
   
;;; Restore the munged system variables to their old values
   
   !X.tickname = OldXTickName
   !Y.tickname = OldYTickName
   
ENDIF

;;; Make the main plot title

IF keyword_set(mTitle) THEN BEGIN

   xTitlePos = 0.5 * dx * ncols + xCharSizeNorm * mxMargin[0]
   yTitlePos = 1. - (myMargin[1] - 1. - MTitOffset) * yCharSizeNorm 

   IF keyword_set(square) THEN $
     mplot_square_coords, xTitlePos, yTitlePos, type = square-1

   xyouts, $
     xTitlePos, $
     yTitlePos, $
     mTitle, $
     /normal, $
     align = 0.5, $
     charsize = 1.25 * mTitSize * charSize

ENDIF

;;; Make main X-axis title

IF keyword_set(mxTitle) THEN BEGIN

   xTitlePos = 0.5 * dx * ncols + xCharSizeNorm * mxMargin[0]
   yTitlePos = yCharSizeNorm * (myMargin[0] - 3. - mxTitOffset)
   
   IF keyword_set(square) THEN $
     mplot_square_coords, xTitlePos, yTitlePos, type = square-1

   xyouts, $
     xTitlePos, $
     yTitlePos, $
     mxTitle, $
     /normal, $
     align = 0.5, $
     charSize = xCharSize * mxTitSize * charSize

ENDIF

;;; Make main Y-axis title

IF keyword_set(myTitle) THEN BEGIN
   
   xTitlePos = xCharSizeNorm * (mxMargin[0] - 6. - myTitOffset)
   yTitlePos = 0.5 * dy * nrows + yCharSizeNorm * myMargin[0]
   
   IF keyword_set(square) THEN $
     mplot_square_coords, xTitlePos, yTitlePos, type = square-1
   
   xyouts, $
     xTitlePos, $
     yTitlePos, $
     myTitle, $
     /normal, $
     align = 0.5, $
     charSize = yCharSize * myTitSize * charSize, $
     orientation = 90
   
ENDIF

;;; Make column heading

IF keyword_set(cTitle) THEN BEGIN

   xTitlePos = 0.5 * (xBot + xTop)
   yTitlePos = 1. - (MYMargin[1] - 0.75 - cTitOffset) * yCharSizeNorm
   
   IF keyword_set(square) THEN $
     mplot_square_coords, xTitlePos, yTitlePos, type = square-1, /yOnly
   
   xyouts, $
     xTitlePos, $
     yTitlePos, $
     cTitle, $
     /normal, $
     align = 0.5, $
     charSize = cTitSize * charSize
   
ENDIF

;;; Make row heading

IF keyword_set(rTitle) THEN BEGIN

   xTitlePos = xCharSizeNorm * (mxMargin[0] - 5. - rTitOffset)
   YTitlePos = 0.5 * (YBot + YTop)
   
   IF keyword_set(square) THEN $
     mplot_square_coords, xTitlePos, yTitlePos, type = square-1, /xOnly
   
   xyouts, $
     xTitlePos, $
     yTitlePos, $
     rTitle, $
     /normal, $
     align = 0.5, $
     charSize = rTitSize * CharSize, $
     orientation = 90
   
ENDIF

END

