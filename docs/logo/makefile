# -*- Mode:make; Coding:us-ascii-unix; fill-column:158 -*-
#########################################################################################################################################################.H.S.##
##
# @file      makefile
# @author    Mitch Richling http://www.mitchr.me/
# @brief     Create MRKISS logo.@EOL
# @std       GNUmake
# @see       https://github.com/richmit/MRKISS/
# @copyright 
#  @parblock
#  Copyright (c) 2025, Mitchell Jay Richling <http://www.mitchr.me/> All rights reserved.
#  
#  Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
#  
#  1. Redistributions of source code must retain the above copyright notice, this list of conditions, and the following disclaimer.
#  
#  2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions, and the following disclaimer in the documentation
#     and/or other materials provided with the distribution.
#  
#  3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software
#     without specific prior written permission.
#  
#  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
#  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
#  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
#  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
#  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
#  DAMAGE.
#  @endparblock
#########################################################################################################################################################.H.E.##

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
IMAGES = MRKISS_logo_m_1000x500.png MRKISS_logo_m_500x250.png MRKISS_logo_m_250x125.png MRKISS_logo_c_1000x500.png MRKISS_logo_c_500x250.png MRKISS_logo_c_250x125.png MRKISS_logo_m_100x50.png MRKISS_logo_c_100x50.png MRKISS_logo_bg_1200x600.png MRKISS_logo_bg_120x60.png MRKISS_logo_bg_300x150.png MRKISS_logo_bg_600x300.png

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
MRKISS_logo.pdf : MRKISS_logo.tex
	pdflatex MRKISS_logo.tex

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
MRKISS_logo_m_1000x500.png : MRKISS_logo.pdf
	gswin32c.exe -q -dNOPAUSE -dBATCH -sDEVICE=pnggray -g1000x500 -dPDFFitPage -dUseCropBox -sOutputFile=MRKISS_logo_m_1000x500.png MRKISS_logo.pdf

MRKISS_logo_m_500x250.png : MRKISS_logo.pdf
	gswin32c.exe -q -dNOPAUSE -dBATCH -sDEVICE=pnggray -g500x250 -dPDFFitPage -dUseCropBox -sOutputFile=MRKISS_logo_m_500x250.png MRKISS_logo.pdf

MRKISS_logo_m_250x125.png : MRKISS_logo.pdf
	gswin32c.exe -q -dNOPAUSE -dBATCH -sDEVICE=pnggray -g250x125 -dPDFFitPage -dUseCropBox -sOutputFile=MRKISS_logo_m_250x125.png MRKISS_logo.pdf

MRKISS_logo_m_100x50.png : MRKISS_logo.pdf
	gswin32c.exe -q -dNOPAUSE -dBATCH -sDEVICE=pnggray -g100x50 -dPDFFitPage -dUseCropBox -sOutputFile=MRKISS_logo_m_100x50.png MRKISS_logo.pdf

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
MRKISS_logo_c_1000x500.png : MRKISS_logo.pdf
	gswin32c.exe -q -dNOPAUSE -dBATCH -sDEVICE=png16m -g1000x500 -dPDFFitPage -dUseCropBox -sOutputFile=MRKISS_logo_c_1000x500.png MRKISS_logo.pdf

MRKISS_logo_c_500x250.png : MRKISS_logo.pdf
	gswin32c.exe -q -dNOPAUSE -dBATCH -sDEVICE=png16m -g500x250 -dPDFFitPage -dUseCropBox -sOutputFile=MRKISS_logo_c_500x250.png MRKISS_logo.pdf

MRKISS_logo_c_250x125.png : MRKISS_logo.pdf
	gswin32c.exe -q -dNOPAUSE -dBATCH -sDEVICE=png16m -g250x125 -dPDFFitPage -dUseCropBox -sOutputFile=MRKISS_logo_c_250x125.png MRKISS_logo.pdf

MRKISS_logo_c_100x50.png : MRKISS_logo.pdf
	gswin32c.exe -q -dNOPAUSE -dBATCH -sDEVICE=png16m -g100x50 -dPDFFitPage -dUseCropBox -sOutputFile=MRKISS_logo_c_100x50.png MRKISS_logo.pdf

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
MRKISS_logo_bg_1200x600.png : MRKISS_logo.pdf
	magick logo_background.png -resize 1200x600 MRKISS_logo_bg_1000x500.png

MRKISS_logo_bg_600x300.png : MRKISS_logo.pdf
	magick logo_background.png -resize 600x300 MRKISS_logo_bg_600x300.png

MRKISS_logo_bg_300x150.png : MRKISS_logo.pdf
	magick logo_background.png -resize 300x150 MRKISS_logo_bg_300x150.png

MRKISS_logo_bg_120x60.png : MRKISS_logo.pdf
	magick logo_background.png -resize 120x60 MRKISS_logo_bg_120x60.png

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
view : $(IMAGES)
	nomacs $(IMAGES)

all : $(IMAGES)

clean : 
	rm -f $(IMAGES) *.pdf *.log *.aux *~
