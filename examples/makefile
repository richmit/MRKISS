# -*- Mode:makefile; Coding:us-ascii-unix; fill-column:158 -*-
#########################################################################################################################################################.H.S.##
##
# @file      makefile
# @author    Mitch Richling http://www.mitchr.me/
# @brief     Build MRKISS examples.@EOL
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

################################################################################################################################################################
MRKISS_PATH = ..

 include $(MRKISS_PATH)/make_includes/tools_gfortran.mk
# include $(MRKISS_PATH)/make_includes/tools_flang.mk
# include $(MRKISS_PATH)/make_includes/tools_ifx.mk
# include $(MRKISS_PATH)/make_includes/tools_lfortran.mk
# include $(MRKISS_PATH)/make_includes/tools_nvfortran.mk

include $(MRKISS_PATH)/make_includes/include.mk
################################################################################################################################################################

IMG_VIEWER = nomacs

EXE_TARGETS = three_body lorenz minimal brusselator

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
three_body : three_body.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) three_body.f90 $(MRKISS_OBJ_FILES) -o $@

three_body_steps_adapt_etab_wt-fix-delta-steps.csv three_body_steps_adapt_etab_wt-pho-t-max.csv three_body_steps_adapt_etab_wt-isct.csv three_body_steps_adapt_etab_wt-std.csv three_body_steps_condy_stab_wt.csv tree_body_steps_fixed_stab_wt.csv &: three_body
	./three_body$(EXE_SUFFIX)
	ls -l three_body_*.csv

three_body_unfixed_pos.png three_body.png three_body_fixed_pos.png three_body_fixed_vel.png three_body_adaptive_maxt.png three_body_ylen.png three_body_moon.png &: three_body_steps_adapt_etab_wt-fix-delta-steps.csv three_body_steps_adapt_etab_wt-pho-t-max.csv three_body_steps_adapt_etab_wt-isct.csv three_body_steps_adapt_etab_wt-std.csv three_body_steps_condy_stab_wt.csv tree_body_steps_fixed_stab_wt.csv three_body.R
	R --no-save --no-echo -q -f three_body.R
	$(IMG_VIEWER) three_body.png three_body_steps_adapt_etab_wt-pho-t-max.csv three_body_ylen.png three_body_moon.png three_body_fixed_pos.png three_body_fixed_vel.png three_body_unfixed_pos.png &

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
lorenz : lorenz.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) lorenz.f90 $(MRKISS_OBJ_FILES) -o $@

lorenz_fixed.csv lorenz_condy.csv lorenz_sloppy_condy.csv lorenz_sloppy_condy_short.csv &: lorenz
	./lorenz$(EXE_SUFFIX)

lorenz_sloppy_condy_short.svg lorenz_sloppy_condy.svg lorenz_fixed.svg lorenz_condy.svg &: lorenz_condy.csv lorenz_sloppy_condy_short.csv lorenz_sloppy_condy.csv lorenz_fixed.csv lorenz.gplt
	gnuplot -p < lorenz.gplt

lorenz_sloppy_condy_short.png : lorenz_sloppy_condy_short.svg
	magick -density 200 -background white lorenz_sloppy_condy_short.svg lorenz_sloppy_condy_short.png

lorenz_sloppy_condy.png : lorenz_sloppy_condy.svg
	magick -density 200 -background white lorenz_sloppy_condy.svg lorenz_sloppy_condy.png

lorenz_fixed.png : lorenz_fixed.svg
	magick -density 200 -background white lorenz_fixed.svg lorenz_fixed.png

lorenz_fixed-c.png : lorenz_fixed.svg
	magick -density 600 -background white lorenz_fixed.svg -crop 1000x800+0+200 lorenz_fixed-c.png

lorenz_condy.png : lorenz_condy.svg
	magick -density 200 -background white lorenz_condy.svg lorenz_condy.png

lorenz_png : lorenz_sloppy_condy_short.png lorenz_sloppy_condy.png lorenz_fixed.png lorenz_condy.png lorenz_fixed-c.png
	$(IMG_VIEWER) lorenz_fixed.png lorenz_sloppy_condy_short.png lorenz_sloppy_condy.png lorenz_condy.png lorenz_fixed-c.png

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
brusselator : brusselator.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) brusselator.f90 $(MRKISS_OBJ_FILES) -o $@

brusselator.csv : brusselator
	./brusselator$(EXE_SUFFIX)
	head -n 10 brusselator.csv
	tail -n 10 brusselator.csv

brusselator.png : brusselator.csv
	R --no-save --no-echo -q -f brusselator.R
	$(IMG_VIEWER) brusselator.png &

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
minimal : minimal.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) minimal.f90 $(MRKISS_OBJ_FILES) -o $@

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
clean : clean_multi_mrkiss
	rm -f $(addsuffix .exe,$(EXE_TARGETS)) $(EXE_TARGETS) *.csv *.png *~ a.out a.exe
