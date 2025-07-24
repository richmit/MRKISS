# -*- Mode:makefile; Coding:us-ascii-unix; fill-column:158 -*-
#########################################################################################################################################################.H.S.##
##
# @file      makefile
# @author    Mitch Richling http://www.mitchr.me/
# @brief     Make MRKISS tests.@EOL
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

FD = ../scripts/float_diff.rb

TESTS = test_rkf45_stab_5 test_rkf45_etab_5 test_rkf45_stab_4 test_rkf45_etab_4 test_rk4_stab test_rk4_ref test_rk4_ref_stab test_rk4_steps

EXE_TARGETS = rkf45_ref_vs_tab rk4_ref_vs_tab

all : $(EXE_TARGETS)

####################################################################################################
rkf45_ref_vs_tab : rkf45_ref_vs_tab.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) rkf45_ref_vs_tab.f90 $(MRKISS_OBJ_FILES) -o $@

short_b : short_b.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) short_b.f90 $(MRKISS_OBJ_FILES) -o $@

short_b_all.out short_b_sub.out &: short_b
	short_b$(EXE_SUFFIX)

test_short_b : short_b_all.out short_b_sub.out
	$(FD) -e 1e-10 short_b_all.out short_b_sub.out

rkf45_ref_vs_tab_stab_5.out rkf45_ref_vs_tab_stab_4.out rkf45_ref_vs_tab_ref_5.out rkf45_ref_vs_tab_ref_4.out rkf45_ref_vs_tab_etab_5.out rkf45_ref_vs_tab_etab_4.out &: rkf45_ref_vs_tab
	rkf45_ref_vs_tab$(EXE_SUFFIX)

.PHONY: test_rkf45_stab_5
test_rkf45_stab_5 : rkf45_ref_vs_tab_ref_5.out rkf45_ref_vs_tab_stab_5.out
	$(FD) -e 1e-10 rkf45_ref_vs_tab_ref_5.out rkf45_ref_vs_tab_stab_5.out

.PHONY: test_rkf45_etab_5
test_rkf45_etab_5 : rkf45_ref_vs_tab_ref_5.out rkf45_ref_vs_tab_etab_5.out
	$(FD) -e 1e-10 rkf45_ref_vs_tab_ref_5.out rkf45_ref_vs_tab_etab_5.out

.PHONY: test_rkf45_etab_4
test_rkf45_stab_4 : rkf45_ref_vs_tab_ref_4.out rkf45_ref_vs_tab_stab_4.out
	$(FD) -e 1e-10 rkf45_ref_vs_tab_ref_4.out rkf45_ref_vs_tab_stab_4.out

.PHONY: test_rk4_steps
test_rkf45_etab_4 : rkf45_ref_vs_tab_ref_4.out rkf45_ref_vs_tab_etab_4.out
	$(FD) -e 1e-10 rkf45_ref_vs_tab_ref_4.out rkf45_ref_vs_tab_etab_4.out

####################################################################################################
rk4_ref_vs_tab : rk4_ref_vs_tab.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) rk4_ref_vs_tab.f90 $(MRKISS_OBJ_FILES) -o $@

rk4_ref_vs_tab_hnd.out rk4_ref_vs_tab_stab.out rk4_ref_vs_tab_ref.out &: rk4_ref_vs_tab
	rk4_ref_vs_tab$(EXE_SUFFIX)

.PHONY: test_rk4_steps
test_rk4_steps : rk4_ref_vs_tab_steps.out rk4_ref_vs_tab_stab.out 
	sed "s/^              //" rk4_ref_vs_tab_steps.out > rk4_ref_vs_tab_steps.tmp
	$(FD) -e 1e-10 rk4_ref_vs_tab_steps.tmp rk4_ref_vs_tab_stab.out 

.PHONY: test_rk4_stab
test_rk4_stab : rk4_ref_vs_tab_hnd.out rk4_ref_vs_tab_stab.out
	$(FD) -e 1e-6 rk4_ref_vs_tab_hnd.out rk4_ref_vs_tab_stab.out

.PHONY: test_rk4_ref
test_rk4_ref : rk4_ref_vs_tab_hnd.out rk4_ref_vs_tab_ref.out
	$(FD) -e 1e-6 rk4_ref_vs_tab_hnd.out rk4_ref_vs_tab_ref.out

.PHONY: test_rk4_ref_stab
test_rk4_ref_stab : rk4_ref_vs_tab_stab.out rk4_ref_vs_tab_ref.out
	$(FD) -e 1e-10 rk4_ref_vs_tab_stab.out rk4_ref_vs_tab_ref.out

####################################################################################################

tc1.mk : tc_make_make.rb tc1_template.f90
	ruby tc_make_make.rb tc1_template.f90

include tc1.mk

tc1_plot_error.png tc1_plot_sol.png &: tc1.R data_tc1
	R --no-save --no-echo -q -f tc1.R
	$(IMG_VIEWER) tc1_plot_error.png tc1_plot_sol.png

####################################################################################################

tc2.mk : tc_make_make.rb tc2_template.f90
	ruby tc_make_make.rb tc2_template.f90

include tc2.mk

tc2_plot_error.png tc2_plot_sol.png &: tc2.R data_tc2
	R --no-save --no-echo -q -f tc2.R
	$(IMG_VIEWER) tc2_plot_error.png tc2_plot_sol.png

####################################################################################################
.PHONY: tests
tests : $(TESTS) test_tc1 test_tc2

.PHONY: clean
clean : clean_multi_mrkiss clean_tc1 clean_tc2
	rm -f $(addsuffix .exe,$(EXE_TARGETS)) $(EXE_TARGETS) *.csv *.png *~ a.out a.exe *.out *.tmp

.PHONY: clean_gen_source
clean_src : clean_src_tc1 clean_src_tc2
