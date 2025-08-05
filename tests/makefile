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
TD = diff

TESTS = test_dp54 test_rkf45 test_rk4 test_rich test_short_b test_statuscodes

EXE_TARGETS = rkf45 rk4 short_b rich dp54 seqt statuscodes

all : $(EXE_TARGETS)


####################################################################################################
statuscodes : statuscodes.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) statuscodes.f90 $(MRKISS_OBJ_FILES) -o $@

statuscodes.out &: statuscodes
	statuscodes$(EXE_SUFFIX)

test_statuscodes : statuscodes.out
	$(TD) statuscodes.out data/statuscodes.out

####################################################################################################
short_b : short_b.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) short_b.f90 $(MRKISS_OBJ_FILES) -o $@

short_b_all.out short_b_sub.out &: short_b
	short_b$(EXE_SUFFIX)

test_short_b_all_vs_sub : short_b_all.out short_b_sub.out
	$(FD) -b -e 1e-10 short_b_all.out short_b_sub.out

test_short_b_sub_vs_arc : short_b_sub.out
	$(FD) -b -e 1e-10 short_b_sub.out data/short_b_sub.out

test_short_b : test_short_b_sub_vs_arc test_short_b_all_vs_sub

####################################################################################################
dp54 : dp54.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) dp54.f90 $(MRKISS_OBJ_FILES) -o $@

DP54_OUTS = dp54_stab_5.out dp54_stab_4.out dp54_ref_5.out dp54_ref_4.out dp54_etab_5.out dp54_etab_4.out

$(DP54_OUTS) &: dp54
	dp54$(EXE_SUFFIX)

.PHONY: test_dp54_ref_vs_stab_5
test_dp54_ref_vs_stab_5 : dp54_ref_5.out dp54_stab_5.out
	$(FD) -b -e 1e-7 dp54_ref_5.out dp54_stab_5.out

.PHONY: test_dp54_ref_vs_stab_4
test_dp54_ref_vs_stab_4 : dp54_ref_4.out dp54_stab_4.out
	$(FD) -b -e 1e-7 dp54_ref_4.out dp54_stab_4.out

.PHONY: test_dp54_stab_vs_etab_5
test_dp54_stab_vs_etab_5 : dp54_stab_5.out dp54_etab_5.out
	$(FD) -b -e 1e-10 dp54_stab_5.out dp54_etab_5.out

.PHONY: test_dp54_stab_vs_etab_4
test_dp54_stab_vs_etab_4 : dp54_stab_4.out dp54_etab_4.out
	$(FD) -b -e 1e-10 dp54_stab_4.out dp54_etab_4.out

.PHONY: test_dp54_archive
test_dp54_archive: $(DP54_OUTS)
	$(foreach fil,$^,$(FD) -b $(fil) data/$(fil); )

.PHONY: test_dp54
test_dp54 : test_dp54_ref_vs_stab_5 test_dp54_ref_vs_stab_4 test_dp54_stab_vs_etab_5 test_dp54_stab_vs_etab_4 test_dp54_archive

####################################################################################################
rkf45 : rkf45.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) rkf45.f90 $(MRKISS_OBJ_FILES) -o $@

RKF45_OUTS = rkf45_stab_5.out rkf45_stab_4.out rkf45_ref_5.out rkf45_ref_4.out rkf45_etab_5.out rkf45_etab_4.out

$(RKF45_OUTS) &: rkf45
	rkf45$(EXE_SUFFIX)

.PHONY: test_rkf45_ref_vs_stab_5
test_rkf45_ref_vs_stab_5 : rkf45_ref_5.out rkf45_stab_5.out
	$(FD) -b -e 1e-7 rkf45_ref_5.out rkf45_stab_5.out

.PHONY: test_rkf45_ref_vs_stab_4
test_rkf45_ref_vs_stab_4 : rkf45_ref_4.out rkf45_stab_4.out
	$(FD) -b -e 1e-7 rkf45_ref_4.out rkf45_stab_4.out

.PHONY: test_rkf45_stab_vs_etab_5
test_rkf45_stab_vs_etab_5 : rkf45_stab_5.out rkf45_etab_5.out
	$(FD) -b -e 1e-10 rkf45_stab_5.out rkf45_etab_5.out

.PHONY: test_rkf45_stab_vs_etab_4
test_rkf45_stab_vs_etab_4 : rkf45_stab_4.out rkf45_etab_4.out
	$(FD) -b -e 1e-10 rkf45_stab_4.out rkf45_etab_4.out

.PHONY: test_rkf45_archive
test_rkf45_archive: $(RKF45_OUTS)
	$(foreach fil,$^,$(FD) -b $(fil) data/$(fil); )

.PHONY: test_rkf45
test_rkf45 : test_rkf45_ref_vs_stab_5 test_rkf45_ref_vs_stab_4 test_rkf45_stab_vs_etab_5 test_rkf45_stab_vs_etab_4 test_rkf45_archive

####################################################################################################
rk4 : rk4.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) rk4.f90 $(MRKISS_OBJ_FILES) -o $@

RK4_OUTS = rk4_steps.out rk4_hnd.out rk4_stab.out rk4_ref.out rk4_frog.out rk4_frog_end.out rk4_steps_end.out

$(RK4_OUTS) &: rk4
	rk4$(EXE_SUFFIX)

.PHONY: test_rk4_stab_vs_steps
test_rk4_stab_vs_steps : rk4_steps.out rk4_stab.out 
	$(FD) -b -e 1e-10 rk4_steps.out rk4_stab.out 

.PHONY: test_rk4_ref_vs_stab
test_rk4_ref_vs_stab : rk4_ref.out rk4_stab.out 
	$(FD) -b -e 1e-10 rk4_ref.out rk4_stab.out 

.PHONY: test_rk4_ref_vs_hnd
test_rk4_ref_vs_hnd : rk4_ref.out rk4_hnd.out 
	$(FD) -b -e 1e-6 rk4_ref.out rk4_hnd.out 

.PHONY: test_rk4_frog_d_vs_e
test_rk4_frog_d_vs_e : rk4_frog.out rk4_frog_end.out
	$(FD) -b -e 1e-10 rk4_frog.out rk4_frog_end.out

.PHONY: test_rk4_frog_vs_steps
test_rk4_frog_vs_steps : rk4_frog.out rk4_steps.out 
	$(FD) -b -e 1e-10 -r '^ +11 ' rk4_frog.out rk4_steps.out 

.PHONY: test_rk4_steps_d_vs_e
test_rk4_steps_d_vs_e : rk4_steps.out rk4_steps_end.out
	$(FD) -b -e 1e-10 rk4_steps.out rk4_steps_end.out

.PHONY: test_rk4_archive
test_rk4_archive: $(RK4_OUTS)
	$(foreach fil,$^,$(FD) -b $(fil) data/$(fil); )

.PHONY: test_rk4
test_rk4 : test_rk4_stab_vs_steps test_rk4_ref_vs_stab test_rk4_ref_vs_hnd test_rk4_frog_d_vs_e test_rk4_steps_d_vs_e test_rk4_frog_vs_steps test_rk4_archive 

####################################################################################################
rich : rich.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) rich.f90 $(MRKISS_OBJ_FILES) -o $@

rich.out : rich
	./rich$(EXE_SUFFIX)

.PHONY: test_rich
test_rich : rich.out 
	$(FD) -b -e 1e-10 rich.out data/rich.out

rich_sol.png rich_error.png &: rich.R rich.out
	R --no-save --no-echo -q -f rich.R

.PHONY: rich_png
rich_png: rich_sol.png rich_error.png
	$(IMG_VIEWER) rich_sol.png rich_error.png

####################################################################################################

tc1.mk : tc_make_make.rb tc1_template.f90
	ruby tc_make_make.rb tc1_template.f90

include tc1.mk

tc1_plot_ferror.png tc1_plot_error.png tc1_plot_sol.png &: tc1.R data_tc1
	R --no-save --no-echo -q -f tc1.R

.PHONY: tc1_png
tc1_png : tc1_plot_ferror.png tc1_plot_error.png tc1_plot_sol.png 
	$(IMG_VIEWER) tc1_plot_ferror.png tc1_plot_error.png tc1_plot_sol.png

####################################################################################################

tc2.mk : tc_make_make.rb tc2_template.f90
	ruby tc_make_make.rb tc2_template.f90

include tc2.mk

tc2_plot_error.png tc2_plot_sol.png &: tc2.R data_tc2
	R --no-save --no-echo -q -f tc2.R

.PHONY: tc2_png
tc2_png : tc2_plot_error.png tc2_plot_sol.png
	$(IMG_VIEWER) tc2_plot_error.png tc2_plot_sol.png

####################################################################################################
seqt : seqt.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) seqt.f90 $(MRKISS_OBJ_FILES) -o $@

seqt.out : seqt
	./seqt$(EXE_SUFFIX)

.PHONY: test_seqt
test_seqt : seqt.out 
	$(FD) -b -e 1e-10 seqt.out data/seqt.out

####################################################################################################
.PHONY: tests
tests : $(TESTS) test_tc1 test_tc2

.PHONY: clean
clean : clean_multi_mrkiss clean_tc1 clean_tc2
	rm -f $(addsuffix .exe,$(EXE_TARGETS)) $(EXE_TARGETS) *.csv *.png *~ a.out a.exe *.out *.tmp

.PHONY: clean_gen_source
clean_src : clean_src_tc1 clean_src_tc2
