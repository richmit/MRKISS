# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Run all tests for tc1
.PHONY: test_tc1
test_tc1 : test_tc1_bogacki_shampine_3_2_b1 test_tc1_bogacki_shampine_3_2_b2 test_tc1_bogacki_shampine_4_5_b1 test_tc1_bogacki_shampine_4_5_b2 test_tc1_cash_karp_5_4_b1 test_tc1_cash_karp_5_4_b2 test_tc1_dormand_prince_5_4_b1 test_tc1_dormand_prince_5_4_b2 test_tc1_dormand_prince_7_8_b1 test_tc1_dormand_prince_7_8_b2 test_tc1_fehlberg_4_5_b1 test_tc1_fehlberg_4_5_b2 test_tc1_fehlberg_7_8_b1 test_tc1_fehlberg_7_8_b2 test_tc1_heun_euler_2_1_b1 test_tc1_heun_euler_2_1_b2 test_tc1_sofroniou_spaletta_4_3_b1 test_tc1_sofroniou_spaletta_4_3_b2 test_tc1_tsitouras_arkode_5_4_b1 test_tc1_tsitouras_arkode_5_4_b2 test_tc1_verner_1978_6_5_b1 test_tc1_verner_1978_6_5_b2 test_tc1_verner_2010_6_5_b1 test_tc1_verner_2010_6_5_b2 test_tc1_verner_7_6_b1 test_tc1_verner_7_6_b2 test_tc1_verner_8_7_b1 test_tc1_verner_8_7_b2 test_tc1_verner_9_8_b1 test_tc1_verner_9_8_b2 test_tc1_euler_1 test_tc1_feagin_10 test_tc1_knoth_wolke_3 test_tc1_kutta_4 test_tc1_kutta_three_eight_4 test_tc1_midpoint_2 test_tc1_ralston_2 test_tc1_ralston_3 test_tc1_ralston_4

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Make source code for tc1
.PHONY: src_tc1
tc1_bogacki_shampine_3_2_b1.f90 tc1_bogacki_shampine_3_2_b2.f90 tc1_bogacki_shampine_4_5_b1.f90 tc1_bogacki_shampine_4_5_b2.f90 tc1_cash_karp_5_4_b1.f90 tc1_cash_karp_5_4_b2.f90 tc1_dormand_prince_5_4_b1.f90 tc1_dormand_prince_5_4_b2.f90 tc1_dormand_prince_7_8_b1.f90 tc1_dormand_prince_7_8_b2.f90 tc1_fehlberg_4_5_b1.f90 tc1_fehlberg_4_5_b2.f90 tc1_fehlberg_7_8_b1.f90 tc1_fehlberg_7_8_b2.f90 tc1_heun_euler_2_1_b1.f90 tc1_heun_euler_2_1_b2.f90 tc1_sofroniou_spaletta_4_3_b1.f90 tc1_sofroniou_spaletta_4_3_b2.f90 tc1_tsitouras_arkode_5_4_b1.f90 tc1_tsitouras_arkode_5_4_b2.f90 tc1_verner_1978_6_5_b1.f90 tc1_verner_1978_6_5_b2.f90 tc1_verner_2010_6_5_b1.f90 tc1_verner_2010_6_5_b2.f90 tc1_verner_7_6_b1.f90 tc1_verner_7_6_b2.f90 tc1_verner_8_7_b1.f90 tc1_verner_8_7_b2.f90 tc1_verner_9_8_b1.f90 tc1_verner_9_8_b2.f90 tc1_euler_1.f90 tc1_feagin_10.f90 tc1_knoth_wolke_3.f90 tc1_kutta_4.f90 tc1_kutta_three_eight_4.f90 tc1_midpoint_2.f90 tc1_ralston_2.f90 tc1_ralston_3.f90 tc1_ralston_4.f90 &: tc1_template.f90
	ruby tc_make_make.rb tc1_template.f90

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Clean source code for tc1
.PHONY: clean_src_tc1
clean_src_tc1 :
	rm -f tc1_bogacki_shampine_3_2_b1.f90 tc1_bogacki_shampine_3_2_b2.f90 tc1_bogacki_shampine_4_5_b1.f90 tc1_bogacki_shampine_4_5_b2.f90 tc1_cash_karp_5_4_b1.f90 tc1_cash_karp_5_4_b2.f90 tc1_dormand_prince_5_4_b1.f90 tc1_dormand_prince_5_4_b2.f90 tc1_dormand_prince_7_8_b1.f90 tc1_dormand_prince_7_8_b2.f90 tc1_fehlberg_4_5_b1.f90 tc1_fehlberg_4_5_b2.f90 tc1_fehlberg_7_8_b1.f90 tc1_fehlberg_7_8_b2.f90 tc1_heun_euler_2_1_b1.f90 tc1_heun_euler_2_1_b2.f90 tc1_sofroniou_spaletta_4_3_b1.f90 tc1_sofroniou_spaletta_4_3_b2.f90 tc1_tsitouras_arkode_5_4_b1.f90 tc1_tsitouras_arkode_5_4_b2.f90 tc1_verner_1978_6_5_b1.f90 tc1_verner_1978_6_5_b2.f90 tc1_verner_2010_6_5_b1.f90 tc1_verner_2010_6_5_b2.f90 tc1_verner_7_6_b1.f90 tc1_verner_7_6_b2.f90 tc1_verner_8_7_b1.f90 tc1_verner_8_7_b2.f90 tc1_verner_9_8_b1.f90 tc1_verner_9_8_b2.f90 tc1_euler_1.f90 tc1_feagin_10.f90 tc1_knoth_wolke_3.f90 tc1_kutta_4.f90 tc1_kutta_three_eight_4.f90 tc1_midpoint_2.f90 tc1_ralston_2.f90 tc1_ralston_3.f90 tc1_ralston_4.f90

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Clean non-source code for tc1
.PHONY: clean_tc1
clean_tc1 :
	rm -f tc1_bogacki_shampine_3_2_b1.out tc1_bogacki_shampine_3_2_b2.out tc1_bogacki_shampine_4_5_b1.out tc1_bogacki_shampine_4_5_b2.out tc1_cash_karp_5_4_b1.out tc1_cash_karp_5_4_b2.out tc1_dormand_prince_5_4_b1.out tc1_dormand_prince_5_4_b2.out tc1_dormand_prince_7_8_b1.out tc1_dormand_prince_7_8_b2.out tc1_fehlberg_4_5_b1.out tc1_fehlberg_4_5_b2.out tc1_fehlberg_7_8_b1.out tc1_fehlberg_7_8_b2.out tc1_heun_euler_2_1_b1.out tc1_heun_euler_2_1_b2.out tc1_sofroniou_spaletta_4_3_b1.out tc1_sofroniou_spaletta_4_3_b2.out tc1_tsitouras_arkode_5_4_b1.out tc1_tsitouras_arkode_5_4_b2.out tc1_verner_1978_6_5_b1.out tc1_verner_1978_6_5_b2.out tc1_verner_2010_6_5_b1.out tc1_verner_2010_6_5_b2.out tc1_verner_7_6_b1.out tc1_verner_7_6_b2.out tc1_verner_8_7_b1.out tc1_verner_8_7_b2.out tc1_verner_9_8_b1.out tc1_verner_9_8_b2.out tc1_euler_1.out tc1_feagin_10.out tc1_knoth_wolke_3.out tc1_kutta_4.out tc1_kutta_three_eight_4.out tc1_midpoint_2.out tc1_ralston_2.out tc1_ralston_3.out tc1_ralston_4.out tc1_bogacki_shampine_3_2_b1 tc1_bogacki_shampine_3_2_b2 tc1_bogacki_shampine_4_5_b1 tc1_bogacki_shampine_4_5_b2 tc1_cash_karp_5_4_b1 tc1_cash_karp_5_4_b2 tc1_dormand_prince_5_4_b1 tc1_dormand_prince_5_4_b2 tc1_dormand_prince_7_8_b1 tc1_dormand_prince_7_8_b2 tc1_fehlberg_4_5_b1 tc1_fehlberg_4_5_b2 tc1_fehlberg_7_8_b1 tc1_fehlberg_7_8_b2 tc1_heun_euler_2_1_b1 tc1_heun_euler_2_1_b2 tc1_sofroniou_spaletta_4_3_b1 tc1_sofroniou_spaletta_4_3_b2 tc1_tsitouras_arkode_5_4_b1 tc1_tsitouras_arkode_5_4_b2 tc1_verner_1978_6_5_b1 tc1_verner_1978_6_5_b2 tc1_verner_2010_6_5_b1 tc1_verner_2010_6_5_b2 tc1_verner_7_6_b1 tc1_verner_7_6_b2 tc1_verner_8_7_b1 tc1_verner_8_7_b2 tc1_verner_9_8_b1 tc1_verner_9_8_b2 tc1_euler_1 tc1_feagin_10 tc1_knoth_wolke_3 tc1_kutta_4 tc1_kutta_three_eight_4 tc1_midpoint_2 tc1_ralston_2 tc1_ralston_3 tc1_ralston_4 tc1_bogacki_shampine_3_2_b1$(EXE_SUFFIX) tc1_bogacki_shampine_3_2_b2$(EXE_SUFFIX) tc1_bogacki_shampine_4_5_b1$(EXE_SUFFIX) tc1_bogacki_shampine_4_5_b2$(EXE_SUFFIX) tc1_cash_karp_5_4_b1$(EXE_SUFFIX) tc1_cash_karp_5_4_b2$(EXE_SUFFIX) tc1_dormand_prince_5_4_b1$(EXE_SUFFIX) tc1_dormand_prince_5_4_b2$(EXE_SUFFIX) tc1_dormand_prince_7_8_b1$(EXE_SUFFIX) tc1_dormand_prince_7_8_b2$(EXE_SUFFIX) tc1_fehlberg_4_5_b1$(EXE_SUFFIX) tc1_fehlberg_4_5_b2$(EXE_SUFFIX) tc1_fehlberg_7_8_b1$(EXE_SUFFIX) tc1_fehlberg_7_8_b2$(EXE_SUFFIX) tc1_heun_euler_2_1_b1$(EXE_SUFFIX) tc1_heun_euler_2_1_b2$(EXE_SUFFIX) tc1_sofroniou_spaletta_4_3_b1$(EXE_SUFFIX) tc1_sofroniou_spaletta_4_3_b2$(EXE_SUFFIX) tc1_tsitouras_arkode_5_4_b1$(EXE_SUFFIX) tc1_tsitouras_arkode_5_4_b2$(EXE_SUFFIX) tc1_verner_1978_6_5_b1$(EXE_SUFFIX) tc1_verner_1978_6_5_b2$(EXE_SUFFIX) tc1_verner_2010_6_5_b1$(EXE_SUFFIX) tc1_verner_2010_6_5_b2$(EXE_SUFFIX) tc1_verner_7_6_b1$(EXE_SUFFIX) tc1_verner_7_6_b2$(EXE_SUFFIX) tc1_verner_8_7_b1$(EXE_SUFFIX) tc1_verner_8_7_b2$(EXE_SUFFIX) tc1_verner_9_8_b1$(EXE_SUFFIX) tc1_verner_9_8_b2$(EXE_SUFFIX) tc1_euler_1$(EXE_SUFFIX) tc1_feagin_10$(EXE_SUFFIX) tc1_knoth_wolke_3$(EXE_SUFFIX) tc1_kutta_4$(EXE_SUFFIX) tc1_kutta_three_eight_4$(EXE_SUFFIX) tc1_midpoint_2$(EXE_SUFFIX) tc1_ralston_2$(EXE_SUFFIX) tc1_ralston_3$(EXE_SUFFIX) tc1_ralston_4$(EXE_SUFFIX)

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Make all the data files
.PHONY: data_tc1
data_tc1 : tc1_bogacki_shampine_3_2_b1.out tc1_bogacki_shampine_3_2_b2.out tc1_bogacki_shampine_4_5_b1.out tc1_bogacki_shampine_4_5_b2.out tc1_cash_karp_5_4_b1.out tc1_cash_karp_5_4_b2.out tc1_dormand_prince_5_4_b1.out tc1_dormand_prince_5_4_b2.out tc1_dormand_prince_7_8_b1.out tc1_dormand_prince_7_8_b2.out tc1_fehlberg_4_5_b1.out tc1_fehlberg_4_5_b2.out tc1_fehlberg_7_8_b1.out tc1_fehlberg_7_8_b2.out tc1_heun_euler_2_1_b1.out tc1_heun_euler_2_1_b2.out tc1_sofroniou_spaletta_4_3_b1.out tc1_sofroniou_spaletta_4_3_b2.out tc1_tsitouras_arkode_5_4_b1.out tc1_tsitouras_arkode_5_4_b2.out tc1_verner_1978_6_5_b1.out tc1_verner_1978_6_5_b2.out tc1_verner_2010_6_5_b1.out tc1_verner_2010_6_5_b2.out tc1_verner_7_6_b1.out tc1_verner_7_6_b2.out tc1_verner_8_7_b1.out tc1_verner_8_7_b2.out tc1_verner_9_8_b1.out tc1_verner_9_8_b2.out tc1_euler_1.out tc1_feagin_10.out tc1_knoth_wolke_3.out tc1_kutta_4.out tc1_kutta_three_eight_4.out tc1_midpoint_2.out tc1_ralston_2.out tc1_ralston_3.out tc1_ralston_4.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc1_bogacki_shampine_3_2_b1 : tc1_bogacki_shampine_3_2_b1.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc1_bogacki_shampine_3_2_b1.f90 $(MRKISS_OBJ_FILES) -o $@

tc1_bogacki_shampine_3_2_b1.out : tc1_bogacki_shampine_3_2_b1
	tc1_bogacki_shampine_3_2_b1$(EXE_SUFFIX)

.PHONY: test_bogacki_shampine_3_2_b1
test_tc1_bogacki_shampine_3_2_b1 : tc1_bogacki_shampine_3_2_b1.out
	$(FD) -Zdq -e 1e-10 data/tc1_bogacki_shampine_3_2_b1.out tc1_bogacki_shampine_3_2_b1.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc1_bogacki_shampine_3_2_b2 : tc1_bogacki_shampine_3_2_b2.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc1_bogacki_shampine_3_2_b2.f90 $(MRKISS_OBJ_FILES) -o $@

tc1_bogacki_shampine_3_2_b2.out : tc1_bogacki_shampine_3_2_b2
	tc1_bogacki_shampine_3_2_b2$(EXE_SUFFIX)

.PHONY: test_bogacki_shampine_3_2_b2
test_tc1_bogacki_shampine_3_2_b2 : tc1_bogacki_shampine_3_2_b2.out
	$(FD) -Zdq -e 1e-10 data/tc1_bogacki_shampine_3_2_b2.out tc1_bogacki_shampine_3_2_b2.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc1_bogacki_shampine_4_5_b1 : tc1_bogacki_shampine_4_5_b1.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc1_bogacki_shampine_4_5_b1.f90 $(MRKISS_OBJ_FILES) -o $@

tc1_bogacki_shampine_4_5_b1.out : tc1_bogacki_shampine_4_5_b1
	tc1_bogacki_shampine_4_5_b1$(EXE_SUFFIX)

.PHONY: test_bogacki_shampine_4_5_b1
test_tc1_bogacki_shampine_4_5_b1 : tc1_bogacki_shampine_4_5_b1.out
	$(FD) -Zdq -e 1e-10 data/tc1_bogacki_shampine_4_5_b1.out tc1_bogacki_shampine_4_5_b1.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc1_bogacki_shampine_4_5_b2 : tc1_bogacki_shampine_4_5_b2.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc1_bogacki_shampine_4_5_b2.f90 $(MRKISS_OBJ_FILES) -o $@

tc1_bogacki_shampine_4_5_b2.out : tc1_bogacki_shampine_4_5_b2
	tc1_bogacki_shampine_4_5_b2$(EXE_SUFFIX)

.PHONY: test_bogacki_shampine_4_5_b2
test_tc1_bogacki_shampine_4_5_b2 : tc1_bogacki_shampine_4_5_b2.out
	$(FD) -Zdq -e 1e-10 data/tc1_bogacki_shampine_4_5_b2.out tc1_bogacki_shampine_4_5_b2.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc1_cash_karp_5_4_b1 : tc1_cash_karp_5_4_b1.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc1_cash_karp_5_4_b1.f90 $(MRKISS_OBJ_FILES) -o $@

tc1_cash_karp_5_4_b1.out : tc1_cash_karp_5_4_b1
	tc1_cash_karp_5_4_b1$(EXE_SUFFIX)

.PHONY: test_cash_karp_5_4_b1
test_tc1_cash_karp_5_4_b1 : tc1_cash_karp_5_4_b1.out
	$(FD) -Zdq -e 1e-10 data/tc1_cash_karp_5_4_b1.out tc1_cash_karp_5_4_b1.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc1_cash_karp_5_4_b2 : tc1_cash_karp_5_4_b2.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc1_cash_karp_5_4_b2.f90 $(MRKISS_OBJ_FILES) -o $@

tc1_cash_karp_5_4_b2.out : tc1_cash_karp_5_4_b2
	tc1_cash_karp_5_4_b2$(EXE_SUFFIX)

.PHONY: test_cash_karp_5_4_b2
test_tc1_cash_karp_5_4_b2 : tc1_cash_karp_5_4_b2.out
	$(FD) -Zdq -e 1e-10 data/tc1_cash_karp_5_4_b2.out tc1_cash_karp_5_4_b2.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc1_dormand_prince_5_4_b1 : tc1_dormand_prince_5_4_b1.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc1_dormand_prince_5_4_b1.f90 $(MRKISS_OBJ_FILES) -o $@

tc1_dormand_prince_5_4_b1.out : tc1_dormand_prince_5_4_b1
	tc1_dormand_prince_5_4_b1$(EXE_SUFFIX)

.PHONY: test_dormand_prince_5_4_b1
test_tc1_dormand_prince_5_4_b1 : tc1_dormand_prince_5_4_b1.out
	$(FD) -Zdq -e 1e-10 data/tc1_dormand_prince_5_4_b1.out tc1_dormand_prince_5_4_b1.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc1_dormand_prince_5_4_b2 : tc1_dormand_prince_5_4_b2.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc1_dormand_prince_5_4_b2.f90 $(MRKISS_OBJ_FILES) -o $@

tc1_dormand_prince_5_4_b2.out : tc1_dormand_prince_5_4_b2
	tc1_dormand_prince_5_4_b2$(EXE_SUFFIX)

.PHONY: test_dormand_prince_5_4_b2
test_tc1_dormand_prince_5_4_b2 : tc1_dormand_prince_5_4_b2.out
	$(FD) -Zdq -e 1e-10 data/tc1_dormand_prince_5_4_b2.out tc1_dormand_prince_5_4_b2.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc1_dormand_prince_7_8_b1 : tc1_dormand_prince_7_8_b1.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc1_dormand_prince_7_8_b1.f90 $(MRKISS_OBJ_FILES) -o $@

tc1_dormand_prince_7_8_b1.out : tc1_dormand_prince_7_8_b1
	tc1_dormand_prince_7_8_b1$(EXE_SUFFIX)

.PHONY: test_dormand_prince_7_8_b1
test_tc1_dormand_prince_7_8_b1 : tc1_dormand_prince_7_8_b1.out
	$(FD) -Zdq -e 1e-10 data/tc1_dormand_prince_7_8_b1.out tc1_dormand_prince_7_8_b1.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc1_dormand_prince_7_8_b2 : tc1_dormand_prince_7_8_b2.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc1_dormand_prince_7_8_b2.f90 $(MRKISS_OBJ_FILES) -o $@

tc1_dormand_prince_7_8_b2.out : tc1_dormand_prince_7_8_b2
	tc1_dormand_prince_7_8_b2$(EXE_SUFFIX)

.PHONY: test_dormand_prince_7_8_b2
test_tc1_dormand_prince_7_8_b2 : tc1_dormand_prince_7_8_b2.out
	$(FD) -Zdq -e 1e-10 data/tc1_dormand_prince_7_8_b2.out tc1_dormand_prince_7_8_b2.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc1_fehlberg_4_5_b1 : tc1_fehlberg_4_5_b1.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc1_fehlberg_4_5_b1.f90 $(MRKISS_OBJ_FILES) -o $@

tc1_fehlberg_4_5_b1.out : tc1_fehlberg_4_5_b1
	tc1_fehlberg_4_5_b1$(EXE_SUFFIX)

.PHONY: test_fehlberg_4_5_b1
test_tc1_fehlberg_4_5_b1 : tc1_fehlberg_4_5_b1.out
	$(FD) -Zdq -e 1e-10 data/tc1_fehlberg_4_5_b1.out tc1_fehlberg_4_5_b1.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc1_fehlberg_4_5_b2 : tc1_fehlberg_4_5_b2.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc1_fehlberg_4_5_b2.f90 $(MRKISS_OBJ_FILES) -o $@

tc1_fehlberg_4_5_b2.out : tc1_fehlberg_4_5_b2
	tc1_fehlberg_4_5_b2$(EXE_SUFFIX)

.PHONY: test_fehlberg_4_5_b2
test_tc1_fehlberg_4_5_b2 : tc1_fehlberg_4_5_b2.out
	$(FD) -Zdq -e 1e-10 data/tc1_fehlberg_4_5_b2.out tc1_fehlberg_4_5_b2.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc1_fehlberg_7_8_b1 : tc1_fehlberg_7_8_b1.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc1_fehlberg_7_8_b1.f90 $(MRKISS_OBJ_FILES) -o $@

tc1_fehlberg_7_8_b1.out : tc1_fehlberg_7_8_b1
	tc1_fehlberg_7_8_b1$(EXE_SUFFIX)

.PHONY: test_fehlberg_7_8_b1
test_tc1_fehlberg_7_8_b1 : tc1_fehlberg_7_8_b1.out
	$(FD) -Zdq -e 1e-10 data/tc1_fehlberg_7_8_b1.out tc1_fehlberg_7_8_b1.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc1_fehlberg_7_8_b2 : tc1_fehlberg_7_8_b2.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc1_fehlberg_7_8_b2.f90 $(MRKISS_OBJ_FILES) -o $@

tc1_fehlberg_7_8_b2.out : tc1_fehlberg_7_8_b2
	tc1_fehlberg_7_8_b2$(EXE_SUFFIX)

.PHONY: test_fehlberg_7_8_b2
test_tc1_fehlberg_7_8_b2 : tc1_fehlberg_7_8_b2.out
	$(FD) -Zdq -e 1e-10 data/tc1_fehlberg_7_8_b2.out tc1_fehlberg_7_8_b2.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc1_heun_euler_2_1_b1 : tc1_heun_euler_2_1_b1.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc1_heun_euler_2_1_b1.f90 $(MRKISS_OBJ_FILES) -o $@

tc1_heun_euler_2_1_b1.out : tc1_heun_euler_2_1_b1
	tc1_heun_euler_2_1_b1$(EXE_SUFFIX)

.PHONY: test_heun_euler_2_1_b1
test_tc1_heun_euler_2_1_b1 : tc1_heun_euler_2_1_b1.out
	$(FD) -Zdq -e 1e-10 data/tc1_heun_euler_2_1_b1.out tc1_heun_euler_2_1_b1.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc1_heun_euler_2_1_b2 : tc1_heun_euler_2_1_b2.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc1_heun_euler_2_1_b2.f90 $(MRKISS_OBJ_FILES) -o $@

tc1_heun_euler_2_1_b2.out : tc1_heun_euler_2_1_b2
	tc1_heun_euler_2_1_b2$(EXE_SUFFIX)

.PHONY: test_heun_euler_2_1_b2
test_tc1_heun_euler_2_1_b2 : tc1_heun_euler_2_1_b2.out
	$(FD) -Zdq -e 1e-10 data/tc1_heun_euler_2_1_b2.out tc1_heun_euler_2_1_b2.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc1_sofroniou_spaletta_4_3_b1 : tc1_sofroniou_spaletta_4_3_b1.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc1_sofroniou_spaletta_4_3_b1.f90 $(MRKISS_OBJ_FILES) -o $@

tc1_sofroniou_spaletta_4_3_b1.out : tc1_sofroniou_spaletta_4_3_b1
	tc1_sofroniou_spaletta_4_3_b1$(EXE_SUFFIX)

.PHONY: test_sofroniou_spaletta_4_3_b1
test_tc1_sofroniou_spaletta_4_3_b1 : tc1_sofroniou_spaletta_4_3_b1.out
	$(FD) -Zdq -e 1e-10 data/tc1_sofroniou_spaletta_4_3_b1.out tc1_sofroniou_spaletta_4_3_b1.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc1_sofroniou_spaletta_4_3_b2 : tc1_sofroniou_spaletta_4_3_b2.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc1_sofroniou_spaletta_4_3_b2.f90 $(MRKISS_OBJ_FILES) -o $@

tc1_sofroniou_spaletta_4_3_b2.out : tc1_sofroniou_spaletta_4_3_b2
	tc1_sofroniou_spaletta_4_3_b2$(EXE_SUFFIX)

.PHONY: test_sofroniou_spaletta_4_3_b2
test_tc1_sofroniou_spaletta_4_3_b2 : tc1_sofroniou_spaletta_4_3_b2.out
	$(FD) -Zdq -e 1e-10 data/tc1_sofroniou_spaletta_4_3_b2.out tc1_sofroniou_spaletta_4_3_b2.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc1_tsitouras_arkode_5_4_b1 : tc1_tsitouras_arkode_5_4_b1.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc1_tsitouras_arkode_5_4_b1.f90 $(MRKISS_OBJ_FILES) -o $@

tc1_tsitouras_arkode_5_4_b1.out : tc1_tsitouras_arkode_5_4_b1
	tc1_tsitouras_arkode_5_4_b1$(EXE_SUFFIX)

.PHONY: test_tsitouras_arkode_5_4_b1
test_tc1_tsitouras_arkode_5_4_b1 : tc1_tsitouras_arkode_5_4_b1.out
	$(FD) -Zdq -e 1e-10 data/tc1_tsitouras_arkode_5_4_b1.out tc1_tsitouras_arkode_5_4_b1.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc1_tsitouras_arkode_5_4_b2 : tc1_tsitouras_arkode_5_4_b2.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc1_tsitouras_arkode_5_4_b2.f90 $(MRKISS_OBJ_FILES) -o $@

tc1_tsitouras_arkode_5_4_b2.out : tc1_tsitouras_arkode_5_4_b2
	tc1_tsitouras_arkode_5_4_b2$(EXE_SUFFIX)

.PHONY: test_tsitouras_arkode_5_4_b2
test_tc1_tsitouras_arkode_5_4_b2 : tc1_tsitouras_arkode_5_4_b2.out
	$(FD) -Zdq -e 1e-10 data/tc1_tsitouras_arkode_5_4_b2.out tc1_tsitouras_arkode_5_4_b2.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc1_verner_1978_6_5_b1 : tc1_verner_1978_6_5_b1.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc1_verner_1978_6_5_b1.f90 $(MRKISS_OBJ_FILES) -o $@

tc1_verner_1978_6_5_b1.out : tc1_verner_1978_6_5_b1
	tc1_verner_1978_6_5_b1$(EXE_SUFFIX)

.PHONY: test_verner_1978_6_5_b1
test_tc1_verner_1978_6_5_b1 : tc1_verner_1978_6_5_b1.out
	$(FD) -Zdq -e 1e-10 data/tc1_verner_1978_6_5_b1.out tc1_verner_1978_6_5_b1.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc1_verner_1978_6_5_b2 : tc1_verner_1978_6_5_b2.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc1_verner_1978_6_5_b2.f90 $(MRKISS_OBJ_FILES) -o $@

tc1_verner_1978_6_5_b2.out : tc1_verner_1978_6_5_b2
	tc1_verner_1978_6_5_b2$(EXE_SUFFIX)

.PHONY: test_verner_1978_6_5_b2
test_tc1_verner_1978_6_5_b2 : tc1_verner_1978_6_5_b2.out
	$(FD) -Zdq -e 1e-10 data/tc1_verner_1978_6_5_b2.out tc1_verner_1978_6_5_b2.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc1_verner_2010_6_5_b1 : tc1_verner_2010_6_5_b1.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc1_verner_2010_6_5_b1.f90 $(MRKISS_OBJ_FILES) -o $@

tc1_verner_2010_6_5_b1.out : tc1_verner_2010_6_5_b1
	tc1_verner_2010_6_5_b1$(EXE_SUFFIX)

.PHONY: test_verner_2010_6_5_b1
test_tc1_verner_2010_6_5_b1 : tc1_verner_2010_6_5_b1.out
	$(FD) -Zdq -e 1e-10 data/tc1_verner_2010_6_5_b1.out tc1_verner_2010_6_5_b1.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc1_verner_2010_6_5_b2 : tc1_verner_2010_6_5_b2.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc1_verner_2010_6_5_b2.f90 $(MRKISS_OBJ_FILES) -o $@

tc1_verner_2010_6_5_b2.out : tc1_verner_2010_6_5_b2
	tc1_verner_2010_6_5_b2$(EXE_SUFFIX)

.PHONY: test_verner_2010_6_5_b2
test_tc1_verner_2010_6_5_b2 : tc1_verner_2010_6_5_b2.out
	$(FD) -Zdq -e 1e-10 data/tc1_verner_2010_6_5_b2.out tc1_verner_2010_6_5_b2.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc1_verner_7_6_b1 : tc1_verner_7_6_b1.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc1_verner_7_6_b1.f90 $(MRKISS_OBJ_FILES) -o $@

tc1_verner_7_6_b1.out : tc1_verner_7_6_b1
	tc1_verner_7_6_b1$(EXE_SUFFIX)

.PHONY: test_verner_7_6_b1
test_tc1_verner_7_6_b1 : tc1_verner_7_6_b1.out
	$(FD) -Zdq -e 1e-10 data/tc1_verner_7_6_b1.out tc1_verner_7_6_b1.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc1_verner_7_6_b2 : tc1_verner_7_6_b2.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc1_verner_7_6_b2.f90 $(MRKISS_OBJ_FILES) -o $@

tc1_verner_7_6_b2.out : tc1_verner_7_6_b2
	tc1_verner_7_6_b2$(EXE_SUFFIX)

.PHONY: test_verner_7_6_b2
test_tc1_verner_7_6_b2 : tc1_verner_7_6_b2.out
	$(FD) -Zdq -e 1e-10 data/tc1_verner_7_6_b2.out tc1_verner_7_6_b2.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc1_verner_8_7_b1 : tc1_verner_8_7_b1.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc1_verner_8_7_b1.f90 $(MRKISS_OBJ_FILES) -o $@

tc1_verner_8_7_b1.out : tc1_verner_8_7_b1
	tc1_verner_8_7_b1$(EXE_SUFFIX)

.PHONY: test_verner_8_7_b1
test_tc1_verner_8_7_b1 : tc1_verner_8_7_b1.out
	$(FD) -Zdq -e 1e-10 data/tc1_verner_8_7_b1.out tc1_verner_8_7_b1.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc1_verner_8_7_b2 : tc1_verner_8_7_b2.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc1_verner_8_7_b2.f90 $(MRKISS_OBJ_FILES) -o $@

tc1_verner_8_7_b2.out : tc1_verner_8_7_b2
	tc1_verner_8_7_b2$(EXE_SUFFIX)

.PHONY: test_verner_8_7_b2
test_tc1_verner_8_7_b2 : tc1_verner_8_7_b2.out
	$(FD) -Zdq -e 1e-10 data/tc1_verner_8_7_b2.out tc1_verner_8_7_b2.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc1_verner_9_8_b1 : tc1_verner_9_8_b1.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc1_verner_9_8_b1.f90 $(MRKISS_OBJ_FILES) -o $@

tc1_verner_9_8_b1.out : tc1_verner_9_8_b1
	tc1_verner_9_8_b1$(EXE_SUFFIX)

.PHONY: test_verner_9_8_b1
test_tc1_verner_9_8_b1 : tc1_verner_9_8_b1.out
	$(FD) -Zdq -e 1e-10 data/tc1_verner_9_8_b1.out tc1_verner_9_8_b1.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc1_verner_9_8_b2 : tc1_verner_9_8_b2.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc1_verner_9_8_b2.f90 $(MRKISS_OBJ_FILES) -o $@

tc1_verner_9_8_b2.out : tc1_verner_9_8_b2
	tc1_verner_9_8_b2$(EXE_SUFFIX)

.PHONY: test_verner_9_8_b2
test_tc1_verner_9_8_b2 : tc1_verner_9_8_b2.out
	$(FD) -Zdq -e 1e-10 data/tc1_verner_9_8_b2.out tc1_verner_9_8_b2.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc1_euler_1 : tc1_euler_1.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc1_euler_1.f90 $(MRKISS_OBJ_FILES) -o $@

tc1_euler_1.out : tc1_euler_1
	tc1_euler_1$(EXE_SUFFIX)

.PHONY: test_euler_1
test_tc1_euler_1 : tc1_euler_1.out
	$(FD) -Zdq -e 1e-10 data/tc1_euler_1.out tc1_euler_1.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc1_feagin_10 : tc1_feagin_10.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc1_feagin_10.f90 $(MRKISS_OBJ_FILES) -o $@

tc1_feagin_10.out : tc1_feagin_10
	tc1_feagin_10$(EXE_SUFFIX)

.PHONY: test_feagin_10
test_tc1_feagin_10 : tc1_feagin_10.out
	$(FD) -Zdq -e 1e-10 data/tc1_feagin_10.out tc1_feagin_10.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc1_knoth_wolke_3 : tc1_knoth_wolke_3.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc1_knoth_wolke_3.f90 $(MRKISS_OBJ_FILES) -o $@

tc1_knoth_wolke_3.out : tc1_knoth_wolke_3
	tc1_knoth_wolke_3$(EXE_SUFFIX)

.PHONY: test_knoth_wolke_3
test_tc1_knoth_wolke_3 : tc1_knoth_wolke_3.out
	$(FD) -Zdq -e 1e-10 data/tc1_knoth_wolke_3.out tc1_knoth_wolke_3.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc1_kutta_4 : tc1_kutta_4.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc1_kutta_4.f90 $(MRKISS_OBJ_FILES) -o $@

tc1_kutta_4.out : tc1_kutta_4
	tc1_kutta_4$(EXE_SUFFIX)

.PHONY: test_kutta_4
test_tc1_kutta_4 : tc1_kutta_4.out
	$(FD) -Zdq -e 1e-10 data/tc1_kutta_4.out tc1_kutta_4.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc1_kutta_three_eight_4 : tc1_kutta_three_eight_4.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc1_kutta_three_eight_4.f90 $(MRKISS_OBJ_FILES) -o $@

tc1_kutta_three_eight_4.out : tc1_kutta_three_eight_4
	tc1_kutta_three_eight_4$(EXE_SUFFIX)

.PHONY: test_kutta_three_eight_4
test_tc1_kutta_three_eight_4 : tc1_kutta_three_eight_4.out
	$(FD) -Zdq -e 1e-10 data/tc1_kutta_three_eight_4.out tc1_kutta_three_eight_4.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc1_midpoint_2 : tc1_midpoint_2.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc1_midpoint_2.f90 $(MRKISS_OBJ_FILES) -o $@

tc1_midpoint_2.out : tc1_midpoint_2
	tc1_midpoint_2$(EXE_SUFFIX)

.PHONY: test_midpoint_2
test_tc1_midpoint_2 : tc1_midpoint_2.out
	$(FD) -Zdq -e 1e-10 data/tc1_midpoint_2.out tc1_midpoint_2.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc1_ralston_2 : tc1_ralston_2.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc1_ralston_2.f90 $(MRKISS_OBJ_FILES) -o $@

tc1_ralston_2.out : tc1_ralston_2
	tc1_ralston_2$(EXE_SUFFIX)

.PHONY: test_ralston_2
test_tc1_ralston_2 : tc1_ralston_2.out
	$(FD) -Zdq -e 1e-10 data/tc1_ralston_2.out tc1_ralston_2.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc1_ralston_3 : tc1_ralston_3.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc1_ralston_3.f90 $(MRKISS_OBJ_FILES) -o $@

tc1_ralston_3.out : tc1_ralston_3
	tc1_ralston_3$(EXE_SUFFIX)

.PHONY: test_ralston_3
test_tc1_ralston_3 : tc1_ralston_3.out
	$(FD) -Zdq -e 1e-10 data/tc1_ralston_3.out tc1_ralston_3.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc1_ralston_4 : tc1_ralston_4.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc1_ralston_4.f90 $(MRKISS_OBJ_FILES) -o $@

tc1_ralston_4.out : tc1_ralston_4
	tc1_ralston_4$(EXE_SUFFIX)

.PHONY: test_ralston_4
test_tc1_ralston_4 : tc1_ralston_4.out
	$(FD) -Zdq -e 1e-10 data/tc1_ralston_4.out tc1_ralston_4.out

