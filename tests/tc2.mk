# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Run all tests for tc2
.PHONY: test_tc2
test_tc2 : test_tc2_bogacki_shampine_3_2_b1 test_tc2_bogacki_shampine_3_2_b2 test_tc2_bogacki_shampine_4_5_b1 test_tc2_bogacki_shampine_4_5_b2 test_tc2_cash_karp_5_4_b1 test_tc2_cash_karp_5_4_b2 test_tc2_dormand_prince_5_4_b1 test_tc2_dormand_prince_5_4_b2 test_tc2_dormand_prince_7_8_b1 test_tc2_dormand_prince_7_8_b2 test_tc2_fehlberg_4_5_b1 test_tc2_fehlberg_4_5_b2 test_tc2_fehlberg_7_8_b1 test_tc2_fehlberg_7_8_b2 test_tc2_heun_euler_2_1_b1 test_tc2_heun_euler_2_1_b2 test_tc2_sofroniou_spaletta_4_3_b1 test_tc2_sofroniou_spaletta_4_3_b2 test_tc2_tsitouras_arkode_5_4_b1 test_tc2_tsitouras_arkode_5_4_b2 test_tc2_verner_1978_6_5_b1 test_tc2_verner_1978_6_5_b2 test_tc2_verner_2010_6_5_b1 test_tc2_verner_2010_6_5_b2 test_tc2_verner_7_6_b1 test_tc2_verner_7_6_b2 test_tc2_verner_8_7_b1 test_tc2_verner_8_7_b2 test_tc2_verner_9_8_b1 test_tc2_verner_9_8_b2 test_tc2_euler_1 test_tc2_feagin_10 test_tc2_nystrom_5 test_tc2_knoth_wolke_3 test_tc2_kutta_4 test_tc2_kutta_three_eight_4 test_tc2_midpoint_2 test_tc2_ralston_2 test_tc2_ralston_3 test_tc2_ralston_4

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Make source code for tc2
.PHONY: src_tc2
tc2_bogacki_shampine_3_2_b1.f90 tc2_bogacki_shampine_3_2_b2.f90 tc2_bogacki_shampine_4_5_b1.f90 tc2_bogacki_shampine_4_5_b2.f90 tc2_cash_karp_5_4_b1.f90 tc2_cash_karp_5_4_b2.f90 tc2_dormand_prince_5_4_b1.f90 tc2_dormand_prince_5_4_b2.f90 tc2_dormand_prince_7_8_b1.f90 tc2_dormand_prince_7_8_b2.f90 tc2_fehlberg_4_5_b1.f90 tc2_fehlberg_4_5_b2.f90 tc2_fehlberg_7_8_b1.f90 tc2_fehlberg_7_8_b2.f90 tc2_heun_euler_2_1_b1.f90 tc2_heun_euler_2_1_b2.f90 tc2_sofroniou_spaletta_4_3_b1.f90 tc2_sofroniou_spaletta_4_3_b2.f90 tc2_tsitouras_arkode_5_4_b1.f90 tc2_tsitouras_arkode_5_4_b2.f90 tc2_verner_1978_6_5_b1.f90 tc2_verner_1978_6_5_b2.f90 tc2_verner_2010_6_5_b1.f90 tc2_verner_2010_6_5_b2.f90 tc2_verner_7_6_b1.f90 tc2_verner_7_6_b2.f90 tc2_verner_8_7_b1.f90 tc2_verner_8_7_b2.f90 tc2_verner_9_8_b1.f90 tc2_verner_9_8_b2.f90 tc2_euler_1.f90 tc2_feagin_10.f90 tc2_nystrom_5.f90 tc2_knoth_wolke_3.f90 tc2_kutta_4.f90 tc2_kutta_three_eight_4.f90 tc2_midpoint_2.f90 tc2_ralston_2.f90 tc2_ralston_3.f90 tc2_ralston_4.f90 &: tc2_template.f90
	ruby tc_make_make.rb tc2_template.f90

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Clean source code for tc2
.PHONY: clean_src_tc2
clean_src_tc2 :
	rm -f tc2_bogacki_shampine_3_2_b1.f90 tc2_bogacki_shampine_3_2_b2.f90 tc2_bogacki_shampine_4_5_b1.f90 tc2_bogacki_shampine_4_5_b2.f90 tc2_cash_karp_5_4_b1.f90 tc2_cash_karp_5_4_b2.f90 tc2_dormand_prince_5_4_b1.f90 tc2_dormand_prince_5_4_b2.f90 tc2_dormand_prince_7_8_b1.f90 tc2_dormand_prince_7_8_b2.f90 tc2_fehlberg_4_5_b1.f90 tc2_fehlberg_4_5_b2.f90 tc2_fehlberg_7_8_b1.f90 tc2_fehlberg_7_8_b2.f90 tc2_heun_euler_2_1_b1.f90 tc2_heun_euler_2_1_b2.f90 tc2_sofroniou_spaletta_4_3_b1.f90 tc2_sofroniou_spaletta_4_3_b2.f90 tc2_tsitouras_arkode_5_4_b1.f90 tc2_tsitouras_arkode_5_4_b2.f90 tc2_verner_1978_6_5_b1.f90 tc2_verner_1978_6_5_b2.f90 tc2_verner_2010_6_5_b1.f90 tc2_verner_2010_6_5_b2.f90 tc2_verner_7_6_b1.f90 tc2_verner_7_6_b2.f90 tc2_verner_8_7_b1.f90 tc2_verner_8_7_b2.f90 tc2_verner_9_8_b1.f90 tc2_verner_9_8_b2.f90 tc2_euler_1.f90 tc2_feagin_10.f90 tc2_nystrom_5.f90 tc2_knoth_wolke_3.f90 tc2_kutta_4.f90 tc2_kutta_three_eight_4.f90 tc2_midpoint_2.f90 tc2_ralston_2.f90 tc2_ralston_3.f90 tc2_ralston_4.f90

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Clean non-source code for tc2
.PHONY: clean_tc2
clean_tc2 :
	rm -f tc2_bogacki_shampine_3_2_b1.out tc2_bogacki_shampine_3_2_b2.out tc2_bogacki_shampine_4_5_b1.out tc2_bogacki_shampine_4_5_b2.out tc2_cash_karp_5_4_b1.out tc2_cash_karp_5_4_b2.out tc2_dormand_prince_5_4_b1.out tc2_dormand_prince_5_4_b2.out tc2_dormand_prince_7_8_b1.out tc2_dormand_prince_7_8_b2.out tc2_fehlberg_4_5_b1.out tc2_fehlberg_4_5_b2.out tc2_fehlberg_7_8_b1.out tc2_fehlberg_7_8_b2.out tc2_heun_euler_2_1_b1.out tc2_heun_euler_2_1_b2.out tc2_sofroniou_spaletta_4_3_b1.out tc2_sofroniou_spaletta_4_3_b2.out tc2_tsitouras_arkode_5_4_b1.out tc2_tsitouras_arkode_5_4_b2.out tc2_verner_1978_6_5_b1.out tc2_verner_1978_6_5_b2.out tc2_verner_2010_6_5_b1.out tc2_verner_2010_6_5_b2.out tc2_verner_7_6_b1.out tc2_verner_7_6_b2.out tc2_verner_8_7_b1.out tc2_verner_8_7_b2.out tc2_verner_9_8_b1.out tc2_verner_9_8_b2.out tc2_euler_1.out tc2_feagin_10.out tc2_nystrom_5.out tc2_knoth_wolke_3.out tc2_kutta_4.out tc2_kutta_three_eight_4.out tc2_midpoint_2.out tc2_ralston_2.out tc2_ralston_3.out tc2_ralston_4.out tc2_bogacki_shampine_3_2_b1 tc2_bogacki_shampine_3_2_b2 tc2_bogacki_shampine_4_5_b1 tc2_bogacki_shampine_4_5_b2 tc2_cash_karp_5_4_b1 tc2_cash_karp_5_4_b2 tc2_dormand_prince_5_4_b1 tc2_dormand_prince_5_4_b2 tc2_dormand_prince_7_8_b1 tc2_dormand_prince_7_8_b2 tc2_fehlberg_4_5_b1 tc2_fehlberg_4_5_b2 tc2_fehlberg_7_8_b1 tc2_fehlberg_7_8_b2 tc2_heun_euler_2_1_b1 tc2_heun_euler_2_1_b2 tc2_sofroniou_spaletta_4_3_b1 tc2_sofroniou_spaletta_4_3_b2 tc2_tsitouras_arkode_5_4_b1 tc2_tsitouras_arkode_5_4_b2 tc2_verner_1978_6_5_b1 tc2_verner_1978_6_5_b2 tc2_verner_2010_6_5_b1 tc2_verner_2010_6_5_b2 tc2_verner_7_6_b1 tc2_verner_7_6_b2 tc2_verner_8_7_b1 tc2_verner_8_7_b2 tc2_verner_9_8_b1 tc2_verner_9_8_b2 tc2_euler_1 tc2_feagin_10 tc2_nystrom_5 tc2_knoth_wolke_3 tc2_kutta_4 tc2_kutta_three_eight_4 tc2_midpoint_2 tc2_ralston_2 tc2_ralston_3 tc2_ralston_4 tc2_bogacki_shampine_3_2_b1$(EXE_SUFFIX) tc2_bogacki_shampine_3_2_b2$(EXE_SUFFIX) tc2_bogacki_shampine_4_5_b1$(EXE_SUFFIX) tc2_bogacki_shampine_4_5_b2$(EXE_SUFFIX) tc2_cash_karp_5_4_b1$(EXE_SUFFIX) tc2_cash_karp_5_4_b2$(EXE_SUFFIX) tc2_dormand_prince_5_4_b1$(EXE_SUFFIX) tc2_dormand_prince_5_4_b2$(EXE_SUFFIX) tc2_dormand_prince_7_8_b1$(EXE_SUFFIX) tc2_dormand_prince_7_8_b2$(EXE_SUFFIX) tc2_fehlberg_4_5_b1$(EXE_SUFFIX) tc2_fehlberg_4_5_b2$(EXE_SUFFIX) tc2_fehlberg_7_8_b1$(EXE_SUFFIX) tc2_fehlberg_7_8_b2$(EXE_SUFFIX) tc2_heun_euler_2_1_b1$(EXE_SUFFIX) tc2_heun_euler_2_1_b2$(EXE_SUFFIX) tc2_sofroniou_spaletta_4_3_b1$(EXE_SUFFIX) tc2_sofroniou_spaletta_4_3_b2$(EXE_SUFFIX) tc2_tsitouras_arkode_5_4_b1$(EXE_SUFFIX) tc2_tsitouras_arkode_5_4_b2$(EXE_SUFFIX) tc2_verner_1978_6_5_b1$(EXE_SUFFIX) tc2_verner_1978_6_5_b2$(EXE_SUFFIX) tc2_verner_2010_6_5_b1$(EXE_SUFFIX) tc2_verner_2010_6_5_b2$(EXE_SUFFIX) tc2_verner_7_6_b1$(EXE_SUFFIX) tc2_verner_7_6_b2$(EXE_SUFFIX) tc2_verner_8_7_b1$(EXE_SUFFIX) tc2_verner_8_7_b2$(EXE_SUFFIX) tc2_verner_9_8_b1$(EXE_SUFFIX) tc2_verner_9_8_b2$(EXE_SUFFIX) tc2_euler_1$(EXE_SUFFIX) tc2_feagin_10$(EXE_SUFFIX) tc2_nystrom_5$(EXE_SUFFIX) tc2_knoth_wolke_3$(EXE_SUFFIX) tc2_kutta_4$(EXE_SUFFIX) tc2_kutta_three_eight_4$(EXE_SUFFIX) tc2_midpoint_2$(EXE_SUFFIX) tc2_ralston_2$(EXE_SUFFIX) tc2_ralston_3$(EXE_SUFFIX) tc2_ralston_4$(EXE_SUFFIX)

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Make all the data files
.PHONY: data_tc2
data_tc2 : tc2_bogacki_shampine_3_2_b1.out tc2_bogacki_shampine_3_2_b2.out tc2_bogacki_shampine_4_5_b1.out tc2_bogacki_shampine_4_5_b2.out tc2_cash_karp_5_4_b1.out tc2_cash_karp_5_4_b2.out tc2_dormand_prince_5_4_b1.out tc2_dormand_prince_5_4_b2.out tc2_dormand_prince_7_8_b1.out tc2_dormand_prince_7_8_b2.out tc2_fehlberg_4_5_b1.out tc2_fehlberg_4_5_b2.out tc2_fehlberg_7_8_b1.out tc2_fehlberg_7_8_b2.out tc2_heun_euler_2_1_b1.out tc2_heun_euler_2_1_b2.out tc2_sofroniou_spaletta_4_3_b1.out tc2_sofroniou_spaletta_4_3_b2.out tc2_tsitouras_arkode_5_4_b1.out tc2_tsitouras_arkode_5_4_b2.out tc2_verner_1978_6_5_b1.out tc2_verner_1978_6_5_b2.out tc2_verner_2010_6_5_b1.out tc2_verner_2010_6_5_b2.out tc2_verner_7_6_b1.out tc2_verner_7_6_b2.out tc2_verner_8_7_b1.out tc2_verner_8_7_b2.out tc2_verner_9_8_b1.out tc2_verner_9_8_b2.out tc2_euler_1.out tc2_feagin_10.out tc2_nystrom_5.out tc2_knoth_wolke_3.out tc2_kutta_4.out tc2_kutta_three_eight_4.out tc2_midpoint_2.out tc2_ralston_2.out tc2_ralston_3.out tc2_ralston_4.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc2_bogacki_shampine_3_2_b1 : tc2_bogacki_shampine_3_2_b1.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc2_bogacki_shampine_3_2_b1.f90 $(MRKISS_OBJ_FILES) -o $@

tc2_bogacki_shampine_3_2_b1.out : tc2_bogacki_shampine_3_2_b1
	tc2_bogacki_shampine_3_2_b1$(EXE_SUFFIX)

.PHONY: test_bogacki_shampine_3_2_b1
test_tc2_bogacki_shampine_3_2_b1 : tc2_bogacki_shampine_3_2_b1.out
	$(FD) -Zdqn -e 1e-10 data/tc2_bogacki_shampine_3_2_b1.out tc2_bogacki_shampine_3_2_b1.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc2_bogacki_shampine_3_2_b2 : tc2_bogacki_shampine_3_2_b2.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc2_bogacki_shampine_3_2_b2.f90 $(MRKISS_OBJ_FILES) -o $@

tc2_bogacki_shampine_3_2_b2.out : tc2_bogacki_shampine_3_2_b2
	tc2_bogacki_shampine_3_2_b2$(EXE_SUFFIX)

.PHONY: test_bogacki_shampine_3_2_b2
test_tc2_bogacki_shampine_3_2_b2 : tc2_bogacki_shampine_3_2_b2.out
	$(FD) -Zdqn -e 1e-10 data/tc2_bogacki_shampine_3_2_b2.out tc2_bogacki_shampine_3_2_b2.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc2_bogacki_shampine_4_5_b1 : tc2_bogacki_shampine_4_5_b1.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc2_bogacki_shampine_4_5_b1.f90 $(MRKISS_OBJ_FILES) -o $@

tc2_bogacki_shampine_4_5_b1.out : tc2_bogacki_shampine_4_5_b1
	tc2_bogacki_shampine_4_5_b1$(EXE_SUFFIX)

.PHONY: test_bogacki_shampine_4_5_b1
test_tc2_bogacki_shampine_4_5_b1 : tc2_bogacki_shampine_4_5_b1.out
	$(FD) -Zdqn -e 1e-10 data/tc2_bogacki_shampine_4_5_b1.out tc2_bogacki_shampine_4_5_b1.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc2_bogacki_shampine_4_5_b2 : tc2_bogacki_shampine_4_5_b2.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc2_bogacki_shampine_4_5_b2.f90 $(MRKISS_OBJ_FILES) -o $@

tc2_bogacki_shampine_4_5_b2.out : tc2_bogacki_shampine_4_5_b2
	tc2_bogacki_shampine_4_5_b2$(EXE_SUFFIX)

.PHONY: test_bogacki_shampine_4_5_b2
test_tc2_bogacki_shampine_4_5_b2 : tc2_bogacki_shampine_4_5_b2.out
	$(FD) -Zdqn -e 1e-10 data/tc2_bogacki_shampine_4_5_b2.out tc2_bogacki_shampine_4_5_b2.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc2_cash_karp_5_4_b1 : tc2_cash_karp_5_4_b1.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc2_cash_karp_5_4_b1.f90 $(MRKISS_OBJ_FILES) -o $@

tc2_cash_karp_5_4_b1.out : tc2_cash_karp_5_4_b1
	tc2_cash_karp_5_4_b1$(EXE_SUFFIX)

.PHONY: test_cash_karp_5_4_b1
test_tc2_cash_karp_5_4_b1 : tc2_cash_karp_5_4_b1.out
	$(FD) -Zdqn -e 1e-10 data/tc2_cash_karp_5_4_b1.out tc2_cash_karp_5_4_b1.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc2_cash_karp_5_4_b2 : tc2_cash_karp_5_4_b2.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc2_cash_karp_5_4_b2.f90 $(MRKISS_OBJ_FILES) -o $@

tc2_cash_karp_5_4_b2.out : tc2_cash_karp_5_4_b2
	tc2_cash_karp_5_4_b2$(EXE_SUFFIX)

.PHONY: test_cash_karp_5_4_b2
test_tc2_cash_karp_5_4_b2 : tc2_cash_karp_5_4_b2.out
	$(FD) -Zdqn -e 1e-10 data/tc2_cash_karp_5_4_b2.out tc2_cash_karp_5_4_b2.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc2_dormand_prince_5_4_b1 : tc2_dormand_prince_5_4_b1.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc2_dormand_prince_5_4_b1.f90 $(MRKISS_OBJ_FILES) -o $@

tc2_dormand_prince_5_4_b1.out : tc2_dormand_prince_5_4_b1
	tc2_dormand_prince_5_4_b1$(EXE_SUFFIX)

.PHONY: test_dormand_prince_5_4_b1
test_tc2_dormand_prince_5_4_b1 : tc2_dormand_prince_5_4_b1.out
	$(FD) -Zdqn -e 1e-10 data/tc2_dormand_prince_5_4_b1.out tc2_dormand_prince_5_4_b1.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc2_dormand_prince_5_4_b2 : tc2_dormand_prince_5_4_b2.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc2_dormand_prince_5_4_b2.f90 $(MRKISS_OBJ_FILES) -o $@

tc2_dormand_prince_5_4_b2.out : tc2_dormand_prince_5_4_b2
	tc2_dormand_prince_5_4_b2$(EXE_SUFFIX)

.PHONY: test_dormand_prince_5_4_b2
test_tc2_dormand_prince_5_4_b2 : tc2_dormand_prince_5_4_b2.out
	$(FD) -Zdqn -e 1e-10 data/tc2_dormand_prince_5_4_b2.out tc2_dormand_prince_5_4_b2.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc2_dormand_prince_7_8_b1 : tc2_dormand_prince_7_8_b1.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc2_dormand_prince_7_8_b1.f90 $(MRKISS_OBJ_FILES) -o $@

tc2_dormand_prince_7_8_b1.out : tc2_dormand_prince_7_8_b1
	tc2_dormand_prince_7_8_b1$(EXE_SUFFIX)

.PHONY: test_dormand_prince_7_8_b1
test_tc2_dormand_prince_7_8_b1 : tc2_dormand_prince_7_8_b1.out
	$(FD) -Zdqn -e 1e-10 data/tc2_dormand_prince_7_8_b1.out tc2_dormand_prince_7_8_b1.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc2_dormand_prince_7_8_b2 : tc2_dormand_prince_7_8_b2.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc2_dormand_prince_7_8_b2.f90 $(MRKISS_OBJ_FILES) -o $@

tc2_dormand_prince_7_8_b2.out : tc2_dormand_prince_7_8_b2
	tc2_dormand_prince_7_8_b2$(EXE_SUFFIX)

.PHONY: test_dormand_prince_7_8_b2
test_tc2_dormand_prince_7_8_b2 : tc2_dormand_prince_7_8_b2.out
	$(FD) -Zdqn -e 1e-10 data/tc2_dormand_prince_7_8_b2.out tc2_dormand_prince_7_8_b2.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc2_fehlberg_4_5_b1 : tc2_fehlberg_4_5_b1.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc2_fehlberg_4_5_b1.f90 $(MRKISS_OBJ_FILES) -o $@

tc2_fehlberg_4_5_b1.out : tc2_fehlberg_4_5_b1
	tc2_fehlberg_4_5_b1$(EXE_SUFFIX)

.PHONY: test_fehlberg_4_5_b1
test_tc2_fehlberg_4_5_b1 : tc2_fehlberg_4_5_b1.out
	$(FD) -Zdqn -e 1e-10 data/tc2_fehlberg_4_5_b1.out tc2_fehlberg_4_5_b1.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc2_fehlberg_4_5_b2 : tc2_fehlberg_4_5_b2.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc2_fehlberg_4_5_b2.f90 $(MRKISS_OBJ_FILES) -o $@

tc2_fehlberg_4_5_b2.out : tc2_fehlberg_4_5_b2
	tc2_fehlberg_4_5_b2$(EXE_SUFFIX)

.PHONY: test_fehlberg_4_5_b2
test_tc2_fehlberg_4_5_b2 : tc2_fehlberg_4_5_b2.out
	$(FD) -Zdqn -e 1e-10 data/tc2_fehlberg_4_5_b2.out tc2_fehlberg_4_5_b2.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc2_fehlberg_7_8_b1 : tc2_fehlberg_7_8_b1.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc2_fehlberg_7_8_b1.f90 $(MRKISS_OBJ_FILES) -o $@

tc2_fehlberg_7_8_b1.out : tc2_fehlberg_7_8_b1
	tc2_fehlberg_7_8_b1$(EXE_SUFFIX)

.PHONY: test_fehlberg_7_8_b1
test_tc2_fehlberg_7_8_b1 : tc2_fehlberg_7_8_b1.out
	$(FD) -Zdqn -e 1e-10 data/tc2_fehlberg_7_8_b1.out tc2_fehlberg_7_8_b1.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc2_fehlberg_7_8_b2 : tc2_fehlberg_7_8_b2.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc2_fehlberg_7_8_b2.f90 $(MRKISS_OBJ_FILES) -o $@

tc2_fehlberg_7_8_b2.out : tc2_fehlberg_7_8_b2
	tc2_fehlberg_7_8_b2$(EXE_SUFFIX)

.PHONY: test_fehlberg_7_8_b2
test_tc2_fehlberg_7_8_b2 : tc2_fehlberg_7_8_b2.out
	$(FD) -Zdqn -e 1e-10 data/tc2_fehlberg_7_8_b2.out tc2_fehlberg_7_8_b2.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc2_heun_euler_2_1_b1 : tc2_heun_euler_2_1_b1.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc2_heun_euler_2_1_b1.f90 $(MRKISS_OBJ_FILES) -o $@

tc2_heun_euler_2_1_b1.out : tc2_heun_euler_2_1_b1
	tc2_heun_euler_2_1_b1$(EXE_SUFFIX)

.PHONY: test_heun_euler_2_1_b1
test_tc2_heun_euler_2_1_b1 : tc2_heun_euler_2_1_b1.out
	$(FD) -Zdqn -e 1e-10 data/tc2_heun_euler_2_1_b1.out tc2_heun_euler_2_1_b1.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc2_heun_euler_2_1_b2 : tc2_heun_euler_2_1_b2.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc2_heun_euler_2_1_b2.f90 $(MRKISS_OBJ_FILES) -o $@

tc2_heun_euler_2_1_b2.out : tc2_heun_euler_2_1_b2
	tc2_heun_euler_2_1_b2$(EXE_SUFFIX)

.PHONY: test_heun_euler_2_1_b2
test_tc2_heun_euler_2_1_b2 : tc2_heun_euler_2_1_b2.out
	$(FD) -Zdqn -e 1e-10 data/tc2_heun_euler_2_1_b2.out tc2_heun_euler_2_1_b2.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc2_sofroniou_spaletta_4_3_b1 : tc2_sofroniou_spaletta_4_3_b1.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc2_sofroniou_spaletta_4_3_b1.f90 $(MRKISS_OBJ_FILES) -o $@

tc2_sofroniou_spaletta_4_3_b1.out : tc2_sofroniou_spaletta_4_3_b1
	tc2_sofroniou_spaletta_4_3_b1$(EXE_SUFFIX)

.PHONY: test_sofroniou_spaletta_4_3_b1
test_tc2_sofroniou_spaletta_4_3_b1 : tc2_sofroniou_spaletta_4_3_b1.out
	$(FD) -Zdqn -e 1e-10 data/tc2_sofroniou_spaletta_4_3_b1.out tc2_sofroniou_spaletta_4_3_b1.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc2_sofroniou_spaletta_4_3_b2 : tc2_sofroniou_spaletta_4_3_b2.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc2_sofroniou_spaletta_4_3_b2.f90 $(MRKISS_OBJ_FILES) -o $@

tc2_sofroniou_spaletta_4_3_b2.out : tc2_sofroniou_spaletta_4_3_b2
	tc2_sofroniou_spaletta_4_3_b2$(EXE_SUFFIX)

.PHONY: test_sofroniou_spaletta_4_3_b2
test_tc2_sofroniou_spaletta_4_3_b2 : tc2_sofroniou_spaletta_4_3_b2.out
	$(FD) -Zdqn -e 1e-10 data/tc2_sofroniou_spaletta_4_3_b2.out tc2_sofroniou_spaletta_4_3_b2.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc2_tsitouras_arkode_5_4_b1 : tc2_tsitouras_arkode_5_4_b1.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc2_tsitouras_arkode_5_4_b1.f90 $(MRKISS_OBJ_FILES) -o $@

tc2_tsitouras_arkode_5_4_b1.out : tc2_tsitouras_arkode_5_4_b1
	tc2_tsitouras_arkode_5_4_b1$(EXE_SUFFIX)

.PHONY: test_tsitouras_arkode_5_4_b1
test_tc2_tsitouras_arkode_5_4_b1 : tc2_tsitouras_arkode_5_4_b1.out
	$(FD) -Zdqn -e 1e-10 data/tc2_tsitouras_arkode_5_4_b1.out tc2_tsitouras_arkode_5_4_b1.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc2_tsitouras_arkode_5_4_b2 : tc2_tsitouras_arkode_5_4_b2.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc2_tsitouras_arkode_5_4_b2.f90 $(MRKISS_OBJ_FILES) -o $@

tc2_tsitouras_arkode_5_4_b2.out : tc2_tsitouras_arkode_5_4_b2
	tc2_tsitouras_arkode_5_4_b2$(EXE_SUFFIX)

.PHONY: test_tsitouras_arkode_5_4_b2
test_tc2_tsitouras_arkode_5_4_b2 : tc2_tsitouras_arkode_5_4_b2.out
	$(FD) -Zdqn -e 1e-10 data/tc2_tsitouras_arkode_5_4_b2.out tc2_tsitouras_arkode_5_4_b2.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc2_verner_1978_6_5_b1 : tc2_verner_1978_6_5_b1.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc2_verner_1978_6_5_b1.f90 $(MRKISS_OBJ_FILES) -o $@

tc2_verner_1978_6_5_b1.out : tc2_verner_1978_6_5_b1
	tc2_verner_1978_6_5_b1$(EXE_SUFFIX)

.PHONY: test_verner_1978_6_5_b1
test_tc2_verner_1978_6_5_b1 : tc2_verner_1978_6_5_b1.out
	$(FD) -Zdqn -e 1e-10 data/tc2_verner_1978_6_5_b1.out tc2_verner_1978_6_5_b1.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc2_verner_1978_6_5_b2 : tc2_verner_1978_6_5_b2.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc2_verner_1978_6_5_b2.f90 $(MRKISS_OBJ_FILES) -o $@

tc2_verner_1978_6_5_b2.out : tc2_verner_1978_6_5_b2
	tc2_verner_1978_6_5_b2$(EXE_SUFFIX)

.PHONY: test_verner_1978_6_5_b2
test_tc2_verner_1978_6_5_b2 : tc2_verner_1978_6_5_b2.out
	$(FD) -Zdqn -e 1e-10 data/tc2_verner_1978_6_5_b2.out tc2_verner_1978_6_5_b2.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc2_verner_2010_6_5_b1 : tc2_verner_2010_6_5_b1.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc2_verner_2010_6_5_b1.f90 $(MRKISS_OBJ_FILES) -o $@

tc2_verner_2010_6_5_b1.out : tc2_verner_2010_6_5_b1
	tc2_verner_2010_6_5_b1$(EXE_SUFFIX)

.PHONY: test_verner_2010_6_5_b1
test_tc2_verner_2010_6_5_b1 : tc2_verner_2010_6_5_b1.out
	$(FD) -Zdqn -e 1e-10 data/tc2_verner_2010_6_5_b1.out tc2_verner_2010_6_5_b1.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc2_verner_2010_6_5_b2 : tc2_verner_2010_6_5_b2.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc2_verner_2010_6_5_b2.f90 $(MRKISS_OBJ_FILES) -o $@

tc2_verner_2010_6_5_b2.out : tc2_verner_2010_6_5_b2
	tc2_verner_2010_6_5_b2$(EXE_SUFFIX)

.PHONY: test_verner_2010_6_5_b2
test_tc2_verner_2010_6_5_b2 : tc2_verner_2010_6_5_b2.out
	$(FD) -Zdqn -e 1e-10 data/tc2_verner_2010_6_5_b2.out tc2_verner_2010_6_5_b2.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc2_verner_7_6_b1 : tc2_verner_7_6_b1.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc2_verner_7_6_b1.f90 $(MRKISS_OBJ_FILES) -o $@

tc2_verner_7_6_b1.out : tc2_verner_7_6_b1
	tc2_verner_7_6_b1$(EXE_SUFFIX)

.PHONY: test_verner_7_6_b1
test_tc2_verner_7_6_b1 : tc2_verner_7_6_b1.out
	$(FD) -Zdqn -e 1e-10 data/tc2_verner_7_6_b1.out tc2_verner_7_6_b1.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc2_verner_7_6_b2 : tc2_verner_7_6_b2.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc2_verner_7_6_b2.f90 $(MRKISS_OBJ_FILES) -o $@

tc2_verner_7_6_b2.out : tc2_verner_7_6_b2
	tc2_verner_7_6_b2$(EXE_SUFFIX)

.PHONY: test_verner_7_6_b2
test_tc2_verner_7_6_b2 : tc2_verner_7_6_b2.out
	$(FD) -Zdqn -e 1e-10 data/tc2_verner_7_6_b2.out tc2_verner_7_6_b2.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc2_verner_8_7_b1 : tc2_verner_8_7_b1.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc2_verner_8_7_b1.f90 $(MRKISS_OBJ_FILES) -o $@

tc2_verner_8_7_b1.out : tc2_verner_8_7_b1
	tc2_verner_8_7_b1$(EXE_SUFFIX)

.PHONY: test_verner_8_7_b1
test_tc2_verner_8_7_b1 : tc2_verner_8_7_b1.out
	$(FD) -Zdqn -e 1e-10 data/tc2_verner_8_7_b1.out tc2_verner_8_7_b1.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc2_verner_8_7_b2 : tc2_verner_8_7_b2.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc2_verner_8_7_b2.f90 $(MRKISS_OBJ_FILES) -o $@

tc2_verner_8_7_b2.out : tc2_verner_8_7_b2
	tc2_verner_8_7_b2$(EXE_SUFFIX)

.PHONY: test_verner_8_7_b2
test_tc2_verner_8_7_b2 : tc2_verner_8_7_b2.out
	$(FD) -Zdqn -e 1e-10 data/tc2_verner_8_7_b2.out tc2_verner_8_7_b2.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc2_verner_9_8_b1 : tc2_verner_9_8_b1.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc2_verner_9_8_b1.f90 $(MRKISS_OBJ_FILES) -o $@

tc2_verner_9_8_b1.out : tc2_verner_9_8_b1
	tc2_verner_9_8_b1$(EXE_SUFFIX)

.PHONY: test_verner_9_8_b1
test_tc2_verner_9_8_b1 : tc2_verner_9_8_b1.out
	$(FD) -Zdqn -e 1e-10 data/tc2_verner_9_8_b1.out tc2_verner_9_8_b1.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc2_verner_9_8_b2 : tc2_verner_9_8_b2.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc2_verner_9_8_b2.f90 $(MRKISS_OBJ_FILES) -o $@

tc2_verner_9_8_b2.out : tc2_verner_9_8_b2
	tc2_verner_9_8_b2$(EXE_SUFFIX)

.PHONY: test_verner_9_8_b2
test_tc2_verner_9_8_b2 : tc2_verner_9_8_b2.out
	$(FD) -Zdqn -e 1e-10 data/tc2_verner_9_8_b2.out tc2_verner_9_8_b2.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc2_euler_1 : tc2_euler_1.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc2_euler_1.f90 $(MRKISS_OBJ_FILES) -o $@

tc2_euler_1.out : tc2_euler_1
	tc2_euler_1$(EXE_SUFFIX)

.PHONY: test_euler_1
test_tc2_euler_1 : tc2_euler_1.out
	$(FD) -Zdqn -e 1e-10 data/tc2_euler_1.out tc2_euler_1.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc2_feagin_10 : tc2_feagin_10.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc2_feagin_10.f90 $(MRKISS_OBJ_FILES) -o $@

tc2_feagin_10.out : tc2_feagin_10
	tc2_feagin_10$(EXE_SUFFIX)

.PHONY: test_feagin_10
test_tc2_feagin_10 : tc2_feagin_10.out
	$(FD) -Zdqn -e 1e-10 data/tc2_feagin_10.out tc2_feagin_10.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc2_nystrom_5 : tc2_nystrom_5.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc2_nystrom_5.f90 $(MRKISS_OBJ_FILES) -o $@

tc2_nystrom_5.out : tc2_nystrom_5
	tc2_nystrom_5$(EXE_SUFFIX)

.PHONY: test_nystrom_5
test_tc2_nystrom_5 : tc2_nystrom_5.out
	$(FD) -Zdqn -e 1e-10 data/tc2_nystrom_5.out tc2_nystrom_5.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc2_knoth_wolke_3 : tc2_knoth_wolke_3.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc2_knoth_wolke_3.f90 $(MRKISS_OBJ_FILES) -o $@

tc2_knoth_wolke_3.out : tc2_knoth_wolke_3
	tc2_knoth_wolke_3$(EXE_SUFFIX)

.PHONY: test_knoth_wolke_3
test_tc2_knoth_wolke_3 : tc2_knoth_wolke_3.out
	$(FD) -Zdqn -e 1e-10 data/tc2_knoth_wolke_3.out tc2_knoth_wolke_3.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc2_kutta_4 : tc2_kutta_4.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc2_kutta_4.f90 $(MRKISS_OBJ_FILES) -o $@

tc2_kutta_4.out : tc2_kutta_4
	tc2_kutta_4$(EXE_SUFFIX)

.PHONY: test_kutta_4
test_tc2_kutta_4 : tc2_kutta_4.out
	$(FD) -Zdqn -e 1e-10 data/tc2_kutta_4.out tc2_kutta_4.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc2_kutta_three_eight_4 : tc2_kutta_three_eight_4.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc2_kutta_three_eight_4.f90 $(MRKISS_OBJ_FILES) -o $@

tc2_kutta_three_eight_4.out : tc2_kutta_three_eight_4
	tc2_kutta_three_eight_4$(EXE_SUFFIX)

.PHONY: test_kutta_three_eight_4
test_tc2_kutta_three_eight_4 : tc2_kutta_three_eight_4.out
	$(FD) -Zdqn -e 1e-10 data/tc2_kutta_three_eight_4.out tc2_kutta_three_eight_4.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc2_midpoint_2 : tc2_midpoint_2.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc2_midpoint_2.f90 $(MRKISS_OBJ_FILES) -o $@

tc2_midpoint_2.out : tc2_midpoint_2
	tc2_midpoint_2$(EXE_SUFFIX)

.PHONY: test_midpoint_2
test_tc2_midpoint_2 : tc2_midpoint_2.out
	$(FD) -Zdqn -e 1e-10 data/tc2_midpoint_2.out tc2_midpoint_2.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc2_ralston_2 : tc2_ralston_2.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc2_ralston_2.f90 $(MRKISS_OBJ_FILES) -o $@

tc2_ralston_2.out : tc2_ralston_2
	tc2_ralston_2$(EXE_SUFFIX)

.PHONY: test_ralston_2
test_tc2_ralston_2 : tc2_ralston_2.out
	$(FD) -Zdqn -e 1e-10 data/tc2_ralston_2.out tc2_ralston_2.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc2_ralston_3 : tc2_ralston_3.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc2_ralston_3.f90 $(MRKISS_OBJ_FILES) -o $@

tc2_ralston_3.out : tc2_ralston_3
	tc2_ralston_3$(EXE_SUFFIX)

.PHONY: test_ralston_3
test_tc2_ralston_3 : tc2_ralston_3.out
	$(FD) -Zdqn -e 1e-10 data/tc2_ralston_3.out tc2_ralston_3.out

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc2_ralston_4 : tc2_ralston_4.f90 $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FFLAGS) tc2_ralston_4.f90 $(MRKISS_OBJ_FILES) -o $@

tc2_ralston_4.out : tc2_ralston_4
	tc2_ralston_4$(EXE_SUFFIX)

.PHONY: test_ralston_4
test_tc2_ralston_4 : tc2_ralston_4.out
	$(FD) -Zdqn -e 1e-10 data/tc2_ralston_4.out tc2_ralston_4.out

