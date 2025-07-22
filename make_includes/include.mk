#---------------------------------------------------------------------------------------------------------------------------------------------------------------
ifndef MRKISS_PATH
	   $(error ERROR: The variable MRKISS_PATH must be set! It should point to the MRKISS source directory!!!)
endif

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# By default MSYS2 on windows has an environment variable named "OS" set to "Windows_NT".
# I use this variable to adjust the names of object files, shared library files, and executable files.
ifeq ($(OS),Windows_NT)
	EXE_SUFFIX := .exe
	SLB_SUFFIX := .dll
	OBJ_SUFFIX := .obj
else
	EXE_SUFFIX :=
	SLB_SUFFIX := .so
	OBJ_SUFFIX := .o
endif

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
MRKISS_MODS := mrkiss_config mrkiss_utils mrkiss_solvers_wt mrkiss_erk_kutta_4 mrkiss_eerk_dormand_prince_5_4 mrkiss_eerk_fehlberg_7_8 mrkiss_eerk_verner_9_8 mrkiss_eerk_bogacki_shampine_3_2 mrkiss_eerk_bogacki_shampine_4_5 mrkiss_eerk_fehlberg_4_5 mrkiss_erk_euler_1 mrkiss_eerk_heun_euler_2_1 mrkiss_erk_midpoint_2 mrkiss_eerk_dormand_prince_7_8 mrkiss_erk_kutta_three_eight_4 mrkiss_erk_feagin_10 mrkiss_solvers_nt mrkiss_eerk_cash_karp_5_4 mrkiss_erk_ralston_2 mrkiss_erk_ralston_3 mrkiss_erk_ralston_4 mrkiss_erk_knoth_wolke_3 mrkiss_eerk_sofroniou_spaletta_4_3 mrkiss_eerk_verner_2010_6_5 mrkiss_eerk_verner_1978_6_5 mrkiss_eerk_tsitouras_arkode_5_4

MRKISS_MOD_FILES := $(addsuffix .mod,$(MRKISS_MODS))
MRKISS_OBJ_FILES := $(addsuffix $(OBJ_SUFFIX),$(MRKISS_MODS))

MRKISS_STATIC_LIB_FILE  := libmrkiss.a
MRKISS_SHARED_LIB_FILE  := libmrkiss$(SLB_SUFFIX)
MRKISS_LIB_FILES        := $(MRKISS_STATIC_LIB_FILE) $(MRKISS_SHARED_LIB_FILE)

$(MRKISS_PATH)/lib/mrkiss_solvers_nt.f90 : $(MRKISS_PATH)/lib/wt2nt.sed $(MRKISS_PATH)/lib/mrkiss_solvers_wt.f90 $(MRKISS_PATH)/make_includes/include.mk
	sed -f $(MRKISS_PATH)/lib/wt2nt.sed $(MRKISS_PATH)/lib/mrkiss_solvers_wt.f90 > $(MRKISS_PATH)/lib/mrkiss_solvers_nt.f90

mrkiss_config$(OBJ_SUFFIX) mrkiss_config.mod &: $(MRKISS_PATH)/lib/mrkiss_config.f90
	rm -f $(basename $@)$(OBJ_SUFFIX) $(basename $@).mod
	$(FC) $(FFLAGS) -c $< -o $(basename $@)$(OBJ_SUFFIX)

mrkiss_utils$(OBJ_SUFFIX) mrkiss_utils.mod &: $(MRKISS_PATH)/lib/mrkiss_utils.f90 mrkiss_config.mod
	rm -f $(basename $@)$(OBJ_SUFFIX) $(basename $@).mod
	$(FC) $(FFLAGS) -c $< -o $(basename $@)$(OBJ_SUFFIX)

mrkiss_erk_feagin_10$(OBJ_SUFFIX) mrkiss_erk_feagin_10.mod &: $(MRKISS_PATH)/lib/mrkiss_erk_feagin_10.f90 mrkiss_config.mod
	rm -f $(basename $@)$(OBJ_SUFFIX) $(basename $@).mod
	$(FC) $(FFLAGS) -c $< -o $(basename $@)$(OBJ_SUFFIX)

mrkiss_erk_knoth_wolke_3$(OBJ_SUFFIX) mrkiss_erk_knoth_wolke_3.mod &: $(MRKISS_PATH)/lib/mrkiss_erk_knoth_wolke_3.f90 mrkiss_config.mod
	rm -f $(basename $@)$(OBJ_SUFFIX) $(basename $@).mod
	$(FC) $(FFLAGS) -c $< -o $(basename $@)$(OBJ_SUFFIX)

mrkiss_eerk_tsitouras_arkode_5_4$(OBJ_SUFFIX) mrkiss_eerk_tsitouras_arkode_5_4.mod &: $(MRKISS_PATH)/lib/mrkiss_eerk_tsitouras_arkode_5_4.f90 mrkiss_config.mod
	rm -f $(basename $@)$(OBJ_SUFFIX) $(basename $@).mod
	$(FC) $(FFLAGS) -c $< -o $(basename $@)$(OBJ_SUFFIX)

mrkiss_erk_ralston_2$(OBJ_SUFFIX) mrkiss_erk_ralston_2.mod &: $(MRKISS_PATH)/lib/mrkiss_erk_ralston_2.f90 mrkiss_config.mod
	rm -f $(basename $@)$(OBJ_SUFFIX) $(basename $@).mod
	$(FC) $(FFLAGS) -c $< -o $(basename $@)$(OBJ_SUFFIX)

mrkiss_eerk_sofroniou_spaletta_4_3$(OBJ_SUFFIX) mrkiss_eerk_sofroniou_spaletta_4_3.mod &: $(MRKISS_PATH)/lib/mrkiss_eerk_sofroniou_spaletta_4_3.f90 mrkiss_config.mod
	rm -f $(basename $@)$(OBJ_SUFFIX) $(basename $@).mod
	$(FC) $(FFLAGS) -c $< -o $(basename $@)$(OBJ_SUFFIX)

mrkiss_erk_ralston_3$(OBJ_SUFFIX) mrkiss_erk_ralston_3.mod &: $(MRKISS_PATH)/lib/mrkiss_erk_ralston_3.f90 mrkiss_config.mod
	rm -f $(basename $@)$(OBJ_SUFFIX) $(basename $@).mod
	$(FC) $(FFLAGS) -c $< -o $(basename $@)$(OBJ_SUFFIX)

mrkiss_erk_ralston_4$(OBJ_SUFFIX) mrkiss_erk_ralston_4.mod &: $(MRKISS_PATH)/lib/mrkiss_erk_ralston_4.f90 mrkiss_config.mod
	rm -f $(basename $@)$(OBJ_SUFFIX) $(basename $@).mod
	$(FC) $(FFLAGS) -c $< -o $(basename $@)$(OBJ_SUFFIX)

mrkiss_eerk_verner_1978_6_5$(OBJ_SUFFIX) mrkiss_eerk_verner_1978_6_5.mod &: $(MRKISS_PATH)/lib/mrkiss_eerk_verner_1978_6_5.f90 mrkiss_config.mod
	rm -f $(basename $@)$(OBJ_SUFFIX) $(basename $@).mod
	$(FC) $(FFLAGS) -c $< -o $(basename $@)$(OBJ_SUFFIX)

mrkiss_eerk_verner_2010_6_5$(OBJ_SUFFIX) mrkiss_eerk_verner_2010_6_5.mod &: $(MRKISS_PATH)/lib/mrkiss_eerk_verner_2010_6_5.f90 mrkiss_config.mod
	rm -f $(basename $@)$(OBJ_SUFFIX) $(basename $@).mod
	$(FC) $(FFLAGS) -c $< -o $(basename $@)$(OBJ_SUFFIX)

mrkiss_eerk_verner_9_8$(OBJ_SUFFIX) mrkiss_eerk_verner_9_8.mod &: $(MRKISS_PATH)/lib/mrkiss_eerk_verner_9_8.f90 mrkiss_config.mod
	rm -f $(basename $@)$(OBJ_SUFFIX) $(basename $@).mod
	$(FC) $(FFLAGS) -c $< -o $(basename $@)$(OBJ_SUFFIX)

mrkiss_eerk_heun_euler_2_1$(OBJ_SUFFIX) mrkiss_eerk_heun_euler_2_1.mod &: $(MRKISS_PATH)/lib/mrkiss_eerk_heun_euler_2_1.f90 mrkiss_config.mod
	rm -f $(basename $@)$(OBJ_SUFFIX) $(basename $@).mod
	$(FC) $(FFLAGS) -c $< -o $(basename $@)$(OBJ_SUFFIX)

mrkiss_erk_euler_1$(OBJ_SUFFIX) mrkiss_erk_euler_1.mod &: $(MRKISS_PATH)/lib/mrkiss_erk_euler_1.f90 mrkiss_config.mod
	rm -f $(basename $@)$(OBJ_SUFFIX) $(basename $@).mod
	$(FC) $(FFLAGS) -c $< -o $(basename $@)$(OBJ_SUFFIX)

mrkiss_eerk_dormand_prince_7_8$(OBJ_SUFFIX) mrkiss_eerk_dormand_prince_7_8.mod &: $(MRKISS_PATH)/lib/mrkiss_eerk_dormand_prince_7_8.f90 mrkiss_config.mod
	rm -f $(basename $@)$(OBJ_SUFFIX) $(basename $@).mod
	$(FC) $(FFLAGS) -c $< -o $(basename $@)$(OBJ_SUFFIX)

mrkiss_eerk_dormand_prince_5_4$(OBJ_SUFFIX) mrkiss_eerk_dormand_prince_5_4.mod &: $(MRKISS_PATH)/lib/mrkiss_eerk_dormand_prince_5_4.f90 mrkiss_config.mod
	rm -f $(basename $@)$(OBJ_SUFFIX) $(basename $@).mod
	$(FC) $(FFLAGS) -c $< -o $(basename $@)$(OBJ_SUFFIX)

mrkiss_eerk_cash_karp_5_4$(OBJ_SUFFIX) mrkiss_eerk_cash_karp_5_4.mod &: $(MRKISS_PATH)/lib/mrkiss_eerk_cash_karp_5_4.f90 mrkiss_config.mod
	rm -f $(basename $@)$(OBJ_SUFFIX) $(basename $@).mod
	$(FC) $(FFLAGS) -c $< -o $(basename $@)$(OBJ_SUFFIX)

mrkiss_eerk_bogacki_shampine_3_2$(OBJ_SUFFIX) mrkiss_eerk_bogacki_shampine_3_2.mod &: $(MRKISS_PATH)/lib/mrkiss_eerk_bogacki_shampine_3_2.f90 mrkiss_config.mod
	rm -f $(basename $@)$(OBJ_SUFFIX) $(basename $@).mod
	$(FC) $(FFLAGS) -c $< -o $(basename $@)$(OBJ_SUFFIX)

mrkiss_eerk_bogacki_shampine_4_5$(OBJ_SUFFIX) mrkiss_eerk_bogacki_shampine_4_5.mod &: $(MRKISS_PATH)/lib/mrkiss_eerk_bogacki_shampine_4_5.f90 mrkiss_config.mod
	rm -f $(basename $@)$(OBJ_SUFFIX) $(basename $@).mod
	$(FC) $(FFLAGS) -c $< -o $(basename $@)$(OBJ_SUFFIX)

mrkiss_eerk_fehlberg_4_5$(OBJ_SUFFIX) mrkiss_eerk_fehlberg_4_5.mod &: $(MRKISS_PATH)/lib/mrkiss_eerk_fehlberg_4_5.f90 mrkiss_config.mod
	rm -f $(basename $@)$(OBJ_SUFFIX) $(basename $@).mod
	$(FC) $(FFLAGS) -c $< -o $(basename $@)$(OBJ_SUFFIX)

mrkiss_eerk_fehlberg_7_8$(OBJ_SUFFIX) mrkiss_eerk_fehlberg_7_8.mod &: $(MRKISS_PATH)/lib/mrkiss_eerk_fehlberg_7_8.f90 mrkiss_config.mod
	rm -f $(basename $@)$(OBJ_SUFFIX) $(basename $@).mod
	$(FC) $(FFLAGS) -c $< -o $(basename $@)$(OBJ_SUFFIX)

mrkiss_erk_midpoint_2$(OBJ_SUFFIX) mrkiss_erk_midpoint_2.mod &: $(MRKISS_PATH)/lib/mrkiss_erk_midpoint_2.f90 mrkiss_config.mod
	rm -f $(basename $@)$(OBJ_SUFFIX) $(basename $@).mod
	$(FC) $(FFLAGS) -c $< -o $(basename $@)$(OBJ_SUFFIX)

mrkiss_erk_kutta_4$(OBJ_SUFFIX) mrkiss_erk_kutta_4.mod &: $(MRKISS_PATH)/lib/mrkiss_erk_kutta_4.f90 mrkiss_config.mod
	rm -f $(basename $@)$(OBJ_SUFFIX) $(basename $@).mod
	$(FC) $(FFLAGS) -c $< -o $(basename $@)$(OBJ_SUFFIX)

mrkiss_erk_kutta_three_eight_4$(OBJ_SUFFIX) mrkiss_erk_kutta_three_eight_4.mod &: $(MRKISS_PATH)/lib/mrkiss_erk_kutta_three_eight_4.f90 mrkiss_config.mod
	rm -f $(basename $@)$(OBJ_SUFFIX) $(basename $@).mod
	$(FC) $(FFLAGS) -c $< -o $(basename $@)$(OBJ_SUFFIX)

mrkiss_solvers_wt$(OBJ_SUFFIX) mrkiss_solvers_wt.mod &: $(MRKISS_PATH)/lib/mrkiss_solvers_wt.f90 mrkiss_config.mod
	rm -f $(basename $@)$(OBJ_SUFFIX) $(basename $@).mod
	$(FC) $(FFLAGS) -c $< -o $(basename $@)$(OBJ_SUFFIX)

mrkiss_solvers_nt$(OBJ_SUFFIX) mrkiss_solvers_nt.mod &: $(MRKISS_PATH)/lib/mrkiss_solvers_nt.f90 mrkiss_config.mod
	rm -f $(basename $@)$(OBJ_SUFFIX) $(basename $@).mod
	$(FC) $(FFLAGS) -c $< -o $(basename $@)$(OBJ_SUFFIX)

$(MRKISS_STATIC_LIB_FILE) : $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(AR) rcs $(MRKISS_STATIC_LIB_FILE) $(MRKISS_OBJ_FILES)

$(MRKISS_SHARED_LIB_FILE) : $(MRKISS_MOD_FILES) $(MRKISS_OBJ_FILES)
	$(FC) $(FSHFLG)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
.PHONY: all_mrkiss_lib
all_mrkiss_lib : $(MRKISS_LIB_FILES)

.PHONY: all_mrkiss_mod
all_mrkiss_mod : $(MRKISS_MOD_FILES)

.PHONY: all_mrkiss_obj
all_mrkiss_obj : $(MRKISS_OBJ_FILES)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
.PHONY: clean_mrkiss_mod
clean_mrkiss_mod :
	rm -f $(MRKISS_MOD_FILES)

.PHONY: clean_mrkiss_obj
clean_mrkiss_obj :
	rm -f $(MRKISS_OBJ_FILES)

.PHONY: clean_mrkiss_lib
clean_mrkiss_lib :
	rm -f $(MRKISS_LIB_FILES)

.PHONY: clean_mrkiss
clean_mrkiss : clean_mrkiss_obj clean_mrkiss_mod clean_mrkiss_lib

.PHONY: clean_multi_mrkiss
clean_multi_mrkiss :
	rm -f $(addsuffix .mod,$(MRKISS_MODS))
	rm -f $(addsuffix .obj,$(MRKISS_MODS))
	rm -f $(addsuffix .o,$(MRKISS_MODS))
	rm -f libmrkiss.so
	rm -f libmrkiss.dll
	rm -f libmrkiss.a

.PHONY: clean_source_mrkiss
clean_source_mrkiss :
	rm -f $(MRKISS_PATH)/lib/mrkiss_solvers_nt.f90

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
