! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      rk4_ref_vs_tab.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @brief     Test RK4 code.@EOL
!! @std       F2023
!! @see       https://github.com/richmit/MRKISS/
!! @copyright 
!!  @parblock
!!  Copyright (c) 2025, Mitchell Jay Richling <http://www.mitchr.me/> All rights reserved.
!!  
!!  Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following
!!  conditions are met:
!!  
!!  1. Redistributions of source code must retain the above copyright notice, this list of conditions, and the following
!!     disclaimer.
!!  
!!  2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions, and the following
!!     disclaimer in the documentation and/or other materials provided with the distribution.
!!  
!!  3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products
!!     derived from this software without specific prior written permission.
!!  
!!  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
!!  INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
!!  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
!!  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
!!  USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
!!  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
!!  OF THE POSSIBILITY OF SUCH DAMAGE.
!!  @endparblock
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.E.!!

!----------------------------------------------------------------------------------------------------------------------------------
program rk4_ref_vs_tab
  use, intrinsic :: iso_fortran_env,    only: output_unit, error_unit
  use            :: mrkiss_config,      only: rk, ik
  use            :: mrkiss_solvers_wt,  only: one_step_rk4_wt, one_step_stab_wt, steps_fixed_stab_wt
  use            :: mrkiss_utils,       only: print_t_y_sol
  use            :: mrkiss_erk_kutta_4, only: a, b, c

  implicit none
  integer(kind=ik),  parameter :: max_step = 11
  integer(kind=ik),  parameter :: deq_dim  = 1
  real(kind=rk),     parameter :: param(1) = [1.0_rk]
  real(kind=rk),     parameter :: t_iv = 0.0_rk
  real(kind=rk),     parameter :: t_delta = 0.1_rk
  real(kind=rk),     parameter :: y_iv(deq_dim) = [1.0_rk]
  real(kind=rk),     parameter :: y_hwrk(11) = [ 1.0000000000, 0.9655827899, 0.9377962750, 0.9189181059, 0.9104421929, &
                                                 0.9130598390, 0.9267065986, 0.9506796142, 0.9838057659, 1.0246280460, 1.0715783953 ]

  integer(kind=ik)             :: step, status, istats(16)
  real(kind=rk)                :: y_delta(deq_dim), y_cv(deq_dim), t_cv, t_y_sol(1+deq_dim, max_step)
  integer                      :: out_io_stat, out_io_unit

  character(len=*), parameter  :: fmt = "(i5,f20.15,f20.15)"

  open(newunit=out_io_unit, file="rk4_ref_vs_tab_hnd.out", form='formatted', action='write', iostat=out_io_stat)
  t_cv = t_iv
  do step=1,max_step
     write (out_io_unit, fmt=fmt) step, t_cv, y_hwrk(step)
     call one_step_rk4_wt(status, y_delta, eq, t_cv, y_cv, param, t_delta)
     t_cv = t_cv + t_delta
  end do
  close(unit=out_io_unit, status='keep', iostat=out_io_stat)

  open(newunit=out_io_unit, file="rk4_ref_vs_tab_ref.out", form='formatted', action='write', iostat=out_io_stat)
  y_cv = y_iv
  t_cv = t_iv
  do step=1,max_step
     write (out_io_unit, fmt=fmt) step, t_cv, y_cv
     call one_step_rk4_wt(status, y_delta, eq, t_cv, y_cv, param, t_delta)
     y_cv = y_cv + y_delta
     t_cv = t_cv + t_delta
  end do
  close(unit=out_io_unit, status='keep', iostat=out_io_stat)

  open(newunit=out_io_unit, file="rk4_ref_vs_tab_stab.out", form='formatted', action='write', iostat=out_io_stat)
  y_cv = y_iv
  t_cv = t_iv
  do step=1,max_step
     write (out_io_unit, fmt=fmt) step, t_cv, y_cv
     call one_step_stab_wt(status, y_delta, eq, t_cv, y_cv, param, a, b, c, t_delta)
     y_cv = y_cv + y_delta
     t_cv = t_cv + t_delta
  end do
  close(unit=out_io_unit, status='keep', iostat=out_io_stat)

  call steps_fixed_stab_wt(status, istats, t_y_sol, eq, t_iv, y_iv, param, a, b, c, t_delta_o=t_delta)
  call print_t_y_sol(status, t_y_sol, filename_o="rk4_ref_vs_tab_steps.out", end_o=istats(1), width_o=19, no_titles_o=1)

contains

  subroutine eq(status, dydt, t, y, param)
    integer(kind=ik), intent(out) :: status
    real(kind=rk),    intent(out) :: dydt(:)
    real(kind=rk),    intent(in)  :: t
    real(kind=rk),    intent(in)  :: y(:)
    real(kind=rk),    intent(in)  :: param(:)
    dydt(1) = (5*t**2-y(1))/exp(t+y(1))
    status = 0
  end subroutine eq

end program rk4_ref_vs_tab
