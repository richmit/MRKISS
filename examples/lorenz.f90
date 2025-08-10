! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      lorenz.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @brief     MRKISS example solving Lorenz system.@EOL
!! @keywords  Runge Kutta ODE IVP initial value problem ordinary differential equation numerical
!! @std       F2023
!! @see       https://github.com/richmit/MRKISS
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
program lorenz
  use :: mrkiss_config,      only: rk, t_delta_tiny, istats_size
  use :: mrkiss_solvers_nt,  only: steps_fixed_stab, steps_sloppy_condy_stab, steps_condy_stab
  use :: mrkiss_utils,       only: print_solution
  use :: mrkiss_erk_kutta_4, only: a, b, c

  implicit none

  integer,        parameter :: deq_dim       = 3
  integer,        parameter :: num_points    = 100000
  real(kind=rk),  parameter :: y_iv(deq_dim) = [1.0_rk, 0.0_rk, 0.0_rk]
  real(kind=rk),  parameter :: param(3)      = [10.0_rk, 28.0_rk, 8.0_rk/3.0_rk]
  real(kind=rk),  parameter :: t_delta       = 0.01_rk
  real(kind=rk),  parameter :: t_max         = 100.0_rk

  real(kind=rk)             :: solution(1+2*deq_dim, num_points)
  integer                   :: status, istats(istats_size)
  integer                   :: c_beg, c_end, c_rate

  ! BEGIN: lorenz_fixed
  ! This solution will have fixed t-delta, but no control over y-delta.
  call system_clock(count_rate=c_rate)
  call system_clock(c_beg)
  call steps_fixed_stab(status, istats, solution, eq, y_iv, param, a, b, c, t_delta_o=t_delta, t_max_o=t_max)
  call system_clock(c_end)
  print '(a,f10.3)', "fixed                      "
  print '(a,i10)',   "                   Status: ", status
  print '(a,f10.3)', "             Milliseconds: ", 1000*(c_end-c_beg)/DBLE(c_rate)
  print '(a,i10)',   "          Solution Points: ", istats(1)
  print '(a,i10)',   "   Regular one_step calls: ", istats(2)
  print '(a,i10)',   "Adjustment one_step calls: ", istats(3)
  call print_solution(status, solution, filename_o="lorenz_fixed.csv", end_o=istats(1), t_min_o=50.0_rk)
  ! END: lorenz_fixed

  ! BEGIN: lorenz_fixed-y
  ! This solution will have y-delta approximately capped to a maximum of 1.0 for all steps.
  call system_clock(count_rate=c_rate)
  call system_clock(c_beg)
  call steps_sloppy_condy_stab(status, istats, solution, eq, y_iv, param, a, b, c, 1.0_rk, t_delta, t_max_o=t_max)
  call system_clock(c_end)
  print '(a,f10.3)', "sloppy_condy               "
  print '(a,i10)',   "                   Status: ", status
  print '(a,f10.3)', "             Milliseconds: ", 1000*(c_end-c_beg)/DBLE(c_rate)
  print '(a,i10)',   "          Solution Points: ", istats(1)
  print '(a,i10)',   "   Regular one_step calls: ", istats(2)
  print '(a,i10)',   "Adjustment one_step calls: ", istats(3)
  call print_solution(status, solution, filename_o="lorenz_sloppy_condy.csv", end_o=istats(1), t_min_o=50.0_rk)

  ! This solution will have y-delta approximately equal to 1.0 for all steps.
  call system_clock(count_rate=c_rate)
  call system_clock(c_beg)
  call steps_sloppy_condy_stab(status, istats, solution, eq, y_iv, param, a, b, c, 1.0_rk, t_delta, t_max_o=t_max, &
                                  adj_short_o=1)
  call system_clock(c_end)
  print '(a,f10.3)', "sloppy_condy short         "
  print '(a,i10)',   "                   Status: ", status
  print '(a,f10.3)', "             Milliseconds: ", 1000*(c_end-c_beg)/DBLE(c_rate)
  print '(a,i10)',   "          Solution Points: ", istats(1)
  print '(a,i10)',   "   Regular one_step calls: ", istats(2)
  print '(a,i10)',   "Adjustment one_step calls: ", istats(3)
  call print_solution(status, solution, filename_o="lorenz_sloppy_condy_short.csv", end_o=istats(1), t_min_o=50.0_rk)
  ! END: lorenz_fixed-y

  ! BEGIN: lorenz_clip-y
  ! This solution will have y-delta equal to 1.0 for all steps.
  call system_clock(count_rate=c_rate)
  call system_clock(c_beg)
  call steps_condy_stab(status, istats, solution, eq, y_iv, param, a, b, c, 1.0_rk, t_delta*7, t_max_o=t_max)
  call system_clock(c_end)
  print '(a,f10.3)', "condy                      "
  print '(a,i10)',   "                   Status: ", status
  print '(a,f10.3)', "             Milliseconds: ", 1000*(c_end-c_beg)/DBLE(c_rate)
  print '(a,i10)',   "          Solution Points: ", istats(1)
  print '(a,i10)',   "   Regular one_step calls: ", istats(2)
  print '(a,i10)',   "Adjustment one_step calls: ", istats(3)
  call print_solution(status, solution, filename_o="lorenz_condy.csv", end_o=istats(1), t_min_o=50.0_rk)
  ! END: lorenz_clip-y

contains
  
  subroutine eq(status, dydt, y, param)
    integer,          intent(out) :: status
    real(kind=rk),    intent(out) :: dydt(:)
    real(kind=rk),    intent(in)  :: y(:)
    real(kind=rk),    intent(in)  :: param(:)
    dydt(1) = param(1)*(y(2)-y(1))       ! a(y-x)
    dydt(2) = y(1)*(param(2)-y(3))-y(2)  ! x(b-z)-y
    dydt(3) = y(1)*y(2)-param(3)*y(3)    ! xy-cy
    status = 0
  end subroutine eq

end program
