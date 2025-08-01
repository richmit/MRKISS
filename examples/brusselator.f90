! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      lorenz.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @brief     MRKISS example solving Brusselator.@EOL
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
program brusselator
  use, intrinsic :: iso_fortran_env,                 only: output_unit, error_unit
  use            :: mrkiss_config,                   only: rk, ik, t_delta_tiny
  use            :: mrkiss_solvers_nt,               only: steps_fixed_stab_nt, steps_condy_stab_nt, steps_sloppy_condy_stab_nt, steps_adapt_etab_nt
  use            :: mrkiss_utils,                    only: print_solution, analyze_solution
  use            :: mrkiss_eerk_dormand_prince_5_4,  only: a, b1, b2, c, p1, p2

  implicit none

  integer,        parameter :: deq_dim       = 2
  integer,        parameter :: num_points    = 100000
  real(kind=rk),  parameter :: y_iv(deq_dim) = [1.5_rk, 3.0_rk]
  real(kind=rk),  parameter :: param(1)      = [0.0_rk]
  real(kind=rk),  parameter :: t_end         = 20.0_rk

  real(kind=rk)             :: solution(1+2*deq_dim, num_points)
  integer(kind=ik)          :: status, istats(16), i
  integer                   :: c_beg, c_end, c_rate

  ! Call the solver, time how long it takes, report some stuff, and dump out the results.
  call system_clock(count_rate=c_rate)
  call system_clock(c_beg)
  call steps_fixed_stab_nt(status, istats, solution, eq, y_iv, param, a, b1, c, t_end_o=t_end)
  !call steps_condy_stab_nt(status, istats, solution, eq, y_iv, param, a, b1, c, 0.01_rk, 1e-1_rk, y_delta_len_tol_o=1e-15_rk, t_max_o=t_end)
  !call steps_sloppy_condy_stab_nt(status, istats, solution, eq, y_iv, param, a, b1, c, 0.01_rk, 1e-2_rk, t_max_o=t_end)
  !call steps_adapt_etab_nt(status, istats, solution, eq, y_iv, param, a, b1, b2, c, p1, p2, t_delta_max_o=0.1_rk, t_max_o=t_end)
  call system_clock(c_end)

  print '(a)',       ""
  print '(a)',       "Summary of run:                              "
  print '(a,f10.3)', "                               Milliseconds: ", 1000*(c_end-c_beg)/DBLE(c_rate)
  print '(a,i10)',   "                                     Status: ", status
  print '(a,i10)',   "                            Solution Points: ", istats(1)
  print '(a,i10)',   "                      Normal one_step calls: ", istats(2)
  print '(a,i10)',   "   y_delta length constraint one_step calls: ", istats(3)
  print '(a,i10)',   "    y_delta error constraint one_step calls: ", istats(4)
  print '(a,i10)',   "           stepp new t_delta one_step calls: ", istats(5)
  print '(a,i10)',   "               SDF bisection one_step calls: ", istats(6)
  print '(a,i10)',   "            max_bisect_o bisection failures: ", istats(7)
  print '(a,i10)',   "        non-containiment bisection failures: ", istats(8)
  print '(a)',       ""

  ! Positive values for status indicate an error.
  if (status > 0) then
     print '(a)', "Something went wrong!"
     error stop
  end if

  call print_solution(status, solution, filename_o="brusselator.csv", end_o=istats(1))
  call analyze_solution(status, solution, end_o=istats(1))

contains
  
  subroutine eq(status, dydt, y, param)
    integer(kind=ik), intent(out) :: status
    real(kind=rk),    intent(out) :: dydt(:)
    real(kind=rk),    intent(in)  :: y(:)
    real(kind=rk),    intent(in)  :: param(:)
    dydt = [ 1+y(1)**2*y(2)-4*y(1), 3*y(1)-y(1)**2*y(2) ]
    status = 0
  end subroutine eq

end program
