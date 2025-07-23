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
  use, intrinsic :: iso_fortran_env,   only: output_unit, error_unit
  use            :: mrkiss_config,     only: rk, ik, t_delta_tiny
  use            :: mrkiss_solvers_nt, only: steps_fixed_stab_nt, steps_condy_stab_nt, steps_adapt_etab_nt
  use            :: mrkiss_utils,      only: print_t_y_sol
  use            :: mrkiss_erk_kutta_4,  only: a, b, c
!  use            :: mrkiss_eerk_dormand_prince_5_4,  only: a, b=>b1, c

  implicit none

  integer,        parameter :: deq_dim       = 2
  integer,        parameter :: num_points    = 10000
  real(kind=rk),  parameter :: y_iv(deq_dim) = [1.5_rk, 3.0_rk]
  real(kind=rk),  parameter :: param(1)      = [0.0_rk]
  real(kind=rk),  parameter :: t_end         = 20.0_rk

  real(kind=rk)             :: t_y_sol(1+deq_dim, num_points)
  integer(kind=ik)          :: status, istats(16)
  integer                   :: c_beg, c_end, c_rate

  ! Call the solver, and time how long it takes.
  call system_clock(count_rate=c_rate)
  call system_clock(c_beg)
  call steps_fixed_stab_nt(status, istats, t_y_sol, eq, y_iv, param, a, b, c, t_end_o=t_end)
  call system_clock(c_end)

  print '(a,f10.3)', "             Milliseconds: ", 1000*(c_end-c_beg)/DBLE(c_rate)
  print '(a,i10)',   "          Solution Points: ", istats(1)
  print '(a,i10)',   "     Total one_step calls: ", istats(2)
  print '(a,i10)',   "Adjustment one_step calls: ", istats(3)

  ! Positive values for status indicate an error.
  if (status > 0) then
     print '(a)', "Something went wrong!"
     error stop
  end if

  call print_t_y_sol(status, t_y_sol, filename_o="brusselator.csv", end_o=istats(1))

contains
  
  subroutine eq(status, dydt, y, param)
    integer(kind=ik), intent(out) :: status
    real(kind=rk),    intent(out) :: dydt(:)
    real(kind=rk),    intent(in)  :: y(:)
    real(kind=rk),    intent(in)  :: param(:)
    dydt(1) = 1+y(1)**2*y(2)-4*y(1)
    dydt(2) = 3*y(1)-y(1)**2*y(2)
    status = 0
  end subroutine eq

end program
