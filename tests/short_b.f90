! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      short_b.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @brief     Test reduced stage count when b is smaller a.@EOL
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
program short_b
  use, intrinsic :: iso_fortran_env,                  only: output_unit, error_unit
  use            :: mrkiss_config,                    only: rk, ik, bk
  use            :: mrkiss_solvers_wt,                only: steps_fixed_stab_wt
  use            :: mrkiss_utils,                     only: print_solution
  use            :: mrkiss_eerk_bogacki_shampine_3_2, only: a, b=>b1, c, s1

  implicit none

  integer(kind=ik),  parameter :: num_points = 11
  integer(kind=ik),  parameter :: deq_dim  = 1
  real(kind=rk),     parameter :: param(1) = [1.0_rk]
  real(kind=rk),     parameter :: t_iv = 0.0_rk
  real(kind=rk),     parameter :: t_delta = 0.1_rk
  real(kind=rk),     parameter :: y_iv(deq_dim) = [1.0_rk]

  real(kind=rk)                :: solution(1+deq_dim, num_points)
  integer(kind=ik)             :: status, istats(16)

  call steps_fixed_stab_wt(status, istats, solution, eq, t_iv, y_iv, param, a, b, c, t_delta_o=t_delta, sol_w_dy_o=.false._bk)
  call print_solution(status, solution, filename_o="short_b_all.out", end_o=istats(1), sol_w_dy_o=.false._bk)

  call steps_fixed_stab_wt(status, istats, solution, eq, t_iv, y_iv, param, a, b(1:s1), c, t_delta_o=t_delta, sol_w_dy_o=.false._bk)
  call print_solution(status, solution, filename_o="short_b_sub.out", end_o=istats(1), sol_w_dy_o=.false._bk)

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

end program short_b
