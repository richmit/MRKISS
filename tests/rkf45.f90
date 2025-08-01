! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      rkf45.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @brief     Test code for RKF45.@EOL
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
program rkf45
  use :: mrkiss_config,            only: rk, ik
  use :: mrkiss_solvers_wt,        only: one_step_rkf45_wt, one_step_etab_wt, one_step_stab_wt
  use :: mrkiss_utils,             only: print_solution
  use :: mrkiss_eerk_fehlberg_4_5, only: a, b1, b2, p1, p2, c

  implicit none

  integer(kind=ik),  parameter :: max_step      = 11
  integer(kind=ik),  parameter :: deq_dim       = 1
  real(kind=rk),     parameter :: param(1)      = [1.0_rk]
  real(kind=rk),     parameter :: t_iv          = 0.0_rk
  real(kind=rk),     parameter :: t_delta       = 0.1_rk
  real(kind=rk),     parameter :: y_iv(deq_dim) = [1.0_rk]

  integer(kind=ik)             :: step, status
  real(kind=rk)                :: y_delta(deq_dim), y_tmp(deq_dim), sol(1+2*deq_dim, max_step)

  sol = 0
  sol(1:2,1) = [ t_iv, y_iv ]
  do step=2,max_step
     call one_step_rkf45_wt(status, y_delta, y_tmp, sol(3:3,step-1), eq, sol(1,step-1), sol(2:2,step-1), param, t_delta)
     sol(1:2,step) = sol(1:2,step-1) + [ t_delta, y_delta ]
  end do
  call eq(status, sol(3:3,step-1), sol(1,step-1), sol(2:2,step-1), param)
  call print_solution(status, sol, filename_o="rkf45_ref_5.out", width_o=20)

  sol = 0
  sol(1:2,1) = [ t_iv, y_iv ]
  do step=2,max_step
     call one_step_rkf45_wt(status, y_tmp, y_delta, sol(3:3,step-1), eq, sol(1,step-1), sol(2:2,step-1), param, t_delta)
     sol(1:2,step) = sol(1:2,step-1) + [ t_delta, y_delta ]
  end do
  call eq(status, sol(3:3,step-1), sol(1,step-1), sol(2:2,step-1), param)
  call print_solution(status, sol, filename_o="rkf45_ref_4.out", width_o=20)

  sol = 0
  sol(1:2,1) = [ t_iv, y_iv ]
  do step=2,max_step
     call one_step_etab_wt(status, y_delta, y_tmp, sol(3:3,step-1), eq, sol(1,step-1), sol(2:2,step-1), param, a, b1, b2, c, t_delta)
     sol(1:2,step) = sol(1:2,step-1) + [ t_delta, y_delta ]
  end do
  call eq(status, sol(3:3,step-1), sol(1,step-1), sol(2:2,step-1), param)
  call print_solution(status, sol, filename_o="rkf45_etab_5.out", width_o=20)

  sol = 0
  sol(1:2,1) = [ t_iv, y_iv ]
  do step=2,max_step
     call one_step_etab_wt(status, y_tmp, y_delta, sol(3:3,step-1), eq, sol(1,step-1), sol(2:2,step-1), param, a, b1, b2, c, t_delta)
     sol(1:2,step) = sol(1:2,step-1) + [ t_delta, y_delta ]
  end do
  call eq(status, sol(3:3,step-1), sol(1,step-1), sol(2:2,step-1), param)
  call print_solution(status, sol, filename_o="rkf45_etab_4.out", width_o=20)

  sol = 0
  sol(1:2,1) = [ t_iv, y_iv ]
  do step=2,max_step
     call one_step_stab_wt(status, y_delta, sol(3:3,step-1), eq, sol(1,step-1), sol(2:2,step-1), param, a, b1, c, t_delta)
     sol(1:2,step) = sol(1:2,step-1) + [ t_delta, y_delta ]
  end do
  call eq(status, sol(3:3,step-1), sol(1,step-1), sol(2:2,step-1), param)
  call print_solution(status, sol, filename_o="rkf45_stab_5.out", width_o=20)

  sol = 0
  sol(1:2,1) = [ t_iv, y_iv ]
  do step=2,max_step
     call one_step_stab_wt(status, y_delta, sol(3:3,step-1), eq, sol(1,step-1), sol(2:2,step-1), param, a, b2, c, t_delta)
     sol(1:2,step) = sol(1:2,step-1) + [ t_delta, y_delta ]
  end do
  call eq(status, sol(3:3,step-1), sol(1,step-1), sol(2:2,step-1), param)
  call print_solution(status, sol, filename_o="rkf45_stab_4.out", width_o=20)

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

end program rkf45
