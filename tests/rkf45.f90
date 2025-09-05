! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!.!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
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
!.!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.E.!!

!----------------------------------------------------------------------------------------------------------------------------------
program rkf45
  use :: mrkiss_config,                  only: rk
  use :: mrkiss_solvers_wt,              only: one_step_rkf45, one_step_one, one_step_all
  use :: mrkiss_utils,                   only: print_solution
  use :: mrkiss_eerk_fehlberg_4_5,       only: a, b, c

  implicit none

  integer,           parameter :: max_step      = 11
  integer,           parameter :: deq_dim       = 1
  real(kind=rk),     parameter :: param(1)      = [1.0_rk]
  real(kind=rk),     parameter :: t_iv          = 0.0_rk
  real(kind=rk),     parameter :: t_delta       = 0.1_rk
  real(kind=rk),     parameter :: y_iv(deq_dim) = [1.0_rk]

  integer                      :: step, status
  real(kind=rk)                :: y_deltas(deq_dim,size(b, 2)), y_delta(deq_dim)
  real(kind=rk)                :: sol(1+2*deq_dim, max_step)

  sol = 0
  sol(1:2,1) = [ t_iv, y_iv ]
  do step=2,max_step
     call one_step_rkf45(status, y_deltas, sol(3:3,step-1), eq, sol(1,step-1), sol(2:2,step-1), param, t_delta)
     sol(1:2,step) = sol(1:2,step-1) + [ t_delta, y_deltas(:,1) ]
  end do
  call eq(status, sol(3:3,step-1), sol(1,step-1), sol(2:2,step-1), param)
  call print_solution(status, sol, filename_o="rkf45_ref_4.out", fmt_w_o=-1)

  sol = 0
  sol(1:2,1) = [ t_iv, y_iv ]
  do step=2,max_step
     call one_step_rkf45(status, y_deltas, sol(3:3,step-1), eq, sol(1,step-1), sol(2:2,step-1), param, t_delta)
     sol(1:2,step) = sol(1:2,step-1) + [ t_delta, y_deltas(:,2) ]
  end do
  call eq(status, sol(3:3,step-1), sol(1,step-1), sol(2:2,step-1), param)
  call print_solution(status, sol, filename_o="rkf45_ref_5.out", fmt_w_o=-1)

  sol = 0
  sol(1:2,1) = [ t_iv, y_iv ]
  do step=2,max_step
     call one_step_all(status, y_deltas, sol(3:3,step-1), eq, sol(1,step-1), sol(2:2,step-1), param, a, b, c, t_delta)
     sol(1:2,step) = sol(1:2,step-1) + [ t_delta, y_deltas(:,1) ]
  end do
  call eq(status, sol(3:3,step-1), sol(1,step-1), sol(2:2,step-1), param)
  call print_solution(status, sol, filename_o="rkf45_all_4.out", fmt_w_o=-1)

  sol = 0
  sol(1:2,1) = [ t_iv, y_iv ]
  do step=2,max_step
     call one_step_all(status, y_deltas, sol(3:3,step-1), eq, sol(1,step-1), sol(2:2,step-1), param, a, b, c, t_delta)
     sol(1:2,step) = sol(1:2,step-1) + [ t_delta, y_deltas(:,2) ]
  end do
  call eq(status, sol(3:3,step-1), sol(1,step-1), sol(2:2,step-1), param)
  call print_solution(status, sol, filename_o="rkf45_all_5.out", fmt_w_o=-1)

  sol = 0
  sol(1:2,1) = [ t_iv, y_iv ]
  do step=2,max_step
     call one_step_one(status, y_delta, sol(3:3,step-1), eq, sol(1,step-1), sol(2:2,step-1), param, a, b(:,1:1), c, t_delta)
     sol(1:2,step) = sol(1:2,step-1) + [ t_delta, y_delta ]
  end do
  call eq(status, sol(3:3,step-1), sol(1,step-1), sol(2:2,step-1), param)
  call print_solution(status, sol, filename_o="rkf45_one_4.out", fmt_w_o=-1)

  sol = 0
  sol(1:2,1) = [ t_iv, y_iv ]
  do step=2,max_step
     call one_step_one(status, y_delta, sol(3:3,step-1), eq, sol(1,step-1), sol(2:2,step-1), param, a, b(:,2:2), c, t_delta)
     sol(1:2,step) = sol(1:2,step-1) + [ t_delta, y_delta ]
  end do
  call eq(status, sol(3:3,step-1), sol(1,step-1), sol(2:2,step-1), param)
  call print_solution(status, sol, filename_o="rkf45_one_5.out", fmt_w_o=-1)

contains

  subroutine eq(status, dydt, t, y, param)
    integer,          intent(out) :: status
    real(kind=rk),    intent(out) :: dydt(:)
    real(kind=rk),    intent(in)  :: t
    real(kind=rk),    intent(in)  :: y(:)
    real(kind=rk),    intent(in)  :: param(:)
    dydt(1) = (5*t**2-y(1))/exp(t+y(1))
    status = 0
  end subroutine eq

end program rkf45
