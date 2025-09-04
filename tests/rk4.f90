! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!.!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      rk4.f90
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
!.!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.E.!!

!----------------------------------------------------------------------------------------------------------------------------------
program rk4
  use            :: mrkiss_config,      only: rk, istats_size
  use            :: mrkiss_solvers_wt,  only: one_step_rk4, one_step, fixed_t_steps, fixed_t_steps_between
  use            :: mrkiss_utils,       only: print_solution
  use            :: mrkiss_erk_kutta_4, only: a, b, c

  implicit none
  integer,          parameter :: max_pts                     = 11
  integer,          parameter :: deq_dim                     = 1
  real(kind=rk),    parameter :: param(1)                    = [1.0_rk]
  real(kind=rk),    parameter :: t_iv                        = 0.0_rk
  real(kind=rk),    parameter :: t_delta                     = 0.1_rk
  real(kind=rk),    parameter :: t_end                       = 1.0_rk
  real(kind=rk),    parameter :: y_iv(deq_dim)               = [1.0_rk]
  real(kind=rk),    parameter :: sol_h(1+2*deq_dim, max_pts) = reshape([ 0.0000000000_rk, 1.0000000000_rk, -0.3678794411_rk, &
                                                                         0.1000000000_rk, 0.9655827899_rk, -0.3154430003_rk, &
                                                                         0.2000000000_rk, 0.9377962750_rk, -0.2364818517_rk, &
                                                                         0.3000000000_rk, 0.9189181059_rk, -0.1385886320_rk, &
                                                                         0.4000000000_rk, 0.9104421929_rk, -0.0297863398_rk, &
                                                                         0.5000000000_rk, 0.9130598390_rk,  0.0820103570_rk, &
                                                                         0.6000000000_rk, 0.9267065986_rk,  0.1897229700_rk, &
                                                                         0.7000000000_rk, 0.9506796142_rk,  0.2877487252_rk, &
                                                                         0.8000000000_rk, 0.9838057659_rk,  0.3723152399_rk, &
                                                                         0.9000000000_rk, 1.0246280460_rk,  0.4414926096_rk, &
                                                                         1.0000000000_rk, 1.0715783953_rk,  0.4949291477_rk], [1+2*deq_dim, max_pts])
  integer                     :: step, status, istats(istats_size)
  real(kind=rk)               :: y_deltas(deq_dim, size(b, 2)), sol(1+2*deq_dim, max_pts)

  call print_solution(status, sol_h, filename_o="rk4_hnd.out", fmt_w_o=-1)

  sol = 0
  sol(1:2,1) = [ t_iv, y_iv ]
  do step=2,max_pts
     call one_step_rk4(status, y_deltas, sol(3:3,step-1), eq, sol(1,step-1), sol(2:2,step-1), param, t_delta)
     sol(1:2,step) = sol(1:2,step-1) + [ t_delta, y_deltas(:,1) ]
  end do
  call eq(status, sol(3:3,step-1), sol(1,step-1), sol(2:2,step-1), param)
  call print_solution(status, sol, filename_o="rk4_ref.out", fmt_w_o=-1)

  sol = 0
  sol(1:2,1) = [ t_iv, y_iv ]
  do step=2,max_pts
     call one_step(status, y_deltas, sol(3:3,step-1), eq, sol(1,step-1), sol(2:2,step-1), param, a, b, c, t_delta)
     sol(1:2,step) = sol(1:2,step-1) + [ t_delta, y_deltas(:,1) ]
  end do
  call eq(status, sol(3:3,step-1), sol(1,step-1), sol(2:2,step-1), param)
  call print_solution(status, sol, filename_o="rk4_one.out", fmt_w_o=-1)

  sol = 0
  call fixed_t_steps(status, istats, sol, eq, t_iv, y_iv, param, a, b, c, t_delta_o=t_delta)
  call print_solution(status, sol, filename_o="rk4_fixed.out", fmt_w_o=-1)

  sol = 0
  call fixed_t_steps(status, istats, sol(:,max_pts:max_pts), eq, t_iv, y_iv, param, a, b, c, t_delta_o=t_delta, max_pts_o=max_pts)
  call print_solution(status, sol, filename_o="rk4_fixed_frog_delta.out", fmt_w_o=-1, start_o=max_pts)

  sol = 0
  call fixed_t_steps(status, istats, sol, eq, t_iv, y_iv, param, a, b, c, t_end_o=t_end)
  call print_solution(status, sol, filename_o="rk4_fixed_end.out", fmt_w_o=-1)

  sol = 0
  call fixed_t_steps(status, istats, sol(:,max_pts:max_pts), eq, t_iv, y_iv, param, a, b, c, t_end_o=t_end, max_pts_o=max_pts)
  call print_solution(status, sol, filename_o="rk4_fixed_frog_end.out", fmt_w_o=-1, start_o=max_pts)

  sol = 0
  call fixed_t_steps(status, istats, sol, eq, t_iv, y_iv, param, a, b, c, t_end_o=t_end)
  sol(2:, :) = 0
  call fixed_t_steps_between(status, istats, sol, eq, y_iv, param, a, b, c, 1)
  call print_solution(status, sol, filename_o="rk4_points1.out", fmt_w_o=-1)

  sol = 0
  call fixed_t_steps(status, istats, sol, eq, t_iv, y_iv, param, a, b, c, t_end_o=t_end)
  sol(2:, :) = 0
  call fixed_t_steps_between(status, istats, sol, eq, y_iv, param, a, b, c, 10)
  call print_solution(status, sol, filename_o="rk4_points10.out", fmt_w_o=-1)

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

end program rk4
