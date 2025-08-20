! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!.!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      step_too_far.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @brief     Demonstrate step size and order on accuracy.@EOL
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
!! @filedetails
!!
!!  In this example we solve a simple equation with various step sizes in order to observe the relationship between step size and
!!  accuracy.
!!
!!  The system we solve:
!!     @f[ y'(t)=e^t + y(t) \,\,\,\mathrm{with}\,\,\, y(0)=0 @f]
!!
!!  We can solve this equation symbolically:
!!     @f[ y(t) = te^t  @f]
!!
!!  By construction, the truncation error for an RK method decreases as the step size decreases.  Round-off error on the other
!!  hand increases as the step size decreases.  Total error is the sum of truncation and round-off error.  In this experiment we
!!  directly measure total error.  For moderate step sizes we observe truncation error dominating the total error.  As the step
!!  size gets smaller, we see the total error continue to improve as expected; however, the nice smooth response curve begins to
!!  roughen up a bit with what looks like random noise.  Eventually we reach small enough step sizes that round-off error begins
!!  to dominate the results, and accuracy degrades as step size continues to decrease. The point at which this happens is very
!!  much dependent upon the RK method, the problem, and how the method is implemented.  This last point is important.  Simply
!!  rearranging the order of operations can have dramatic impact on final error.
!!
!.!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.E.!!

!----------------------------------------------------------------------------------------------------------------------------------
program step_too_far

  use :: mrkiss_config,          only: rk, istats_size
  use :: mrkiss_solvers_wt,      only: steps_fixed_stab
  use :: mrkiss_utils,           only: print_solution
  use :: mrkiss_erk_kutta_4,     only: a, b, c

  implicit none

  integer,        parameter :: deq_dim       = 1
  real(kind=rk),  parameter :: y_iv(deq_dim) = [0.0_rk]
  real(kind=rk),  parameter :: t_iv          = 0.0_rk
  real(kind=rk),  parameter :: param(1)      = [0.0_rk]
  real(kind=rk),  parameter :: t_end         = 1.0_rk

  real(kind=rk)             :: solution(1+2*deq_dim, 1)
  integer                   :: status, istats(istats_size), sso, num_pts
  logical                   :: fi

  fi = .true.
  do sso = 1000,2100
     num_pts = 1.005_rk ** sso
     print '("sso=",i4," num_pts=",i0)', sso, num_pts
     call steps_fixed_stab(status, istats, solution, eq, t_iv, y_iv, param, a, b, c, max_pts_o=num_pts, t_end_o=t_end)
     call print_solution(status, solution, filename_o="step_too_far.csv", tag_o=sso, prt_titles_o=fi, append_o=.not. fi)
     fi = .false.
  end do

contains

  subroutine eq(status, dydt, t, y, param)
    integer,          intent(out) :: status
    real(kind=rk),    intent(out) :: dydt(:)
    real(kind=rk),    intent(in)  :: t
    real(kind=rk),    intent(in)  :: y(:)
    real(kind=rk),    intent(in)  :: param(:)
    dydt = [ exp(t) + y(1) ]
    status = 0
  end subroutine eq

end program step_too_far
