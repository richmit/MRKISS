! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      rich.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @brief     Test case Richardson extrapolation option.@EOL
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
program tc1_TCASEN
  use :: mrkiss_config,      only: rk, ik
  use :: mrkiss_solvers_wt,  only: one_step_stab_wt, one_richardson_step_stab_wt
  use :: mrkiss_erk_euler_1, only: a, b, c, p

  implicit none

  integer(kind=ik),  parameter :: deq_dim       = 1
  integer(kind=ik),  parameter :: max_steps     = 30
  real(kind=rk),     parameter :: t_end         = 3.0_rk
  real(kind=rk),     parameter :: t_delta       = t_end / (max_steps - 1)
  real(kind=rk),     parameter :: param(1)      = [0.0_rk]
  real(kind=rk),     parameter :: t_iv          = 0.0_rk
  real(kind=rk),     parameter :: y_iv(deq_dim) = [1.0_rk]

  integer(kind=ik)             :: step, status
  real(kind=rk)                :: y_delta(deq_dim), y1_cv(deq_dim), y2_cv(deq_dim), t_cv, y_tru(deq_dim), dy(deq_dim)
  integer                      :: out_io_unit

  character(len=*), parameter  :: fmt = "(i5,f23.17,f23.17,f23.17,f23.17,f23.17,f23.17)"

  open(newunit=out_io_unit, file="rich.out", form='formatted', action='write')
  t_cv  = t_iv
  y1_cv = y_iv
  y2_cv = y_iv
  do step=1,max_steps
     call ysol(status, y_tru, t_cv, param)
     write (out_io_unit, fmt=fmt) step, t_cv, y1_cv, y2_cv, y_tru, abs(y_tru-y1_cv), abs(y_tru-y2_cv)
     call one_step_stab_wt(status, y_delta, dy, eq, t_cv, y1_cv, param, a, b, c, t_delta)
     y1_cv = y1_cv + y_delta
     call one_richardson_step_stab_wt(status, y_delta, dy, eq, t_cv, y2_cv, param, a, b, c, p, t_delta)
     y2_cv = y2_cv + y_delta
     t_cv  = t_cv + t_delta
  end do
  close(unit=out_io_unit, status='keep')

contains

  subroutine eq(status, dydt, t, y, param)
    integer(kind=ik), intent(out) :: status
    real(kind=rk),    intent(out) :: dydt(:)
    real(kind=rk),    intent(in)  :: t
    real(kind=rk),    intent(in)  :: y(:)
    real(kind=rk),    intent(in)  :: param(:)
    dydt(1) = -2*t*y(1)
    status = 0
  end subroutine eq

  subroutine ysol(status, y, t, param)
    integer(kind=ik), intent(out) :: status
    real(kind=rk),    intent(out) :: y(:)
    real(kind=rk),    intent(in)  :: t
    real(kind=rk),    intent(in)  :: param(:)
    y = exp(-t**2)
    status = 0
  end subroutine ysol

end program tc1_TCASEN
