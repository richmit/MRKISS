! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!.!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      tc1_template.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @brief     Test case.@EOL
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
program tc1_verner_7_6_b1
  use :: mrkiss_config,                      only: rk
  use :: mrkiss_solvers_wt,                  only: one_step_stab
  use :: mrkiss_eerk_verner_7_6,             only: a, b=>b1, c   ! TCASE_COM: verner_7_6_b1

  implicit none

  integer,           parameter :: deq_dim       = 1
  integer,           parameter :: max_steps     = 30
  real(kind=rk),     parameter :: t_end         = 3.0_rk
  real(kind=rk),     parameter :: t_delta       = t_end / (max_steps - 1)
  real(kind=rk),     parameter :: param(1)      = [0.0_rk]
  real(kind=rk),     parameter :: t_iv          = 0.0_rk
  real(kind=rk),     parameter :: y_iv(deq_dim) = [1.0_rk]

  integer          :: step, status
  real(kind=rk)    :: y_delta(deq_dim), y_cv(deq_dim), t_cv, y_tmp(deq_dim), dy(deq_dim)
  integer          :: out_io_stat, out_io_unit

  character(len=*), parameter  :: fmt = "(a40,i5,f23.17,f23.17,f23.17,f23.17)"

  open(newunit=out_io_unit, file="tc1_verner_7_6_b1.out", form='formatted', action='write', iostat=out_io_stat)
  t_cv = t_iv
  y_cv = y_iv
  do step=1,max_steps
     call ysol(status, y_tmp, t_cv, param)
     write (out_io_unit, fmt=fmt) "verner_7_6_b1", step, t_cv, y_cv, y_tmp, abs(y_tmp-y_cv)
     call one_step_stab(status, y_delta, dy, eq, t_cv, y_cv, param, a, b, c, t_delta=t_delta)
     t_cv = t_cv + t_delta
     y_cv = y_cv + y_delta
  end do
  close(unit=out_io_unit, status='keep', iostat=out_io_stat)

contains

  subroutine eq(status, dydt, t, y, param)
    integer,          intent(out) :: status
    real(kind=rk),    intent(out) :: dydt(:)
    real(kind=rk),    intent(in)  :: t
    real(kind=rk),    intent(in)  :: y(:)
    real(kind=rk),    intent(in)  :: param(:)
    dydt(1) = -2*t*y(1)
    status = 0
  end subroutine eq

  subroutine ysol(status, y, t, param)
    integer,          intent(out) :: status
    real(kind=rk),    intent(out) :: y(:)
    real(kind=rk),    intent(in)  :: t
    real(kind=rk),    intent(in)  :: param(:)
    y = exp(-t**2)
    status = 0
  end subroutine ysol

end program tc1_verner_7_6_b1
