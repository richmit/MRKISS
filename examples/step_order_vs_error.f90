! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      step_order_vs_error.f90
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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.E.!!

!----------------------------------------------------------------------------------------------------------------------------------
program step_order_vs_error

  use :: mrkiss_config,          only: rk, ik, istats_size
  use :: mrkiss_solvers_wt,      only: steps_fixed_stab_wt, steps_points_stab_wt
  use :: mrkiss_utils,           only: print_solution, seq
  use :: mrkiss_erk_kutta_4,     only: a4=>a, b4=>b, c4=>c
  use :: mrkiss_eerk_verner_9_8, only: a9=>a, b9=>b1, c9=>c

  implicit none

  integer,        parameter :: deq_dim       = 1
  integer,        parameter :: num_points    = 10
  real(kind=rk),  parameter :: y_iv(deq_dim) = [0.0_rk]
  real(kind=rk),  parameter :: param(2)      = [1.0_rk, 1.0_rk]
  real(kind=rk),  parameter :: pi            = 4.0_rk * atan(1.0_rk)
  real(kind=rk),  parameter :: t_start       = 0.0_rk
  real(kind=rk),  parameter :: t_end         = 2.0_rk * pi

  real(kind=rk)             :: solution(1+2*deq_dim, num_points)
  integer(kind=ik)          :: status, istats(istats_size), spp
  character(len=512)        :: filename

  call seq(status, solution(1,:), from_o=t_start, to_o=t_end)

  do spp = 1,10
     call steps_points_stab_wt(status, istats, solution, eq, y_iv, param, a4, b4, c4, spp)
     write (filename, '("step_order_vs_error_04_",i2.2,".csv")') spp
     call print_solution(status, solution, filename_o=trim(filename))

     call steps_points_stab_wt(status, istats, solution, eq, y_iv, param, a9, b9, c9, spp)
     write (filename, '("step_order_vs_error_09_",i2.2,".csv")') spp
     call print_solution(status, solution, filename_o=trim(filename))
  end do

contains
  
  subroutine eq(status, dydt, t, y, param)
    integer(kind=ik), intent(out) :: status
    real(kind=rk),    intent(out) :: dydt(:)
    real(kind=rk),    intent(in)  :: t
    real(kind=rk),    intent(in)  :: y(:)
    real(kind=rk),    intent(in)  :: param(:)
    dydt = [ t * param(1)*cos(param(2) * t * t) ]
    status = 0
  end subroutine eq

end program step_order_vs_error
