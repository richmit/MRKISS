! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      mrkiss_eerk_bogacki_shampine_3_2.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @brief     Butcher tableau for Bogacki Shampine's 4 step, order (3,2) Runge-Kutta method.@EOL
!! @keywords  ode ivp differential equation initial value problem rk
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
!> Butcher tableau for Bogacki Shampine's 4 step, order (3,2) Runge-Kutta method
!!
!! Known Aliases: 'Shampine3' OrdinaryDiffEq.jl), 'ode23' (MATLAB & Octave), 'ARKODE_BOGACKI_SHAMPINE_4_2_3' (SUNDIALS).
!!
!! References:
!!   Bogacki & Shampine (1989); A 3(2) pair of Runge-Kutta formulas; Applied Mathematics Letters; zotero://select/items/0_66MV7TIP
!!
module mrkiss_eerk_bogacki_shampine_3_2
  use mrkiss_config, only: rk, ik
  implicit none
  public
  integer(kind=ik), parameter :: s      = 4
  real(kind=rk),    parameter :: a(s,s) = reshape([  0.0_rk,  0.0_rk,  0.0_rk, 0.0_rk, &
                                                    18.0_rk,  0.0_rk,  0.0_rk, 0.0_rk, &
                                                     0.0_rk, 27.0_rk,  0.0_rk, 0.0_rk, &
                                                     8.0_rk, 12.0_rk, 16.0_rk, 0.0_rk], [s, s]) / 36.0_rk
  real(kind=rk),    parameter :: c(s)   = [          0.0_rk,  2.0_rk,  3.0_rk, 4.0_rk]          /  4.0_rk
  integer(kind=ik), parameter :: p1     = 3
  integer(kind=ik), parameter :: s1     = 3
  real(kind=rk),    parameter :: b1(s)  = [          2.0_rk,  3.0_rk,  4.0_rk, 0.0_rk]          /  9.0_rk
  integer(kind=ik), parameter :: p2     = 2
  real(kind=rk),    parameter :: b2(s)  = [          7.0_rk,  6.0_rk,  8.0_rk, 3.0_rk]          / 24.0_rk
end module mrkiss_eerk_bogacki_shampine_3_2
