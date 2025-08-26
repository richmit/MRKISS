! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!.!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      mrkiss_erk_ralston_4.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @brief     Butcher tableau for ralston's 4 stage order (4) Runge-Kutta method.@EOL
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
!.!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.E.!!

!----------------------------------------------------------------------------------------------------------------------------------
!> Butcher tableau for ralston's 4 stage order (4) Runge-Kutta method
!!
!! IMO: This method was designed to minimize truncation error over every other consideration includeing stability, roundoff
!!      error, performance, and storage.
!!
!! @par References:
!!  - Ralston (1962); Runge-Kutta Methods with Minimum Error Bounds; Math. of Comp. 16 (80); p431-437; zotero://select/items/0_2FK7F4J3
!!
module mrkiss_erk_ralston_4
  use mrkiss_config, only: rk
  implicit none
  public
  !> The order of the overall method
  integer,          parameter :: s      = 4
  !> The @f$\mathbf{a}@f$ matrix for the Butcher Tableau. @hideinitializer @hideinlinesource
  real(kind=rk),    parameter :: a(s,s) = reshape([ 0.0000000000000000000000000000000000000_rk, 0.0000000000000000000000000000000000000_rk,  0.0000000000000000000000000000000000000_rk, 0.0000000000000000000000000000000000000_rk, &
                                                    0.4000000000000000000000000000000000000_rk, 0.0000000000000000000000000000000000000_rk,  0.0000000000000000000000000000000000000_rk, 0.0000000000000000000000000000000000000_rk, &
                                                    0.2969776092477536000706054677229125627_rk, 0.1587596449710358318526744693899731432_rk,  0.0000000000000000000000000000000000000_rk, 0.0000000000000000000000000000000000000_rk, &
                                                    0.2181003882259204675961605401197504035_rk, -3.0509651486929308053535826782740859769_rk, 3.8328647604670103377574221381543355735_rk, 0.0000000000000000000000000000000000000_rk], [s, s])
  !> The @f$\mathbf{c}@f$ matrix for the Butcher Tableau. @hideinitializer @hideinlinesource
  real(kind=rk),    parameter :: c(s)   = [         0.0000000000000000000000000000000000000_rk,  0.4000000000000000000000000000000000000_rk, 0.4557372542187894319232799371128857059_rk, 1.0000000000000000000000000000000000000_rk]                 
  !> The order of the method
  integer,          parameter :: p      = 4
  !> The @f$\mathbf{b}@f$ matrix for the Butcher Tableau. @hideinitializer @hideinlinesource
  real(kind=rk),    parameter :: b(s)   = [         0.1747602822626903712548676424114517824_rk, -0.5514806628787329405457611464815246174_rk, 1.2055355993965235350277772006141793793_rk, 0.1711847812195190342631163034558934558_rk]                 
end module mrkiss_erk_ralston_4




