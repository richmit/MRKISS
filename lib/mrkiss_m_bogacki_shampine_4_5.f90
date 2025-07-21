! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      mrkiss_m_bogacki_shampine_4_5.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @brief     Butcher tableau for Bogacki Shampine's 8 step, order (4,5) Runge-Kutta method.@EOL
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
!> Butcher tableau for Bogacki Shampine's 8 step, order (4,5) Runge-Kutta method
!!
!! IMO: This is a good general use method with excellent error coefficients.
!!
!! Known Aliases: 'bs5' (OrdinaryDiffEq.jl)
!!
!! References:
!!   Bogacki & Shampine (1996); An Efficient Runge-Kutta (4,5) Pair; Comp. & Math. with Appl. 32 (6); p15-28; zotero://select/items/0_3SDMHUHW
!!   Peter Stone (2012); Bogacki-Shampine Combined 7 Stage, Order 4 and 5 Runge-Kutta Scheme with an Additional 8 Stage, Order 4 FSAL Embedded Scheme; zotero://select/items/0_AS3ZHL53
!!
module mrkiss_m_bogacki_shampine_4_5
  use mrkiss_config, only: rk, ik
  implicit none
  public
  integer(kind=ik), parameter :: s      = 8
  real(kind=rk),    parameter :: a(s,s) = reshape([                0.0_rk,                0.0_rk,                  0.0_rk,                 0.0_rk,                  0.0_rk,                 0.0_rk,                0.0_rk,   0.0_rk, &
                                                    3120695459481600.0_rk,                0.0_rk,                  0.0_rk,                 0.0_rk,                  0.0_rk,                 0.0_rk,                0.0_rk,   0.0_rk, &
                                                    1386975759769600.0_rk,  2773951519539200.0_rk,                 0.0_rk,                 0.0_rk,                  0.0_rk,                 0.0_rk,                0.0_rk,   0.0_rk, &
                                                    2497466191334400.0_rk, -8843486841446400.0_rk, 14370666117350400.0_rk,                 0.0_rk,                  0.0_rk,                 0.0_rk,                0.0_rk,   0.0_rk, &
                                                    4287015984742400.0_rk, -6808790093414400.0_rk,  5499407383142400.0_rk,  9505148563456000.0_rk,                  0.0_rk,                 0.0_rk,                0.0_rk,   0.0_rk, &
                                                     496197227266650.0_rk,  4308687480988800.0_rk,  2017107901256175.0_rk,  2999969691624360.0_rk,   4221167266531215.0_rk,                 0.0_rk,                0.0_rk,   0.0_rk, &
                                                    3400276386124800.0_rk, -7247749520793600.0_rk,  7730886733516800.0_rk, 12002032194969600.0_rk, -18951557956531200.0_rk, 21790284919603200.0_rk,                0.0_rk,   0.0_rk, &
                                                    1362982317496800.0_rk,                 0.0_rk,  5366804365082160.0_rk,  3653764255997856.0_rk,    161746760199024.0_rk,  6732568048926720.0_rk, 1446307009187040.0_rk,   0.0_rk], [s, s]) / 18724172756889600.0_rk
  real(kind=rk),    parameter :: c(s)   = [                        0.0_rk,                42.0_rk,                56.0_rk,               108.0_rk,                168.0_rk,               189.0_rk,              252.0_rk, 252.0_rk]          /               252.0_rk
  integer(kind=ik), parameter :: p1     = 4
  real(kind=rk),    parameter :: b1(s)  = [                 50746150.0_rk,                 0.0_rk,         199815255.0_rk,         136035858.0_rk,            6022107.0_rk,         250664960.0_rk,         53848470.0_rk,   0.0_rk]          /         697132800.0_rk
  integer(kind=ik), parameter :: p2     = 5
  real(kind=rk),    parameter :: b2(s)  = [                104760110.0_rk,                 0.0_rk,         385163505.0_rk,         295063692.0_rk,          -14425047.0_rk,         516006400.0_rk,        107696940.0_rk,   0.0_rk]          /        1394265600.0_rk
end module mrkiss_m_bogacki_shampine_4_5
