! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      mrkiss_eerk_dormand_prince_5_4.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @brief     Butcher tableau for Dormand & Prince's 7 stage, Order (5,4) Runge-Kutta method.@EOL
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
!> Butcher tableau for Dormand & Prince's 7 stage, Order (5,4) Runge-Kutta method
!!
!! IMO: This is a good general use method with excellent principle truncation error coefficients.
!!
!! Known Aliases: 'The Dormand-Prince Method', 'RKDP', 'DOPRI', 'DP5' (OrdinaryDiffEq.jl), 'ode45' (MATLAB & Octave), 
!!                'RK5(4)7M' (Dormand & Prince), & DOPRI5 (Hairer), 'ARKODE_DORMAND_PRINCE_7_4_5' (SUNDIALS)
!!
!! References:
!!  Dormand & Prince (1980); A family of embedded Runge-Kutta formulae; J. Comput. Appl. Math. 6, no. 1
!!  Hairer, Norsett & Wanner (2009). Solving Ordinary Differential Equations. I: Nonstiff Problems. p178
!!  Butcher (2016); Numerical Methods for Ordinary Differential Equations. 3rd Ed; Wiley; p224
!!
module mrkiss_eerk_dormand_prince_5_4
  use mrkiss_config, only: rk, ik
  implicit none
  public
  integer(kind=ik), parameter :: s      = 7
  real(kind=rk),    parameter :: a(s,s) = reshape([          0.0_rk,             0.0_rk,            0.0_rk,           0.0_rk,           0.0_rk,          0.0_rk,      0.0_rk, &
                                                    3427256448.0_rk,             0.0_rk,            0.0_rk,           0.0_rk,           0.0_rk,          0.0_rk,      0.0_rk, &
                                                    1285221168.0_rk,    3855663504.0_rk,            0.0_rk,           0.0_rk,           0.0_rk,          0.0_rk,      0.0_rk, &
                                                   16755475968.0_rk,  -63975453696.0_rk,  60929003520.0_rk,           0.0_rk,           0.0_rk,          0.0_rk,      0.0_rk, &
                                                   50596564480.0_rk, -198708787200.0_rk, 168327864320.0_rk, -4983390720.0_rk,           0.0_rk,          0.0_rk,      0.0_rk, &
                                                   48774576060.0_rk, -184344854400.0_rk, 152622973440.0_rk,  4770896760.0_rk, -4687309620.0_rk,          0.0_rk,      0.0_rk, &
                                                    1561900725.0_rk,             0.0_rk,   7698240000.0_rk, 11156433750.0_rk, -5524329195.0_rk, 2244036960.0_rk,      0.0_rk], [s, s]) / 17136282240.0_rk
  real(kind=rk),    parameter :: c(s)   = [                  0.0_rk,            18.0_rk,           27.0_rk,          72.0_rk,          80.0_rk,         90.0_rk,     90.0_rk]          /          90.0_rk
  integer(kind=ik), parameter :: p1     = 5
  integer(kind=ik), parameter :: s1     = 6
  real(kind=rk),    parameter :: b1(s)  = [              12985.0_rk,             0.0_rk,        64000.0_rk,       92750.0_rk,      -45927.0_rk,      18656.0_rk,      0.0_rk]          /      142464.0_rk
  integer(kind=ik), parameter :: p2     = 4
  real(kind=rk),    parameter :: b2(s)  = [            1921409.0_rk,             0.0_rk,      9690880.0_rk,    13122270.0_rk,    -5802111.0_rk,    1902912.0_rk, 534240.0_rk]          /    21369600.0_rk
end module mrkiss_eerk_dormand_prince_5_4
