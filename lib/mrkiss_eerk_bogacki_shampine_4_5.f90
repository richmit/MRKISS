! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!.!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      mrkiss_eerk_bogacki_shampine_4_5.f90
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
!.!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.E.!!

!----------------------------------------------------------------------------------------------------------------------------------
!> Butcher tableau for Bogacki Shampine's 7 step, order (4,5) Runge-Kutta method
!!
!! @image html eerk_bogacki_shampine_4_5-stab.png
!!
!! @par IMO
!! This is a good general use method with excellent error coefficients.
!!
!! @par Known Aliases
!! 'bs5' (OrdinaryDiffEq.jl)
!!
!! @par Stability Image Links
!! <a href="eerk_bogacki_shampine_4_5-stab.png">  <img src="eerk_bogacki_shampine_4_5-stab.png"  width="256px"> </a>
!! <a href="eerk_bogacki_shampine_4_5-astab.png"> <img src="eerk_bogacki_shampine_4_5-astab.png" width="256px"> </a>
!! <a href="eerk_bogacki_shampine_4_5-star1.png"> <img src="eerk_bogacki_shampine_4_5-star1.png" width="256px"> </a>
!! <a href="eerk_bogacki_shampine_4_5-star2.png"> <img src="eerk_bogacki_shampine_4_5-star2.png" width="256px"> </a>
!!
!! @par References:
!!  - Bogacki & Shampine (1996); An Efficient Runge-Kutta (4,5) Pair; Comp. & Math. with Appl. 32 (6); p15-28; 
!!    zotero://select/items/0_3SDMHUHW
!!  - Peter Stone (2012); Bogacki-Shampine Combined 7 Stage, Order 4 and 5 Runge-Kutta Scheme with an Additional 8 
!!    Stage, Order 4 FSAL Embedded Scheme; zotero://select/items/0_AS3ZHL53
!!
module mrkiss_eerk_bogacki_shampine_4_5
  use mrkiss_config, only: rk
  implicit none
  public
  !> The order of the overall method
  integer,          parameter :: s      = 7
  !> Number of methods
  integer,          parameter :: m      = 2
  !> The @f$\mathbf{a}@f$ matrix for the Butcher Tableau. @hideinitializer @hideinlinesource
  real(kind=rk),    parameter :: a(s,s) = reshape([                0.0_rk,                 0.0_rk,                0.0_rk,                0.0_rk,                 0.0_rk,                0.0_rk,          0.0_rk, &
       &                                             624139091896320.0_rk,                 0.0_rk,                0.0_rk,                0.0_rk,                 0.0_rk,                0.0_rk,          0.0_rk, &
       &                                             277395151953920.0_rk,   554790303907840.0_rk,                0.0_rk,                0.0_rk,                 0.0_rk,                0.0_rk,          0.0_rk, &
       &                                             499493238266880.0_rk, -1768697368289280.0_rk, 2874133223470080.0_rk,                0.0_rk,                 0.0_rk,                0.0_rk,          0.0_rk, &
       &                                             857403196948480.0_rk, -1361758018682880.0_rk, 1099881476628480.0_rk, 1901029712691200.0_rk,                 0.0_rk,                0.0_rk,          0.0_rk, &
       &                                              99239445453330.0_rk,   861737496197760.0_rk,  403421580251235.0_rk,  599993938324872.0_rk,   844233453306243.0_rk,                0.0_rk,          0.0_rk, &
       &                                             680055277224960.0_rk, -1449549904158720.0_rk, 1546177346703360.0_rk, 2400406438993920.0_rk, -3790311591306240.0_rk, 4358056983920640.0_rk,         0.0_rk], [s, s]) / 3744834551377920.0_rk
  !> The @f$\mathbf{b}@f$ matrix for the Butcher Tableau. @hideinitializer @hideinlinesource
  real(kind=rk),    parameter :: b(s,m) = reshape([        101492300.0_rk,                 0.0_rk,        399630510.0_rk,        272071716.0_rk,          12044214.0_rk,        501329920.0_rk, 107696940.0_rk, &
       &                                                   104760110.0_rk,                 0.0_rk,        385163505.0_rk,        295063692.0_rk,         -14425047.0_rk,        516006400.0_rk, 107696940.0_rk], [s, m]) /       1394265600.0_rk
  !> The @f$\mathbf{c}@f$ matrix for the Butcher Tableau. @hideinitializer @hideinlinesource
  real(kind=rk),    parameter :: c(s)   = [                        0.0_rk,                42.0_rk,               56.0_rk,              108.0_rk,               168.0_rk,              189.0_rk,       252.0_rk]          /               252.0_rk
  !> The method orders
  integer,          parameter :: p(m)   = [4, 5]
  !> Number of stages for each method
  integer,          parameter :: se(m)  = [7, 7]
end module mrkiss_eerk_bogacki_shampine_4_5
