! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      mrkiss_config.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @brief     Configuration for MRKISS == MR RK KISS == Mitch Richling's Runge-Kutta Keep It Simple Stupid.@EOL
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
!> Configuration for MRKISS == MR RK KISS == Mitch Richling's Runge-Kutta Keep It Simple Stupid
!!
module mrkiss_config
  use, intrinsic:: iso_c_binding, only: c_int32_t, c_double, c_float
  implicit none
  private

  ! Real & integer types for externally viable interfaces and arguments
  integer,          parameter, public :: ik                 = c_int32_t                       !< Integer kinds used in interfaces
  integer,          parameter, public :: rk                 = c_double                        !< Real kind used in interfaces

  ! Absolute constants
  real(kind=rk),    parameter, public :: zero_epsilon       = 1.0e-12_rk                      !< Used to test for zero
  real(kind=rk),    parameter, public :: t_delta_tiny       = tiny(zero_epsilon) * 1.0e18_rk  !< Smallest value for t_delta

  ! ab initio parameters for defaults when we know nothing
  real(kind=rk),    parameter, public :: t_delta_ai         = 1.0e-4_rk                       !< t_delta ai default
  real(kind=rk),    parameter, public :: error_tol_abs_ai   = 1.0e-5_rk                       !< error_tol_abs ai default
  real(kind=rk),    parameter, public :: error_tol_rel_ai   = 1.0e-3_rk                       !< error_tol_rel ai default
  integer(kind=ik), parameter, public :: max_bisect_ai      = 1000                            !< max_bisect ai default
  real(kind=rk),    parameter, public :: t_delta_fac_min_ai = 0.5_rk                          !< t_delta_fac_min ai default
  real(kind=rk),    parameter, public :: t_delta_fac_max_ai = 2.0_rk                          !< t_delta_fac_max ai default
  real(kind=rk),    parameter, public :: t_delta_fac_fdg_ai = 0.5_rk                          !< t_delta_fac_fdg ai default
  real(kind=rk),    parameter, public :: sdf_tol_ai         = 1.0e-3                          !< sdf_tol ai default
end module mrkiss_config


!----------------------------------------------------------------------------------------------------------------------------------
!> \mainpage 
!! 
!! @image html MRKISS_logo_c_500x250.png
!!
!! MRKISS == MR RK KISS == Mitch Richling's Runge-Kutta Keep It Simple Stupid
!!
!! [MRKISS](https://github.com/richmit/MRKISS) is a *tiny* library with *zero dependencies* that aims to make it easy to *use*
!! and *experiment with* explicit Runge-Kutta methods.
!! 
!! For an overview of MRKISS and some tutorial information, check out the [documentation](https://richmit.github.io/MRKISS/index.html).
!! 
!! A couple of the examples in the repository have longer explanatory documentation:
!!    - [Three Body Problem](https://richmit.github.io/MRKISS/ex_three_body.html)
!!    - [Lornez Attracter](https://richmit.github.io/MRKISS/ex_lorenz.html)
!! 
!! Generated [API documentation](https://www.mitchr.me/SS/MRKISS/doc-lib/html/index.html) is viable as well.
!! 
!! Development News:
!!   - [changelog](https://richmit.github.io/MRKISS/changelog.html)
!!   - [roadmap](https://richmit.github.io/MRKISS/roadmap.html)
!! 
!! This library powers the [Strange Attractor Zoo](https://richmit.github.io/StrangeAttractorZoo/).
!! 
!! The canonical URL for this documentation: https://www.mitchr.me/SS/MRKISS/doc-lib/html/index.html
!!

! Status codes are assigned in blocks to subroutines and interfaces.  Status codes are frequently "passed up" the call chain.
! i.e. a routine may return a status code that was returned to it by another routine.  Assigning the codes in blocks allows the
! user to know from which subroutine a status originated.
!
!
! Assigned Status Ranges
! - 0001-0255 ... interface  deq_iface_*t
! - 0256-0511 ... interface  stepp_iface_*t
! - 0512-0767 ... interface  sdf_iface_*t
! - 1232-1247 ... subroutine one_step_etab_*t
! - 1248-1263 ... subroutine one_step_stab_*t
! - 1216-1231 ... subroutine one_richardson_step_stab_*t
! - 1200-1215 ... subroutine one_step_rk4_*t
! - 1184-1199 ... subroutine one_step_rkf45_*t
! - 1263-1279 ... subroutine one_step_dp54_*t
! - 1120-1151 ... subroutine steps_fixed_stab_*t
! - 1024-1055 ... subroutine steps_condy_stab_*t
! - 1280-1296 ... subroutine steps_sloppy_condy_stab_*t
! - 1056-1119 ... subroutine steps_adapt_etab_*t
! - 1152-1183 ... subroutine print_solution
! - 1297-1313 ... subroutine analyze_solution
! - 1314:1330 ... subroutine seq
! - 1331:1347 ... subroutine interpolate_solution
! - 1348-1364 ... Unallocated
! - 1365-1381 ... Unallocated
! - 1382-1398 ... Unallocated
! - 1399-1415 ... Unallocated
! - 1416-1432 ... Unallocated
! - 1433-1449 ... Unallocated
! - 1450-1466 ... Unallocated
! - 1467-1483 ... Unallocated
! - 1484-1500 ... Unallocated
! - 1501-1517 ... Unallocated
! - 1518-1534 ... Unallocated
! - 1535-1551 ... Unallocated
! - 1552-1568 ... Unallocated
! - 1569-1585 ... Unallocated
! - 1586-1602 ... Unallocated
! - 1603-1619 ... Unallocated
! - 1620-1636 ... Unallocated
! - 1637-1653 ... Unallocated
! - 1654-1670 ... Unallocated
! - 1671-1687 ... Unallocated
! - 1688-1704 ... Unallocated
! - 1705-1721 ... Unallocated
! - 1722-1738 ... Unallocated
! - 1739-1755 ... Unallocated
! - 1756-1772 ... Unallocated
! - 1773-1789 ... Unallocated
! - 1790-1806 ... Unallocated
! - 1807-1823 ... Unallocated
! - 1824-1840 ... Unallocated
! - 1841-1857 ... Unallocated
! - 1858-1874 ... Unallocated
! - 1875-1891 ... Unallocated
! - 1892-1908 ... Unallocated
! - 1909-1925 ... Unallocated
! - 1926-1942 ... Unallocated
! - 1943-1959 ... Unallocated
! - 1960-1976 ... Unallocated
! - 1977-1993 ... Unallocated
! - 1994-2010 ... Unallocated
!
! (let ((s "\n"))
!   (cl-loop for f from 1348 to 2000 by 17
!            do (print f)
!            do (setq s (concat s (format "%d-%d\n" f (+ 16 f)))))
!   s)
