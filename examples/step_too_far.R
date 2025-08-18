#!/usr/bin/env -S Rscript
# -*- Mode:ess-r; Coding:us-ascii-unix; fill-column:158 -*-
#########################################################################################################################################################.H.S.##
##
# @file      step_too_far.R
# @author    Mitch Richling http://www.mitchr.me/
# @brief     Plot results from the step_too_far.f90 example.@EOL
# @std       GNU-R
# @see       https://github.com/richmit/MRKISS/
# @copyright
#  @parblock
#  Copyright (c) 2025, Mitchell Jay Richling <http://www.mitchr.me/> All rights reserved.
#
#  Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
#
#  1. Redistributions of source code must retain the above copyright notice, this list of conditions, and the following disclaimer.
#
#  2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions, and the following disclaimer in the documentation
#     and/or other materials provided with the distribution.
#
#  3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software
#     without specific prior written permission.
#
#  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
#  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
#  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
#  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
#  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
#  DAMAGE.
#  @endparblock
#########################################################################################################################################################.H.E.##

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
solDat <- do.call(rbind, lapply(list.files(pattern = "^step_too_far_[0-9]+\\.csv$"),
                                function(f) {
                                  fread(f) %>%
                                    mutate(errt   = abs(1-t),
                                           erry1at= abs(y1-exp(t)),
                                           erry1  = abs(y1-exp(1)),
                                           sso    = as.integer(first(strsplit(f, "[_.]"))[4]),
                                           pts    = 1.005^sso,
                                           delta  = 1/(pts-1)); }))

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
gp <- ggplot(solDat) +
  geom_line( aes(x=delta, y=erry1)) +
  scale_y_log10() +
  scale_x_log10() +
  labs(title='Accuracy: Step Size Vs. Absolute Total Error', x='Step Size', y='Absolute Error')
ggsave(filename='step_too_far.png', plot=gp, width=2*1024, height=1023, units='px', dpi=150)
