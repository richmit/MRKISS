#!/usr/bin/env -S Rscript
# -*- Mode:ess-r; Coding:us-ascii-unix; fill-column:158 -*-
#########################################################################################################################################################.H.S.##
##
# @file      tc2.R
# @author    Mitch Richling http://www.mitchr.me/
# @brief     Plot graphs for TC2.@EOL
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
a<-do.call(rbind, lapply(list.files(pattern = "^tc2_.*.out$"), fread))

a <- a %>% transmute(case=V1, step=V2, t=V3, approx=V4, exact=V5, error=V6)

keyStep <- 1
keyStep <- max(a$step)

a$case <- factor(a$case, levels=(a %>% filter(step==keyStep) %>% arrange(desc(error)))$case, ordered=TRUE)

gp <- ggplot(a)+geom_line(aes(x=step, y=approx, col=case)) + 
  guides(color = guide_legend(ncol = 1)) +
  labs(title="TC2: Solution", x='Time Step', y='y')
ggsave(filename='tc2_plot_sol.png', plot=gp, width=1024, height=1024, units='px', dpi=100)

gp <- ggplot(a %>% filter(step>1)) + 
  geom_line(aes(x=step, y=error/exact, col=case)) +
  guides(color = guide_legend(ncol = 1)) +
  scale_y_log10() +
  labs(title="TC2: Global Error", x='Time Step', y='Error')
ggsave(filename='tc2_plot_error.png', plot=gp, width=1024, height=1024, units='px', dpi=100)

