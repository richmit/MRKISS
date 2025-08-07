#!/usr/bin/env -S Rscript
# -*- Mode:ess-r; Coding:us-ascii-unix; fill-column:158 -*-
#########################################################################################################################################################.H.S.##
##
# @file      step_order_vs_error.R
# @author    Mitch Richling http://www.mitchr.me/
# @brief     Make plots for step_order_vs_error.f90 example.@EOL
# @keywords  MRKISS
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
crvDat <- data.table(t=seq(0,2*pi, length.out=500)) %>% mutate(truy1=sin(t*t)/2)
drvDat <- data.table(t=seq(0,2*pi, length.out=500)) %>% mutate(trudy1=t*cos(t*t))

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
truDat <- data.table(t=seq(0,2*pi, length.out=10)) %>% transmute(truy1=sin(t*t)/2)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
solDat <- do.call(rbind, lapply(list.files(pattern = "^step_order_vs_error_[0-9][0-9]_[0-9][0-9]\\.csv$"), 
                                function(f) { 
                                  rko=first(strsplit(f, "[_.]"))[5];
                                  spp=first(strsplit(f, "[_.]"))[6];
                                  cbind(fread(f), truDat) %>% 
                                    mutate(err=abs(y1-truy1), order=rko, steps_per_point=spp); })) %>% 
  filter(i>1 & steps_per_point!='01')

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
gp <- ggplot(crvDat) +
  geom_line(aes(x=t, y=truy1)) +
  labs(title='Solution', x='t', y='y') 
ggsave(filename='step_order_vs_error_soly.png', plot=gp, width=1024, height=800, units='px', dpi=150)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
gp <- ggplot(drvDat) +
  geom_line(aes(x=t, y=trudy1)) +
  labs(title='Solution Derivative', x='t', y='y') 
ggsave(filename='step_order_vs_error_sold.png', plot=gp, width=1024, height=800, units='px', dpi=150)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
gp <- ggplot(solDat) +
  geom_line( aes(x=t, y=err, shape=order, col=steps_per_point)) +
  geom_point(aes(x=t, y=err, shape=order, col=steps_per_point)) +
  annotate("text", x = 2, y = 1e-8, label = "4th Order\n\n9th Order", color = "black", size = 6) +
  annotate("segment", 
           x    = min(solDat$t), 
           xend = max(solDat$t), 
           y    = 1e-9,
           yend = 1e-5,
           linewidth=3) +
  scale_y_log10() +
  labs(title='Accuracy: Step Size & Order', x='t', y='Absolute Error') +
  guides(shape = "none")
print(gp)
ggsave(filename='step_order_vs_error_err.png', plot=gp, width=1024, height=1024, units='px', dpi=150)
