#!/usr/bin/env -S Rscript
# -*- Mode:ess-r; Coding:us-ascii-unix; fill-column:158 -*-
#########################################################################################################################################################.H.S.##
##
# @file      three_body.R
# @author    Mitch Richling http://www.mitchr.me/
# @brief     Plot the output from three_body.f90.@EOL
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
adDat <- fread('three_body_adaptive_steps-std.csv')
ftDat <- fread('tree_body_fixed_t_steps.csv')
fyDat <- fread('three_body_fixed_y_steps.csv')
loDat <- fread('fixed_t_steps-dp.csv')
slDat <- fread('sloppy_fixed_y_steps.csv')
a2Dat <- fread('three_body_adaptive_steps-fix-delta-steps.csv')
a3Dat <- fread('three_body_adaptive_steps-pho-t-max.csv')
a4Dat <- fread('three_body_adaptive_steps-isct.csv')
aiDat <- fread('three_body_steps_adapt_std_interpolated.csv')
alDat <- fread('three_body_steps_adapt_std_interpolated_lin.csv')
erDat <- data.table(b=c('Earth'), x=c(0), y=c(0))
moDat <- data.table(x=cos(seq(0, 2*pi, 0.01)), y=sin(seq(0, 2*pi, 0.01)))
m0Dat <- data.table(x=1.0, y=0.0)

gp <- ggplot() + 
  geom_path(data=aiDat, aes(x=y1, y=y2, col='Interpolated')) + 
  geom_point(data=adDat, aes(x=y1, y=y2, col='Adaptive')) +
  scale_colour_manual(values=c("Interpolated"="darkblue", "Adaptive"="red")) +
  labs(title='Restricted Three Body Problem', subtitle='Interpolated Adaptive Solution (Hermite)', 
       x=expression(x[1]), y=expression(x[2]), col='') +
  coord_fixed()
ggsave(filename='three_body_interp_adapt_path.png', plot=gp, width=1024, height=800, units='px', dpi=150)

gp <- ggplot() + 
  geom_path(data=alDat, aes(x=y1, y=y2, col='Interpolated')) + 
  geom_point(data=adDat, aes(x=y1, y=y2, col='Adaptive')) +
  scale_colour_manual(values=c("Interpolated"="darkblue", "Adaptive"="red")) +
  labs(title='Restricted Three Body Problem', subtitle='Interpolated Adaptive Solution (Linear)', 
       x=expression(x[1]), y=expression(x[2]), col='') +
  coord_fixed()
ggsave(filename='three_body_lin_interp_adapt_path.png', plot=gp, width=1024, height=800, units='px', dpi=150)

gp <- ggplot(rbind(data.table(t=ftDat$t, aerr=abs(aiDat$y1-ftDat$y1), bse=abs(ftDat$y1) , var='x1'),
                   data.table(t=ftDat$t, aerr=abs(aiDat$y2-ftDat$y2), bse=abs(ftDat$y2) , var='x2'),
                   data.table(t=ftDat$t, aerr=abs(aiDat$y3-ftDat$y3), bse=abs(ftDat$y3) , var='v1'),
                   data.table(t=ftDat$t, aerr=abs(aiDat$y4-ftDat$y4), bse=abs(ftDat$y4) , var='v2')) %>%
             filter(aerr>0 & bse>0) %>%
             mutate(rerr=aerr/bse)) + 
  geom_line(aes(x=t, y=rerr, col=var), linewidth=2, alpha=0.5) +
  scale_colour_manual(values=c("x1"="darkgreen", "x2"="darkblue", "v1"="darkgoldenrod", "v2"="darkred"),
                      labels=c(expression(x[1]), expression(x[2]), expression(v[1]), expression(v[2]))) +
  scale_y_log10() +
  labs(title='Interpolated Adaptive Solution', subtitle='Relative Error', x=expression(t), y='error', col='') 
ggsave(filename='three_body_interp_adapt_error.png', plot=gp, width=1024, height=800, units='px', dpi=150)

gp <- ggplot() + 
  geom_point(data=erDat, aes(x=x, y=y, col='Earth')) +
  geom_path(data=moDat, aes(x=x, y=y, col='Moon')) +
  geom_path(data=ftDat, aes(x=y1, y=y2, col='Fixed Steps'))  +
  geom_point(data=adDat, aes(x=y1, y=y2, col='Adaptive Steps')) +
  geom_point(data=m0Dat, aes(x=x, y=y, col='Moon')) +
  scale_colour_manual(values=c("Earth"="blue", "Moon"="grey", "Fixed Steps"="pink", "Adaptive Steps"="red")) +
  labs(title='Restricted Three Body Problem', x=expression(x[1]), y=expression(x[2]), col='') +
  coord_fixed()
ggsave(filename='three_body.png', plot=gp, width=1024, height=800, units='px', dpi=150)

gp <- ggplot() + 
  geom_point(data=erDat, aes(x=x, y=y, col='Earth')) +
  geom_path(data=moDat, aes(x=x, y=y, col='Moon')) +
  geom_path(data=ftDat, aes(x=y1, y=y2, col='High Order Fixed Steps'))  +
  geom_path(data=loDat, aes(x=y1, y=y2, col='Low Order Fixed Steps')) +
  geom_point(data=m0Dat, aes(x=x, y=y, col='Moon')) +
  scale_colour_manual(values=c("Earth"="blue", 
                               "Moon"="grey", 
                               "High Order Fixed Steps"="pink", "Low Order Fixed Steps"="red")) +
  labs(title='Restricted Three Body Problem', x=expression(x[1]), y=expression(x[2]), col='', 
       subtitle='High vs. Low Order Fixed Steps') +
  coord_fixed()
ggsave(filename='three_body-dp.png', plot=gp, width=1024, height=800, units='px', dpi=150)

gp <- ggplot() + 
  geom_point(data=erDat, aes(x=x, y=y, col='Earth')) +
  geom_path(data=moDat, aes(x=x, y=y, col='Moon')) +
  geom_path(data=ftDat, aes(x=y1, y=y2, col='Fixed Steps'))  +
  geom_point(data=a2Dat, aes(x=y1, y=y2, col='Adaptive Steps')) +
  geom_point(data=m0Dat, aes(x=x, y=y, col='Moon')) +
  scale_colour_manual(values=c("Earth"="blue", "Moon"="grey", "Fixed Steps"="pink", "Adaptive Steps"="red")) +
  labs(title='Restricted Three Body Problem', x=expression(x[1]), y=expression(x[2]), col='') +
  coord_fixed()
ggsave(filename='three_body_ylen.png', plot=gp, width=1024, height=800, units='px', dpi=150)

gp <- ggplot() + 
  geom_point(data=erDat, aes(x=x, y=y, col='Earth')) +
  geom_path(data=moDat, aes(x=x, y=y, col='Moon')) +
  geom_path(data=ftDat, aes(x=y1, y=y2, col='Fixed Steps'))  +
  geom_point(data=a3Dat, aes(x=y1, y=y2, col='Adaptive Steps')) +
  geom_point(data=m0Dat, aes(x=x, y=y, col='Moon')) +
  scale_colour_manual(values=c("Earth"="blue", "Moon"="grey", "Fixed Steps"="pink", "Adaptive Steps"="red")) +
  labs(title='Restricted Three Body Problem', x=expression(x[1]), y=expression(x[2]), col='') +
  coord_fixed()
ggsave(filename='three_body_maxt.png', plot=gp, width=1024, height=800, units='px', dpi=150)

gp <- ggplot() + 
  geom_point(data=erDat, aes(x=x, y=y, col='Earth')) +
  geom_path(data=moDat, aes(x=x, y=y, col='Moon')) +
  geom_path(data=ftDat, aes(x=y1, y=y2, col='Fixed Steps'))  +
  geom_point(data=a4Dat, aes(x=y1, y=y2, col='Adaptive Steps')) +
  geom_point(data=m0Dat, aes(x=x, y=y, col='Moon')) +
  scale_colour_manual(values=c("Earth"="blue", "Moon"="grey", "Fixed Steps"="pink", "Adaptive Steps"="red")) +
  labs(title='Restricted Three Body Problem', x=expression(x[1]), y=expression(x[2]), col='', 
       subtitle='Moon orbit intersection') +
  coord_fixed()
ggsave(filename='three_body_moon.png', plot=gp, width=1024, height=800, units='px', dpi=150)

gp <- ggplot() + 
  geom_point(data=ftDat %>% filter(t<0.15), aes(x=y1, y=y2-0.01, col='Fixed Time Steps')) + 
  geom_path( data=ftDat %>% filter(t<0.15), aes(x=y1, y=y2-0.01, col='Fixed Time Steps')) +
  geom_point(data=slDat %>% filter(t<0.15), aes(x=y1, y=y2-0.02, col='Sloppy Fixed Time Steps')) + 
  geom_path( data=slDat %>% filter(t<0.15), aes(x=y1, y=y2-0.02, col='Sloppy Fixed Time Steps')) +
  geom_point(data=fyDat %>% filter(t<0.15), aes(x=y1, y=y2, col='Fixed Position Steps')) +
  geom_path( data=fyDat %>% filter(t<0.15), aes(x=y1, y=y2, col='Fixed Position Steps')) +
  labs(title='Restricted Three Body Problem', x=expression(x[1]), y=expression(x[2]), col='', 
       subtitle='Fixed Position Steps vs Fixed Time Steps (position)') +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        legend.position = c(0.2, 0.7)) +
  coord_fixed()
ggsave(filename='three_body_fixed_pos.png', plot=gp, width=1024, height=600, units='px', dpi=150)

gp <- ggplot() + 
  geom_point(data=ftDat %>% filter(t<0.15), aes(x=y4-0.12, y=y3-0.15, col='Fixed Time Steps')) + 
  geom_path( data=ftDat %>% filter(t<0.15), aes(x=y4-0.12, y=y3-0.15, col='Fixed Time Steps')) +
  geom_point(data=slDat %>% filter(t<0.15), aes(x=y4-0.12, y=y3-0.22, col='Sldat Fixed Time Steps')) + 
  geom_path( data=slDat %>% filter(t<0.15), aes(x=y4-0.12, y=y3-0.22, col='Sldat Fixed Time Steps')) +
  geom_point(data=fyDat %>% filter(t<0.15), aes(x=y4, y=y3, col='Fixed Position Steps')) +
  geom_path( data=fyDat %>% filter(t<0.15), aes(x=y4, y=y3, col='Fixed Position Steps')) +
  labs(title='Restricted Three Body Problem', x=expression(v[1]), y=expression(v[2]), col='', 
       subtitle='Fixed Position Steps vs Fixed Time Steps (velocity)') +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        legend.position = c(0.7, 0.7)) +
  coord_fixed()
ggsave(filename='three_body_fixed_vel.png', plot=gp, width=1024, height=600, units='px', dpi=150)
