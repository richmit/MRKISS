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
adDat <- fread('three_body_steps_adapt_etab_wt-std.csv')
ftDat <- fread('tree_body_steps_fixed_stab_wt.csv')
fyDat <- fread('three_body_steps_condy_stab_wt.csv')
loDat <- fread('steps_fixed_stab_wt-dp.csv')
loDat <- fread('steps_fixed_stab_wt-dp.csv')
slDat <- fread('steps_sloppy_condy_stab_wt.csv')
a2Dat <- fread('three_body_steps_adapt_etab_wt-fix-delta-steps.csv')
a3Dat <- fread('three_body_steps_adapt_etab_wt-pho-t-max.csv')
a4Dat <- fread('three_body_steps_adapt_etab_wt-isct.csv')

erDat <- data.table(b=c('Earth'), x=c(0), y=c(0))
moDat <- data.table(x=cos(seq(0, 2*pi, 0.01)), y=sin(seq(0, 2*pi, 0.01)))
ivDat <- data.table(x=0.994, y=0.0)

gp <- ggplot() + 
  geom_point(data=erDat, aes(x=x, y=y, col='Earth')) +
  geom_path(data=moDat, aes(x=x, y=y, col='Moon')) +
  geom_path(data=ftDat, aes(x=y1, y=y2, col='Fixed Steps'))  +
  geom_point(data=adDat, aes(x=y1, y=y2, col='Adaptive Steps')) +
  geom_point(data=ivDat, aes(x=x, y=y, col='IV')) +
  scale_colour_manual(values=c("Earth"="blue", "Moon"="grey", "Fixed Steps"="pink", "Adaptive Steps"="red", "IV"="black")) +
  labs(title='Restricted Three Body Problem', x='x', y='y', col='', subtitle='No stepp_o') +
  coord_fixed()
ggsave(filename='three_body.png', plot=gp, width=1024, height=800, units='px', dpi=150)

gp <- ggplot() + 
  geom_point(data=erDat, aes(x=x, y=y, col='Earth')) +
  geom_path(data=moDat, aes(x=x, y=y, col='Moon')) +
  geom_path(data=ftDat, aes(x=y1, y=y2, col='High Order Fixed Steps'))  +
  geom_path(data=loDat, aes(x=y1, y=y2, col='Low Order Fixed Steps')) +
  scale_colour_manual(values=c("Earth"="blue", "Moon"="grey", "High Order Fixed Steps"="pink", "Low Order Fixed Steps"="red")) +
  labs(title='Restricted Three Body Problem', x='x', y='y', col='', subtitle='High vs. Low Order Fixed Steps') +
  coord_fixed()
ggsave(filename='three_body-dp.png', plot=gp, width=1024, height=800, units='px', dpi=150)

gp <- ggplot() + 
  geom_point(data=erDat, aes(x=x, y=y, col='Earth')) +
  geom_path(data=moDat, aes(x=x, y=y, col='Moon')) +
  geom_path(data=ftDat, aes(x=y1, y=y2, col='Fixed Steps'))  +
  geom_point(data=a2Dat, aes(x=y1, y=y2, col='Adaptive Steps')) +
  geom_point(data=ivDat, aes(x=x, y=y, col='IV')) +
  scale_colour_manual(values=c("Earth"="blue", "Moon"="grey", "Fixed Steps"="pink", "Adaptive Steps"="red", "IV"="black")) +
  labs(title='Restricted Three Body Problem', x='x', y='y', col='', subtitle='y_delta_length via stepp_o') +
  coord_fixed()
ggsave(filename='three_body_ylen.png', plot=gp, width=1024, height=800, units='px', dpi=150)

gp <- ggplot() + 
  geom_point(data=erDat, aes(x=x, y=y, col='Earth')) +
  geom_path(data=moDat, aes(x=x, y=y, col='Moon')) +
  geom_path(data=ftDat, aes(x=y1, y=y2, col='Fixed Steps'))  +
  geom_point(data=a3Dat, aes(x=y1, y=y2, col='Adaptive Steps')) +
  geom_point(data=ivDat, aes(x=x, y=y, col='IV')) +
  scale_colour_manual(values=c("Earth"="blue", "Moon"="grey", "Fixed Steps"="pink", "Adaptive Steps"="red", "IV"="black")) +
  labs(title='Restricted Three Body Problem', x='x', y='y', col='', subtitle='max_t via stepp_o') +
  coord_fixed()
ggsave(filename='three_body_maxt.png', plot=gp, width=1024, height=800, units='px', dpi=150)

gp <- ggplot() + 
  geom_point(data=erDat, aes(x=x, y=y, col='Earth')) +
  geom_path(data=moDat, aes(x=x, y=y, col='Moon')) +
  geom_path(data=ftDat, aes(x=y1, y=y2, col='Fixed Steps'))  +
  geom_point(data=a4Dat, aes(x=y1, y=y2, col='Adaptive Steps')) +
  geom_point(data=ivDat, aes(x=x, y=y, col='IV')) +
  scale_colour_manual(values=c("Earth"="blue", "Moon"="grey", "Fixed Steps"="pink", "Adaptive Steps"="red", "IV"="black")) +
  labs(title='Restricted Three Body Problem', x='x', y='y', col='', subtitle='Moon orbit intersection') +
  coord_fixed()
ggsave(filename='three_body_moon.png', plot=gp, width=1024, height=800, units='px', dpi=150)

gp <- ggplot() + 
  geom_point(data=ftDat %>% filter(t<0.15), aes(x=y1, y=y2-0.01, col='Fixed Time Steps')) + 
  geom_path( data=ftDat %>% filter(t<0.15), aes(x=y1, y=y2-0.01, col='Fixed Time Steps')) +
  geom_point(data=slDat %>% filter(t<0.15), aes(x=y1, y=y2-0.02, col='Sloppy Fixed Time Steps')) + 
  geom_path( data=slDat %>% filter(t<0.15), aes(x=y1, y=y2-0.02, col='Sloppy Fixed Time Steps')) +
  geom_point(data=fyDat %>% filter(t<0.15), aes(x=y1, y=y2, col='Fixed Position Steps')) +
  geom_path( data=fyDat %>% filter(t<0.15), aes(x=y1, y=y2, col='Fixed Position Steps')) +
  labs(title='Restricted Three Body Problem', x='x', y='y', col='', subtitle='Fixed Position Steps vs Fixed Time Steps (position)') +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(),
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
  labs(title='Restricted Three Body Problem', x='x', y='y', col='', subtitle='Fixed Position Steps vs Fixed Time Steps (velocity)') +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(),
        legend.position = c(0.7, 0.7)) +
  coord_fixed()
ggsave(filename='three_body_fixed_vel.png', plot=gp, width=1024, height=600, units='px', dpi=150)
