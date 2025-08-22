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
solDat <- fread("step_too_far.csv") %>%
  mutate(errt   = abs(1-t),
         y      = y1,
         erryat = abs(y-exp(t)),
         erry   = abs(y-exp(1)),
         sso    = tag,
         pts    = 1.005^sso,
         delta  = 1/(pts-1)) %>% 
  filter(errt>0 & erryat>0 & erry>0)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# Plot the raw results.
gp <- ggplot(solDat) +
  geom_line( aes(x=delta, y=erry), col='indianred3') +
  geom_point( aes(x=delta, y=erry), col='brown4', size=0.25) +
  scale_y_log10() +
  scale_x_log10() +
  labs(title='Accuracy: Step Size Vs. Total Error', 
       subtitle='Experimental results from RK4 ', x='Step Size', y='Total Error')
ggsave(filename='step_too_far.png', plot=gp, width=2*1024, height=1023, units='px', dpi=150)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# Compute the log transformed linear regression for the truncation error dominated part of the dataset
treDat <- solDat %>% 
  transmute(x=delta, y=erryat) %>% 
  filter(x>0 & y>0) %>% 
  mutate(xt=log(x), yt=log(y)) %>% 
  filter(x>2e-3)
treFit <- lm(yt ~ xt, data=treDat)     
treDat <- treDat %>% 
  mutate(yf=exp(coef(treFit)[1])*x^(coef(treFit)[2]))

# Note the value for 'xt' in the fit will be the order of the RK method used.  
# This is a practical way experimentally to compute the order for a RK method.
print(summary(treFit))

## ggplot(data=treDat, aes(x=x)) +
##   geom_line(aes(y=y), col='red') +
##   geom_line(aes(y=yf), col='blue') +
##   scale_y_log10() +
##   scale_x_log10() 

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# Compute the log transformed linear regression for the round-off error dominated part of the dataset
roeDat <- solDat %>% 
  transmute(x=delta, y=erryat) %>% 
  filter(x>0 & y>0) %>% 
  mutate(xt=log(x), yt=log(y)) %>% 
  filter(x<2e-4)
roeFit <- lm(yt ~ xt, data=roeDat)     
roeDat <- roeDat %>% 
  mutate(yf=exp(coef(roeFit)[1])*x^(coef(roeFit)[2]))

## ggplot(data=roeDat, aes(x=x)) +
##   geom_line(aes(y=y), col='red') +
##   geom_line(aes(y=yf), col='blue') +
##   scale_y_log10() +
##   scale_x_log10() 

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
gp <- ggplot() + 
  geom_density(data=treDat, aes(x=y-yf, fill='Truncation Error'), alpha=0.75, linewidth=0) + 
  geom_density(data=roeDat, aes(x=y-yf, fill='Round-off Error'), alpha=0.5, linewidth=0) +
  scale_fill_manual(name='Error Type', 
                      values=c('Truncation Error' = 'goldenrod',
                               'Round-off Error'  = 'darkolivegreen3')) +
  labs(title='Truncation Vs. Round-off Residual Distribution', 
       subtitle='Experimental results from RK4.', 
       x='Error', y='') +
  theme(axis.text.y=element_blank())
ggsave(filename='step_too_far_trdst.png', plot=gp, width=1024, height=600, units='px', dpi=100)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# Add total, truncation, round-off error to our solution data and plot everything.
solDat <- solDat %>% mutate(erryattre=exp(coef(treFit)[1])*delta^(coef(treFit)[2]),
                            erryatroe=exp(coef(roeFit)[1])*delta^(coef(roeFit)[2]),
                            erryattoe=erryattre+erryatroe)

gp <- ggplot(data=solDat, aes(x=delta)) +
  geom_line(aes(y=erryattre, col='Mean Truncation Error'), linewidth=5, alpha=0.7) +
  geom_line(aes(y=erryatroe, col='Mean Round-off Error'), linewidth=5, alpha=0.7) +
  geom_line(aes(y=erryattoe, col='Mean Total Error'), linewidth=3) +
  geom_point(aes(y=erryat, col='Total Error'), size=0.5) +
  scale_y_log10(limits=range(solDat$erryat)) +
  scale_x_log10() +
  scale_colour_manual(name='Error Type', 
                      values=c('Mean Total Error'      = 'darkorchid3',
                               'Mean Truncation Error' = 'goldenrod',
                               'Mean Round-off Error'  = 'darkolivegreen3',                               
                               'Total Error'           = 'indianred3')) +
  labs(title='Error Vs. Step Size', 
       subtitle='Experimental results from RK4 illustrating total error as a sum of round-off and truncation errors.', 
       x='Step Size', y='Errors')
ggsave(filename='step_too_far_mean.png', plot=gp, width=1024, height=600, units='px', dpi=100)


#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# Compute the log transformed linear regression for t
troeDat <- solDat %>% 
  transmute(x=delta, y=errt) %>% 
  filter(x>0 & y>0) %>% 
  mutate(xt=log(x), yt=log(y))
troeFit <- lm(yt ~ xt, data=troeDat)     
troeDat <- troeDat %>% 
  mutate(yf=exp(coef(troeFit)[1])*x^(coef(troeFit)[2]))

gp <- ggplot(data=troeDat, aes(x=x)) +
  geom_line(aes(y=yf, col='Mean Round-off Error'), alpha=0.7, linewidth=10) +
  geom_point(aes(y=y, col='Total Error'), size=0.5) +
  scale_y_log10() +
  scale_x_log10() +
  scale_colour_manual(name='Error Type', 
                      values=c('Mean Round-off Error'  = 'darkolivegreen3',                               
                               'Total Error'           = 'indianred3')) +
  labs(title='Independent Variable Error Vs. Step Size', 
       subtitle='Experimental results from RK4.', 
       x='Step Size', y='Errors')
ggsave(filename='step_too_far_tfit.png', plot=gp, width=1024, height=600, units='px', dpi=100)

