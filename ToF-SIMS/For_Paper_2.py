#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Jun 14 12:16:31 2023

@author: kan092
"""

# -------------------------------------------------------------------------
# TASK  01: BRAIN DATASET AND NOISE ONLY PORTION
# TASK  02: FIGURES IN COLUMN FORMAT - DYE dataset
# TASK  03: FIGURES IN COLUMN FORMAT - BRAIN DATASET
# TASK  04: CALCULATE THE REDUCTION IN SEASONALITY
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
# TASK  01: BRAIN DATASET AND NOISE ONLY PORTION
# -------------------------------------------------------------------------
#%%

import numpy as np
import tensorly as tl
from tensorly.decomposition import parafac
import matplotlib.pyplot as plt
import pandas as pd
import Functions as fn
from statsmodels.tsa.seasonal import seasonal_decompose
from matplotlib.patches import ConnectionPatch

# PUTTING IT ALL TOGETHER
filename = '/Users/kan092/Documents/Research/TOF-SIMS/Data/brain_raster.npy'
data = np.load(filename)
jj = 1838
slice = data[120:200,220:280,jj]  #   160:180,220:240


# X and Y factors
f = plt.figure()
# f.set_size_inches(10, 4)
f, (ax1, ax2, ax3, ax4) = plt.subplots(1, 4, gridspec_kw={'width_ratios': [2.5, 1, 1.3, 1.7]}, figsize=(8, 2) )
# f, (ax1, ax2, ax3) = plt.subplots(1, 3, gridspec_kw={'width_ratios': [2.5, 1, 1.2]})

# Plot 1
ax1.imshow(data[:,:,jj])
ax1.vlines(x =220, ymin =120, ymax = 200, colors = 'black', linewidth = 0.8 )
ax1.vlines(x =280, ymin =120, ymax = 200, colors = 'black', linewidth = 0.8 )
ax1.hlines(y =120, xmin =220, xmax = 280, colors = 'black', linewidth = 0.8 )
ax1.hlines(y =200, xmin =220, xmax = 280, colors = 'black', linewidth = 0.8 )
ax1.axes.get_xaxis().set_visible(False)
ax1.axes.get_yaxis().set_visible(False)

# Plot 2
ax2.imshow(slice)
ax2.axes.get_xaxis().set_visible(False)
ax2.axes.get_yaxis().set_visible(False)


# Connecting lines
con1 = ConnectionPatch(xyA=(280,200), xyB=(0,79), coordsA="data", coordsB="data",
                      axesA=ax1, axesB=ax2, color="black")
ax2.add_artist(con1)

con2 = ConnectionPatch(xyA=(280,120), xyB=(0,0), coordsA="data", coordsB="data",
                      axesA=ax1, axesB=ax2, color="black")
ax2.add_artist(con2)

# Second part
filename = '/Users/kan092/Documents/Research/TOF-SIMS/Data/brain_raster.npy'
data = np.load(filename)
jj = 1838
slice = data[160:180,220:240 ,jj]  #    120:200,220:280
shp = slice.shape
shp

xrows = np.arange(shp[0]) 
xmod = xrows%20
yrows = np.arange(shp[1]) 
ymod = yrows%20
zvals = slice.flatten()
xv, yv = np.meshgrid(ymod, xmod)


# Plot a single tile and the x and y with z coordinates
# X and Y factors
ax3.imshow(slice)
ax3.axes.get_xaxis().set_visible(False)
ax3.axes.get_yaxis().set_visible(False)

ax2.vlines(x =20, ymin =40, ymax = 60, colors = 'black', linewidth = 0.8 )
ax2.vlines(x =0, ymin =40, ymax = 60, colors = 'black', linewidth = 0.8 )
ax2.hlines(y =40, xmin =0, xmax = 20, colors = 'black', linewidth = 0.8 )
ax2.hlines(y =60, xmin =0, xmax = 20, colors = 'black', linewidth = 0.8 )

# Connecting lines
con1 = ConnectionPatch(xyA=(20,40), xyB=(0,0), coordsA="data", coordsB="data",
                      axesA=ax2, axesB=ax3, color="black")
ax3.add_artist(con1)

con2 = ConnectionPatch(xyA=(20,60), xyB=(0,19), coordsA="data", coordsB="data",
                      axesA=ax2, axesB=ax3, color="black")
ax3.add_artist(con2) 
# plt.show()

ss = ax4.get_subplotspec()
ax4.remove()
ax4 = f.add_subplot(ss, projection='3d')

#ax4 = f.add_subplot(244, projection='3d')
ax4.contour3D(xv, yv, slice, 50, cmap='binary')
ax4.tick_params(left = False, right = False , labelleft = False ,
                labelbottom = False, bottom = False)
# ax4.set_xlabel('x')
# ax4.set_ylabel('y')
# ax4.set_zlabel('z')
# # ax4.xaxis._axinfo['label']['space_factor'] = 0
# ax4.margins(0)
plt.savefig("Graphs/For_Paper/Bain_Noise_Tile.pdf", dpi = 500)
plt.show()

# -------------------------------------------------------------------------
# TASK  02: FIGURES IN COLUMN FORMAT
# -------------------------------------------------------------------------
#%% 

import numpy as np
import tensorly as tl
from tensorly.decomposition import parafac
import matplotlib.pyplot as plt
from matplotlib import cm
from matplotlib.ticker import LinearLocator
import pandas as pd
import Functions as fn
from statsmodels.tsa.seasonal import seasonal_decompose
import Functions as fn
import pickle
from scipy.signal import find_peaks
import brainRaster as ss

filename = '/Users/kan092/Documents/Research/TOF-SIMS/Data/dye_raster.npy'
data = np.load(filename)
data.shape

# Methods
# Method - tensor
data_tens = fn.tensor_tile_removal(data)

# Method - LDA
data = np.load(filename)
data_lda = fn.lda_tile_removal(data, 20)

# -----------------------------------------------------------------------------
# FIRST COLUMN
jj = 127
# Method - linear multiplicative
data = np.load(filename)
data_lin_multi = fn.linear_multiplicative(data[:,:,jj],20)

# Method - Simple interpolate
data = np.load(filename)
data_interp = fn.interpolate_x_y(data[:,:,jj],20,2)

# Method - Simple averaging
data = np.load(filename)
data_avg = fn.average_edges2(data,20,2,2)

# Method - Seamless Stitching
data = np.load(filename)
data_ss = ss.seamlessStitch(data[:,:, jj],tileSizeX = 20, tileSizeY = 20)


# -----------------------------------------------------------------------------
# SECOND COLUMN

kk = 462
# Method - linear multiplicative
data = np.load(filename)
data_lin_multi2= fn.linear_multiplicative(data[:,:,kk],20)


# Method - Simple interpolate
data = np.load(filename)
data_interp2 = fn.interpolate_x_y(data[:,:,kk],20,2)


# Method - Seamless Stitching
data = np.load(filename)
data_ss2 = ss.seamlessStitch(data[:,:, kk],tileSizeX = 20, tileSizeY = 20)


#%%
# PLOTTING
data = np.load(filename)
# metric_tens = fn.tile_removal_metrics(data[:,:,jj], data_tens[:,:,jj], 20)
# metric_lda = fn.tile_removal_metrics(data[:,:,jj], data_lda[0][:,:,jj], 20)
# metric_linear = fn.tile_removal_metrics(data[:,:,jj], data_lin_multi, 20)
# metric_interp = fn.tile_removal_metrics(data[:,:,jj], data_interp, 20)
# metric_avg = fn.tile_removal_metrics(data[:,:,jj], data_avg[:,:,jj], 20)
# metric_ss = fn.tile_removal_metrics(data[:,:,jj], data_ss, 20)

# PLOTS START - # QUESTION 1
# ------------------------------------------------------------
# Placing the plots in the plane
plt.figure(figsize=(4,14))  # figsize=(10,10
plot0 = plt.subplot2grid((7, 2), (0, 0))
plot1 = plt.subplot2grid((7, 2), (3, 0))
plot2 = plt.subplot2grid((7, 2), (2, 0))
plot3 = plt.subplot2grid((7, 2), (4, 0))
plot4 = plt.subplot2grid((7, 2), (6, 0))
plot5 = plt.subplot2grid((7, 2), (5, 0))
plot6 = plt.subplot2grid((7, 2), (1, 0))
plot10 = plt.subplot2grid((7, 2), (0, 1))
plot11 = plt.subplot2grid((7, 2), (3, 1))
plot12 = plt.subplot2grid((7, 2), (2, 1))
plot13 = plt.subplot2grid((7, 2), (4, 1))
plot14 = plt.subplot2grid((7, 2), (6, 1))
plot15 = plt.subplot2grid((7, 2), (5, 1))
plot16 = plt.subplot2grid((7, 2), (1, 1))

plt.subplots_adjust(wspace=0, hspace=0)
#plt.show()


# Remove ticks for all images

plot0.tick_params(left = False, right = False , labelleft = False ,
                labelbottom = False, bottom = False)
plot1.tick_params(left = False, right = False , labelleft = False ,
                labelbottom = False, bottom = False)
plot2.tick_params(left = False, right = False , labelleft = False ,
                labelbottom = False, bottom = False)
plot3.tick_params(left = False, right = False , labelleft = False ,
                labelbottom = False, bottom = False)
plot4.tick_params(left = False, right = False , labelleft = False ,
                labelbottom = False, bottom = False)
plot5.tick_params(left = False, right = False , labelleft = False ,
                labelbottom = False, bottom = False)
plot6.tick_params(left = False, right = False , labelleft = False ,
                labelbottom = False, bottom = False)

plot10.tick_params(left = False, right = False , labelleft = False ,
                labelbottom = False, bottom = False)
plot11.tick_params(left = False, right = False , labelleft = False ,
                labelbottom = False, bottom = False)
plot12.tick_params(left = False, right = False , labelleft = False ,
                labelbottom = False, bottom = False)
plot13.tick_params(left = False, right = False , labelleft = False ,
                labelbottom = False, bottom = False)
plot14.tick_params(left = False, right = False , labelleft = False ,
                labelbottom = False, bottom = False)
plot15.tick_params(left = False, right = False , labelleft = False ,
                labelbottom = False, bottom = False)
plot16.tick_params(left = False, right = False , labelleft = False ,
                labelbottom = False, bottom = False)
# ------------------------------------------------------------



xoffset = -0.03
yoffset = 0.3
fsz =  8
plot0.imshow(data[:,:,jj])
plot0.set_title('Control (Original)', fontsize = fsz,rotation='vertical',x=xoffset,y=yoffset)


# Method - tensor
plot1.imshow(data_tens[:,:,jj])
plot1.set_title('Tensor', fontsize = fsz, rotation='vertical',x=xoffset,y=yoffset)


# Method - linear multiplicative
plot2.imshow(data_lin_multi)
plot2.set_title('Linear', fontsize = fsz,rotation='vertical',x=xoffset,y=yoffset)


# Method - LDA
plot3.imshow(data_lda[0][:,:,jj])
plot3.set_title('LDA',fontsize = fsz,rotation='vertical',x=xoffset,y=yoffset)


# Method - Simple interpolate
plot4.imshow(data_interp)
plot4.set_title('Interpolation',fontsize = fsz,rotation='vertical',x=xoffset,y=yoffset)


# Method - Simple averaging
plot5.imshow(data_avg[:,:,jj])
plot5.set_title('Averaging',fontsize = fsz,rotation='vertical',x=xoffset,y=yoffset)


# Method - Seamless Stitching
plot6.imshow(data_ss)
plot6.set_title('Seamless Stitching',fontsize = fsz,rotation='vertical',x=xoffset,y=(yoffset-0.15))

#plt.savefig("Graphs/For_Paper/Dye_127.pdf")

#plt.show()



plot10.imshow(data[:,:,kk])
#plot10.set_title('Original', fontsize = fsz,rotation='vertical',x=xoffset,y=yoffset)


# Method - tensor
plot11.imshow(data_tens[:,:,kk])
#plot11.set_title('Tensor', fontsize = fsz, rotation='vertical',x=xoffset,y=yoffset)


# Method - linear multiplicative
plot12.imshow(data_lin_multi2)
#plot12.set_title('Linear', fontsize = fsz,rotation='vertical',x=xoffset,y=yoffset)


# Method - LDA
plot13.imshow(data_lda[0][:,:,kk])
#plot13.set_title('LDA',fontsize = fsz,rotation='vertical',x=xoffset,y=yoffset)


# Method - Simple interpolate
plot14.imshow(data_interp2)
#plot14.set_title('Interpolation',fontsize = fsz,rotation='vertical',x=xoffset,y=yoffset)


# Method - Simple averaging
plot15.imshow(data_avg[:,:,kk])
#plot15.set_title('Averaging',fontsize = fsz,rotation='vertical',x=xoffset,y=yoffset)


# Method - Seamless Stitching
plot16.imshow(data_ss2)
#plot16.set_title('Seamless Stitching',fontsize = fsz,rotation='vertical',x=xoffset,y=(yoffset-0.15))


plt.savefig("Graphs/For_Paper/Dye_127_462.pdf", dpi = 400)

plt.show()


# -------------------------------------------------------------------------
# TASK  03: FIGURES IN COLUMN FORMAT - BRAIN DATASET
# -------------------------------------------------------------------------

import numpy as np
import tensorly as tl
from tensorly.decomposition import parafac
import matplotlib.pyplot as plt
from matplotlib import cm
from matplotlib.ticker import LinearLocator
import pandas as pd
import Functions as fn
from statsmodels.tsa.seasonal import seasonal_decompose
import Functions as fn
import pickle
from scipy.signal import find_peaks
import brainRaster as ss

filename = '/Users/kan092/Documents/Research/TOF-SIMS/Data/brain_raster.npy'
data = np.load(filename)
data.shape

# Methods
# Method - tensor
data_tens = fn.tensor_tile_removal(data)

# Method - LDA
data = np.load(filename)
data_lda = fn.lda_tile_removal(data, 20)

# -----------------------------------------------------------------------------
# FIRST COLUMN
jj = 1838
# Method - linear multiplicative
data = np.load(filename)
data_lin_multi = fn.linear_multiplicative(data[:,:,jj],20)

# Method - Simple interpolate
data = np.load(filename)
data_interp = fn.interpolate_x_y(data[:,:,jj],20,2)

# Method - Simple averaging
data = np.load(filename)
data_avg = fn.average_edges2(data,20,2,2)

# Method - Seamless Stitching
data = np.load(filename)
data_ss = ss.seamlessStitch(data[:,:, jj],tileSizeX = 20, tileSizeY = 20)


# -----------------------------------------------------------------------------
# SECOND COLUMN

kk = 261
# Method - linear multiplicative
data = np.load(filename)
data_lin_multi2= fn.linear_multiplicative(data[:,:,kk],20)


# Method - Simple interpolate
data = np.load(filename)
data_interp2 = fn.interpolate_x_y(data[:,:,kk],20,2)


# Method - Seamless Stitching
data = np.load(filename)
data_ss2 = ss.seamlessStitch(data[:,:, kk],tileSizeX = 20, tileSizeY = 20)


#%%
# PLOTTING
data = np.load(filename)
# metric_tens = fn.tile_removal_metrics(data[:,:,jj], data_tens[:,:,jj], 20)
# metric_lda = fn.tile_removal_metrics(data[:,:,jj], data_lda[0][:,:,jj], 20)
# metric_linear = fn.tile_removal_metrics(data[:,:,jj], data_lin_multi, 20)
# metric_interp = fn.tile_removal_metrics(data[:,:,jj], data_interp, 20)
# metric_avg = fn.tile_removal_metrics(data[:,:,jj], data_avg[:,:,jj], 20)
# metric_ss = fn.tile_removal_metrics(data[:,:,jj], data_ss, 20)

# PLOTS START - # QUESTION 1
# ------------------------------------------------------------
# Placing the plots in the plane
plt.figure(figsize=(4,8))  # figsize=(10,10
plot0 = plt.subplot2grid((7, 2), (0, 0))
plot1 = plt.subplot2grid((7, 2), (3, 0))
plot2 = plt.subplot2grid((7, 2), (2, 0))
plot3 = plt.subplot2grid((7, 2), (4, 0))
plot4 = plt.subplot2grid((7, 2), (6, 0))
plot5 = plt.subplot2grid((7, 2), (5, 0))
plot6 = plt.subplot2grid((7, 2), (1, 0))
plot10 = plt.subplot2grid((7, 2), (0, 1))
plot11 = plt.subplot2grid((7, 2), (3, 1))
plot12 = plt.subplot2grid((7, 2), (2, 1))
plot13 = plt.subplot2grid((7, 2), (4, 1))
plot14 = plt.subplot2grid((7, 2), (6, 1))
plot15 = plt.subplot2grid((7, 2), (5, 1))
plot16 = plt.subplot2grid((7, 2), (1, 1))

plt.subplots_adjust(wspace=0, hspace=0)
#plt.show()


# Remove ticks for all images

plot0.tick_params(left = False, right = False , labelleft = False ,
                labelbottom = False, bottom = False)
plot1.tick_params(left = False, right = False , labelleft = False ,
                labelbottom = False, bottom = False)
plot2.tick_params(left = False, right = False , labelleft = False ,
                labelbottom = False, bottom = False)
plot3.tick_params(left = False, right = False , labelleft = False ,
                labelbottom = False, bottom = False)
plot4.tick_params(left = False, right = False , labelleft = False ,
                labelbottom = False, bottom = False)
plot5.tick_params(left = False, right = False , labelleft = False ,
                labelbottom = False, bottom = False)
plot6.tick_params(left = False, right = False , labelleft = False ,
                labelbottom = False, bottom = False)

plot10.tick_params(left = False, right = False , labelleft = False ,
                labelbottom = False, bottom = False)
plot11.tick_params(left = False, right = False , labelleft = False ,
                labelbottom = False, bottom = False)
plot12.tick_params(left = False, right = False , labelleft = False ,
                labelbottom = False, bottom = False)
plot13.tick_params(left = False, right = False , labelleft = False ,
                labelbottom = False, bottom = False)
plot14.tick_params(left = False, right = False , labelleft = False ,
                labelbottom = False, bottom = False)
plot15.tick_params(left = False, right = False , labelleft = False ,
                labelbottom = False, bottom = False)
plot16.tick_params(left = False, right = False , labelleft = False ,
                labelbottom = False, bottom = False)
# ------------------------------------------------------------



xoffset = -0.03
yoffset = 0.3
fsz =  8
plot0.imshow(data[:,:,jj])
plot0.set_title('Control (Ori)', fontsize = fsz,rotation='vertical',x=xoffset,y=(yoffset-0.2))


# Method - tensor
plot1.imshow(data_tens[:,:,jj])
plot1.set_title('Tensor', fontsize = fsz, rotation='vertical',x=xoffset,y=yoffset)


# Method - linear multiplicative
plot2.imshow(data_lin_multi)
plot2.set_title('Linear', fontsize = fsz,rotation='vertical',x=xoffset,y=yoffset)


# Method - LDA
plot3.imshow(data_lda[0][:,:,jj])
plot3.set_title('LDA',fontsize = fsz,rotation='vertical',x=xoffset,y=yoffset)


# Method - Simple interpolate
plot4.imshow(data_interp)
plot4.set_title('Interpolation',fontsize = fsz,rotation='vertical',x=xoffset,y=(yoffset-0.2))


# Method - Simple averaging
plot5.imshow(data_avg[:,:,jj])
plot5.set_title('Averaging',fontsize = fsz,rotation='vertical',x=xoffset,y=(yoffset-0.1))


# Method - Seamless Stitching
plot6.imshow(data_ss)
plot6.set_title('Seamless',fontsize = fsz,rotation='vertical',x=xoffset,y=(yoffset-0.15))

#plt.savefig("Graphs/For_Paper/Dye_127.pdf")

#plt.show()



plot10.imshow(data[:,:,kk])
#plot10.set_title('Original', fontsize = fsz,rotation='vertical',x=xoffset,y=yoffset)


# Method - tensor
plot11.imshow(data_tens[:,:,kk])
#plot11.set_title('Tensor', fontsize = fsz, rotation='vertical',x=xoffset,y=yoffset)


# Method - linear multiplicative
plot12.imshow(data_lin_multi2)
#plot12.set_title('Linear', fontsize = fsz,rotation='vertical',x=xoffset,y=yoffset)


# Method - LDA
plot13.imshow(data_lda[0][:,:,kk])
#plot13.set_title('LDA',fontsize = fsz,rotation='vertical',x=xoffset,y=yoffset)


# Method - Simple interpolate
plot14.imshow(data_interp2)
#plot14.set_title('Interpolation',fontsize = fsz,rotation='vertical',x=xoffset,y=yoffset)


# Method - Simple averaging
plot15.imshow(data_avg[:,:,kk])
#plot15.set_title('Averaging',fontsize = fsz,rotation='vertical',x=xoffset,y=yoffset)


# Method - Seamless Stitching
plot16.imshow(data_ss2)
#plot16.set_title('Seamless',fontsize = fsz,rotation='vertical',x=xoffset,y=(yoffset-0.15))


plt.savefig("Graphs/For_Paper/Brain_1838_261.pdf", dpi = 400)

plt.show()


# -------------------------------------------------------------------------
# TASK  04: CALCULATE THE REDUCTION IN SEASONALITY
# -------------------------------------------------------------------------
#%%
import numpy as np
import tensorly as tl
from tensorly.decomposition import parafac
import matplotlib.pyplot as plt
from matplotlib import cm
from matplotlib.ticker import LinearLocator
import pandas as pd
import Functions as fn
from statsmodels.tsa.seasonal import seasonal_decompose
import Functions as fn
import pickle
from scipy.signal import find_peaks
import brainRaster as ss


# ------------------------------------------
# DYE RASTER 
filename = '/Users/kan092/Documents/Research/TOF-SIMS/Data/dye_raster.npy'
data = np.load(filename)
data.shape

# Methods
# Method - tensor
data_tens = fn.tensor_tile_removal(data)


# Method - LDA
data = np.load(filename)
data_lda = fn.lda_tile_removal(data, 20)


# -----------------------------------------------------------------------------
# FIRST COLUMN
jj = 127
data = np.load(filename)
fn.tile_removal_seasonality(data[:,:,jj], data_tens[:,:,jj], 20)
fn.tile_removal_seasonality(data[:,:,jj], data_lda[0][:,:,jj], 20)


# Method - linear multiplicative
data = np.load(filename)
data_lin_multi = fn.linear_multiplicative(data[:,:,jj],20)
fn.tile_removal_seasonality(data[:,:,jj], data_lin_multi, 20)

# Method - Simple interpolate
data = np.load(filename)
data_interp = fn.interpolate_x_y(data[:,:,jj],20,2)
fn.tile_removal_seasonality(data[:,:,jj], data_interp, 20)

# Method - Simple averaging
data = np.load(filename)
data_avg = fn.average_edges2(data,20,2,2)
fn.tile_removal_seasonality(data[:,:,jj], data_avg[:,:,jj], 20)

# Method - Seamless Stitching
data = np.load(filename)
data_ss = ss.seamlessStitch(data[:,:, jj],tileSizeX = 20, tileSizeY = 20)
fn.tile_removal_seasonality(data[:,:,jj], data_ss, 20)

# -----------------------------------------------------------------------------
# SECOND COLUMN

kk = 462
data = np.load(filename)
fn.tile_removal_seasonality(data[:,:,kk], data_tens[:,:,kk], 20)
fn.tile_removal_seasonality(data[:,:,kk], data_lda[0][:,:,kk], 20)


# Method - linear multiplicative
data = np.load(filename)
data_lin_multi2= fn.linear_multiplicative(data[:,:,kk],20)
fn.tile_removal_seasonality(data[:,:,kk], data_lin_multi2, 20)


# Method - Simple interpolate
data = np.load(filename)
data_interp2 = fn.interpolate_x_y(data[:,:,kk],20,2)
fn.tile_removal_seasonality(data[:,:,kk], data_interp2, 20)


data = np.load(filename)
fn.tile_removal_seasonality(data[:,:,kk], data_avg[:,:,kk], 20)


# Method - Seamless Stitching
data = np.load(filename)
data_ss2 = ss.seamlessStitch(data[:,:, kk],tileSizeX = 20, tileSizeY = 20)
fn.tile_removal_seasonality(data[:,:,kk], data_ss2, 20)


# ------------------------------------------
# BRAIN RASTER 
filename = '/Users/kan092/Documents/Research/TOF-SIMS/Data/brain_raster.npy'
data = np.load(filename)
data.shape

# Methods
# Method - tensor
data_tens = fn.tensor_tile_removal(data)


# Method - LDA
data = np.load(filename)
data_lda = fn.lda_tile_removal(data, 20)


# -----------------------------------------------------------------------------
# FIRST COLUMN
jj = 1838
data = np.load(filename)
fn.tile_removal_seasonality(data[:,:,jj], data_tens[:,:,jj], 20)
fn.tile_removal_seasonality(data[:,:,jj], data_lda[0][:,:,jj], 20)


# Method - linear multiplicative
data = np.load(filename)
data_lin_multi = fn.linear_multiplicative(data[:,:,jj],20)
fn.tile_removal_seasonality(data[:,:,jj], data_lin_multi, 20)

# Method - Simple interpolate
data = np.load(filename)
data_interp = fn.interpolate_x_y(data[:,:,jj],20,2)
fn.tile_removal_seasonality(data[:,:,jj], data_interp, 20)

# Method - Simple averaging
data = np.load(filename)
data_avg = fn.average_edges2(data,20,2,2)
fn.tile_removal_seasonality(data[:,:,jj], data_avg[:,:,jj], 20)

# Method - Seamless Stitching
data = np.load(filename)
data_ss = ss.seamlessStitch(data[:,:, jj],tileSizeX = 20, tileSizeY = 20)
fn.tile_removal_seasonality(data[:,:,jj], data_ss, 20)

# -----------------------------------------------------------------------------
# SECOND COLUMN

kk = 261
data = np.load(filename)
fn.tile_removal_seasonality(data[:,:,kk], data_tens[:,:,kk], 20)
fn.tile_removal_seasonality(data[:,:,kk], data_lda[0][:,:,kk], 20)


# Method - linear multiplicative
data = np.load(filename)
data_lin_multi2= fn.linear_multiplicative(data[:,:,kk],20)
fn.tile_removal_seasonality(data[:,:,kk], data_lin_multi2, 20)


# Method - Simple interpolate
data = np.load(filename)
data_interp2 = fn.interpolate_x_y(data[:,:,kk],20,2)
fn.tile_removal_seasonality(data[:,:,kk], data_interp2, 20)


data = np.load(filename)
fn.tile_removal_seasonality(data[:,:,kk], data_avg[:,:,kk], 20)


# Method - Seamless Stitching
data = np.load(filename)
data_ss2 = ss.seamlessStitch(data[:,:, kk],tileSizeX = 20, tileSizeY = 20)
fn.tile_removal_seasonality(data[:,:,kk], data_ss2, 20)


# ------------------------------------------
# HYPERSPECTRAL RASTER 
filename = '/Users/kan092/Documents/Research/TOF-SIMS/Data/Sevvandi/hyperspectral_data.npy'
data = np.load(filename)
data.shape

# Methods
# Method - tensor
data_tens = fn.tensor_tile_removal(data)


# Method - LDA
data = np.load(filename)
data_lda = fn.lda_tile_removal(data, 10)


# -----------------------------------------------------------------------------
# FIRST COLUMN
jj = 23
data = np.load(filename)
fn.tile_removal_seasonality(data[:,:,jj], data_tens[:,:,jj], 10)
fn.tile_removal_seasonality(data[:,:,jj], data_lda[0][:,:,jj], 10)


# Method - linear multiplicative
data = np.load(filename)
data_lin_multi = fn.linear_multiplicative(data[:,:,jj],10)
fn.tile_removal_seasonality(data[:,:,jj], data_lin_multi, 10)

# Method - Simple interpolate
data = np.load(filename)
data_interp = fn.interpolate_x_y(data[:,:,jj],10,2)
fn.tile_removal_seasonality(data[:,:,jj], data_interp, 10)

# Method - Simple averaging
data = np.load(filename)
data_avg = fn.average_edges2(data,10,2,2)
fn.tile_removal_seasonality(data[:,:,jj], data_avg[:,:,jj], 10)

# Method - Seamless Stitching
data = np.load(filename)
data_ss = ss.seamlessStitch(data[:,:, jj],tileSizeX = 10, tileSizeY = 10)
fn.tile_removal_seasonality(data[:,:,jj], data_ss, 10)

# -----------------------------------------------------------------------------
# SECOND COLUMN

kk = 115
data = np.load(filename)
fn.tile_removal_seasonality(data[:,:,kk], data_tens[:,:,kk], 10)
fn.tile_removal_seasonality(data[:,:,kk], data_lda[0][:,:,kk], 10)


# Method - linear multiplicative
data = np.load(filename)
data_lin_multi2= fn.linear_multiplicative(data[:,:,kk],10)
fn.tile_removal_seasonality(data[:,:,kk], data_lin_multi2, 10)


# Method - Simple interpolate
data = np.load(filename)
data_interp2 = fn.interpolate_x_y(data[:,:,kk],10,2)
fn.tile_removal_seasonality(data[:,:,kk], data_interp2, 10)


data = np.load(filename)
fn.tile_removal_seasonality(data[:,:,kk], data_avg[:,:,kk], 10)


# Method - Seamless Stitching
data = np.load(filename)
data_ss2 = ss.seamlessStitch(data[:,:, kk],tileSizeX = 10, tileSizeY = 10)
fn.tile_removal_seasonality(data[:,:,kk], data_ss2, 10)



# ------------------------------------------
# RESISTOR RASTER 
filename = '/Users/kan092/Documents/Research/TOF-SIMS/Data/capacitor_raster.npy'
data = np.load(filename)
data.shape

# Methods
# Method - tensor
data_tens = fn.tensor_tile_removal(data)


# Method - LDA
data = np.load(filename)
data_lda = fn.lda_tile_removal(data, 50)


# -----------------------------------------------------------------------------
# FIRST COLUMN
jj = 35
data = np.load(filename)
fn.tile_removal_seasonality(data[:,:,jj], data_tens[:,:,jj], 50)
fn.tile_removal_seasonality(data[:,:,jj], data_lda[0][:,:,jj], 50)


# Method - linear multiplicative
data = np.load(filename)
data_lin_multi = fn.linear_multiplicative(data[:,:,jj],50)
fn.tile_removal_seasonality(data[:,:,jj], data_lin_multi, 50)

# Method - Simple interpolate
data = np.load(filename)
data_interp = fn.interpolate_x_y(data[:,:,jj],50,2)
fn.tile_removal_seasonality(data[:,:,jj], data_interp, 50)

# Method - Simple averaging
data = np.load(filename)
data_avg = fn.average_edges2(data,50,2,2)
fn.tile_removal_seasonality(data[:,:,jj], data_avg[:,:,jj], 50)

# Method - Seamless Stitching
data = np.load(filename)
data_ss = ss.seamlessStitch(data[:,:, jj],tileSizeX = 50, tileSizeY = 50)
fn.tile_removal_seasonality(data[:,:,jj], data_ss, 50)

# -----------------------------------------------------------------------------
# SECOND COLUMN

kk = 108
data = np.load(filename)
fn.tile_removal_seasonality(data[:,:,kk], data_tens[:,:,kk], 50)
fn.tile_removal_seasonality(data[:,:,kk], data_lda[0][:,:,kk], 50)


# Method - linear multiplicative
data = np.load(filename)
data_lin_multi2= fn.linear_multiplicative(data[:,:,kk],50)
fn.tile_removal_seasonality(data[:,:,kk], data_lin_multi2, 50)


# Method - Simple interpolate
data = np.load(filename)
data_interp2 = fn.interpolate_x_y(data[:,:,kk],50,2)
fn.tile_removal_seasonality(data[:,:,kk], data_interp2, 50)


data = np.load(filename)
fn.tile_removal_seasonality(data[:,:,kk], data_avg[:,:,kk], 50)


# Method - Seamless Stitching
data = np.load(filename)
data_ss2 = ss.seamlessStitch(data[:,:, kk],tileSizeX = 50, tileSizeY = 50)
fn.tile_removal_seasonality(data[:,:,kk], data_ss2, 50)























































































