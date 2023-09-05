#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Mar 27 12:11:53 2023

@author: kan092
"""

# -------------------------------------------------------------------------
# TASK  01: APPROXIMATION USING POLYMER DATASET
# TASK  02: BRAIN DATASET AND NOISE ONLY PORTION
# TASK  03: RESULTS - DYE RASTER DATASET
# TASK  04: RESULTS - BRAIN RASTER DATASET
# TASK  05: RESULTS - HYPERSPECTRAL DATASET
# TASK  06: RESULTS - CAPACITOR DATASET
# -------------------------------------------------------------------------











# -------------------------------------------------------------------------
# TASK  01: APPROXIMATION USING POLYMER DATASET
# -------------------------------------------------------------------------
#%%

import numpy as np
import tensorly as tl
from tensorly.decomposition import parafac
import matplotlib.pyplot as plt
import pandas as pd
import Functions as fn
from statsmodels.tsa.seasonal import seasonal_decompose


filename = '/Users/kan092/Documents/Research/TOF-SIMS/Data/Sevvandi/hyperspectral_data.npy'
data = np.load(filename)
data.shape
data
datat = tl.tensor(data)


# Do parafac decomposition on 
factors1 = parafac(data, rank = 1)

print([f.shape for f in factors1[1]])

x_factors = factors1[1][0]
x_factors.shape
y_factors = factors1[1][1]
y_factors.shape
temp_factors = factors1[1][2]
temp_factors.shape

datahat = fn.reconstruct_tensor(factors1)



plt.plot(x_factors)
plt.plot(y_factors)
plt.plot(temp_factors)



# X and Y factors
fig = plt.figure()
ax = fig.add_subplot(1, 2, 1)
ax.plot(y_factors)

ax2 = fig.add_subplot(1, 2, 2)
ax2.plot(x_factors)
plt.savefig("Graphs/For_Paper/x_y_curves_1.pdf")
plt.show()


# inds
# Out[528]: 
# Int64Index([  23,   56,   86,  115,  136,  211,  292,  337,  342,  381,  464,
#              553,  808,  886,  964, 1074, 1162, 1177, 1214, 1240, 1256, 1263,
#             1285, 1303, 1375, 1551, 1998],
#            dtype='int64')



kk = 381

resultant = np.array([datat[:,:,kk], datahat[:,:,kk]])
min_val, max_val = np.amin(resultant), np.amax(resultant)
fig = plt.figure()

ax = fig.add_subplot(1, 2, 1)
ax.imshow(datat[:,:,kk],  vmin=min_val, vmax=max_val)

ax2 = fig.add_subplot(1, 2, 2)
ax2.imshow( datahat[:,:,kk],  vmin=min_val, vmax=max_val)

plt.savefig("hyper_spectral_approx_1.pdf")

plt.show()
ax3 = fig.add_subplot(1, 3, 3)
ax3.imshow( datahat[:,:,kk])

plt.show()


# -------------------------------------------------------------------------
# TASK  02: BRAIN DATASET AND NOISE ONLY PORTION
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


filename = '/Users/kan092/Documents/Research/TOF-SIMS/Data/brain_raster.npy'
data = np.load(filename)

jj = 1838
slice = data[120:200,220:280,jj]  #   160:180,220:240


# X and Y factors
f = plt.figure()
f, (ax1, ax2) = plt.subplots(1, 2, gridspec_kw={'width_ratios': [2.5, 1]})

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
plt.savefig("Graphs/For_Paper/brain_noise_part.pdf")
plt.show()


#%%
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
fig = plt.figure()
ax1 = fig.add_subplot(1, 2, 1)
ax1.imshow(slice)

ax2 = fig.add_subplot(122, projection='3d')
ax2.contour3D(xv, yv, slice, 50, cmap='binary')
ax2.set_xlabel('x')
ax2.set_ylabel('y')
ax2.set_zlabel('z')
#plt.savefig("Graphs/For_Paper/Tile_and_Surface.pdf")
plt.show()

# -----------------------------------------------------
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
# TASK  03: RESULTS - DYE RASTER DATASET
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

filename = '/Users/kan092/Documents/Research/TOF-SIMS/Data/dye_raster.npy'
data = np.load(filename)
data.shape

# Methods
# Method - tensor
data_tens = fn.tensor_tile_removal(data)

# Method - LDA
data = np.load(filename)
data_lda = fn.lda_tile_removal(data, 20)

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

#%%
data = np.load(filename)
metric_tens = fn.tile_removal_metrics(data[:,:,jj], data_tens[:,:,jj], 20)
metric_lda = fn.tile_removal_metrics(data[:,:,jj], data_lda[0][:,:,jj], 20)
metric_linear = fn.tile_removal_metrics(data[:,:,jj], data_lin_multi, 20)
metric_interp = fn.tile_removal_metrics(data[:,:,jj], data_interp, 20)
metric_avg = fn.tile_removal_metrics(data[:,:,jj], data_avg[:,:,jj], 20)
metric_ss = fn.tile_removal_metrics(data[:,:,jj], data_ss, 20)

# PLOTS START - # QUESTION 1
# ------------------------------------------------------------
# Placing the plots in the plane
plt.figure(figsize=(8,4))
plot0 = plt.subplot2grid((4, 4), (1, 0), colspan=1, rowspan = 2)
plot1 = plt.subplot2grid((4, 4), (0, 1), colspan=1, rowspan = 2)
plot2 = plt.subplot2grid((4, 4), (0, 2), colspan=1, rowspan = 2)
plot3 = plt.subplot2grid((4, 4), (0, 3), colspan=1, rowspan = 2)
plot4 = plt.subplot2grid((4, 4), (2, 1), colspan=1, rowspan = 2)
plot5 = plt.subplot2grid((4, 4), (2, 2), colspan=1, rowspan = 2)
plot6 = plt.subplot2grid((4, 4), (2, 3), colspan=1, rowspan = 2)



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
# ------------------------------------------------------------
fsz =  8
plot0.imshow(data[:,:,jj])
plot0.set_title('Original', fontsize = fsz)


# Method - tensor
plot1.imshow(data_tens[:,:,jj])
plot1.set_title('Tensor', fontsize = fsz)


# Method - linear multiplicative
plot2.imshow(data_lin_multi)
plot2.set_title('Linear', fontsize = fsz)


# Method - LDA
plot3.imshow(data_lda[0][:,:,jj])
plot3.set_title('LDA',fontsize = fsz)


# Method - Simple interpolate
plot4.imshow(data_interp)
plot4.set_title('Interpolation',fontsize = fsz)


# Method - Simple averaging
plot5.imshow(data_avg[:,:,jj])
plot5.set_title('Averaging',fontsize = fsz)


# Method - Seamless Stitching
plot6.imshow(data_ss)
plot6.set_title('Seamless Stitching',fontsize = fsz)

plt.savefig("Graphs/For_Paper/Dye_127.pdf")

plt.show()

# ------------------------------------------------------------
# QUESTION 2
jj = 462
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


data = np.load(filename)
metric_tens = fn.tile_removal_metrics(data[:,:,jj], data_tens[:,:,jj], 20)
metric_tens
metric_lda = fn.tile_removal_metrics(data[:,:,jj], data_lda[0][:,:,jj], 20)
metric_lda
metric_linear = fn.tile_removal_metrics(data[:,:,jj], data_lin_multi, 20)
metric_linear
metric_interp = fn.tile_removal_metrics(data[:,:,jj], data_interp, 20)
metric_interp
metric_avg = fn.tile_removal_metrics(data[:,:,jj], data_avg[:,:,jj], 20)
metric_avg
metric_ss = fn.tile_removal_metrics(data[:,:,jj], data_ss, 20)
metric_ss

# PLOTS START - # QUESTION 2
# ------------------------------------------------------------
# Placing the plots in the plane
plt.figure(figsize=(8,4))
plot0 = plt.subplot2grid((4, 4), (1, 0), colspan=1, rowspan = 2)
plot1 = plt.subplot2grid((4, 4), (0, 1), colspan=1, rowspan = 2)
plot2 = plt.subplot2grid((4, 4), (0, 2), colspan=1, rowspan = 2)
plot3 = plt.subplot2grid((4, 4), (0, 3), colspan=1, rowspan = 2)
plot4 = plt.subplot2grid((4, 4), (2, 1), colspan=1, rowspan = 2)
plot5 = plt.subplot2grid((4, 4), (2, 2), colspan=1, rowspan = 2)
plot6 = plt.subplot2grid((4, 4), (2, 3), colspan=1, rowspan = 2)



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


# ------------------------------------------------------------
# QUESTION 2
fsz =  8
plot0.imshow(data[:,:,jj])
plot0.set_title('Original', fontsize = fsz)


# Method - tensor
plot1.imshow(data_tens[:,:,jj])
plot1.set_title('Tensor', fontsize = fsz)


# Method - linear multiplicative
plot2.imshow(data_lin_multi)
plot2.set_title('Linear', fontsize = fsz)


# Method - LDA
plot3.imshow(data_lda[0][:,:,jj])
plot3.set_title('LDA',fontsize = fsz)


# Method - Simple interpolate
plot4.imshow(data_interp)
plot4.set_title('Interpolation',fontsize = fsz)


# Method - Simple averaging
plot5.imshow(data_avg[:,:,jj])
plot5.set_title('Averaging',fontsize = fsz)

# Method - Seamless Stitching
plot6.imshow(data_ss)
plot6.set_title('Seamless Stitching',fontsize = fsz)

plt.savefig("Graphs/For_Paper/Dye_462.pdf")

plt.show()


# -------------------------------------------------------------------------
# TASK  04: RESULTS - BRAIN RASTER DATASET
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

filename = '/Users/kan092/Documents/Research/TOF-SIMS/Data/brain_raster.npy'
data = np.load(filename)
data.shape

# (array([  49,   73,  181,  261,  268,  332,  443,  799,  997, 1127, 1357,
 #       1394, 1505, 1664, 1678, 1838, 1861]),)


# Methods
# Method - tensor
data_tens = fn.tensor_tile_removal(data)

# Method - LDA
data = np.load(filename)
data_lda = fn.lda_tile_removal(data, 20)

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


data = np.load(filename)
metric_tens = fn.tile_removal_metrics(data[:,:,jj], data_tens[:,:,jj], 20)
metric_tens
metric_lda = fn.tile_removal_metrics(data[:,:,jj], data_lda[0][:,:,jj], 20)
metric_lda
metric_linear = fn.tile_removal_metrics(data[:,:,jj], data_lin_multi, 20)
metric_linear
metric_interp = fn.tile_removal_metrics(data[:,:,jj], data_interp, 20)
metric_interp
metric_avg = fn.tile_removal_metrics(data[:,:,jj], data_avg[:,:,jj], 20)
metric_avg
metric_ss = fn.tile_removal_metrics(data[:,:,jj], data_ss, 20)
metric_ss


# PLOTS START - # QUESTION 3
# ------------------------------------------------------------
# Placing the plots in the plane
# plt.figure(figsize=(8,2.5))
# plot0 = plt.subplot2grid((4, 4), (1, 0), colspan=1, rowspan = 2)
# plot1 = plt.subplot2grid((4, 4), (0, 1), colspan=1, rowspan = 2)
# plot2 = plt.subplot2grid((4, 4), (0, 2), colspan=1, rowspan = 2)
# plot3 = plt.subplot2grid((4, 4), (0, 3), colspan=1, rowspan = 2)
# plot4 = plt.subplot2grid((4, 4), (2, 1), colspan=1, rowspan = 2)
# plot5 = plt.subplot2grid((4, 4), (2, 2), colspan=1, rowspan = 2)


plt.figure(figsize=(8,4.5))
plot0 = plt.subplot2grid((3, 3), (1, 0), colspan=1, rowspan = 1)
plot1 = plt.subplot2grid((3, 3), (0, 1), colspan=1, rowspan = 1)
plot2 = plt.subplot2grid((3, 3), (0, 2), colspan=1, rowspan = 1)
plot3 = plt.subplot2grid((3, 3), (1, 1), colspan=1, rowspan = 1)
plot4 = plt.subplot2grid((3, 3), (1, 2), colspan=1, rowspan = 1)
plot5 = plt.subplot2grid((3, 3), (2, 1), colspan=1, rowspan = 1)
plot6 = plt.subplot2grid((3, 3), (2, 2), colspan=1, rowspan = 2)



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


# ------------------------------------------------------------
fsz =  8
plot0.imshow(data[:,:,jj])
plot0.set_title('Original', fontsize = fsz)


# Method - tensor
plot1.imshow(data_tens[:,:,jj])
plot1.set_title('Tensor', fontsize = fsz)


# Method - linear multiplicative
plot2.imshow(data_lin_multi)
plot2.set_title('Linear', fontsize = fsz)


# Method - LDA
plot3.imshow(data_lda[0][:,:,jj])
plot3.set_title('LDA',fontsize = fsz)


# Method - Simple interpolate
plot4.imshow(data_interp)
plot4.set_title('Interpolation',fontsize = fsz)


# Method - Simple averaging
plot5.imshow(data_avg[:,:,jj])
plot5.set_title('Averaging',fontsize = fsz)


# Method - Seamless Stitching
plot6.imshow(data_ss)
plot6.set_title('Seamless Stitching',fontsize = fsz)

plt.savefig("Graphs/For_Paper/Brain_1838.pdf")

plt.show()


# QUESTION 4
jj = 261
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

data = np.load(filename)
metric_tens = fn.tile_removal_metrics(data[:,:,jj], data_tens[:,:,jj], 20)
metric_tens
metric_lda = fn.tile_removal_metrics(data[:,:,jj], data_lda[0][:,:,jj], 20)
metric_lda
metric_linear = fn.tile_removal_metrics(data[:,:,jj], data_lin_multi, 20)
metric_linear
metric_interp = fn.tile_removal_metrics(data[:,:,jj], data_interp, 20)
metric_interp
metric_avg = fn.tile_removal_metrics(data[:,:,jj], data_avg[:,:,jj], 20)
metric_avg
metric_ss = fn.tile_removal_metrics(data[:,:,jj], data_ss, 20)
metric_ss


# PLOTS START - # QUESTION 4
# ------------------------------------------------------------
# Placing the plots in the plane
plt.figure(figsize=(8,4.5))
plot0 = plt.subplot2grid((3, 3), (1, 0), colspan=1, rowspan = 1)
plot1 = plt.subplot2grid((3, 3), (0, 1), colspan=1, rowspan = 1)
plot2 = plt.subplot2grid((3, 3), (0, 2), colspan=1, rowspan = 1)
plot3 = plt.subplot2grid((3, 3), (1, 1), colspan=1, rowspan = 1)
plot4 = plt.subplot2grid((3, 3), (1, 2), colspan=1, rowspan = 1)
plot5 = plt.subplot2grid((3, 3), (2, 1), colspan=1, rowspan = 1)
plot6 = plt.subplot2grid((3, 3), (2, 2), colspan=1, rowspan = 2)


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

# ------------------------------------------------------------
# QUESTION 4
fsz =  8
plot0.imshow(data[:,:,jj])
plot0.set_title('Original', fontsize = fsz)


# Method - tensor
plot1.imshow(data_tens[:,:,jj])
plot1.set_title('Tensor', fontsize = fsz)


# Method - linear multiplicative
plot2.imshow(data_lin_multi)
plot2.set_title('Linear', fontsize = fsz)


# Method - LDA
plot3.imshow(data_lda[0][:,:,jj])
plot3.set_title('LDA',fontsize = fsz)


# Method - Simple interpolate
plot4.imshow(data_interp)
plot4.set_title('Interpolation',fontsize = fsz)


# Method - Simple averaging
plot5.imshow(data_avg[:,:,jj])
plot5.set_title('Averaging',fontsize = fsz)

# Method - Seamless Stitching
plot6.imshow(data_ss)
plot6.set_title('Seamless Stitching',fontsize = fsz)

plt.savefig("Graphs/For_Paper/Brain_261.pdf")

plt.show()


# -------------------------------------------------------------------------
# TASK  05: RESULTS - HYPERSPECTRAL DATASET
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

filename = '/Users/kan092/Documents/Research/TOF-SIMS/Data/Sevvandi/hyperspectral_data.npy'
data = np.load(filename)
data.shape

# Int64Index([  23,   56,   86,  115,  136,  211,  292,  337,  342,  381,  464,
#              553,  808,  886,  964, 1074, 1162, 1177, 1214, 1240, 1256, 1263,
#             1285, 1303, 1375, 1551, 1998],

#115  #23


# Methods
# Method - tensor
data_tens = fn.tensor_tile_removal(data)

# Method - LDA
data = np.load(filename)
data_lda = fn.lda_tile_removal(data, 10)

jj = 23
# Method - linear multiplicative
data = np.load(filename)
data_lin_multi = fn.linear_multiplicative(data[:,:,jj],10)

# Method - Simple interpolate
data = np.load(filename)
data_interp = fn.interpolate_x_y(data[:,:,jj],10,2)


# Method - Simple averaging
data = np.load(filename)
data_avg = fn.average_edges2(data,10,2,2)


# Method - Seamless Stitching
data = np.load(filename)
data_ss = ss.seamlessStitch(data[:,:, jj],tileSizeX = 10, tileSizeY = 10)


data = np.load(filename)
metric_tens = fn.tile_removal_metrics(data[:,:,jj], data_tens[:,:,jj], 10)
metric_tens
metric_lda = fn.tile_removal_metrics(data[:,:,jj], data_lda[0][:,:,jj], 10)
metric_lda
metric_linear = fn.tile_removal_metrics(data[:,:,jj], data_lin_multi, 10)
metric_linear
metric_interp = fn.tile_removal_metrics(data[:,:,jj], data_interp, 10)
metric_interp
metric_avg = fn.tile_removal_metrics(data[:,:,jj], data_avg[:,:,jj], 10)
metric_avg
metric_ss = fn.tile_removal_metrics(data[:,:,jj], data_ss, 10)
metric_ss


# PLOTS START - # QUESTION 5
# ------------------------------------------------------------
# Placing the plots in the plane
plt.figure(figsize=(8,4))
plot0 = plt.subplot2grid((4, 4), (1, 0), colspan=1, rowspan = 2)
plot1 = plt.subplot2grid((4, 4), (0, 1), colspan=1, rowspan = 2)
plot2 = plt.subplot2grid((4, 4), (0, 2), colspan=1, rowspan = 2)
plot3 = plt.subplot2grid((4, 4), (0, 3), colspan=1, rowspan = 2)
plot4 = plt.subplot2grid((4, 4), (2, 1), colspan=1, rowspan = 2)
plot5 = plt.subplot2grid((4, 4), (2, 2), colspan=1, rowspan = 2)
plot6 = plt.subplot2grid((4, 4), (2, 3), colspan=1, rowspan = 2)


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


# ------------------------------------------------------------
fsz =  8
plot0.imshow(data[:,:,jj])
plot0.set_title('Original', fontsize = fsz)


# Method - tensor
plot1.imshow(data_tens[:,:,jj])
plot1.set_title('Tensor', fontsize = fsz)


# Method - linear multiplicative
plot2.imshow(data_lin_multi)
plot2.set_title('Linear', fontsize = fsz)


# Method - LDA
plot3.imshow(data_lda[0][:,:,jj])
plot3.set_title('LDA',fontsize = fsz)


# Method - Simple interpolate
plot4.imshow(data_interp)
plot4.set_title('Interpolation',fontsize = fsz)


# Method - Simple averaging
plot5.imshow(data_avg[:,:,jj])
plot5.set_title('Averaging',fontsize = fsz)

# Method - Seamless Stitching
plot6.imshow(data_ss)
plot6.set_title('Seamless Stitching',fontsize = fsz)

plt.savefig("Graphs/For_Paper/Hyperspectral_23.pdf")

plt.show()



# ------------------------------------------------------------
# QUESTION 6
jj = 115
# Method - linear multiplicative
data = np.load(filename)
data_lin_multi = fn.linear_multiplicative(data[:,:,jj],10)

# Method - Simple interpolate
data = np.load(filename)
data_interp = fn.interpolate_x_y(data[:,:,jj],10,2)


# Method - Simple averaging
data = np.load(filename)
data_avg = fn.average_edges2(data,10,2,2)


# Method - Seamless Stitching
data = np.load(filename)
data_ss = ss.seamlessStitch(data[:,:, jj],tileSizeX = 10, tileSizeY = 10)


data = np.load(filename)
metric_tens = fn.tile_removal_metrics(data[:,:,jj], data_tens[:,:,jj], 10)
metric_tens
metric_lda = fn.tile_removal_metrics(data[:,:,jj], data_lda[0][:,:,jj], 10)
metric_lda
metric_linear = fn.tile_removal_metrics(data[:,:,jj], data_lin_multi, 10)
metric_linear
metric_interp = fn.tile_removal_metrics(data[:,:,jj], data_interp, 10)
metric_interp
metric_avg = fn.tile_removal_metrics(data[:,:,jj], data_avg[:,:,jj], 10)
metric_avg
metric_ss = fn.tile_removal_metrics(data[:,:,jj], data_ss, 10)
metric_ss



# PLOTS START - # QUESTION 6
# ------------------------------------------------------------
# Placing the plots in the plane
plt.figure(figsize=(8,4))
plot0 = plt.subplot2grid((4, 4), (1, 0), colspan=1, rowspan = 2)
plot1 = plt.subplot2grid((4, 4), (0, 1), colspan=1, rowspan = 2)
plot2 = plt.subplot2grid((4, 4), (0, 2), colspan=1, rowspan = 2)
plot3 = plt.subplot2grid((4, 4), (0, 3), colspan=1, rowspan = 2)
plot4 = plt.subplot2grid((4, 4), (2, 1), colspan=1, rowspan = 2)
plot5 = plt.subplot2grid((4, 4), (2, 2), colspan=1, rowspan = 2)
plot6 = plt.subplot2grid((4, 4), (2, 3), colspan=1, rowspan = 2)

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

# ------------------------------------------------------------
fsz =  8
plot0.imshow(data[:,:,jj])
plot0.set_title('Original', fontsize = fsz)


# Method - tensor
plot1.imshow(data_tens[:,:,jj])
plot1.set_title('Tensor', fontsize = fsz)


# Method - linear multiplicative
plot2.imshow(data_lin_multi)
plot2.set_title('Linear', fontsize = fsz)


# Method - LDA
plot3.imshow(data_lda[0][:,:,jj])
plot3.set_title('LDA',fontsize = fsz)


# Method - Simple interpolate
plot4.imshow(data_interp)
plot4.set_title('Interpolation',fontsize = fsz)


# Method - Simple averaging
plot5.imshow(data_avg[:,:,jj])
plot5.set_title('Averaging',fontsize = fsz)


# Method - Seamless Stitching
plot6.imshow(data_ss)
plot6.set_title('Seamless Stitching',fontsize = fsz)


plt.savefig("Graphs/For_Paper/Hyperspectral_115.pdf")


plt.show()


# -------------------------------------------------------------------------
# TASK  06: RESULTS - CAPACITOR DATASET
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


filename = '/Users/kan092/Documents/Research/TOF-SIMS/Data/capacitor_raster.npy'
data = np.load(filename)
data.shape

#(array([  0,  35,  42,  65,  69,  72,  74, 108, 112, 116]),)

# 108 #35


# Methods
# Method - tensor
data_tens = fn.tensor_tile_removal(data)

# Method - LDA
data = np.load(filename)
data_lda = fn.lda_tile_removal(data, 50)

jj = 35
# Method - linear multiplicative
data = np.load(filename)
data_lin_multi = fn.linear_multiplicative(data[:,:,jj],50)

# Method - Simple interpolate
data = np.load(filename)
data_interp = fn.interpolate_x_y(data[:,:,jj],50,2)


# Method - Simple averaging
data = np.load(filename)
data_avg = fn.average_edges2(data,50,2,2)


# Method - Seamless Stitching
data = np.load(filename)
data_ss = ss.seamlessStitch(data[:,:, jj],tileSizeX = 50, tileSizeY = 50)


data = np.load(filename)
metric_tens = fn.tile_removal_metrics(data[:,:,jj], data_tens[:,:,jj], 50)
metric_tens
metric_lda = fn.tile_removal_metrics(data[:,:,jj], data_lda[0][:,:,jj], 50)
metric_lda
metric_linear = fn.tile_removal_metrics(data[:,:,jj], data_lin_multi, 50)
metric_linear
metric_interp = fn.tile_removal_metrics(data[:,:,jj], data_interp, 50)
metric_interp
metric_avg = fn.tile_removal_metrics(data[:,:,jj], data_avg[:,:,jj], 50)
metric_avg
metric_ss = fn.tile_removal_metrics(data[:,:,jj], data_ss, 50)
metric_ss

# PLOTS START - # QUESTION 7
# ------------------------------------------------------------
# Placing the plots in the plane
plt.figure(figsize=(8,3.5))
plot0 = plt.subplot2grid((4, 4), (1, 0), colspan=1, rowspan = 2)
plot1 = plt.subplot2grid((4, 4), (0, 1), colspan=1, rowspan = 2)
plot2 = plt.subplot2grid((4, 4), (0, 2), colspan=1, rowspan = 2)
plot3 = plt.subplot2grid((4, 4), (0, 3), colspan=1, rowspan = 2)
plot4 = plt.subplot2grid((4, 4), (2, 1), colspan=1, rowspan = 2)
plot5 = plt.subplot2grid((4, 4), (2, 2), colspan=1, rowspan = 2)
plot6 = plt.subplot2grid((4, 4), (2, 3), colspan=1, rowspan = 2)


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

# ------------------------------------------------------------
fsz =  8
plot0.imshow(data[:,:,jj])
plot0.set_title('Original', fontsize = fsz)


# Method - tensor
plot1.imshow(data_tens[:,:,jj])
plot1.set_title('Tensor', fontsize = fsz)


# Method - linear multiplicative
plot2.imshow(data_lin_multi)
plot2.set_title('Linear', fontsize = fsz)


# Method - LDA
plot3.imshow(data_lda[0][:,:,jj])
plot3.set_title('LDA',fontsize = fsz)


# Method - Simple interpolate
plot4.imshow(data_interp)
plot4.set_title('Interpolation',fontsize = fsz)


# Method - Simple averaging
plot5.imshow(data_avg[:,:,jj])
plot5.set_title('Averaging',fontsize = fsz)


# Method - Seamless Stitching
plot6.imshow(data_ss)
plot6.set_title('Seamless Stitching',fontsize = fsz)


plt.savefig("Graphs/For_Paper/Capacitor_35.pdf")

plt.show()


# QUESTION 8

jj = 108
# Method - linear multiplicative
data = np.load(filename)
data_lin_multi = fn.linear_multiplicative(data[:,:,jj],50)

# Method - Simple interpolate
data = np.load(filename)
data_interp = fn.interpolate_x_y(data[:,:,jj],50,2)


# Method - Simple averaging
data = np.load(filename)
data_avg = fn.average_edges2(data,50,2,2)


# Method - Seamless Stitching
data = np.load(filename)
data_ss = ss.seamlessStitch(data[:,:, jj],tileSizeX = 50, tileSizeY = 50)

data = np.load(filename)
metric_tens = fn.tile_removal_metrics(data[:,:,jj], data_tens[:,:,jj], 50)
metric_tens
metric_lda = fn.tile_removal_metrics(data[:,:,jj], data_lda[0][:,:,jj], 50)
metric_lda
metric_linear = fn.tile_removal_metrics(data[:,:,jj], data_lin_multi, 50)
metric_linear
metric_interp = fn.tile_removal_metrics(data[:,:,jj], data_interp, 50)
metric_interp
metric_avg = fn.tile_removal_metrics(data[:,:,jj], data_avg[:,:,jj], 50)
metric_avg
metric_ss = fn.tile_removal_metrics(data[:,:,jj], data_ss, 50)
metric_ss

# PLOTS START - # QUESTION 7
# ------------------------------------------------------------
# Placing the plots in the plane
plt.figure(figsize=(8,3.5))
plot0 = plt.subplot2grid((4, 4), (1, 0), colspan=1, rowspan = 2)
plot1 = plt.subplot2grid((4, 4), (0, 1), colspan=1, rowspan = 2)
plot2 = plt.subplot2grid((4, 4), (0, 2), colspan=1, rowspan = 2)
plot3 = plt.subplot2grid((4, 4), (0, 3), colspan=1, rowspan = 2)
plot4 = plt.subplot2grid((4, 4), (2, 1), colspan=1, rowspan = 2)
plot5 = plt.subplot2grid((4, 4), (2, 2), colspan=1, rowspan = 2)
plot6 = plt.subplot2grid((4, 4), (2, 3), colspan=1, rowspan = 2)


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


# ------------------------------------------------------------
fsz =  8
plot0.imshow(data[:,:,jj])
plot0.set_title('Original', fontsize = fsz)


# Method - tensor
plot1.imshow(data_tens[:,:,jj])
plot1.set_title('Tensor', fontsize = fsz)


# Method - linear multiplicative
plot2.imshow(data_lin_multi)
plot2.set_title('Linear', fontsize = fsz)


# Method - LDA
plot3.imshow(data_lda[0][:,:,jj])
plot3.set_title('LDA',fontsize = fsz)


# Method - Simple interpolate
plot4.imshow(data_interp)
plot4.set_title('Interpolation',fontsize = fsz)


# Method - Simple averaging
plot5.imshow(data_avg[:,:,jj])
plot5.set_title('Averaging',fontsize = fsz)

# Method - Seamless Stitching
plot6.imshow(data_ss)
plot6.set_title('Seamless Stitching',fontsize = fsz)


plt.savefig("Graphs/For_Paper/Capacitor_108.pdf")

plt.show()





# # Method - Seamless Stitching
# data = np.load(filename)
# data_ss = ss.seamlessStitch(data[:,:, jj],tileSizeX = 20, tileSizeY = 20)

# metric_ss = fn.tile_removal_metrics(data[:,:,jj], data_ss, 20)
# metric_ss

# plot6 = plt.subplot2grid((4, 4), (2, 3), colspan=1, rowspan = 2)

# plot6.tick_params(left = False, right = False , labelleft = False ,
#                 labelbottom = False, bottom = False)

# # Method - Seamless Stitching
# plot6.imshow(data_ss)
# plot6.set_title('Seamless Stitching',fontsize = fsz)


























