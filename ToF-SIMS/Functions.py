#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Sep 12 17:26:15 2022

@author: kan092
"""

def tensor_staged_tile_removal(data,step):
    import math  
    import numpy as np
    from tensorly.decomposition import parafac
    import Functions as fn
    
    data_out = np.copy(data)
    
    sum_tots = np.sum(np.sum(data, axis = 0), axis = 0)
    num_times = math.ceil(max(sum_tots)/step)
    st = 0
    en = step
    for ss in range(num_times):
        inds = np.where((sum_tots > st) & (sum_tots < en))
        if len(inds[0] > 0):
            data2 = data[:,:,inds[0]]
            # parafac
            factors = parafac(data2, rank = 1)
            # reconstruct
            out = fn.reconstruct_tensor(factors)
            data_rem = data2 - out
        
            data_out[:,:,inds[0]] = data_rem
        st+= step - 1
        en+= step    
    return data_out
    

def tensor_tile_removal(data):
    from tensorly.decomposition import parafac
    import Functions as fn
    
    factors = parafac(data, rank = 1)
    out = fn.reconstruct_tensor(factors)
    data2 = data - out
    return data2
    

def reconstruct_tensor(factors):
    # factors is the decomposed tensor
    import numpy as np
    num_times = factors[1][0].shape[1];
    for i in range(num_times):
        opr1 = np.outer( factors[1][0][:,i], factors[1][1][:,i] )
        opr2 = np.tensordot(opr1, factors[1][2][:,i], axes = 0)
        if i == 0:
            opr = opr2
        else:
            opr = opr + opr2
    return opr

    
def seasonality(x,pr):
    # computes the strength of seasonality of the time series x
    from statsmodels.tsa.seasonal import seasonal_decompose
    import numpy as np
    import statistics as st
    # Additive Decomposition
    additive_decomposition = seasonal_decompose(x, model='additive', period=pr)
    additive_decomposition.plot()
    
    seas = additive_decomposition.seasonal
    resid = additive_decomposition.resid
    resid2 = resid[~np.isnan(resid)]

    sr = seas + resid
    sr2 = sr[~np.isnan(sr)]
    
    out = 1 -  st.variance(resid2)/st.variance(sr2)
    return out
    

def reconstruct_partial_tensor(factors,jj):
    # factors is the decomposed tensor
    import numpy as np
    opr1 = np.outer( factors[1][0][:,jj], factors[1][1][:,jj] )
    opr = np.tensordot(opr1, factors[1][2][:,jj], axes = 0)
    return opr


def construct_seasonal_array(xfact,yfact,zfact,pr):
    from statsmodels.tsa.seasonal import seasonal_decompose
    import numpy as np
   
    # Additive Decomposition
    x_decomposition = seasonal_decompose(xfact, model='additive', period=pr)
    y_decomposition = seasonal_decompose(yfact, model='additive', period=pr)
    x_seas = x_decomposition.seasonal
    y_seas = y_decomposition.seasonal
    opr1 = np.outer(x_seas, y_seas)
    opr = np.tensordot(opr1, zfact, axes = 0)
    return opr


def average_edges(data,period,width,surround):
    import numpy as np
    
    data_out = data
  
    # Part 1 - The rows and columns exactly on the borders
    rows = np.arange(period, data.shape[0], period) -1 
    if width == 1:
        data_out[rows,:,:] = 0.5*( data[(rows-1),:,:] + data[(rows+1),:,:])
    elif width == 2:
        data_out[rows,:,:] = 0.25*(data[(rows-2),:,:] + 
                               data[(rows-1),:,:] + 
                               data[(rows+1),:,:] + 
                               data[(rows+2),:,:] )
    elif width == 3:
        data_out[rows,:,:] = (data[(rows-3),:,:] + 
                          data[(rows-2),:,:] + 
                          data[(rows-1),:,:] + 
                          data[(rows+1),:,:] +
                          data[(rows+2),:,:] +
                          data[(rows+3),:,:] )/6
    elif width == 4:
        data_out[rows,:,:] = (data[(rows-4),:,:] + 
                          data[(rows-3),:,:] + 
                          data[(rows-2),:,:] + 
                          data[(rows-1),:,:] + 
                          data[(rows+1),:,:] +
                          data[(rows+2),:,:] +
                          data[(rows+3),:,:] +
                          data[(rows+4),:,:])/8        
    elif width == 10:
        data_out[rows,:,:] = (data[(rows-10),:,:] +
                              data[(rows-9),:,:] +
                              data[(rows-8),:,:] +
                              data[(rows-7),:,:] +
                              data[(rows-6),:,:] +                          
                              data[(rows-5),:,:] +                
                              data[(rows-4),:,:] +
                              data[(rows-3),:,:] + 
                              data[(rows-2),:,:] + 
                              data[(rows-1),:,:] + 
                              data[(rows+1),:,:] +
                              data[(rows+2),:,:] +
                              data[(rows+3),:,:] +
                              data[(rows+4),:,:] + 
                              data[(rows+5),:,:] +
                              data[(rows+6),:,:] +
                              data[(rows+7),:,:] +
                              data[(rows+8),:,:] +
                              data[(rows+9),:,:] +
                              data[(rows+10),:,:] )/20      
    
   
    cols = np.arange(period, data.shape[1], period) -1
    if width == 1:
        data_out[:,cols,:] = 0.5*( data[:,(cols-1),:] + data[:,(cols+1),:])
    elif width == 2:
        data_out[:,cols,:] = 0.25*(data[:,(cols-2),:] + 
                               data[:,(cols-1),:] + 
                               data[:,(cols+1),:] +
                               data[:,(cols+2),:] )
    elif width == 3:
        data_out[:,cols,:] = (data[:,(cols-3),:] + 
                          data[:,(cols-2),:] + 
                          data[:,(cols-1),:] + 
                          data[:,(cols+1),:] +
                          data[:,(cols+2),:] +
                          data[:,(cols+3),:]  )/6
    elif width == 4:
        data_out[:,cols,:] = (data[:,(cols-4),:] +
                          data[:,(cols-3),:] + 
                          data[:,(cols-2),:] + 
                          data[:,(cols-1),:] + 
                          data[:,(cols+1),:] +
                          data[:,(cols+2),:] +
                          data[:,(cols+3),:] +
                          data[:,(cols+4),:])/8
    elif width == 10:
        data_out[:,cols,:] = (data[:,(cols-10),:] +
                              data[:,(cols-9),:] +
                              data[:,(cols-8),:] +
                              data[:,(cols-7),:] +
                              data[:,(cols-6),:] +                          
                              data[:,(cols-5),:] +                
                              data[:,(cols-4),:] +
                              data[:,(cols-3),:] + 
                              data[:,(cols-2),:] + 
                              data[:,(cols-1),:] + 
                              data[:,(cols+1),:] +
                              data[:,(cols+2),:] +
                              data[:,(cols+3),:] +
                              data[:,(cols+4),:] + 
                              data[:,(cols+5),:] +
                              data[:,(cols+6),:] +
                              data[:,(cols+7),:] +
                              data[:,(cols+8),:] +
                              data[:,(cols+9),:] +
                              data[:,(cols+10),:] )/20  
        
    if surround==True:
       
        # Part 2 - The rows and columns 1 before the border    
        rows = np.arange(period, data.shape[0], period) -2 
        if width == 1:
            data_out[rows,:,:] = 0.5*( data[(rows-1),:,:] + data[(rows+1),:,:])
        elif width == 2:
            data_out[rows,:,:] = 0.25*(data[(rows-2),:,:] + 
                                   data[(rows-1),:,:] + 
                                   data[(rows+1),:,:] + 
                                   data[(rows+2),:,:] )
        elif width == 3:
            data_out[rows,:,:] = (data[(rows-3),:,:] + 
                              data[(rows-2),:,:] + 
                              data[(rows-1),:,:] + 
                              data[(rows+1),:,:] +
                              data[(rows+2),:,:] +
                              data[(rows+3),:,:] )/6
        elif width == 4:
            data_out[rows,:,:] = (data[(rows-4),:,:] + 
                              data[(rows-3),:,:] + 
                              data[(rows-2),:,:] + 
                              data[(rows-1),:,:] + 
                              data[(rows+1),:,:] +
                              data[(rows+2),:,:] +
                              data[(rows+3),:,:] +
                              data[(rows+4),:,:])/8        
        
       
        cols = np.arange(period, data.shape[1], period) -2
        if width == 1:
            data_out[:,cols,:] = 0.5*( data[:,(cols-1),:] + data[:,(cols+1),:])
        elif width == 2:
            data_out[:,cols,:] = 0.25*(data[:,(cols-2),:] + 
                                   data[:,(cols-1),:] + 
                                   data[:,(cols+1),:] +
                                   data[:,(cols+2),:] )
        elif width == 3:
            data_out[:,cols,:] = (data[:,(cols-3),:] + 
                              data[:,(cols-2),:] + 
                              data[:,(cols-1),:] + 
                              data[:,(cols+1),:] +
                              data[:,(cols+2),:] +
                              data[:,(cols+3),:]  )/6
        elif width == 4:
            data_out[:,cols,:] = (data[:,(cols-4),:] +
                              data[:,(cols-3),:] + 
                              data[:,(cols-2),:] + 
                              data[:,(cols-1),:] + 
                              data[:,(cols+1),:] +
                              data[:,(cols+2),:] +
                              data[:,(cols+3),:] +
                              data[:,(cols+4),:])/8    
            
        
        # Part 3 - The rows and columns 1 after the border     
        rows = np.arange(period, data.shape[0], period)  
        if width == 1:
            data_out[rows,:,:] = 0.5*( data[(rows-1),:,:] + data[(rows+1),:,:])
        elif width == 2:
            data_out[rows,:,:] = 0.25*(data[(rows-2),:,:] + 
                                   data[(rows-1),:,:] + 
                                   data[(rows+1),:,:] + 
                                   data[(rows+2),:,:] )
        elif width == 3:
            data_out[rows,:,:] = (data[(rows-3),:,:] + 
                              data[(rows-2),:,:] + 
                              data[(rows-1),:,:] + 
                              data[(rows+1),:,:] +
                              data[(rows+2),:,:] +
                              data[(rows+3),:,:] )/6
        elif width == 4:
            data_out[rows,:,:] = (data[(rows-4),:,:] + 
                              data[(rows-3),:,:] + 
                              data[(rows-2),:,:] + 
                              data[(rows-1),:,:] + 
                              data[(rows+1),:,:] +
                              data[(rows+2),:,:] +
                              data[(rows+3),:,:] +
                              data[(rows+4),:,:])/8        
        
        
        cols = np.arange(period, data.shape[1], period) -2
        if width == 1:
            data_out[:,cols,:] = 0.5*( data[:,(cols-1),:] + data[:,(cols+1),:])
        elif width == 2:
            data_out[:,cols,:] = 0.25*(data[:,(cols-2),:] + 
                                   data[:,(cols-1),:] + 
                                   data[:,(cols+1),:] +
                                   data[:,(cols+2),:] )
        elif width == 3:
            data_out[:,cols,:] = (data[:,(cols-3),:] + 
                              data[:,(cols-2),:] + 
                              data[:,(cols-1),:] + 
                              data[:,(cols+1),:] +
                              data[:,(cols+2),:] +
                              data[:,(cols+3),:]  )/6
        elif width == 4:
            data_out[:,cols,:] = (data[:,(cols-4),:] +
                              data[:,(cols-3),:] + 
                              data[:,(cols-2),:] + 
                              data[:,(cols-1),:] + 
                              data[:,(cols+1),:] +
                              data[:,(cols+2),:] +
                              data[:,(cols+3),:] +
                              data[:,(cols+4),:])/8            
        
    return data_out     



def average_edges2(data,period,width,surround):
    import numpy as np
    
    data_out = np.copy(data)

  
    # Part 1 - The rows and columns exactly on the borders
    cols = np.arange(period, data.shape[1], period) -1
    if width == 1:
        data_out[:,cols,:] = 0.5*( data[:,(cols-1),:] + data[:,(cols+1),:])
    else:
        for kk in cols:
            data_out[:,kk,:] = np.average(data[:,np.r_[(kk-width):(kk-1),(kk+1):(kk+width)] ,:], axis = 1) 
       
       
        
       
    data_trans = np.copy(np.transpose(data_out, axes = [1,0,2]))
    
    rows = np.arange(period, data.shape[0], period) -1 
    if width == 1:
        data_out[rows,:,:] = 0.5*( data[(rows-1),:,:] + data[(rows+1),:,:])
    else:
        for ll in rows:
            data_out[ll,:,:] = np.average(data_trans[:,np.r_[(ll-width):(ll-1),(ll+1):(ll+width)] ,:], axis = 1) 
       
        
    # Part 2 - The surrounding columns and rows - smoothen them too
    if surround>0:
        if (surround == 1 and width == 1):
            cols2 = np.arange(period, data.shape[1], period) - 2
            data_out[:,cols2,:] = 0.5*( data[:,(cols2-1),:] + data[:,(cols2+1),:])
            rows2 = np.arange(period, data.shape[0], period) - 2
            data_out[rows2,:,:] = 0.5*( data[(rows2-1),:,:] + data[(rows2+1),:,:])
        else:
            for ss in range(1, (surround+1)):
                if width == 1:
                    cols2 = np.arange(period, data.shape[1], period) - 1 - ss
                    data_out[:,cols2,:] = 0.5*( data[:,(cols2-1),:] + data[:,(cols2+1),:])
                    rows2 = np.arange(period, data.shape[0], period) - 1 - ss 
                    data_out[rows2,:,:] = 0.5*( data[(rows2-1),:,:] + data[(rows2+1),:,:])
                else:
                    for kk in cols:
                        data_out[:,(kk-ss),:] = np.average(data[:,np.r_[(kk-ss-width):(kk-ss-1),(kk-ss+1):(kk-ss+width)] ,:], axis = 1) 
                        data_out[:,(kk+ss),:] = np.average(data[:,np.r_[(kk+ss-width):(kk+ss-1),(kk+ss+1):(kk+ss+width)] ,:], axis = 1) 
                        
                        data_trans = np.copy(np.transpose(data_out, axes = [1,0,2]))
                    for ll in rows:
                        data_out[(ll-ss),:,:] = np.average(data_trans[:,np.r_[(ll-ss-width):(ll-ss-1),(ll-ss+1):(ll-ss+width)] ,:], axis = 1) 
                        data_out[(ll+ss),:,:] = np.average(data_trans[:,np.r_[(ll+ss-width):(ll+ss-1),(ll+ss+1):(ll+ss+width)] ,:], axis = 1) 

    return data_out
        

    
def make_labels(d1, d2, period):
    import numpy as np
    
    lab_matrix = np.zeros((d1, d2))
    cols = np.arange(period, (d2+1), period) - 1
    rows = np.arange(period, (d1+1), period) - 1
    lab_matrix[:,cols] = 1
    lab_matrix[rows,:] = 1
    
    labels = np.reshape(lab_matrix, (lab_matrix.shape[0]*lab_matrix.shape[1], -1))
    labels = labels[:,0]
    return labels
    
    
    
def lda_tile_removal(data,period):
       
    import numpy as np
    from sklearn.discriminant_analysis import LinearDiscriminantAnalysis as LDA
    from sklearn.preprocessing import StandardScaler
    import Functions as fn
    
    # unfold data
    data_unfolded = np.reshape(data, (data.shape[0]*data.shape[1], -1))

    # get labels
    labs = fn.make_labels(data.shape[0], data.shape[1], period)

    # Scale input first
    sc = StandardScaler()
    dat_unf_sc = sc.fit_transform(data_unfolded)

    # Fit LDA
    lda = LDA(n_components=1)
    lda_model = lda.fit(dat_unf_sc, labs)

    # Get LDA scores
    lda_scores = lda_model.transform(dat_unf_sc)
    # multiply lda scores with the loadings
    approx1 =  np.matmul(lda_scores, lda_model.coef_)
    # minus the result from the scaled data
    data_unf_sc_2 = dat_unf_sc - approx1
    # find the inverse transform of that
    data_unf_2 = sc.inverse_transform(data_unf_sc_2)
    # reshape it to the original shape
    data2 = data_unf_2.reshape(data.shape[0], data.shape[1], data.shape[2])
    
    # scale back approx1
    approx1_sc = sc.inverse_transform(approx1)
    # reshape it 
    approx = approx1_sc.reshape(data.shape[0], data.shape[1], data.shape[2])

    return data2, approx
    
    
def divide_by_noise(data,noise,period):
    
    import numpy as np
    
    data_out = np.copy(data)
    xlen = data.shape[0]     
    ylen = data.shape[1]
     
    x_st = 0
    x_en = period
    y_st = 0
    y_en = period
    while y_st < ylen:
        while x_st < xlen:
            chunk = data[x_st:x_en,y_st:y_en]
            chunkf = chunk.flatten()
            newz = chunkf/noise
            data_out[x_st:x_en,y_st:y_en] = newz.reshape(period,period)
            x_st = x_st + period
            x_en = x_en + period
        x_st = 0
        x_en = period    
        y_st = y_st + period
        y_en = y_en + period
    return data_out
    
    
    
def fit_noise(data,period):
    
    import numpy as np
    from sklearn.linear_model import LinearRegression
    
    shp = data.shape
    xrows = np.arange(shp[0]) 
    xmod = xrows%period
    yrows = np.arange(shp[1]) 
    ymod = yrows%period
    zvals = data.flatten()

    xv, yv = np.meshgrid(ymod, xmod)

    xx = xv.flatten()
    yy = yv.flatten()

    # Linear regression
    X = np.stack((xx, yy), axis = 1)
    reg = LinearRegression().fit(X, zvals)
    
    
    xpred = np.arange(period)
    ypred = np.arange(period)
    xxpred, yypred = np.meshgrid(ypred, xpred)
    xxp = xxpred.flatten()
    yyp = yypred.flatten()
    
    Xpred = np.stack((xxp, yyp), axis = 1)
    preds = reg.predict(Xpred)
    
    return preds


def linear_multiplicative(data, period):
    import Functions as fn
    noise = fn.fit_noise(data, period)
    out = fn.divide_by_noise(data, noise, period)
    return out


def interpolate_one_y(data,pos1,pos2):
    import numpy as np
    from scipy.interpolate import RegularGridInterpolator
    datac = np.copy(data)
    
    x = range(data.shape[0])
    pos = np.array([pos1, pos2])
    datanew = data[:,pos]
    interp = RegularGridInterpolator((x, pos), datanew,
                                     bounds_error=False, fill_value=None)
    nn = pos2 - pos1 
    yy = np.linspace(pos1, pos2, nn)
    X, Y = np.meshgrid(x, yy, indexing='ij')
    Z = interp((X,Y))
    datac[:,pos1:pos2] = Z
    return datac    
    
    
def interpolate_y(data,period,width):
    import numpy as np
    num_times = int(data.shape[1]/period )
    datac = np.copy(data)
        
    for ii in range(1,num_times):
        st = ii*period - width
        en = ii*period + width
        datac = interpolate_one_y(datac, st, en)
    return datac
    
    

def interpolate_one_x(data,pos1,pos2):
    import numpy as np
    from scipy.interpolate import RegularGridInterpolator
    
    datac = np.copy(data)
    y = range(data.shape[1])
    pos = np.array([pos1, pos2])
    datanew = data[pos,:]
    interp = RegularGridInterpolator((pos,y), datanew,
                                     bounds_error=False, fill_value=None)
    nn = pos2 - pos1 
    xx = np.linspace(pos1, pos2, nn)
    X, Y = np.meshgrid(xx, y, indexing='ij')
    Z = interp((X,Y))
    datac[pos1:pos2,:] = Z
    return datac    
    
    
def interpolate_x(data,period,width):
    import numpy as np
    num_times = int(data.shape[0]/period )
    datac = np.copy(data)
        
    for ii in range(1,num_times):
        st = ii*period - width
        en = ii*period + width
        datac = interpolate_one_x(datac, st, en)
    return datac    
    

def interpolate_x_y(data, period, width):
    import numpy as np  
    import Functions as fn
    
    datac = np.copy(data)
    datac = fn.interpolate_x(datac, period, width)
    datac = fn.interpolate_y(datac, period, width)
    return datac
    
    
    
def mutual_info_cor_x_y(data, period):
    # data is a 2D array
    from sklearn.feature_selection import mutual_info_regression
    import numpy as np
    
    
    shp = data.shape
    zvals = data.flatten()

    xmod = np.arange(shp[0])%period
    ymod = np.arange(shp[1])%period

    xv, yv = np.meshgrid(ymod, xmod)

    xx = xv.flatten()
    yy = yv.flatten()
    xy = xx + yy

    X = np.stack((xx, yy, xy), axis = 1)
    corr_xz = np.corrcoef(xx, zvals)[0,1]
    corr_yz = np. corrcoef(yy, zvals)[0,1]
    cor_xyz = np.corrcoef(xy, zvals)[0,1]
    mu1 = mutual_info_regression(X, zvals, random_state=5)   
    return mu1, corr_xz, corr_yz, cor_xyz
    
    
def tile_removal_metrics(ori, rem, period):
    # ori is original data
    # rem is tiling artefact removed data
    # period is the period of the dataset
        
    import Functions as fn
    import numpy as np
    
    mutual_cor_ori = fn.mutual_info_cor_x_y(ori, period)
    mutual_cor_rem = fn.mutual_info_cor_x_y(rem, period)
    mutual_diff = mutual_cor_ori[0] - mutual_cor_rem[0]
    mutual_diff_perc = mutual_diff/mutual_cor_ori[0]
    
    cor_diff = np.array(mutual_cor_ori[1:4]) - np.array(mutual_cor_rem[1:4])
    cor_diff_perc = cor_diff/mutual_cor_ori[1:4]
    
    seas_ori = fn.seasonality_2D(ori, period)
    seas_rem = fn.seasonality_2D(rem, period)
    seas_diff = np.array(seas_ori) - np.array(seas_rem)
    seas_diff_perc = seas_diff/seas_ori
    
    diff = abs(ori - rem)
    change_prop = np.sum(diff)/np.sum(ori)
    
    return mutual_cor_ori, seas_ori, mutual_diff_perc, cor_diff_perc, seas_diff_perc, change_prop


def seasonality_2D(data, period):
    # data is a 2D matrix
    
    import Functions as fn
    import numpy as np
    
    datax = np.sum(data, axis=0)
    datay = np.sum(data, axis=1)
    
    xseas = fn.seasonality(datax, period)
    yseas = fn.seasonality(datay, period)
    
    return xseas, yseas


def tile_removal_seasonality(ori, rem, period):
    
    import Functions as fn
    import numpy as np
    

    seas_ori = fn.seasonality_2D(ori, period)
    seas_rem = fn.seasonality_2D(rem, period)

    return seas_ori, seas_rem    





















































    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    