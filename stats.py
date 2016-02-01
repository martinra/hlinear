import csv
import glob
import matplotlib
import matplotlib.pyplot as plt
import os
import pandas as pd
# import numpy as np


def time_parse(st):
    num, unit = st.split(' ')
    if unit == "s":
        mult = 1
    elif unit == "m":
        mult = 60
    elif unit == "ms":
        mult = 0.001
    elif unit == "µs":
        mult = 0.000001
    return float(num)*mult

dir = '/home/ghitza/Sync/haskell/hlinear/bench_timings/'
flint_mean_dct = dict()
hlinear_mean_dct = dict()
ratio_mean_dct = dict()
for res_fname in sorted(glob.glob(os.path.join(dir, '*.data'))):
    flint_lst = []
    hlinear_lst = []
    ratio_lst = []
    with open(res_fname, "r") as fnm:
        fr = csv.reader(fnm, delimiter=',')
        for row in fr:
            flint_lst.append(time_parse(row[0].strip()))
            hlinear_lst.append(time_parse(row[1].strip()))
            f = flint_lst[-1]
            h = hlinear_lst[-1]
            if f == h:
                rat = 0
            elif f > h:
                rat = 100.0*(f-h)/h
            else:
                rat = -100.0*(h-f)/f
            ratio_lst.append(rat)
    flint_ser = pd.Series(flint_lst)
    hlinear_ser = pd.Series(hlinear_lst)
    ratio_ser = pd.Series(ratio_lst)
    key = int(os.path.basename(res_fname).split('.')[0])
    flint_mean_dct[key] = flint_ser.mean()
    hlinear_mean_dct[key] = hlinear_ser.mean()
    ratio_mean_dct[key] = ratio_ser.mean()
matplotlib.style.use('ggplot')
flint_mean_ser = pd.Series(flint_mean_dct)
flint_mean_ser.to_csv(os.path.join(dir, 'flint_mean.series'))
hlinear_mean_ser = pd.Series(hlinear_mean_dct)
hlinear_mean_ser.to_csv(os.path.join(dir, 'hlinear_mean.series'))
plt.figure()
flint_mean_ser.plot()
hlinear_mean_ser.plot()
plt.savefig(os.path.join(dir, 'flint_vs_hlinear.png'))
plt.close()
ratio_mean_ser = pd.Series(ratio_mean_dct)
ratio_mean_ser.to_csv(os.path.join(dir, 'ratio_mean.series'))
plt.figure()
ratio_mean_ser.plot()
plt.savefig(os.path.join(dir, 'ratio_mean.png'))
plt.close()
with open(os.path.join(dir, 'ratio_summary'), 'w') as fnm:
    fnm.write("%s" % ratio_mean_ser.describe())
