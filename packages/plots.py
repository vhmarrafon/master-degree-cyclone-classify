import numpy as np 
import pandas as pd
import matplotlib.pyplot as plt
import subprocess, os
import pandas as pd
from mpl_toolkits.basemap import Basemap
from global_land_mask import globe as glb
from datetime import datetime
# from mpl_toolkits.basemap import Basemap

def cyclone_type_position(data_path=None, model=None, mtype=None, cyc_type=None):
    '''
    Function to 
    '''

    cyc_position = {'lats':list(), 'lons':list()}
    # -------------------------------------- 
    if not data_path[-1] == '/':
        data_path += '/'

    if not model[-1] == '/':
        data_path += '/'
    
    if not mtype[-1] == '/':
        data_path += '/'
    # --------------------------------------

    for dir in os.listdir(data_path + model + mtype):

        # setting input path
        path = data_path + model + mtype + str(dir+'/')


        # setting CPS outputs files
        files = [file for file in os.listdir(path) if '_track.txt' in file]

        if not files:
            continue

        else:
            print(path)


        # classifing by genesis
        for file in files:

            # print(str(dir), file)

            # checking if file is empty
            if os.path.getsize(path + file) == 0:
                continue
            
            if cyc_type:
                cyc_class = subprocess.check_output(f'tail -n 1 {path + file}', shell=True)
                cyc_class = str(cyc_class).split(sep=' ')[0][2:]
            

                # counting the number of cyclones per class 
            
                if cyc_class == cyc_type:
                    # print(cyc_class)
                    # setting cyclone season

                    first_line = subprocess.check_output(f'head -n 1 {path + file}', shell=True).decode("utf-8")

                else:
                    continue

            else:
       
                first_line = subprocess.check_output(f'head -n 1 {path + file}', shell=True).decode("utf-8")
        
                # _file = pd.read_csv(path + file, sep = ' ', decimal='.')
                # _file.columns = ['date', '-1', 'lat', 'lon', 'prs', 'B', 'b1', 'vtl', 'vtu', '-1-']

                # saving lat and lon
                # print(str(first_line.split()[0]), path + file)
                # print(float(first_line.split()[1]), float(first_line.split()[2])-360, first_line )

            #__lat =  float(first_line.split()[1])
            #__lon = float(first_line.split()[2])-360
            __lat = float(first_line.split()[2])
            __lon = float(first_line.split()[3])
            __lon  = __lon - 360 if __lon > 180 else __lon

            cyc_position['lats'].append(__lat)
            cyc_position['lons'].append(__lon) 



    plot(cyc_position, cyc_type=cyc_type)

def cyctype_time_series(data_path=None, model=None, mtype=None):
    '''
    Function to 
    '''
    # -------------------------------------- 
    if not data_path[-1] == '/':
        data_path += '/'

    if not model[-1] == '/':
        data_path += '/'
    
    if not mtype[-1] == '/':
        data_path += '/'
    # --------------------------------------

    plot_dict = {'year': list(), 'EXTRATROPICAL':list(), 'SUBTROPICAL':list(), 'TROPICAL':list()}

    for dir in os.listdir(data_path + model + mtype):

        # setting input path
        path = data_path + model + mtype + str(dir+'/')


        # setting CPS outputs files
        files = [file for file in os.listdir(path) if '_track.txt' in file]

        plot_dict[f'year'].append(dir[-4:])

        for cyc_type in ['EXTRATROPICAL', 'SUBTROPICAL', 'TROPICAL']:
            plot_dict[cyc_type].append(0)

        # classifing by genesis
        for file in files:

            # print(str(dir), file)

            # checking if file is empty
            if os.path.getsize(path + file) == 0:
                continue

            cyc_class = subprocess.check_output(f'tail -n 1 {path + file}', shell=True)
            cyc_class = str(cyc_class).split(sep=' ')[0][2:]          

            # counting the number of cyclones per class 
            
            for cyc_type in ['EXTRATROPICAL', 'SUBTROPICAL', 'TROPICAL']:
                if cyc_class in [cyc_type]:
                    plot_dict[cyc_type][-1] += 1

    print(pd.DataFrame(plot_dict))


def plot_from_track():

    coords = {'lats':[], 'lons':[]}
    for ano in range(1979, 2006):
    
        fh = open(f'/home/vitorh/Documentos/CYCLONE/CPS/data/redvidascicloneseraint/track{ano}.LF01PARv.ncp','r')
        for i,line in enumerate(fh):
    
            try:
                date = datetime.strptime(line.split()[0], "%Y%m%d%H")
                lat, lon = float(line.split()[4]), float(line.split()[5])
        
            except:
             continue
            
            coords['lats'].append(lat)
            coords['lons'].append(lon)
    
        fh.close()

    plot(coords)



def plot(cyc_position, cyc_type=None):
    fig, ax = plt.subplots(figsize=(6,6), dpi=150)
    map = Basemap(llcrnrlon=-90.,llcrnrlat=-70.,urcrnrlon=5.,urcrnrlat=10.,
                  area_thresh = 0.01, resolution='h', projection='cyl')

    # setting lats
    lats = range(-70, 6, 15)
    lons = range(-90, 11, 15)
    map.drawmapboundary(fill_color='#0F4180')
    map.drawcoastlines(zorder = 0)
    map.fillcontinents(color='#3C800F',lake_color='aqua')
    # map.drawcoastlines()

    map.drawparallels(lats,labels = [False,True,True,False], linewidth=0.1)
    map.drawmeridians(lons,labels = [True,False,False,True], linewidth=0.1)

    xp ,yp = map(cyc_position['lons'], cyc_position['lats'])
    map.plot(xp, yp,  color = 'yellow', marker ='o', markersize = 1.25, ls = '')
    # map.readshapefile('../sample_files/comarques', 'comarques')
    if cyc_type:
        plt.title(f"Initial position of {cyc_type.lower()} cyclones")
        plt.savefig(f'Position_{cyc_type}', bbox_inches='tight', dpi = 300 )

    else:
        plt.title(f"Initial position of cyclones")
        plt.savefig(f'Position_cyclones.png', bbox_inches='tight', dpi = 300 )

    plt.show()
