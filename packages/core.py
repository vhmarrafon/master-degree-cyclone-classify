import pandas as pd
import numpy as np
import subprocess, os, io
import packages.genese_class as phcyc
from geopy import distance
from global_land_mask import globe as glb
from shutil import copyfile
import math
from datetime import datetime, timedelta

# import cartopy.io.shapereader as shpreader
# import shapely.geometry as sgeom
# from shapely.ops import unary_union
# from shapely.prepared import prep


def genesis_classifier(data_path=None, model=None, mtype=None, interval=None):
    '''
    (I) Function to classifier the cyclone genisis phase 
    
    :param data_path str: path of CPS datas
    :param model str: name of model directory of CPS datas
    :param mtype str: type of model, GCM or RCM
    :param count bool: if True count the number of cyclones by kind
    :return : the function write the outputs with genesis classifier of cyclones 
    '''
    if not data_path[-1] == '/':
        data_path += '/'

    if not model[-1] == '/':
        data_path += '/'
    
    if not mtype[-1] == '/':
        data_path += '/'

    # looping of folders
    for dir in os.listdir(data_path + model + mtype):
        
        if dir.lower() == 'output':
            continue
        # setting input path
        path = data_path + model + mtype + str(dir+'/')

        # setting CPS outputs files
        files = [file for file in os.listdir(path) if '_track.txt' in file]

        # setting output path
        cyc_out_path = f"{data_path}output/{model}{mtype}{str(dir)}/"
        subprocess.call(['rm','-rf', cyc_out_path])
        subprocess.call(['mkdir', cyc_out_path])

        # classifing by genesis
        for file in files:

            # checking if file is empty
            if os.path.getsize(path + file) == 0:
                print(f'file ({path + file}) with null size')
                continue
            

            # setting the number of rows in file
            number_lines = subprocess.check_output(f'wc -l {path + file}', shell=True)
            number_lines = int(str(number_lines).split(sep=' ')[0][2:])
            
            # checking if cyclone has 24h or more

            if number_lines < 5:
                continue
        
            
            # setting cyclone season
            first_line = subprocess.check_output(f'head -n 1 {path + file}', shell=True).decode("utf-8")
            # with open(path + file, "r") as _file:
            #     for first_line in _file:
            #         break

            # getting genesis infos
            month =  datetime.strptime(first_line.split()[0], "%Y%m%d%H").month


            if interval:
                year = datetime.strptime(first_line.split()[0], "%Y%m%d%H").year

                if not (interval[0] <= int(year) <= interval[1]):
                    continue


            if month in [1, 2, 12]:
                sea = 'DJF'

            elif month in [3, 4, 5]:
                sea = 'MAM'

            elif month in [6, 7, 8]:
                sea = 'JJA'

            elif month in [9, 10, 11]:
                sea = 'SON'
 

            if check_output_ocean(path + file, dx=180):
                # calling fortran routine to classifier cyclones
                phcyc.genese_classifier(path + file, len(path + file), int(number_lines), sea)

                # saving outputs
                # if check_output_ocean('/home/vitorh/Documentos/CYCLONE/CPS/output_pure.txt', _dlon):
                subprocess.call(['mv','-f','output_pure.txt',f'{cyc_out_path}output_{file}'])
                    # subprocess.call(f'cp {(path + file)[:-9]}*phase*png {cyc_out_path}', shell=True)
                  
            else:
                continue
            


def genesis_infos(data_path=None, model=None, mtype=None):
    '''
    (*) Function to check basic stats of genesis classifier 

    :param data_path str: path of CPS datas
    :param model str: name of model directory of CPS datas
    :param mtype str: type of model, GCM or RCM
    :return : the function write the general stats of number of cyclones 

    '''

    # -------------------------------------- 
    if not data_path[-1] == '/':
        data_path += '/'

    if not model[-1] == '/':
        data_path += '/'
    
    if not mtype[-1] == '/':
        data_path += '/'
    # -------------------------------------- 

    # count
    count = dict()
    for cyc in ['EXTRATROPICAL', 'TROPICAL', 'SUBTROPICAL', 'ANALYSIS']:
        count[cyc] = 0

    # looping for files
    for dir in os.listdir(data_path + model + mtype):

        # setting input path
        path = data_path + model + mtype + str(dir+'/')

        # setting CPS outputs files
        files = [file for file in os.listdir(path) if '_track.txt' in file]    
        
        # classifing by genesis
        for file in files:

            # checking if file is empty
            if os.path.getsize(path + file) == 0:
                continue

            cyc_class = subprocess.check_output(f'tail -n 1 {path + file}', shell=True)
            cyc_class = str(cyc_class).split(sep=' ')[0][2:]
            

            # counting the number of cyclones per class 
            for cyc in ['EXTRATROPICAL', 'TROPICAL', 'SUBTROPICAL', 'ANALYSIS']:
                if cyc_class == cyc:
                    count[cyc] += 1
    
    print('Total number of cyclones:')
    for cyc in ['EXTRATROPICAL', 'TROPICAL', 'SUBTROPICAL', 'ANALYSIS']:
        print(f'{cyc}: {count[cyc]}')


def filter_distance(data_path=None, model=None, mtype=None, threshold=550. ):
    '''
    (II) Function to filter cyclones by distance

    :param data_path str: path of CPS datas
    :param model str: name of model directory of CPS datas
    :param mtype str: type of model, GCM or RCM
    :param threshold float: threshold in km of distance into two cyclones
    :return : the function removing cyclones into threshold in the same date

    '''

    # -------------------------------------- 
    if not data_path[-1] == '/':
        data_path += '/'

    if not model[-1] == '/':
        data_path += '/'
    
    if not mtype[-1] == '/':
        data_path += '/'
    # -------------------------------------- 


    # looping for files
    for dir in os.listdir(data_path + model + mtype):
        
        # filter
        fill = {
            'lats': list(), 'lons': list(), 'date':list(), 
            'file':list(), 'nlin': list(), 'class': list()
            }

        # setting input path
        path = data_path + model + mtype + str(dir+'/')

        # setting CPS outputs files
        files = [file for file in os.listdir(path) if '_track.txt' in file]     

        for file in files:
            # setting the number of rows in file
            number_lines = subprocess.check_output(f'wc -l {path + file}', shell=True)
            
            number_lines = int(str(number_lines).split(sep=' ')[0][2:])
            
            # getting cyclone
            cyc_class = subprocess.check_output(f'tail -n 1 {path + file}', shell=True)
            cyc_class = str(cyc_class).split(sep=' ')[0][2:]

            # if cyc_class in ['EXTRATROPICAL']:
            #     if not check_output_ocean(path + file, out=True, flex=False):
            #         subprocess.call(f'rm -rf {path+file}', shell=True)
            #         continue


            # setting filter params
            # print(path + file)
            with io.open(path + file, mode="r", encoding="utf-8") as cycfile:
                # cycfile = _file.readline()

                nlin=1
                cont = True
                
                for line in cycfile:
                    try:
                        _date = str(line.split()[0])
                        _lat = float(line.split()[1])
                        _lon = float(line.split()[2])
                        _lon = _lon - 360 if float(_lon) > 180. else _lon

                    except:
                        print(cycfile)
                        break

                    if _date in fill['date']:
                        same_date = np.argwhere(np.array(fill['date']) == _date)
                        
                        for dt in same_date: # 
                            if (fill['file'][int(dt)] != str(path+file)):
                            
                                pt1 = (_lat, _lon)
                                _lon2 = fill['lons'][int(dt)]
                                _lon2 -= +360 if float(_lon2) > 180. else 0
                                pt2 = (fill['lats'][int(dt)], _lon2)

                                # Haversine formula to calculate distance
                                dist = distance.distance(pt1, pt2).km
                                #dl = math.radians(np.abs(pt1[1] - pt2[1]))
                                #lat1, lat2 = math.radians(pt1[0]), math.radians(pt2[0])
                                #radius = 6371
                                #dist = np.arccos(np.sin(lat1) * np.sin(lat2) + np.cos(lat1) * np.cos(lat2) * np.cos(dl))*radius
                                #print('DISTANCIAS :', dist, _dist)
                                if  dist <= threshold and fill['class'][int(dt)] == cyc_class:
                                    if fill['nlin'][int(dt)] > number_lines:
                                        subprocess.call(['rm','-rf', path+file])
                                        cont = False
                                        
                                    else:
                                        subprocess.call(['rm','-rf', fill['file'][int(dt)]])
                                    
                                    
                                    break

                    if cont:
                        fill['date'].append(_date)
                        fill['lats'].append(_lat)
                        fill['lons'].append(_lon)
                        fill['file'].append(path+file)
                        fill['nlin'].append(number_lines)
                        fill['class'].append(cyc_class)
                        nlin += 1
                    else:
                        break


                    if nlin == number_lines-1:
                        break


def check_output_ocean(_file, dx=None, out=False, flex=True):

    try:

        first_line = subprocess.check_output(f'head -n 1 {_file}', shell=True).decode("utf-8")
        last_line = subprocess.check_output(f'tail -n 1 {_file}', shell=True).decode("utf-8")

        __lat = float(first_line.split()[2])
        __lon = float(first_line.split()[3])
        
        __latf = float(last_line.split()[2])
        __lonf = float(last_line.split()[3])
        # __lat = float(first_line.split()[1]) if __lat >= -12. else __lat
        # __lon = float(first_line.split()[2]) if __lat >= -12. else __lon

        __lon  = __lon - 360 if __lon > 180 else __lon
        __lonf  = __lonf - 360 if __lonf > 180 else __lonf

        radius = 6371.
        _dlon = (dx / (radius * np.cos(math.radians(__lat)))) * (180. / math.pi)
        if flex:
            case1 = (glb.is_ocean(__lat, __lon) or glb.is_ocean(__lat, __lon + _dlon) or glb.is_ocean(__lat - 1.4, __lon))  and (-70. <= __lon <= -20.)

            if case1 and glb.is_ocean(__latf, __lonf) :
                return True


            else:
                return False

        else:
            return glb.is_ocean(__lat, __lon)

    except Exception as e:
        print(_file, e)
        return False