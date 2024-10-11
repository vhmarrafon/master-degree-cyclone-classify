import os

import pandas as pd
import numpy as np
import subprocess 


def load_cycdf(path):
    
    '''
    Function to load and preliminar adjustment of DataFrame
    
    : param path str: path file of cyc
    : return pd.DataFrame
    '''
    # reading csv/txt sep = spaces
    df = pd.read_csv(path, sep='\s+')

    # reseting columns
    df = (df.T.reset_index().T).reset_index(drop=True)[:-1]
    
    df = df.rename(columns = {0:'time', 1:'lat', 2:'lon', 3:'slp', 4:'x1',
                              5:'B', 6:'vtl', 7:'vtu', 8:'class'})
    
    return df


def list_extratropical_cyclones(path):
    '''
    Function to list all extratropical cyclones into path

    :param path str: path of cyclones datas
    '''

    extra_files = []

    files = [str(file) for file in os.listdir(path) if '_track.txt' in file]

    for file in files:

        cyc_class = subprocess.check_output(f'tail -n 1 {path + file}', shell=True)
        cyc_class = str(cyc_class).split(sep=' ')[0][2:]

        if cyc_class in ['EXTRATROPICAL', 'SUBTROPICAL']:
            extra_files.append(file)


    return extra_files


def find_TT_cases(data_path=None, model=None, mtype=None):
    '''
    (III) Function to detect tropical transition cases

    :param data_path str: path of CPS datas
    :param model str: name of model directory of CPS datas
    :param mtype str: type of model, GCM or RCM

    :return : the function removing cyclones into threshold in the same date
    '''

    if not data_path[-1] == '/':
        data_path += '/'

    if not model[-1] == '/':
        data_path += '/'
    
    if not mtype[-1] == '/':
        data_path += '/'

    

    try:
        subprocess.call(f'rm -rf {data_path + "TT/" + model + mtype}', shell=True)

    except Exception as e:
        print('creating TT path in:', data_path)
        print(e)
    # looping for files
    for dir in os.listdir(data_path + model + mtype):

        tt_cases = []

         # setting input path
        path = data_path + model + mtype + str(dir+'/')

        # setting CPS outputs files
        files = list_extratropical_cyclones(path)   

        # output path
        
        opath =  data_path + 'TT/' + model + mtype + str(dir+'/')

        for file in files:
            
            n_trop = 0
            flex = 0
            trop_cond = 0
            trop_cond2 = 0
            extra_b = 0

            df = load_cycdf(path + file)

            for n in df.index:

                if n <= 7:
                    extra_b += 1
                    try:
                        if float(df['vtl'][n]) <= 10 and float(df['vtu'][n]) <=0.: 
                            continue
                            

                    except:
                        print(path+file)
                        continue

                else:

                    if extra_b >= 3:
                        if np.abs(float(df['B'][n])) <= 10. and float(df['vtl'][n]) >= -5  and float(df['vtu'][n]) >= -50  and  float(df['lat'][n]) >= -40.:
                            n_trop += 1

                        else:
                            flex += 1

                        if flex > 2 and n_trop < 4:
                            flex = 0
                            n_trop = 0
                            trop_cond =0

                        if float(df['vtu'][n]) > 0:
                            trop_cond += 1

                        if float(df['vtl'][n]) > 0:
                            trop_cond2 += 1

                    else:
                        break


            if n_trop >= 4 and trop_cond >= 1 and trop_cond2 >= 2:
                tt_cases.append(path + file)
               # break

        if not tt_cases:
            continue


        os.makedirs(opath, exist_ok=True)
        
        

        for file in tt_cases:
            
            subprocess.call(f'cp {file} {opath}', shell=True)
            print(file)

