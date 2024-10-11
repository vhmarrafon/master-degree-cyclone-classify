import packages.core as cyc_pck
import packages.TT as tt
import packages.plots as cyc_plt

# setting path parameters
data_path = '/home/vitorh/Documentos/CYCLONE/CPS/data/'

# 
for model in ['MPI/', 'GFDL/', 'HadGEM/']:

    for mtype in ['GCM/', 'RCM/']:


# 
#for model in ['ERA-Int/']:
#    for mtype in ['REANALYSIS/']:


        print(f'started : {model[:-1]} {mtype[:-1]}')
        
        # #classifing genesis of cyclone
        #cyc_pck.genesis_classifier(data_path=data_path, model=model, mtype=mtype)

      #   # #  filtering by distance 
      #   cyc_pck.filter_distance(data_path=data_path+'output/', model=model, mtype=mtype)

        # # cyclones 1st step infos
        # cyc_pck.genesis_infos(data_path=data_path+'output/', model=model, mtype=mtype)
        #cyc_plt.plot_from_track()

        tt.find_TT_cases(data_path=data_path+'output/', model=model, mtype=mtype)
      #   
      

        cyc_pck.genesis_infos(data_path=data_path+'output/TT/', model=model, mtype=mtype)

        # for ctype in [ 'EXTRATROPICAL']:
        #         cyc_plt.cyclone_type_position(data_path=data_path+'output/', model=model, mtype=mtype, cyc_type=ctype)


