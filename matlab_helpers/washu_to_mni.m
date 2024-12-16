%% change dir %%

cd ~/Projects/pacman/behavioral_analyses/matlab_helpers
%% BJH021 table

bjh21 = load('~/Projects/knight_server/remote/WashU/data/_VERA/VERA_BJH021/BJH021_MNI_brain.mat')
bjh21_table = table(repmat({'BJH021'}, length(bjh21.electrodeNames), 1), bjh21.electrodeNames, bjh21.tala.electrodes(:,1), bjh21.tala.electrodes(:,2), bjh21.tala.electrodes(:,3), ...
    'VariableNames', {'subject', 'Electrode', 'X', 'Y', 'Z'});

%% BJH025 table %%

bjh25 = load('~/Projects/pacman/behavioral_analyses/data/ieeg_mni/BJH025_MNI_brain.mat')
bjh25_table = table(repmat({'BJH025'}, length(bjh25.electrodeNames), 1), bjh25.electrodeNames, bjh25.tala.electrodes(:,1), bjh25.tala.electrodes(:,2), bjh25.tala.electrodes(:,3), ...
    'VariableNames', {'subject', 'Electrode', 'X', 'Y', 'Z'});
    
%% BJH026 table %%

bjh26 = load('~/Projects/pacman/behavioral_analyses/data/ieeg_mni/BJH026_MNI_brain.mat')
bjh26_table = table(repmat({'BJH026'}, length(bjh26.electrodeNames), 1), bjh26.electrodeNames, bjh26.tala.electrodes(:,1), bjh26.tala.electrodes(:,2), bjh26.tala.electrodes(:,3), ...
    'VariableNames', {'subject', 'Electrode', 'X', 'Y', 'Z'});   
    
%% BJH027 table %%

bjh27 = load('~/Projects/pacman/behavioral_analyses/data/ieeg_mni/BJH027_MNI_brain.mat')
bjh27_table = table(repmat({'BJH027'}, length(bjh27.electrodeNames), 1), bjh27.electrodeNames, bjh27.tala.electrodes(:,1), bjh27.tala.electrodes(:,2), bjh27.tala.electrodes(:,3), ...
    'VariableNames', {'subject', 'Electrode', 'X', 'Y', 'Z'});   
    
%% BJH029 table %%

bjh29 = load('~/Projects/pacman/behavioral_analyses/data/ieeg_mni/BJH029_MNI_brain.mat')
bjh29_table = table(repmat({'BJH029'}, length(bjh29.electrodeNames), 1), bjh29.electrodeNames, bjh29.tala.electrodes(:,1), bjh29.tala.electrodes(:,2), bjh29.tala.electrodes(:,3), ...
    'VariableNames', {'subject', 'Electrode', 'X', 'Y', 'Z'});   
    
%% BJH039 table %%

bjh39 = load('~/Projects/pacman/behavioral_analyses/data/ieeg_mni/BJH039_MNI_brain.mat')
bjh39_table = table(repmat({'BJH039'}, length(bjh39.electrodeNames), 1), bjh39.electrodeNames, bjh39.tala.electrodes(:,1), bjh39.tala.electrodes(:,2), bjh39.tala.electrodes(:,3), ...
    'VariableNames', {'subject', 'Electrode', 'X', 'Y', 'Z'});   
    
%% BJH041 table %%

bjh41 = load('~/Projects/pacman/behavioral_analyses/data/ieeg_mni/BJH041_MNI_brain.mat')
bjh41_table = table(repmat({'BJH041'}, length(bjh41.electrodeNames), 1), bjh41.electrodeNames, bjh41.tala.electrodes(:,1), bjh41.tala.electrodes(:,2), bjh41.tala.electrodes(:,3), ...
    'VariableNames', {'subject', 'Electrode', 'X', 'Y', 'Z'});       

%% BJH016 table %%

bjh16 = load('~/Projects/pacman/behavioral_analyses/data/ieeg_mni/BJH016_MNI_brain.mat')
bjh16_table = table(repmat({'BJH016'}, length(bjh16.electrodeNames), 1), bjh16.electrodeNames, bjh16.tala.electrodes(:,1), bjh16.tala.electrodes(:,2), bjh16.tala.electrodes(:,3), ...
    'VariableNames', {'subject', 'Electrode', 'X', 'Y', 'Z'});

%% BJH017 table %%

bjh17 = load('~/Projects/knight_server/remote/WashU/data/_VERA/VERA_BJH017/BJH017_MNI_brain.mat')
bjh17_table = table(repmat({'BJH017'}, length(bjh17.electrodeNames), 1), bjh17.electrodeNames, bjh17.tala.electrodes(:,1), bjh17.tala.electrodes(:,2), bjh17.tala.electrodes(:,3), ...
    'VariableNames', {'subject', 'Electrode', 'X', 'Y', 'Z'});

%% BJH046 table %%

bjh46 = load('~/Projects/knight_server/remote/WashU/data/_VERA/VERA_BJH046/BJH046_MNI.mat')
bjh46_table = table(repmat({'BJH046'}, length(bjh46.electrodeNames), 1), bjh46.electrodeNames, bjh46.tala.electrodes(:,1), bjh46.tala.electrodes(:,2), bjh46.tala.electrodes(:,3), ...
    'VariableNames', {'subject', 'Electrode', 'X', 'Y', 'Z'});

%% BJH050 table %%

bjh50 = load('~/Projects/knight_server/remote/WashU/data/_VERA/VERA_BJH050/BJH050_MNI.mat')
bjh50_table = table(repmat({'BJH050'}, length(bjh50.electrodeNames), 1), bjh50.electrodeNames, bjh50.tala.electrodes(:,1), bjh50.tala.electrodes(:,2), bjh50.tala.electrodes(:,3), ...
    'VariableNames', {'subject', 'Electrode', 'X', 'Y', 'Z'});

%% BJH051 table %%

bjh51 = load('~/Projects/knight_server/remote/WashU/data/_VERA/VERA_BJH051/BJH051_MNI_brain.mat')
bjh51_table = table(repmat({'BJH051'}, length(bjh51.electrodeNames), 1), bjh51.electrodeNames, bjh51.tala.electrodes(:,1), bjh51.tala.electrodes(:,2), bjh51.tala.electrodes(:,3), ...
    'VariableNames', {'subject', 'Electrode', 'X', 'Y', 'Z'});

%% SLCH002 table %%

slch2 = load('~/Projects/pacman/behavioral_analyses/data/ieeg_mni/SLCH002_MNI_brain.mat')
slch2_table = table(repmat({'SLCH002'}, length(slch2.electrodeNames), 1), slch2.electrodeNames, slch2.tala.electrodes(:,1), slch2.tala.electrodes(:,2), slch2.tala.electrodes(:,3), ...
    'VariableNames', {'subject', 'Electrode', 'X', 'Y', 'Z'});
%% combine %%

washu_mni_table = [bjh21_table; bjh25_table; bjh26_table; bjh27_table; bjh29_table; bjh39_table; bjh41_table; bjh46_table; bjh50_table; bjh51_table; bjh16_table; bjh17_table; slch2_table]

%% Export the table to a CSV file
filename = '../data/ieeg_mni/washu_mni_table.csv';
writetable(washu_mni_table, filename);