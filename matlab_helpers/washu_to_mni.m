%% change dir %%

cd ~/Projects/pacman/behavioral_analysis/matlab_helpers
%% BJH021 table

bjh21 = load('~/Projects/knight_server/remote/WashU/data/_VERA/VERA_BJH021/BJH021_MNI_brain.mat')
bjh21_table = table(repmat({'BJH021'}, length(bjh21.electrodeNames), 1), bjh21.electrodeNames, bjh21.tala.electrodes(:,1), bjh21.tala.electrodes(:,2), bjh21.tala.electrodes(:,3), ...
    'VariableNames', {'subject', 'Electrode', 'X', 'Y', 'Z'});

%% BJH025 table %%

bjh25 = load('~/Projects/pacman/behavioral_analysis/data/ieeg_mni/BJH025_MNI_brain.mat')
bjh25_table = table(repmat({'BJH025'}, length(bjh25.electrodeNames), 1), bjh25.electrodeNames, bjh25.tala.electrodes(:,1), bjh25.tala.electrodes(:,2), bjh25.tala.electrodes(:,3), ...
    'VariableNames', {'subject', 'Electrode', 'X', 'Y', 'Z'});

%% BJH016 table %%

bjh16 = load('~/Projects/pacman/behavioral_analysis/data/ieeg_mni/BJH016_MNI_brain.mat')
bjh16_table = table(repmat({'BJH016'}, length(bjh16.electrodeNames), 1), bjh16.electrodeNames, bjh16.tala.electrodes(:,1), bjh16.tala.electrodes(:,2), bjh16.tala.electrodes(:,3), ...
    'VariableNames', {'subject', 'Electrode', 'X', 'Y', 'Z'});

%% SLCH002 table %%

slch2 = load('~/Projects/pacman/behavioral_analysis/data/ieeg_mni/SLCH002_MNI_brain.mat')
slch2_table = table(repmat({'SLCH002'}, length(slch2.electrodeNames), 1), slch2.electrodeNames, slch2.tala.electrodes(:,1), slch2.tala.electrodes(:,2), slch2.tala.electrodes(:,3), ...
    'VariableNames', {'subject', 'Electrode', 'X', 'Y', 'Z'});
%% combine %%

washu_mni_table = [bjh21_table; bjh25_table; bjh16_table; slch2_table]

%% Export the table to a CSV file
filename = '../data/ieeg_mni/washu_mni_table.csv';
writetable(washu_mni_table, filename);