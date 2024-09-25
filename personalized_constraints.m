%%%%%%%%% personalized exchange fluxes %%%%%%%%%%%
%%%%%%%%%%%%%%%%%% 25-09-2024 %%%%%%%%%%%%%%%%%%%%

%%
% import_table_constraints;
opts = spreadsheetImportOptions("NumVariables", 6);
opts.Sheet = "LB";
opts.DataRange = "A2:F166";
opts.VariableNames = ["RXNID", "Minimal", "EQUATION", "VarName4", "presentinMardinoglu", "MOD"];
opts.VariableTypes = ["string", "double", "string", "string", "categorical", "double"];
opts = setvaropts(opts, ["RXNID", "EQUATION", "VarName4"], "WhitespaceRule", "preserve");
opts = setvaropts(opts, ["RXNID", "EQUATION", "VarName4", "presentinMardinoglu"], "EmptyFieldRule", "auto");

% Import the data
LB = readtable("personalized_hume.xlsx", opts, "UseExcel", false);
clear opts

%%
opts = spreadsheetImportOptions("NumVariables", 212);
opts.Sheet = "UB";
opts.DataRange = "A2:HD166";
opts.VariableNames = ["RXNID", "Minimal", "EQUATION", "VarName4", "presentinMardinoglu", "NOPERSmax", "cirrhosis_0", "cirrhosis_1", "cirrhosis_10", "cirrhosis_11", "cirrhosis_12", "cirrhosis_13", "cirrhosis_2", "cirrhosis_3", "cirrhosis_4", "cirrhosis_5", "cirrhosis_6", "cirrhosis_7", "cirrhosis_8", "cirrhosis_9", "NASH_F01_0", "NASH_F01_1", "NASH_F01_10", "NASH_F01_11", "NASH_F01_12", "NASH_F01_13", "NASH_F01_14", "NASH_F01_15", "NASH_F01_16", "NASH_F01_17", "NASH_F01_18", "NASH_F01_19", "NASH_F01_2", "NASH_F01_20", "NASH_F01_21", "NASH_F01_22", "NASH_F01_23", "NASH_F01_24", "NASH_F01_25", "NASH_F01_26", "NASH_F01_27", "NASH_F01_28", "NASH_F01_29", "NASH_F01_3", "NASH_F01_30", "NASH_F01_31", "NASH_F01_32", "NASH_F01_33", "NASH_F01_4", "NASH_F01_5", "NASH_F01_6", "NASH_F01_7", "NASH_F01_8", "NASH_F01_9", "NASH_F2_0", "NASH_F2_1", "NASH_F2_10", "NASH_F2_11", "NASH_F2_12", "NASH_F2_13", "NASH_F2_14", "NASH_F2_15", "NASH_F2_16", "NASH_F2_17", "NASH_F2_18", "NASH_F2_19", "NASH_F2_2", "NASH_F2_20", "NASH_F2_21", "NASH_F2_22", "NASH_F2_23", "NASH_F2_24", "NASH_F2_25", "NASH_F2_26", "NASH_F2_27", "NASH_F2_28", "NASH_F2_29", "NASH_F2_3", "NASH_F2_30", "NASH_F2_31", "NASH_F2_32", "NASH_F2_33", "NASH_F2_34", "NASH_F2_35", "NASH_F2_36", "NASH_F2_37", "NASH_F2_38", "NASH_F2_39", "NASH_F2_4", "NASH_F2_40", "NASH_F2_41", "NASH_F2_42", "NASH_F2_43", "NASH_F2_44", "NASH_F2_45", "NASH_F2_46", "NASH_F2_47", "NASH_F2_48", "NASH_F2_49", "NASH_F2_5", "NASH_F2_50", "NASH_F2_51", "NASH_F2_52", "NASH_F2_6", "NASH_F2_7", "NASH_F2_8", "NASH_F2_9", "NASH_F3_0", "NASH_F3_1", "NASH_F3_10", "NASH_F3_11", "NASH_F3_12", "NASH_F3_13", "NASH_F3_14", "NASH_F3_15", "NASH_F3_16", "NASH_F3_17", "NASH_F3_18", "NASH_F3_19", "NASH_F3_2", "NASH_F3_20", "NASH_F3_21", "NASH_F3_22", "NASH_F3_23", "NASH_F3_24", "NASH_F3_25", "NASH_F3_26", "NASH_F3_27", "NASH_F3_28", "NASH_F3_29", "NASH_F3_3", "NASH_F3_30", "NASH_F3_31", "NASH_F3_32", "NASH_F3_33", "NASH_F3_34", "NASH_F3_35", "NASH_F3_36", "NASH_F3_37", "NASH_F3_38", "NASH_F3_39", "NASH_F3_4", "NASH_F3_40", "NASH_F3_41", "NASH_F3_42", "NASH_F3_43", "NASH_F3_44", "NASH_F3_45", "NASH_F3_46", "NASH_F3_47", "NASH_F3_48", "NASH_F3_49", "NASH_F3_5", "NASH_F3_50", "NASH_F3_51", "NASH_F3_52", "NASH_F3_53", "NASH_F3_6", "NASH_F3_7", "NASH_F3_8", "NASH_F3_9", "Steatosis_0", "Steatosis_1", "Steatosis_10", "Steatosis_11", "Steatosis_12", "Steatosis_13", "Steatosis_14", "Steatosis_15", "Steatosis_16", "Steatosis_17", "Steatosis_18", "Steatosis_19", "Steatosis_2", "Steatosis_20", "Steatosis_21", "Steatosis_22", "Steatosis_23", "Steatosis_24", "Steatosis_25", "Steatosis_26", "Steatosis_27", "Steatosis_28", "Steatosis_29", "Steatosis_3", "Steatosis_30", "Steatosis_31", "Steatosis_32", "Steatosis_33", "Steatosis_34", "Steatosis_35", "Steatosis_36", "Steatosis_37", "Steatosis_38", "Steatosis_39", "Steatosis_4", "Steatosis_40", "Steatosis_41", "Steatosis_42", "Steatosis_43", "Steatosis_44", "Steatosis_45", "Steatosis_46", "Steatosis_47", "Steatosis_48", "Steatosis_49", "Steatosis_5", "Steatosis_50", "Steatosis_6", "Steatosis_7", "Steatosis_8", "Steatosis_9"];
opts.VariableTypes = ["string", "double", "string", "string", "categorical", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double"];
opts = setvaropts(opts, ["RXNID", "EQUATION", "VarName4"], "WhitespaceRule", "preserve");
opts = setvaropts(opts, ["RXNID", "EQUATION", "VarName4", "presentinMardinoglu"], "EmptyFieldRule", "auto");

% Import the data
UB = readtable("personalized2_hume.xlsx", opts, "UseExcel", false);
clear opts

%% 
nummodel = 206;
LBconstraints = table2cell(LB); 
lb = LBconstraints(:,6);

UBconstraints = table2cell(UB);
ub = UBconstraints(:, 7:end);

rxns_open = cellstr(UBconstraints(:,1));
precursors = rxns_open;

lb_precursor = lb;
ub_precursor = ub;

for mod = 1:nummodel
    [selExc, selUpt] = findExcRxns(Hepmodels{1,mod}, [], true); %ho controllato serve true
    rxn_selUpt = Hepmodels{1,mod}.rxns(selUpt);
    flag = ismember(rxn_selUpt, rxns_open);
    Hepmodels{1, mod} = changeObjective(Hepmodels{1,mod}, "HMR_9034_r",1);
    Hepmodels{1,mod}= changeRxnBounds(Hepmodels{1,mod},rxn_selUpt(~flag), 0, 'u');
    Hepmodels{1,mod}= changeRxnBounds(Hepmodels{1,mod},"fake_protein",13, 'u');
end

%%
for mod = 1:nummodel
   flag = ismember(precursors, Hepmodels{1, mod}.rxns);
   precursors_flag = precursors(flag, :);
   ub_flag = cell2mat(ub_precursor(flag, mod));
   lb_flag = cell2mat(lb_precursor(flag, :));
   Hepmodels{1,mod} = changeRxnBounds(Hepmodels{1,mod}, precursors_flag , ub_flag , 'u');
   Hepmodels{1,mod} = changeRxnBounds(Hepmodels{1,mod}, precursors_flag , lb_flag,  'l');
end
