clear all; close all; clc;
load('HybridDataFix')

%nend=2;
nend=size(HybridData,2)
datatable=[];
for n=1:nend
    nextduration=HybridData{n}.TimeSteps(end);
    nextpath=HybridData{n}.Paths{end};
    
    playId=n*ones(nextduration,1);
    nexttimes=(0.002*1:nextduration)';
    nextpathx=nextpath(:,1);
    nextpathA=nextpath(:,2);
    nextFidelity=HybridData{n}.Fidelity(end)*ones(nextduration,1);
    nextOriginal=HybridData{n}.OriginalScore*ones(nextduration,1);
    DataMatrix=[playId,nexttimes,nextpathx,nextpathA,nextOriginal,nextFidelity];
    datatable=[datatable;DataMatrix];
    disp(['Added datafile n=',num2str(n)])
end

dlmwrite('HybridDataRFormat.dat',datatable,'delimiter',',','precision',10);

%load('WiggleUserData.mat')