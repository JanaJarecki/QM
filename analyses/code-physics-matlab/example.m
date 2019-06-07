% load('HybridDataRFormat.dat') Remember to load the data first time
close all;
clc;
pathId=2;
speed=0.005;

indices=find(HybridDataRFormat(:,1)==pathId);
path=HybridDataRFormat(indices,3:4);
plotter(path,speed);