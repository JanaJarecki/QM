function plotter(path,speed)
    addpath('subroutines');
    addpath('levels');

    level=BringHomeWaterConstructor();
    propagator = Propagator(level.x, level.normalDt,level.kin);
    psi = level.startState;

    plotfigure=figure;

    for k= 1:size(path,1) 
       pause(speed)
       set(0,'CurrentFigure',plotfigure)
       clf
       potentialk = level.potential(path(k,:));
       psi = propagator.propagateInTime(psi, potentialk);

       theWavePlusPot = level.waveScale.*abs(psi').^2 + potentialk;

       hold on
       for p=1:size(level.targets,1)
        rectangle('Position',level.targets(p,:), 'FaceColor','g')
       end

       plot(level.x,potentialk,'k');

       % ylim([min(path(:,2))-10,max(path(:,2))+10])
       ylim([-180,10]) 
       plot(level.x,theWavePlusPot,'r');
       plot(path(:,1),path(:,2),'.b','markersize',4);
       xlabel('Position')
       ylabel('Amplitude')
       hold off
       drawnow;
    end
end
