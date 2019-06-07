function [fidelities,optimumPath,iterations] = runOptimization(level,optimizationMethod,startguess,optParams,PlotPath)
    if PlotPath
        plot=Plotter(level);
    else
        plot=0;
    end
    %Your optimazation method should NOT use more inputs than level, plot,
    %startguess and optParms
    %optParams.
    switch optimizationMethod
        case 'Krotov'
            addpath('Krotov');
            [fidelities, optimumPath,iterations] = Krotov(level,plot,startguess,optParams);
        case 'Simplex'
            addpath('Simplex');
            [fidelities, optimumPath,iterations] = Simplex(level,plot,startguess,optParams);
        case 'PSO'
            addpath('PSO');
            [fidelities, optimumPath,iterations] = PSO(level,plot,startguess,optParams);
        otherwise
            error('Unknown optimization Method')
    end
    close(plot.pathFigure);
end


