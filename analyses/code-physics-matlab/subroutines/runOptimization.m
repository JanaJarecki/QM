function [fidelities,optimumPath,iterations,varargout] = runOptimization(level,optimizationMethod,startguess,optParams,PlotPath)
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
            [fidelities, optimumPath,iterations,varargout] = Krotov_noUbug2(level,plot,startguess,optParams);
        case 'Simplex'
            addpath('Simplex');
            [fidelities, optimumPath,iterations,varargout] = Simplex(level,plot,startguess,optParams);
        case 'PSO'
            addpath('PSO');
            [fidelities, optimumPath,iterations,varargout] = PSO(level,plot,startguess,optParams);
        case 'fPSO'
            addpath('PSO');
            [fidelities, optimumPath,iterations,optimumPts,endfidelities] = fPSO(level,plot,startguess,optParams);
            varargout{1}=optimumPts;
            varargout{2}=endfidelities;
        otherwise
            error('Unknown optimization Method')
    end
    if PlotPath
        close(plot.pathFigure);
    end
end


