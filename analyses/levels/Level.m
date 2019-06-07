classdef Level < handle
    
    properties
        levelname;          % Level name.
        
        % Potenetial and wave parameters:
        Vlat;               % Amplitude of static potential.
        waist;              % Waist of the tweezer       
        kin;                % The kinematic factor h^2/2m in reduced units.
        waveScale;          % How much the normsquare of the wavefunction is scaled when plotted.
        potential;          % The potential function
        divpotential;       % Derivative of the potential       
        
        % Spatial parameters:
        numSpatialPts;      % Number of points on x-axis. Should be 2^x for FFT.
        xMin;               % The size of the x-axis.
        xMax; 
        yMin;               % The size of the y-axis.
        yMax;
        x;                  % The x-axis.
        dx;                 % The x stepsize
        
        % Time parameters:
        maxT;               % Maximal time usage allowed in game seconds
        normalFps;          % Time between frames in game seconds
        normalDt;           % Time steps used in simulation
    
        % Initial and final:
        paramstart;         % Start values of control parameters
        parammid;           % Mid values of control parameters, used in BringHomeWater
        paramfinal;         % Final values of control parameters
        
        startState;         % The initial state vector
        targetState;        % The final state function of parameters. This is a function since the final state may change.
        
        % Optimization parameters:
        nFourierPts;
        amplitudeScaleFactor;
        maxCost;            % Used in lsqnonlin to subtract, i.e. making it to a maximizing problem
        
         % Constraints on the u parameter
        dudt;              % Maximal allowed change in controlparameters
        uMax;              % Maximal value of controlparameter
        uMin;              % Mininum value of controlparameter
        
        % TransportTargets
        targets;            % Target area
        obstacles;          % Obstacles
        points;             % Positions of points
        
        % Functions calculating the default guesses
        defaultguess;
    end

    methods
         function obj = Level(name)
            obj.levelname=name;
         end
         
         % Functions that load parameters
         function Updateaxis(self)
            self.x=linspace(self.xMin,self.xMax,self.numSpatialPts);
            self.dx=self.x(2)-self.x(1);
         end  
    end
end
   