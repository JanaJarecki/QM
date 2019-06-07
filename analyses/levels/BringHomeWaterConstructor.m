function bringhomewater=BringHomeWaterConstructor()
    % This file creates an instance of level, which is the transport level
    bringhomewater = Level('bringhomewater');
    % Load parameters for potential and wave
    bringhomewater.Vlat=130;
    bringhomewater.waist=0.25;
    bringhomewater.kin = 0.5;

    % spatial parameters
    bringhomewater.numSpatialPts=2^7; %%% chose e.g. 2^6=64, take exponent of 2 for FFT
    bringhomewater.xMin = -1;
    bringhomewater.xMax = 1;
    bringhomewater.yMin = -150;
    %bringhomewater.yMax = 100;
    bringhomewater.yMax = 0.1;
    bringhomewater.Updateaxis();
    bringhomewater.waveScale = 300;

    % time parameters
    bringhomewater.maxT = 20.0; % in seconds
    bringhomewater.normalFps = 0.025;
    bringhomewater.normalDt = 0.002;

    % starting conditions
    bringhomewater.paramstart = [-0.8 ,-100]; % starting parameters: [xo amp]
    bringhomewater.parammid = [0.6, -100];
    bringhomewater.paramfinal = [-0.75 ,-100];

    % optimization parameters
    bringhomewater.nFourierPts = 30;
    bringhomewater.amplitudeScaleFactor = 50000;
    bringhomewater.maxCost = 100; %%% used in lsqnonlin to subtract, i.e. making it to a maximizing problem

    % Constraints for the u parameters
    bringhomewater.uMax = [bringhomewater.xMax, bringhomewater.yMax];
    bringhomewater.uMin = [bringhomewater.xMin, bringhomewater.yMin];
    bringhomewater.dudt = [8*10^-1,100];
    
    % functions for potential and Hamiltonian 
    x0 = 0.6;
    bringhomewater.potential = @(u) ( -bringhomewater.Vlat.*exp(-2.0.*(bringhomewater.x - x0).*(bringhomewater.x - x0)./(bringhomewater.waist*bringhomewater.waist)) + u(2)*exp(-2*(bringhomewater.x - u(1)).^2/bringhomewater.waist^2) );
    bringhomewater.divpotential = { @(u) ( u(2)*4/bringhomewater.waist^2.*(bringhomewater.x - u(1)).*exp(-2*(bringhomewater.x - u(1)).^2/bringhomewater.waist^2) ), @(u) ( -exp(-2*(bringhomewater.x-u(1)).^2/bringhomewater.waist^2) ) };
   
    % Set initial state to start state
    [empty,vecStart] = calcEigStates(bringhomewater.x,bringhomewater.potential([bringhomewater.paramstart(1),bringhomewater.paramstart(2)]),bringhomewater.kin, true);
    bringhomewater.startState = vecStart(:,1);

    % Set the target state function 
    [empty,vecFinal]=calcEigStates(bringhomewater.x,bringhomewater.potential([bringhomewater.paramfinal(1),bringhomewater.paramfinal(2)]),bringhomewater.kin, true);
    target = vecFinal(:,1);
    bringhomewater.targetState = @(u) getTargetState(u);
    bringhomewater.targets = [-0.95, -150, 0.4, 240];
    
    function targetState = getTargetState(u)
      % disp('test')
        if u(1)>(-0.5500)
            u(1)=-0.5500;
          %  disp('GRR')
        end
        if u(1)<-0.95
            u(1)=-0.95;
           % disp('GRR')
        end
        if u(2)>0
            u(2)=0;
           % disp('GRR')
        end
        if u(2)<-150
            u(2)=-150;
          %  disp('GRR')
        end
       % u(1)
%        % u(2)
        
        [empty,vecFinal]=calcEigStates(bringhomewater.x,bringhomewater.potential(u)+bringhomewater.Vlat.*exp(-2.0.*(bringhomewater.x - x0).*(bringhomewater.x - x0)./(bringhomewater.waist*bringhomewater.waist)),bringhomewater.kin, true);
        target = vecFinal(:,1);
        targetState = target;
    end
    % Set the default guess builders:
    bringhomewater.defaultguess = @(nInt) ...
        [linspace(bringhomewater.paramstart(1),bringhomewater.parammid(1),floor(nInt/2)) linspace(bringhomewater.parammid(1),bringhomewater.paramfinal(1),ceil(nInt/2));
                linspace(bringhomewater.paramstart(2),bringhomewater.parammid(2),floor(nInt/2)) linspace(bringhomewater.parammid(2),bringhomewater.paramfinal(2),ceil(nInt/2))]';
   
    
end
