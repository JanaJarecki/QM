function [score,fidelity] = costFunction(levelObject,u_i)
    %%% get parameters %%%
    timeStep = levelObject.normalDt;
    xArray = levelObject.x;
    dx = levelObject.dx;
    kinematicFactor = levelObject.kin;
    V = @(u) levelObject.potential(u);
    %%% comment: dVdu is inserted manually to circumvent complicated anonymous functions....

    %%% path guess
    lenT = size(u_i,1);

    psi0 = levelObject.startState;
    target = levelObject.targetState(u_i(end,:)); %% target state ALWAYS needs input from path!!

    % Find the final state for the current u
    psiT = psi0;
    for n=1:lenT-1
        psiT = propegateInTime(psiT,xArray,timeStep,kinematicFactor,V(u_i(n,:)).',true,true);
    end
    fidelity = abs(psiT'*target * dx).^2; 
    cost = fidelity;

    T = (lenT)*levelObject.normalFps; % we are using all the nInt steps in our solution
    timeScore = exp(- T / 8.0 * 0.1);
    if (fidelity <= 0.8)
        score = 50 * fidelity / 0.8;
    else
        score = 50 * (1 + (fidelity - 0.8) / 0.2);
    end
    score = score*timeScore; % Being fast is rewarded.
end