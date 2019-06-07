function guess = correctGuess( guess,Params )
%%% This function is needed, because the data in the database is saved in
%%% such a way that the actual x and y position of the mouse on the screen
%%% are saved. These can be outside the game window, so here we need to
%%% correct for that. In addition, we have to correct for the static
%%% potential too in other places.

switch Params.levelname
            case 'transport'
                guess = correctConstrainedMouseMovementInGuess(guess,Params);
                guess(2,:) = guess(2,:) - ( -Params.Vlat.*(sin(pi*guess(1,:)).^2));
                %%% set the score- and fidelityCalculator
%                 Params.fidelityCalculator = FidelityCalculatorTransport(Params);
            case 'bringhomewater'
                guess = correctConstrainedMouseMovementInGuess(guess,Params);
                x0 = 0.6;
                guess(2,:) = guess(2,:) - ( -Params.Vlat.*exp(-2.0.*(guess(1,:) - x0).*(guess(1,:) - x0)./(Params.waist*Params.waist)));
                %%% set the score- and fidelityCalculator
%                 Params.fidelityCalculator = FidelityCalculatorBringHomeWater(Params);
            case 'qshakeit'
%                 Params.paramstart = guess(:,1);
%                 Params.paramfinal = guess(:,end);
                guess = correctConstrainedMouseMovementInGuess(guess,Params);
                guess(2,:) = 0;
                %%% set the score- and fidelityCalculator
%                 Params.fidelityCalculator = FidelityCalculatorQShakeIt(Params);
            case 'wiggle'
                guess = correctConstrainedMouseMovementInGuess(guess,Params);
                %%% set the score- and fidelityCalculator
%                 Params.fidelityCalculator = FidelityCalculatorWiggleOverlap(Params);
            otherwise
                error('no valid level supplied')
end

    function guess = correctConstrainedMouseMovementInGuess(guess,Params)
        guess(1,guess(1,:)<Params.xMin) = Params.xMin;
        guess(1,guess(1,:)>Params.xMax) = Params.xMax;
        guess(2,guess(2,:)<Params.yMin) = Params.yMin;
        guess(2,guess(2,:)>Params.yMax) = Params.yMax;
    end
        
end

