classdef Propagator
    %PROPAGATOR Summary of this class goes here
    %   Detailed explanation goes here
    
    properties
        kineticDiag
        timeStep
    end
    
    methods
        function P = Propagator(xArray, timeStep, kinematicFactor)  
            P.timeStep = timeStep;
            numSpatialPts=length(xArray);
            spaceStep=abs(xArray(2)-xArray(1));

            kvec=1/(spaceStep*numSpatialPts)*[0:numSpatialPts/2-1 -numSpatialPts/2:-1].'; % creates frequencies from -1/(2spaceStep) to 1/(2spaceStep)

            P.kineticDiag=exp(-1i*(kvec*2*pi).^2*timeStep*kinematicFactor); % actually this is a column vector and not a diagonal matrix

        end
        function psi_out = propagateInTime(P,psi_in, potential)
            potDiag=exp(-1i*potential*P.timeStep/2).'; % actually this is a column vector and not a diagonal matrix
            psi_temp = potDiag.*psi_in;

            psi_fourier = fft(psi_temp); % fouriertransform wave function
            psi_fourier = P.kineticDiag.*psi_fourier;

            psi_temp = ifft(psi_fourier); % fouriertransform wave function back
            psi_temp = potDiag.*psi_temp;

            % renormalize the wave function
            norm = psi_temp'*psi_temp;
            psi_out = psi_temp/sqrt(norm);
        end
    end
    
end

