%propegates a wavefunction dt forward in time in a potential

%Input
%psi_in: the wavefunction to be propagated. A columen vector.
%xArray: the x-array of the problem. A vector (do not matter if column or row vector) of same length as psi_in.
%timeStep: dt. A number.
%kinematicFactor: a number.
%potential: a column vector of same length as psi_in.

%Output
%psi_out: the wavefunction when propagated. Normalized so that
%         sum(abs(psi_out).^2)*dx = 1. A vector of size(psi_in).

function [psi_out] = propegateInTime(psi_in, xArray, timeStep, kinematicFactor, potential, forwardBoolean, normalizeBoolean)

        if forwardBoolean
            im = 1i;
        else
            im = -1i;
        end

        numSpatialPts=length(xArray);
        spaceStep=xArray(2)-xArray(1);
        
        kvec=1/(spaceStep*numSpatialPts)*[0:numSpatialPts/2-1 -numSpatialPts/2:-1].'; % creates frequencies from -1/(2spaceStep) to 1/(2spaceStep)
        
        kineticDiag=exp(-im*(kvec*2*pi).^2*timeStep*kinematicFactor); 
        potDiag=exp(-im*potential*timeStep/2); 
        
        psi_temp = potDiag.*psi_in;
   
        psi_fourier = fft(psi_temp); % fouriertransform wave function
        psi_fourier = kineticDiag.*psi_fourier;
        
        psi_temp = ifft(psi_fourier); % fouriertransform wave function back
        
        psi_temp = potDiag.*psi_temp;
        
        % renormalize the wave function
        if normalizeBoolean
            norm = sqrt(psi_temp'*psi_temp*spaceStep);
            psi_out = psi_temp/norm;
        else
            psi_out = psi_temp;
        end
                
end
