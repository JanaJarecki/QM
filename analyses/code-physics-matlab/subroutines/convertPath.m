function uObject = convertPath(data,level)
    % Tilo magic
    path = str2num(data); %#ok<ST2NM>
    path = [path(1:size(path,2)/2) ; path(size(path,2)/2+1:end)];
    path = [path(:,2:end)];
    
    path=correctConstrainedMouseMovementInGuess(path,level);
    x0 = 0.6;
    path(2,:) = path(2,:) - ( -level.Vlat.*exp(-2.0.*(path(1,:) - x0).*(path(1,:) - x0)./(level.waist*level.waist)));
    path=path';
    
    function guess = correctConstrainedMouseMovementInGuess(guess,Params)
        guess(1,guess(1,:)<Params.xMin) = Params.xMin;
        guess(1,guess(1,:)>Params.xMax) = Params.xMax;
        guess(2,guess(2,:)<Params.yMin) = Params.yMin;
        guess(2,guess(2,:)>Params.yMax) = Params.yMax;
    end
    
uObject = struct('path', path, 'nInt', size(path,1)) ;
end