function path=fourier(levelObject,nInt,cn)
    nbreak=floor(cn(1,2));
    gamma=[linspace(levelObject.paramstart(1),cn(1,1),nbreak) linspace(cn(1,1),levelObject.paramfinal(1),nInt-nbreak);
            linspace(levelObject.paramstart(2),levelObject.paramfinal(2),nInt)]';
    gs=zeros(nInt,2);
    supressionnr=5;
    supression=exp(linspace(1,2.5,supressionnr))';
    t=(1:nInt).*levelObject.normalDt;
    for n=2:size(cn,1)
        if mod(n,2)==0
            gs(:,1)=gs(:,1)+cn(n,1)*cos(2*pi*n*t/nInt)';
            gs(:,2)=gs(:,2)+cn(n,2)*cos(2*pi*n*t/nInt)';
        else
            gs(:,1)=gs(:,1)+cn(n,1)*sin(2*pi*n*t/nInt)';
            gs(:,2)=gs(:,2)+cn(n,2)*sin(2*pi*n*t/nInt)';
        end
    end
    gs(1:supressionnr,1)=gs(1:supressionnr,1)./flipud(supression);
    gs(end-(supressionnr-1):end,1)=gs(end-(supressionnr-1):end,1)./supression;
    gs(1:supressionnr,2)=gs(1:supressionnr,2)./flipud(supression);
    gs(end-(supressionnr-1):end,2)=gs(end-(supressionnr-1):end,2)./supression;

    path=gamma.*(1+gs);
end