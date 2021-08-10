function dataout = krilltrackload(fname)
% Load in a .mat file that's the output of that kril track imaging software
% Pull the x,y,z coordinates of each krill track and arrange them
% into a 3-column matrix. Add a fourth column that just numbers the track.
%
% Also NaNs points where pixels have been clipped to image edges.
%
% For the y-coordinate, use the average from the two images.
%
% Conversion is done using tank positions esimated from images and a 
% linear transformation.
%
% NRR 2021


load(fname)
xypts=udExport.data.xypts;
XM=[];
YM=[];
ZM=[];
KT=[]; % Krill track

for j=1:size(xypts,2)/4
    % Pull tracks, pixel values
    y1=xypts(:,j*4-3);
    z1=xypts(:,j*4-2);
    x1=xypts(:,j*4-1);
    y2=xypts(:,j*4);
    % Remove pixels clipped to image edges (outside of tank)
    I=find(y1==0);y1(I)=nan;
    I=find(y2==0);y2(I)=nan;
    I=find(x1==0);x1(I)=nan;
    I=find(z1==0);z1(I)=nan;
    % Convert from pixels to cm
    ycm1=y1./1900.*50;
    ycm2=y2./1080.*50;
    ycm=(ycm1+ycm2)./2; % Average two y tracks
    xcm=(x1-530)./(1390-530).*35;
    zcm=-(z1-320)./(1100-320).*25;
    % Make column for krill track numbering
    kt=x1;kt(:)=j;
    % Add to columns (and convert to meters)
    XM=[XM;xcm./100];
    YM=[YM;ycm./100];
    ZM=[ZM;zcm./100];
    KT=[KT;kt];
end

dataout=[XM,YM,ZM,KT];
