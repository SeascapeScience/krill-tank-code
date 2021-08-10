d = dir('../MatlabRawFiles/*.mat');
for j=1:length(d)
    datain = krilltrackload(['../MatlabRawFiles/',d(j).name]);
    outfile = [d(j).name(1:end-4),'.csv'];
    datawrite = full(datain);
    csvwrite_with_headers(outfile,datawrite,{'x_meters','y_meters','z_meters','track'});
end

    