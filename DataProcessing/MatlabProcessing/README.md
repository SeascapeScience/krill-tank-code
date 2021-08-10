## Here is a quick shortcut to processing a new .mat file with the matlab code. 

You need to have the included .m files above in your directory, as well as the data (.mat) that you're trying to process. Just replace the ```filename``` below with the name of the file to process. This is matlab code btw.

```filename='20191205_view10_10.5ks(3)_180ts(all)_dvProject.mat';```
```csvwrite_with_headers([filename,'.csv'],full(krilltrackload(filename)),{'x_meters','y_meters','z_meters','track'});```
