#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix kde2stampxx(NumericMatrix map, NumericMatrix counts, int hmax, double s, double sdmin, double sdmax, double d, int nkernels,
                    NumericVector kern, NumericVector kernindex, NumericVector kernlength) {

  int nx = map(_,1).size();
  int ny = map(1,_).size();
  NumericMatrix out(nx+2*hmax,ny+2*hmax);

  for (int ix = 0; ix<nx; ix++) {
    for (int iy = 0; iy<ny; iy++) {
      if (map(ix,iy)>0) {
        double sdpixel = 15*s/sqrt(counts(ix,iy));
        if (sdpixel<sdmin) sdpixel=sdmin;
        if (sdpixel>sdmax) sdpixel=sdmax;
        int i = round(sdpixel/d);
        if (i>=nkernels) i = nkernels-1;
        int h = (sqrt(kernlength[i])-1)/2;
        int k = kernindex[i]-1;
        for (int rx = ix+hmax-h; rx<=ix+hmax+h; rx++) {
          for (int ry = iy+hmax-h; ry<=iy+hmax+h; ry++) {
            out(rx,ry) = out(rx,ry)+map(ix,iy)*kern[k++];
          }
        }
      }
    }
  }

  return out;
}
