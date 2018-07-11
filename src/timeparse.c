#define USE_RINTERNALS 1

#include <Rinternals.h>
#include <stdlib.h>
#include <stdint.h>

#define DIGIT(X) ((X) >= '0' && (X) <= '9')

SEXP parse_time(SEXP str, SEXP sRequiredComp) {
    SEXP res;
    double *tsv;
    int required_components = Rf_asInteger(sRequiredComp);
    int n, i, comp;
    int warn_mstrunc = 0;
    double ts;
    if (TYPEOF(str) != STRSXP) Rf_error("invalid times vector");
    n = LENGTH(str);
    res = Rf_allocVector(REALSXP, n);
    tsv = REAL(res);
    for (i = 0; i < n; i++) {
    	const char *c = CHAR(STRING_ELT(str, i));
        comp = 0;
        ts = 0;
    	if (DIGIT(*c)) {
    	    double hr = 0, min = 0, sec = 0, ms = 0;
    	    while (DIGIT(*c)) {
    	        hr = hr * 10 + (*c - '0');
    	        c++;
    	    }
    		ts += hr / 24;
    		comp++;
    		
    		// skip to the minutes component
    		while (*c && !DIGIT(*c)) c++;
    		
    		if (*c) {
    		    while (DIGIT(*c)) {
    		        min = min * 10 + (*c - '0');
    		        c++;
    		    }
    		    ts += min / 1440;
    			comp++;
    			
    			// skip to the seconds component
    			while (*c && !DIGIT(*c)) c++;
    			
    			if (*c) {
    			    while (DIGIT(*c)) {
    			        sec = sec * 10 + (*c - '0');
    			        c++;
    			    }
    			    ts += sec / 86400;
    			    comp++;
    			    
    			    while (*c && !DIGIT(*c)) c++;
    			    if (*c) {
    			        ms = (*c - '0') * 100;
    			        c++;
    			        if (*c) {
    			            ms = ms + (*c - '0') * 10;
    			        }
    			        c++;
    			        if (*c) {
    			            ms = ms + (*c - '0');
    			        }
    			        c++;
    			        if (*c) {
    			            warn_mstrunc = 1;
    			        }
    			        ts += ms / 86400000;
    			        comp++;
    			    }
    			}
    		}
	    }
    	tsv[i] = (comp >= required_components) ? ts : NA_REAL;
	}
    if (warn_mstrunc == 1) {
        Rf_warning("Time values truncated to ms. Some precision may be lost.");
    }
    return res;
}
