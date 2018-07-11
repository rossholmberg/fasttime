#define USE_RINTERNALS 1

#include <Rinternals.h>
#include <stdlib.h>
#include <stdint.h>

#define DIGIT(X) ((X) >= '0' && (X) <= '9')

/* start of each month in seconds */
static const int cml[] = { 0, 0, 2678400, 5097600, 7776000, 10368000, 13046400, 15638400,
			   18316800, 20995200, 23587200, 26265600, 28857600, 31536000 };

typedef int64_t time_int_t;

SEXP parse_ts(SEXP str, SEXP sRequiredComp) {
    SEXP res;
    double *tsv;
    int required_components = Rf_asInteger(sRequiredComp);
    int n, i;
    int warn_nas_range = 0;
    int warn_nas_components = 0;
    if (TYPEOF(str) != STRSXP) Rf_error("invalid timestamp vector");
    n = LENGTH(str);
    res = Rf_allocVector(REALSXP, n);
    tsv = REAL(res);
    for (i = 0; i < n; i++) {
	const char *c = CHAR(STRING_ELT(str, i));
	int comp = 0;
	double ts = 0.0;
	if (DIGIT(*c)) {
	    int y = 0, m = 0, d = 0, h = 0, mi = 0;
	    while (DIGIT(*c)) { y = y * 10 + (*c - '0'); c++; }
	    if (y < 100) y += 2000;
	    y -= 1970;
	    /* we only support the range of 1970-2199 to cover
	       unsigned int POSIX time without getting into more leap year mess */
	    if (y < 0 || y >= 230 ) {
		tsv[i] = NA_REAL;
	    warn_nas_range = 1;
		continue;
	    } else {
		/* adjust for all leap years prior to the current one */
	      ts += ((time_int_t)((y + 1) / 4)) * (time_int_t) 86400;
		if (y > 130) /* 2100 is an exception - not a leap year */
		    ts -= 86400;
		ts += ((time_int_t) y) * ((time_int_t) 31536000);
		comp++;
		while (*c && !DIGIT(*c)) c++;
		if (*c) {
		    while (DIGIT(*c)) { m = m * 10 + (*c - '0'); c++; }
		    if (m > 0 && m < 13) {
			ts += cml[m];
			if (m > 2 && (y & 3) == 2 && y != 130 /* 2100 again */) ts += 86400;
			comp++;
			while (*c && !DIGIT(*c)) c++;
			if (*c) {
			    while (DIGIT(*c)) { d = d * 10 + (*c - '0'); c++; }
			    if (d > 1) ts += (d - 1) * 86400;
			    comp++;
			    while (*c && !DIGIT(*c)) c++;
			    if (*c) {
				while (DIGIT(*c)) { h = h * 10 + (*c - '0'); c++; }
				ts += h * 3600;
				comp++;
				while (*c && !DIGIT(*c)) c++;
				if (*c) {
				    while (DIGIT(*c)) { mi = mi * 10 + (*c - '0'); c++; }
				    ts += mi * 60;
				    comp++;
				    while (*c && !(DIGIT(*c) || *c == '.')) c++;
				    if (*c) {
					ts += atof(c);
					comp++;
				    }
				}
			    }
			}
		    }
		}
	    }
	}
	if (comp >= required_components) {
	    tsv[i] = ts;
	} else {
	    tsv[i] = NA_REAL;
	    warn_nas_components = 1;
	}
	}
    if (warn_nas_components == 1) Rf_warning("NAs introduced. Too few components on input.");
    if (warn_nas_range == 1) Rf_warning("NAs introduced. Only years 1970-2199 are currently supported.");
    return res;
}
