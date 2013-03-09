#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>

#include <gsl/gsl_errno.h>
#include <gsl/gsl_interp.h>

#define LOG_ERROR stderr

/* Persistent data structure (threaded state) representing the cubic spline
 * interpolant, static storage for associated samples, and helper structures */
struct interpolant
{
	int n;
	double *x, *y;
	gsl_interp_accel *acc;
	gsl_interp *interp;
};
typedef struct interpolant interp_st;

interp_st *
gsl_interp_init_wrapper(double *x, double *y, int n)
{
	interp_st *new = malloc(sizeof(interp_st));
	if (new == NULL) {
#ifdef VERBOSE_ERRORS
		fprintf(LOG_ERROR, "[%s] Error: cannot allocate storage for struct - %s", __func__, strerror(errno));
#endif
		return NULL;
	}
	new->n = n;
	new->x = malloc((size_t)(n) * sizeof(*new->x));
	if (new->x == NULL) {
#ifdef VERBOSE_ERRORS
		fprintf(LOG_ERROR, "[%s] Error: cannot allocate storage for x - %s", __func__, strerror(errno));
#endif
		free(new);
		return NULL;
	}
	new->y = malloc((size_t)(n) * sizeof(*new->y));
	if (new->y == NULL) {
#ifdef VERBOSE_ERRORS
		fprintf(LOG_ERROR, "[%s] Error: cannot allocate storage for y - %s", __func__, strerror(errno));
#endif
		free(new->x);
		free(new);
		return NULL;
	}
	/* copy in data */
	memcpy(new->x, x, (size_t)(n) * sizeof(*new->x));
	memcpy(new->y, y, (size_t)(n) * sizeof(*new->y));
	/* gsl init */
	new->acc = gsl_interp_accel_alloc();
	new->interp = gsl_interp_alloc(gsl_interp_cspline, new->n);
	gsl_interp_init(new->interp, new->x, new->y, new->n);
	return new;
}

void
gsl_interp_free_wrapper(interp_st *p)
{
	gsl_interp_free(p->interp);
	gsl_interp_accel_free(p->acc);
	free(p->x);
	free(p->y);
	free(p);
}

double
gsl_interp_eval_wrapper(interp_st *p, double x)
{
	return gsl_interp_eval(p->interp, p->x, p->y, x, p->acc);
}

double
gsl_interp_eval_deriv_wrapper(interp_st *p, double x)
{
	return gsl_interp_eval_deriv(p->interp, p->x, p->y, x, p->acc);
}

double
gsl_interp_eval_deriv2_wrapper(interp_st *p, double x)
{
	return gsl_interp_eval_deriv2(p->interp, p->x, p->y, x, p->acc);
}

double
gsl_interp_eval_integ_wrapper(interp_st *p, double x0, double x1)
{
	return gsl_interp_eval_integ(p->interp, p->x, p->y, x0, x1, p->acc);
}
