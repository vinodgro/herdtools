/*********************************************************************/
/*                          Litmus                                   */
/*                                                                   */
/*        Luc Maranget, INRIA Paris-Rocquencourt, France.            */
/*        Susmit Sarkar, University of Cambridge, UK.                */
/*                                                                   */
/*  Copyright 2010 Institut National de Recherche en Informatique et */
/*  en Automatique and the authors. All rights reserved.             */
/*  This file is distributed  under the terms of the Lesser GNU      */
/*  General Public License.                                          */
/*********************************************************************/

#ifndef _AFFINITY_H
#define _AFFINITY_H 1

#include "utils.h"

cpus_t *read_affinity(void) ;

cpus_t *read_force_affinity(int n_avail, int verbose) ;

void write_affinity(cpus_t *p) ;

void write_one_affinity(int cpu) ;

void force_one_affinity(int cpu, int sz, int verbose, char *name) ;

#endif
