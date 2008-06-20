/*---------------------------------------------------------------------------*/
/* Version 11-February-1999                               File: redsystem_.c */
/*---------------------------------------------------------------------------*/
/* Copyright N. Cardiel & J. Gorgas, Departamento de Astrofisica             */
/* Universidad Complutense de Madrid, 28040-Madrid, Spain                    */
/* E-mail: ncl@astrax.fis.ucm.es or fjg@astrax.fis.ucm.es                    */
/*---------------------------------------------------------------------------*/
/* This routine is free software; you can redistribute it and/or modify it   */
/* under the terms of the GNU General Public License as published by the Free*/
/* Software Foundation; either version 2 of the License, or (at your option) */
/* any later version. See the file gnu-public-license.txt for details.       */
/*---------------------------------------------------------------------------*/
/*Comment                                                                    */
/*                                                                           */
/* int redsystem_(char *comando)                                             */
/*                                                                           */
/* input: comando                                                            */
/* output: redsystem_ (function)                                             */
/*                                                                           */
/* Executes a command specified in string by calling /bin/sh -c string       */
/*                                                                           */
/* char *comando -> command to be executed (do not forget to add the         */
/*                  suffix \0)                                               */
/*Comment                                                                    */
/*---------------------------------------------------------------------------*/
#include <stdlib.h>

int redsystem_(const char *comando)
{
  int retorno;

  retorno=system(comando);
  return(retorno);
}
