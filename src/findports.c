//
//Copyright Larry Tyree, N6TR, 2011,2012,2013,2014,2015.
//
//This file is part of TR log for linux.
//
//TR log for linux is free software: you can redistribute it and/or
//modify it under the terms of the GNU General Public License as
//published by the Free Software Foundation, either version 2 of the
//License, or (at your option) any later version.
//
//TR log for linux is distributed in the hope that it will be useful,
//but WITHOUT ANY WARRANTY; without even the implied warranty of
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//GNU General Public License for more details.
//
//You should have received a copy of the GNU General
//    Public License along with TR log for linux.  If not, see
//<http://www.gnu.org/licenses/>.
//

#include <stdlib.h>
#include <dirent.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <termios.h>
#include <sys/ioctl.h>
#include <linux/serial.h>
#include <libgen.h>
#include <ieee1284.h>

#define BUFSIZE 1024
#define DEVSIZE 256

void catstr(char *dest, char *src, int ndest) {
   if (strlen(dest)+strlen(src) < ndest-1) {
      strcat(dest,src);
   } else {
      strncat(dest,src,ndest-1);
      dest[ndest-1] = 0;
   }
}

//load names and descriptions of up to n serial ports into the
//buffer array, bufsize is max size of each string, n is the maxium
//number to return. Return value is total number found, or -1 if there
//is an error.
int findserial(char **buf, int bufsize, int n) {

   int i,j,k,nf,nd;
   char c;
   FILE *f1,*f2;
   struct dirent **files;
   const char *ttydir = "/sys/class/tty/";
   char devname[BUFSIZE],tempbuf1[BUFSIZE],tempbuf2[BUFSIZE],tempbuf3[BUFSIZE];
   char manufacturer[DEVSIZE],product[DEVSIZE],dev[DEVSIZE];
   char *path1,*base,*dir;
   struct serial_struct serinfo;
   struct stat st;
   char *bname;
   nd = 0;
   nf = scandir(ttydir, &files, NULL, NULL);
   if (nf == -1) return -1;
   for (i=0;i<nf;i++) {
      if (strcmp(files[i]->d_name,".") && strcmp(files[i]->d_name,"..")) {
         if (strlen(files[i]->d_name)+strlen(ttydir)+ strlen("/device")
            + 1 < BUFSIZE)  {
            memset(product,0,DEVSIZE);
            memset(manufacturer,0,DEVSIZE);
            memset(devname,0,BUFSIZE);
            strcpy(devname,ttydir);
            strcat(devname,files[i]->d_name);
            strcat(devname,"/device");
            if (lstat(devname,&st) == 0 && S_ISLNK(st.st_mode)) {
               if ((path1 = realpath(devname,NULL)) != 0) {
                  if (strcmp(basename(path1),"serial8250") != 0) {
                      strcpy(devname,"/dev/");
                      strcat(devname,files[i]->d_name);
                      if (nd < n)  {
                      buf[nd][0] = 0;
                      catstr(buf[nd],devname,bufsize);
                      if (strlen(path1) <= BUFSIZE) {
                          strcpy(tempbuf3,path1);
                          for (j=0;j<100;j++) { //should only take 1 or 2
                             strcpy(tempbuf1,tempbuf3);
                             strcpy(tempbuf2,tempbuf3);
                             dir = dirname(tempbuf1);
                             base = basename(tempbuf2);
                             if ((strcmp(base,"devices") == 0) ||
                                 (strcmp(base,".") == 0) ||
                                 (strcmp(base,"/") == 0) ||
                                 (strcmp(base,"..") == 0)) break;
                             strcpy(tempbuf3,dir);
                             strcat(tempbuf3,"/");
                             strcat(tempbuf3,"product");
                             if ((f1 = fopen(tempbuf3,"r")) != 0) {
                                 k = 0;
                                 while ((c = (char) fgetc(f1)) != EOF) {
                                    if (c == '\n') break;
                                    product[k++] = c;
                                    if (k > DEVSIZE-2) break;
                                 }
                                product[k] = 0;
                                fclose(f1);
                                catstr(buf[nd],"  ",bufsize);
                                catstr(buf[nd],product,bufsize);
                                strcpy(tempbuf3,dir);
                                strcat(tempbuf3,"/");
                                strcat(tempbuf3,"manufacturer");
                                if ((f2 = fopen(tempbuf3,"r")) != 0) {
                                   k = 0;
                                   while ((c = (char) fgetc(f2)) != EOF) {
                                      if (c == '\n') break;
                                      manufacturer[k++] = c;
                                      if (k > DEVSIZE-2) break;
                                    }
                                    manufacturer[k] = 0;
                                    fclose(f1);
                                    catstr(buf[nd]," ",bufsize);
                                    catstr(buf[nd],manufacturer,bufsize);
                                }
                                break;
                             }
                             strcpy(tempbuf3,dir);
                         }
                      }
                      free(path1);
                      }
                      nd++;
                  }
               }
            }
         }
      }
   }
   return nd;
}

int findparallel(char **buf, int bufsize, int n) {
   int i,j,np;
   struct parport_list ppl;
   struct parport *pp;
   np = -1;
   if (ieee1284_find_ports(&ppl,0) == E1284_OK) {
      np = ppl.portc;
      j=0;
      for (i=0;i<np;i++) {
         if (i >= n) break;
         if (strcmp("/dev/port",ppl.portv[i]->filename) != 0) {
            if ((strlen(ppl.portv[i]->filename) < bufsize-1)) {
               strcpy(buf[j],ppl.portv[i]->filename);
            } else {
               strncpy(buf[i],ppl.portv[i]->filename,bufsize-1);
               buf[j][bufsize-1] = 0;
            }
            j++;
         }
      }
      ieee1284_free_ports(&ppl);
   }
   return j;
}

/*
#define PNUM 10
int main(int argc, char **argv) {
   char **buf;
   int i,n;
   buf = (char**) malloc(PNUM*sizeof(char*));
   for (i=0;i<PNUM;i++) {
      buf[i] = (char*) malloc(80*sizeof(char));
   }
   n = findserial(buf,80,PNUM);
   if (n > PNUM) {
      printf("array too small %d %d\n",n,PNUM);
      n = PNUM;
   }
   for (i=0;i<n;i++) printf("%s\n",buf[i]);
}
*/
