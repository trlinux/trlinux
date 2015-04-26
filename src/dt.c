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

#include <time.h>
#include <stdio.h>

time_t offset = 0L;

void gettimec(short *hour, short *min, short *sec, short *sec100) {
   time_t current;
   struct tm info;
   time(&current);
   current += offset;
   gmtime_r(&current,&info);
   *sec100 = 0;
   *sec = info.tm_sec;
   *min = info.tm_min;
   *hour = info.tm_hour;
}

void getdatec(short *year, short *month, short *mday, short *wday) {
   time_t current;
   struct tm info;
   time(&current);
   current += offset;
   gmtime_r(&current,&info);
   *year = info.tm_year+1900;
   *month = info.tm_mon+1;
   *mday = info.tm_mday;
   *wday = info.tm_wday;
}

void setdatec(short year, short month, short day) {
   time_t current,new,stupid;
   struct tm info;
   time(&current);
   current += offset;
   gmtime_r(&current,&info);
   stupid = mktime(&info); //mktime info uses local time
   info.tm_year = year-1900;
   info.tm_mon = month-1;
   info.tm_mday = day;           //info holds new utc date
   new = mktime(&info);
   offset += new-stupid;
}

void settimec(short hour, short min, short sec, short sec100) {
   time_t current,new,stupid;
   struct tm info;
   time(&current);
   current += offset;
   gmtime_r(&current,&info);
   stupid = mktime(&info);
   info.tm_hour = hour;
   info.tm_min = min;
   info.tm_sec= sec;
   new = mktime(&info);
   offset += new-stupid;
}
