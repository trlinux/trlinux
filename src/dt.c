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
