#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <curl/curl.h>

static size_t write_data(void *ptr, size_t size, size_t nmemb, void *stream) {
   size_t written = fwrite(ptr, size, nmemb, (FILE *)stream);
   return written;
}

int urldownload(char *url, char *filename) {
   CURL *curl;
   FILE *f;
   CURLcode res;
   int retcode = 0;
   curl_global_init(CURL_GLOBAL_ALL);
   curl = curl_easy_init();
   curl_easy_setopt(curl,CURLOPT_URL,url);
   curl_easy_setopt(curl,CURLOPT_FOLLOWLOCATION,1L);
   curl_easy_setopt(curl,CURLOPT_WRITEFUNCTION,write_data);
   f = fopen(filename,"wb");
   if (f) {
      curl_easy_setopt(curl,CURLOPT_WRITEDATA,f);
      res = curl_easy_perform(curl);
      fclose(f);
      if (res != CURLE_OK) retcode = -2;
   } else {
      retcode = -1;
   }
   curl_easy_cleanup(curl);
   return retcode;
}
