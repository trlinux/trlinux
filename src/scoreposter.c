#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <curl/curl.h>

#define BUFLEN 32768
static CURL *curl;
static struct curl_slist *list = NULL;
static unsigned char buf[BUFLEN];
FILE *debug;

struct WriteThis {
   const char *readptr;
   size_t sizeleft;
};

struct MemoryStruct {
   char *memory;
   size_t size;
};

static struct MemoryStruct output;

static size_t read_callback(void *dest,size_t size,size_t nmemb, void *userp) {
   struct WriteThis *wt = (struct WriteThis *)userp;
   size_t buffer_size = size*nmemb;
   if (wt->sizeleft) {
      size_t copy_this_much = wt->sizeleft;
      if (copy_this_much > buffer_size) copy_this_much = buffer_size;
      memcpy(dest,wt->readptr,copy_this_much);
      wt->readptr += copy_this_much;
      wt->sizeleft -= copy_this_much;
      return copy_this_much;
   }
   return 0;
}

static size_t write_memory_callback(void *contents, size_t size, size_t nmemb
   ,void *userp) {
   size_t realsize = size*nmemb;
   struct MemoryStruct *mem = (struct MemoryStruct *) userp;
   mem->memory = realloc(mem->memory,mem->size+realsize+1);
   if (mem->memory == NULL) return 0;
   memcpy(&(mem->memory[mem->size]),contents,realsize);
   mem->size += realsize;
   mem->memory[mem->size] = 0;
   return realsize;
}

int initscoreposter(char *url, char *user) {
   struct curl_slist *list = NULL;
   const char *https = "https:";
   output.memory = malloc(1);
   output.size = 0;
   debug = fopen("curl.dbg","w");
   curl = curl_easy_init();
   if (curl) {
      curl_easy_setopt(curl,CURLOPT_URL,url);
      curl_easy_setopt(curl,CURLOPT_STDERR,debug);
      curl_easy_setopt(curl,CURLOPT_WRITEFUNCTION,write_memory_callback);
      curl_easy_setopt(curl,CURLOPT_WRITEDATA, (void *) &output);
      if ((strncmp(url,https,strlen(https)) == 0) & (user != NULL)) {
         curl_easy_setopt(curl,CURLOPT_USERPWD,user);
      }
      curl_easy_setopt(curl,CURLOPT_POST,1L);
      curl_easy_setopt(curl,CURLOPT_USERAGENT,"TR linux");
      list = curl_slist_append(list,"Content-Type:text/xml");
      list = curl_slist_append(list,"Expect:");
      curl_easy_setopt(curl,CURLOPT_HTTPHEADER,list);
      curl_easy_setopt(curl,CURLOPT_VERBOSE,1L);
      curl_easy_setopt(curl,CURLOPT_READFUNCTION,read_callback);
      fflush(debug);
      return 0;
   }
   return -1;
}
   
int endscoreposter() {
   curl_slist_free_all(list);
   curl_easy_cleanup(curl);
}

void poster() {
   int c;
   int i = 0;
   CURLcode res;
   struct WriteThis wt;
   while ((c = getchar()) != EOF) {
      if (i < BUFLEN) buf[i++] = (unsigned char) c;
      if (c==0) {
         output.size = 0;
         wt.readptr = buf;
         wt.sizeleft = strlen(buf);
         curl_easy_setopt(curl,CURLOPT_READDATA,&wt);
         curl_easy_setopt(curl,CURLOPT_POSTFIELDSIZE, (long) wt.sizeleft);
         res = curl_easy_perform(curl);
         if (res != CURLE_OK) {
            fprintf(debug,"curl failed: %s\n",curl_easy_strerror(res));
         }
         i = 0;
         fprintf(debug,"curl response: %s\n",output.memory);
         fflush(debug);
      }
   }
}
