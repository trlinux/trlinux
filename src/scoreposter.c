#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <curl/curl.h>
#include <assert.h>
#include <strings.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <stdbool.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <ctype.h>
#include <errno.h>

#define BUFLEN 32768
static CURL *curl = NULL;
static struct curl_slist *list = NULL;
static unsigned char buf[BUFLEN];
static FILE *debug = NULL;
static int fd = -1;
static struct addrinfo *rp,*result=NULL;

typedef struct url_parser_url {
   char *protocol;
   char *host;
   int port;
   char *path;
   char *query_string;
   int host_exists;
   char *host_ip;
} url_parser_url_t;

void free_parsed_url(url_parser_url_t *url_parsed) {
   if (url_parsed->protocol) free(url_parsed->protocol);
   if (url_parsed->host) free(url_parsed->host);
   if (url_parsed->path) free(url_parsed->path);
   if (url_parsed->query_string) free(url_parsed->query_string);
   free(url_parsed);
}

int parse_url(char *url, bool verify_host, url_parser_url_t *parsed_url) {
   char *local_url = (char *) malloc(sizeof(char) * (strlen(url) + 1));
   char *token;
   char *token_host;
   char *host_port;
   char *host_ip;
   char *token_ptr;
   char *host_token_ptr;
   char *path = NULL;
// Copy our string
   strcpy(local_url, url);
   token = strtok_r(local_url, ":", &token_ptr);
   parsed_url->protocol = (char *) malloc(sizeof(char) * strlen(token) + 1);
   strcpy(parsed_url->protocol, token);
// Host:Port
   token = strtok_r(NULL, "/", &token_ptr);
   if (token) {
      host_port = (char *) malloc(sizeof(char) * (strlen(token) + 1));
      strcpy(host_port, token);
   } else {
      host_port = (char *) malloc(sizeof(char) * 1);
      strcpy(host_port, "");
   }
   token_host = strtok_r(host_port, ":", &host_token_ptr);
   parsed_url->host_ip = NULL;
   if (token_host) {
      parsed_url->host = (char *) malloc(sizeof(char) * strlen(token_host) + 1);
      strcpy(parsed_url->host, token_host);
      if (verify_host) {
         struct hostent *host;
         host = gethostbyname(parsed_url->host);
         if (host != NULL) {
            parsed_url->host_ip = inet_ntoa(* (struct in_addr *) host->h_addr);
            parsed_url->host_exists = 1;
         } else {
            parsed_url->host_exists = 0;
         }
      } else {
         parsed_url->host_exists = -1;
      }
   } else {
      parsed_url->host_exists = -1;
      parsed_url->host = NULL;
   }
// Port
   token_host = strtok_r(NULL, ":", &host_token_ptr);
   if (token_host)
      parsed_url->port = atoi(token_host);
   else
      parsed_url->port = 0;
   token_host = strtok_r(NULL, ":", &host_token_ptr);
   assert(token_host == NULL);
   token = strtok_r(NULL, "?", &token_ptr);
   parsed_url->path = NULL;
   if (token) {
      path = (char *) realloc(path, sizeof(char) * (strlen(token) + 2));
      strcpy(path, "/");
      strcat(path, token);
      parsed_url->path = (char *) malloc(sizeof(char) * strlen(path) + 1);
      strncpy(parsed_url->path, path, strlen(path));
      free(path);
   } else {
      parsed_url->path = (char *) malloc(sizeof(char) * 2);
      strcpy(parsed_url->path, "/");
   }
   token = strtok_r(NULL, "?", &token_ptr);
   if (token) {
      parsed_url->query_string = (char*) malloc(sizeof(char)*(strlen(token)+1));
      strncpy(parsed_url->query_string, token, strlen(token));
   } else {
      parsed_url->query_string = NULL;
   }
   token = strtok_r(NULL, "?", &token_ptr);
   assert(token == NULL);
   free(local_url);
   free(host_port);
   return 0;
}

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

int initscoreposter(char *url, char *user, int debugin) {
   struct curl_slist *list = NULL;
   url_parser_url_t *parsed_url;
   const char *https = "https:";
   char portstr[64];
   struct addrinfo hints;
   int i,rc;
   int bcast = 1;
   output.memory = malloc(1);
   output.size = 0;
   if (debugin) debug = fopen("curl.dbg","w");
   parsed_url=(url_parser_url_t*) malloc(sizeof(url_parser_url_t));
   if (parse_url(url,0,parsed_url) != 0) {
      if (debug != NULL) fprintf(debug,"URL parse failed\n");
      free(parsed_url);
      return -1;
   }
   for(i=0;i<parsed_url->protocol[i];i++) {
      parsed_url->protocol[i]=tolower(parsed_url->protocol[i]);
   }
   if (strcmp("udp",parsed_url->protocol) == 0) {
      if (debug != NULL) fprintf(debug,"Using UDP\n");
      memset(&hints,'\0',sizeof(hints));
      hints.ai_family = AF_UNSPEC;
      hints.ai_socktype = SOCK_DGRAM;
      hints.ai_protocol = 0;
      hints.ai_flags = AI_ADDRCONFIG;
      snprintf(portstr,strlen(portstr),"%d",parsed_url->port);
      rc = getaddrinfo(parsed_url->host,portstr,&hints,&result);
      free_parsed_url(parsed_url);
      if (rc != 0) {
         if (debug != NULL) fprintf(debug,"Could not resolve address\n");
         return -1;
      }
      for (rp=result;rp != NULL; rp=rp->ai_next) {
         fd=socket(rp->ai_family,rp->ai_socktype,rp->ai_protocol);
         if (fd == -1) continue;
         if (setsockopt(fd,SOL_SOCKET,SO_BROADCAST,&bcast,sizeof(bcast)) == 1) {
            if (debug != NULL) {
               fprintf(debug,"Could not set socket to broadcast\n");
            }
         }
         if (connect(fd,rp->ai_addr,rp->ai_addrlen) != -1) break;
         close(fd);
      }
      if (rp == NULL) {
         if (debug != NULL) {
            fprintf(debug,"Could not connect - %s\n",strerror(errno));
         }
         return -1;
      }

      if (debug != NULL) fflush(debug);
      return 0;
   } else {
      if (debug != 0) fprintf(debug,"Using curl\n");
      free_parsed_url(parsed_url);
      curl = curl_easy_init();
   }
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
      if (debug != NULL) fflush(debug);
      return 0;
   }
   return -1;
}
   
int endscoreposter() {
   if (curl) {
      curl_slist_free_all(list);
      curl_easy_cleanup(curl);
   }
   if (result != NULL) {
      freeaddrinfo(result);
      result = NULL;
   }
}

void poster() {
   int c;
   int rc,i = 0;
   CURLcode res;
   struct WriteThis wt;
   while ((c = getchar()) != EOF) {
      if (i < BUFLEN) buf[i] = (unsigned char) c;
      if ((i != 0) || (c == 0x3c)) i++; //drop new lines before xml start
      if (c==0) {
         if (debug != NULL) {
            fprintf(debug,"Posting the following XML score data:\n");
            fprintf(debug,"%s",buf);
            fprintf(debug,"\n");
         }
         i = 0;
         if (fd >= 0) {
            rc=sendto(fd,buf,strlen(buf),0,rp->ai_addr,rp->ai_addrlen);
            if (debug != NULL) {
               if (rc == -1) {
                  int err = errno;
                  fprintf(debug,"Sendto failed:%d(%s)\n",err,strerror(err));
                  fflush(debug);
               } else {
                  fprintf(debug,"Sendto succeeded\n");
                  fflush(debug);
               }
            }
         }
         if (curl) {
            output.size = 0;
            wt.readptr = buf;
            wt.sizeleft = strlen(buf);
            curl_easy_setopt(curl,CURLOPT_READDATA,&wt);
            curl_easy_setopt(curl,CURLOPT_POSTFIELDSIZE, (long) wt.sizeleft);
            res = curl_easy_perform(curl);
            if (res != CURLE_OK) {
               if (debug != NULL) {
                  fprintf(debug,"curl failed: %s\n",curl_easy_strerror(res));
               }
            }
            if (debug != NULL) {
               fprintf(debug,"curl response: %s\n",output.memory);
               fflush(debug);
            }
         }
      }
   }
}
