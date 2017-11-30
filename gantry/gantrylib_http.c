#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <curl/curl.h>

// struct to avoid strlen calls
struct string {
	char *contents;
	size_t len;
};

// curl uses this to write page data to a buffer
size_t writer(void *ptr, size_t size, size_t nmemb, struct string *userdata) {
	
	size_t bufflen = userdata->len + size*nmemb;
	if ((userdata->contents = realloc(userdata->contents, bufflen + 1)) == NULL) {

		fprintf(stderr, "httpget: realloc() failed\n");
		exit(EXIT_FAILURE);
	}


	memcpy(userdata->contents+userdata->len, ptr, size*nmemb);
	userdata->contents[bufflen] = '\0';	
	userdata->len = bufflen;
	
	return size*nmemb;
}

/* sends a GET request to the target URL, returns the server response
   Currently leaks memory; where do we free the return pointer? */
char* httpget(char *url) {

	CURL *curl;
	CURLcode res;			// Error codes
	struct string data;	 	// Holds file contents
	struct string *s = &data;
	s->len = 0;
	s->contents = malloc(s->len+1);
	s->contents[0] = '\0';

	curl_global_init(CURL_GLOBAL_DEFAULT);
	curl = curl_easy_init();

	curl_easy_setopt(curl, CURLOPT_URL, url);
	curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, writer);
	curl_easy_setopt(curl, CURLOPT_WRITEDATA, s);

	res = curl_easy_perform(curl);
	if (res != CURLE_OK) {
		fprintf(stderr, "httpget: curl_easy_perform() failed: %s\n",
			curl_easy_strerror(res));
	}

	curl_easy_cleanup(curl);
	curl_global_cleanup();
	// free(s->contents);
	return s->contents;
}	

/*  POSTs the provided data to the target URL, returns the server response
    Currently leaks memory; where do we free the return pointer? */
char* httppost(char* target, char *tosend) {
	
	CURL *curl;
	CURLcode res;
	struct curl_slist *headers = NULL;
	headers = curl_slist_append(headers, "Content-Type: application/json");
	struct string data;
	struct string *s = &data;
	s->len = 0;
	s->contents = malloc(s->len+1);
	s->contents[0] = '\0';
	
	curl_global_init(CURL_GLOBAL_DEFAULT);
	curl = curl_easy_init();
	
	curl_easy_setopt(curl, CURLOPT_URL, target);
	curl_easy_setopt(curl, CURLOPT_POSTFIELDSIZE, -1L);	// strlen calculates size
	
	// COPYPOSTFIELDS will copy the data before sending so altering during sending is ok
	curl_easy_setopt(curl, CURLOPT_COPYPOSTFIELDS, tosend);
	// Set HEADERS for POST to Content-Type: application/json
	curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);

	// Put any responses in a dummy array instead of printing to stdout
	curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, writer);
	curl_easy_setopt(curl, CURLOPT_WRITEDATA, s);
	
	res = curl_easy_perform(curl);
	if (res != CURLE_OK) {
		fprintf(stderr, "httppost: curl_easy_perform() failed: %s\n",
			curl_easy_strerror(res));
	}

	curl_easy_cleanup(curl);
	curl_global_cleanup();

	return s->contents;
}


// Main for testing 
#ifdef BUILD_TEST
int main() {

	// GET request	
	char* page = httpget("https://requestb.in/1jhotzd1");
	printf("%s\n",page);	

	// POST
	char* req = "{ \"name\" : \"Jeff\" }";
	char* url = "https://requestb.in/1jhotzd1";
	printf("%s\n",httppost(url, req));
	
	return 0;
}
#endif
