#include <stdio.h>
#include <curl/curl.h>

void httpget(char *url) {

	CURL *curl;
	CURLcode res;		// Error codes
	char outbuf[4096];	// Holds file contents

	curl_global_init(CURL_GLOBAL_DEFAULT);
	curl = curl_easy_init();
	curl_easy_setopt(curl, CURLOPT_URL, url);

	res = curl_easy_perform(curl);
	if (res != CURLE_OK) {
		fprintf(stderr, "httpget: curl_easy_perform() failed: %s\n",
			curl_easy_strerror(res));
	}
	
	curl_easy_cleanup(curl);
}	


int main() {


	httpget("https://curl.haxx.se/lubcurl/c/");
	return 0;
}



