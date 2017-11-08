#include <stdio.h>
#include <curl/curl.h>

int main(void) {

	CURL *curl;
	CURLcode res;	// Error codes

	curl_global_init(CURL_GLOBAL_DEFAULT);
	curl = curl_easy_init();
	curl_easy_setopt(curl, CURLOPT_URL, "http://google.com");

	res = curl_easy_perform(curl);
	if (res != CURLE_OK) {
		fprintf(stderr, "curl_easy_perform() failed: %s\n",
			curl_easy_strerror(res));
	}

	curl_easy_cleanup(curl);

	return 0;
}
