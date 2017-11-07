# README for the tests directory.

## In this directory

- list_pass_tests.txt : contains the list of tests that should pass (e.g. we have implemented the function, the tests is correctly formatted, and the output is listed at the bottom)
- list_fail_tests.txt : contains the list of tests that should fail correctly (e.g. these are correctly implemented functions that we have written a test that should intentionally fail)
- generate_ref.sh: Generates correct output scripts (.err and .out), see below
- test_*.gty : These are tests that should pass
- fail_*.gty : These are tests that should fail



### Format for tests should follow the format:

```
// Pass/Fail What it does

int main(){
	//This is the code
	print(1);
	return 0;
}

// ****TEST****
// 1
```

### Generate the correct output scripts by running.
 
```
chmod 777 generate_ref.sh
`./generate_ref` 
```

### Run Test Suite

- test_all.sh is located in the root gantry directory
- make executable and run (`./test_all.sh`)
- This will create .out, .err files in the root gantry directory that get compared with the files we have here
- The test_all script will also generate some summary files
- When done, remove the generated files

### Automatic Test Integration

- We might do this... more details to come
