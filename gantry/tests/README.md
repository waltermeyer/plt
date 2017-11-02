# README for the tests directory.

### Format for tests should follow the format:

```
// Pass/Fail What it does

int main(){
	//This is the code
	print(1);
	return 0;
}

// ****TEST****
1
```

### Generate the correct output scripts by running.
 
```
chmod 777 generate_ref.sh
`./generate_ref` 
```

