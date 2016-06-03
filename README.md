# zip-jar-haskell
This is an attempt to use the Haskell zip library to zip together classfiles into a jar archive.

## Usage
This program includes a simple executable for testing out creating and adding things into a jar or zip archive with Haskell.

You can compile the executable with the following command.

```
stack build
```

Once you have compiled the executable, you can run it like in the following example. The first arguement is the relative path of the jar archive. The second arguement is the location of the test file within the jar archive. The third arguement is the contents of the test file.

```
./zip-jar-exe "test.jar" "src/Main.txt" "Hello World"
```
