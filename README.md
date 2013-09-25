regex_keyspace
==============

Some code for working with the keyspaces of a subset of regular expressions.

Requires scala 2.9 or higher (`sudo apt-get install scala`).

Compile with:
```bash
make dist
```
    
Run examples:
```bash
scala regex_keyspace.jar "(A|B)"       # count number of possibilities
scala regex_keyspace.jar "(A|B)" count # same, but explicit
scala regex_keyspace.jar "(A|B)" 4     # produce 4 random matches
scala regex_keyspace.jar "(A|B)" all   # produce all possible matches
```
