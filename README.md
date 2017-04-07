The source code for pidgeon.club.

Postgresql stuff
----------------

Create a user with the same name under which `pidgeonclub` will be executed with:
```bash
createuser --interactive
createdb myDatabasename
```

Then add database authentication info to `siteconfig.cfg`.

To compile and execute:

```bash
stack build
stack exec pidgeonclub
```


