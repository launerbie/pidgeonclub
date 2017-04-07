The source code for pidgeon.club.

siteconfig.cfg
--------------
Set user to the user under which pidgeonclub will be executed
and a database password for the user in `siteconfig.cfg`.


Postgresql stuff
----------------

Create a role with the same name under which `pidgeonclub` will be executed with:
```bash
createuser --interactive
createdb myDatabasename
```
Then connect to the database:
```bash
psql -d myDatabasename
```
and set a password with postgres command `\password`.

Compile pidgeonclub
-------------------
To compile and execute:

```bash
stack build
stack exec pidgeonclub
```


