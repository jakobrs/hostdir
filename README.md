# hostdir (temporary name)

Hosts a folder using http

#### Command usage (from `--help`):

```
hostdir v0.1.0.0

  -p   --port=           What port to listen on
  -h   --host=           What host to listen on
       --path=           Set path
  -r   --root=           Root folder
       --help            Show command usage
       --404=            404 page
  -V   --version, --ver  Show version

Valid arguments to --host:
- *   Any host, both IPv4 and IPv6
- *4  Any host, prefer IPv4
- *6  Any host, prefer IPv6
- !4  Any host, only IPv4
- !6  Any host, only IPv6
Any other value is treated as a normal hostname.
```
