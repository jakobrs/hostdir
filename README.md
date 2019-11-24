# hostdir (temporary name)

Hosts a folder using http

#### Command usage (from `--help`):

```
hostdir v0.1.0.0

  -p    --port=                  What port to listen on (default 8080)
  -h    --host=                  What host to listen on (default 127.0.0.1)
  -H    --any-host               Use host '*'
  -r    --root=                  Root folder            (default .)
  -c    --convert                Convert file formats
  -C    --no-convert             Don't convert file formats
  -B    --no-buffer              Don't buffer output
  -l    --buffer-line, --buffer  Use line buffering (default)
  -b[]  --buffer-block[=]        Use block buffering
        --404=                   404 page               (default 404.html)
        --help                   Show command usage
  -V    --version, --ver         Show version

Valid arguments to --host:
- *   Any host, both IPv4 and IPv6
- *4  Any host, prefer IPv4
- *6  Any host, prefer IPv6
- !4  Any host, only IPv4
- !6  Any host, only IPv6
Any other value is treated as a normal hostname.
```
