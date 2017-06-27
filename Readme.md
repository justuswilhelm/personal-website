# Portfolio
This is where I put my portfolio and blog

## Quickstart
```
virtualenv env -ppython3
pip install -r requirements.txt
source env/bin/activate
make test
```

## How to configure server
```
# Download caddy
curl https://caddyserver.com/download/linux/amd64?plugins=dns > caddy.tar.xz
# Untar caddy
tar xvfz caddy.tar.xz -C caddy
# Execute on dev machine
scp Caddyfile root@justus.pw:~/Caddyfile
make
```

## Requirements
- Python 3 with pip
- GNU Make
- Aspell
