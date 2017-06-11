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
tar xvfz -C caddy caddy.tar.xz
# Execute on dev machine
scp Caddyfile root@67.207.75.149:~/Caddyfile
scp -r justus.pw root@67.207.75.149:/www/
```

## Requirements
- Python 3 with pip
- GNU Make
- Aspell
