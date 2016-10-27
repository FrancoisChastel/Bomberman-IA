# Script that run the Prolog server
cd /var/www;
echo "[$(date)] Launching prolog server at port 8080 on localhost"
swipl -f swish.pl -g "main:server(8080). init([])." 
