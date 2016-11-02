# Script that run the Prolog server
echo "[$(date)] Launching prolog server at port 8080 on localhost"
swipl -f core/swish.pl -g "main:server(8080)."
