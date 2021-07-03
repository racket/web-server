remake:
	rm -f private-key.pem server-cert.pem
	openssl req -x509 -newkey rsa:4096 -keyout private-key.pem -out server-cert.pem -nodes -days 365
