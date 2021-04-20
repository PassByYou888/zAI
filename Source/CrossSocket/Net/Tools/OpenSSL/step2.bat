@echo off

rem �ڶ��� ���� CA ֤��

cd "%~DP0"
mkdir keys

set OPENSSL_CONF=%~DP0openssl.cfg

rem ���� CA ˽Կ
openssl genrsa -out keys\ca.key 2048
rem X.509 Certificate Signing Request (CSR) Management.
openssl req -new -key keys\ca.key -out keys\ca.csr -subj /C=CN/ST=BeiJing/L=BeiJing/O=DEMO/CN=www.ssldemo.com
rem X.509 Certificate Data Management.
openssl x509 -days 36500 -req -in keys\ca.csr -signkey keys\ca.key -out keys\ca.crt -extfile v3.ext

pause