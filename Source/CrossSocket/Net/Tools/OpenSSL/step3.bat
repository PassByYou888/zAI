@echo off

rem ������ ���ɷ�������֤��Ϳͻ���֤��

cd "%~DP0"
mkdir keys

set OPENSSL_CONF=%~DP0openssl.cfg

rem ����������Ҫ�� CA ��������ǩ��֤�飬������ǩ��֤��֮ǰ��Ȼ�Ǵ����Լ��� CSR �ļ�
openssl req -new -key keys\server.key -out keys\server.csr -subj /C=CN/ST=BeiJing/L=BeiJing/O=DEMO/CN=www.ssldemo.com
rem ���Լ��� CA ��������֤�飬ǩ��������Ҫ CA ��֤���˽Կ���룬���հ䷢һ������ CA ǩ����֤��
openssl x509 -days 36500 -req -CA keys\ca.crt -CAkey keys\ca.key -CAcreateserial -in keys\server.csr -out keys\server.crt -extfile v3.ext

rem client ��
openssl req -new -key keys\client.key -out keys\client.csr -subj /C=CN/ST=BeiJing/L=BeiJing/O=DEMO/CN=www.ssldemo.com
rem client �˵� CA ǩ��
openssl x509 -days 36500 -req -CA keys\ca.crt -CAkey keys\ca.key -CAcreateserial -in keys\client.csr -out keys\client.crt -extfile v3.ext

pause