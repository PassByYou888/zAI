@echo off

rem ��һ�� Ϊ�������˺Ϳͻ���׼����Կ��˽Կ

cd "%~DP0"
mkdir keys

set OPENSSL_CONF=%~DP0openssl.cfg

rem ���ɷ�������˽Կ
openssl genrsa -out keys\server.key 2048
rem ���ɷ������˹�Կ
openssl rsa -in keys\server.key -pubout -out keys\server.pem


rem ���ɿͻ���˽Կ
openssl genrsa -out keys\client.key 2048
rem ���ɿͻ��˹�Կ
openssl rsa -in keys\client.key -pubout -out keys\client.pem

pause