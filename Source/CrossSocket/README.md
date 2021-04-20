# Delphi 跨平台 Socket 通讯库

作者: WiNDDRiVER(soulawing@gmail.com)

## 特性

- 针对不同平台使用不同的IO模型:
  - IOCP
  > Windows

  - KQUEUE
  > FreeBSD(MacOSX, iOS...)

  - EPOLL
  > Linux(Linux, Android...)

- 支持极高的并发
 
  - Windows    
  > 能跑10万以上的并发数, 需要修改注册表调整默认的最大端口数

  - Mac    
  > 做了初步测试, 测试环境为虚拟机中的 OSX 10.9.5, 即便修改了系统的句柄数限制,
  > 最多也只能打开32000多个并发连接, 或许 OSX Server 版能支持更高的并发吧

- 同时支持IPv4、IPv6

- 零内存拷贝

## 已通过测试
- Windows
- OSX
- iOS
- Android
- Linux

## 建议开发环境
- 要发挥跨平台的完整功能请使用Delphi 10.2 Tokyo及以上的版本
- 最低要求支持泛型和匿名函数的Delphi版本, 具体是从哪个版本开始支持泛型和匿名函数的我也不是太清楚

## 已知问题
- iOS做了初步测试, 连接数超过80以后还有些问题, 不过通常iOS下的应用谁会去开好几十
  连接呢？

- Android初步测试, 并发到450之后就无法增加了, 可能受限于系统的文件句柄数设置.

- Ubuntu桌面版下似乎有内存泄漏
  但是追查不到到底是哪部分代码造成的,
  甚至无法确定是delphi内置的rtl库还是我所写的代码引起的.
  通过 LeakCheck 库能粗略看出引起内存泄漏的是一个 AnsiString 变量,
  并不能定位到具体的代码.
  但是我自己的代码里根本没有任何地方定义或者使用过类似的变量,
  其它Linux发行版本尚未测试.

## 部分测试截图

- **HTTP**(服务端为ubuntu 16.04 desktop)
![20170607110011](https://user-images.githubusercontent.com/3221597/26860614-61b750b4-4b71-11e7-8afc-74c3ebf16f7e.png)

- **HTTPS**(服务端为ubuntu 16.04 desktop)
![20170607142650](https://user-images.githubusercontent.com/3221597/26868229-d8d79f40-4b9a-11e7-927c-bfb3d7e6e55d.png)
