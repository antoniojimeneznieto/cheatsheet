package io.netty.incubator.channel.uring

object  NativeAccess {
    // Even if we do not have access to a class, we can put the package where
    // the class is located and then call the methods from here.
    def someNative: Unit = Native.CMSG_OFFSETOF_CMSG_LEN
    def someotherNative: RingBuffer = Native.createRingBuffer()
  
}

object NettyUring {

    val ioUringChannel = new IOUringChannelOption[Int]()
    
    val ioUringDatagramChannel = new IOUringDatagramChannel()

    //val ioUringEventLoop = new IOUringEventLoop()

    val ioUringServerSocketChannel = new IOUringServerSocketChannel()

    val ioUringSocketChannel = new IOUringSocketChannel()

    val ioUringTcpInfo = new IOUringTcpInfo()





    

}
