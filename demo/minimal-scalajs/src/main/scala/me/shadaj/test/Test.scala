package me.shadaj.test

class Abc {
  val foo = 1
}

object Lol {
  var abc = 1
}

object Test {
  Lol.abc = 2
  def run(): Unit = {
    println("hello, world")
    println((new Abc).foo)
    println((new Abc).isInstanceOf[Abc])
  }
}
