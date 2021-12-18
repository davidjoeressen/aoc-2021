import java.io.File

interface Packet {
  fun versionSum(): Int
  fun eval(): Long
}

data class Literal(val version: Int, val value: Long) : Packet {
  override fun versionSum(): Int = version
  override fun eval(): Long = value
}

data class Operator(val version: Int, val packetType: Int, val subPackets: List<Packet>) : Packet {
  override fun versionSum(): Int = version + subPackets.sumOf { it.versionSum() }
  override fun eval(): Long =
    when (packetType) {
      0 -> subPackets.sumOf { it.eval() }
      1 -> subPackets.map { it.eval() }.reduce { x, y -> x * y }
      2 -> subPackets.minOf { it.eval() }
      3 -> subPackets.maxOf { it.eval() }
      5 -> if (subPackets[0].eval() > subPackets[1].eval()) 1 else 0
      6 -> if (subPackets[0].eval() < subPackets[1].eval()) 1 else 0
      7 -> if (subPackets[0].eval() == subPackets[1].eval()) 1 else 0
      else -> 0
    }
}

fun List<Int>.toInt(): Int = toLong().toInt()
fun List<Int>.toLong(): Long = fold(0) { acc, x -> acc * 2 + x }

fun String.toBitList() =
  flatMap { it.digitToInt(16).toString(2).padStart(4, '0').toList().map(Char::digitToInt) }

fun parseAll(bits: List<Int>): List<Packet> {
  var b = bits
  val packets = mutableListOf<Packet>()
  while (b.size > 10) {
    parse(b).let { (p, rest) ->
      b = rest
      packets += p
    }
  }
  return packets.toList()
}

fun parseMultiple(bits: List<Int>, n: Int): Pair<List<Packet>, List<Int>> {
  var b = bits
  val packets = List(n) {
    parse(b).also { b = it.second }.first
  }
  return packets to b
}

fun parse(bits: List<Int>): Pair<Packet, List<Int>> {
  val version = bits.take(3).toInt()
  val packetType = bits.drop(3).take(3).toInt()
  val payload = bits.drop(6)
  return if (packetType == 4) {
    payload.chunked(5).let { chunks ->
      val i = chunks.indexOfFirst { it.first() == 0 } + 1
      val value = chunks.take(i).flatMap { it.drop(1) }.toLong()
      Literal(version, value) to chunks.drop(i).flatten()
    }
  } else if (payload.first() == 0) {
    val dataLength = payload.drop(1).take(15).toInt()
    payload.drop(16).let {
      Operator(version, packetType, parseAll(it.take(dataLength))) to it.drop(dataLength)
    }
  } else {
    val packageCount = payload.drop(1).take(11).toInt()
    parseMultiple(payload.drop(12), packageCount).let { (ps, rest) ->
      Operator(version, packetType, ps) to rest
    }
  }
}

fun solve(line: String) {
  val parsed = parse(line.toBitList()).first
  println("Part 1: ${parsed.versionSum()}")
  println("Part 2: ${parsed.eval()}")
}

fun main(args: Array<String>) {
  if (args.isEmpty()) {
    println("Usage: java -jar main.jar <file>")
    return
  }
  File(args[0]).forEachLine { solve(it) }
}
