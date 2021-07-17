import java.io.PrintWriter
import scala.io.Source
import scala.util.hashing.MurmurHash3

class BloomFilter[T](var elementsCount: Int, var falsePositiveProbability: Double){
  /**
   * An implementation of standard Bloom Filter working on MurmurHash3 - Non-cryptographic hash function
   */
  // Initial setup
  private val arraySize = getSize
  private val numberOfHashes = getHashCount
  private val bitArray = Array.ofDim[Int](arraySize)

  def getSize: Int ={
    /**
     * returns size of the bit array for given expected number of stored elements
     * and false-positive probability
     */
    val m = -elementsCount*math.log(falsePositiveProbability)/math.pow(math.log(2),2)
    math.round(m).toInt
  }

  def getHashCount: Int = {
    /**
     * returns optimal number of hash functions for given size of bit array
     * and number of stored elements
     */
    val k = getSize*math.log(2)/elementsCount
    math.round(k).toInt
  }

  def insertElement(element: T): Unit = {
    /**
     * remember the element.
     * Before hash element is converted into lowercase string
     */
    var index = 0
    for (i <- 1 to numberOfHashes){
      index = math.abs(MurmurHash3.stringHash(element.toString.toLowerCase, i) % arraySize)
      bitArray(index) = 1
    }
  }

  def belongsToSet(element: T): Boolean = {
    /**
     * check if the element probably belongs to Set.
     * Before hash element is converted into lowercase string
     */
    var index = 0
    for (i <- 1 to numberOfHashes){
      index = math.abs(MurmurHash3.stringHash(element.toString.toLowerCase, i) % arraySize)
      if (bitArray(index) == 0) {
        return false
      }
    }
    true
  }

   def printBitArray: Unit ={
     bitArray.mkString(", ").foreach(print)
     println()
   }
  def printInfo(export_to_txt: Boolean = false): Unit = {
    /**
     * Helper function for getting main info about filter
     */
    val printedString = "----------------------------------------------------" + "\n" +
                        "Information about created bloom filter:" + "\n" +
                        "----------------------------------------------------" + "\n" +
                        " -> number of inserted elements: " + elementsCount + "\n" +
                        " -> false-positive probability: " + falsePositiveProbability + "\n" +
                        " -> optimal size of bit array: " + arraySize + "\n" +
                        " -> optimal number of hash functions: " + numberOfHashes + "\n" +
                        " -> type of hash function: MurmurHash3"

    println(printedString)
    if (export_to_txt){
      // FileWriter
      val writer = new PrintWriter("BitMap_and_bloomFilter_info.txt")
      writer.write(printedString)
      writer.write("\n")
      writer.write("\n")
      writer.write("Bit Map from the bloom filter:")
      writer.write("\n")
      writer.write(bitArray.mkString(", "))
      writer.close()
      println("Successfully exported data to file")
    }
  }

}

object BF {
  def main(args: Array[String]): Unit = {
    /** TEST  of standard Bloom Filter*/

    // get list of BDA students from file:
    val students = Source.fromFile("BDA_students.txt").getLines.toList
    students.foreach(println)
    println("Liczba studentów: " + students.length)

    val bloomFilter = new BloomFilter[String](students.length, 0.001)

    students.foreach(bloomFilter.insertElement)
    bloomFilter.printBitArray
    println("Check of already existing student: ")
    println(bloomFilter.belongsToSet("Stasiak Łukasz"))
    println(bloomFilter.belongsToSet("Wolszczak Krystian"))
    println(bloomFilter.belongsToSet("Nykiel Julia"))
    println("Check non-existing student: ")
    println(bloomFilter.belongsToSet("Kowalski Jan"))

    bloomFilter.printInfo(export_to_txt = true)
  }
}
