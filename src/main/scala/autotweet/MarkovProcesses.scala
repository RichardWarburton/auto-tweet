package autotweet

import scala.collection.Iterable
import scala.collection.mutable.HashMap
import scala.collection.Map
import scala.util.matching.Regex
import scala.util.Random

// TODO: move to n-gram model
// TODO: instead of special casing first gram/Scorable etc and add a beginning-of-line character 
object MarkovProcesses {

  trait Scorable {
    def setScore(score:Double)
    def getScore():Double
  }
  
  case class Word(var count:Long, var initial:Double, next:HashMap[String,Double]) extends Scorable {
    def setScore(score:Double) = initial = score
    def getScore():Double = initial
  }
  
  class Box(process:HashMap[String,Double], word:String) extends Scorable {
    def setScore(score:Double) = process(word) -> score
    def getScore():Double = process(word)
  }
  
  case class Process(aveCount:Int,initials:Map[String,Double], words:HashMap[String,Word])
  
  def learn(messages:Iterator[String]) = {
    val process = new HashMap[String,Word]()
    def getNode(word:String):Word = process.getOrElseUpdate(word,Word(0,0,new HashMap[String,Double]))
    val stripped = messages.map(getWords).toList
    for (words <- stripped) {
      getNode(words.first).initial += 1d
      
      for((word,i) <- words.dropRight(1).zipWithIndex) {
        val next = words(i+1)
        val node = getNode(word)
        node.count += 1
        node.next.update(next,1d + node.next.getOrElseUpdate(next,0d))
      }
      
      getNode(words.last).count += 1
    }
    
    normalise(process.values)
    for(word <- process.values) {
      normalise(word.next.keys.map(w => new Box(word.next,w)))
    }
    val initials = process filter { case (s,w) => w.initial > 0} mapValues (_.initial)
    val ave = stripped.map(_.size).toSeq.sum.toDouble / stripped.size
    new Process(ave.toInt,initials,process)
  }
  
  def generate(proc:Process) = {
    val w0 = pick(proc.initials)
    // TODO: remove try/catch and tidy
    (0 to proc.aveCount).foldLeft((w0,w0)) { case ((acc,x),_) => {
      try {
        val newWord = pick(proc.words(x).next)
        (acc + " " + newWord,newWord)
      } catch {
        case e => (acc,"")
      }
    }}._1
  }
  
  val random = new Random()
  
  def pick(scores:Map[String,Double]) = {
    var count = 0d
    var guess = random.nextDouble()
    scores.find {case (word,value) => {
      count += value
      guess <= count
    }}.get._1
  }
  
  def normalise(values:Iterable[_ <: Scorable]) {
    val total = values.map(_.getScore()).toSeq.sum
    values.foreach(score => score.setScore(score.getScore() / total))
  }
  
  def getWords(msg:String) = new Regex("(#|@|http://)?[A-Za-z'./_]+").findAllIn(msg).toArray
  
}