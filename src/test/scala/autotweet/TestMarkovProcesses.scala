package autotweet

import scala.collection.immutable.HashSet

import org.junit.Test
import org.junit.Assert.assertTrue

class TestMarkovProcesses {

  @Test
  def wordsExtracted() {
    val words = new HashSet() ++ MarkovProcesses.getWords("Links to be posted over xmas  period: slides, presentations and workshops that I'll post up for you all, holla if you you have a favourite!")
    for(expected <- List("Links",
      "to",
      "be",
      "posted",
      "over",
      "xmas",
      "period",
      "slides",
      "presentations",
      "and",
      "workshops",
      "that",
      "I'll",
      "post",
      "up",
      "for",
      "you",
      "all",
      "holla",
      "if",
      "you",
      "you",
      "have",
      "a",
      "favourite")) {
      assertTrue(words.contains(expected))
    }
  }
  
  @Test
  def wordPicked() {
      val words = Map("a" -> 0.5, "b" -> 0.25, "c" -> 0.25)
      for(x <- 0 to 20)
    	  println(MarkovProcesses.pick(words))
  }

}