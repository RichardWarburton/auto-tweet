package autotweet

import scala.io.Source
import java.io.File

object GenerateTweets extends App {

  val user = args(0)
  val tweets = Source.fromFile("var"+File.separatorChar+user).getLines
  val process = MarkovProcesses.learn(tweets)
  // dump strings into a set to remove duplicates
  val generated = Set() ++ (0 to 10).map(_ => MarkovProcesses.generate(process))
  generated foreach println
  
}