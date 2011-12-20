package autotweet

import scala.collection.JavaConversions._
import twitter4j.TwitterFactory
import twitter4j.TwitterFactory
import twitter4j.Twitter
import twitter4j.auth.AccessToken
import twitter4j.conf.ConfigurationBuilder
import twitter4j.Paging
import java.io._

object DownloadTweets extends App {
  
  val username = args(0)
  val twitter = new TwitterFactory().getInstance()
  val tweets = twitter.getUserTimeline(username,new Paging(1,75))
  val filePath = "var"+File.separatorChar+username
  // TODO: fix up error case
  println("Writing to: "+filePath)
  val pw = new PrintWriter(new FileWriter(filePath))
  try {
	  tweets.foreach(x => pw.append(x.toString()))
  } finally {
	  pw.close();
  }
  
}