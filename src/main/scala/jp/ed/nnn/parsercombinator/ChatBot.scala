package jp.ed.nnn.parsercombinator

import java.time.{LocalDateTime, ZoneId}
import scala.util.Random
import scala.util.matching.Regex

case class ChatBot(commands: List[Command])

sealed trait Command {
  def exec(input: String): Boolean
}

case class ReplyCommand(regex: Regex, replies: List[String]) extends Command {
  override def exec(input: String): Boolean = {
    regex.findFirstIn(input) match {
      case Some(_) =>
        println(Random.shuffle(replies).head)
        true
      case None => false
    }
  }
}

case class TimeCommand(regex: Regex, start: Int, end: Int, zone: String, replies: List[String]) extends Command {
  override def exec(input: String): Boolean = {
    val now = LocalDateTime.now().atZone(ZoneId.of(zone))
    val isInTime = start <= now.getHour && now.getHour <= end
    regex.findFirstIn(input) match {
      case Some(_) if isInTime =>
        println(Random.shuffle(replies).head)
        true
      case _ => false
    }
  }
}

case class BirthdayCommand(regex: Regex, month: Int, day: Int, zone: String, replies: List[String]) extends Command {
  override def exec(input: String): Boolean = {
    val now = LocalDateTime.now().atZone(ZoneId.of(zone))
    val isBirthday = now.getMonth.getValue == month && now.getDayOfMonth == day
    regex.findFirstIn(input) match {
      case Some(_) if isBirthday =>
        println(Random.shuffle(replies).head)
        true
      case _ => false
    }
  }
}
