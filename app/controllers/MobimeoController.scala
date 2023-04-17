package controllers

import com.google.inject.Inject
import play.api.libs.json.{JsArray, Json}
import play.api.mvc.{Action, AnyContent, BaseController, ControllerComponents}

import java.text.SimpleDateFormat
import java.util.Calendar
import scala.collection.immutable.ListMap
import scala.io.Source

class MobimeoController @Inject()(val controllerComponents: ControllerComponents) extends BaseController{
  def getVehicleByCoordinatesAndTime(xCoordinate: Int, yCoordinate: Int, time: String): Action[AnyContent] = Action {
    val (
      stopsMap: Map[(Int, Int), Int],
      timesMap: Map[Int, Map[String, List[Int]]],
      linesMap: Map[Int, String],
      delaysMap: Map[String, Int]
    ) = readFiles

    println(stopsMap)
    println(timesMap)
    println(linesMap)
    println(delaysMap)

    val stopId = stopsMap.get((xCoordinate, yCoordinate))

    stopId match {
      case Some(stopId) =>
        val timeLinesMap = timesMap(stopId)

        val lineIds = timeLinesMap.get(time)

        lineIds match {
          case Some(lineIds) =>
            var result: JsArray = JsArray()

            for(lineId <- lineIds) {
              val vehicle = linesMap(lineId)

              result = result.append(Json.obj("lineId" -> lineId, "vehicalName" -> vehicle))
            }
            Ok(result).as("application/json")

          case None => NotFound("Lines for these coordinates ane time not found").as("application/json")
        }
      case None => NotFound("Stop for these coordinates not found").as("application/json")
    }
  }

  def getVehicleByStopId(stopId: Int): Action[AnyContent] = Action {
    val (
      stopsMap: Map[(Int, Int), Int],
      timesMap: Map[Int, Map[String, List[Int]]],
      linesMap: Map[Int, String],
      delaysMap: Map[String, Int]
    ) = readFiles

    println(stopsMap)
    println(timesMap)
    println(linesMap)
    println(delaysMap)


    val now = Calendar.getInstance().getTime
    val minuteFormat = new SimpleDateFormat("HH:mm:ss")
    val currentMinuteAsString = minuteFormat.format(now)

    val timeLinesMap = timesMap.get(stopId)

    timeLinesMap match {
      case Some(timeLines) =>
        var result: JsArray = JsArray()
        if(timeLines.contains(currentMinuteAsString)) {
          for (lineId <- timeLines(currentMinuteAsString)) {
            val vehicle = linesMap(lineId)

            result = result.append(Json.obj("lineId" -> lineId, "vehicalName" -> vehicle))
          }
          Ok(result).as("application/json")
        } else {
          val timeLinesWithCurrentTime: Map[String, List[Int]] = timeLines + (currentMinuteAsString -> List(-1))
          val timeLinesWithCurrentTimeSortedList = timeLinesWithCurrentTime.toSeq.sortWith(sortByTime)
          val timeLinesWithCurrentTimeSortedMap = ListMap(timeLinesWithCurrentTimeSortedList.zipWithIndex: _*)
          val indexOfPickedLine = (timeLinesWithCurrentTimeSortedMap(currentMinuteAsString -> List(-1)) + 1) % timeLinesWithCurrentTimeSortedList.size

          for (lineId <- timeLinesWithCurrentTimeSortedList(indexOfPickedLine)._2) {
            val vehicle = linesMap(lineId)

            result = result.append(Json.obj("lineId" -> lineId, "vehicalName" -> vehicle))
          }
          Ok(result).as("application/json")
        }
      case None => NotFound("Stop not found").as("application/json")
    }
  }

  def getDelayForLineId(lineId: Int): Action[AnyContent] = Action {
    val (
      _: Map[(Int, Int), Int],
      _: Map[Int, Map[String, List[Int]]],
      linesMap: Map[Int, String],
      delaysMap: Map[String, Int]
    ) = readFiles

    val lineName = linesMap.get(lineId)

    lineName match {
      case Some(lineName) =>
        val delay = delaysMap.get(lineName)

        delay match {
          case Some(delay) =>
            Ok(Json.obj("lineId" -> lineId, "vehicalName" -> lineName, "delay" -> delay)).as("application/json")
          case None =>
            Ok(Json.obj("lineId" -> lineId, "vehicalName" -> lineName, "delay" -> 0)).as("application/json")
        }
      case None => NotFound("Line Name not found").as("application/json")
    }
  }

  private def sortByTime(timeLine1: (String, List[Int]), timeLine2: (String, List[Int])): Boolean = {

    val time1Array = timeLine1._1.split(":")
    val time2Array = timeLine2._1.split(":")

    if (time1Array(0) < time2Array(0))
      return true
    else if (time1Array(0) == time2Array(0)) {
      if (time1Array(1) < time2Array(1))
        return true
      else if (time1Array(1) == time2Array(1)) {
        if (time1Array(2) < time2Array(2))
          return true
      }
    }

    false
  }

  private def readFiles = {
    var stopsMap: Map[(Int, Int), Int] = Map[(Int, Int), Int]()
    var timesMap: Map[Int, Map[String, List[Int]]] = Map[Int, Map[String, List[Int]]]()
    var linesMap: Map[Int, String] = Map[Int, String]()
    var delaysMap: Map[String, Int] = Map[String, Int]()

    var src = Source.fromFile("C:\\Users\\toufi\\Downloads\\Scala_MobimeoBackendChallenge\\data\\stops.csv")
    for (line <- src.getLines().drop(1)) {
      val cols = line.split(",").map(_.trim)

      stopsMap = stopsMap + ((cols(1).toInt, cols(2).toInt) -> cols(0).toInt)
    }

    src.close

    src = Source.fromFile("C:\\Users\\toufi\\Downloads\\Scala_MobimeoBackendChallenge\\data\\times.csv")
    for (line <- src.getLines().drop(1)) {
      val cols = line.split(",").map(_.trim)
      val currentStopMap: Option[Map[String, List[Int]]] = timesMap.get(cols(1).toInt)

      if (currentStopMap.isEmpty)
        timesMap = timesMap + (cols(1).toInt -> Map(cols(2) -> List(cols(0).toInt)))
      else {
        var lineList: List[Int] = currentStopMap.get.getOrElse(cols(2), List())

        if(lineList.isEmpty)
          lineList = List(cols(0).toInt)
        else
          lineList = lineList :+ cols(0).toInt

        val newStopMap: Map[String, List[Int]] = currentStopMap.get + (cols(2) -> lineList)
        timesMap = timesMap + (cols(1).toInt -> newStopMap)
      }
    }

    src.close

    src = Source.fromFile("C:\\Users\\toufi\\Downloads\\Scala_MobimeoBackendChallenge\\data\\lines.csv")
    for (line <- src.getLines().drop(1)) {
      val cols = line.split(",").map(_.trim)

      linesMap = linesMap + (cols(0).toInt -> cols(1))
    }

    src.close

    src = Source.fromFile("C:\\Users\\toufi\\Downloads\\Scala_MobimeoBackendChallenge\\data\\delays.csv")
    for (line <- src.getLines().drop(1)) {
      val cols = line.split(",").map(_.trim)

      delaysMap = delaysMap + (cols(0) -> cols(1).toInt)
    }

    src.close
    (stopsMap, timesMap, linesMap, delaysMap)
  }
}
