package com.grandivory.scalatron.bot

import com.grandivory.scalatron.bot.commands._
import com.grandivory.scalatron.bot.util.Direction._
import com.grandivory.scalatron.bot.util.ViewObject.Empty
import com.grandivory.scalatron.bot.util._

import scalax.collection.edge.WDiEdge

// Graph Library
import scalax.collection.mutable.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._

object Bot {

  val angryQuotes: List[String] = "Looking good, sweetheart!" ::
    "I can't find my pants!" :: "I dunno if that's vomit!" :: "I'll have another!" ::
    "Ain't no bad booze!" :: "Bartender! Hey! Bartender!" :: Nil

  val ROLE_MISSILE = "missile"
  val ROLE_GATHERER = "gatherer"
  val GATHERER_MAX_ENERGY = 2000

  def figureSurroundings(rp:RelativePosition, adj:Double => Double): List[Double] = {
    List(
      rp.euclidianDistanceTo(RelativePosition(LeftRight(-1), UpDown(-1))),
      rp.euclidianDistanceTo(RelativePosition(LeftRight(0),  UpDown(-1))),
      rp.euclidianDistanceTo(RelativePosition(LeftRight(1),  UpDown(-1))),

      rp.euclidianDistanceTo(RelativePosition(LeftRight(-1), UpDown(0))),
      rp.euclidianDistanceTo(RelativePosition(LeftRight(1),  UpDown(0))),

      rp.euclidianDistanceTo(RelativePosition(LeftRight(-1), UpDown(1))),
      rp.euclidianDistanceTo(RelativePosition(LeftRight(0),  UpDown(1))),
      rp.euclidianDistanceTo(RelativePosition(LeftRight(1),  UpDown(1)))
    ) map adj
  }

  def computeWeight(go:ViewObject)(in: Double) = go match {
    case Wall => in * -200
    case Snorg => in * -150
    case Fluppet => in * 750
    case Zugar => in * 500
    case Toxifera => in * -200
    case EnemyBot => in * -800
    case EnemySlave => in * -800
    case MasterBot => 0.0
    case Slave => 0.0
    case _ => 0.0
   }

  def computeWeightedArray(go:ViewObject, rp:RelativePosition, adj:Double => Double): List[Double] =
    figureSurroundings(rp, adj) map computeWeight(go)

  def findDirection(in:List[Double]): Direction = {
    val conv:Map[Int, Direction] = Map(
      0 -> UpLeft, 1 -> Up, 2 -> UpRight,
      3 -> Left,            4 -> Right,
      5 -> DownLeft, 6-> Down, 7 -> DownRight
      )

    conv( in.zipWithIndex.maxBy(_._1)._2 )
  }

  /**
    * This is the main function that is called each round that your bot can take an action. Your bot is given an
    * opcode that represents what it should react to, and it must issue a command to perform. The main bot can
    * only react every OTHER round, whereas slave bots can react every round
    */
  def performAction(controlCode: Option[ControlOpCode]): Option[BotCommand] =
    controlCode match {
    case Some(React(
      generation,
      botName,
      currentRound,
      view,
      currentEnergy,
      masterDirection,
      failedMoveDirection,
      numLivingSlaves,
      extraProps)) =>

      var action: Option[BotCommand] = None

      val r = scala.util.Random

      action = if ((r.nextInt(100) % 10) == 0) {
        Some(Move(Direction.randomDirection))
      } else if (view.canSeeOnlyWalls) {
        Some( Spawn(Direction.randomDirection,Some((numLivingSlaves+1).toString),1,None) )
      } else None

      if (action.isEmpty) {

        var summed:List[Double] = List[Double](0,0,0,0,0,0,0,0)

        // Iterate objects in view
        for {(key: RelativePosition, value: ViewObject) <- view.objectsInView} {

          def sign(in:Double): Double = {
            if (in>0 || in == 0) 1
            else  -1
          }

          def weightfunc(obj:ViewObject)(inval:Double) = obj match {
            case Wall => sign(inval) * (1.0 / (inval*inval))
            case _ => 1.0 / inval
          }

          summed = summed zip computeWeightedArray(value, key, weightfunc(value)) map { case (l,r) => l +r }
        }

        action = Some( Move( findDirection(summed) ) )
      }

      action

    case None =>
      Some(Say("?!?!?!?!?") + Move(Direction.Up))
    case _ => None
  }

  private def nearestSafeCell(cell: RelativePosition, view: View): Option[RelativePosition] = {
    view.objectsInView.keys.toList.sortBy(_ - cell).find {
      view.objectAt(_) match {
        case Some(Empty) => true
        case Some(go: GameObject) => go.isGood
        case _ => false
      }
    }
  }

  private def shortestPathTo(cell: RelativePosition, view: View): Option[List[Direction]] = {
    def okToMove(objectInSpot: Option[ViewObject]): Boolean = objectInSpot match {
      case Some(Empty) => true
      case Some(gameObject: GameObject) => gameObject.isGood
      case None => false
    }

    def nextNodes(pathSoFar: Vector[Direction], position: RelativePosition, visited: Set[RelativePosition]):
      Set[(Vector[Direction], RelativePosition)] = {
      Direction.allPossibleDirections map { direction: Direction =>
        (pathSoFar :+ direction) -> (position + direction)
      } filter {
        case (_: Vector[Direction], newPosition: RelativePosition) =>
          !visited.contains(newPosition) &&
            okToMove(view.objectAt(newPosition))
      }
    }

    if (Origin == cell) Some(Nil)
    else if (!okToMove(view.objectAt(cell))) None
    else {
      val allPaths: Stream[(Set[(Vector[Direction], RelativePosition)], Set[RelativePosition])] =
        Stream.iterate((Set((Vector.empty[Direction], Origin: RelativePosition)), Set[RelativePosition](Origin))) {
          case (moveSet: Set[(Vector[Direction], RelativePosition)], visitedPositions) =>
            val nextMoveSets = moveSet flatMap {
              case (directions, currentPosition) =>
                nextNodes(directions, currentPosition, visitedPositions)
            }

            val newVisitedPositions = visitedPositions ++ nextMoveSets.map(_._2)
            (nextMoveSets, newVisitedPositions)
        } takeWhile {
          case (moveSet: Set[(Vector[Direction], RelativePosition)], visitedCells: Set[RelativePosition]) =>
            moveSet.nonEmpty
        }

      allPaths.take(10).flatMap(_._1.toStream).collectFirst {
        case (directions: Vector[Direction], endPosition: RelativePosition) if endPosition == cell => directions.toList
      }
    }
  }

  private def andAlso(left: Option[BotCommand], right: Option[BotCommand]): Option[BotCommand] = {
    (left, right) match {
      case (Some(l), Some(r)) => Some(l + r)
      case (Some(l), None) => left
      case (None, Some(r)) => right
      case (None, None) => None
    }
  }
/*
  private def pathToCell(startNode:ViewObject, endNode:ViewObject, visibleZone:View):Direction = {

    val visibilityZoneGraph: Graph[RelativePosition, WDiEdge] = Graph[RelativePosition, WDiEdge]()
    //visibilityZoneGraph += startNode ~> endNode

    visibleZone.objectsInView.foldLeft(visibilityZoneGraph) { (graphAggregator, item) => item match {
      case (Some(firstNode), Some(secondNode)) => graphAggregator += firstNode ~> secondNode % 1 /*weight value here*/
      case (Some(firstNode), None) => graphAggregator += firstNode
      case _ => graphAggregator
    }}

    visibilityZoneGraph find endNode match {
      case /*Success*/ => {
        visibilityZoneGraph get startNode shortestPathTo endNode match {
          case Some(pathLength) => (pathLength.weight + 1).toInt
          case None => 9999
        }
      }
      // For some reason there is no endNode in the map
      case None => 9999

    }

  }*/
}
