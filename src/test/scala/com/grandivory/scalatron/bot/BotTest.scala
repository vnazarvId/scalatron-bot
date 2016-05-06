package com.grandivory.scalatron.bot

import com.grandivory.scalatron.bot.commands._
import com.grandivory.scalatron.bot.util.{Direction, View}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.FunSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class BotTest extends FunSpec with GeneratorDrivenPropertyChecks {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 10000)

  val genView: Gen[View] = for {
    sideLength <- Gen.choose(1, 10)
  } yield {
    View("_"*sideLength*sideLength)
  }

  val genDirection: Gen[Direction] = Gen.oneOf(
    Direction.Up,
    Direction.UpRight,
    Direction.Right,
    Direction.DownRight,
    Direction.Down,
    Direction.DownLeft,
    Direction.Left,
    Direction.UpLeft
  )

  val genPropertyMap = for {
    size <- Gen.choose(0, 5)
    keys <- Gen.listOfN(size, Gen.alphaStr)
    values <- Gen.listOfN(size, Gen.alphaStr)
  } yield {
    keys.zip(values).toMap
  }

  val genWelcome: Gen[Welcome] = for {
    name <- Gen.identifier
    numRounds <- Gen.choose(1000, 50000)
    currentRound <- Gen.choose(1, numRounds)
    maxSlaves <- Gen.choose(1, 100)
  } yield {
    Welcome(name, numRounds, currentRound, maxSlaves)
  }


  def genReact(maxGeneration: Int = 5, maxEnergy: Int = 20000): Gen[React] = for {
    generation <- Gen.choose(0,maxGeneration)
    name <- Gen.identifier
    currentRound <- Gen.choose(1, 10000)
    view <- genView
    currentEnergy <- Gen.choose(0, maxEnergy)
    masterDirection <- Gen.option(genDirection)
    failedMoveDirection <- Gen.option(genDirection)
    numLivingSlaves <- Gen.choose(0, 10)
    extraProperties <- Gen.option(genPropertyMap)
  } yield {
    React(
      generation,
      name,
      currentRound,
      view,
      currentEnergy,
      masterDirection,
      failedMoveDirection,
      numLivingSlaves,
      extraProperties
    )
  }

  val genGoodbye: Gen[Goodbye] = for {
    finalEnergy <- Gen.choose(1,1000)
  } yield {
    Goodbye(finalEnergy)
  }

  val controlOpCodeGenerator: Gen[ControlOpCode] = Gen.frequency(
    (10, genWelcome),
    (10, genGoodbye),
    (80, genReact())
  )

  implicit val controlOpCodeOptionGenerator: Arbitrary[Option[ControlOpCode]] = Arbitrary {
    Gen.option(controlOpCodeGenerator)
  }

  describe("performAction") {
    it("should never produce more than one of the same action") {
      def hasSameActionMultipleTimes(command: BotCommand): Boolean = command match {
        case MultiCommand(commands: List[BotCommand]) =>
          commands.foldRight((false, Set.empty[String])) {
            (command: BotCommand, result: (Boolean, Set[String])) =>
              val (commandDupeFound: Boolean, commandTypes: Set[String]) = result
              if (commandDupeFound) result
              else if (commandTypes.contains(command.getClass.getSimpleName)) (true, commandTypes)
              else (commandDupeFound, commandTypes + command.getClass.getSimpleName)
            }._1
        case _ => false
      }

      forAll ("ControlOpCode") { opCode: Option[ControlOpCode] =>
        Bot.performAction(opCode) match {
          case Some(action: BotCommand) => assert(!hasSameActionMultipleTimes(action))
          case _ => succeed
        }
      }
    }

    it("should not use invalid characters") {
      def hasInvalidCharacters(command: BotCommand): Boolean = {
        def containsInvalidChars(input: String): Boolean = input.exists("(|,=".contains(_))

        command match {
          case Spawn(_, _, _, Some(slaveProperties)) =>
            slaveProperties.keys.exists(containsInvalidChars) ||
              slaveProperties.values.exists(containsInvalidChars)
          case SetProperties(properties) =>
            properties.keys.exists(containsInvalidChars) ||
              properties.values.exists(containsInvalidChars)
          case Say(text) => containsInvalidChars(text)
          case Status(text) => containsInvalidChars(text)
          case Log(text) => containsInvalidChars(text)
          case MultiCommand(commands) => commands.exists(hasInvalidCharacters)
          case _ => false
        }
      }

      forAll ("ControlOpCode") { opCode: Option[ControlOpCode] =>
        Bot.performAction(opCode) match {
          case Some(botCommand: BotCommand) => assert(!hasInvalidCharacters(botCommand))
          case None => succeed
        }
      }
    }

    it("should not add invalid property names") {
      def hasInvalidProperties(command: BotCommand): Boolean = {
        val invalidProperties = List(
          "generation",
          "name",
          "energy",
          "time",
          "view",
          "direction",
          "master",
          "collision"
        )

        command match {
          case Spawn(_, _, _, Some(slaveProperties)) => slaveProperties.keys.exists(invalidProperties.contains)
          case SetProperties(properties) => properties.keys.exists(invalidProperties.contains)
          case MultiCommand(commands) => commands.exists(hasInvalidProperties)
          case _ => false
        }
      }

      forAll ("controlOpCode") { controlOpCode: Option[ControlOpCode] =>
        Bot.performAction(controlOpCode) match {
          case Some(botAction: BotCommand) => assert(!hasInvalidProperties(botAction))
          case _ => succeed
        }
      }
    }

    it("should not try to explode the master bot") {
      forAll ((genReact(0), "controlOpCode")) { controlOpCode: React =>
        whenever(controlOpCode.generation == 0) {
          Bot.performAction(Some(controlOpCode)) match {
            case Some(_: Explode) => fail("Master bot tried to explode")
            case Some(MultiCommand(commands)) => assert(!commands.exists(_.isInstanceOf[Explode]))
            case _ => succeed
          }
        }
      }
    }

    it("should not try to spawn a slave bot if it doesn't have enough energy") {
      forAll ((genReact(maxEnergy = 99), "controlOpCode")) { controlOpCode: React =>
        whenever(controlOpCode.currentEnergy < 100) {
          Bot.performAction(Some(controlOpCode)) match {
            case Some(_: Spawn) => fail("Tried to spawn a slave bot without enough energy")
            case Some(MultiCommand(commands)) => assert(!commands.exists(_.isInstanceOf[Spawn]))
            case _ => succeed
          }
        }
      }
    }

    it("should not try to give a slave bot more energy than the parent has") {
      def spawnedChildWithExtraEnergy(command: BotCommand)(maxEnergy: Int): Boolean = command match {
        case Spawn(_, _, slaveEnergy, _) => slaveEnergy > maxEnergy
        case MultiCommand(commands) => commands.exists(spawnedChildWithExtraEnergy(_)(maxEnergy))
        case _ => false
      }

      forAll ((genReact(), "controlOpCode")) { controlOpCode: React =>
        Bot.performAction(Some(controlOpCode)) match {
          case Some(botAction: BotCommand) => assert(!spawnedChildWithExtraEnergy(botAction)(controlOpCode.currentEnergy))
          case _ => succeed
        }
      }
    }

    it("should never explode in only its own square") {
      forAll ((genReact(10), "controlOpCode")) { controlOpCode: React =>
        Bot.performAction(Some(controlOpCode)) match {
          case Some(Explode(x)) if x < 2 => fail("Tried to explode without damaging anything")
          case Some(MultiCommand(commands)) => assert(!commands.exists{
            case Explode(x) if x < 2 => true
            case _ => false
          })
          case _ => succeed
        }
      }
    }

    it("should never try to explode over too large an area") {
      forAll ((genReact(10), "controlOpCode")) { controlOpCode: React =>
        Bot.performAction(Some(controlOpCode)) match {
          case Some(Explode(x)) if x > 10 => fail("Tried to explode over too large an area")
          case Some(MultiCommand(commands)) => assert(!commands.exists{
            case Explode(x) if x > 10 => true
            case _ => false
          })
          case _ => succeed
        }
      }
    }
  }
}
