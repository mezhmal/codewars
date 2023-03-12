case class Example(holeCards: List[String], communityCards: List[String], expected: (String, List[String]))

@main def solution(params: String*): Unit =
  val examples: List[Example] = List(
    Example(List("K♠", "A♦"), List("J♣", "Q♥", "9♥", "2♥", "3♦"), ("nothing", List("A", "K", "Q", "J", "9"))),
    Example(List("K♠", "Q♦"), List("J♣", "Q♥", "9♥", "2♥", "3♦"), ("pair", List("Q", "K", "J", "9"))),
    Example(List("K♠", "J♦"), List("J♣", "K♥", "9♥", "2♥", "3♦"), ("two pair", List("K", "J", "9"))),
    Example(List("4♠", "9♦"), List("J♣", "Q♥", "Q♠", "2♥", "Q♦"), ("three-of-a-kind", List("Q", "J", "9"))),
    Example(List("Q♠", "2♦"), List("J♣", "10♥", "9♥", "K♥", "3♦"), ("straight", List("K", "Q", "J", "10", "9"))),
    Example(List("7♠", "10♦"), List("10♣", "Q♥", "8♥", "J♥", "9♦"), ("straight", List("Q", "J", "10", "9", "8"))),
    Example(List("A♠", "K♦"), List("J♥", "5♥", "10♥", "Q♥", "3♥"), ("flush", List("Q", "J", "10", "5", "3"))),
    Example(List("A♠", "K♦"), List("J♦", "5♦", "10♦", "Q♦", "3♦"), ("flush", List("K", "Q", "J", "10", "5"))),
    Example(List("A♠", "A♦"), List("K♣", "K♥", "A♥", "Q♥", "3♦"), ("full house", List("A", "K"))),
    Example(List("K♥", "Q♠"), List("K♣", "Q♥", "3♣", "Q♦", "K♦"), ("full house", List("K", "Q"))),
    Example(List("2♠", "3♦"), List("2♣", "2♥", "3♠", "3♥", "2♦"), ("four-of-a-kind", List("2", "3"))),
    Example(List("5♠", "3♦"), List("5♣", "5♥", "3♠", "3♥", "5♦"), ("four-of-a-kind", List("5", "3"))),
    Example(List("8♠", "6♠"), List("7♠", "5♠", "9♠", "J♠", "10♠"), ("straight-flush", List("J", "10", "9", "8", "7"))),
  )
  for (example <- examples) {
    val result = Poker.hand(example.holeCards, example.communityCards)
    println(s"${if (result == example.expected) '+' else '-'} $result ${if (result == example.expected) '=' else '!'}= ${example.expected}")
  }

object Poker {
  object Combinations {
    val nothing = "nothing"
    val pair = "pair"
    val twoPair = "two pair"
    val threeOfAKind = "three-of-a-kind"
    val straight = "straight"
    val flush = "flush"
    val fullHouse = "full house"
    val fourOfAKind = "four-of-a-kind"
    val straightFlush = "straight-flush"
  }

  type DecodedRank = String
  type EncodedRank = Int
  type Suit = Char
  type SplittedCard = (EncodedRank, Suit)
  type GroupedRanks = (Int, EncodedRank)
  type GroupedSuites = (Int, Suit)
  type Kickers = List[DecodedRank]

  def encodeCardRank(value: DecodedRank): EncodedRank = value match {
    case "J" => 11
    case "Q" => 12
    case "K" => 13
    case "A" => 14
    case s => s.toInt
  }

  def decodeCardRank(value: EncodedRank): DecodedRank = value match {
    case 11 => "J"
    case 12 => "Q"
    case 13 => "K"
    case 14 => "A"
    case i => i.toString
  }

  def splitIntoRankAndSute(card: String): SplittedCard = (encodeCardRank(card.dropRight(1)), card.last)

  def getEncodedRanks(cards: List[String]): List[EncodedRank] = cards.map(_.dropRight(1)).map(encodeCardRank)

  def groupBySuits(cards: List[String]): List[GroupedSuites] = 
    cards.map(_.last).groupBy(_.self).map(s => (s._2.size, s._1)).toList.sorted.reverse

  def groupByRanks(cards: List[String]): List[GroupedRanks] = 
    getEncodedRanks(cards).groupBy(_.self).toList.map(s => (s._2.size, s._1)).sorted.reverse

  def getLongestSequince(ranks: List[EncodedRank]): List[EncodedRank] = {
    val data = ranks.distinct.sorted
    var sequences = List[List[Int]]()
    var sequenceAccumulator = List(data.head)
    for (rank <- data.tail) {
      if (rank == sequenceAccumulator.last + 1) {
        sequenceAccumulator = sequenceAccumulator :+ rank
      } else {
        sequences = sequences :+ sequenceAccumulator
        sequenceAccumulator = List(rank)
      }
    }
    if (sequenceAccumulator.nonEmpty) {
      sequences = sequences :+ sequenceAccumulator
    }
    sequences.sortBy(s => (s.size, s.head)).last
  }

  def checkIfStraightFlush(cards: List[String]): Option[List[String]] =
    groupBySuits(cards).head match {
      case firstGroupedSuit if firstGroupedSuit._1 >= 5 => {
        val suitCards = cards.map(splitIntoRankAndSute).filter(_._2 == firstGroupedSuit._2).sorted
        getLongestSequince(suitCards.map(_._1)) match {
          case x if x.size >= 5 => Some(x.reverse.take(5).map(decodeCardRank))
          case _ => None
        }
      }
      case _ => None
    }

  def checkIfFourOfAKind(cards: List[String]): Option[List[String]] =
    groupByRanks(cards).head match {
      case firstGroupedRank if firstGroupedRank._1 == 4 => {
        val rankOfTheFourCards = firstGroupedRank._2
        val highestOtherRank = getEncodedRanks(cards).filter(_ != rankOfTheFourCards).sorted.last
        Some(List(rankOfTheFourCards, highestOtherRank).map(decodeCardRank))
      }
      case _ => None
    }

  def checkIfFullHouse(cards: List[String]): Option[List[String]] =
    groupByRanks(cards) match {
      case rankGroup1 :: rankGroup2 :: _ => 
        if (rankGroup1._1 != 3 || rankGroup2._1 < 2) None else Some(List(rankGroup1._2, rankGroup2._2).map(decodeCardRank))
      case _ => None
    }

  def checkIfFlush(cards: List[String]): Option[List[String]] =
    groupBySuits(cards).head match {
      case firstGroupedSuit if firstGroupedSuit._1 >= 5 =>
        Some(cards.map(splitIntoRankAndSute).filter(_._2 == firstGroupedSuit._2).map(_._1).sorted.reverse.take(5).map(decodeCardRank))
      case _ => None
    }

  def checkIfStraight(cards: List[String]): Option[List[String]] =
    getLongestSequince(getEncodedRanks(cards)) match {
      case x if x.size >= 5 => Some(x.reverse.take(5).map(decodeCardRank))
      case _ => None
    }

  def checkIfThreeOfAKind(cards: List[String]): Option[List[String]] =
    groupByRanks(cards).head match {
      case x if x._1 == 3 => {
        val rankOfTheThreeCards = x._2
        val twoHighestOtherRanks = getEncodedRanks(cards).filter(_ != rankOfTheThreeCards).sorted.reverse.take(2)
        Some((List(rankOfTheThreeCards) ::: twoHighestOtherRanks).map(decodeCardRank))
      }
      case _ => None
    }

  def checkIfTwoPair(cards: List[String]): Option[List[String]] =
    groupByRanks(cards) match {
      case rankGroup1 :: rankGroup2 :: _ => {
        if (rankGroup1._1 != 2 || rankGroup2._1 != 2) None else {
          val highestOtherRank = getEncodedRanks(cards).filterNot(List(rankGroup1._2, rankGroup2._2).contains(_)).sorted.last
          Some(List(rankGroup1._2, rankGroup2._2, highestOtherRank).map(decodeCardRank))
        }
      }
      case _ => None
    }

  def checkIfPair(cards: List[String]): Option[List[String]] =
    groupByRanks(cards).head match {
      case x if x._1 == 2 => {
        val rankOfPair = x._2
        val threeHighestOtherRanks = getEncodedRanks(cards).filter(_ != rankOfPair).sorted.reverse.take(3)
        Some((List(rankOfPair) ::: threeHighestOtherRanks).map(decodeCardRank))
      }
      case _ => None
    }

  def hand(holeCards: List[String], communityCards: List[String]): (String, List[String]) = {
    val cards = holeCards ::: communityCards
    checkIfStraightFlush(cards) match {
      case Some(kickers) => (Combinations.straightFlush, kickers)
      case None => checkIfFourOfAKind(cards) match {
        case Some(kickers) => (Combinations.fourOfAKind, kickers)
        case None => checkIfFullHouse(cards) match {
          case Some(kickers) => (Combinations.fullHouse, kickers)
          case None => checkIfFlush(cards) match {
            case Some(kickers) => (Combinations.flush, kickers)
            case None => checkIfStraight(cards) match {
              case Some(kickers) => (Combinations.straight, kickers)
              case None => checkIfThreeOfAKind(cards) match {
                case Some(kickers) => (Combinations.threeOfAKind, kickers)
                case None => checkIfTwoPair(cards) match {
                  case Some(kickers) => (Combinations.twoPair, kickers)
                  case None => checkIfPair(cards) match {
                    case Some(kickers) => (Combinations.pair, kickers)
                    case None => (Combinations.nothing, getEncodedRanks(cards).sorted.reverse.take(5).map(decodeCardRank))
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}
