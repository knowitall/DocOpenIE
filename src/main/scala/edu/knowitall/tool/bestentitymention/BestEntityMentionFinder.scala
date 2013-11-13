package edu.knowitall.tool.bestentitymention

import edu.knowitall.repr.bestentitymention.BestEntityMentionResolvedDocument
import edu.knowitall.repr.bestentitymention.BestEntityMention
import edu.knowitall.repr.document.Document
import edu.knowitall.repr.coref.CorefResolved
import edu.knowitall.tool.coref.Mention
import edu.knowitall.repr.ner.StanfordNERAnnotated
import edu.knowitall.repr.document.Sentenced
import edu.knowitall.repr.sentence.Sentence
import edu.knowitall.tool.sentence.OpenIEExtracted
import edu.stanford.nlp.pipeline.Annotation
import edu.stanford.nlp.pipeline.StanfordCoreNLP
import java.util.Properties
import edu.stanford.nlp.ling.CoreAnnotations
import edu.stanford.nlp.ling.CoreLabel
import edu.stanford.nlp.ling.CoreAnnotations.NamedEntityTagAnnotation
import java.io.File
import scala.util.matching.Regex

trait BestEntityMentionFinder {

  def findBestEntity(entityName: String, entityType: String, begOffset: Int, docText: String, namedEntityCollection: NamedEntityCollection): String

}

trait BestEntityMentionsFound extends BestEntityMentionResolvedDocument {
  this: Document with StanfordNERAnnotated =>

  override type B = BestEntityMention

  //compute NamedEntityCollection data structure with NER over sentences
  private lazy val namedEntityCollection = {
    val organizations = getListOfNERType("ORGANIZATION")
    val locations = getListOfNERType("LOCATION")
    val people = getListOfNERType("PERSON")
    NamedEntityCollection(organizations, locations, people)
  }

  private def getListOfNERType(nerType: String) = {
    var nerTypes = List[String]()
    val sentences = scala.collection.JavaConversions.collectionAsScalaIterable(this.NERAnnotatedDoc.get(classOf[CoreAnnotations.SentencesAnnotation]))
    for (sen <- sentences) {
      val tokens = scala.collection.JavaConversions.collectionAsScalaIterable(sen.get(classOf[CoreAnnotations.TokensAnnotation]))
      var relevantTokens = List[CoreLabel]()
      var tokIndex = 0
      for (tok <- tokens) {
        val net = tok.get(classOf[NamedEntityTagAnnotation])
        tok.setNER(net)
        tok.setIndex(tokIndex)
        if (net == nerType) {
          relevantTokens = relevantTokens :+ tok
        }
        tokIndex += 1
      }
      if (!relevantTokens.isEmpty) {
        var lastTok = relevantTokens.head
        var nerToks = List(lastTok)
        for (tok <- relevantTokens.tail) {
          if (lastTok.index() == (tok.index() - 1)) {
            nerToks = nerToks :+ tok
          } else {
            val nerString = nerToks.map(f => f.originalText()).mkString(" ")
            nerTypes = nerTypes :+ nerString
            nerToks = List(tok)
          }
          lastTok = tok
        }
        if (!nerToks.isEmpty) {
          val nerString = nerToks.map(f => f.originalText()).mkString(" ")
          nerTypes = nerTypes :+ nerString
        }
      }
    }
    nerTypes.toList
  }

  private lazy val documentEntities = {
    var entities = List[Entity]()
    val sentences = scala.collection.JavaConversions.collectionAsScalaIterable(this.NERAnnotatedDoc.get(classOf[CoreAnnotations.SentencesAnnotation]))
    for (sen <- sentences) {
      val tokens = scala.collection.JavaConversions.collectionAsScalaIterable(sen.get(classOf[CoreAnnotations.TokensAnnotation]))
      var relevantTokens = List[CoreLabel]()
      var tokIndex = 0
      for (tok <- tokens) {
        val net = tok.get(classOf[NamedEntityTagAnnotation])
        tok.setNER(net)
        tok.setIndex(tokIndex)
        if (net == "ORGANIZATION" || net == "LOCATION" || net == "PERSON") {
          relevantTokens = relevantTokens :+ tok
        }
        tokIndex += 1
      }
      if (!relevantTokens.isEmpty) {
        //var nerType = relevantTokens.head.originalText()
        var lastTok = relevantTokens.head
        var nerToks = List(lastTok)
        for (tok <- relevantTokens.tail) {
          if ((lastTok.index() == (tok.index() - 1)) && (lastTok.ner() == tok.ner())) {
            nerToks = nerToks :+ tok
          } else {
            val entityString = nerToks.map(f => f.originalText()).mkString(" ")
            entities = entities :+ Entity(this.text.substring(nerToks.head.beginPosition(), nerToks.last.endPosition()), nerToks.head.beginPosition(), lastTok.ner())
            nerToks = List(tok)
          }
          lastTok = tok
        }
        if (!nerToks.isEmpty) {
          val entityString = nerToks.map(f => f.originalText()).mkString(" ")
          entities = entities :+ Entity(this.text.substring(nerToks.head.beginPosition(), nerToks.last.endPosition()), nerToks.head.beginPosition(), lastTok.ner())
        }
      }
    }
    entities.toList
  }

  val bestEntityMentionFinder: BestEntityMentionFinder

  //iterate over entities and use bestENtityMentionFinder.findBestEntity to
  // find the bestEntityMentions in the Document
  lazy val bestEntityMentions = {
    documentEntities.flatMap(entity => {
      val bestEntityString = bestEntityMentionFinder.findBestEntity(entity.name.replaceAll("\\s+", " "), entity.entityType, entity.offset, this.text, this.namedEntityCollection)
      if (bestEntityString != entity.name)
        Some(BestEntityMention(entity.name, entity.offset, bestEntityString))
      else
        None
    })
  }
}

class BestEntityMentionFinderOriginalAlgorithm extends BestEntityMentionFinder {
  println("Instantiating new BestEntityMentionFinderOriginalAlgorithm object")
  //where the custom rules should go
  override def findBestEntity(entityName: String, entityType: String, begOffset: Int, docText: String, namedEntityCollection: NamedEntityCollection): String = {
    var alternateName = entityName
    if (entityType != "None") {
      alternateName =
        entityType match {
          case "ORGANIZATION" => { findBestOrganizationString(entityName, namedEntityCollection.organizations, docText, begOffset, namedEntityCollection) }
          case "LOCATION" => {
            findBestLocationString(entityName, namedEntityCollection.locations, docText, begOffset)
          }
          case "PERSON" => { findBestPersonString(entityName, namedEntityCollection.people, docText, begOffset, true) }
        }
    }
    alternateName match {
      case entityName => {
        alternateName = findBestOrganizationString(entityName, namedEntityCollection.organizations, docText, begOffset, namedEntityCollection)
        if (alternateName == entityName) {
          alternateName = findBestLocationString(entityName, namedEntityCollection.locations, docText, begOffset)
        }
        if (alternateName == entityName) {
          alternateName = findBestPersonString(entityName, namedEntityCollection.people, docText, begOffset, false)
        }
      }
      case _ => {}
    }
    alternateName
  }

  private def sortCandidateStringsByProximity(rawDoc: String, candidateStrings: List[String], begOffset: Integer): List[String] = {
    val entityPosition = begOffset
    val uniqueCandidateMap = candidateStrings.groupBy[String](f => f)
    val candidateDistanceTuples = for (uniqueCandidate <- uniqueCandidateMap.keys) yield {
      var nextIndex = rawDoc.indexOf(uniqueCandidate)
      var minDistance = rawDoc.length()
      while (nextIndex != -1) {
        val proximity = entityPosition - nextIndex
        if (proximity > 0) {
          minDistance = math.min(minDistance, proximity)
        }
        nextIndex = rawDoc.indexOf(uniqueCandidate, nextIndex + 1)
      }
      (uniqueCandidate, minDistance)
    }
    candidateDistanceTuples.toList.sortBy(f => f._2).map(x => x._1)
  }

  private def findBestOrganizationString(originalName: String, candidateStrings: List[String], rawDoc: String, begOffset: Integer, namedEntities: NamedEntityCollection): String = {
    val originalString = originalName
    val sortedCandidateStrings = sortCandidateStringsByProximity(rawDoc, candidateStrings, begOffset)

    try {
      val accronymRegex = new Regex("\\([^\\)\\(]{0,15}" + originalString + "[^\\)\\(]{0,15}\\)")
      //if the organization is an acronym
      if (originalString.forall(p => p.isUpper) || accronymRegex.findFirstIn(rawDoc).isDefined) {

        for (cs <- sortedCandidateStrings) {
          val words = cs.split(" ").filter(p => { p(0).isUpper }).takeRight(originalString.length())
          var goodCandidate = true
          var index = 0
          if (words.length >= originalString.length()) {
            for (word <- words) {
              if (word(0) != originalString(index)) {
                goodCandidate = false
              }
              index += 1
            }
            if (goodCandidate) {
              val candidateWords = cs.split(" ")
              var index = 0
              for (cw <- candidateWords) {
                if (cw == words.head) {
                  return candidateWords.slice(index, candidateWords.length) mkString " "
                }
                index += 1
              }
            }
          }
        }

        // if in parentheses and nothing was found...
        //val parenthesisRegexPattern = new Regex("([A-Z]\\w+ (\\w+ )*[A-Z]\\w+)[\\.\\s]*\\([^\\)\\(]{0,5}"+originalString+"[^\\)\\(]{0,5}\\)")
        val accRegexPattern = new Regex("([" + originalString(0).toUpper + originalString(originalString.length() - 1).toUpper + "][\\S]+ ([\\S]+ ){0,2}[A-Z][\\S]+).{0,15}" + originalString)
        val accronymMatch = accRegexPattern.findFirstMatchIn(rawDoc)
        if (accronymMatch.isDefined) {
          var expandedString = accronymMatch.get.group(1)
          if (BestEntityMentionFinderOriginalAlgorithm.stopWords.contains(expandedString.split(" ")(0).toLowerCase())) {
            expandedString = expandedString.split(" ").drop(1).mkString(" ")
          }
          return expandedString
        }

      }
    } catch {
      case e: Exception => {

      }
    }

    //non caps organization, check if there is a longer string than the original
    //name with the original name as the rightmost word
    var probablyOrganization = true
    var originalStringIsLocation = false
    val locations = namedEntities.locations

    for (loc <- locations) {
      if (loc.contains(originalString)) {
        originalStringIsLocation = true
      }
    }

    if (originalStringIsLocation) {
      probablyOrganization = false
    }

    if (probablyOrganization) {
      //do this if original String is not refferring to a location
      for (cs <- candidateStrings) {
        val words = cs.split(" ")
        val originalWords = originalString.split(" ")
        if ((words.length > originalWords.length) &&
          ((words.takeRight(originalWords.length).mkString(" ") == originalString) ||
            (words.take(originalWords.length).mkString(" ") == originalString))) {
          return words mkString " "
        }
      }
    }

    //finally check if the original string if prefix of an organization
    for (cs <- sortedCandidateStrings) {
      if (cs.toLowerCase().startsWith(originalString.toLowerCase()) && cs.length() > originalString.length() && cs.split(" ").length == 1) {
        return cs
      }
    }

    originalString
  }

  private def locationCasing(str: String): String = {
    var words = List[String]()
    for (s <- str.split(" ")) {
      var newS = s
      if (!s.contains(".")) {
        newS = for (c <- s) yield {
          c.toLower
        }
        newS = newS(0).toUpper + newS.tail
      }
      words = words :+ newS
    }
    words mkString " "
  }

  private def expandLocation(containerLocation: String): List[String] = {

    val containerLocationPrefix = if (!containerLocation.last.isLetter) {
      containerLocation.dropRight(1)
    } else {
      containerLocation
    }
    var possibleExpansions = List[String]()

    if (containerLocationPrefix.length() > 2) {
      val stateOrProvinces = BestEntityMentionFinderOriginalAlgorithm.TipsterData.stateOrProvinces
      for (state <- stateOrProvinces) {
        if (state.startsWith(containerLocationPrefix.toLowerCase())) {
          possibleExpansions = locationCasing(state) :: possibleExpansions
        }
      }
    }
    possibleExpansions.toList
  }

  private def expandAbbreviation(str: String): String = {
    val stateAbbreviationMatch = BestEntityMentionFinderOriginalAlgorithm.stateAbbreviationPattern.findFirstMatchIn(str)
    if (stateAbbreviationMatch.isDefined) {
      val abbreviation = stateAbbreviationMatch.get.group(2).toUpperCase() +
        stateAbbreviationMatch.get.group(3).toUpperCase()
      val city = stateAbbreviationMatch.get.group(1)
      val expandedStateAbbreviation = BestEntityMentionFinderOriginalAlgorithm.TipsterData.expandStateAbbreviation(abbreviation, city)
      if (expandedStateAbbreviation.isDefined) {
        expandedStateAbbreviation.get
      } else {
        str
      }
    } else {
      //check for Mass. pattern and expand if valid
      val containedLocation = str.split(",")(0).trim()
      val containerLocation = str.split(",")(1).trim()
      val expandedLocations = expandLocation(containerLocation)
      for (expandedLocation <- expandedLocations) {
        if (locationContainsLocation(expandedLocation, containedLocation)) {
          return (containedLocation + ", " + expandedLocation)
        }
      }
      str
    }
  }
  private def findBestLocationString(originalName: String, candidateStrings: List[String], rawDoc: String, begOffset: Integer): String = {
    val originalString = originalName
    val sortedCandidateStrings = sortCandidateStringsByProximity(rawDoc, candidateStrings, begOffset)
    var candidates = List[String]()
    val originalWords = originalString.split(" ")
    for (cs <- sortedCandidateStrings) {
      val size = cs.split(" ").length
      var index = 0
      while (index < (size - 1)) {
        val words = cs.split(" ").drop(index)
        if ((words.length > (originalWords.length + 1)) &&
          (words.take(originalWords.length).mkString(" ").toLowerCase() == originalString.toLowerCase()) &&
          (words(originalWords.length) == "," || words(originalWords.length) == "in")) {
          candidates = candidates :+ words.take(originalWords.length).mkString(" ") + ", " + words.drop(originalWords.length + 1).mkString(" ")
        }
        index += 1
      }
    }
    candidates = candidates.filter(p => (p.split(" ").length < 7))
    candidates = candidates.filter(p => (isValidLocation(p)))
    if (candidates.isEmpty) {
      //check to see if state is mentioned somewhere, then build a new String with
      //that state or country
      val containerMap = scala.collection.mutable.Map[String, Int]()
      for (cs <- candidateStrings) {
        if (locationContainsLocation(cs, originalString)) {
          if (cs != originalString && cs != "United States") {
            if (containerMap.contains(cs)) {
              containerMap += ((cs, containerMap.get(cs).get + 1))
            } else {
              containerMap += ((cs, 1))
            }
          }
        }
      }
      if (containerMap.isEmpty) {
        //try  regular string searching instead of relying on Stanford NER
        val containedPlace = originalString
        val origQuote = originalString.replaceAll("\\(|\\)", "")
        val locationRegex = new Regex("(" + origQuote + "|" + origQuote.toLowerCase() + "|" + origQuote.toUpperCase() + "),\\s?([A-Z][\\S]+)[\\s\\.\\?!,]")
        val sourceText = rawDoc
        val candidates = scala.collection.mutable.Map[String, Int]()
        for (
          locationRegex(containedLoc, containerLoc) <- locationRegex.findAllMatchIn(sourceText); fullLocation = expandAbbreviation(locationCasing(containedLoc + ", " + containerLoc)).split(",");
          if locationContainsLocation(fullLocation(1).trim(), fullLocation(0).trim())
        ) {
          val containerLocation = fullLocation(1).trim()
          if (candidates.contains(containerLocation)) {
            candidates += ((containerLocation, 1 + candidates.get(containerLocation).get))
          } else {
            candidates += ((containerLocation, 1))
          }
        }
        val headTuple = candidates.toMap.toList.sortBy(f => f._2).headOption
        if (headTuple.isDefined) {
          containedPlace + ", " + headTuple.get._1
        } else {
          originalString
        }
      } else {
        //sort by distance to original string
        val containerStrings = containerMap.keys
        val sortedContainerStrings = sortCandidateStringsByProximity(rawDoc, containerStrings.toList, begOffset)
        locationCasing(originalString + ", " + sortedContainerStrings.head)
      }
    } else {
      val candidate = candidates.head
      expandAbbreviation(locationCasing(candidate))
    }
  }
  private def findBestPersonString(originalName: String, candidateStrings: List[String], docText: String, begOffset: Integer, probablyPerson: Boolean): String = {
    val originalString = originalName
    for (cs <- sortCandidateStringsByProximity(docText, candidateStrings, begOffset)) {
      val words = cs.split(" ")
      val originalWords = originalString.split(" ")
      if ((words.length > originalWords.length) &&
        ((words.takeRight(originalWords.length).mkString(" ") == originalString) ||
          (words.take(originalWords.length).mkString(" ") == originalString)) &&
          (words.length < 4)) {
        return (words mkString " ")
      }
    }

    if (probablyPerson) {
      //try a conservative name regex if nothing from Stanford NER was found
      val nameRegex = """(\.|(\s[a-z]+\s))([A-Z]\w+\s[A-Z]\w+)(\.|(\s[a-z]+\s))""".r
      val rawDoc = docText
      val nameList = for (nameMatch <- nameRegex.findAllMatchIn(rawDoc); name = nameMatch.group(3); if name.contains(originalString)) yield name
      val sortedNameList = sortCandidateStringsByProximity(docText, nameList.toList, begOffset)
      if (sortedNameList.headOption.isDefined) {
        return sortedNameList.head
      }
    }

    originalString
  }

  private def isValidLocation(locationStr: String): Boolean = {
    val placeNames = locationStr.split(",").map(f => f.trim())
    if (placeNames.length == 2) {
      return ((locationContainsLocation(placeNames(1), placeNames(0))) || (!sameLocationType(placeNames(1), placeNames(0))))
    } else {
      return false
    }
  }

  private def sameLocationType(location1: String, location2: String): Boolean = {
    val cities = BestEntityMentionFinderOriginalAlgorithm.TipsterData.cities
    val stateOrProvinces = BestEntityMentionFinderOriginalAlgorithm.TipsterData.stateOrProvinces
    val countries = BestEntityMentionFinderOriginalAlgorithm.TipsterData.countries

    if (cities.contains(location1.toLowerCase()) && cities.contains(location2.toLowerCase())) {
      return true
    }
    if (stateOrProvinces.contains(location1.toLowerCase()) && stateOrProvinces.contains(location2.toLowerCase())) {
      return true
    }
    if (countries.contains(location1.toLowerCase()) && countries.contains(location2.toLowerCase())) {
      return true
    }
    return false
  }

  private def locationContainsLocation(container: String, contained: String): Boolean = {
    val cities = BestEntityMentionFinderOriginalAlgorithm.TipsterData.cities
    val stateOrProvinces = BestEntityMentionFinderOriginalAlgorithm.TipsterData.stateOrProvinces
    val countries = BestEntityMentionFinderOriginalAlgorithm.TipsterData.countries
    val stateCityMap = BestEntityMentionFinderOriginalAlgorithm.TipsterData.provinceCityMap
    val countryCityMap = BestEntityMentionFinderOriginalAlgorithm.TipsterData.countryCityMap

    if (cities.contains(contained.toLowerCase())) {
      if (stateOrProvinces.contains(container.toLowerCase())) {
        val citySet = stateCityMap.get(locationCasing(container))
        if (citySet.isDefined) {
          if (citySet.get.contains(locationCasing(contained))) {
            return true
          }
        }
      }
      if (countries.contains(container.toLowerCase())) {
        val citySet = countryCityMap.get(locationCasing(container))
        if (citySet.isDefined) {
          if (citySet.get.contains(locationCasing(contained))) {
            return true
          }
        }
      }
    }
    return false
  }
}

object BestEntityMentionFinderOriginalAlgorithm {
  private object AbbreviationData {

    val abbreviationMap =
      Map("AL" -> "Alabama",
        "AK" -> "Alaska",
        "AZ" -> "Arizona",
        "AR" -> "Arkansas",
        "CA" -> "California",
        "CO" -> "Colorado",
        "CT" -> "Connecticut",
        "DE" -> "Delaware",
        "FL" -> "Florida",
        "GA" -> "Georgia",
        "HI" -> "Hawaii",
        "ID" -> "Idaho",
        "IL" -> "Illinois",
        "IN" -> "Indiana",
        "IA" -> "Iowa",
        "KS" -> "Kansas",
        "KY" -> "Kentucky",
        "LA" -> "Louisiana",
        "ME" -> "Maine",
        "MD" -> "Maryland",
        "MA" -> "Massachusetts",
        "MI" -> "Michigan",
        "MN" -> "Minnesota",
        "MS" -> "Mississippi",
        "MO" -> "Missouri",
        "MT" -> "Montana",
        "NE" -> "Nebraska",
        "NV" -> "Nevada",
        "NH" -> "New Hampshire",
        "NJ" -> "New Jersey",
        "NM" -> "New Mexico",
        "NY" -> "New York",
        "NC" -> "North Carolina",
        "ND" -> "North Dakota",
        "OH" -> "Ohio",
        "OK" -> "Oklahoma",
        "OR" -> "Oregon",
        "PA" -> "Pennsylvania",
        "RI" -> "Rhode Island",
        "SC" -> "South Carolina",
        "SD" -> "South Dakota",
        "TN" -> "Tennessee",
        "TX" -> "Texas",
        "UT" -> "Utah",
        "VT" -> "Vermont",
        "VA" -> "Virginia",
        "WA" -> "Washington",
        "WV" -> "West Virginia",
        "WI" -> "Wisconsin",
        "WY" -> "Wyoming")
  }

  private val stateAbbreviationPattern = """(\w+),\s([A-Za-z])\.?([A-Za-z])\.?$""".r
  private val stopWords = {
    io.Source.fromFile(new File("/scratch/usr/rbart/git/UWELExpanded/src/main/resources/edu/knowitall/entitylinking/extended/utils/stopwords.txt"))(scala.io.Codec.UTF8).getLines.flatMap(_.split(",")).map(_.toLowerCase).toSet
  }

  private object TipsterData {

    private val tipsterFile = new File("/scratch/usr/rbart/git/UWELExpanded/src/main/resources/edu/knowitall/entitylinking/extended/utils/TipsterGazetteer.txt")
    private val cityProvincePattern = """([^\(]+)\(CITY.+?([^\(\)]+)\(PROVINCE.*""".r
    private val cityCountryPattern = """([^\(]+)\(CITY.+?([^\(\)]+)\(COUNTRY.*""".r

    val citySet = scala.collection.mutable.Set[String]()
    val stateOrProvinceSet = scala.collection.mutable.Set[String]()
    val countrySet = scala.collection.mutable.Set[String]()

    val provinceCityMap = scala.collection.mutable.Map[String, Set[String]]()
    val countryCityMap = scala.collection.mutable.Map[String, Set[String]]()

    // read in tipster lines with latin encoding so as not to get errors.
    scala.io.Source.fromFile(tipsterFile.getPath())(scala.io.Codec.ISO8859).getLines.foreach(line => {
      val cityProvinceMatch = cityProvincePattern.findFirstMatchIn(line)
      if (cityProvinceMatch.isDefined) {
        val city = cityProvinceMatch.get.group(1).trim()
        val province = cityProvinceMatch.get.group(2).trim()

        if (provinceCityMap.contains(province)) {
          val oldSet = provinceCityMap.get(province)
          provinceCityMap += ((province, oldSet.get + city))
        } else {
          provinceCityMap += ((province, Set(city)))
        }
      }

      val cityCountryMatch = cityCountryPattern.findFirstMatchIn(line)
      if (cityCountryMatch.isDefined) {
        val city = cityCountryMatch.get.group(1).trim()
        val country = cityCountryMatch.get.group(2).trim()

        if (countryCityMap.contains(country)) {
          val oldSet = countryCityMap.get(country)
          countryCityMap += ((country, oldSet.get + city))
        } else {
          countryCityMap += ((country, Set(city)))
        }
      }

      val pairs = line.split("\\)")
      val pairSplits = { for (p <- pairs) yield p.split("\\(") }
      for (nameAndLocationType <- pairSplits) {
        if (nameAndLocationType.size == 2) {
          val name = nameAndLocationType(0).trim().toLowerCase()
          val locationType = nameAndLocationType(1).split(" ")(0).trim()
          locationType match {
            case "CITY" => {
              if (!citySet.contains(name)) {
                citySet.add(name)
              }
            }
            case "COUNTRY" => {
              if (!countrySet.contains(name)) {
                countrySet.add(name)
              }
            }
            case "PROVINCE" => {
              if (!stateOrProvinceSet.contains(name)) {
                stateOrProvinceSet.add(name)
              }
            }
            case _ => {}
          }
        }
      }

    })

    lazy val provinceToCityMap = provinceCityMap.toMap
    lazy val cities = citySet.toSet
    lazy val countries = countrySet.toSet
    lazy val stateOrProvinces = stateOrProvinceSet.toSet

    def main(args: Array[String]) = {
      val lcities = provinceToCityMap.get("Louisiana").get
      for (c <- lcities) {
        println(c)
      }
    }

    def expandStateAbbreviation(abr: String, city: String): Option[String] = {
      if (BestEntityMentionFinderOriginalAlgorithm.AbbreviationData.abbreviationMap.contains(abr)) {
        val stateName = BestEntityMentionFinderOriginalAlgorithm.AbbreviationData.abbreviationMap.get(abr)
        if (stateName.isEmpty) return None
        val citiesInState = provinceToCityMap.get(stateName.get)
        if (citiesInState.isEmpty) return None
        if (citiesInState.get.contains(city)) {
          println("Transforming " + abr + " to " + stateName.get)
          return Some((city + ", " + stateName.get))
        } else return None
      } else {
        None
      }
    }
  }

}

case class NamedEntityCollection(
  val organizations: List[String],
  val locations: List[String],
  val people: List[String])

case class Entity(val name: String, val offset: Int, val entityType: String)