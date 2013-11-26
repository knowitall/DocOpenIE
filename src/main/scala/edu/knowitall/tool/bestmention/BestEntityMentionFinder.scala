package edu.knowitall.tool.bestmention

import edu.knowitall.repr.bestmention._
import edu.knowitall.repr.bestmention.EntityType.{Location, Person, Organization, Other}
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
import com.rockymadden.stringmetric.similarity.JaroWinklerMetric
import edu.knowitall.common.Resource.using

trait BestMentionFinder {

  def findBestEntity(
      entity: Entity,
      docText: String,
      namedEntityCollection: NamedEntityCollection): Option[ResolvedBestMention]
}

trait BestMentionsFound extends BestMentionResolvedDocument {
  this: Document with StanfordNERAnnotated =>

  override type B = ResolvedBestMention

  //compute NamedEntityCollection data structure with NER over sentences
  lazy val namedEntityCollection = {
    val organizations = getListOfNERType(Organization)
    val locations = getListOfNERType(Location)
    val people = getListOfNERType(Person)
    NamedEntityCollection(organizations, locations, people)
  }

  def getListOfNERType(nerType: EntityType) = documentEntities.filter(_.entityType == nerType)

  private lazy val documentEntities = {
    var entities = List[Entity]()
    val stanfordSentences = scala.collection.JavaConversions.collectionAsScalaIterable(this.NERAnnotatedDoc.get(classOf[CoreAnnotations.SentencesAnnotation]))
    for (sen <- stanfordSentences) {
      val tokens = scala.collection.JavaConversions.collectionAsScalaIterable(sen.get(classOf[CoreAnnotations.TokensAnnotation]))
      // get tokens that are either org, loc, or per.
      val relevantTokens = {
        for (
          (tok, index) <- tokens.zipWithIndex;
          net = EntityType.from(tok.get(classOf[NamedEntityTagAnnotation]))
          if (net == Organization || net == Location || net == Person)
        ) yield {
          tok.setIndex(index)
          tok
        }
      }
      // split tokens into entity objects.
      if (!relevantTokens.isEmpty) {
        var lastTok = relevantTokens.head
        var nerToks = List(lastTok)
        for (tok <- relevantTokens.tail) {
          if ((lastTok.index() == (tok.index() - 1)) && (lastTok.ner() == tok.ner())) {
            nerToks = nerToks :+ tok
          } else {
            val nerString = nerToks.map(f => f.originalText()).mkString(" ")
            val nerOffset = nerToks.map(_.beginPosition()).min
            val text = this.text.substring(nerToks.head.beginPosition(), nerToks.last.endPosition())
            entities = entities :+ Entity(text, nerOffset, nerString, EntityType.from(lastTok.ner()))
            nerToks = List(tok)
          }
          lastTok = tok
        }
        if (!nerToks.isEmpty) {
          val nerString = nerToks.map(f => f.originalText()).mkString(" ")
          val nerOffset = nerToks.map(_.beginPosition()).min
          val text = this.text.substring(nerToks.head.beginPosition(), nerToks.last.endPosition())
          entities = entities :+ Entity(text, nerOffset, nerString, EntityType.from(lastTok.ner()))
        }
      }
    }
    entities.toList
  }

  val bestMentionFinder: BestMentionFinder

  //iterate over entities and use bestMentionFinder.findBestEntity to
  // find the bestMentions in the Document
  lazy val bestMentions = {
    documentEntities.flatMap(entity => {
      bestMentionFinder.findBestEntity(entity, this.text, this.namedEntityCollection)
    })
  }
}

class BestMentionFinderOriginalAlgorithm extends BestMentionFinder {

  import BestMentionFinderOriginalAlgorithm._

  println("Instantiating new BestMentionFinderOriginalAlgorithm object")
  //where the custom rules should go
  override def findBestEntity(entity: Entity, docText: String, namedEntityCollection: NamedEntityCollection): Option[ResolvedBestMention] = {
    val bestMention: ResolvedBestMention = entity.entityType match {
      case Organization => { findBestOrganizationString(entity, namedEntityCollection.organizations, docText, namedEntityCollection) }
      case Location => {
        findBestLocationString(entity, namedEntityCollection.locations, docText)
      }
      case Person => { findBestPersonString(entity, namedEntityCollection.people, docText, true) }
      case Other => {
        val tryOrg = findBestOrganizationString(entity, namedEntityCollection.organizations, docText, namedEntityCollection)
        if (tryOrg.bestMention == entity.cleanText) {
          val tryLoc = findBestLocationString(entity, namedEntityCollection.locations, docText)
          if (tryLoc.bestMention == entity.cleanText) {
            findBestPersonString(entity, namedEntityCollection.people, docText, false)
          } else {
            tryLoc
          }
        } else {
          tryOrg
        }
      }
    }

    if (bestMention.bestMention != entity.cleanText) {
      Some(bestMention)
    } else {
      None
    }
  }

  private def sortCandidateStringsByProximity(candidateStrings: List[Entity], begOffset: Integer): List[Entity] = {

    candidateStrings.sortBy(e => math.abs(e.offset - begOffset))
  }

  private def findBestOrganizationString(
    entity: Entity,
    candidateStrings: List[Entity],
    rawDoc: String,
    namedEntities: NamedEntityCollection): ResolvedBestMention = {

    val originalString = entity.cleanText
    val sortedCandidateStrings = sortCandidateStringsByProximity(candidateStrings, entity.offset)

    val accronymRegex = new Regex("\\([^\\)\\(]{0,15}" + originalString + "[^\\)\\(]{0,15}\\)")

    //if the organization is an acronym
    if (originalString.forall(p => p.isUpper) || accronymRegex.findFirstIn(rawDoc).isDefined) {

      val acronymMatches = for (
        cs <- sortedCandidateStrings;
        words = cs.nameWords.filter(p => { p(0).isUpper }).takeRight(originalString.length());
        if (words.length >= originalString.length());
        if (!words.zipWithIndex.exists { case (word, index) => word(0) != originalString(index) });
        (cw, index) <- cs.nameWords.zipWithIndex;
        if (cw == words.head)) yield {
        cs.copy(name = cs.nameWords.drop(index).mkString(" "))
      }
      if (acronymMatches.nonEmpty) return FullResolvedBestMention(entity, acronymMatches.head, distinctNameCount(acronymMatches))


      //        // if in parentheses and nothing was found...
      //        //val parenthesisRegexPattern = new Regex("([A-Z]\\w+ (\\w+ )*[A-Z]\\w+)[\\.\\s]*\\([^\\)\\(]{0,5}"+originalString+"[^\\)\\(]{0,5}\\)")
      //        val accRegexPattern = new Regex("([" + originalString(0).toUpper + originalString(originalString.length() - 1).toUpper + "][\\S]+ ([\\S]+ ){0,2}[A-Z][\\S]+).{0,15}" + originalString)
      //        val accronymMatch = accRegexPattern.findFirstMatchIn(rawDoc)
      //        if (accronymMatch.isDefined) {
      //          var expandedString = accronymMatch.get.group(1)
      //          if (BestMentionFinderOriginalAlgorithm.stopWords.contains(expandedString.split(" ")(0).toLowerCase())) {
      //            expandedString = expandedString.split(" ").drop(1).mkString(" ")
      //          }
      //          // catch cases like "Centers for Disease Control (CDC)"... but broken right now
      //          return bestMention(originalString, begOffset, expandedString)
      //        }

    }

    //non caps organization, check if there is a longer string than the original
    //name with the original name as the rightmost word
    var probablyOrganization = true
    var originalStringIsLocation = false
    val locations = namedEntities.locations

    for (loc <- locations) {
      if (loc.name.contains(originalString)) {
        originalStringIsLocation = true
      }
    }

    if (originalStringIsLocation) {
      probablyOrganization = false
    }

    if (probablyOrganization) {
      //do this if original String is not refferring to a location
      val matches = for (
        cs <- candidateStrings;
        originalWords = originalString.split(" ");
        if ((cs.nameWords.length > originalWords.length) &&
            ((cs.nameWords.takeRight(originalWords.length).mkString(" ") == originalString) ||
                (cs.nameWords.take(originalWords.length).mkString(" ") == originalString))))
        yield cs
          // Catch cases where a candidate is a word-prefix or suffix (e.g. Centers for Disease Control => U.S. Centers for Disease Control)
          // Features:
          // proximity
          // number of possible matches (if let the whole loop run)
          // left match
          // right match
      if (matches.nonEmpty)
        return FullResolvedBestMention(entity, matches.head, distinctNameCount(matches))
    }

    // finally check if the original string if prefix of an organization
    val matches = for (cs <- sortedCandidateStrings;
         if (cs.name.toLowerCase().startsWith(originalString.toLowerCase()) &&
             cs.name.length() > originalString.length() &&
             cs.nameWords.length == 1))
      yield cs
        // check if original string is a character-prefix of a one-word candidate.
        // Feaures:
        // proximity
        // length disparity (weak)
    if (matches.nonEmpty)
        return FullResolvedBestMention(entity, matches.head, distinctNameCount(matches))

    IdentityBestMention(entity)
  }



  private def expandLocation(containerLocation: String): List[String] = {

    val containerLocationPrefix = if (!containerLocation.last.isLetter) {
      containerLocation.dropRight(1)
    } else {
      containerLocation
    }
    var possibleExpansions = List[String]()

    if (containerLocationPrefix.length() > 2) {
      val stateOrProvinces = BestMentionFinderOriginalAlgorithm.TipsterData.stateOrProvinces
      for (state <- stateOrProvinces) {
        if (state.startsWith(containerLocationPrefix.toLowerCase())) {
          possibleExpansions = locationCasing(state) :: possibleExpansions
        }
      }
    }
    possibleExpansions.toList
  }

  private def expandAbbreviation(str: String): String = {
    val stateAbbreviationMatch = BestMentionFinderOriginalAlgorithm.stateAbbreviationPattern.findFirstMatchIn(str)
    if (stateAbbreviationMatch.isDefined) {
      val abbreviation = stateAbbreviationMatch.get.group(2).toUpperCase() +
        stateAbbreviationMatch.get.group(3).toUpperCase()
      val city = stateAbbreviationMatch.get.group(1)
      val expandedStateAbbreviation = BestMentionFinderOriginalAlgorithm.TipsterData.expandStateAbbreviation(abbreviation, city)
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
  private def findBestLocationString(entity: Entity, candidateStrings: List[Entity], rawDoc: String): ResolvedBestMention = {
    val originalString = entity.cleanText
    val sortedCandidateStrings = sortCandidateStringsByProximity(candidateStrings, entity.offset)
    var candidates = List[Entity]()
    val originalWords = originalString.split(" ")
    for (cs <- sortedCandidateStrings) {
      val size = cs.nameWords.length
      var index = 0
      while (index < (size - 1)) {
        val words = cs.nameWords.drop(index)
        if ((words.length > (originalWords.length + 1)) &&
          (words.take(originalWords.length).mkString(" ").toLowerCase() == originalString.toLowerCase()) &&
          (words(originalWords.length) == "," || words(originalWords.length) == "in")) {
          // find things that look like abbreviations
          val newText = words.take(originalWords.length).mkString(" ") + ", " + words.drop(originalWords.length + 1).mkString(" ")
          candidates = candidates :+ Entity(cs.text, cs.offset, newText, Location)
        }
        index += 1
      }
    }
    candidates = candidates.filter(p => (p.nameWords.length < 7))
    candidates = candidates.filter(p => (isValidLocation(p.name)))
    if (candidates.isEmpty) {
      //check to see if state is mentioned somewhere, then build a new String with
      //that state or country
      val containerMap = scala.collection.mutable.Map[Entity, Int]()
      for (cs <- candidateStrings) {
        if (locationContainsLocation(cs.name, originalString)) {
          if (cs.name != originalString && cs.name != "United States") {
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
        val candidates = scala.collection.mutable.Map[Entity, Int]()
        val containedMap = scala.collection.mutable.Map[Entity, Entity]()
        for (
          mach <- locationRegex.findAllMatchIn(sourceText);
          locationRegex(containedLoc, containerLoc) = mach;
          containedEntity = Entity(containedLoc, mach.start(0), containedLoc, Location);
          fullLocation = expandAbbreviation(locationCasing(containedLoc + ", " + containerLoc)).split(",");
          if locationContainsLocation(fullLocation(1).trim(), fullLocation(0).trim())
        ) {
          val containerEntity = Entity(containerLoc, mach.start(1), fullLocation(1).trim(), Location)
          if (candidates.contains(containerEntity)) {
            candidates += ((containerEntity, 1 + candidates(containerEntity)))
          } else {
            candidates += ((containerEntity, 1))
            containedMap += ((containerEntity, containedEntity))
          }
        }
        val headTuple = candidates.toMap.toList.sortBy(f => f._2).headOption
        headTuple match {
          case Some((containerEntity, count)) => {
            // regex found something that looks better
            // Features:
            // proximity
            // size of "candidates" in this scope
            // containment relationship features
            // was abbreviation expanded
            ContainmentBestMention(entity, containedMap(containerEntity), containerEntity, distinctNameCount(candidates.keys.toSeq))
          }
          case None =>
            IdentityBestMention(entity)
        }
      } else {
        //sort by distance to original string
        val containers = containerMap.keys.toList
        val sortedContainerStrings = sortCandidateStringsByProximity(containers, entity.offset)
        ContainerBestMention(entity, sortedContainerStrings.head, distinctNameCount(containers))
      }
    } else {
      val candidate = candidates.head
      FullResolvedBestMention(entity, candidate.copy(name = expandAbbreviation(locationCasing(candidate.name))), distinctNameCount(candidates))
    }
  }
  private def findBestPersonString(
    entity: Entity,
    candidateStrings: List[Entity],
    docText: String,
    probablyPerson: Boolean): ResolvedBestMention = {

    val originalString = entity.cleanText
    val matches = for (cs <- sortCandidateStringsByProximity(candidateStrings, entity.offset);
         words = cs.nameWords;
         originalWords = originalString.split(" ");
         if ((words.length > originalWords.length) &&
            ((words.takeRight(originalWords.length).mkString(" ") == originalString) ||
              (words.take(originalWords.length).mkString(" ") == originalString)) &&
              (words.length < 4))) yield {
        // if 'Peterson' -> 'Scott Peterson' or 'Scott' -> 'Scott Peterson', return
        // the first case like this that we find.
        // TODO: collect all hits and decide what to do from there (e.g. bail if conflicts, or maybe return them all and let ranker sort them out?)
        // Possible features:
        // -- Proximity
        // -- Suffix match (e.g. 'Peterson' -> 'Scott Peterson')
        // -- Prefix match (e.g. 'Scott' -> 'Scott Peterson')
        // -- words.length - originalWords.length (would this be useful?)
        // -- number of candidates that make it through this filter at any proximity
        cs
    }
    if (matches.nonEmpty) return FullResolvedBestMention(entity, matches.head, distinctNameCount(matches))

    // Instead of using candidateStrings (stanford NER),
    // use a regex to search for noncap Cap Cap noncap strings
    // that contain originalString.
    // take the closest one if there is one. (how close?)
    // Possible features:
    // -- Proximity
    // -- Similarity
    // This rule actually doesn't find anything except identity strings.
    if (probablyPerson) {
      //try a conservative name regex if nothing from Stanford NER was found
      val nameRegex = """(\.|(\s[a-z]+\s))([A-Z]\w+\s[A-Z]\w+)(\.|(\s[a-z]+\s))""".r
      val rawDoc = docText
      val nameList = for (
        nameMatch <- nameRegex.findAllMatchIn(rawDoc).toList;
        name = nameMatch.group(3);
        if (name.contains(originalString))
      ) yield Entity(name, nameMatch.start(3), name, Person)
      if (nameList.nonEmpty) {
        val sortedNameList = sortCandidateStringsByProximity(nameList, entity.offset)
        return FullResolvedBestMention(entity, sortedNameList.head, distinctNameCount(nameList))
      }
    }

    IdentityBestMention(entity)
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
    val cities = BestMentionFinderOriginalAlgorithm.TipsterData.cities
    val stateOrProvinces = BestMentionFinderOriginalAlgorithm.TipsterData.stateOrProvinces
    val countries = BestMentionFinderOriginalAlgorithm.TipsterData.countries

    val l1lc = location1.toLowerCase()
    val l2lc = location2.toLowerCase()

    Seq(cities, stateOrProvinces, countries)
    .exists(set => set.contains(l1lc) && set.contains(l2lc))
  }
}

object BestMentionFinderOriginalAlgorithm {

  def distinctNameCount(entities: Seq[Entity]): Int = entities.map(_.name).distinct.size

  def locationContainsLocation(container: String, contained: String): Boolean = {
    val cities = BestMentionFinderOriginalAlgorithm.TipsterData.cities
    val stateOrProvinces = BestMentionFinderOriginalAlgorithm.TipsterData.stateOrProvinces
    val countries = BestMentionFinderOriginalAlgorithm.TipsterData.countries
    val stateCityMap = BestMentionFinderOriginalAlgorithm.TipsterData.provinceCityMap
    val countryCityMap = BestMentionFinderOriginalAlgorithm.TipsterData.countryCityMap

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

  def locationCasing(str: String): String = {
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

  object TipsterData {
    lazy val stateOrProvinces = {
      val old = OldTipsterData.stateOrProvinces
      val knew = NewTipsterData.provinceNameMap.keySet
      requireSet(old, knew)
      knew
    }
    def expandStateAbbreviation(abr: String, city: String): Option[String] = {
      val old = OldTipsterData.expandStateAbbreviation(abr, city)
      val knew = NewTipsterData.expandStateAbbreviation(abr, city)
      require(old == knew)
      knew
    }
    lazy val cities = {
      val old = OldTipsterData.cities
      val knew = NewTipsterData.cityNameMap.keySet
      requireSet(old, knew)
      knew
    }
    lazy val countries = {
      val old = OldTipsterData.countries
      val knew = NewTipsterData.countryNameMap.keySet
      requireSet(old, knew)
      knew
    }
    lazy val provinceCityMap = {
      val old = OldTipsterData.provinceCityMap.mapValues(_.toSet).toMap
      val knew = NewTipsterData.provinceCityMap.map { case (name, cities) =>
        (name, cities.map(_.name).toSet)
      }
      requireMap(old, knew)
      knew
    }
    lazy val countryCityMap = {
      val old = OldTipsterData.countryCityMap.mapValues(_.toSet).toMap
      val knew = NewTipsterData.countryCityMap.map { case (name, cities) =>
        (name, cities.map(_.name).toSet)
      }
      requireMap(old, knew)
      knew
    }
    def totalCount(s: String) = {
      val old = OldTipsterData.totalCount(s)
      val knew = NewTipsterData.totalCount(s)
      require(old > knew)
      knew
    }
    private def requireSet[T](s1: Set[T], s2: Set[T]): Unit = {
      if (!s1.equals(s2)) {
        val s1Diff = s1 &~ s2
        val s2Diff = s2 &~ s1
        System.err.println(s"old diff(${s1Diff.size}):" + s1Diff.mkString(", ").take(5))
        System.err.println(s"new diff(${s2Diff.size}):" + s2Diff.mkString(", ").take(5))
        throw new IllegalArgumentException("Sets not equal.")
      }
    }
    private def requireMap[K,V](m1: Map[K,Set[V]], m2: Map[K,Set[V]]): Unit = {
      if (!m1.equals(m2)) {
        val m1Diff = m1 -- m2.keys
        val m2Diff = m2 -- m1.keys
        def ks(k: K, vs: Set[V]) = {
          val vstr = vs.take(5).mkString(",")
          s"[$k -> $vstr](${vs.size})"
        }
        System.err.println(s"old diff(${m1Diff.size}):" + m1Diff.map(kv=>(ks(kv._1, kv._2)).mkString(", ")))
        System.err.println(s"new diff(${m2Diff.size}):" + m2Diff.map(kv=>(ks(kv._1, kv._2)).mkString(", ")))
        throw new IllegalArgumentException("Sets not equal.")
      }
    }
  }
  
  object OldTipsterData {

    private val tipsterFile = new File("/scratch/usr/rbart/git/UWELExpanded/src/main/resources/edu/knowitall/entitylinking/extended/utils/TipsterGazetteer.txt")
    private val cityProvincePattern = """([^\(]+)\(CITY.+?([^\(\)]+)\(PROVINCE.*""".r
    private val cityCountryPattern = """([^\(]+)\(CITY.+?([^\(\)]+)\(COUNTRY.*""".r

    var cityList = List[String]()
    var stateOrProvinceList = List[String]()
    var countryList = List[String]()

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
              cityList ::= name
            }
            case "COUNTRY" => {
              countryList ::= name
            }
            case "PROVINCE" => {
              stateOrProvinceList ::= name
            }
            case _ => {}
          }
        }
      }

    })

    lazy val provinceToCityMap = provinceCityMap.toMap
    lazy val cities = cityList.distinct.toSet
    lazy val countries = countryList.distinct.toSet
    lazy val stateOrProvinces = stateOrProvinceList.distinct.toSet

    lazy val cityCounts = cityList.groupBy(identity).map(p => (p._1, p._2.size))
    lazy val countryCounts = countryList.groupBy(identity).map(p => (p._1, p._2.size))
    lazy val stateOrProvinceCounts = stateOrProvinceList.groupBy(identity).map(p => (p._1, p._2.size))

    def totalCount(s: String) = cityCounts.getOrElse(s, 0) + countryCounts.getOrElse(s, 0) + stateOrProvinceCounts.getOrElse(s, 0)

    def main(args: Array[String]) = {
      val lcities = provinceToCityMap.get("Louisiana").get
      for (c <- lcities) {
        println(c)
      }
    }

    def expandStateAbbreviation(abr: String, city: String): Option[String] = {
      if (BestMentionFinderOriginalAlgorithm.AbbreviationData.abbreviationMap.contains(abr)) {
        val stateName = BestMentionFinderOriginalAlgorithm.AbbreviationData.abbreviationMap.get(abr)
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

  object NewTipsterData {

    private val tipsterFile = new File("/scratch/usr/rbart/git/UWELExpanded/src/main/resources/edu/knowitall/entitylinking/extended/utils/TipsterGazetteer.txt")
    private val cityProvincePattern = """([^\(]+)\(CITY.+?([^\(\)]+)\(PROVINCE.*""".r
    private val cityCountryPattern = """([^\(]+)\(CITY.+?([^\(\)]+)\(COUNTRY.*""".r

    sealed abstract class TipsterType(val name: String)
    object TipsterType {
      val typs = Seq(CITY, PROVINCE, COUNTRY)
      def apply(s: String) = typs.find(_.name == s).getOrElse(OTHER)
    }
    case object CITY extends TipsterType("CITY")
    case object PROVINCE extends TipsterType("PROVINCE")
    case object COUNTRY extends TipsterType("COUNTRY")
    case object OTHER extends TipsterType("OTHER")

    sealed abstract class TipsterLocation(val name: String, val typ: TipsterType)
    case class TopLocation(
      override val name: String,
      override val typ: TipsterType) extends TipsterLocation(name, typ)

    case class ContainedLocation(
      override val name: String,
      override val typ: TipsterType,
      val contName: String,
      contTyp: TipsterType) extends TipsterLocation(name, typ)

    private val tipsterRegex = ("" +
      """([^\(\)]+)""" + // the location's name, a string without parentheses (possibly with whitespace, we trim later)
      """\((CITY|PROVINCE|COUNTRY)""" + // city, province, or country - the type of the location
      """(?:\s(\d+))?\)""").r // an optional uniqueness number (and the closing paren)

    // pairs of (containee, container)
    val tipsterLocations = using(io.Source.fromFile(tipsterFile)(io.Codec.ISO8859)) { source =>
      source.getLines.flatMap { line =>
        val matches = tipsterRegex.findAllMatchIn(line).sliding(2, 1)
        matches.map {
          case Seq(m1, m2) => ContainedLocation(m1.group(1).trim(), TipsterType(m1.group(2).trim()), m2.group(1).trim(), TipsterType(m2.group(2).trim()))
          case Seq(m) => TopLocation(m.group(1).trim(), TipsterType(m.group(2).trim()))
        }
      }.toList
    }

    System.err.println(s"Loaded ${tipsterLocations.size} tipster locations.")

    def nameMap(t: TipsterType) = tipsterLocations.filter(_.typ == t).toSeq.groupBy(_.name.toLowerCase)

    val cityNameMap = nameMap(CITY)
    val provinceNameMap = nameMap(PROVINCE)
    val countryNameMap = nameMap(COUNTRY)

    def containmentMap(containerType: TipsterType, containeeType: TipsterType) = {
      tipsterLocations.collect {
        case loc @ ContainedLocation(_, containeeType, _, containerType) => loc
      }.toSeq.groupBy(_.contName)
    }

    val provinceCityMap = containmentMap(PROVINCE, CITY)

    val countryCityMap = containmentMap(COUNTRY, CITY)

    def allCitiesInProvince(provinceName: String) = provinceCityMap.getOrElse(provinceName, Nil)

    def allCitiesInCountry(countryName: String) = countryCityMap.getOrElse(countryName, Nil)

    def sameLocationType(location1: String, location2: String): Boolean = {

      val l1lc = location1.toLowerCase()
      val l2lc = location2.toLowerCase()

      Seq(cityNameMap, provinceNameMap, countryNameMap)
        .exists(map => map.contains(l1lc) && map.contains(l2lc))
    }

    def locationContainsLocation(container: String, contained: String): Boolean = {

      val cer = container.toLowerCase
      val ced = contained.toLowerCase

      // contained by a country?
      def containedByCountry = allCitiesInCountry(cer).exists(_.name.toLowerCase == ced)

      def containedByProvince = allCitiesInProvince(cer).exists(_.name.toLowerCase == ced)

      containedByCountry || containedByProvince
    }

    def totalCount(s: String) = (cityNameMap.getOrElse(s, Nil) ++ countryNameMap.getOrElse(s, Nil) ++ provinceNameMap.getOrElse(s, Nil)).size

    def main(args: Array[String]) = {
      val lcities = allCitiesInProvince("louisiana")
      for (c <- lcities) {
        println(c)
      }
    }

    def expandStateAbbreviation(abr: String, city: String): Option[String] = {
      if (BestMentionFinderOriginalAlgorithm.AbbreviationData.abbreviationMap.contains(abr)) {
        val stateName = BestMentionFinderOriginalAlgorithm.AbbreviationData.abbreviationMap.get(abr)
        if (stateName.isEmpty) return None
        val citiesInState = allCitiesInProvince(stateName.get)
        if (citiesInState.isEmpty) return None
        if (citiesInState.exists(_.name == city)) {
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
  val organizations: List[Entity],
  val locations: List[Entity],
  val people: List[Entity])
