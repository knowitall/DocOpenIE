package edu.knowitall.tool.bestmention.classifier

import edu.knowitall.tool.conf.Labelled
import edu.knowitall.repr.bestmention.ResolvedBestMention

abstract class LabelledResolvedBestMentionReader() extends Iterable[Labelled[ResolvedBestMention]] {

}