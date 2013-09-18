/**
 *
 */
package edu.jhu.jacana.align.feature.fr_en

import edu.jhu.jacana.align.feature.AlignFeature
import edu.jhu.jacana.align.AlignPair
import edu.jhu.jacana.align.resource.WiktionaryMultilingual
import edu.jhu.jacana.align.Alphabet
import edu.jhu.jacana.align.AlignFeatureVector
import edu.jhu.jacana.align.IndexLabelAlphabet.NONE_STATE

/**
 * @author Xuchen Yao
 *
 */
object FrEnDictionaryFeature extends AlignFeature {
    override def init() { 
    	WiktionaryMultilingual.init("fr")
    }
    
 	def addPhraseBasedFeature(pair: AlignPair, ins:AlignFeatureVector, i:Int, srcSpan:Int, j:Int, tgtSpan:Int, currState:Int, featureAlphabet: Alphabet){
 		if (j == -1) {
		} else {
            val srcTokens = pair.srcTokens.slice(i, i+srcSpan).mkString(" ")
           	val tgtTokens = pair.tgtTokens.slice(j, j+tgtSpan).mkString(" ")
           	
			if (WiktionaryMultilingual.exists(srcTokens, tgtTokens)) {
       	    	ins.addFeature("InWiktionary", NONE_STATE, currState, 1.0, srcSpan, featureAlphabet) 
			}
	
		}       
    }   
}