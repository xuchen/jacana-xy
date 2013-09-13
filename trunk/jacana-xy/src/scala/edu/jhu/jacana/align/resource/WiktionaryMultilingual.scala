/**
 *
 */
package edu.jhu.jacana.align.resource

import scala.collection.mutable.HashSet
import edu.jhu.jacana.util.FileManager
import edu.jhu.jacana.nlp.SnowballStemmer
import edu.jhu.jacana.nlp.SnowballStemmerFrench

/**
 * currently hard-coded to support English and French
 * @author Xuchen Yao
 *
 */
object WiktionaryMultilingual {
    
    val dict = new HashSet[String]()
    
    def init(language: String) {
        val dictPaths = Array(FileManager.getResource(String.format("resources/wiktionary/en-%s.csv.gz", language)), 
                FileManager.getResource(String.format("resources/freedict/en-%s.%s-en.dict.gz", language, language)))
        for (dictPath <- dictPaths) {
	        val reader = FileManager.getReader(dictPath)
	        var line:String = null
	        line = reader.readLine()
	        while (line != null) {
	            val splits = line.split("\t")
	            val engWord = splits(0)
	            for (foreignWord <- splits.slice(2, splits.length)) {
	                dict.add(makeKey(engWord, foreignWord))
	                dict.add(makeKey(foreignWord, engWord))
	            }
	            line = reader.readLine()
	        }
	        reader.close()
        }
    }
    
    def exists(w1:String, w2:String): Boolean = {
        return dict.apply(makeKey(w1, w2)) || dict.apply(makeKey(w2, w1))
    }
    
    def makeKey(w1:String, w2:String) = SnowballStemmer.stem(w1.toLowerCase()) + " || " + SnowballStemmerFrench.stem(w2.toLowerCase())

    def main(args: Array[String]): Unit = {
        WiktionaryMultilingual.init("fr")
        println(WiktionaryMultilingual.exists("zoom out", "dézoomer"))
        println(WiktionaryMultilingual.exists("zoom out", "dezoomer"))
        println(WiktionaryMultilingual.exists("adoptive", "adoptif"))
        println(WiktionaryMultilingual.exists("adoptive", "adoptifs"))
        println(WiktionaryMultilingual.exists("I", "je"))
        println(WiktionaryMultilingual.exists("je", "I"))
    }

}