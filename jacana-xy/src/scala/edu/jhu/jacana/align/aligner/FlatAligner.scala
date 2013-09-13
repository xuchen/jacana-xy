/**
 *
 */
package edu.jhu.jacana.align.aligner

import edu.jhu.jacana.align.reader.MsrReader
import edu.jhu.jacana.align.feature.AlignFeature
import edu.jhu.jacana.align.feature.StringSimilarityAlignFeature
import edu.jhu.jacana.align.feature.DistortionAlignFeature
import edu.jhu.jacana.align.AlignTrainData
import edu.jhu.jacana.align.AlignTrainRecord
import edu.jhu.jacana.align.IndexLabelAlphabet
import edu.jhu.jacana.align.util.Loggable
import edu.jhu.jacana.align.util.AlignerParams
import edu.jhu.jacana.align.crf.NestedLinearChainCRF
import edu.jhu.jacana.align.feature._
import edu.jhu.jacana.align.Alphabet
import java.io.FileInputStream
import edu.jhu.jacana.align.evaluation.AlignEvaluator
import edu.jhu.jacana.align.crf.LinearChainCRF
import edu.jhu.jacana.align.feature.fr_en.FrEnDictionaryFeature

/**
 * An aligner that takes flat input of two sentence pairs (''src'' -> ''tgt'') and align them token by token.
 * Target word positions are the hidden states of a linear chain CRF, with various features extracted
 * over pairs of tokens in ''src'' and ''tgt''.
 * 
 * sample training:
 * edu.jhu.jacana.align.aligner.FlatAligner -r alignment-data/fr-en/Hansards.french-english.train.json 
 * 				-d alignment-data/fr-en/Hansards.french-english.dev.json 
 *     			-e alignment-data/fr-en/Hansards.french-english.test.json 
 *        		-src fr -tgt en -o /tmp/fr-en.json -m /tmp/fr-en.model
 * @author Xuchen Yao
 *
 */
class FlatAligner extends AbstractFlatAligner {

	object Languages extends Enumeration {
	    type Languages = Value
	    val EN, FR = Value
	}
	import Languages._
    
    def initParams() {
         val prop = new java.util.Properties()
	     // quicker convergence
         //prop.setProperty("epsForConvergence", "0.00001")
         prop.setProperty("epsForConvergence", "0.01")
         prop.setProperty("trainer", "ll")
         if (this.phraseBased) {
             prop.setProperty("modelGraph", "semi-markov")
             prop.setProperty("segmentViterbi", "true")
         }
         
         if (configFilename != null)
	         prop.load(new FileInputStream(configFilename))
         AlignerParams.parseParameters(prop)
         
         if (srcLanguage.startsWith("fr") && tgtLanguage.startsWith("en")) {
	         FeatureExtractors =  Array[AlignFeature](DistortionAlignFeature, StringSimilarityAlignFeature, 
	                 PositionalAlignFeature, FrEnDictionaryFeature)
         } else {
             System.err.println(String.format("language pair %s->%s is not supported", srcLanguage, tgtLanguage))
             System.exit(-1)
         }
         for (f <- FeatureExtractors)
             f.init()
    }
    
    def initModel(featureAlphabet: Alphabet) {
        
         if (this.initialModelFilename != null)
             this.readModel(this.initialModelFilename)
         else {
             if (this.phraseBased) {
                 crf = new NestedLinearChainCRF(featureAlphabet, FeatureExtractors)
             } else {
                 crf = new LinearChainCRF(featureAlphabet, FeatureExtractors)
             }
         }
         if (crf.numF == 0) {
             log.fatal("Feature Count is 0! Call extractFeatures() first before initializing the model!")
             System.exit(-1)
         }
    }
    
}
  
object FlatAligner {
    
    def main(args: Array[String]): Unit = {
        
        val aligner = new FlatAligner()
        aligner.parseArguments(args)
        
        //aligner.phraseBased  = true
        
        aligner.initParams()
        var tiny = true
        
        aligner.eclipseMode = false
        
        var devData:AlignTrainData = null
        
        if (aligner.trainFilename == null && !aligner.eclipseMode) {
        	aligner.readModel(aligner.modelFilename)
        } else {
	        if (aligner.eclipseMode) {
	            aligner.transpose = false
		        if (tiny) {
		        	aligner.trainFilename = aligner.dataDirFrEn + "Hansards.french-english.train.json"
			        aligner.devFilename = aligner.dataDirFrEn + "Hansards.french-english.test.json"
		        } else {
					aligner.trainFilename = aligner.dataDirFrEn + "Hansards.french-english.train.json"
		        }
	            if (aligner.phraseBased)
	                aligner.modelFilename = "/tmp/flatPhraseAligner.model"
                else
	                aligner.modelFilename = "/tmp/flatTokenAligner.model"
	        }
	        
	        aligner.printCurrentMem("before reading train data")
	        aligner.setMemUsageStart()
			val trainData = new AlignTrainData(aligner.trainFilename, aligner.transpose, aligner.tokenize)
			AlignTrainRecord.printAlignStat()
			aligner.printConsumedMemInBetween("trainData (before feature extraction)")
			
	        val featureAlphabet = new Alphabet()
			aligner.extractFeatures(trainData, featureAlphabet)
			aligner.printConsumedMemInBetween("trainData (after feature extraction)")
			println("train data size: " + trainData.size())
			
			// can only be called *after* extracting features on the training data
			// 'cause feature extraction also sets the max possible state index.
			IndexLabelAlphabet.freeze()
	        
	        println("Total Features: " + featureAlphabet.size)
	        println("Total Labels: " + IndexLabelAlphabet.totalStates)
   	    
	        aligner.printCurrentMem("after reading train data, before reading dev data")
	        aligner.setMemUsageStart()
			devData = if (aligner.devFilename!=null) new AlignTrainData(aligner.devFilename, aligner.transpose, aligner.tokenize) else null
			
	        if (devData!=null) {
	        	AlignerParams.train = false
	        	aligner.extractFeatures(devData, featureAlphabet)
	        	AlignerParams.train = true
	        	aligner.printConsumedMemInBetween("devData (after feature extraction)")
	        	aligner.printCurrentMem("after reading dev data")
	        }
   	    
	        aligner.initModel(featureAlphabet)
	        aligner.crf.train(trainData.getTrainData, devData, aligner.modelFilename)
	        if (aligner.modelFilename != null)
	        	aligner.saveModel(aligner.modelFilename)
    	}
        
        aligner.crf.printFeatureWeights()
        
        if (aligner.alignFilename != null) {
			val s = System.nanoTime
            val total_align = aligner.decode(aligner.alignFilename, aligner.outputFilename, aligner.alignFilename2)
			val speed_in_ms = (System.nanoTime - s) *1.0/ total_align / 1e6
			val speed_in_num = 1000.0 / speed_in_ms
			println(f"Decoding time: $speed_in_ms%.2f ms per alignment ($speed_in_num%.2f alignments per seconds)")
			
        } else if (aligner.testFilename != null || aligner.eclipseMode) {
		    if (aligner.eclipseMode) {
		        if (tiny) {
		           	aligner.testFilename = "Hansards.french-english.test.json"
		        } else {
		           	aligner.testFilename = "Hansards.french-english.test.json"
		        }
		    	aligner.outputFilename = "/tmp/"+aligner.testFilename+ (if (aligner.transpose) ".t2s" else ".s2t")
		       	aligner.testFilename = aligner.dataDirFrEn + aligner.testFilename
		    }
			val testData = new AlignTrainData(aligner.testFilename, aligner.transpose, aligner.tokenize)
			
	        
			val s = System.nanoTime
	        val (total_align, total, total_non_zero, correct, correct_non_zero) = aligner.decode(testData, aligner.outputFilename)
			val speed_in_ms = (System.nanoTime - s) *1.0/ total_align / 1e6
			val speed_in_num = 1000.0 / speed_in_ms
			println(f"Decoding time: $speed_in_ms%.2f ms per alignment ($speed_in_num%.2f alignments per seconds)")
	        
	        println("Precision for all: %.2f (%d/%d)".format(correct*1.0/total, correct, total))
	        println("Precision for align: %.2f (%d/%d)".format(correct_non_zero*1.0/total_non_zero, correct_non_zero, total_non_zero))
	        
	        AlignEvaluator.evaluate(testData)
	        println("done.")
        }
    }

}