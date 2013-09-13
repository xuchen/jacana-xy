package edu.jhu.jacana.align.reader

import edu.jhu.jacana.align.AlignTrainData
import java.io.PrintWriter
import java.io.File

/**
 * Convert from MSR format to JSON format for Jacana Alignment Browser
 */
object MSR2JSON {
    
    def main(args: Array[String]) {
        var input = "alignment-data/msr/converted/RTE2_dev_M.align.txt"
        if (args.length>0) {
            input = args(0)
        }
        val json = (new AlignTrainData(input)).toJSON
        
        var output = "alignment-data/msr/converted/RTE2_dev_M.align.json" 
        if (args.length>1 ) {
            output = args(1)
        }
        val writer = new PrintWriter(new File(output))
        writer.write(json)
        writer.flush()
        writer.close()
    }    

}