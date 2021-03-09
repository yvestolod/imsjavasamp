package demo.ibm.sample;

import java.sql.*;

public class JavaSample {
	
	private static final String VERSION = "1.1";

	public static void main(String[] args) {
		// TODO Auto-generated method stub

		writeToStdout("demo.ibm.Sample.JavaSample (Version " + VERSION + ") called.");
		
		for (int i = 0; i < args.length; i++) {
			writeToStdout(getResponse(args[i]));
		}
	}

	public static void writeToStdout(String text) {
		
		// If you have the following in the Job or started task
		//
		// //STDOUT   DD  SYSOUT=* 
		//
		// then the output is written to the job log
		Timestamp ts = new Timestamp(System.currentTimeMillis());
		System.out.println(" " + ts + " " + text);
	}	
	
    public static String getResponse(String text) {
    	
    	String resp = "";
    	
    	resp = "The string '" + text + "' was processed.";
    	
    	return resp;
    }
	
}
