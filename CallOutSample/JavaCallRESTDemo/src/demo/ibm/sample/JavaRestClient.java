package demo.ibm.sample;

import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URL;

import com.fasterxml.jackson.databind.ObjectMapper;

public class JavaRestClient {

    public static void main(String[] args) {
    	
        // The following is for testing purposes only, the main method is
        // not called by the COBOL program
    	String response = getClaimResult("MEDICAL", "199.99");
    	System.out.println("[" + response + "]");
    	
    	response = getClaimResult("MEDICAL", "299.99");
    	System.out.println("[" + response + "]");
    }
    
    // This is the method that is called by the COBOL program
    public static String getClaimResult(String claimType, String claimAmount) {
    	
        // Get the value of the REST API endpoint from the RESTURI and RESTPORT
        // environment variables.  This should be set before calling this method.
    	String restEndpointURL = System.getenv("RESTURI").toLowerCase().trim();
    	String restEndpointPORT = System.getenv("RESTPORT").trim();
    	
    	String httpEndpoint = "";
    	
    	if (restEndpointURL.startsWith("http://")) {
    		httpEndpoint = restEndpointURL + ":" + restEndpointPORT + "/claim/rule?claimType=" +
                           claimType.toUpperCase().trim() + "&claimAmount=" + claimAmount.trim();
    	} else {
    		httpEndpoint = "http://" + restEndpointURL + ":" + restEndpointPORT + "/claim/rule?claimType=" +
                    claimType.toUpperCase().trim() + "&claimAmount=" + claimAmount.trim();	
    	}
    	
    	System.out.println("REST API Endpoint: " + httpEndpoint);
    	String output = null;

        try {
        	// Create a URL object for the REST API Endpoint
        	URL url = new URL(httpEndpoint);

        	// Open the connection to the REST API Endpoint
        	HttpURLConnection connection = (HttpURLConnection) url.openConnection();

        	// Set the properties, headers.
        	connection.setRequestProperty("accept", "application/json");

        	// Execute the actual HTTP request
        	InputStream responseStream = connection.getInputStream();

        	// Manually converting the response body InputStream to the Claims object using Jackson
        	ObjectMapper mapper = new ObjectMapper();
        	Claims claim = mapper.readValue(responseStream, Claims.class);
        	
        	// Set output to be returned to caller
        	output = claim.getStatus() + ";" + claim.getReason();
        	
        } catch (IOException e) {
        	e.printStackTrace();
        	System.out.println(e);
        	output = "IOException occured";
        }
        
        return output;
    }
}
