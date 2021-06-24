package demo.ibm.sample;

import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URL;

import com.fasterxml.jackson.databind.ObjectMapper;

public class JavaRestClient {

    public static void main(String[] args) {
    	
    	String response = getClaimResult("MEDICAL", "199.99");
    	System.out.println("[" + response + "]");
    	
    	response = getClaimResult("MEDICAL", "299.99");
    	System.out.println("[" + response + "]");
    }
    
    public static String getClaimResult(String claimType, String claimAmount) {
    	
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
        	// Create a neat value object to hold the URL
        	URL url = new URL(httpEndpoint);

        	// Open a connection(?) on the URL(?) and cast the response(??)
        	HttpURLConnection connection = (HttpURLConnection) url.openConnection();

        	// Now it's "open", we can set the request method, headers etc.
        	connection.setRequestProperty("accept", "application/json");

        	// This line makes the request
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
