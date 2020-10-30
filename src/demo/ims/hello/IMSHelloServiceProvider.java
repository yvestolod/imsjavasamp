package demo.ims.hello;

// This Java class provides the methods to build the welcome message for the
// sample Java IMS program

public class IMSHelloServiceProvider {
	
	private String displayMessage;
	private String displayName;
	private String messageType;
	
	private static final String HELLO_MSG_1 = "MSG1";
	private static final String HELLO_MSG_2 = "MSG2";
	private static final String HELLO_MSG_3 = "MSG3";
	
	public IMSHelloServiceProvider(String displayName) {
		
		this.displayName = displayName.trim().toUpperCase();
	}
	
	public void setMessageType(String messageType) {
		this.messageType = messageType.trim().toUpperCase();
	}
	
	public String getDisplayMessage(int msgID) {
		
		switch (messageType) {
			
			case HELLO_MSG_1 :
				
				displayMessage = "HI " + displayName + "! HELLO FROM JAVA IN IMS WORLD! " +
				                 "(MSG = " + msgID + ")";
				break;
				
			case HELLO_MSG_2 :
				displayMessage = "WELCOME " + displayName + "! HELLO FROM JAVA IN IMS WORLD! " +
								 "(MSG = " + msgID + ")";
				break;
				
			case HELLO_MSG_3 :
			default :
				displayMessage = "GREETINGS " + displayName + "! HELLO FROM JAVA IN IMS WORLD! " +
								 "(MSG = " + msgID + ") ";
				break;
		}
		
		return displayMessage;
	}
}
