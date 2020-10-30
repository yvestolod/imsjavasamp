package demo.ims.hello;

import com.ibm.ims.dli.DLIException;
import com.ibm.ims.dli.tm.Application;
import com.ibm.ims.dli.tm.ApplicationFactory;
import com.ibm.ims.dli.tm.IOMessage;
import com.ibm.ims.dli.tm.MessageQueue;
import com.ibm.ims.dli.tm.Transaction;
import java.sql.Timestamp;

// This is a very simple Java program that accepts one parameter (Name) as input
// message and returns a welcome message with the input (name) included in the
// output message
//
// Invocation from IMS terminal:
//
// IMSJAVA John
//
// Result:
// Hello John, welcome to IMS Java World!
//

public class IMSHelloMain {
	
	public static int callCount = 0;
	private static final String Version = "1.1";
	
	public static void main(String[] args) {
		
		callCount++;
		
		Timestamp ts = new Timestamp(System.currentTimeMillis());

		System.out.println(" " + ts +" IMSHelloJava (Version " + Version + ") called counter = " + callCount);

		// Application is used to get a Transaction object
		Application app = ApplicationFactory.createApplication();

		// Transaction is primarily used for commit or roll back calls
		Transaction tran = app.getTransaction();

		// Get a handle to the MessageQueue object for sending and receiving
		// messages to and from the IMS message queue
		MessageQueue messageQueue = app.getMessageQueue();

		IOMessage inputMessage = null;
		IOMessage outputMessage = null;
		IMSHelloServiceProvider serviceProvider = null;
		String messageType = "";
		String displayName = "";
		
		try {

			// Initialize the input and output messages to the IOMessage object
			inputMessage = app.getIOMessage("class://demo.ims.hello.message.InputMessage");
			outputMessage = app.getIOMessage("class://demo.ims.hello.message.OutputMessage");

			// Get unique message from the message queue, if there is one
			while (messageQueue.getUnique(inputMessage)) {	

				// get the customer number from the InputMessage
				messageType = inputMessage.getString("MESSAGE_TYPE");
				displayName = inputMessage.getString("DISPLAY_NAME");
				
				serviceProvider = new IMSHelloServiceProvider(displayName);
				
				if (serviceProvider != null) {
					serviceProvider.setMessageType(messageType);
					
					outputMessage.setString("HELLO_MSG", serviceProvider.getDisplayMessage(callCount));

					// Insert the populated OutputMessage into the message queue
					messageQueue.insert(outputMessage, MessageQueue.DEFAULT_DESTINATION);					
					
					tran.commit();
				}
			}
			
		} catch (Exception e) {

			System.out.println(e.getMessage());

		try {

			if (e.getMessage().length() > 500) {
				outputMessage.setString("ERROR_MSG", e.getMessage().substring(0, 500));
			} else {
				outputMessage.setString("ERROR_MSG", e.getMessage());
			}
			messageQueue.insert(outputMessage, MessageQueue.DEFAULT_DESTINATION);

		} catch (DLIException e1) {
			System.out.println("DLIException encountered");
			System.out.println(e1.getMessage());
		}

		} finally {
			app.end();
		}
	}
}