package demo.ims.hello.message;

import com.ibm.ims.application.IMSFieldMessage;
import com.ibm.ims.base.DLITypeInfo;

public class OutputMessage extends IMSFieldMessage {
	private static final long serialVersionUID = 23432884;

	static DLITypeInfo[] fieldInfo = {

			new DLITypeInfo("HELLO_MSG", DLITypeInfo.CHAR, 1, 100),
			new DLITypeInfo("ERROR_MSG", DLITypeInfo.CHAR, 101, 500)	        

	};

	public OutputMessage() {  
		super(fieldInfo, 600, false);                   
	}
}
