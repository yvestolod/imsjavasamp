package demo.ims.hello.message;

import com.ibm.ims.application.IMSFieldMessage;
import com.ibm.ims.base.DLITypeInfo;

public class InputMessage extends IMSFieldMessage {

	private static final long serialVersionUID = 1L;
	static DLITypeInfo[] fieldInfo = 
		{
				// MESSAGE_TYPE is MSG1, MSG2 or MSG3
				new DLITypeInfo("MESSAGE_TYPE", DLITypeInfo.CHAR, 1, 5),
				new DLITypeInfo("DISPLAY_NAME", DLITypeInfo.CHAR, 6, 30)
		};  

	/**
	 * Required no arguments constructor
	 */
	public InputMessage()
	{ 
		super(fieldInfo, 35, false); 
	}
}
