package demo.ibm.sample;

import java.math.BigDecimal;

public class Claims {
	
	private String claimType = null;
	private BigDecimal claimAmount = null;
	private String status = null;
	private String reason = null;
	
	
	public String getClaimType()  {
		return this.claimType;
	}
	
	public void setClaimType(String claimType) {
		this.claimType = claimType;
	}
	
	public String getStatus() {
		return this.status;
	}
	
	public BigDecimal getClaimAmount() {
		return this.claimAmount;
	}
	
	public void setClaimAmount(BigDecimal claimAmount) {
		this.claimAmount = claimAmount;
	}
	
	public void setStatus(String status) {
		this.status = status;
	}
	
	public String getReason() {
		return this.reason;
	}
	
	public void setReason(String reason) {
		this.reason = reason;
	}

}

