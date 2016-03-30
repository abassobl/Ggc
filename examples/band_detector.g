sender{//compartiment
	@A{//attributs : att1;att2
		AHL:[][low != mid != high];
		detect:[][]
	}
	@O{//observers : obs1;obs2 with obs1, obs2 : var:ag1,ag2,...
		obs1:AHL(low);
		obs2:AHL(mid);
		obs3:AHL(high)
	}
	@B{//behaviours : bh1;bh2
		[light]{detect >> AHL(low)};
		[light]{detect >> AHL(mid)};
		[light]{detect >> AHL(high)}
	}
//sub compartment list
}
receiver{//compartiment
	@A{//attributs : att1;att2
		AHL:[][low != mid != high];
		GFP:[][]
	}
	@O{//observers : obs1;obs2 with obs1, obs2 : var:ag1,ag2,...
		obs1:GFP;
		obs2:!GFP
	}
	@B{//behaviours : bh1;bh2
		AHL(low) >> !GFP;
		AHL(mid) >> GFP;
		AHL(high) >> !GFP
	}
//sub compartment list
}