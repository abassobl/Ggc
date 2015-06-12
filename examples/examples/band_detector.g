sender{//compartiment
	@A{//attributs : att1;att2
		AHL:[][low != mid != high];
		detect:[][];
		light:[][]
	}
	@O{//observers : obs1;obs2 with obs1, obs2 : var:ag1,ag2,...
		obs1:AHL(low);
		obs2:AHL(mid);
		obs3:AHL(high)
	}
	@B{	
		[light]{detect >> AHL(low)};
		[light]{detect >> AHL(mid)};
		[light]{detect >> AHL(high)}
	}
};
receiver{//compartiment
	@A{//attributs : att1;att2
		AHL:[][low != mid != high];
		GFP:[][];
		obs1:[][];
		obs2:[][]
	}
	@O{//observers : obs1;obs2 with obs1, obs2 : var:ag1,ag2,...
		obs1:GFP;
		obs2:!GFP
	}
	@B{	
		AHL(low) >> !GFP;
		AHL(mid) >> GFP;
		AHL(high) >> !GFP
	}
}