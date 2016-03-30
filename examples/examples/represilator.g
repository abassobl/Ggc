Repressilator{//compartiment
	@A{//attributs : att1;att2
		g1:[][];
		g2:[][];
		TetR_lite:[][]
	}
	@O{//observers : obs1;obs2 with obs1, obs2 : var:ag1,ag2,...
		obs1:TetR_lite;
		obs2:!TetR_lite
	}
	@B{//behaviours : bh1;bh2
		g1 >> !g2;
		g2 >> !TetR_lite;
		TetR_lite >> !g1
	}
};
Reporter{
	@A{//attributs : att1;att2
		TetR_lite:[][];
		GFP:[][]
	}
	@O{//observers : obs1;obs2 with obs1, obs2 : var:ag1,ag2,...
		obs1:GFP;
		obs2:!GFP
	}
	@B{//behaviours : bh1;bh2
		TetR_lite >> !GFP
	}
}