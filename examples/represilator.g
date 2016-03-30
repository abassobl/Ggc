repressilator{
	@Observer{
		obs1:TetR;
		obs2:!TetR;
	}
	@Behavior{
		g1 >> !g2;
		g2 >> !TetR;
		TetR >> !g1;
	}
}
reporter{
	@Observer{
		obs1:GFP;
		obs2:!GFP;
	}
	@Behavior{
		TetR >> !GFP;
	}
}
