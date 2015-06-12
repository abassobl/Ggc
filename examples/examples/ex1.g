{//compartiment
	@A{//attributs : att1;att2
		g1:[][];
		g2:[low < mid < high][];
		g3:[][st1 != st2];
		g4:[u_high > high > mid > low][up != down];
		ml1:[][];
		ml2:[][]
	}
	@O{//observers : obs1;obs2 with obs1, obs2 : var:ag1,ag2,...
		obs1:g4(up),g3(st1);
		obs2:g4(down),!g1
	}
	@B{
		[]{g2(high) >> g4(high)};
		[ml1,ml2]{g2(mid) >> g3(st1)};
		g3(st1) >* g4(up);
		g3(st2) >> g4(down),!g1
	}
}