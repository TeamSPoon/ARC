
:-style_check(-discontiguous).

small(0).
small(1).
small(2).
small(3).
medium(4).
medium(5).
medium(6).
large(7).
large(8).
large(9).
large(10).


piece(0, p0_0).
cenGX(p0_0, 10).
cenGY(p0_0, 2).
size(p0_0, 8).

color(p0_0,blue).
lhs(p0_0).
piece(0, p0_1).
cenGX(p0_1, 9).
cenGY(p0_1, 1).
size(p0_1, 0).

color(p0_1,blue).
lhs(p0_1).
piece(0, p0_2).
cenGX(p0_2, 6).
cenGY(p0_2, 9).
size(p0_2, 6).

color(p0_2,green).
upright(p0_2).
piece(0, p0_3).
cenGX(p0_3, 10).
cenGY(p0_3, 6).
size(p0_3, 9).

color(p0_3,red).
lhs(p0_3).
piece(0, p0_4).
cenGX(p0_4, 10).
cenGY(p0_4, 3).
size(p0_4, 3).

color(p0_4,green).
upright(p0_4).
child(p0_0, p0_4).
child(p0_4, p0_0).
piece(1, p1_0).
cenGX(p1_0, 5).
cenGY(p1_0, 3).
size(p1_0, 10).

color(p1_0,red).
upright(p1_0).
piece(1, p1_1).
cenGX(p1_1, 7).
cenGY(p1_1, 5).
size(p1_1, 5).

color(p1_1,green).
strange(p1_1).
piece(1, p1_2).
cenGX(p1_2, 8).
cenGY(p1_2, 5).
size(p1_2, 4).

color(p1_2,red).
rhs(p1_2).
piece(1, p1_3).
cenGX(p1_3, 5).
cenGY(p1_3, 4).
size(p1_3, 9).

color(p1_3,blue).
rhs(p1_3).
child(p1_1, p1_2).
child(p1_1, p1_2).
child(p1_2, p1_1).
child(p1_2, p1_1).
child(p1_3, p1_0).
child(p1_0, p1_3).
piece(2, p2_0).
cenGX(p2_0, 6).
cenGY(p2_0, 0).
size(p2_0, 0).

color(p2_0,red).
rhs(p2_0).
piece(2, p2_1).
cenGX(p2_1, 8).
cenGY(p2_1, 1).
size(p2_1, 6).

color(p2_1,blue).
strange(p2_1).
piece(2, p2_2).
cenGX(p2_2, 8).
cenGY(p2_2, 4).
size(p2_2, 6).

color(p2_2,red).
lhs(p2_2).
piece(3, p3_0).
cenGX(p3_0, 0).
cenGY(p3_0, 10).
size(p3_0, 0).

color(p3_0,blue).
rhs(p3_0).
piece(3, p3_1).
cenGX(p3_1, 9).
cenGY(p3_1, 1).
size(p3_1, 1).

color(p3_1,red).
lhs(p3_1).
piece(3, p3_2).
cenGX(p3_2, 9).
cenGY(p3_2, 10).
size(p3_2, 7).

color(p3_2,blue).
upright(p3_2).
child(p3_0, p3_2).
child(p3_0, p3_2).
child(p3_2, p3_0).
child(p3_2, p3_0).
piece(4, p4_0).
cenGX(p4_0, 7).
cenGY(p4_0, 9).
size(p4_0, 8).

color(p4_0,green).
upright(p4_0).
piece(4, p4_1).
cenGX(p4_1, 5).
cenGY(p4_1, 5).
size(p4_1, 10).

color(p4_1,blue).
strange(p4_1).
piece(4, p4_2).
cenGX(p4_2, 0).
cenGY(p4_2, 7).
size(p4_2, 10).

color(p4_2,red).
upright(p4_2).
piece(4, p4_3).
cenGX(p4_3, 7).
cenGY(p4_3, 8).
size(p4_3, 2).

color(p4_3,blue).
rhs(p4_3).
child(p4_2, p4_3).
child(p4_2, p4_3).
child(p4_3, p4_2).
child(p4_3, p4_2).
child(p4_3, p4_0).
child(p4_0, p4_3).
piece(5, p5_0).
cenGX(p5_0, 1).
cenGY(p5_0, 1).
size(p5_0, 10).

color(p5_0,blue).
strange(p5_0).
piece(5, p5_1).
cenGX(p5_1, 1).
cenGY(p5_1, 8).
size(p5_1, 1).

color(p5_1,red).
upright(p5_1).
piece(5, p5_2).
cenGX(p5_2, 7).
cenGY(p5_2, 0).
size(p5_2, 1).

color(p5_2,green).
rhs(p5_2).
piece(5, p5_3).
cenGX(p5_3, 6).
cenGY(p5_3, 6).
size(p5_3, 7).

color(p5_3,blue).
lhs(p5_3).
piece(6, p6_0).
cenGX(p6_0, 0).
cenGY(p6_0, 6).
size(p6_0, 3).

color(p6_0,green).
strange(p6_0).
piece(6, p6_1).
cenGX(p6_1, 5).
cenGY(p6_1, 3).
size(p6_1, 3).

color(p6_1,green).
lhs(p6_1).
piece(6, p6_2).
cenGX(p6_2, 2).
cenGY(p6_2, 5).
size(p6_2, 8).

color(p6_2,red).
rhs(p6_2).
piece(6, p6_3).
cenGX(p6_3, 3).
cenGY(p6_3, 5).
size(p6_3, 4).

color(p6_3,blue).
rhs(p6_3).
piece(6, p6_4).
cenGX(p6_4, 3).
cenGY(p6_4, 8).
size(p6_4, 9).

color(p6_4,red).
strange(p6_4).
piece(7, p7_0).
cenGX(p7_0, 8).
cenGY(p7_0, 2).
size(p7_0, 10).

color(p7_0,red).
rhs(p7_0).
piece(7, p7_1).
cenGX(p7_1, 3).
cenGY(p7_1, 1).
size(p7_1, 5).

color(p7_1,blue).
upright(p7_1).
piece(7, p7_2).
cenGX(p7_2, 2).
cenGY(p7_2, 1).
size(p7_2, 10).

color(p7_2,blue).
rhs(p7_2).
child(p7_2, p7_1).
child(p7_1, p7_2).
piece(8, p8_0).
cenGX(p8_0, 8).
cenGY(p8_0, 6).
size(p8_0, 2).

color(p8_0,red).
upright(p8_0).
piece(8, p8_1).
cenGX(p8_1, 2).
cenGY(p8_1, 2).
size(p8_1, 7).

color(p8_1,red).
rhs(p8_1).
piece(8, p8_2).
cenGX(p8_2, 8).
cenGY(p8_2, 7).
size(p8_2, 8).

color(p8_2,blue).
lhs(p8_2).
child(p8_2, p8_0).
child(p8_0, p8_2).
piece(9, p9_0).
cenGX(p9_0, 10).
cenGY(p9_0, 6).
size(p9_0, 10).

color(p9_0,red).
strange(p9_0).
piece(9, p9_1).
cenGX(p9_1, 3).
cenGY(p9_1, 0).
size(p9_1, 3).

color(p9_1,blue).
rhs(p9_1).
piece(9, p9_2).
cenGX(p9_2, 0).
cenGY(p9_2, 8).
size(p9_2, 5).

color(p9_2,red).
rhs(p9_2).
piece(9, p9_3).
cenGX(p9_3, 4).
cenGY(p9_3, 0).
size(p9_3, 8).

color(p9_3,red).
strange(p9_3).
piece(9, p9_4).
cenGX(p9_4, 3).
cenGY(p9_4, 4).
size(p9_4, 2).

color(p9_4,blue).
upright(p9_4).
child(p9_1, p9_3).
child(p9_3, p9_1).
piece(10, p10_0).
cenGX(p10_0, 3).
cenGY(p10_0, 7).
size(p10_0, 6).

color(p10_0,red).
strange(p10_0).
piece(10, p10_1).
cenGX(p10_1, 7).
cenGY(p10_1, 4).
size(p10_1, 1).

color(p10_1,red).
upright(p10_1).
piece(10, p10_2).
cenGX(p10_2, 3).
cenGY(p10_2, 4).
size(p10_2, 2).

color(p10_2,blue).
upright(p10_2).
piece(11, p11_0).
cenGX(p11_0, 7).
cenGY(p11_0, 6).
size(p11_0, 7).

color(p11_0,green).
upright(p11_0).
piece(11, p11_1).
cenGX(p11_1, 1).
cenGY(p11_1, 9).
size(p11_1, 6).

color(p11_1,red).
rhs(p11_1).
piece(11, p11_2).
cenGX(p11_2, 1).
cenGY(p11_2, 1).
size(p11_2, 10).

color(p11_2,blue).
upright(p11_2).
piece(12, p12_0).
cenGX(p12_0, 4).
cenGY(p12_0, 10).
size(p12_0, 9).

color(p12_0,blue).
rhs(p12_0).
piece(12, p12_1).
cenGX(p12_1, 3).
cenGY(p12_1, 0).
size(p12_1, 0).

color(p12_1,blue).
strange(p12_1).
piece(12, p12_2).
cenGX(p12_2, 1).
cenGY(p12_2, 5).
size(p12_2, 3).

color(p12_2,red).
lhs(p12_2).
piece(12, p12_3).
cenGX(p12_3, 4).
cenGY(p12_3, 9).
size(p12_3, 8).

color(p12_3,blue).
upright(p12_3).
child(p12_0, p12_3).
child(p12_3, p12_0).
piece(13, p13_0).
cenGX(p13_0, 6).
cenGY(p13_0, 3).
size(p13_0, 9).

color(p13_0,blue).
lhs(p13_0).
piece(13, p13_1).
cenGX(p13_1, 0).
cenGY(p13_1, 5).
size(p13_1, 3).

color(p13_1,red).
rhs(p13_1).
piece(13, p13_2).
cenGX(p13_2, 7).
cenGY(p13_2, 3).
size(p13_2, 4).

color(p13_2,red).
upright(p13_2).
child(p13_0, p13_1).
child(p13_0, p13_1).
child(p13_0, p13_2).
child(p13_1, p13_0).
child(p13_1, p13_0).
child(p13_2, p13_0).
piece(14, p14_0).
cenGX(p14_0, 9).
cenGY(p14_0, 4).
size(p14_0, 10).

color(p14_0,red).
strange(p14_0).
piece(14, p14_1).
cenGX(p14_1, 7).
cenGY(p14_1, 4).
size(p14_1, 5).

color(p14_1,blue).
rhs(p14_1).
piece(14, p14_2).
cenGX(p14_2, 1).
cenGY(p14_2, 1).
size(p14_2, 2).

color(p14_2,green).
upright(p14_2).
piece(14, p14_3).
cenGX(p14_3, 5).
cenGY(p14_3, 8).
size(p14_3, 5).

color(p14_3,green).
rhs(p14_3).
piece(14, p14_4).
cenGX(p14_4, 1).
cenGY(p14_4, 2).
size(p14_4, 10).

color(p14_4,blue).
strange(p14_4).
child(p14_4, p14_2).
child(p14_2, p14_4).
piece(15, p15_0).
cenGX(p15_0, 3).
cenGY(p15_0, 3).
size(p15_0, 8).

color(p15_0,red).
rhs(p15_0).
piece(15, p15_1).
cenGX(p15_1, 5).
cenGY(p15_1, 8).
size(p15_1, 7).

color(p15_1,blue).
lhs(p15_1).
piece(15, p15_2).
cenGX(p15_2, 5).
cenGY(p15_2, 8).
size(p15_2, 2).

color(p15_2,blue).
upright(p15_2).
child(p15_1, p15_2).
child(p15_2, p15_1).
piece(16, p16_0).
cenGX(p16_0, 10).
cenGY(p16_0, 0).
size(p16_0, 5).

color(p16_0,green).
rhs(p16_0).
piece(16, p16_1).
cenGX(p16_1, -1).
cenGY(p16_1, 3).
size(p16_1, 9).

color(p16_1,blue).
upright(p16_1).
piece(16, p16_2).
cenGX(p16_2, 10).
cenGY(p16_2, 6).
size(p16_2, 7).

color(p16_2,red).
rhs(p16_2).
piece(16, p16_3).
cenGX(p16_3, 7).
cenGY(p16_3, 9).
size(p16_3, 0).

color(p16_3,red).
lhs(p16_3).
piece(16, p16_4).
cenGX(p16_4, 0).
cenGY(p16_4, 3).
size(p16_4, 9).

color(p16_4,green).
upright(p16_4).
child(p16_1, p16_4).
child(p16_1, p16_4).
child(p16_4, p16_1).
child(p16_4, p16_1).
piece(17, p17_0).
cenGX(p17_0, 5).
cenGY(p17_0, 0).
size(p17_0, 4).

color(p17_0,green).
upright(p17_0).
piece(17, p17_1).
cenGX(p17_1, 4).
cenGY(p17_1, 0).
size(p17_1, 7).

color(p17_1,blue).
rhs(p17_1).
piece(17, p17_2).
cenGX(p17_2, 2).
cenGY(p17_2, 8).
size(p17_2, 5).

color(p17_2,red).
strange(p17_2).
child(p17_1, p17_0).
child(p17_0, p17_1).
piece(18, p18_0).
cenGX(p18_0, 0).
cenGY(p18_0, 7).
size(p18_0, 1).

color(p18_0,blue).
upright(p18_0).
piece(18, p18_1).
cenGX(p18_1, 6).
cenGY(p18_1, 5).
size(p18_1, 9).

color(p18_1,red).
lhs(p18_1).
piece(18, p18_2).
cenGX(p18_2, 9).
cenGY(p18_2, 10).
size(p18_2, 10).

color(p18_2,green).
rhs(p18_2).
piece(18, p18_3).
cenGX(p18_3, 10).
cenGY(p18_3, 10).
size(p18_3, 6).

color(p18_3,red).
rhs(p18_3).
child(p18_3, p18_2).
child(p18_2, p18_3).
piece(19, p19_0).
cenGX(p19_0, 2).
cenGY(p19_0, 8).
size(p19_0, 1).

color(p19_0,blue).
strange(p19_0).
piece(19, p19_1).
cenGX(p19_1, 2).
cenGY(p19_1, 6).
size(p19_1, 10).

color(p19_1,red).
upright(p19_1).
piece(19, p19_2).
cenGX(p19_2, 4).
cenGY(p19_2, 0).
size(p19_2, 8).

color(p19_2,blue).
lhs(p19_2).
piece(19, p19_3).
cenGX(p19_3, 3).
cenGY(p19_3, 6).
size(p19_3, 9).

color(p19_3,blue).
lhs(p19_3).
piece(19, p19_4).
cenGX(p19_4, 3).
cenGY(p19_4, 5).
size(p19_4, 2).

color(p19_4,blue).
lhs(p19_4).
child(p19_3, p19_1).
child(p19_1, p19_3).
piece(20, p20_0).
cenGX(p20_0, 6).
cenGY(p20_0, 7).
size(p20_0, 7).

color(p20_0,red).
strange(p20_0).
piece(20, p20_1).
cenGX(p20_1, 7).
cenGY(p20_1, 7).
size(p20_1, 4).

color(p20_1,blue).
rhs(p20_1).
piece(20, p20_2).
cenGX(p20_2, 10).
cenGY(p20_2, 7).
size(p20_2, 0).

color(p20_2,blue).
lhs(p20_2).
child(p20_1, p20_0).
child(p20_0, p20_1).
piece(21, p21_0).
cenGX(p21_0, 3).
cenGY(p21_0, 5).
size(p21_0, 8).

color(p21_0,blue).
strange(p21_0).
piece(21, p21_1).
cenGX(p21_1, 2).
cenGY(p21_1, 4).
size(p21_1, 9).

color(p21_1,blue).
upright(p21_1).
piece(21, p21_2).
cenGX(p21_2, 2).
cenGY(p21_2, 3).
size(p21_2, 9).

color(p21_2,blue).
strange(p21_2).
piece(21, p21_3).
cenGX(p21_3, 0).
cenGY(p21_3, 10).
size(p21_3, 1).

color(p21_3,red).
rhs(p21_3).
child(p21_2, p21_1).
child(p21_1, p21_2).
piece(22, p22_0).
cenGX(p22_0, 6).
cenGY(p22_0, 5).
size(p22_0, 3).

color(p22_0,green).
upright(p22_0).
piece(22, p22_1).
cenGX(p22_1, 3).
cenGY(p22_1, 9).
size(p22_1, 0).

color(p22_1,red).
rhs(p22_1).
piece(22, p22_2).
cenGX(p22_2, 3).
cenGY(p22_2, 8).
size(p22_2, 10).

color(p22_2,red).
rhs(p22_2).
child(p22_1, p22_2).
child(p22_2, p22_1).
piece(23, p23_0).
cenGX(p23_0, 2).
cenGY(p23_0, 2).
size(p23_0, 9).

color(p23_0,blue).
upright(p23_0).
piece(23, p23_1).
cenGX(p23_1, 9).
cenGY(p23_1, 1).
size(p23_1, 7).

color(p23_1,green).
upright(p23_1).
piece(23, p23_2).
cenGX(p23_2, 10).
cenGY(p23_2, 1).
size(p23_2, 9).

color(p23_2,blue).
rhs(p23_2).
piece(23, p23_3).
cenGX(p23_3, 3).
cenGY(p23_3, 10).
size(p23_3, 5).

color(p23_3,blue).
lhs(p23_3).
piece(23, p23_4).
cenGX(p23_4, 2).
cenGY(p23_4, 8).
size(p23_4, 3).

color(p23_4,green).
upright(p23_4).
child(p23_2, p23_1).
child(p23_1, p23_2).
piece(24, p24_0).
cenGX(p24_0, 6).
cenGY(p24_0, 0).
size(p24_0, 7).

color(p24_0,blue).
upright(p24_0).
piece(24, p24_1).
cenGX(p24_1, 10).
cenGY(p24_1, 1).
size(p24_1, 2).

color(p24_1,red).
rhs(p24_1).
piece(24, p24_2).
cenGX(p24_2, 6).
cenGY(p24_2, 1).
size(p24_2, 2).

color(p24_2,blue).
upright(p24_2).
child(p24_0, p24_2).
child(p24_2, p24_0).
piece(25, p25_0).
cenGX(p25_0, 3).
cenGY(p25_0, 10).
size(p25_0, 6).

color(p25_0,blue).
lhs(p25_0).
piece(25, p25_1).
cenGX(p25_1, 3).
cenGY(p25_1, 3).
size(p25_1, 4).

color(p25_1,blue).
upright(p25_1).
piece(25, p25_2).
cenGX(p25_2, 3).
cenGY(p25_2, 5).
size(p25_2, 3).

color(p25_2,red).
rhs(p25_2).
piece(26, p26_0).
cenGX(p26_0, 7).
cenGY(p26_0, 5).
size(p26_0, 9).

color(p26_0,green).
upright(p26_0).
piece(26, p26_1).
cenGX(p26_1, 5).
cenGY(p26_1, 7).
size(p26_1, 6).

color(p26_1,blue).
strange(p26_1).
piece(26, p26_2).
cenGX(p26_2, 1).
cenGY(p26_2, 9).
size(p26_2, 8).

color(p26_2,red).
strange(p26_2).
piece(26, p26_3).
cenGX(p26_3, 1).
cenGY(p26_3, 8).
size(p26_3, 10).

color(p26_3,blue).
rhs(p26_3).
piece(26, p26_4).
cenGX(p26_4, 5).
cenGY(p26_4, 8).
size(p26_4, 6).

color(p26_4,red).
rhs(p26_4).
child(p26_1, p26_4).
child(p26_1, p26_4).
child(p26_4, p26_1).
child(p26_4, p26_1).
piece(27, p27_0).
cenGX(p27_0, 9).
cenGY(p27_0, 5).
size(p27_0, 4).

color(p27_0,blue).
rhs(p27_0).
piece(27, p27_1).
cenGX(p27_1, 7).
cenGY(p27_1, 9).
size(p27_1, 1).

color(p27_1,blue).
lhs(p27_1).
piece(27, p27_2).
cenGX(p27_2, 10).
cenGY(p27_2, 7).
size(p27_2, 0).

color(p27_2,blue).
rhs(p27_2).
piece(27, p27_3).
cenGX(p27_3, 7).
cenGY(p27_3, 0).
size(p27_3, 5).

color(p27_3,red).
upright(p27_3).
piece(28, p28_0).
cenGX(p28_0, 5).
cenGY(p28_0, 10).
size(p28_0, 9).

color(p28_0,green).
lhs(p28_0).
piece(28, p28_1).
cenGX(p28_1, 1).
cenGY(p28_1, 4).
size(p28_1, 8).

color(p28_1,blue).
rhs(p28_1).
piece(28, p28_2).
cenGX(p28_2, 0).
cenGY(p28_2, 4).
size(p28_2, 2).

color(p28_2,green).
rhs(p28_2).
piece(28, p28_3).
cenGX(p28_3, 3).
cenGY(p28_3, 1).
size(p28_3, 0).

color(p28_3,green).
rhs(p28_3).
piece(28, p28_4).
cenGX(p28_4, 8).
cenGY(p28_4, 3).
size(p28_4, 5).

color(p28_4,red).
strange(p28_4).
child(p28_2, p28_1).
child(p28_1, p28_2).
piece(29, p29_0).
cenGX(p29_0, 0).
cenGY(p29_0, 2).
size(p29_0, 9).

color(p29_0,blue).
rhs(p29_0).
piece(29, p29_1).
cenGX(p29_1, 0).
cenGY(p29_1, 2).
size(p29_1, 6).

color(p29_1,blue).
upright(p29_1).
piece(29, p29_2).
cenGX(p29_2, 9).
cenGY(p29_2, 8).
size(p29_2, 6).

color(p29_2,blue).
rhs(p29_2).
child(p29_0, p29_1).
child(p29_1, p29_0).
piece(30, p30_0).
cenGX(p30_0, 4).
cenGY(p30_0, 2).
size(p30_0, 7).

color(p30_0,blue).
rhs(p30_0).
piece(30, p30_1).
cenGX(p30_1, 6).
cenGY(p30_1, 1).
size(p30_1, 0).

color(p30_1,red).
rhs(p30_1).
piece(30, p30_2).
cenGX(p30_2, 1).
cenGY(p30_2, 1).
size(p30_2, 7).

color(p30_2,green).
strange(p30_2).
piece(30, p30_3).
cenGX(p30_3, 4).
cenGY(p30_3, 2).
size(p30_3, 4).

color(p30_3,red).
lhs(p30_3).
piece(30, p30_4).
cenGX(p30_4, 4).
cenGY(p30_4, 1).
size(p30_4, 5).

color(p30_4,red).
rhs(p30_4).
child(p30_0, p30_3).
child(p30_0, p30_3).
child(p30_0, p30_4).
child(p30_3, p30_0).
child(p30_3, p30_0).
child(p30_4, p30_0).
piece(31, p31_0).
cenGX(p31_0, 3).
cenGY(p31_0, 7).
size(p31_0, 0).

color(p31_0,red).
lhs(p31_0).
piece(31, p31_1).
cenGX(p31_1, 9).
cenGY(p31_1, 9).
size(p31_1, 1).

color(p31_1,red).
strange(p31_1).
piece(31, p31_2).
cenGX(p31_2, 0).
cenGY(p31_2, 1).
size(p31_2, 1).

color(p31_2,red).
strange(p31_2).
piece(31, p31_3).
cenGX(p31_3, 3).
cenGY(p31_3, 6).
size(p31_3, 3).

color(p31_3,blue).
rhs(p31_3).
piece(32, p32_0).
cenGX(p32_0, 9).
cenGY(p32_0, 3).
size(p32_0, 3).

color(p32_0,red).
lhs(p32_0).
piece(32, p32_1).
cenGX(p32_1, 9).
cenGY(p32_1, 1).
size(p32_1, 5).

color(p32_1,blue).
rhs(p32_1).
piece(32, p32_2).
cenGX(p32_2, 0).
cenGY(p32_2, 1).
size(p32_2, 2).

color(p32_2,green).
strange(p32_2).
piece(33, p33_0).
cenGX(p33_0, 2).
cenGY(p33_0, 2).
size(p33_0, 9).

color(p33_0,red).
rhs(p33_0).
piece(33, p33_1).
cenGX(p33_1, 2).
cenGY(p33_1, 2).
size(p33_1, 8).

color(p33_1,blue).
lhs(p33_1).
piece(33, p33_2).
cenGX(p33_2, 7).
cenGY(p33_2, 0).
size(p33_2, 8).

color(p33_2,green).
upright(p33_2).
piece(33, p33_3).
cenGX(p33_3, 8).
cenGY(p33_3, 4).
size(p33_3, 0).

color(p33_3,red).
upright(p33_3).
child(p33_0, p33_1).
child(p33_1, p33_0).
piece(34, p34_0).
cenGX(p34_0, 3).
cenGY(p34_0, 3).
size(p34_0, 4).

color(p34_0,red).
lhs(p34_0).
piece(34, p34_1).
cenGX(p34_1, 4).
cenGY(p34_1, 9).
size(p34_1, 10).

color(p34_1,blue).
upright(p34_1).
piece(34, p34_2).
cenGX(p34_2, 4).
cenGY(p34_2, 5).
size(p34_2, 10).

color(p34_2,red).
strange(p34_2).
piece(34, p34_3).
cenGX(p34_3, 9).
cenGY(p34_3, 6).
size(p34_3, 1).

color(p34_3,red).
upright(p34_3).
piece(34, p34_4).
cenGX(p34_4, 10).
cenGY(p34_4, 6).
size(p34_4, 8).

color(p34_4,blue).
rhs(p34_4).
child(p34_0, p34_4).
child(p34_0, p34_4).
child(p34_4, p34_0).
child(p34_4, p34_0).
child(p34_4, p34_3).
child(p34_3, p34_4).
piece(35, p35_0).
cenGX(p35_0, 5).
cenGY(p35_0, 9).
size(p35_0, 1).

color(p35_0,red).
rhs(p35_0).
piece(35, p35_1).
cenGX(p35_1, 8).
cenGY(p35_1, 1).
size(p35_1, 9).

color(p35_1,red).
upright(p35_1).
piece(35, p35_2).
cenGX(p35_2, 7).
cenGY(p35_2, 1).
size(p35_2, 7).

color(p35_2,blue).
upright(p35_2).
child(p35_2, p35_1).
child(p35_1, p35_2).
piece(36, p36_0).
cenGX(p36_0, 0).
cenGY(p36_0, 4).
size(p36_0, 5).

color(p36_0,red).
upright(p36_0).
piece(36, p36_1).
cenGX(p36_1, 0).
cenGY(p36_1, 8).
size(p36_1, 8).

color(p36_1,blue).
lhs(p36_1).
piece(36, p36_2).
cenGX(p36_2, 9).
cenGY(p36_2, 7).
size(p36_2, 9).

color(p36_2,red).
strange(p36_2).
piece(36, p36_3).
cenGX(p36_3, 0).
cenGY(p36_3, 8).
size(p36_3, 8).

color(p36_3,blue).
rhs(p36_3).
piece(37, p37_0).
cenGX(p37_0, 0).
cenGY(p37_0, 0).
size(p37_0, 6).

color(p37_0,green).
rhs(p37_0).
piece(37, p37_1).
cenGX(p37_1, 1).
cenGY(p37_1, 9).
size(p37_1, 1).

color(p37_1,blue).
upright(p37_1).
piece(37, p37_2).
cenGX(p37_2, 9).
cenGY(p37_2, 11).
size(p37_2, 10).

color(p37_2,green).
rhs(p37_2).
piece(37, p37_3).
cenGX(p37_3, 9).
cenGY(p37_3, 10).
size(p37_3, 9).

color(p37_3,green).
strange(p37_3).
child(p37_2, p37_3).
child(p37_3, p37_2).
piece(38, p38_0).
cenGX(p38_0, 1).
cenGY(p38_0, 7).
size(p38_0, 6).

color(p38_0,green).
upright(p38_0).
piece(38, p38_1).
cenGX(p38_1, 1).
cenGY(p38_1, 7).
size(p38_1, 10).

color(p38_1,blue).
upright(p38_1).
piece(38, p38_2).
cenGX(p38_2, 1).
cenGY(p38_2, 4).
size(p38_2, 4).

color(p38_2,red).
lhs(p38_2).
piece(38, p38_3).
cenGX(p38_3, 0).
cenGY(p38_3, 3).
size(p38_3, 7).

color(p38_3,red).
strange(p38_3).
child(p38_0, p38_1).
child(p38_0, p38_1).
child(p38_1, p38_0).
child(p38_1, p38_0).
piece(39, p39_0).
cenGX(p39_0, 4).
cenGY(p39_0, 4).
size(p39_0, 3).

color(p39_0,red).
strange(p39_0).
piece(39, p39_1).
cenGX(p39_1, 0).
cenGY(p39_1, 7).
size(p39_1, 10).

color(p39_1,red).
strange(p39_1).
piece(39, p39_2).
cenGX(p39_2, 2).
cenGY(p39_2, 3).
size(p39_2, 9).

color(p39_2,blue).
lhs(p39_2).
piece(39, p39_3).
cenGX(p39_3, 3).
cenGY(p39_3, 9).
size(p39_3, 6).

color(p39_3,green).
upright(p39_3).
piece(39, p39_4).
cenGX(p39_4, 0).
cenGY(p39_4, 8).
size(p39_4, 6).

color(p39_4,blue).
upright(p39_4).
piece(40, p40_0).
cenGX(p40_0, 8).
cenGY(p40_0, 2).
size(p40_0, 3).

color(p40_0,red).
upright(p40_0).
piece(40, p40_1).
cenGX(p40_1, 8).
cenGY(p40_1, 2).
size(p40_1, 7).

color(p40_1,blue).
upright(p40_1).
piece(40, p40_2).
cenGX(p40_2, 10).
cenGY(p40_2, 5).
size(p40_2, 3).

color(p40_2,red).
upright(p40_2).
child(p40_1, p40_0).
child(p40_0, p40_1).
piece(41, p41_0).
cenGX(p41_0, 2).
cenGY(p41_0, 7).
size(p41_0, 8).

color(p41_0,blue).
rhs(p41_0).
piece(41, p41_1).
cenGX(p41_1, 2).
cenGY(p41_1, 0).
size(p41_1, 7).

color(p41_1,red).
rhs(p41_1).
piece(41, p41_2).
cenGX(p41_2, 4).
cenGY(p41_2, 3).
size(p41_2, 9).

color(p41_2,red).
rhs(p41_2).
piece(42, p42_0).
cenGX(p42_0, 0).
cenGY(p42_0, 5).
size(p42_0, 8).

color(p42_0,blue).
upright(p42_0).
piece(42, p42_1).
cenGX(p42_1, 0).
cenGY(p42_1, 0).
size(p42_1, 2).

color(p42_1,red).
lhs(p42_1).
piece(42, p42_2).
cenGX(p42_2, -1).
cenGY(p42_2, 0).
size(p42_2, 9).

color(p42_2,blue).
upright(p42_2).
piece(42, p42_3).
cenGX(p42_3, 7).
cenGY(p42_3, 6).
size(p42_3, 10).

color(p42_3,blue).
upright(p42_3).
piece(42, p42_4).
cenGX(p42_4, 0).
cenGY(p42_4, 0).
size(p42_4, 10).

color(p42_4,blue).
upright(p42_4).
child(p42_1, p42_4).
child(p42_1, p42_4).
child(p42_4, p42_1).
child(p42_4, p42_1).
child(p42_4, p42_2).
child(p42_2, p42_4).
piece(43, p43_0).
cenGX(p43_0, 6).
cenGY(p43_0, 2).
size(p43_0, 10).

color(p43_0,red).
upright(p43_0).
piece(43, p43_1).
cenGX(p43_1, 0).
cenGY(p43_1, 8).
size(p43_1, 3).

color(p43_1,red).
rhs(p43_1).
piece(43, p43_2).
cenGX(p43_2, 3).
cenGY(p43_2, 6).
size(p43_2, 1).

color(p43_2,green).
upright(p43_2).
piece(43, p43_3).
cenGX(p43_3, 0).
cenGY(p43_3, 10).
size(p43_3, 3).

color(p43_3,blue).
upright(p43_3).
piece(43, p43_4).
cenGX(p43_4, 6).
cenGY(p43_4, 5).
size(p43_4, 6).

color(p43_4,red).
strange(p43_4).
piece(44, p44_0).
cenGX(p44_0, 5).
cenGY(p44_0, 10).
size(p44_0, 7).

color(p44_0,green).
upright(p44_0).
piece(44, p44_1).
cenGX(p44_1, 8).
cenGY(p44_1, 7).
size(p44_1, 7).

color(p44_1,blue).
rhs(p44_1).
piece(44, p44_2).
cenGX(p44_2, 0).
cenGY(p44_2, 8).
size(p44_2, 0).

color(p44_2,blue).
upright(p44_2).
piece(44, p44_3).
cenGX(p44_3, 9).
cenGY(p44_3, 7).
size(p44_3, 10).

color(p44_3,green).
lhs(p44_3).
child(p44_1, p44_3).
child(p44_3, p44_1).
piece(45, p45_0).
cenGX(p45_0, 7).
cenGY(p45_0, 8).
size(p45_0, 1).

color(p45_0,red).
rhs(p45_0).
piece(45, p45_1).
cenGX(p45_1, 5).
cenGY(p45_1, 0).
size(p45_1, 4).

color(p45_1,green).
strange(p45_1).
piece(45, p45_2).
cenGX(p45_2, 3).
cenGY(p45_2, 2).
size(p45_2, 0).

color(p45_2,red).
rhs(p45_2).
piece(45, p45_3).
cenGX(p45_3, 6).
cenGY(p45_3, 10).
size(p45_3, 7).

color(p45_3,blue).
strange(p45_3).
piece(45, p45_4).
cenGX(p45_4, 5).
cenGY(p45_4, 10).
size(p45_4, 7).

color(p45_4,red).
upright(p45_4).
child(p45_3, p45_4).
child(p45_4, p45_3).
piece(46, p46_0).
cenGX(p46_0, 9).
cenGY(p46_0, 6).
size(p46_0, 4).

color(p46_0,green).
strange(p46_0).
piece(46, p46_1).
cenGX(p46_1, 8).
cenGY(p46_1, 7).
size(p46_1, 0).

color(p46_1,blue).
lhs(p46_1).
piece(46, p46_2).
cenGX(p46_2, 5).
cenGY(p46_2, 1).
size(p46_2, 8).

color(p46_2,blue).
upright(p46_2).
piece(46, p46_3).
cenGX(p46_3, 9).
cenGY(p46_3, 3).
size(p46_3, 4).

color(p46_3,green).
strange(p46_3).
piece(46, p46_4).
cenGX(p46_4, 5).
cenGY(p46_4, 1).
size(p46_4, 2).

color(p46_4,green).
upright(p46_4).
child(p46_2, p46_4).
child(p46_4, p46_2).
piece(47, p47_0).
cenGX(p47_0, 9).
cenGY(p47_0, 10).
size(p47_0, 7).

color(p47_0,red).
upright(p47_0).
piece(47, p47_1).
cenGX(p47_1, 3).
cenGY(p47_1, 1).
size(p47_1, 10).

color(p47_1,green).
upright(p47_1).
piece(47, p47_2).
cenGX(p47_2, 1).
cenGY(p47_2, 2).
size(p47_2, 4).

color(p47_2,red).
lhs(p47_2).
piece(47, p47_3).
cenGX(p47_3, 4).
cenGY(p47_3, 1).
size(p47_3, 7).

color(p47_3,blue).
lhs(p47_3).
piece(47, p47_4).
cenGX(p47_4, 5).
cenGY(p47_4, 8).
size(p47_4, 9).

color(p47_4,red).
rhs(p47_4).
child(p47_3, p47_1).
child(p47_1, p47_3).
piece(48, p48_0).
cenGX(p48_0, 6).
cenGY(p48_0, 6).
size(p48_0, 10).

color(p48_0,blue).
strange(p48_0).
piece(48, p48_1).
cenGX(p48_1, 7).
cenGY(p48_1, 6).
size(p48_1, 2).

color(p48_1,green).
upright(p48_1).
piece(48, p48_2).
cenGX(p48_2, 1).
cenGY(p48_2, 10).
size(p48_2, 4).

color(p48_2,red).
lhs(p48_2).
child(p48_0, p48_1).
child(p48_1, p48_0).
piece(49, p49_0).
cenGX(p49_0, 9).
cenGY(p49_0, 10).
size(p49_0, 8).

color(p49_0,red).
rhs(p49_0).
piece(49, p49_1).
cenGX(p49_1, 0).
cenGY(p49_1, 2).
size(p49_1, 1).

color(p49_1,blue).
lhs(p49_1).
piece(49, p49_2).
cenGX(p49_2, 4).
cenGY(p49_2, 9).
size(p49_2, 10).

color(p49_2,blue).
rhs(p49_2).
piece(49, p49_3).
cenGX(p49_3, 4).
cenGY(p49_3, 8).
size(p49_3, 10).

color(p49_3,red).
lhs(p49_3).
piece(50, p50_0).
cenGX(p50_0, 2).
cenGY(p50_0, 0).
size(p50_0, 7).

color(p50_0,blue).
rhs(p50_0).
piece(50, p50_1).
cenGX(p50_1, 4).
cenGY(p50_1, 7).
size(p50_1, 7).

color(p50_1,blue).
rhs(p50_1).
piece(51, p51_0).
cenGX(p51_0, 4).
cenGY(p51_0, 7).
size(p51_0, 8).

color(p51_0,green).
upright(p51_0).
piece(51, p51_1).
cenGX(p51_1, 1).
cenGY(p51_1, 9).
size(p51_1, 4).

color(p51_1,red).
strange(p51_1).
piece(51, p51_2).
cenGX(p51_2, 2).
cenGY(p51_2, 3).
size(p51_2, 8).

color(p51_2,green).
rhs(p51_2).
piece(52, p52_0).
cenGX(p52_0, 2).
cenGY(p52_0, 7).
size(p52_0, 3).

color(p52_0,red).
upright(p52_0).
piece(52, p52_1).
cenGX(p52_1, 6).
cenGY(p52_1, 10).
size(p52_1, 2).

color(p52_1,green).
lhs(p52_1).
piece(52, p52_2).
cenGX(p52_2, 9).
cenGY(p52_2, 3).
size(p52_2, 10).

color(p52_2,blue).
strange(p52_2).
piece(52, p52_3).
cenGX(p52_3, 5).
cenGY(p52_3, 4).
size(p52_3, 7).

color(p52_3,blue).
rhs(p52_3).
piece(53, p53_0).
cenGX(p53_0, 3).
cenGY(p53_0, 0).
size(p53_0, 4).

color(p53_0,green).
upright(p53_0).
piece(53, p53_1).
cenGX(p53_1, 4).
cenGY(p53_1, 3).
size(p53_1, 0).

color(p53_1,red).
lhs(p53_1).
piece(53, p53_2).
cenGX(p53_2, 4).
cenGY(p53_2, 6).
size(p53_2, 1).

color(p53_2,green).
rhs(p53_2).
piece(53, p53_3).
cenGX(p53_3, 3).
cenGY(p53_3, 5).
size(p53_3, 2).

color(p53_3,blue).
upright(p53_3).
piece(54, p54_0).
cenGX(p54_0, 4).
cenGY(p54_0, 2).
size(p54_0, 10).

color(p54_0,blue).
rhs(p54_0).
piece(54, p54_1).
cenGX(p54_1, 2).
cenGY(p54_1, 7).
size(p54_1, 6).

color(p54_1,blue).
lhs(p54_1).
piece(54, p54_2).
cenGX(p54_2, 1).
cenGY(p54_2, 10).
size(p54_2, 9).

color(p54_2,green).
upright(p54_2).
piece(54, p54_3).
cenGX(p54_3, 0).
cenGY(p54_3, 3).
size(p54_3, 8).

color(p54_3,blue).
strange(p54_3).
piece(55, p55_0).
cenGX(p55_0, 4).
cenGY(p55_0, 4).
size(p55_0, 7).

color(p55_0,blue).
strange(p55_0).
piece(55, p55_1).
cenGX(p55_1, 10).
cenGY(p55_1, 2).
size(p55_1, 2).

color(p55_1,green).
upright(p55_1).
piece(55, p55_2).
cenGX(p55_2, 8).
cenGY(p55_2, 3).
size(p55_2, 3).

color(p55_2,blue).
strange(p55_2).
piece(55, p55_3).
cenGX(p55_3, 1).
cenGY(p55_3, 2).
size(p55_3, 10).

color(p55_3,blue).
strange(p55_3).
piece(55, p55_4).
cenGX(p55_4, 3).
cenGY(p55_4, 2).
size(p55_4, 4).

color(p55_4,green).
rhs(p55_4).
piece(56, p56_0).
cenGX(p56_0, 4).
cenGY(p56_0, 5).
size(p56_0, 3).

color(p56_0,green).
upright(p56_0).
piece(56, p56_1).
cenGX(p56_1, 6).
cenGY(p56_1, 3).
size(p56_1, 4).

color(p56_1,green).
strange(p56_1).
piece(56, p56_2).
cenGX(p56_2, 9).
cenGY(p56_2, 2).
size(p56_2, 1).

color(p56_2,green).
lhs(p56_2).
piece(56, p56_3).
cenGX(p56_3, 2).
cenGY(p56_3, 0).
size(p56_3, 5).

color(p56_3,green).
strange(p56_3).
piece(57, p57_0).
cenGX(p57_0, 5).
cenGY(p57_0, 7).
size(p57_0, 1).

color(p57_0,green).
upright(p57_0).
piece(57, p57_1).
cenGX(p57_1, 8).
cenGY(p57_1, 6).
size(p57_1, 3).

color(p57_1,blue).
rhs(p57_1).
piece(57, p57_2).
cenGX(p57_2, 9).
cenGY(p57_2, 0).
size(p57_2, 2).

color(p57_2,blue).
lhs(p57_2).
piece(57, p57_3).
cenGX(p57_3, 7).
cenGY(p57_3, 0).
size(p57_3, 5).

color(p57_3,blue).
rhs(p57_3).
piece(58, p58_0).
cenGX(p58_0, 5).
cenGY(p58_0, 4).
size(p58_0, 2).

color(p58_0,red).
lhs(p58_0).
piece(58, p58_1).
cenGX(p58_1, 9).
cenGY(p58_1, 10).
size(p58_1, 2).

color(p58_1,blue).
upright(p58_1).
piece(58, p58_2).
cenGX(p58_2, 0).
cenGY(p58_2, 3).
size(p58_2, 1).

color(p58_2,green).
rhs(p58_2).
piece(58, p58_3).
cenGX(p58_3, 10).
cenGY(p58_3, 1).
size(p58_3, 8).

color(p58_3,red).
rhs(p58_3).
piece(58, p58_4).
cenGX(p58_4, 1).
cenGY(p58_4, 6).
size(p58_4, 8).

color(p58_4,blue).
strange(p58_4).
piece(59, p59_0).
cenGX(p59_0, 0).
cenGY(p59_0, 4).
size(p59_0, 4).

color(p59_0,blue).
upright(p59_0).
piece(59, p59_1).
cenGX(p59_1, 10).
cenGY(p59_1, 7).
size(p59_1, 6).

color(p59_1,blue).
lhs(p59_1).
piece(59, p59_2).
cenGX(p59_2, 9).
cenGY(p59_2, 4).
size(p59_2, 7).

color(p59_2,green).
strange(p59_2).
piece(59, p59_3).
cenGX(p59_3, 2).
cenGY(p59_3, 1).
size(p59_3, 2).

color(p59_3,green).
lhs(p59_3).
piece(59, p59_4).
cenGX(p59_4, 7).
cenGY(p59_4, 7).
size(p59_4, 5).

color(p59_4,green).
rhs(p59_4).
piece(60, p60_0).
cenGX(p60_0, 3).
cenGY(p60_0, 2).
size(p60_0, 10).

color(p60_0,green).
upright(p60_0).
piece(60, p60_1).
cenGX(p60_1, 3).
cenGY(p60_1, 1).
size(p60_1, 9).

color(p60_1,green).
strange(p60_1).
piece(60, p60_2).
cenGX(p60_2, 10).
cenGY(p60_2, 3).
size(p60_2, 6).

color(p60_2,blue).
upright(p60_2).
piece(60, p60_3).
cenGX(p60_3, 0).
cenGY(p60_3, 0).
size(p60_3, 3).

color(p60_3,green).
lhs(p60_3).
piece(60, p60_4).
cenGX(p60_4, 0).
cenGY(p60_4, 4).
size(p60_4, 9).

color(p60_4,red).
upright(p60_4).
child(p60_0, p60_1).
child(p60_0, p60_1).
child(p60_1, p60_0).
child(p60_1, p60_0).
piece(61, p61_0).
cenGX(p61_0, 6).
cenGY(p61_0, 1).
size(p61_0, 1).

color(p61_0,blue).
upright(p61_0).
piece(61, p61_1).
cenGX(p61_1, 2).
cenGY(p61_1, 0).
size(p61_1, 0).

color(p61_1,blue).
strange(p61_1).
piece(61, p61_2).
cenGX(p61_2, 3).
cenGY(p61_2, 1).
size(p61_2, 3).

color(p61_2,red).
lhs(p61_2).
piece(61, p61_3).
cenGX(p61_3, 7).
cenGY(p61_3, 6).
size(p61_3, 4).

color(p61_3,blue).
strange(p61_3).
piece(61, p61_4).
cenGX(p61_4, 7).
cenGY(p61_4, 5).
size(p61_4, 2).

color(p61_4,green).
strange(p61_4).
child(p61_3, p61_4).
child(p61_3, p61_4).
child(p61_4, p61_3).
child(p61_4, p61_3).
piece(62, p62_0).
cenGX(p62_0, 1).
cenGY(p62_0, 3).
size(p62_0, 3).

color(p62_0,blue).
upright(p62_0).
piece(62, p62_1).
cenGX(p62_1, 5).
cenGY(p62_1, 0).
size(p62_1, 2).

color(p62_1,blue).
lhs(p62_1).
piece(62, p62_2).
cenGX(p62_2, 2).
cenGY(p62_2, 3).
size(p62_2, 9).

color(p62_2,green).
lhs(p62_2).
child(p62_0, p62_2).
child(p62_0, p62_2).
child(p62_2, p62_0).
child(p62_2, p62_0).
piece(63, p63_0).
cenGX(p63_0, 3).
cenGY(p63_0, 0).
size(p63_0, 9).

color(p63_0,green).
strange(p63_0).
piece(63, p63_1).
cenGX(p63_1, 5).
cenGY(p63_1, 10).
size(p63_1, 9).

color(p63_1,blue).
strange(p63_1).
piece(63, p63_2).
cenGX(p63_2, 9).
cenGY(p63_2, 0).
size(p63_2, 0).

color(p63_2,red).
strange(p63_2).
piece(63, p63_3).
cenGX(p63_3, 2).
cenGY(p63_3, 5).
size(p63_3, 5).

color(p63_3,red).
strange(p63_3).
piece(63, p63_4).
cenGX(p63_4, 10).
cenGY(p63_4, 6).
size(p63_4, 1).

color(p63_4,green).
lhs(p63_4).
piece(64, p64_0).
cenGX(p64_0, 5).
cenGY(p64_0, 9).
size(p64_0, 6).

color(p64_0,red).
strange(p64_0).
piece(64, p64_1).
cenGX(p64_1, 0).
cenGY(p64_1, 8).
size(p64_1, 7).

color(p64_1,red).
upright(p64_1).
piece(64, p64_2).
cenGX(p64_2, 6).
cenGY(p64_2, 4).
size(p64_2, 2).

color(p64_2,blue).
strange(p64_2).
piece(64, p64_3).
cenGX(p64_3, 1).
cenGY(p64_3, 0).
size(p64_3, 8).

color(p64_3,green).
strange(p64_3).
piece(65, p65_0).
cenGX(p65_0, 5).
cenGY(p65_0, 6).
size(p65_0, 9).

color(p65_0,blue).
lhs(p65_0).
piece(65, p65_1).
cenGX(p65_1, 1).
cenGY(p65_1, 8).
size(p65_1, 2).

color(p65_1,green).
strange(p65_1).
piece(65, p65_2).
cenGX(p65_2, 3).
cenGY(p65_2, 0).
size(p65_2, 7).

color(p65_2,blue).
rhs(p65_2).
piece(65, p65_3).
cenGX(p65_3, 1).
cenGY(p65_3, 2).
size(p65_3, 8).

color(p65_3,red).
upright(p65_3).
piece(65, p65_4).
cenGX(p65_4, 4).
cenGY(p65_4, 8).
size(p65_4, 2).

color(p65_4,blue).
upright(p65_4).
piece(66, p66_0).
cenGX(p66_0, 6).
cenGY(p66_0, 8).
size(p66_0, 6).

color(p66_0,red).
lhs(p66_0).
piece(66, p66_1).
cenGX(p66_1, 5).
cenGY(p66_1, 10).
size(p66_1, 9).

color(p66_1,red).
upright(p66_1).
piece(66, p66_2).
cenGX(p66_2, 8).
cenGY(p66_2, 6).
size(p66_2, 5).

color(p66_2,red).
rhs(p66_2).
piece(66, p66_3).
cenGX(p66_3, 10).
cenGY(p66_3, 5).
size(p66_3, 4).

color(p66_3,blue).
lhs(p66_3).
piece(67, p67_0).
cenGX(p67_0, 2).
cenGY(p67_0, 8).
size(p67_0, 1).

color(p67_0,red).
strange(p67_0).
piece(67, p67_1).
cenGX(p67_1, 7).
cenGY(p67_1, 8).
size(p67_1, 7).

color(p67_1,red).
upright(p67_1).
piece(67, p67_2).
cenGX(p67_2, 5).
cenGY(p67_2, 3).
size(p67_2, 9).

color(p67_2,green).
strange(p67_2).
piece(67, p67_3).
cenGX(p67_3, 10).
cenGY(p67_3, 1).
size(p67_3, 1).

color(p67_3,blue).
lhs(p67_3).
piece(67, p67_4).
cenGX(p67_4, 4).
cenGY(p67_4, 0).
size(p67_4, 4).

color(p67_4,red).
strange(p67_4).
piece(68, p68_0).
cenGX(p68_0, 5).
cenGY(p68_0, 5).
size(p68_0, 4).

color(p68_0,blue).
lhs(p68_0).
piece(68, p68_1).
cenGX(p68_1, 3).
cenGY(p68_1, 4).
size(p68_1, 2).

color(p68_1,blue).
upright(p68_1).
piece(69, p69_0).
cenGX(p69_0, 5).
cenGY(p69_0, 6).
size(p69_0, 10).

color(p69_0,blue).
strange(p69_0).
piece(69, p69_1).
cenGX(p69_1, 9).
cenGY(p69_1, 8).
size(p69_1, 7).

color(p69_1,blue).
strange(p69_1).
piece(69, p69_2).
cenGX(p69_2, 6).
cenGY(p69_2, 5).
size(p69_2, 8).

color(p69_2,green).
lhs(p69_2).
piece(69, p69_3).
cenGX(p69_3, 10).
cenGY(p69_3, 2).
size(p69_3, 9).

color(p69_3,green).
upright(p69_3).
piece(69, p69_4).
cenGX(p69_4, 0).
cenGY(p69_4, 6).
size(p69_4, 3).

color(p69_4,red).
upright(p69_4).
piece(70, p70_0).
cenGX(p70_0, 0).
cenGY(p70_0, 0).
size(p70_0, 10).

color(p70_0,blue).
lhs(p70_0).
piece(71, p71_0).
cenGX(p71_0, 7).
cenGY(p71_0, 7).
size(p71_0, 0).

color(p71_0,blue).
upright(p71_0).
piece(71, p71_1).
cenGX(p71_1, 5).
cenGY(p71_1, 8).
size(p71_1, 10).

color(p71_1,blue).
rhs(p71_1).
piece(71, p71_2).
cenGX(p71_2, 6).
cenGY(p71_2, 2).
size(p71_2, 4).

color(p71_2,red).
lhs(p71_2).
piece(71, p71_3).
cenGX(p71_3, 1).
cenGY(p71_3, 9).
size(p71_3, 4).

color(p71_3,green).
upright(p71_3).
piece(71, p71_4).
cenGX(p71_4, 9).
cenGY(p71_4, 4).
size(p71_4, 2).

color(p71_4,red).
strange(p71_4).
piece(72, p72_0).
cenGX(p72_0, 2).
cenGY(p72_0, 8).
size(p72_0, 2).

color(p72_0,green).
strange(p72_0).
piece(72, p72_1).
cenGX(p72_1, 7).
cenGY(p72_1, 2).
size(p72_1, 1).

color(p72_1,green).
lhs(p72_1).
piece(73, p73_0).
cenGX(p73_0, 1).
cenGY(p73_0, 3).
size(p73_0, 8).

color(p73_0,blue).
lhs(p73_0).
piece(73, p73_1).
cenGX(p73_1, 4).
cenGY(p73_1, 4).
size(p73_1, 0).

color(p73_1,green).
strange(p73_1).
piece(74, p74_0).
cenGX(p74_0, 1).
cenGY(p74_0, 6).
size(p74_0, 0).

color(p74_0,red).
rhs(p74_0).
piece(74, p74_1).
cenGX(p74_1, 8).
cenGY(p74_1, 6).
size(p74_1, 3).

color(p74_1,red).
lhs(p74_1).
piece(75, p75_0).
cenGX(p75_0, 4).
cenGY(p75_0, 6).
size(p75_0, 2).

color(p75_0,green).
upright(p75_0).
piece(75, p75_1).
cenGX(p75_1, 0).
cenGY(p75_1, 5).
size(p75_1, 0).

color(p75_1,blue).
rhs(p75_1).
piece(75, p75_2).
cenGX(p75_2, 10).
cenGY(p75_2, 1).
size(p75_2, 1).

color(p75_2,red).
upright(p75_2).
piece(75, p75_3).
cenGX(p75_3, 3).
cenGY(p75_3, 1).
size(p75_3, 3).

color(p75_3,green).
strange(p75_3).
piece(75, p75_4).
cenGX(p75_4, 1).
cenGY(p75_4, 7).
size(p75_4, 6).

color(p75_4,blue).
strange(p75_4).
piece(76, p76_0).
cenGX(p76_0, 2).
cenGY(p76_0, 6).
size(p76_0, 5).

color(p76_0,green).
upright(p76_0).
piece(76, p76_1).
cenGX(p76_1, 2).
cenGY(p76_1, 8).
size(p76_1, 2).

color(p76_1,green).
upright(p76_1).
piece(76, p76_2).
cenGX(p76_2, 4).
cenGY(p76_2, 2).
size(p76_2, 8).

color(p76_2,blue).
strange(p76_2).
piece(76, p76_3).
cenGX(p76_3, 5).
cenGY(p76_3, 5).
size(p76_3, 1).

color(p76_3,blue).
upright(p76_3).
piece(76, p76_4).
cenGX(p76_4, 7).
cenGY(p76_4, 0).
size(p76_4, 4).

color(p76_4,green).
rhs(p76_4).
piece(77, p77_0).
cenGX(p77_0, 6).
cenGY(p77_0, 10).
size(p77_0, 2).

color(p77_0,blue).
lhs(p77_0).
piece(77, p77_1).
cenGX(p77_1, 10).
cenGY(p77_1, 6).
size(p77_1, 10).

color(p77_1,blue).
upright(p77_1).
piece(78, p78_0).
cenGX(p78_0, 1).
cenGY(p78_0, 5).
size(p78_0, 6).

color(p78_0,red).
strange(p78_0).
piece(78, p78_1).
cenGX(p78_1, 9).
cenGY(p78_1, 2).
size(p78_1, 3).

color(p78_1,green).
lhs(p78_1).
piece(79, p79_0).
cenGX(p79_0, 4).
cenGY(p79_0, 6).
size(p79_0, 8).

color(p79_0,blue).
rhs(p79_0).
piece(79, p79_1).
cenGX(p79_1, 0).
cenGY(p79_1, 9).
size(p79_1, 3).

color(p79_1,green).
upright(p79_1).
piece(79, p79_2).
cenGX(p79_2, 10).
cenGY(p79_2, 4).
size(p79_2, 10).

color(p79_2,red).
upright(p79_2).
piece(79, p79_3).
cenGX(p79_3, 0).
cenGY(p79_3, 9).
size(p79_3, 8).

color(p79_3,red).
lhs(p79_3).
child(p79_1, p79_3).
child(p79_1, p79_3).
child(p79_3, p79_1).
child(p79_3, p79_1).
piece(80, p80_0).
cenGX(p80_0, 6).
cenGY(p80_0, 4).
size(p80_0, 5).

color(p80_0,blue).
rhs(p80_0).
piece(80, p80_1).
cenGX(p80_1, 2).
cenGY(p80_1, 8).
size(p80_1, 10).

color(p80_1,red).
rhs(p80_1).
piece(81, p81_0).
cenGX(p81_0, 0).
cenGY(p81_0, 7).
size(p81_0, 9).

color(p81_0,red).
upright(p81_0).
piece(81, p81_1).
cenGX(p81_1, 10).
cenGY(p81_1, 3).
size(p81_1, 2).

color(p81_1,blue).
rhs(p81_1).
piece(81, p81_2).
cenGX(p81_2, 9).
cenGY(p81_2, 10).
size(p81_2, 8).

color(p81_2,blue).
upright(p81_2).
piece(81, p81_3).
cenGX(p81_3, 7).
cenGY(p81_3, 8).
size(p81_3, 4).

color(p81_3,blue).
upright(p81_3).
piece(81, p81_4).
cenGX(p81_4, 9).
cenGY(p81_4, 3).
size(p81_4, 4).

color(p81_4,green).
upright(p81_4).
child(p81_1, p81_4).
child(p81_1, p81_4).
child(p81_4, p81_1).
child(p81_4, p81_1).
piece(82, p82_0).
cenGX(p82_0, 5).
cenGY(p82_0, 10).
size(p82_0, 7).

color(p82_0,red).
rhs(p82_0).
piece(82, p82_1).
cenGX(p82_1, 4).
cenGY(p82_1, 0).
size(p82_1, 4).

color(p82_1,green).
lhs(p82_1).
piece(82, p82_2).
cenGX(p82_2, 1).
cenGY(p82_2, 3).
size(p82_2, 4).

color(p82_2,red).
rhs(p82_2).
piece(82, p82_3).
cenGX(p82_3, 2).
cenGY(p82_3, 4).
size(p82_3, 3).

color(p82_3,green).
lhs(p82_3).
piece(83, p83_0).
cenGX(p83_0, 6).
cenGY(p83_0, 7).
size(p83_0, 7).

color(p83_0,blue).
lhs(p83_0).
piece(83, p83_1).
cenGX(p83_1, 6).
cenGY(p83_1, 9).
size(p83_1, 0).

color(p83_1,green).
rhs(p83_1).
piece(83, p83_2).
cenGX(p83_2, 0).
cenGY(p83_2, 8).
size(p83_2, 0).

color(p83_2,red).
strange(p83_2).
piece(84, p84_0).
cenGX(p84_0, 5).
cenGY(p84_0, 4).
size(p84_0, 10).

color(p84_0,red).
upright(p84_0).
piece(84, p84_1).
cenGX(p84_1, 6).
cenGY(p84_1, 2).
size(p84_1, 5).

color(p84_1,red).
rhs(p84_1).
piece(85, p85_0).
cenGX(p85_0, 2).
cenGY(p85_0, 2).
size(p85_0, 5).

color(p85_0,green).
upright(p85_0).
piece(85, p85_1).
cenGX(p85_1, 6).
cenGY(p85_1, 2).
size(p85_1, 2).

color(p85_1,green).
upright(p85_1).
piece(85, p85_2).
cenGX(p85_2, 0).
cenGY(p85_2, 7).
size(p85_2, 0).

color(p85_2,blue).
rhs(p85_2).
piece(86, p86_0).
cenGX(p86_0, 8).
cenGY(p86_0, 6).
size(p86_0, 5).

color(p86_0,blue).
upright(p86_0).
piece(86, p86_1).
cenGX(p86_1, 10).
cenGY(p86_1, 2).
size(p86_1, 2).

color(p86_1,green).
rhs(p86_1).
piece(86, p86_2).
cenGX(p86_2, 10).
cenGY(p86_2, 4).
size(p86_2, 1).

color(p86_2,blue).
upright(p86_2).
piece(86, p86_3).
cenGX(p86_3, 7).
cenGY(p86_3, 5).
size(p86_3, 4).

color(p86_3,red).
strange(p86_3).
piece(86, p86_4).
cenGX(p86_4, 4).
cenGY(p86_4, 9).
size(p86_4, 9).

color(p86_4,green).
lhs(p86_4).
piece(87, p87_0).
cenGX(p87_0, 2).
cenGY(p87_0, 5).
size(p87_0, 8).

color(p87_0,red).
rhs(p87_0).
piece(87, p87_1).
cenGX(p87_1, 5).
cenGY(p87_1, 2).
size(p87_1, 10).

color(p87_1,red).
strange(p87_1).
piece(88, p88_0).
cenGX(p88_0, 9).
cenGY(p88_0, 1).
size(p88_0, 5).

color(p88_0,green).
upright(p88_0).
piece(88, p88_1).
cenGX(p88_1, 1).
cenGY(p88_1, 3).
size(p88_1, 0).

color(p88_1,blue).
upright(p88_1).
piece(89, p89_0).
cenGX(p89_0, 7).
cenGY(p89_0, 10).
size(p89_0, 10).

color(p89_0,red).
lhs(p89_0).
piece(89, p89_1).
cenGX(p89_1, 8).
cenGY(p89_1, 8).
size(p89_1, 5).

color(p89_1,blue).
upright(p89_1).
piece(90, p90_0).
cenGX(p90_0, 8).
cenGY(p90_0, 6).
size(p90_0, 0).

color(p90_0,green).
rhs(p90_0).
piece(90, p90_1).
cenGX(p90_1, 8).
cenGY(p90_1, 2).
size(p90_1, 10).

color(p90_1,blue).
upright(p90_1).
piece(90, p90_2).
cenGX(p90_2, 1).
cenGY(p90_2, 9).
size(p90_2, 8).

color(p90_2,blue).
strange(p90_2).
piece(90, p90_3).
cenGX(p90_3, 0).
cenGY(p90_3, 1).
size(p90_3, 3).

color(p90_3,blue).
lhs(p90_3).
piece(90, p90_4).
cenGX(p90_4, 6).
cenGY(p90_4, 6).
size(p90_4, 7).

color(p90_4,blue).
upright(p90_4).
piece(91, p91_0).
cenGX(p91_0, 0).
cenGY(p91_0, 2).
size(p91_0, 7).

color(p91_0,blue).
rhs(p91_0).
piece(91, p91_1).
cenGX(p91_1, 4).
cenGY(p91_1, 4).
size(p91_1, 10).

color(p91_1,blue).
upright(p91_1).
piece(91, p91_2).
cenGX(p91_2, 8).
cenGY(p91_2, 8).
size(p91_2, 1).

color(p91_2,red).
upright(p91_2).
piece(92, p92_0).
cenGX(p92_0, 10).
cenGY(p92_0, 9).
size(p92_0, 4).

color(p92_0,green).
strange(p92_0).
piece(92, p92_1).
cenGX(p92_1, 1).
cenGY(p92_1, 4).
size(p92_1, 0).

color(p92_1,green).
upright(p92_1).
piece(93, p93_0).
cenGX(p93_0, 7).
cenGY(p93_0, 9).
size(p93_0, 10).

color(p93_0,blue).
lhs(p93_0).
piece(93, p93_1).
cenGX(p93_1, 1).
cenGY(p93_1, 1).
size(p93_1, 10).

color(p93_1,red).
upright(p93_1).
piece(93, p93_2).
cenGX(p93_2, 6).
cenGY(p93_2, 5).
size(p93_2, 5).

color(p93_2,green).
lhs(p93_2).
piece(93, p93_3).
cenGX(p93_3, 6).
cenGY(p93_3, 3).
size(p93_3, 5).

color(p93_3,blue).
rhs(p93_3).
piece(94, p94_0).
cenGX(p94_0, 10).
cenGY(p94_0, 9).
size(p94_0, 0).

color(p94_0,green).
lhs(p94_0).
piece(94, p94_1).
cenGX(p94_1, 3).
cenGY(p94_1, 10).
size(p94_1, 9).

color(p94_1,red).
upright(p94_1).
piece(94, p94_2).
cenGX(p94_2, 9).
cenGY(p94_2, 10).
size(p94_2, 7).

color(p94_2,red).
strange(p94_2).
piece(95, p95_0).
cenGX(p95_0, 4).
cenGY(p95_0, 6).
size(p95_0, 3).

color(p95_0,red).
upright(p95_0).
piece(95, p95_1).
cenGX(p95_1, 10).
cenGY(p95_1, 6).
size(p95_1, 5).

color(p95_1,green).
upright(p95_1).
piece(95, p95_2).
cenGX(p95_2, 6).
cenGY(p95_2, 0).
size(p95_2, 3).

color(p95_2,blue).
lhs(p95_2).
piece(96, p96_0).
cenGX(p96_0, 7).
cenGY(p96_0, 2).
size(p96_0, 9).

color(p96_0,blue).
lhs(p96_0).
piece(97, p97_0).
cenGX(p97_0, 8).
cenGY(p97_0, 0).
size(p97_0, 1).

color(p97_0,green).
lhs(p97_0).
piece(97, p97_1).
cenGX(p97_1, 6).
cenGY(p97_1, 2).
size(p97_1, 7).

color(p97_1,green).
strange(p97_1).
piece(97, p97_2).
cenGX(p97_2, 7).
cenGY(p97_2, 8).
size(p97_2, 2).

color(p97_2,blue).
upright(p97_2).
piece(98, p98_0).
cenGX(p98_0, 0).
cenGY(p98_0, 6).
size(p98_0, 2).

color(p98_0,blue).
lhs(p98_0).
piece(98, p98_1).
cenGX(p98_1, 5).
cenGY(p98_1, 8).
size(p98_1, 5).

color(p98_1,green).
lhs(p98_1).
piece(98, p98_2).
cenGX(p98_2, 5).
cenGY(p98_2, 2).
size(p98_2, 9).

color(p98_2,red).
upright(p98_2).
piece(98, p98_3).
cenGX(p98_3, 2).
cenGY(p98_3, 9).
size(p98_3, 1).

color(p98_3,green).
rhs(p98_3).
piece(98, p98_4).
cenGX(p98_4, 5).
cenGY(p98_4, 4).
size(p98_4, 1).

color(p98_4,green).
lhs(p98_4).
piece(99, p99_0).
cenGX(p99_0, 8).
cenGY(p99_0, 6).
size(p99_0, 5).

color(p99_0,green).
rhs(p99_0).
piece(99, p99_1).
cenGX(p99_1, 7).
cenGY(p99_1, 8).
size(p99_1, 4).

color(p99_1,blue).
rhs(p99_1).
piece(99, p99_2).
cenGX(p99_2, 5).
cenGY(p99_2, 2).
size(p99_2, 4).

color(p99_2,red).
lhs(p99_2).