module no_support_drilling(d1, d2, h1, h2, fn1 = 36, fn2 = 36, layer = 0.1) {
  assert(d1 < d2);
  overlap = 0.001;

  cylinder(d = d1, h = h1, $fn = fn1);
  translate([0, 0, layer / 2 - overlap]) cube([d1, d1, layer], center = true);

  difference() {
    translate([0, 0, -h2])
      cylinder(d = d2, h = h2, $fn = fn2);

    for (a = [0, 180])
      rotate([0, 0, a])
        translate([d1 / 2, -d2 / 2 - overlap, -layer])
          cube([d2 / 2 + overlap, d2 + 2 * overlap, layer + overlap]);
  }
}
