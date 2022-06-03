// 
// FISH FEEDER
//
// based on threads & screws by:
// Created 2016-2017 by Ryan A. Colyer.
// This work is released with CC0 into the public domain.
// https://creativecommons.org/publicdomain/zero/1.0/
//module ScrewThread(outer_diam, height, pitch=0, tooth_angle=30, tolerance=0.4, tip_height=0, tooth_height=0, tip_min_fract=0)
//module AugerThread(outer_diam, inner_diam, height, pitch, tooth_angle=30, tolerance=0.4, tip_height=0, tip_min_fract=0)
// AugerThread(40, 8, 100, 20, tooth_angle=15, tip_height=10);
// https://www.thingiverse.com/thing:1686322
// v2.1

/*
pipeInnerD = 22.2;
pipeOuterD = 26.0;
screwInnerD = pipeInnerD/5;

pipeInnerD = 81;
pipeOuterD = 88.4;
screwInnerD = pipeInnerD/5;

pipeInnerD = 41.6;
pipeOuterD = 48.0;
screwInnerD = pipeInnerD/5;
*/

pipeInnerD = 41.6;
pipeOuterD = 48.0;
screwInnerD = pipeInnerD/5;

//for the conveyor screw
screwTolerance = -0.5;
baseTolerance = -0.8;

//for the bottle connector
capScrew = 2.7;
capScrewTuak = 7.3; // Tuak 3 x

$fn=100;

//rotate([0,0,$t*-360]) 
//translate([0, 50, 2]) dusjagrScrew();
//translate([0, 0, 1]) becekScrew();
color("magenta") translate([0, 0, 18.0]) pipeHolder();
motorHolder();

//testFit();
//PETScrew();

//stepperCon();


//Test the fit of the ring
module testFit() {
    difference(){
        translate([0, 0, 3]) cylinder(h=6,d=pipeOuterD+4, center =true);
        translate([0, 0, 3]) cylinder(h=6.2,d1=0.99*pipeOuterD,d2=1.02*pipeOuterD, center =true);
    }
    //Pipe Entry
    difference(){
        translate([0, 0, -3]) cylinder(h=6,d1=pipeOuterD+1,d2=pipeOuterD+4, center =true);
        translate([0, 0, -3]) cylinder(h=6.2,d=pipeInnerD, center =true);
    }
}


module motorHolder() {
    difference(){
    union(){    
      minkowski(){
      union(){
        translate([0, 0, -2]) cylinder(h=1,d=pipeOuterD+20, center =true);
        translate([pipeOuterD/2, 0, -2]) cube([pipeOuterD,pipeOuterD+20,1], center=true);
        }
      sphere(1);
      }
    
      minkowski(){
      translate([pipeOuterD-1, 0, 10]) cube([2,pipeOuterD+20,24], center=true);
      sphere(1);
      }
      /*
      difference(){
      translate([-pipeInnerD/2-10, 0, 0]) minkowski(){
        translate([0, 0, -1.75]) cube([pipeInnerD+20,pipeOuterD/3,1.5], center=true);
        sphere(1);
        }
       translate([0, 0, -8]) cylinder(h=22.2,d=pipeInnerD, center =true);
      }
      */
    
      //screw Foot
      translate([pipeOuterD-4, -27.5, 11])  rotate([0,90,0]) cylinder(h=4,d=8, center =true, $fn=6);
      translate([pipeOuterD-4, 27.5, 11])  rotate([0,90,0]) cylinder(h=4,d=8, center =true, $fn=6);
    
      //screw port Motor
      //translate([8, 17.5, 4]) cylinder(h=8,d=8, center =true, $fn=16);
      //translate([8, -17.5, 4]) cylinder(h=8,d=8, center =true, $fn=16);
    }
    
    //holes Motor
    //motorStepper ();
    motorServoMG996R ();
    
    //holes Foot
    translate([pipeOuterD-6, -27.5, 11]) rotate([0,90,0]) cylinder(h=20.2,d=3.6, center =true, $fn=5);
    translate([pipeOuterD-6, 27.5, 11]) rotate([0,90,0]) cylinder(h=20.2,d=3.6, center =true, $fn=5);
  }
}


module motorStepper (){
    stepperScrewDia = 3.4;
    cylinder(h=20.2,d=10, center =true, $fn=50);
    translate([8, 17.5, 0]) cylinder(h=20.2,d=stepperScrewDia, center =true, $fn=5);
    translate([8, -17.5, 0]) cylinder(h=20.2,d=stepperScrewDia, center =true, $fn=5);
}

module motorServoMG996R (){
    serveScrewDia = 3.3;
    cylinder(h=20.2,d=19, center =true, $fn=50);
    translate([34.65, 4.93, 0]) cylinder(h=20.2,d=serveScrewDia, center =true, $fn=5);
    translate([34.65, -4.93, 0]) cylinder(h=20.2,d=serveScrewDia, center =true, $fn=5);
    translate([-14.85, 4.93, 0]) cylinder(h=20.2,d=serveScrewDia, center =true, $fn=5);
    translate([-14.85, -4.93, 0]) cylinder(h=20.2,d=serveScrewDia, center =true, $fn=5);
}


module pipeHolder() {
    //Pipe Connection
  difference(){
    union(){
    difference(){
        translate([0, 0, 9]) cylinder(h=14,d=pipeOuterD+4, center =true);
        translate([0, 0, 9]) cylinder(h=14.4,d1=0.99*pipeOuterD,d2=1.02*pipeOuterD, center =true);
        }
    //Pipe Entry
    difference(){
        translate([0, 0, -8]) cylinder(h=22,d1=pipeOuterD+1,d2=pipeOuterD+4, center =true);
        translate([0, 0, -8]) cylinder(h=22.2,d=pipeInnerD, center =true);
        }
        
    //Funnel
  difference(){
    translate([-pipeOuterD/2+13, 0, 6]) union(){
    difference(){
        translate([-31, 0, -9]) rotate([0,103,0]) cylinder(h=42,d1=36.5,d2=23, center =true);
        translate([-30, 0, -9]) rotate([0,103,0]) cylinder(h=40.2,d1=36.5-8,d2=20-4, center =true);
   
        //cut the top
        translate([-52, 0, -1]) rotate([0,90,0]) cube([2*pipeOuterD+20,2*pipeOuterD+20,20], center=true);   
        //translate([-10, 0, -14]) rotate([0,90,0]) cylinder(h=20,d=19, center =true);
        }
        
     //BottlePort
        difference(){
        translate([-43, 0, -5.5]) rotate([180,90,0]) PETScrew();
        translate([-46, 0, -5.5]) rotate([180,90,0]) cylinder(h=20.2,d=26, center =true, $fn=50);
        }
    }
    translate([0, 0, 5]) cylinder(h=8.2,d1=0.99*pipeOuterD,d2=1.02*pipeOuterD, center =true);
    translate([0, 0, -9]) cylinder(h=22.2,d=pipeInnerD, center =true);
  }
  }
  translate([-pipeOuterD/2, 0, -6.5]) rotate([0,103,0]) cylinder(h=60,d=18, center =true);
  }
}

module PETScrew(){
    difference(){
        minkowski(){
      translate([0, 0, 5]) cylinder(h=12,d=34, center =true, $fn=100);
            sphere(1);
        }
      translate([0, 0, 0]) AugerThread(27.8, 26.2, 14, capScrew, tooth_angle=45, tip_height=0, , tip_min_fract=0.2);
      translate([0, 0, 11.5]) cylinder(h=5,d=29.2, center =true);   
    }
}

module PETScrewTuak(){
  difference(){
    translate([0, 0, 4]) cylinder(h=10,d=34, center =true, $fn=100);
      union(){
          AugerThread(31.2, 28.6, 12, capScrewTuak, tooth_angle=45, tip_height=0, , tip_min_fract=0.2);
          rotate([0,0,240])AugerThread(31.2, 28.6, 12, capScrew, tooth_angle=45, tip_height=0, , tip_min_fract=0.2);
          rotate([0,0,120]) AugerThread(31.2, 28.6, 12, capScrew, tooth_angle=45, tip_height=0, , tip_min_fract=0.2);
      }
   translate([0, 0, 9]) cylinder(h=4,d=31.2, center =true);   
  }
}

module dusjagrScrew() {
    difference(){
    color("hotpink") translate([0, 0, 0])
    union() {
        translate([0, 0, 2]) AugerThread(pipeInnerD+screwTolerance, screwInnerD, 65, 12, tooth_angle=15, tip_height=10, , tip_min_fract=0.2);
        translate([0, 0, 1]) cylinder(h=2,d=pipeInnerD+baseTolerance, center =true);
        translate([0, 0, 5.5]) cylinder(h=7,d1=pipeInnerD+baseTolerance,d2=screwInnerD,center =true);
        }
    translate([0, 0, 2.8]) motorCon();
    }
}

module becekScrew() {
    difference(){
    color("hotpink") translate([0, 0, 0])
    union() {
        translate([0, 0, 2]) AugerThread(pipeInnerD+screwTolerance, screwInnerD, pipeInnerD*3.5, pipeInnerD/2, tooth_angle=15, tip_height=pipeInnerD/4, , tip_min_fract=0.2);
        translate([0, 0, 1]) cylinder(h=2,d=pipeInnerD+baseTolerance, center =true);
        translate([0, 0, 5.5]) cylinder(h=7,d1=pipeInnerD+baseTolerance,d2=screwInnerD,center =true);
        }
    translate([0, 0, 2.8]) stepperCon();
    }
}

module motorCon() {
    difference(){
    cylinder(h=6,d1=5.2,d2=5, center =true, $fn=50);
       translate([0, 3.2, 0]) cube([10,3,10], center=true);
       translate([0, -3.2, 0]) cube([10,3,10], center=true);
    }
}

module stepperCon() {
    difference(){
    cylinder(h=6,d1=5.99,d2=5.8, center =true, $fn=100);
        for(i =  [0:14.4:360]){
            rotate([0,0,i]) translate([0, 3.05, 0]) cylinder(h=8,d=0.55, center =true, $fn=6);
        }
    }
}


screw_resolution = 0.4;  // in mm


// This generates a closed polyhedron from an array of arrays of points,
// with each inner array tracing out one loop outlining the polyhedron.
// pointarrays should contain an array of N arrays each of size P outlining a
// closed manifold.  The points must obey the right-hand rule.  For example,
// looking down, the P points in the inner arrays are counter-clockwise in a
// loop, while the N point arrays increase in height.  Points in each inner
// array do not need to be equal height, but they usually should not meet or
// cross the line segments from the adjacent points in the other arrays.
// (N>=2, P>=3)
// Core triangles:
//   [j][i], [j+1][i], [j+1][(i+1)%P]
//   [j][i], [j+1][(i+1)%P], [j][(i+1)%P]
//   Then triangles are formed in a loop with the middle point of the first
//   and last array.
module ClosePoints(pointarrays) {
  function recurse_avg(arr, n=0, p=[0,0,0]) = (n>=len(arr)) ? p :
    recurse_avg(arr, n+1, p+(arr[n]-p)/(n+1));

  N = len(pointarrays);
  P = len(pointarrays[0]);
  NP = N*P;
  lastarr = pointarrays[N-1];
  midbot = recurse_avg(pointarrays[0]);
  midtop = recurse_avg(pointarrays[N-1]);

  faces_bot = [
    for (i=[0:P-1])
      [0,i+1,1+(i+1)%len(pointarrays[0])]
  ];

  loop_offset = 1;
  bot_len = loop_offset + P;

  faces_loop = [
    for (j=[0:N-2], i=[0:P-1], t=[0:1])
      [loop_offset, loop_offset, loop_offset] + (t==0 ?
      [j*P+i, (j+1)*P+i, (j+1)*P+(i+1)%P] :
      [j*P+i, (j+1)*P+(i+1)%P, j*P+(i+1)%P])
  ];

  top_offset = loop_offset + NP - P;
  midtop_offset = top_offset + P;

  faces_top = [
    for (i=[0:P-1])
      [midtop_offset,top_offset+(i+1)%P,top_offset+i]
  ];

  points = [
    for (i=[-1:NP])
      (i<0) ? midbot :
      ((i==NP) ? midtop :
      pointarrays[floor(i/P)][i%P])
  ];
  faces = concat(faces_bot, faces_loop, faces_top);

  polyhedron(points=points, faces=faces);
}



// This creates a vertical rod at the origin with external threads.  It uses
// metric standards by default.
module ScrewThread(outer_diam, height, pitch=0, tooth_angle=30, tolerance=0.4, tip_height=0, tooth_height=0, tip_min_fract=0) {

  pitch = (pitch==0) ? ThreadPitch(outer_diam) : pitch;
  tooth_height = (tooth_height==0) ? pitch : tooth_height;
  tip_min_fract = (tip_min_fract<0) ? 0 :
    ((tip_min_fract>0.9999) ? 0.9999 : tip_min_fract);

  outer_diam_cor = outer_diam + 0.25*tolerance; // Plastic shrinkage correction
  inner_diam = outer_diam - tooth_height/tan(tooth_angle);
  or = (outer_diam_cor < screw_resolution) ?
    screw_resolution/2 : outer_diam_cor / 2;
  ir = (inner_diam < screw_resolution) ? screw_resolution/2 : inner_diam / 2;
  height = (height < screw_resolution) ? screw_resolution : height;

  steps_per_loop_try = ceil(2*3.14159265359*or / screw_resolution);
  steps_per_loop = (steps_per_loop_try < 4) ? 4 : steps_per_loop_try;
  hs_ext = 3;
  hsteps = ceil(3 * height / pitch) + 2*hs_ext;

  extent = or - ir;

  tip_start = height-tip_height;
  tip_height_sc = tip_height / (1-tip_min_fract);

  tip_height_ir = (tip_height_sc > tooth_height/2) ?
    tip_height_sc - tooth_height/2 : tip_height_sc;

  tip_height_w = (tip_height_sc > tooth_height) ? tooth_height : tip_height_sc;
  tip_wstart = height + tip_height_sc - tip_height - tip_height_w;


  function tooth_width(a, h, pitch, tooth_height, extent) =
    let(
      ang_full = h*360.0/pitch-a,
      ang_pn = atan2(sin(ang_full), cos(ang_full)),
      ang = ang_pn < 0 ? ang_pn+360 : ang_pn,
      frac = ang/360,
      tfrac_half = tooth_height / (2*pitch),
      tfrac_cut = 2*tfrac_half
    )
    (frac > tfrac_cut) ? 0 : (
      (frac <= tfrac_half) ?
        ((frac / tfrac_half) * extent) :
        ((1 - (frac - tfrac_half)/tfrac_half) * extent)
    );


  pointarrays = [
    for (hs=[0:hsteps])
      [
        for (s=[0:steps_per_loop-1])
          let(
            ang_full = s*360.0/steps_per_loop,
            ang_pn = atan2(sin(ang_full), cos(ang_full)),
            ang = ang_pn < 0 ? ang_pn+360 : ang_pn,

            h_fudge = pitch*0.001,

            h_mod =
              (hs%3 == 2) ?
                ((s == steps_per_loop-1) ? tooth_height - h_fudge : (
                 (s == steps_per_loop-2) ? tooth_height/2 : 0)) : (
              (hs%3 == 0) ?
                ((s == steps_per_loop-1) ? pitch-tooth_height/2 : (
                 (s == steps_per_loop-2) ? pitch-tooth_height + h_fudge : 0)) :
                ((s == steps_per_loop-1) ? pitch-tooth_height/2 + h_fudge : (
                 (s == steps_per_loop-2) ? tooth_height/2 : 0))
              ),

            h_level =
              (hs%3 == 2) ? tooth_height - h_fudge : (
              (hs%3 == 0) ? 0 : tooth_height/2),

            h_ub = floor((hs-hs_ext)/3) * pitch
              + h_level + ang*pitch/360.0 - h_mod,
            h_max = height - (hsteps-hs) * h_fudge,
            h_min = hs * h_fudge,
            h = (h_ub < h_min) ? h_min : ((h_ub > h_max) ? h_max : h_ub),

            ht = h - tip_start,
            hf_ir = ht/tip_height_ir,
            ht_w = h - tip_wstart,
            hf_w_t = ht_w/tip_height_w,
            hf_w = (hf_w_t < 0) ? 0 : ((hf_w_t > 1) ? 1 : hf_w_t),

            ext_tip = (h <= tip_wstart) ? extent : (1-hf_w) * extent,
            wnormal = tooth_width(ang, h, pitch, tooth_height, ext_tip),
            w = (h <= tip_wstart) ? wnormal :
              (1-hf_w) * wnormal +
              hf_w * (0.1*screw_resolution + (wnormal * wnormal * wnormal /
                (ext_tip*ext_tip+0.1*screw_resolution))),
            r = (ht <= 0) ? ir + w :
              ( (ht < tip_height_ir ? ((2/(1+(hf_ir*hf_ir))-1) * ir) : 0) + w)
          )
          [r*cos(ang), r*sin(ang), h]
      ]
  ];


  ClosePoints(pointarrays);
}


// This creates a vertical rod at the origin with external auger-style
// threads.
module AugerThread(outer_diam, inner_diam, height, pitch, tooth_angle=30, tolerance=0.4, tip_height=0, tip_min_fract=0) {
  tooth_height = tan(tooth_angle)*(outer_diam-inner_diam);
  ScrewThread(outer_diam, height, pitch, tooth_angle, tolerance, tip_height,
    tooth_height, tip_min_fract);
}


// This creates a threaded hole in its children using metric standards by
// default.
module ScrewHole(outer_diam, height, position=[0,0,0], rotation=[0,0,0], pitch=0, tooth_angle=30, tolerance=0.4, tooth_height=0) {
  extra_height = 0.001 * height;

  difference() {
    children();
    translate(position)
      rotate(rotation)
      translate([0, 0, -extra_height/2])
      ScrewThread(1.01*outer_diam + 1.25*tolerance, height + extra_height,
        pitch, tooth_angle, tolerance, tooth_height=tooth_height);
  }
}


// This creates an auger-style threaded hole in its children.
module AugerHole(outer_diam, inner_diam, height, pitch, position=[0,0,0], rotation=[0,0,0], tooth_angle=30, tolerance=0.4) {
  tooth_height = tan(tooth_angle)*(outer_diam-inner_diam);
  ScrewHole(outer_diam, height, position, rotation, pitch, tooth_angle,
    tolerance, tooth_height=tooth_height) children();
}


// This inserts a ClearanceHole in its children.
// The rotation vector is applied first, then the position translation,
// starting from a position upward from the z-axis at z=0.
module ClearanceHole(diameter, height, position=[0,0,0], rotation=[0,0,0], tolerance=0.4) {
  extra_height = 0.001 * height;

  difference() {
    children();
    translate(position)
      rotate(rotation)
      translate([0, 0, -extra_height/2])
      cylinder(h=height + extra_height, r=(diameter/2+tolerance));
  }
}




