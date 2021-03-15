using Khepri
backend(autocad)

############################

#EXECUTIONS

erase = 1 #0 - does not erase
structure_d = 1 #0 - does not erase
walls_d = 1 #0 - does not draw
interior_walls_d = 1 #0 - does not draw
slabs_d = 1 #0 - does not draw
glass_d = 1 #0 - does not draw
frames_d = 1 #0 - does not draw
handrails_d = 1 #0 - does not draw
flooring_d = 1 #0 - does not draw
ramp_d = 1 #0 - does not draw
room_divisions_d = 1 #0 - does not draw
lamp_d = 1 #0 - does not draw
slab_handrail_d = 1 #0 - does not draw
ramp_handrail_d = 1 #0 - does not draw
lighting_d = 1 #0 - does not draw
render = 0 #0 - does not render

############################

#DEFINITIONS

#structure
p0 = u0() #base point
rb = 20 #base radius of building
e = 0.3 #wall and slab thickness
t_glass = 0.03 #glass thickness
h_floor = 3.6 #floor height
h_d_floor = h_floor - e #floor height
h_rail = 1 #handrail height
n_floors = 5 #number of floors
h_floor = 3.6 #floor height
h_total = h_floor * n_floors #total height
h_total_1 = h_total - h_floor #total hieght minus one floor
n_walls = 5 #number of interior walls

#spiral
w0 = rb * 0.5 #interior radius
w1 = rb * 0.65 #exterior radius
n0_spiral = 200 #number of points of the internal spiral
c = w1 - w0 #legth dimension of ramp slab
n_h = (n_floors - 1) * 200 #number of points of the spiral
fa0_spiral = 2*pi #final angle of the internal spiral
a_pts = 2*pi/n0_spiral #angle between each point of the spiral
w_g = w0 + e / 2 #initial handrail radius
n_w0 = 200 #number of points of the exterior spiral
a_w = 2 * pi / n_w0 #angle between points of the spiral
a_g0 = 2 * pi #internal handrail maximum angle
a_g1 = a_g0 * (w1 - 1) / w1 #2pi*12/13 2pi*25/16 external handrail maximun angle
n_w1 = round(n_w0 * a_g1 / a_g0) #number of points of interior spiral
h_floor0 = h_floor #maximum height of interior spiral
fa0_spiral2 = fa0_spiral #fa0_spiral + a-pts #final angle of the internal spiral
fa1_spiral = fa0_spiral * (w1 - 0.5)/w1 #2pi*12/13 2pi*25/26 final angle of the external spiral
n1_spiral = round(n0_spiral * fa1_spiral/fa0_spiral, digits=0) #number of points of the external spiral
ia0_spiral = 0 #initial angle of the internal spiral
ia1_spiral = a_pts #a_pts*2 #initial angle of the external spiral
n_pts_f =n0_spiral/20 #final handrail number of points
a_rail_f = 0 #final handrail angle  (/ pi 104) (/ (- 1 (/ a-g1 a-g0)) 1.4)

#ramp
rm_spiral = w0 + c / 2 #median radius of the spiral
h_step = 0.3 #spiral ramp step blocks height
l_step = w1 - w0 #spiral ramp step blocks length

#lighting
n_z_frames = 14 #number of zenith roof frames
n_g_pts = 8 #glass windows number of points

#lamps
n_lights = 16 #number of lamps per floor
h_lights = h_d - 0.1 # height of lamps in floor
r_lights = 0.1 #lamp radius

############################

#LAYERS

flooring_layer = create_layer("Flooring")
slab_layer = create_layer("Slab")
ramp_layer = create_layer("Ramp")
wall_layer = create_layer("Wall")
glass_layer = create_layer("Glass")
slab_handrail_layer = create_layer("Slab Handrail")
ramp__handrail_layer = create_layer("Ramp Handrail")
lights_layer = create_layer("Lights")
person_layer = create_layer("Person")

############################

#FUNCTIONS

#erase

if erase == 1
    delete_all_shapes()
end

#slab
b_slab(p, r, r1_spiral, e_slab, t_glass, h_d_floor, h_total) =
  with(current_layer, slab_layer) do
    subtraction(
     cylinder(p + vz(- e_slab), r - t_glass, p),
     cylinder(p0 + vz(- h_total), r1_spiral, p0 + vz(h_total)))
    subtraction(
     cylinder(p + vz(h_d_floor), r, p +vz(h_floor)),
     cylinder(p0 + vz(h_d_floor - h_total), r1_spiral, p0 + vz(h_total)))
   end

#walls
b_wall(p, r, a1, da1, a2, da2, h_d, e) =
  with(current_layer, wall_layer) do
     #thicken(
      extrusion(
       arc(p, r, a1, da1), h_d), - e#)
     #thicken(
      extrusion(
       arc(p, r, a2, da2), h_d), - e#)
    end

#interior wall
interior_wall(p, r, r1_spiral, a, e_slab, h_d_floor) =
 let d0 = r1_spiral + 2, #minimum radiaus
     d1 = r - 2 - e_slab #maximum radiaus
     with(current_layer, wall_layer) do
       right_cuboid(p + vcyl(d0, a, h_d_floor/2),
                     e_slab/2, h_d_floor,
                     p +vcyl(d1, a, h_d_floor/2))
     end
 end

 #interior walls
 interior_walls(p, r, r1_spiral, e_slab, h_d_floor) =
   let interior_wall_rec(n_wall) =
     let a = random_range(0, 2*pi)
       interior_wall(p, r, r1_spiral, a, e_slab, h_d_floor)
     map(interior_wall_rec,
        division(0, n_walls, n_walls, false))
     end
   end

#glass
glass(p, r, a1, da1, a2, da2, h_d, e, n_g_pts) =
   with(current_layer, glass_layer) do
     #thicken(
      extrusion(
       arc(p, r, a1, da1), h_d), - e#)
       #thicken(
        extrusion(
         arc(p, r, a2, da2), h_d), - e#)
     end

#zenith lighting
frame(p, n, r1_spiral, fi_i, fi_f, e_slab) =
    with(current_layer, frame_layer) do
       right_cuboid(p +vcyl(r1_spiral, fi_i, 0),
                     e_slab/2, e_slab/2,
                     p +vcyl(r1_spiral, fi_f, 0))
                 end

frames(p, n, r1_spiral, e_slab) =
    let frame_rec(n) =
        let fi_i = random_range(0, pi),
            fi_f = random_range(pi, 2*pi)
    frame(p, n, r1_spiral, fi_i, fi_f, e_slab)
        end
    map(frame_rec,
        division(0, n_z_frames, n_z_frames, false))
    end

#flooring
flooring(p, r, r1_spiral, t_glass, h_total) =
  with(current_layer, flooring_layer) do
    subtraction(
     cylinder(p, r - t_glass, p + vz(t_glass)),
     cylinder(p0 + vz(- h_total), r1_spiral, p0 + vz(h_total)))
  end

#lighting
lights(p, r) =
  let h_lights = h_floor - r_lights
      let light_rec(a) =
        with(current_layer, lights_layer) do
          sphere(p + vcyl(r, a, - e - r_lights), r_lights)
        end
    map(light_rec,
      division(0, 2*pi, n_lights, false))
      end
  end

#slabs, walls, glass, frames, flooring and lighting
b_structure(p, r, r1_spiral, n_z_frames, e_slab, t_glass, n_g_pts, h_d_floor, h_floor, h_total, n_floors) =
  let structure_rec(n_floor) =
    #main
    let f = random_range(0.95, 1.05),
        at = random_range(0, 2*pi),
        rr = r * f - r,
        pt = p + vcyl(rr, at, h_floor * n_floor),
    #walls
        a1 = random_range(0, 2*pi),
        da1 = random_range(3 * pi/4, 5 * pi/4),
        a2 = a1 + da1,
        da2 = random_range(pi/3, pi/4),
        a3 = a2 + da2,
        da3 = random_range(pi/4, pi),
        a4 = a3 + da3,
        da4 = a1 + 2*pi - a4

        #elements
        if slabs_d == 1
            b_slab(pt, r, r1_spiral, e_slab, t_glass, h_d_floor, h_total)
        end
        if flooring_d == 1
            flooring(pt, r, r1_spiral, t_glass, h_total)
        end
        if walls_d == 1
           b_wall(pt, r, a1, da1, a3, da3, h_d_floor, e_slab)
        end
        if glass_d == 1
           glass(pt, r, a2, da2, a4, da4, h_d_floor, t_glass, n_g_pts)
        end
        #interior walls
        if interior_walls_d == 1
           interior_walls(pt, r, r1_spiral, e_slab, h_d_floor)
        end
        #lighting
        if lighting_d  == 1
           lights(pt, r - r_lights)
        end
    end
    map(structure_rec,
         division(0, n_floors, n_floors, false))
  end

  #ramp
ramp_block(p, ro, c_step, e_step, h_floor, alfa, n_pts) =
let block(fi, z) =
  with(current_layer, ramp_layer) do
    right_cuboid(p + vcyl(ro,
                        fi,
                        z - e_step),
                  c_step, e_step,
                  p + vcyl(ro,
                        fi + alfa*2,
                        z - e_step + alfa))
    end
    map(block,
         division(0, 2*pi, n_pts - 1, false),
         division(0, h_floor, n_pts - 1, false))
end

slab_handrail(p, ro, c, e_v, alfa, a, n_pts, h_floor) =
 let handrail(fi) =
   with(current_layer, glass_layer) do
       right_cuboid(p + vcyl(ro, fi, c / 2 + h_floor), e_v, c, p + vcyl(ro, fi + alfa, c / 2 + h_floor))
   end
 map_division(handrail, alfa, a, n_pts - 1)
end

#ramp handrail
ramp_handrail(p, ro, c, e_v, alfa, ai, af, n_pts, h_floor) =
  let handrail(fi, z) =
    with(current_layer, glass_layer) do
      right_cuboid(p + vcyl(ro - e_v/2,
                          fi,
                          z + c/2 - e_v + h_floor),
                    e_v, c,
                    p + vcyl(ro - e_v/2,
                          fi + alfa,
                          z + c/2 - e_v + alfa/2 + h_floor))
    end
   map(handrail,
       division(ai, af, n_pts - 1),
       division(0, h_floor, n_pts - 1))
   end

ramp_handrail_f(p, n, t_glass, fi, alfa) =
     let handrail(ro) =
       with(current_layer, glass_layer) do
         right_cuboid(p + vcyl(ro,
                             fi + alfa,
                             h_rail/2 - t_glass),
                       t_glass, h_rail,
                       p + vcyl(ro + (w1 - w0)/n,
                             fi + alfa,
                             h_rail/2 - t_glass))
       end
     map(handrail,
          division(w0, w1 - (w1 - w0)/n, n - 1))
    end

#handrails
handrails(p, ro0, ro1, c, e_v, ia0_spiral, ia1_spiral, fa0_spiral, fa1_spiral, n0_spiral, n1_spiral, h_floor, n_floors) =
    begin
        let slab_handrail_rec(n_floor) =
        slab_handrail(p + vz(h_floor * n_floor), ro1, c, e_v, ia1_spiral, fa1_spiral, n1_spiral, h_floor)
        map(slab_handrail_rec,
          division(0, n_floors - 1, n_floors - 1, false))
        end
        let ramp_handrail_rec(n_floor) =
        begin
           ramp_handrail(p + vz(h_floor * (n_floor - 1)), ro0, c, e_v, a_pts, ia0_spiral, fa0_spiral2, n0_spiral, h_floor)
           ramp_handrail(p + vz(h_floor * (n_floor - 1)), ro1, c, e_v, a_pts, ia1_spiral, fa1_spiral, n1_spiral, h_floor)
        end
        map(ramp_handrail_rec,
              division(0, n_floors - 1, n_floors - 1, false))
        end
        ramp_handrail_f(p0 + vz(h_total_1), n_pts_f, t_glass, a_rail_f, a_pts)
    end


#ramp
ramp(p, ro, c_step, e_step, h_floor, alfa, n_pts, n_floors) =
let ramp_rec(n_floor) =
  ramp_block(p + vz(h_floor * n_floor + e_step/2), ro, c_step, e_step, h_floor, alfa, n_pts)
  map(ramp_rec,
     division(0, n_floors - 1, n_floors - 1, false))
 end

############################

if handrails_d == 1
  handrails(p0, w0, w1, h_rail, t_glass, a_pts, ia1_spiral, fa0_spiral, fa1_spiral, n0_spiral, n1_spiral, h_floor, n_floors)
end

if ramp_d == 1
  ramp(p0, rm_spiral, l_step, h_step, h_floor, a_pts, n0_spiral, n_floors)
end

if structure_d == 1
  b_structure(p0, rb, w1, n_z_frames, e, t_glass, n_g_pts, h_d_floor, h_floor, h_total, n_floors)
end
