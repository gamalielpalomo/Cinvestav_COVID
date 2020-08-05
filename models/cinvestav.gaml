/***
* Name: cinvestav
* Author: Gamaliel Palomo
* Description: 
* Tags: Cinvestav, epidemic, back-to-activities
***/

model cinvestav

import "Constants.gaml"
import "tools/DXF_Loader.gaml"

global{
	date starting_date <- #now;
	
	shape_file pedestrian_path_shape_file <- shape_file(dataset_path+ useCase+"/pedestrian_path.shp", gama.pref_gis_default_crs);
	string scenario;
	float distance_people;
	float separator_proba <- 0.0;
	bool showAvailableDesk<-false;
	
	bool display_pedestrian_path;
	bool display_free_space <- false;
	
	bool use_parallel <- false;
	
	bool show_dynamic_bottleneck <- true;  //show or not the bottleneck
	int bottleneck_livespan <- 5; //to livespan of a bottleneck agent (to avoid glitching aspect) 
	float coeff_speed_slow <- 2.0; //a people is considered as "slow" if its real speed is lower than it wanted speed / coeff_speed_slow during min_num_step_bottleneck
	int min_num_step_bottleneck <- 3;
	float distance_bottleneck <- 2.0; //a bottleneck is considered if there is at least min_num_people_bottleneck slow people at a distance of distance_bottleneck;
	int min_num_people_bottleneck <- 2; 
	
	list<room> available_offices;
	graph pedestrian_network;
	geometry shape <- envelope(the_dxf_file);
	init{
		starting_date <- date([current_date.year,current_date.month,current_date.day,7]);
		validator <- false;
		do initiliaze_dxf;
		create pedestrian_path from: pedestrian_path_shape_file;
		pedestrian_network <- as_edge_graph(pedestrian_path);
		loop se over: the_dxf_file  {
			string type <- se get layer;
			if (type = walls) {
				create wall with: [shape::clean(polygon(se.points))];
			} else if type = entrance {
				create building_entrance  with: [shape::polygon(se.points), type::type];
			} else if type in [offices, supermarket, meeting_rooms,coffee,storage] {
				create room with: [shape::polygon(se.points), type::type];
			}
		}
		ask room + building_entrance {
			do initialization;
		}
		ask room + building_entrance{
			
			geometry contour <- nil;
			float dist <-0.3;
			int cpt <- 0;
			loop while: contour = nil {
				cpt <- cpt + 1;
				contour <- copy(shape.contour);
				ask wall at_distance 1.0 {
					contour <- contour - (shape +dist);
				}
				if cpt < 10 {
					ask (room + building_entrance) at_distance 1.0 {
						contour <- contour - (shape + dist);
					}
				}
				if cpt = 20 {
					break;
				}
				dist <- dist * 0.5;	
			} 
			if contour != nil {
				entrances <- points_on (contour, 2.0);
			}
			ask places {
				point pte <- myself.entrances closest_to self;
				dists <- self distance_to pte;
			}
					
		}
		map<string, list<room>> rooms_type <- room group_by each.type;
		available_offices <- rooms_type[offices] where each.is_available();
		create working;
		create going_home_act with:[activity_places:: building_entrance as list];
		create eating_outside_act with:[activity_places:: building_entrance as list];
		do initialize_pedestrian_model;
		
		
		ask building_entrance {
			if (not empty(pedestrian_path)) {
				list<pedestrian_path> paths <- pedestrian_path at_distance 10.0;
				closest_path <-  paths with_min_of (each.free_space distance_to location);
				if (closest_path != nil) {
					init_place <- shape inter closest_path ;
					if (init_place = nil) {
						init_place <- (shape closest_points_with closest_path)[0]; 
					}
				}else {
					init_place <- shape; 
				}
			} else {
				init_place <- shape;
			}
		}
		
	}
	reflex people_arriving when: not empty(available_offices) and every(2 #s)
	{	
		do create_people(rnd(0,min(3, length(available_offices))));
	}
	action create_people(int nb){
		create people number:nb{
			pedestrian_model <- "SFM";
			obstacle_species <- [people,wall];
			working_place <- one_of(available_offices);
			working_place.nb_affected <- working_place.nb_affected + 1;
			if not(working_place.is_available()) {
				available_offices >> working_place;
			}
			current_activity <- first(working);
			target_room <- current_activity.get_place(self);
			target <- target_room.entrances closest_to self;
			goto_entrance <- true;
			location <- any_location_in (one_of(building_entrance).init_place);
			date lunch_time <- date(current_date.year,current_date.month,current_date.day,11, 30) add_seconds rnd(0, 40 #mn);
			bool return_after_lunch <- true;
			agenda_day[lunch_time] <-first(eating_outside_act) ;
			lunch_time <- lunch_time add_seconds rnd(30 #mn, 90 #mn);
			if return_after_lunch{agenda_day[lunch_time] <-first(working);}
			agenda_day[date(current_date.year,current_date.month,current_date.day,rnd(16,22), rnd(59),rnd(59))] <- first(going_home_act);
		}
	}
	action initialize_pedestrian_model {
		
		ask pedestrian_path {
			float dist <- max(1.0, self distance_to (wall closest_to self));
			do initialize obstacles:[wall] distance: dist;
			free_space <- free_space.geometries first_with (each overlaps shape);
		}
	}
}

species people skills:[escape_pedestrian] parallel:use_parallel{
	bool is_outside;
	room working_place;
	room target_room;
	point target;
	place_in_room target_place;
	bool has_place <- false;
	bool goto_entrance <- false;
	bool go_outside_room <- false;
	activity current_activity;
	rgb color <- #yellow;
	map<date, activity> agenda_day;
	int counter <- 0;
	
	float speed <- min(5,gauss(4,1)) #km/#h;
	bool is_slow <- false update: false;
	bool is_slow_real <- false;
	
	reflex define_activity when: not empty(agenda_day) and after(agenda_day.keys[0]){
		if(target_place != nil and (has_place) ) {target_room.available_places << target_place;}
		string n <- current_activity = nil ? "" : copy(current_activity.name);
		current_activity <- agenda_day.values[0];
		agenda_day >> first(agenda_day);
		target <- target_room.entrances closest_to self;
		target_room <- current_activity.get_place(self);
		go_outside_room <- true;
		is_outside <- false;
		goto_entrance <- false;
		target_place <- nil;
	}
	
	reflex go_to_activity when:target!=nil{
		bool arrived <- false;
		if goto_entrance {
			if (final_target = nil) {
				do compute_virtual_path pedestrian_graph:pedestrian_network final_target: target ;
			}
			point prev_loc <- copy(location);
			do walk;
			float r_s <- prev_loc distance_to location;
			is_slow <- r_s < (speed/coeff_speed_slow);
			if (is_slow) {
				counter <- counter + 1;
				if (counter >= min_num_step_bottleneck) {
					is_slow_real <- true;
				}
			} else {
				is_slow_real <- false;
				counter <- 0;
			}
			
			arrived <- final_target = nil;
			if (arrived) {
				is_slow_real <- false;
				counter <- 0;
			}
		}
		else {
			do goto target: target;
			arrived <- location = target;
		}
		if(arrived) {
			if (go_outside_room) {
				target <- target_room.entrances closest_to self;
				
				go_outside_room <- false;
				goto_entrance <- true;
			}
			else if (goto_entrance) {
				target_place <- target_room.get_target(self);
				if target_place != nil {
					target <- target_place.location;
					goto_entrance <- false;
				} else {
					room tr <- current_activity.get_place(self);
					if (tr != nil ) {
						target_room <- tr;
						target <- target_room.entrances closest_to self;
					}
				}
			} else {
				has_place <- true;
				target <- nil;
				if (species(target_room) = building_entrance) {
					is_outside <- true;
				}
				if (current_activity.name = going_home) {
					do die;
				}
			}	
		}
	}
	
	aspect default{
		if not is_outside{
			draw circle(0.2) color:color;
		}
	}
	
}

species activity {
	list<room> activity_places;
	
	room get_place(people p) {
		if flip(0.3) {
			return one_of(activity_places with_max_of length(each.available_places));
		} else {
			list<room> rs <- (activity_places where not empty(each.available_places));
			if empty(rs) {
				rs <- activity_places;
			}
			return rs closest_to p;
		}
	}
	
}

species working parent: activity {
	
	room get_place(people p) {
		return p.working_place;
	}
}

species going_home_act parent: activity  {
	string name <- going_home;
	room get_place(people p) {
		return building_entrance closest_to p;
	}
}

species eating_outside_act parent: activity  {
	string name <- eating_outside;
	room get_place(people p) {
		return building_entrance closest_to p;
	}
}

species room{
	
	int nb_affected;
	pedestrian_path closest_path;
	string type;
	list<point> entrances;
	list<place_in_room> places;
	list<place_in_room> available_places;
	geometry init_place;
	
	action initialization{
		map<geometry, place_in_room> pr;
		list<geometry> squares;
		if (scenario = "distance") or (type != offices) {
			squares <-  to_squares(shape, distance_people, true) where (each.location overlaps shape);
		}
		if not empty(squares){
			loop element over:squares{
				create place_in_room{
					location <- element.location;
					pr[element] <- self;
					myself.places << self;
				}
			}
			if empty(places) {
				create place_in_room {
					location <- myself.location;
					myself.places << self;
				}
			} 
		}
		available_places <- copy(places);
	}
	bool is_available {
		return nb_affected < length(places);
	}
	place_in_room get_target(people p){
		place_in_room place <- (available_places with_max_of each.dists);
		available_places >> place;
		return place;
	}
	aspect default{
		draw shape color: standard_color_per_layer[type];
		loop e over: entrances {draw square(0.2) at: {e.location.x,e.location.y,0.001} color: #magenta border: #black;}
		loop p over: available_places {draw square(0.2) at: {p.location.x,p.location.y,0.001} color: #cyan border: #black;}
	}
	aspect available_places_info{
		if(showAvailableDesk and (type="Offices" or type="Meeeting rooms")){
		 	draw string(length(available_places)) at: {location.x-20#px,location.y,1.0} color:#white font:font("Helvetica", 20 , #bold) perspective:false; 	
		} 
	}
	
}

species place_in_room{
	float dists;
}

species wall {
	aspect default {
		draw shape color: #gray;
	}
}

species pedestrian_path skills:[pedestrian_road] frequency: 0{
	aspect default {
		if (display_pedestrian_path) {
			if(display_free_space and free_space != nil) {draw free_space color: #lightpink border: #black;}
			draw shape color: #red width:2;
		}
		
	}
}

species building_entrance parent: room {
	place_in_room get_target(people p){
		return place_in_room closest_to p;
	}
	aspect default {
		draw shape color: standard_color_per_layer[type];
		draw init_place color:#magenta border: #black;
		loop e over: entrances {draw square(0.1) at: e color: #magenta border: #black;}
		loop p over: available_places {draw square(0.1) at: p.location color: #cyan border: #black;}
	}
}

experiment experimento type:gui{
	parameter "fileName:" var: useCase category: 'file' <- "Laboratory" among: ["Laboratory"];
	parameter "Variable of study" var: scenario category:'Initialization'  <- "distance" among: ["distance"];
	parameter "Social distance" var: distance_people category:"Visualization" <- 2.0 min:0.0#m max:5.0#m;
	parameter "Show available desk:" category: "Visualization" var:showAvailableDesk <-false;
	parameter "Draw Pedestrian Path:" category: "Visualization" var:display_pedestrian_path <- false;
	output{
		display GUI type:opengl draw_env:false{
			species room  refresh: false;
			species room aspect: available_places_info refresh: true;
			species wall refresh:false;
			species building_entrance aspect: default;
			species pedestrian_path;
			species people;
			overlay size:{50#px,50#px}{
				draw "0123456789:" at:{0#px,0#px} color:rgb(0,0,0,0) font: font("Arial", 40,#bold);
				draw string(current_date.hour)+":"+string(current_date.minute) at:{150#px,30#px} color:#white font: font("Arial", 40,#bold);
			}
		}
	}	
}