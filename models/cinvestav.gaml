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
	shape_file pedestrian_path_shape_file <- shape_file(dataset_path+ useCase+"/pedestrian_path.shp", gama.pref_gis_default_crs);
	string scenario;
	float distance_people;
	float separator_proba <- 0.0;
	bool showAvailableDesk<-false;
	graph pedestrian_network;
	geometry shape <- envelope(the_dxf_file);
	init{
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
	}
}

species room{
	
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
species pedestrian_path skills: [pedestrian_road] frequency: 0{

}
species people{}
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
	output{
		display GUI type:opengl {
			species room  refresh: false;
			species room aspect: available_places_info refresh: true;
		}
	}	
}