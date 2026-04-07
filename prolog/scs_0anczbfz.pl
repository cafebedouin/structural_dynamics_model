%% Auto-generated structural config sensitivity overlay — DO NOT EDIT
:- use_module(config).
:- (retract(config:param(sigmoid_steepness, _)) -> true ; true),
   asserta(config:param(sigmoid_steepness, 3)).
:- [stack].
:- [product_site_export].
:- product_site_export:run_product_export_to('/home/scott/bin/structural_dynamics_model/outputs/scs_out_3098uo09.json'), halt.
