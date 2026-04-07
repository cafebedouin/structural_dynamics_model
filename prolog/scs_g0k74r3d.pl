%% Auto-generated structural config sensitivity overlay — DO NOT EDIT
:- use_module(config).
:- (retract(config:param(snare_chi_floor, _)) -> true ; true),
   asserta(config:param(snare_chi_floor, 0.45)).
:- [stack].
:- [product_site_export].
:- product_site_export:run_product_export_to('/home/scott/bin/structural_dynamics_model/outputs/scs_out_xyna7un_.json'), halt.
