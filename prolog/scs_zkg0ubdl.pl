%% Auto-generated structural config sensitivity overlay — DO NOT EDIT
:- use_module(config).
:- (retract(config:param(site_mode, _)) -> true ; true),
   asserta(config:param(site_mode, product)).
:- (retract(config:param(sigmoid_steepness, _)) -> true ; true),
   asserta(config:param(sigmoid_steepness, 12)).
:- [stack].
:- bifurcation_export:export_product_classifications, halt.
