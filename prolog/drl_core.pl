:- module(drl_core, [
    dr_type/2,            
    dr_mismatch/3,        
    dr_action/2,
    dr_signature/2,
    % NEW: Indexed classification (re-exports from constraint_indexing)
    constraint_classification/3,   % Indexed version
    constraint_claim_indexed/2,    % Backward compatible (renamed to avoid collision)
    multi_index_report/1,          % Analysis utility
    compare_perspectives/2,        % Comparison utility
    discover_my_context/1          % Interactive context builder
]).

:- use_module(narrative_ontology).
:- use_module(v3_1_config).
:- use_module(structural_signatures).  % v3.2 signature detection
:- use_module(constraint_indexing).
:- use_module(constraint_instances).

% Re-export indexed classification predicates from constraint_indexing
:- reexport(constraint_indexing, [
    constraint_classification/3,
    constraint_claim_indexed/2,
    multi_index_report/1,
    compare_perspectives/2,
    discover_my_context/1
]).

% --- Classification Logic ---

is_mountain(C) :-
    v3_1_config:param(mountain_suppression_ceiling, Ceil),
    narrative_ontology:constraint_metric(C, suppression_requirement, E), E =< Ceil.

is_rope(C) :-
    v3_1_config:param(rope_extraction_ceiling, XCeil),
    narrative_ontology:constraint_metric(C, extractiveness, X), X =< XCeil.

is_noose(C) :-
    v3_1_config:param(noose_extraction_floor, XFloor),
    v3_1_config:param(noose_suppression_floor, EFloor),
    narrative_ontology:constraint_metric(C, extractiveness, X), X >= XFloor,
    narrative_ontology:constraint_metric(C, suppression_requirement, E), E >= EFloor.

is_tangled_rope(C) :-
    v3_1_config:param(rope_extraction_ceiling, RopeX),
    v3_1_config:param(tangled_rope_extraction_ceil, TangledX),
    narrative_ontology:constraint_metric(C, extractiveness, X), 
    X > RopeX, X =< TangledX.

is_zombie(C) :-
    v3_1_config:param(zombie_extraction_ceiling, XCeil),
    narrative_ontology:constraint_metric(C, extractiveness, X), X =< XCeil,
    narrative_ontology:constraint_metric(C, suppression_requirement, E), E > XCeil.

% Canonical Mapping (v3.2: Signature-aware)
% First get metric-based type, then check if signature overrides it
dr_type(C, Type) :-
    metric_based_type(C, MetricType),
    structural_signatures:integrate_signature_with_modal(C, MetricType, Type).

% Metric-based classification (unchanged from v3.1)
metric_based_type(C, mountain)     :- is_mountain(C), !.
metric_based_type(C, noose)        :- is_noose(C), !.
metric_based_type(C, rope)         :- is_rope(C), !.
metric_based_type(C, tangled_rope) :- is_tangled_rope(C), !.
metric_based_type(C, zombie)       :- is_zombie(C), !.
metric_based_type(_, unknown).

% --- Action Routing (Grouped to avoid Discontiguous warning) ---

dr_action(C, accept)      :- dr_type(C, mountain).
dr_action(C, maintain)    :- dr_type(C, rope).
dr_action(C, reform)      :- dr_type(C, tangled_rope).
dr_action(C, cut)         :- dr_type(C, noose).
dr_action(C, bypass)      :- dr_type(C, zombie).
dr_action(C, investigate) :- dr_type(C, unknown).

% --- DRL Error Taxonomy ---

dr_mismatch(C, type_1_false_mountain, severe) :-
    narrative_ontology:constraint_claim(C, mountain),
    \+ is_mountain(C).

dr_mismatch(C, type_3_noose_as_rope, severe) :-
    narrative_ontology:constraint_claim(C, rope),
    is_noose(C).

dr_mismatch(C, type_5_zombie_as_noose, moderate) :-
    narrative_ontology:constraint_claim(C, noose),
    is_zombie(C).

/* ================================================================
   v3.2 STRUCTURAL SIGNATURE DETECTION
   ================================================================ */

%% dr_signature(+Constraint, -Signature)
%  Detects structural signature: natural_law | coordination_scaffold | 
%  constructed_constraint | ambiguous
dr_signature(C, Signature) :-
    structural_signatures:constraint_signature(C, Signature).

