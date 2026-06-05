:- module(modal_evaluator, [
    classify_modal/2,
    detect_type_error/3
]).

:- use_module(narrative_ontology).
:- use_module(v3_1_config). 

%% classify_modal(+ConstraintID, -ModalType)
% Rule M: Mountain Identification (Decay=0, Enforcement=0)
classify_modal(C, mountain) :-
    narrative_ontology:constraint_metric(C, suppression_requirement, 0.0),
    narrative_ontology:constraint_metric(C, snap_back_potential, 0.0), !.

% Rule N: Snare Identification (High Enforcement, High Snap-back)
classify_modal(C, snare) :-
    narrative_ontology:constraint_metric(C, suppression_requirement, E), E > 0.5,
    narrative_ontology:constraint_metric(C, snap_back_potential, S), S > 0.5, !.

%% detect_type_error(+C, -ErrorType, -Severity)
% Section IV: Type I Error (False Mountain)
% UPDATED: Now uses dynamic param to avoid hardcoded mismatch.
detect_type_error(C, type_1_false_mountain, severe) :-
    narrative_ontology:constraint_claim(C, mountain),
    v3_1_config:param(mountain_suppression_ceiling, Ceiling),
    narrative_ontology:constraint_metric(C, suppression_requirement, E), 
    E > Ceiling, !.

% Section IV: Type III Error (Snare Misclassified as Rope)
detect_type_error(C, type_3_extractive_rope, severe) :-
    narrative_ontology:constraint_claim(C, rope),
    narrative_ontology:constraint_metric(C, extractiveness, X), X > 0.7, !.

%% detect_gravity_extraction(+C, -Warning)
% Detects when a Snare is being sold as a Mountain (The "Khatri Move").
detect_gravity_extraction(C, Warning) :-
    narrative_ontology:constraint_claim(C, mountain),
    % Check for high enforcement energy (Suppression)
    narrative_ontology:constraint_metric(C, suppression_requirement, E), E > 0.5,
    Warning = 'GRAVITY EXTRACTION DETECTED: Constraint claimed as Mountain requires active suppression energy.'.

%% verify_piton_status(+C, -Warning)
% Detects if a Piton is still consuming "Maintenance Energy" without utility.
verify_piton_status(C, Warning) :-
    narrative_ontology:constraint_claim(C, piton),
    narrative_ontology:constraint_metric(C, intensity, I), I > 0.1,
    narrative_ontology:constraint_metric(C, extractiveness, X), X == 0.0,
    Warning = 'PITON INEFFICIENCY: High-intensity constraint persists with zero extraction or coordination utility.'.
