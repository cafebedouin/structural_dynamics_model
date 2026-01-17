:- module(report_generator, [generate_full_report/1]).

:- use_module(narrative_ontology).
:- use_module(v3_1_config).
:- use_module(v3_1_coercion_projection).
:- use_module(pattern_analysis).
:- use_module(constraint_bridge).
:- use_module(modal_evaluator).
:- use_module(drl_core).          % The Architect (Ontology)
:- use_module(uke_dr_bridge).     % The Gauge (Feasibility)

/**
 * generate_full_report(+IntervalID)
 * Orchestrates the final diagnostic output, prioritizing DRL integrity.
 */
generate_full_report(IntervalID) :-
    interval(IntervalID, T_start, Tn),
    classify_interval(IntervalID, Pattern, Conf),
    
    format('~n~n====================================================~n'),
    format('   DEFERENTIAL REALISM (DR) EXECUTIVE SUMMARY      ~n'),
    format('====================================================~n'),
    format('Timeline:       ~w to ~w [cite: 39]~n', [T_start, Tn]),
    format('Structural Pattern: ~w [cite: 30]~n', [Pattern]),
    format('Confidence:     ~w [cite: 36]~n', [Conf]),

    % --- SECTION 1: DRL ONTOLOGY AUDIT (REALITY VS. CLAIM) ---
    format('~n[CONSTRAINT INVENTORY: REALITY AUDIT]~n'),
    format('  ~20s | ~12s | ~12s | ~8s~n', ['Constraint', 'Claimed', 'Actual', 'Action']),
    format('  ----------------------------------------------------------------------~n'),
    forall(narrative_ontology:constraint_claim(C, Claimed),
	   ( drl_core:dr_type(C, Actual),
             drl_core:dr_action(C, Action),
             format('  ~20w | ~12w | ~12w | ~8w~n', [C, Claimed, Actual, Action])
	   )),

    % --- SECTION 2: META-LOGICAL ERROR DETECTION ---
    format('~n[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]~n'),
    (   setof((C, Err, Sev), drl_core:dr_mismatch(C, Err, Sev), Errors)
    ->  forall(member((C, Err, Sev), Errors),
               format('  ! ALERT [~w]: ~w detected for ~w ~n', [Sev, Err, C]))
    ;   format('  No classification errors detected. System is Ontologically Coherent.~n')
    ),

    % --- SECTION 3: UKE_DR FEASIBILITY BRIDGE ---
    format('~n[UKE_DR FEASIBILITY BRIDGE]~n'),
    format('  ~40s | ~12s~n', ['Recommendation', 'UKE Status']),
    format('  ----------------------------------------------------------------------~n'),
    forall(narrative_ontology:recommendation(RID, Summary),
	   ( uke_dr_bridge:uke_status(RID, UKEStatus, Reasons),
             format('  - ~40w | ~12w~n', [Summary, UKEStatus]),
             forall(member(R, Reasons), format('    > ~w~n', [R]))
	   )),

    % --- SECTION 4: KINETIC MAGNITUDE (KAPPA) ---
    findall(Kappa, (level(L), v3_1_coercion_projection:coercion_magnitude(L, Tn, Kappa)), Kappas),
    (Kappas \= [] ->
        sum_list(Kappas, Sum), length(Kappas, N), AvgK is Sum / N,
        format('~nAggregate Magnitude (Kappa) at Tn: ~2f [cite: 135]~n', [AvgK])
    ;   true),

    format('====================================================~n').

