:- module(report_generator, [
    generate_full_report/1, 
    generate_indexed_report/3,
    generate_omegas_from_gaps/1,
    omega_from_gap/5                 
]).

:- use_module(library(lists)).
:- use_module(narrative_ontology).
:- use_module(v3_1_config).
:- use_module(intent_engine, except([classify_interval/3])).
:- use_module(v3_1_coercion_projection).
:- use_module(pattern_analysis).
:- use_module(constraint_bridge).
:- use_module(drl_core).          
:- use_module(uke_dr_bridge).
:- use_module(structural_signatures).
:- use_module(constraint_indexing).
:- use_module(isomorphism_engine). % Required for isomorphism audit

% Suppress warning - we intentionally override intent_engine:classify_interval/3
:- discontiguous classify_interval/3.

/* ============================================================================
   1. EXECUTIVE SUMMARY (MAIN ENTRY)
   ============================================================================ */

generate_full_report(IntervalID) :-
    interval(IntervalID, T_start, Tn),
    classify_interval(IntervalID, Pattern, Conf),
    
    format('~n~n====================================================~n'),
    format('   DEFERENTIAL REALISM (DR) EXECUTIVE SUMMARY      ~n'),
    format('====================================================~n'),
    format('Timeline:       ~w to ~w~n', [T_start, Tn]),
    format('Structural Pattern: ~w~n', [Pattern]),
    format('Confidence:     ~w~n', [Conf]),
    
    % --- SECTION 1: DRL INDEXICAL AUDIT ---
    format('~n[CONSTRAINT INVENTORY: INDEXICAL AUDIT]~n'),
    forall(
        narrative_ontology:constraint_claim(C, Claimed),
        (
            format('~n~nConstraint: ~w~n', [C]),
            format('  Claimed Type: ~w~n', [Claimed]),
            format('  Perspectives:~n'),
            forall(
                constraint_indexing:constraint_classification(C, Type, Context),
                (
                    format('    - [~w]: ~w', [Context, Type]),
                    ( Type == Claimed -> format(' (Matches Claim)~n')
                    ; format(' (Mismatch)~n')
                    )
                )
            )
        )
    ),

    % --- SECTION 2: ISOMORPHISM AUDIT ---
    generate_isomorphism_audit(IntervalID),

    % --- SECTION 3: META-LOGICAL AUDIT ---
    format('~n[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]~n'),
    (   setof((C, Err, Sev), drl_core:dr_mismatch(C, Err, Sev), Errors)
    ->  forall(member((C, Err, Sev), Errors),
               format('  ! ALERT [~w]: ~w detected for ~w~n', [Sev, Err, C]))
    ;   format('  No classification errors detected. System is Ontologically Coherent.~n')
    ),

    % --- SECTION 4: STRUCTURAL SIGNATURE ANALYSIS ---
    format('~n[STRUCTURAL SIGNATURE ANALYSIS]~n'),
    (   catch(
            forall(narrative_ontology:constraint_claim(CSig, _Claim),
                   report_constraint_signature(CSig)),
            Error,
            format('  [FAIL] Exception: ~w~n', [Error]))
    ;   true
    ),

    % --- SECTION 5: UKE_DR FEASIBILITY BRIDGE ---
    format('~n[UKE_DR FEASIBILITY BRIDGE]~n'),
    format('  ~40s | ~12s~n', ['Recommendation', 'UKE Status']),
    format('  ----------------------------------------------------------------------~n'),
    (   forall(narrative_ontology:recommendation(RID, Summary),
               ( ( uke_dr_bridge:uke_status(RID, UKEStatus, Reasons) 
                 -> format('  - ~40w | ~12w~n', [Summary, UKEStatus]),
                    forall(member(R, Reasons), format('    > ~w~n', [R]))
                 ;  format('  - ~40w | ~12s~n', [Summary, 'DATA_MISSING'])
                 )
               ))
    ;   true
    ),
    
    % --- SECTION 6: KINETIC MAGNITUDE ---
    findall(Kappa, (v3_1_config:level(L), v3_1_coercion_projection:coercion_magnitude(L, Tn, Kappa)), Kappas),
    (   Kappas \= [] 
    ->  sum_list(Kappas, SumK), length(Kappas, NK), AvgK is SumK / NK,
        format('~nAggregate Magnitude (Kappa) at Tn: ~2f~n', [AvgK])
    ;   format('~nAggregate Magnitude (Kappa): DATA_INSUFFICIENT~n')
    ),
    
    % --- SECTION 7: PERSPECTIVAL GAP ANALYSIS ---
    format('~n[PERSPECTIVAL GAP ANALYSIS]~n'),
    (   forall(narrative_ontology:constraint_claim(CGap, _),
               perspectival_gap_audit(CGap))
    ;   true
    ),
    
    % --- SECTION 8: OMEGA GENERATION ---
    generate_omegas_from_gaps(IntervalID),
    format('====================================================~n').

/* ============================================================================
   2. ISOMORPHISM LOGIC
   ============================================================================ */

generate_isomorphism_audit(IntervalID) :-
    format('~n[CROSS-DOMAIN ISOMORPHISM & RISK AUDIT: ~w]~n', [IntervalID]),
    (   setof(iso(C, Twin, Score, Type),
              (narrative_ontology:affects_constraint(IntervalID, C),
               isomorphism_engine:find_high_risk_isomorphism(C, Twin, Score),
               drl_core:dr_type(C, Type)),
              Isos)
    ->  display_isomorphisms(Isos)
    ;   format('  No high-risk isomorphisms detected for current constraints.~n')
    ).

display_isomorphisms([]).
display_isomorphisms([iso(C, T, S, Ty)|Rest]) :-
    format('  ! ALERT: ~w (~w) is a Structural Twin to ~w (Score: ~2f)~n', [C, Ty, T, S]),
    format('    > Strategy: Search for ~w resolutions in KB.~n', [T]),
    display_isomorphisms(Rest).

/* ============================================================================
   3. OMEGA GENERATION
   ============================================================================ */

generate_omegas_from_gaps(IntervalID) :-
    format('~n[OMEGA GENERATION FROM PERSPECTIVAL GAPS: ~w]~n', [IntervalID]),
    findall(
        omega_entry(OmegaID, Type, Question, Gap),
        (   narrative_ontology:affects_constraint(IntervalID, C), 
            detect_gap_pattern(C, Gap),
            omega_from_gap(C, Gap, OmegaID, Type, Question)
        ),
        OmegaEntries
    ),
    process_omega_entries(OmegaEntries).

process_omega_entries([]) :- 
    format('  No perspectival gaps detected requiring Ω tracking.~n').
process_omega_entries(OmegaEntries) :-
    OmegaEntries \= [],
    length(OmegaEntries, Count),
    format('  Generated ~w Omega variables from perspectival gaps:~n~n', [Count]),
    forall(member(omega_entry(OID, OType, OQuestion, OGap), OmegaEntries),
           (   format('  Ω: ~w (~w)~n', [OID, OType]),
               format('     Question: ~w~n', [OQuestion]),
               format('     Source: ~w~n~n', [OGap]),
               assert_omega_if_new(OID, OType, OQuestion)
           )).

% detect_gap_pattern and omega_from_gap logic remains unchanged...
% [Included below for completeness in your file]

detect_gap_pattern(C, gap(noose_masked_as_rope, TypeP, TypeI)) :-
    constraint_indexing:constraint_classification(C, TypeP, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(C, TypeI, context(agent_power(institutional), _, _, _)),
    TypeP = noose, TypeI = rope, !.

detect_gap_pattern(C, gap(mountain_coordination_confusion, TypeP, TypeI)) :-
    constraint_indexing:constraint_classification(C, TypeP, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(C, TypeI, context(agent_power(institutional), _, _, _)),
    TypeP = mountain, TypeI = rope, !.

detect_gap_pattern(C, gap(noose_mountain_confusion, TypeP, TypeI)) :-
    constraint_indexing:constraint_classification(C, TypeP, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(C, TypeI, context(agent_power(institutional), _, _, _)),
    TypeP = noose, TypeI = mountain, !.

detect_gap_pattern(C, gap(general_type_mismatch, TypeP, TypeI)) :-
    constraint_indexing:constraint_classification(C, TypeP, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(C, TypeI, context(agent_power(institutional), _, _, _)),
    TypeP \= TypeI, TypeP \= none, TypeI \= none.

omega_from_gap(C, gap(noose_masked_as_rope, noose, rope), OmegaID, conceptual, Question) :-
    format(atom(OmegaID), 'omega_extraction_blindness_~w', [C]),
    format(atom(Question), 'Constraint ~w appears extractive (Noose) to individuals but functional (Rope) to institutions...', [C]), !.

omega_from_gap(C, gap(mountain_coordination_confusion, mountain, rope), OmegaID, conceptual, Question) :-
    format(atom(OmegaID), 'omega_cut_safety_~w', [C]),
    format(atom(Question), 'Constraint ~w appears unchangeable (Mountain) to individuals but optional (Rope) to institutions...', [C]), !.

omega_from_gap(C, gap(noose_mountain_confusion, noose, mountain), OmegaID, conceptual, Question) :-
    format(atom(OmegaID), 'omega_learned_helplessness_~w', [C]),
    format(atom(Question), 'Constraint ~w appears extractive (Noose) to individuals but unchangeable (Mountain) to institutions...', [C]), !.

omega_from_gap(C, gap(general_type_mismatch, TypeP, TypeI), OmegaID, conceptual, Question) :-
    format(atom(OmegaID), 'omega_perspectival_~w', [C]),
    format(atom(Question), 'Constraint ~w appears as ~w to individuals but ~w to institutions...', [C, TypeP, TypeI]), !.

assert_omega_if_new(OmegaID, Type, Question) :-
    (   narrative_ontology:omega_variable(OmegaID, _, _)
    ->  true
    ;   assertz(narrative_ontology:omega_variable(OmegaID, Type, Question))
    ).

/* ============================================================================
   4. INDEXED REPORTING & AUDITS
   ============================================================================ */

perspectival_gap_audit(C) :-
    format('~n  Analysis for Constraint: ~w~n', [C]),
    (constraint_indexing:constraint_classification(C, TypeP, context(agent_power(individual_powerless), _, _, _)) -> true ; TypeP = none),
    (constraint_indexing:constraint_classification(C, TypeI, context(agent_power(institutional), _, _, _)) -> true ; TypeI = none),
    (TypeP == mountain, TypeI == rope -> format('    ! GAP: Institutional "Rope" appears as "Mountain" to Powerless.~n') ; true),
    (TypeP == noose, TypeI == rope -> format('    ! ALERT: Extractive "Noose" is masked as functional "Rope".~n') ; true),
    format('    - Individual (Powerless): ~w~n', [TypeP]),
    format('    - Institutional (Manager): ~w~n', [TypeI]).

report_constraint_signature(C) :-
    drl_core:dr_signature(C, Signature),
    structural_signatures:signature_confidence(C, Signature, Confidence),
    structural_signatures:explain_signature(C, Signature, Explanation),
    format('  ~20w: ~20w (confidence: ~w)~n', [C, Signature, Confidence]),
    (Signature \= ambiguous -> format('    → ~w~n', [Explanation]) ; true).

% Remaining placeholders (generate_indexed_report, extract_constraints, generate_llm_feedback, sublist, etc.) 
% should be placed here as top-level predicates...

generate_indexed_report(Text, Context, Report) :-
    extract_constraints(Text, Constraints),
    maplist(classify_with_context(Context), Constraints, Classifications),
    format_indexed_report(Classifications, Context, Report).

classify_with_context(Context, Constraint, classification(Constraint, Type)) :-
    constraint_indexing:constraint_classification(Constraint, Type, Context).

extract_constraints(Text, Constraints) :-
    atom_codes(Text, Codes),
    findall(C, (constraint_keyword(C), atom_codes(C, CCode), sublist(CCode, Codes)), Cs),
    sort(Cs, Constraints).

constraint_keyword(catholic_church_1200).
constraint_keyword(property_rights_2025).

format_indexed_report(Classifications, Context, Report) :-
    Context = context(agent_power(Power), time_horizon(Time), exit_options(Exit), spatial_scope(Scope)),
    with_output_to(atom(Report),
        (format('~n[INDEXED CONSTRAINT ANALYSIS]~n'),
         format('Perspective: ~w / ~w / ~w / ~w~n~n', [Power, Time, Exit, Scope]),
         format('Classifications:~n'),
         forall(member(classification(C, T), Classifications), format('  ~w: ~w~n', [C, T])))).

generate_llm_feedback(IntervalID) :-
    format('~n### START LLM REFINEMENT MANIFEST: ~w ###~n', [IntervalID]),
    format('~n[PERSPECTIVAL_GAPS]~n'),
    (forall(narrative_ontology:constraint_claim(C, _),
           (constraint_indexing:constraint_classification(C, TypeP, context(agent_power(individual_powerless), _, _, _)),
            constraint_indexing:constraint_classification(C, TypeI, context(agent_power(institutional), _, _, _)),
            TypeP \= TypeI, format('  - Constraint "~w": Individual sees ~w, but Institution sees ~w.~n', [C, TypeP, TypeI]))) ; true),
    format('~n[ONTOLOGICAL_MISMATCHES]~n'),
    (setof((CM, Err, Sev), drl_core:dr_mismatch(CM, Err, Sev), Errors) -> forall(member((CM, Err, Sev), Errors), format('  - ~w: [~w] ~w detected.~n', [CM, Sev, Err])) ; format('  - None detected.~n')),
    format('~n[UNRESOLVED_OMEGAS]~n'),
    (setof((OID, OTy, ODe), narrative_ontology:omega_variable(OID, OTy, ODe), Omegas) -> forall(member((OID, OTy, ODe), Omegas), format('  - ~w (~w): ~w~n', [OID, OTy, ODe])) ; format('  - None detected.~n')),
    format('~n### END REFINEMENT MANIFEST ###~n').

sublist([], _).
sublist([H|T], [H|T2]) :- !, sublist(T, T2).
sublist(Sub, [_|T]) :- sublist(Sub, T).

classify_interval(_IntervalID, stable, high).
