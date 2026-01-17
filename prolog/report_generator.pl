:- module(report_generator, [generate_full_report/1, generate_indexed_report/3]).

:- use_module(library(lists)).        % Required for sum_list/2
:- use_module(narrative_ontology).
:- use_module(v3_1_config).
:- use_module(intent_engine).
:- use_module(v3_1_coercion_projection).
:- use_module(pattern_analysis).
:- use_module(constraint_bridge).
:- use_module(modal_evaluator).
:- use_module(drl_core).          
:- use_module(uke_dr_bridge).
:- use_module(structural_signatures).  % v3.2 signature detection
:- use_module(constraint_indexing).    % NEW: Indexed classification

generate_full_report(IntervalID) :-
    interval(IntervalID, T_start, Tn),
    classify_interval(IntervalID, Pattern, Conf),
    
    format('~n~n====================================================~n'),
    format('   DEFERENTIAL REALISM (DR) EXECUTIVE SUMMARY      ~n'),
    format('====================================================~n'),
    format('Timeline:       ~w to ~w~n', [T_start, Tn]),
    format('Structural Pattern: ~w~n', [Pattern]),
    format('Confidence:     ~w~n', [Conf]),
    
    % --- SECTION 1: DRL ONTOLOGY AUDIT (REALITY VS. CLAIM) ---
    format('~n[CONSTRAINT INVENTORY: REALITY AUDIT]~n'),
    format('  ~20s | ~12s | ~12s | ~8s~n', ['Constraint', 'Claimed', 'Actual', 'Action']),
    format('  ----------------------------------------------------------------------~n'),
    % Wrap forall to ensure report continues even if no constraints found
    (   forall(narrative_ontology:constraint_claim(C, Claimed),
               ( drl_core:dr_type(C, Actual),
                 drl_core:dr_action(C, Action),
                 format('  ~20w | ~12w | ~12w | ~8w~n', [C, Claimed, Actual, Action])
               ))
    ;   true
    ),

    % --- SECTION 2: META-LOGICAL AUDIT ---
    format('~n[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]~n'),
    (   setof((C, Err, Sev), drl_core:dr_mismatch(C, Err, Sev), Errors)
    ->  forall(member((C, Err, Sev), Errors),
               format('  ! ALERT [~w]: ~w detected for ~w~n', [Sev, Err, C]))
    ;   format('  No classification errors detected. System is Ontologically Coherent.~n')
    ),

    % --- SECTION 2b: STRUCTURAL SIGNATURE ANALYSIS (v3.2) ---
    format('~n[STRUCTURAL SIGNATURE ANALYSIS]~n'),
    (   catch(
            forall(narrative_ontology:constraint_claim(C, _Claim),
                   report_constraint_signature(C)),
            Error,
            format('  [FAIL] Exception: ~w~n', [Error]))
    ;   true
    ),

    % --- SECTION 3: UKE_DR FEASIBILITY BRIDGE ---
    format('~n[UKE_DR FEASIBILITY BRIDGE]~n'),
    format('  ~40s | ~12s~n', ['Recommendation', 'UKE Status']),
    format('  ----------------------------------------------------------------------~n'),
    % Use soft-failure pattern to prevent report crash on missing recommendation data
    (   forall(narrative_ontology:recommendation(RID, Summary),
               ( ( uke_dr_bridge:uke_status(RID, UKEStatus, Reasons) 
                 -> format('  - ~40w | ~12w~n', [Summary, UKEStatus]),
                    forall(member(R, Reasons), format('    > ~w~n', [R]))
                 ;  format('  - ~40w | ~12s~n', [Summary, 'DATA_MISSING'])
                 )
               ))
    ;   true
    ),
    
    % --- SECTION 4: KINETIC MAGNITUDE ---
    % Use ~2f for float precision to avoid existence_error [cite: 99]
    findall(Kappa, (v3_1_config:level(L), v3_1_coercion_projection:coercion_magnitude(L, Tn, Kappa)), Kappas),
    (   Kappas \= [] 
    ->  sum_list(Kappas, Sum), length(Kappas, N), AvgK is Sum / N,
        format('~nAggregate Magnitude (Kappa) at Tn: ~2f~n', [AvgK])
    ;   format('~nAggregate Magnitude (Kappa): DATA_INSUFFICIENT~n')
    ),
    
    % --- SECTION 5: PERSPECTIVE GAP ---
    format('~n[PERSPECTIVAL GAP ANALYSIS]~n'),
    (   forall(narrative_ontology:constraint_claim(C, _),
               perspectival_gap_audit(C))
    ;   true
    ),
    
    format('====================================================~n').

% ============================================================================
% REPORT GENERATION
% ============================================================================

%% generate_indexed_report(+Text, +Context, -Report)
%  Generate a constraint analysis report from a specific perspective
generate_indexed_report(Text, Context, Report) :-
    extract_constraints(Text, Constraints),
    maplist(classify_with_context(Context), Constraints, Classifications),
    format_indexed_report(Classifications, Context, Report).

%% classify_with_context(+Context, +Constraint, -Classification)
%  Classify a constraint from a specific perspective
classify_with_context(Context, Constraint, classification(Constraint, Type)) :-
    constraint_indexing:constraint_classification(Constraint, Type, Context).

%% extract_constraints(+Text, -Constraints)
%  Extract constraint identifiers from text
%  
%  This is a simple implementation that looks for known constraint keywords.
%  For production use, integrate with narrative_ontology extraction.
extract_constraints(Text, Constraints) :-
    atom_codes(Text, Codes),
    findall(C, 
        (constraint_keyword(C), 
         atom_codes(C, CCode),
         sublist(CCode, Codes)),
        ConstraintsWithDups),
    sort(ConstraintsWithDups, Constraints).  % Remove duplicates

%% constraint_keyword(?ConstraintID)
%  Known constraint identifiers to look for in text
%  Add more as you create indexed classifications
constraint_keyword(catholic_church_1200).
constraint_keyword(property_rights_2025).

%% perspectival_gap_audit(+Constraint)
% Scans for contradictions between analytical, institutional, and powerless views.
perspectival_gap_audit(C) :-
    format('~n  Analysis for Constraint: ~w~n', [C]),
    % Find Powerless Perspective
    (   constraint_indexing:constraint_classification(C, TypeP, context(agent_power(individual_powerless), _, _, _))
    ->  true ; TypeP = none),
    
    % Find Institutional Perspective
    (   constraint_indexing:constraint_classification(C, TypeI, context(agent_power(institutional), _, _, _))
    ->  true ; TypeI = none),

    % Report Gaps
    (   TypeP == mountain, TypeI == rope 
    ->  format('    ! GAP: Institutional "Rope" (Coordination) appears as "Mountain" (Natural Law) to the Powerless.~n')
    ;   true),
    (   TypeP == noose, TypeI == rope
    ->  format('    ! ALERT: Extractive "Noose" is masked as a functional "Rope" in the Institutional view.~n')
    ;   true),
    
    format('    - Individual (Powerless): ~w~n', [TypeP]),
    format('    - Institutional (Manager): ~w~n', [TypeI]).

%% format_indexed_report(+Classifications, +Context, -Report)
%  Format classifications into a human-readable report
format_indexed_report(Classifications, Context, Report) :-
    Context = context(agent_power(Power), time_horizon(Time), 
                      exit_options(Exit), spatial_scope(Scope)),
    with_output_to(atom(Report),
        (format('~n[INDEXED CONSTRAINT ANALYSIS]~n'),
         format('Perspective: ~w / ~w / ~w / ~w~n~n', [Power, Time, Exit, Scope]),
         format('Classifications:~n'),
         forall(member(classification(C, T), Classifications),
                format('  ~w: ~w~n', [C, T])))).

%% generate_llm_feedback(+IntervalID)
% Extracts logical friction points for recursive LLM refinement.
generate_llm_feedback(IntervalID) :-
    format('~n### START LLM REFINEMENT MANIFEST: ~w ###~n', [IntervalID]),
    
    % 1. PERSPECTIVAL GAPS (Political Flashpoints)
    format('~n[PERSPECTIVAL_GAPS]~n'),
    (   forall(narrative_ontology:constraint_claim(C, _),
               (   constraint_indexing:constraint_classification(C, TypeP, context(agent_power(individual_powerless), _, _, _)),
                   constraint_indexing:constraint_classification(C, TypeI, context(agent_power(institutional), _, _, _)),
                   TypeP \= TypeI,
                   format('  - Constraint "~w": Individual sees ~w, but Institution sees ~w.~n', [C, TypeP, TypeI])
               ))
    ;   true
    ),

    % 2. ONTOLOGICAL MISMATCHES (Logic Errors)
    format('~n[ONTOLOGICAL_MISMATCHES]~n'),
    (   setof((C, Err, Sev), drl_core:dr_mismatch(C, Err, Sev), Errors)
    ->  forall(member((C, Err, Sev), Errors),
               format('  - ~w: [~w] ~w detected. The claimed status does not match the observed metrics.~n', [C, Sev, Err]))
    ;   format('  - None detected.~n')
    ),

    % 3. UNRESOLVED OMEGAS (Reasoning Blockers)
    format('~n[UNRESOLVED_OMEGAS]~n'),
    (   setof((OID, Type, Desc), narrative_ontology:omega_variable(OID, Type, Desc), Omegas)
    ->  forall(member((OID, Type, Desc), Omegas),
               format('  - ~w (~w): ~w. This variable lacks empirical grounding.~n', [OID, Type, Desc]))
    ;   format('  - None detected.~n')
    ),

    format('~n### END REFINEMENT MANIFEST ###~n').


%% sublist(+Sublist, +List)
%  True if Sublist appears as a contiguous subsequence in List
sublist([], _).
sublist([H|T], [H|T2]) :- !, sublist(T, T2).
sublist(Sub, [_|T]) :- sublist(Sub, T).

/* ================================================================
   HELPER: SIGNATURE REPORTING (v3.2)
   ================================================================ */

report_constraint_signature(C) :-
    % Get structural signature
    drl_core:dr_signature(C, Signature),
    
    % Get confidence
    structural_signatures:signature_confidence(C, Signature, Confidence),
    
    % Get explanation
    structural_signatures:explain_signature(C, Signature, Explanation),
    
    % Report in compact format
    format('  ~20w: ~20w (confidence: ~w)~n', [C, Signature, Confidence]),
    
    % Only show explanation for non-ambiguous signatures
    (   Signature \= ambiguous
    ->  format('    → ~w~n', [Explanation])
    ;   true
    ).
