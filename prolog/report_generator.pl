:- module(report_generator, [
    generate_full_report/1, 
    generate_indexed_report/3,
    generate_omegas_from_gaps/1,     % NEW
    omega_from_gap/5                 % NEW - exposed for testing (FIXED: was /4)
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

% Suppress warning - we intentionally override intent_engine:classify_interval/3
:- discontiguous classify_interval/3.

% ============================================================================
% OMEGA GENERATION FROM PERSPECTIVAL GAPS (NEW)
% ============================================================================

%% generate_omegas_from_gaps(+IntervalID)
%  Scans for perspectival gaps and generates Ω variables automatically
%  This is the main entry point for converting gaps into tracked uncertainties
generate_omegas_from_gaps(_IntervalID) :-
    format('~n[OMEGA GENERATION FROM PERSPECTIVAL GAPS]~n'),
    
    % Find all constraints with gaps
    findall(
        omega_entry(OmegaID, Type, Question, Gap),
        (   narrative_ontology:constraint_claim(C, _),
            detect_gap_pattern(C, Gap),
            omega_from_gap(C, Gap, OmegaID, Type, Question)
        ),
        OmegaEntries
    ),
    
    % Report generation
    (   OmegaEntries = []
    ->  format('  No perspectival gaps detected requiring Ω tracking.~n')
    ;   length(OmegaEntries, Count),
        format('  Generated ~w Omega variables from perspectival gaps:~n~n', [Count]),
        forall(member(omega_entry(OID, OType, OQuestion, OGap), OmegaEntries),
               (   format('  Ω: ~w (~w)~n', [OID, OType]),
                   format('     Question: ~w~n', [OQuestion]),
                   format('     Source: ~w~n~n', [OGap])
               ))
    ),
    
    % Store for later use (optional - depends on your workflow)
    forall(member(omega_entry(OID, OType, OQuestion, _), OmegaEntries),
           assert_omega_if_new(OID, OType, OQuestion)).

%% detect_gap_pattern(+Constraint, -GapPattern)
%  Detects specific perspectival gap patterns that require Ω generation
detect_gap_pattern(C, gap(noose_masked_as_rope, TypeP, TypeI)) :-
    % Pattern 1: Powerless sees Noose, Institution sees Rope
    % CRITICAL: Extraction hidden from those with power to change it
    constraint_indexing:constraint_classification(
        C, TypeP, 
        context(agent_power(individual_powerless), _, _, _)
    ),
    constraint_indexing:constraint_classification(
        C, TypeI,
        context(agent_power(institutional), _, _, _)
    ),
    TypeP = noose,
    TypeI = rope,
    !.

detect_gap_pattern(C, gap(mountain_coordination_confusion, TypeP, TypeI)) :-
    % Pattern 2: Powerless sees Mountain, Institution sees Rope
    % DANGER: Institution may cut "optional coordination" that's survival necessity
    constraint_indexing:constraint_classification(
        C, TypeP,
        context(agent_power(individual_powerless), _, _, _)
    ),
    constraint_indexing:constraint_classification(
        C, TypeI,
        context(agent_power(institutional), _, _, _)
    ),
    TypeP = mountain,
    TypeI = rope,
    !.

detect_gap_pattern(C, gap(noose_mountain_confusion, TypeP, TypeI)) :-
    % Pattern 3: Powerless sees Noose, Institution sees Mountain
    % STALEMATE: Institution believes unchangeable, powerless experiences extraction
    constraint_indexing:constraint_classification(
        C, TypeP,
        context(agent_power(individual_powerless), _, _, _)
    ),
    constraint_indexing:constraint_classification(
        C, TypeI,
        context(agent_power(institutional), _, _, _)
    ),
    TypeP = noose,
    TypeI = mountain,
    !.

detect_gap_pattern(C, gap(general_type_mismatch, TypeP, TypeI)) :-
    % Pattern 4: Any other type mismatch between powerless and institutional
    constraint_indexing:constraint_classification(
        C, TypeP,
        context(agent_power(individual_powerless), _, _, _)
    ),
    constraint_indexing:constraint_classification(
        C, TypeI,
        context(agent_power(institutional), _, _, _)
    ),
    TypeP \= TypeI,
    TypeP \= none,
    TypeI \= none.

%% omega_from_gap(+Constraint, +GapPattern, -OmegaID, -Type, -Question)
%  Generates Ω variable from detected gap pattern
%  Returns structured Omega ready for assertion or reporting

% Pattern 1: Noose masked as Rope - EXTRACTION BLINDNESS
omega_from_gap(C, gap(noose_masked_as_rope, noose, rope), OmegaID, conceptual, Question) :-
    format(atom(OmegaID), 'omega_extraction_blindness_~w', [C]),
    format(atom(Question),
        'Constraint ~w appears extractive (Noose) to individuals but functional (Rope) to institutions. Is this: (a) institutions unaware of extraction, (b) institutions benefiting and obscuring, or (c) different legitimate framings? Policy authority depends on answer.',
        [C]),
    !.

% Pattern 2: Mountain/Rope confusion - CATASTROPHIC CUT RISK
omega_from_gap(C, gap(mountain_coordination_confusion, mountain, rope), OmegaID, conceptual, Question) :-
    format(atom(OmegaID), 'omega_cut_safety_~w', [C]),
    format(atom(Question),
        'Constraint ~w appears unchangeable (Mountain) to individuals but optional (Rope) to institutions. If institutions cut this "optional coordination," do individuals have alternatives, or does this destroy their only survival mechanism? Scaffold assessment required before any intervention.',
        [C]),
    !.

% Pattern 3: Noose/Mountain confusion - LEARNED HELPLESSNESS
omega_from_gap(C, gap(noose_mountain_confusion, noose, mountain), OmegaID, conceptual, Question) :-
    format(atom(OmegaID), 'omega_learned_helplessness_~w', [C]),
    format(atom(Question),
        'Constraint ~w appears extractive (Noose) to individuals but unchangeable (Mountain) to institutions. Is this: (a) institutions falsely claiming necessity to preserve extraction, (b) institutions genuinely unable to change despite unfairness, or (c) individuals misidentifying coordination as extraction? Resolution strategy depends on which.',
        [C]),
    !.

% Pattern 4: General mismatch - PERSPECTIVE DEPENDENCY
omega_from_gap(C, gap(general_type_mismatch, TypeP, TypeI), OmegaID, conceptual, Question) :-
    format(atom(OmegaID), 'omega_perspectival_~w', [C]),
    format(atom(Question),
        'Constraint ~w appears as ~w to individuals but ~w to institutions. Which perspective should dominate policy decisions, or does this require a transition mechanism serving both views?',
        [C, TypeP, TypeI]),
    !.

%% assert_omega_if_new(+OmegaID, +Type, +Question)
%  Asserts Omega variable if not already present
%  Prevents duplicate Omegas across multiple runs
assert_omega_if_new(OmegaID, Type, Question) :-
    (   narrative_ontology:omega_variable(OmegaID, _, _)
    ->  true  % Already exists, skip
    ;   assertz(narrative_ontology:omega_variable(OmegaID, Type, Question))
    ).

% ============================================================================
% ORIGINAL REPORT GENERATION (ENHANCED)
% ============================================================================

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
    findall(Kappa, (v3_1_config:level(L), v3_1_coercion_projection:coercion_magnitude(L, Tn, Kappa)), Kappas),
    (   Kappas \= [] 
    ->  sum_list(Kappas, Sum), length(Kappas, N), AvgK is Sum / N,
        format('~nAggregate Magnitude (Kappa) at Tn: ~2f~n', [AvgK])
    ;   format('~nAggregate Magnitude (Kappa): DATA_INSUFFICIENT~n')
    ),
    
    % --- SECTION 5: PERSPECTIVAL GAP ANALYSIS ---
    format('~n[PERSPECTIVAL GAP ANALYSIS]~n'),
    (   forall(narrative_ontology:constraint_claim(C, _),
               perspectival_gap_audit(C))
    ;   true
    ),
    
    % --- SECTION 6: OMEGA GENERATION (NEW) ---
    generate_omegas_from_gaps(IntervalID),
    
    format('====================================================~n').

% ============================================================================
% INDEXED REPORT GENERATION
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
extract_constraints(Text, Constraints) :-
    atom_codes(Text, Codes),
    findall(C, 
        (constraint_keyword(C), 
         atom_codes(C, CCode),
         sublist(CCode, Codes)),
        ConstraintsWithDups),
    sort(ConstraintsWithDups, Constraints).

%% constraint_keyword(?ConstraintID)
%  Known constraint identifiers to look for in text
constraint_keyword(catholic_church_1200).
constraint_keyword(property_rights_2025).

%% perspectival_gap_audit(+Constraint)
%  Scans for contradictions between analytical, institutional, and powerless views
perspectival_gap_audit(C) :-
    format('~n  Analysis for Constraint: ~w~n', [C]),
    
    % Find Powerless Perspective
    (   constraint_indexing:constraint_classification(C, TypeP, context(agent_power(individual_powerless), _, _, _))
    ->  true ; TypeP = none),
    
    % Find Institutional Perspective
    (   constraint_indexing:constraint_classification(C, TypeI, context(agent_power(institutional), _, _, _))
    ->  true ; TypeI = none),

    % Report Critical Gaps
    (   TypeP == mountain, TypeI == rope 
    ->  format('    ! GAP: Institutional "Rope" (Coordination) appears as "Mountain" (Natural Law) to the Powerless.~n'),
        format('    ! DANGER: Cutting this constraint could be catastrophic for powerless individuals.~n')
    ;   true),
    
    (   TypeP == noose, TypeI == rope
    ->  format('    ! ALERT: Extractive "Noose" is masked as a functional "Rope" in the Institutional view.~n'),
        format('    ! BLINDNESS: Institutions may not see the extraction they benefit from.~n')
    ;   true),
    
    (   TypeP == noose, TypeI == mountain
    ->  format('    ! STALEMATE: Institutions believe constraint is unchangeable while individuals experience extraction.~n')
    ;   true),
    
    % Report all perspectives
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
%  Extracts logical friction points for recursive LLM refinement
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
               format('  - ~w (~w): ~w~n', [OID, Type, Desc]))
    ;   format('  - None detected.~n')
    ),

    format('~n### END REFINEMENT MANIFEST ###~n').

% ============================================================================
% HELPER PREDICATES
% ============================================================================

%% sublist(+Sublist, +List)
%  True if Sublist appears as a contiguous subsequence in List
sublist([], _).
sublist([H|T], [H|T2]) :- !, sublist(T, T2).
sublist(Sub, [_|T]) :- sublist(Sub, T).

%% report_constraint_signature(+C)
%  Reports structural signature for a constraint
report_constraint_signature(C) :-
    drl_core:dr_signature(C, Signature),
    structural_signatures:signature_confidence(C, Signature, Confidence),
    structural_signatures:explain_signature(C, Signature, Explanation),
    format('  ~20w: ~20w (confidence: ~w)~n', [C, Signature, Confidence]),
    (   Signature \= ambiguous
    ->  format('    → ~w~n', [Explanation])
    ;   true
    ).

% ============================================================================
% CLASSIFICATION HELPERS
% ============================================================================

%% classify_interval(+IntervalID, -Pattern, -Confidence)
%  Placeholder for interval classification
%  Should be implemented based on your domain logic
classify_interval(_IntervalID, stable, high) :-
    % Default implementation - replace with actual logic
    true.

/* ================================================================
   VERSION INFO
   ================================================================ */

/*
VERSION: v4.0 - OMEGA GENERATION LAYER

NEW FEATURES:
- generate_omegas_from_gaps/1: Automatic Ω generation from perspectival gaps
- detect_gap_pattern/2: Pattern matching for critical gap types
- omega_from_gap/5: Structured Ω creation with typed questions
- assert_omega_if_new/3: Duplicate prevention

GAP PATTERNS RECOGNIZED:
1. noose_masked_as_rope: Extraction invisible to power (conceptual Ω)
2. mountain_coordination_confusion: Catastrophic cut risk (conceptual Ω)
3. noose_mountain_confusion: Learned helplessness (conceptual Ω)
4. general_type_mismatch: Perspective dependency (conceptual Ω)

OMEGA TYPES USED:
- conceptual: Framework/perspective conflicts requiring resolution
- empirical: (reserved for data gaps)
- preference: (reserved for value conflicts)

INTEGRATION:
- Called automatically in generate_full_report/1
- Omegas stored in narrative_ontology namespace
- Compatible with existing LLM feedback system
*/
