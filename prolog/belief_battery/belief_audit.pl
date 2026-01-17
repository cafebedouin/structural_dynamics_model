% ============================================================================
% MODULE: belief_audit (Enhanced v2.0)
% ============================================================================
% Diagnostic interface for Individual Practical Battery
% Provides detailed reporting, cascade analysis, and extraction detection
% ============================================================================

:- module(belief_audit, [
    audit/1,
    audit_detailed/1,
    audit_all/0,
    find_nooses/0,
    show_cascades/1,
    belief_summary/0,
    compare_to_institution/1
]).

:- use_module(individual_belief_battery).

% --- 1. BASIC AUDIT ---

%% audit(+Belief)
% Quick audit of a single belief
audit(Belief) :-
    format('~n=== QUICK AUDIT: ~w ===~n', [Belief]),
    (belief_classification(Belief, Type) ->
        format('Classification: ~w~n', [Type])
    ;
        format('Classification: UNDEFINED~n')
    ),
    
    (institutional_claim(Belief, InstType) ->
        format('Institutional Claim: ~w~n', [InstType]),
        (Type \= InstType ->
            format('⚠️  CONFLICT DETECTED~n')
        ; true)
    ; true).

% --- 2. DETAILED AUDIT ---

%% audit_detailed(+Belief)
% Comprehensive audit with explanation and cascade analysis
audit_detailed(Belief) :-
    format('~n╔══════════════════════════════════════════════════════════════╗~n'),
    format('║ DETAILED BELIEF AUDIT: ~w~*|~n', [Belief, 32]),
    format('╚══════════════════════════════════════════════════════════════╝~n~n'),
    
    % Context
    my_context(Ctx),
    format('[YOUR CONTEXT]~n'),
    format('  ~w~n~n', [Ctx]),
    
    % Classification
    format('[CLASSIFICATION]~n'),
    (belief_classification(Belief, Type) ->
        format('  Your Belief: ~w~n', [Type])
    ;
        format('  Your Belief: UNDEFINED~n')
    ),
    
    % Explanation
    (belief_explanation(Belief, Explanation) ->
        format('~n[RATIONALE]~n'),
        format('  ~w~n', [Explanation])
    ; true),
    
    % Institutional claim
    format('~n[INSTITUTIONAL POSITION]~n'),
    (institutional_claim(Belief, InstType) ->
        format('  Claimed as: ~w~n', [InstType])
    ;
        format('  No institutional claim recorded~n')
    ),
    
    % Conflict detection
    (type_1_error(Belief) ->
        format('~n[⚠️  EXTRACTION ALERT]~n'),
        format('  This is a Type I Error (False Mountain)~n'),
        format('  A Noose is being presented as unchangeable natural law~n'),
        format('  Common use: Enable extraction/subjugation~n')
    ; true),
    
    % Cascade analysis
    (cascade_analysis(Belief, _) ->
        format('~n[CASCADE ANALYSIS]~n'),
        format('  If you accept this belief:~n    '),
        (cascade_analysis(Belief, accept) ; true),
        format('  If you reject this belief:~n    '),
        (cascade_analysis(Belief, reject) ; true)
    ; true),
    
    format('~n').

% --- 3. COMPLETE BATTERY AUDIT ---

%% audit_all
% Comprehensive audit of entire belief battery
audit_all :-
    format('~n╔══════════════════════════════════════════════════════════════╗~n'),
    format('║        COMPLETE INDIVIDUAL BELIEF BATTERY AUDIT              ║~n'),
    format('╚══════════════════════════════════════════════════════════════╝~n~n'),
    
    my_context(Ctx),
    format('[YOUR CONTEXT]~n'),
    format('  Power Level:  ~w~n', [Ctx.agent_power]),
    format('  Time Horizon: ~w~n', [Ctx.time_horizon]),
    format('  Exit Options: ~w~n', [Ctx.exit_options]),
    format('  Spatial Scope: ~w~n~n', [Ctx.spatial_scope]),
    
    % Tier 0: Theory
    format('[TIER 0: UNIVERSAL MOUNTAINS]~n'),
    format('Things true regardless of who/when/where:~n~n'),
    forall(
        (belief_classification(B, mountain), \+ member(B, [mortality_acceptance])),
        format('  ■ ~w~n', [B])
    ),
    
    % Count Tier 1
    format('~n[TIER 1: INDIVIDUAL PRACTICE]~n'),
    findall(B, (belief_classification(B, rope)), Ropes),
    length(Ropes, RopeCount),
    format('Pragmatic choices (~d beliefs):~n~n', [RopeCount]),
    
    % Group by section
    format('  AGENCY & CHANGE:~n'),
    forall(
        member(B, [meaningful_agency, personal_change, identity_stability]),
        format('    ⊞ ~w~n', [B])
    ),
    
    format('~n  MEANING & PURPOSE:~n'),
    forall(
        member(B, [life_meaning, suffering_meaning]),
        format('    ⊞ ~w~n', [B])
    ),
    
    format('~n  RELATIONSHIPS:~n'),
    forall(
        member(B, [relationship_meaning, family_obligations]),
        format('    ⊞ ~w~n', [B])
    ),
    
    format('~n  WORK & VALUE:~n'),
    forall(
        member(B, [work_value, wealth_status_pursuit]),
        format('    ⊞ ~w~n', [B])
    ),
    
    format('~n  EPISTEMIC NORMS:~n'),
    forall(
        member(B, [belief_norms, moral_intuitions]),
        format('    ⊞ ~w~n', [B])
    ),
    
    format('~n  POWER & SYSTEMS:~n'),
    forall(
        member(B, [power_structures_natural, participation_in_nooses]),
        format('    ⊞ ~w~n', [B])
    ),
    
    format('~n  DEATH & RISK:~n'),
    forall(
        member(B, [mortality_acceptance, risk_taking, future_planning]),
        format('    ~w: ~w~n', [B, Type]) :- belief_classification(B, Type)
    ),
    
    format('~n  TRUST & COOPERATION:~n'),
    forall(
        member(B, [trust_default, human_nature, defector_punishment]),
        format('    ⊞ ~w~n', [B])
    ),
    
    format('~n  IDENTITY & EXPRESSION:~n'),
    forall(
        member(B, [social_conformity, gender_sexual_identity, cultural_religious_identity]),
        format('    ⊞ ~w~n', [B])
    ),
    
    format('~n  CONSUMPTION & RESOURCES:~n'),
    forall(
        member(B, [consumption_level, experiences_vs_possessions, giving_obligation]),
        format('    ⊞ ~w~n', [B])
    ),
    
    format('~n  KNOWLEDGE & LEARNING:~n'),
    forall(
        member(B, [specialization, formal_education, curiosity_vs_practicality]),
        format('    ⊞ ~w~n', [B])
    ),
    
    format('~n  POLITICAL PARTICIPATION:~n'),
    forall(
        member(B, [electoral_participation, direct_action, local_vs_global]),
        format('    ⊞ ~w~n', [B])
    ),
    
    format('~n  EXISTENTIAL STANCE:~n'),
    forall(
        member(B, [optimism_pessimism, cosmic_meaning, having_children, time_allocation]),
        format('    ⊞ ~w~n', [B])
    ),
    
    % Extraction points
    format('~n[EXTRACTION POINT DETECTION]~n'),
    format('Nooses commonly claimed as Mountains:~n~n'),
    findall(E, type_1_error(E), Errors),
    length(Errors, ErrorCount),
    format('Found ~d institutional Nooses:~n~n', [ErrorCount]),
    forall(
        type_1_error(B),
        (format('  ⚠️  ~w~n', [B]),
         institutional_claim(B, mountain),
         format('      Claimed: Mountain (unchangeable)~n'),
         format('      Actually: Noose (extraction mechanism)~n~n'))
    ).

% --- 4. NOOSE DETECTION ---

%% find_nooses
% Specifically identifies extraction points
find_nooses :-
    format('~n╔══════════════════════════════════════════════════════════════╗~n'),
    format('║           EXTRACTION POINT DETECTION                        ║~n'),
    format('╚══════════════════════════════════════════════════════════════╝~n~n'),
    
    format('Searching for Type I Errors (False Mountains)...~n~n'),
    
    findall(B, type_1_error(B), Nooses),
    length(Nooses, Count),
    format('Found ~d extraction mechanisms:~n~n', [Count]),
    
    forall(
        type_1_error(B),
        (
            format('─────────────────────────────────────────────────────────~n'),
            format('⚠️  ~w~n~n', [B]),
            format('  INSTITUTIONAL CLAIM: '),
            (institutional_claim(B, Type) -> 
                format('~w~n', [Type])
            ;   format('UNDEFINED~n')),
            format('  YOUR CLASSIFICATION: '),
            (belief_classification(B, YourType) ->
                format('~w~n', [YourType])
            ;   format('UNDEFINED~n')),
            (belief_explanation(B, Exp) ->
                format('~n  WHY THIS MATTERS:~n  ~w~n', [Exp])
            ; true),
            format('~n')
        )
    ),
    format('─────────────────────────────────────────────────────────────~n~n').

% --- 5. CASCADE ANALYSIS ---

%% show_cascades(+Belief)
% Display cascade implications of accepting/rejecting belief
show_cascades(Belief) :-
    format('~n=== CASCADE ANALYSIS: ~w ===~n~n', [Belief]),
    
    (cascade_analysis(Belief, accept) ->
        format('[IF YOU ACCEPT THIS BELIEF]~n'),
        cascade_analysis(Belief, accept),
        format('~n')
    ; true),
    
    (cascade_analysis(Belief, reject) ->
        format('[IF YOU REJECT THIS BELIEF]~n'),
        cascade_analysis(Belief, reject),
        format('~n')
    ; true),
    
    (cascade_analysis(Belief, _) ->
        true
    ;
        format('No cascade analysis available for this belief.~n')
    ).

% --- 6. SUMMARY STATISTICS ---

%% belief_summary
% Statistical summary of belief battery
belief_summary :-
    format('~n=== BELIEF BATTERY SUMMARY ===~n~n'),
    
    findall(B, belief_classification(B, mountain), Mountains),
    length(Mountains, MCount),
    format('Mountains (Universal): ~d~n', [MCount]),
    
    findall(B, belief_classification(B, rope), Ropes),
    length(Ropes, RCount),
    format('Ropes (Strategic choices): ~d~n', [RCount]),
    
    findall(B, belief_classification(B, varies), Varies),
    length(Varies, VCount),
    format('Varies (Context-dependent): ~d~n', [VCount]),
    
    findall(E, type_1_error(E), Errors),
    length(Errors, ECount),
    format('~nType I Errors detected: ~d~n', [ECount]),
    
    Total is MCount + RCount + VCount,
    format('~nTotal beliefs mapped: ~d~n', [Total]).

% --- 7. INSTITUTIONAL COMPARISON ---

%% compare_to_institution(+Belief)
% Compare your belief to institutional claim
compare_to_institution(Belief) :-
    format('~n=== INSTITUTIONAL COMPARISON: ~w ===~n~n', [Belief]),
    
    format('[YOUR POSITION]~n'),
    (belief_classification(Belief, YourType) ->
        format('  Classification: ~w~n', [YourType]),
        (belief_explanation(Belief, Exp) ->
            format('  Reasoning: ~w~n', [Exp])
        ; true)
    ;
        format('  UNDEFINED~n')
    ),
    
    format('~n[INSTITUTIONAL POSITION]~n'),
    (institutional_claim(Belief, InstType) ->
        format('  Claimed as: ~w~n', [InstType])
    ;
        format('  No institutional claim~n')
    ),
    
    format('~n[ANALYSIS]~n'),
    (type_1_error(Belief) ->
        format('  ⚠️  CONFLICT: Type I Error detected~n'),
        format('  This Noose is being presented as Mountain~n'),
        format('  Effect: Enables extraction/control~n'),
        format('  Strategy: Reject institutional framing~n')
    ;
        (belief_classification(Belief, YourType),
         institutional_claim(Belief, InstType),
         YourType \= InstType) ->
            format('  Disagreement detected~n'),
            format('  You: ~w | Institution: ~w~n', [YourType, InstType])
        ;
            format('  No conflict detected~n')
    ).

