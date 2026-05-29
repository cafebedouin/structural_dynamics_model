% ============================================================================
% TEST: cs_kernel_axiom_conflict SIGNATURE DISTINCTION
% ============================================================================
% Verifies that contradiction and forecloses are independent signals and that
% their combination produces the two distinct diagnostic patterns:
%
%   licensed_plurality: cs_kernel_axiom_conflict fires + coexists_with edge
%   real_closure:       cs_kernel_axiom_conflict fires + forecloses edge
%
% Selectivity:
%   A↔C: contradiction declared (incompatible axioms)
%   B↔C: contradiction declared (incompatible axioms)
%   A↔B: NO contradiction (different justifications, compatible in one framework)
%
% This mirrors the capital punishment triplet:
%   retributive (A) and deterrence (B) both support CP — compatible justifications.
%   abolition (C) holds CP categorically impermissible — contradicts both A and B.
%   SCOPE declares A↔C and B↔C, NOT A↔B.
%
% For real closure: second kernel (total_war) where one reading forecloses the
%   other AND contradiction is declared — the combination fires as real closure.
% ============================================================================

:- use_module(cs_axiom_engine).
:- use_module(cs_kernel_registry).
:- use_module(narrative_ontology).
:- use_module(library(plunit)).

:- multifile
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_contradiction/2.

% ---------------------------------------------------------------------------
% KERNEL 1: capital punishment (triplet structure)
% Three readings, only A↔C and B↔C have declared contradictions.
% All edges are coexists_with (socially live positions, not logically entailed).
% ---------------------------------------------------------------------------

narrative_ontology:cs_kernel_id(cp_retributive, cp_kernel).
narrative_ontology:cs_kernel_id(cp_deterrence, cp_kernel).
narrative_ontology:cs_kernel_id(cp_abolition, cp_kernel).

narrative_ontology:cs_reading_relation(cp_retributive, cp_deterrence, coexists_with).
narrative_ontology:cs_reading_relation(cp_retributive, cp_abolition, coexists_with).
narrative_ontology:cs_reading_relation(cp_deterrence, cp_retributive, coexists_with).
narrative_ontology:cs_reading_relation(cp_deterrence, cp_abolition, coexists_with).
narrative_ontology:cs_reading_relation(cp_abolition, cp_retributive, coexists_with).
narrative_ontology:cs_reading_relation(cp_abolition, cp_deterrence, coexists_with).

narrative_ontology:cs_axiom(cp_retributive, foundational, proportionate_punishment_grounds_execution).
narrative_ontology:cs_axiom(cp_deterrence,  foundational, rational_deterrence_justifies_execution).
narrative_ontology:cs_axiom(cp_abolition,   foundational, state_killing_categorically_impermissible).

% SCOPE declaration: A↔C and B↔C are contradictory; A↔B is NOT (different justifications)
narrative_ontology:cs_axiom_contradiction(proportionate_punishment_grounds_execution, state_killing_categorically_impermissible).
narrative_ontology:cs_axiom_contradiction(state_killing_categorically_impermissible, proportionate_punishment_grounds_execution).
narrative_ontology:cs_axiom_contradiction(rational_deterrence_justifies_execution, state_killing_categorically_impermissible).
narrative_ontology:cs_axiom_contradiction(state_killing_categorically_impermissible, rational_deterrence_justifies_execution).

% ---------------------------------------------------------------------------
% KERNEL 2: total war doctrine (forecloses case)
% Two readings; doctrine reading forecloses the deterrence reading.
% Contradiction declared — shows real closure when forecloses + contradiction.
% ---------------------------------------------------------------------------

narrative_ontology:cs_kernel_id(tw_doctrine,  tw_kernel).
narrative_ontology:cs_kernel_id(tw_deterrence, tw_kernel).

% forecloses edge: the nuclear deterrence doctrine forecloses the total-war-victory doctrine
narrative_ontology:cs_reading_relation(tw_deterrence, tw_doctrine, forecloses).
narrative_ontology:cs_reading_relation(tw_doctrine,   tw_deterrence, coexists_with).  % historical coexistence before nuclear era

narrative_ontology:cs_axiom(tw_doctrine,   foundational, decisive_victory_achievable_through_total_mobilization).
narrative_ontology:cs_axiom(tw_deterrence, foundational, total_war_victory_structurally_impossible).

% SCOPE declaration: the two foundational axioms are contradictory
narrative_ontology:cs_axiom_contradiction(decisive_victory_achievable_through_total_mobilization, total_war_victory_structurally_impossible).
narrative_ontology:cs_axiom_contradiction(total_war_victory_structurally_impossible, decisive_victory_achievable_through_total_mobilization).

% ---------------------------------------------------------------------------
% KERNEL 3: forecloses-without-contradiction (structural pressure only)
% One reading forecloses another but NO contradiction declared on their axioms.
% cs_kernel_axiom_conflict must NOT fire (distinguishes from real closure).
% ---------------------------------------------------------------------------

narrative_ontology:cs_kernel_id(sp_reading_a, sp_kernel).
narrative_ontology:cs_kernel_id(sp_reading_b, sp_kernel).

narrative_ontology:cs_reading_relation(sp_reading_a, sp_reading_b, forecloses).
narrative_ontology:cs_axiom(sp_reading_a, foundational, axiom_pressure_alpha).
narrative_ontology:cs_axiom(sp_reading_b, foundational, axiom_pressure_beta).
% No cs_axiom_contradiction declared — structural pressure only.

% ---------------------------------------------------------------------------
% TESTS
% ---------------------------------------------------------------------------

:- begin_tests(contradiction_signatures).

% --- Licensed plurality: contradiction + coexists_with ---

test(lp_retributive_abolition) :-
    cs_axiom_engine:cs_kernel_axiom_conflict(cp_kernel, C1, C2, _Pair),
    ( (C1 == cp_retributive, C2 == cp_abolition)
    ; (C1 == cp_abolition,   C2 == cp_retributive) ), !,
    % Verify edge is coexists_with (not forecloses) → licensed plurality
    ( narrative_ontology:cs_reading_relation(C1, C2, coexists_with)
    ; narrative_ontology:cs_reading_relation(C2, C1, coexists_with) ).

test(lp_deterrence_abolition) :-
    cs_axiom_engine:cs_kernel_axiom_conflict(cp_kernel, C1, C2, _Pair),
    ( (C1 == cp_deterrence, C2 == cp_abolition)
    ; (C1 == cp_abolition,  C2 == cp_deterrence) ), !,
    ( narrative_ontology:cs_reading_relation(C1, C2, coexists_with)
    ; narrative_ontology:cs_reading_relation(C2, C1, coexists_with) ).

% selectivity: retributive↔deterrence must NOT fire (no contradiction declared)
test(no_conflict_retributive_deterrence, [fail]) :-
    cs_axiom_engine:cs_kernel_axiom_conflict(cp_kernel, C1, C2, _),
    (   (C1 == cp_retributive, C2 == cp_deterrence)
    ;   (C1 == cp_deterrence,  C2 == cp_retributive) ).

% exactly 2 conflicts in the cp_kernel (A↔C, B↔C), not 3
test(lp_conflict_count) :-
    findall(C1-C2, cs_axiom_engine:cs_kernel_axiom_conflict(cp_kernel, C1, C2, _), Pairs),
    length(Pairs, 2).

% --- Real closure: contradiction + forecloses ---

test(rc_total_war) :-
    cs_axiom_engine:cs_kernel_axiom_conflict(tw_kernel, C1, C2, _Pair),
    % Verify at least one forecloses edge between the conflicting readings
    ( narrative_ontology:cs_reading_relation(C1, C2, forecloses)
    ; narrative_ontology:cs_reading_relation(C2, C1, forecloses) ).

% --- Structural pressure only: forecloses edge, no contradiction → silent ---

test(structural_pressure_silent, [fail]) :-
    cs_axiom_engine:cs_kernel_axiom_conflict(sp_kernel, _, _, _).

% --- Independence demonstration ---
% The two cp_kernel conflicts are licensed plurality; the tw_kernel conflict is real closure.
% They coexist in the same engine call, distinguished by edge type, not by different predicates.
test(both_signatures_expressible) :-
    % licensed plurality: at least one conflict with coexists_with edge in cp_kernel
    cs_axiom_engine:cs_kernel_axiom_conflict(cp_kernel, LP1, LP2, _),
    (   narrative_ontology:cs_reading_relation(LP1, LP2, coexists_with)
    ;   narrative_ontology:cs_reading_relation(LP2, LP1, coexists_with) ),
    % real closure: conflict with forecloses edge in tw_kernel
    cs_axiom_engine:cs_kernel_axiom_conflict(tw_kernel, RC1, RC2, _),
    (   narrative_ontology:cs_reading_relation(RC1, RC2, forecloses)
    ;   narrative_ontology:cs_reading_relation(RC2, RC1, forecloses) ),
    % the two kernels have different signatures
    \+ (LP1 == RC1, LP2 == RC2).

:- end_tests(contradiction_signatures).
