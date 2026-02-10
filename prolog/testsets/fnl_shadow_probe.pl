% ============================================================================
% CONSTRAINT STORY: fnl_shadow_probe
% ============================================================================
% Purpose: Investigation 1 probe -- designed to trigger False Natural Law
% (FNL) detection. Claims mountain (naturality) but has coupling violations
% that should cause Boltzmann non-compliance, triggering FNL signature
% and the tangled_rope override in resolve_modal_signature_conflict.
%
% Key design:
% - constraint_claim = mountain (triggers claimed_natural/2)
% - base_extractiveness = 0.30 with scope modifiers creating Chi values
%   that cross classification boundaries at different Power x Scope combos
% - suppression_requirement = 0.50 (above mountain ceiling 0.05)
% - 3 indexed classifications (passes epistemic access check)
% - constraint_beneficiary + constraint_victim (enables coordination gates)
% - requires_active_enforcement (structural evidence of construction)
% ============================================================================

:- module(constraint_fnl_shadow_probe, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * constraint_id: fnl_shadow_probe
 * human_readable: FNL Shadow Mode Probe (Physics-Washed Construction)
 * domain: investigation/testing
 *
 * SUMMARY:
 * A synthetic constraint designed to trigger False Natural Law detection.
 * Claims to be a mountain (natural law) but has structural coupling that
 * reveals construction. The base_extractiveness of 0.30 with scope
 * modifiers creates classification variance across power positions,
 * producing non-zero coupling. The suppression_requirement of 0.50
 * exceeds the mountain ceiling (0.05), and active enforcement plus
 * beneficiary/victim structure proves construction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Extractiveness moderate enough to create cross-threshold Chi at different powers
domain_priors:base_extractiveness(fnl_shadow_probe, 0.30).
% Suppression well above mountain ceiling (0.05) -- enables tangled_rope path
domain_priors:suppression_score(fnl_shadow_probe, 0.50).
% Low theater -- not performative
domain_priors:theater_ratio(fnl_shadow_probe, 0.10).

% Direct metric facts
narrative_ontology:constraint_metric(fnl_shadow_probe, extractiveness, 0.30).
narrative_ontology:constraint_metric(fnl_shadow_probe, suppression_requirement, 0.50).
narrative_ontology:constraint_metric(fnl_shadow_probe, theater_ratio, 0.10).

% MOUNTAIN CLAIM -- this is the key: claims naturality despite structural construction
narrative_ontology:constraint_claim(fnl_shadow_probe, mountain).

% Active enforcement proves this is NOT a natural law
domain_priors:requires_active_enforcement(fnl_shadow_probe).

% Beneficiary/victim structure -- natural laws don't have these
narrative_ontology:constraint_beneficiary(fnl_shadow_probe, institutional_apparatus).
narrative_ontology:constraint_victim(fnl_shadow_probe, constrained_subjects).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POWERLESS SUBJECT (SNARE)
% Chi = 0.30 * 1.42 (powerless) * 0.8 (local) = 0.341
% With suppression 0.50 this crosses into snare territory
constraint_indexing:constraint_classification(fnl_shadow_probe, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: INSTITUTIONAL POWER (MOUNTAIN)
% Chi = 0.30 * -0.12 (institutional) * 1.0 (national) = -0.036
% Near-zero chi from institutional perspective -- looks natural
constraint_indexing:constraint_classification(fnl_shadow_probe, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (TANGLED ROPE)
% Chi = 0.30 * 1.15 (analytical) * 1.2 (global) = 0.414
% Analytical perspective sees the coupling
constraint_indexing:constraint_classification(fnl_shadow_probe, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fnl_shadow_probe_tests).

test(claims_mountain) :-
    % Verify the constraint claims mountain (naturality)
    narrative_ontology:constraint_claim(fnl_shadow_probe, mountain).

test(has_three_indexed_classifications) :-
    % Epistemic access: need >= 3 for Boltzmann test
    findall(Ctx, constraint_indexing:constraint_classification(fnl_shadow_probe, _, Ctx), Ctxs),
    length(Ctxs, N),
    N >= 3.

test(perspectival_gap_exists) :-
    % Mountain from institutional, snare from powerless -- perspectival gap
    constraint_indexing:constraint_classification(fnl_shadow_probe, mountain, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(fnl_shadow_probe, snare, context(agent_power(powerless), _, _, _)).

test(structural_signature_detected) :-
    % The signature engine should detect this as FNL or FCR (not natural_law)
    structural_signatures:constraint_signature(fnl_shadow_probe, Sig),
    Sig \= natural_law.

:- end_tests(fnl_shadow_probe_tests).

/* ==========================================================================
   5. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fnl_shadow_probe, 0, 10).

/* ==========================================================================
   6. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Stable extraction over time (mimics natural law pattern)
narrative_ontology:measurement(fnl_tr_t0, fnl_shadow_probe, theater_ratio, 0, 0.08).
narrative_ontology:measurement(fnl_tr_t5, fnl_shadow_probe, theater_ratio, 5, 0.09).
narrative_ontology:measurement(fnl_tr_t10, fnl_shadow_probe, theater_ratio, 10, 0.10).

narrative_ontology:measurement(fnl_ex_t0, fnl_shadow_probe, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(fnl_ex_t5, fnl_shadow_probe, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(fnl_ex_t10, fnl_shadow_probe, base_extractiveness, 10, 0.30).

/* ==========================================================================
   7. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fnl_shadow_probe, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
