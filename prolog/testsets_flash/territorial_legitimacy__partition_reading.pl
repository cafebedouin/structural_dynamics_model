% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy__partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy__partition_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: territorial_legitimacy__partition_reading
 *   human_readable: Territorial Legitimacy via International Partition (UN Resolution 181)
 *   domain: political_theory/international_law/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint defines territorial legitimacy in the Israeli-Palestinian
 *   conflict through the lens of international legal partition, primarily UN
 *   Resolution 181 (1948) and subsequent resolutions affirming the 1967
 *   lines. It posits the legitimacy of both Israeli and Palestinian states
 *   within recognized borders, rendering settlements beyond 1967
 *   illegitimate, and structurally enabling a two-state solution. It is a
 *   'tangled rope' because it genuinely attempts to coordinate competing
 *   claims but has become highly extractive due to incomplete enforcement and
 *   asymmetric power dynamics, requiring active enforcement to maintain its
 *   (contested) legitimacy.
 *
 * KEY AGENTS:
 *   - international_legal_framework: Agenda-setter (institutional/analytical) — provides normative basis
 *   - states_recognizing_1948_partition: Beneficiary (organized/mobile) — benefits from stable framework
 *   - palestinian_authority: Beneficiary/Payer (moderate/constrained) — seeks statehood, bears occupation costs
 *   - israeli_government: Beneficiary/Agenda-setter (institutional/constrained) — benefits from recognition, resists full implementation
 *   - palestinian_refugees: Victim (powerless/trapped) — bears displacement costs, right of return unresolved
 *   - israeli_settlers_beyond_1967_lines: Victim (moderate/identity_locked) — deemed illegitimate, faces potential displacement
 *   - human_rights_organizations: Observer (organized/analytical) — monitors adherence to international law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy__partition_reading, 0.6).
domain_priors:suppression_score(territorial_legitimacy__partition_reading, 0.7).
domain_priors:theater_ratio(territorial_legitimacy__partition_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy__partition_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy__partition_reading, "Territorial Legitimacy via International Partition (UN Resolution 181)").
narrative_ontology:topic_domain(territorial_legitimacy__partition_reading, "political_theory/international_law/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy__partition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__partition_reading, 'c2162f3e-fa1a-453d-b84f-b2136209e1cd').
narrative_ontology:cs_kernel_codification('c2162f3e-fa1a-453d-b84f-b2136209e1cd', formalized).
narrative_ontology:cs_authority_grounding('c2162f3e-fa1a-453d-b84f-b2136209e1cd', lineage).
narrative_ontology:cs_interpretation_layer_present('c2162f3e-fa1a-453d-b84f-b2136209e1cd').
narrative_ontology:cs_reading_relation('c2162f3e-fa1a-453d-b84f-b2136209e1cd', territorial_legitimacy__security_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('c2162f3e-fa1a-453d-b84f-b2136209e1cd', territorial_legitimacy__indigenous_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('c2162f3e-fa1a-453d-b84f-b2136209e1cd', foundational, state_sovereignty_via_international_recognition).
narrative_ontology:cs_axiom_status(state_sovereignty_via_international_recognition, holdable).
narrative_ontology:cs_axiom_grounding('c2162f3e-fa1a-453d-b84f-b2136209e1cd', state_sovereignty_via_international_recognition, conventional).
narrative_ontology:cs_axiom('c2162f3e-fa1a-453d-b84f-b2136209e1cd', foundational, territorial_acquisition_by_force_is_illegitimate).
narrative_ontology:cs_axiom_status(territorial_acquisition_by_force_is_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('c2162f3e-fa1a-453d-b84f-b2136209e1cd', territorial_acquisition_by_force_is_illegitimate, deontological).
narrative_ontology:cs_reference_frame('c2162f3e-fa1a-453d-b84f-b2136209e1cd', post_wwii_self_determination_framework).
narrative_ontology:cs_drift_state('c2162f3e-fa1a-453d-b84f-b2136209e1cd', contemporary_unilateralism_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c2162f3e-fa1a-453d-b84f-b2136209e1cd', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy__partition_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, international_legal_framework).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, states_recognizing_1948_partition).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, palestinian_refugees).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, israeli_settlers_beyond_1967_lines).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy__partition_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(territorial_legitimacy__partition_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy__partition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy__partition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy__partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint's extractiveness (0.7 at end) is high because the framework, while offering a path to statehood for Palestinians, has not delivered full sovereignty or ended occupation, leading to ongoing costs for Palestinians. Suppression (0.85 at end) is also high, reflecting the active military and political efforts required to maintain the status quo and prevent alternative resolutions. Theater ratio (0.45 at end) indicates that while diplomatic efforts and legal pronouncements continue, a significant portion of the activity is performative, masking the lack of full implementation. The metrics show a trend of increasing extractiveness and suppression over time, reflecting the hardening of positions and the failure to achieve a lasting resolution.
 *
 * PERSPECTIVAL GAP:
 *   The international legal framework and states recognizing partition view this as a legitimate, albeit challenging, path to peace. However, for Palestinian refugees and Israeli settlers beyond 1967, the framework imposes significant costs and existential threats, leading to a perception of extraction and suppression. The Israeli government benefits from recognition but experiences the framework as a constraint on its territorial ambitions.
 *
 * DIRECTIONALITY LOGIC:
 *   The international legal framework and states recognizing partition are beneficiaries (low d) as they uphold a principle of international order. The Palestinian Authority is a mixed beneficiary/payer, seeking statehood but bearing costs. The Israeli government is a beneficiary of recognition but a target regarding settlement policy. Palestinian refugees and Israeli settlers beyond 1967 are clear victims/targets (high d) as their claims or presence are directly challenged by this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (two-state solution via partition) is still 'live' but its effectiveness is contested. The high extractiveness and suppression, coupled with a rising theater ratio, suggest that while the mandate persists, its function has degraded, becoming more about managing an intractable conflict than resolving it. This prevents mislabeling it as a pure rope (which would imply symmetric benefits) or a pure snare (which would ignore the genuine coordination function it still attempts to provide).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_gap_vs_legitimacy,
    'Is the perceived extractiveness of this reading due to a fundamental flaw in the partition principle itself, or primarily due to a lack of enforcement by the international community?',
    'Analysis of historical instances where international legal frameworks were fully enforced, and their outcomes. Counterfactual modeling of full enforcement in this context.',
    'If due to enforcement gap, the partition reading remains a valid (though unfulfilled) ''rope'' or ''scaffold''. If due to fundamental flaw, it is structurally a ''snare'' or ''tangled_rope'' regardless of enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_gap_vs_legitimacy, conceptual, 'Distinguishing between a flawed principle and flawed implementation.').

omega_variable(
    settler_identity_lock_resolution,
    'To what extent is the ''identity_locked'' exit option for Israeli settlers beyond 1967 lines a genuine, irreducible identity fusion, versus a politically constructed and reversible attachment?',
    'Sociological studies of settler communities, analysis of historical precedents for population transfers or withdrawals, and the role of state incentives/disincentives.',
    'If genuinely irreducible, the human cost of enforcing the partition reading is higher, potentially shifting the classification towards a ''snare'' for this group. If reversible, the ''identity_locked'' status is a political artifact, and the constraint is less fundamentally extractive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(settler_identity_lock_resolution, empirical, 'Nature of identity lock for settlers.').

omega_variable(
    partition_vs_one_state_viability,
    'Is the two-state solution, as envisioned by the partition reading, still a viable and just outcome, or has demographic and territorial reality rendered it obsolete, necessitating a one-state solution?',
    'Demographic projections, analysis of territorial contiguity, and political feasibility studies for both two-state and one-state models.',
    'If obsolete, the partition reading''s ''tangled rope'' classification might shift towards a ''piton'' (performing a function no longer viable) or even a ''snare'' (actively preventing a more just outcome). If still viable, its coordination function remains relevant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_vs_one_state_viability, preference, 'Viability of the two-state solution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy__partition_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1948, territorial_legitimacy__partition_reading, theater_ratio, 1948, 0.2).
narrative_ontology:measurement(terr_tr_t1967, territorial_legitimacy__partition_reading, theater_ratio, 1967, 0.3).
narrative_ontology:measurement(terr_tr_t1993, territorial_legitimacy__partition_reading, theater_ratio, 1993, 0.25).
narrative_ontology:measurement(terr_tr_t2000, territorial_legitimacy__partition_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(terr_tr_t2010, territorial_legitimacy__partition_reading, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(terr_tr_t2024, territorial_legitimacy__partition_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(terr_be_t1948, territorial_legitimacy__partition_reading, base_extractiveness, 1948, 0.4).
narrative_ontology:measurement(terr_be_t1967, territorial_legitimacy__partition_reading, base_extractiveness, 1967, 0.6).
narrative_ontology:measurement(terr_be_t1993, territorial_legitimacy__partition_reading, base_extractiveness, 1993, 0.55).
narrative_ontology:measurement(terr_be_t2000, territorial_legitimacy__partition_reading, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(terr_be_t2010, territorial_legitimacy__partition_reading, base_extractiveness, 2010, 0.68).
narrative_ontology:measurement(terr_be_t2024, territorial_legitimacy__partition_reading, base_extractiveness, 2024, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1948, territorial_legitimacy__partition_reading, suppression_requirement, 1948, 0.5).
narrative_ontology:measurement(terr_su_t1967, territorial_legitimacy__partition_reading, suppression_requirement, 1967, 0.7).
narrative_ontology:measurement(terr_su_t1993, territorial_legitimacy__partition_reading, suppression_requirement, 1993, 0.65).
narrative_ontology:measurement(terr_su_t2000, territorial_legitimacy__partition_reading, suppression_requirement, 2000, 0.75).
narrative_ontology:measurement(terr_su_t2010, territorial_legitimacy__partition_reading, suppression_requirement, 2010, 0.8).
narrative_ontology:measurement(terr_su_t2024, territorial_legitimacy__partition_reading, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy__partition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(territorial_legitimacy__partition_reading, territorial_legitimacy__security_necessity_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__partition_reading, territorial_legitimacy__indigenous_continuity_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__partition_reading, right_of_return_for_palestinians).
narrative_ontology:affects_constraint(territorial_legitimacy__partition_reading, israeli_settlement_expansion).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'territorial_legitimacy' kernel. It focuses on international legal partition and state recognition. Other readings include 'security_necessity_reading' and 'indigenous_continuity_reading', which offer alternative bases for legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
