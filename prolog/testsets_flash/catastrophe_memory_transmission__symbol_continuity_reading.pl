% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_transmission__symbol_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_transmission__symbol_continuity_reading, []).

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
 *   constraint_id: catastrophe_memory_transmission__symbol_continuity_reading
 *   human_readable: Catastrophe Memory Transmission: Symbol Continuity Reading
 *   domain: religious_studies/collective_memory/ritual_studies
 *
 * SUMMARY:
 *   This constraint describes the 'symbol continuity' reading of how
 *   communities transmit memory of catastrophe through ritual. Here, the
 *   primary function of ritual is to preserve communal identity and mourning
 *   practices by ensuring the faithful transmission of symbolic forms. This
 *   fidelity is seen as the mechanism for the community's survival, even if
 *   it comes at the cost of adaptive capacity. The constraint is a Tangled
 *   Rope because it coordinates identity and meaning but extracts adaptive
 *   flexibility.
 *
 * KEY AGENTS:
 *   - communal_identity: Primary beneficiary (institutional/generational) — sustained by ritual fidelity.
 *   - ritual_specialists: Agenda setter/beneficiary (organized/biographical) — responsible for maintaining ritual fidelity, derive status from it.
 *   - community_members: Payer/beneficiary (moderate/biographical) — participate in ritual, gain identity, but bear the cost of reduced adaptive capacity.
 *   - adaptive_capacity: Victim (abstract/generational) — sacrificed to maintain symbolic form.
 *   - external_observers: Observer (analytical/civilizational) — analyze the trade-offs between fidelity and adaptation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_transmission__symbol_continuity_reading, 0.65).
domain_priors:suppression_score(catastrophe_memory_transmission__symbol_continuity_reading, 0.7).
domain_priors:theater_ratio(catastrophe_memory_transmission__symbol_continuity_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission__symbol_continuity_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_transmission__symbol_continuity_reading, "Catastrophe Memory Transmission: Symbol Continuity Reading").
narrative_ontology:topic_domain(catastrophe_memory_transmission__symbol_continuity_reading, "religious_studies/collective_memory/ritual_studies").

domain_priors:requires_active_enforcement(catastrophe_memory_transmission__symbol_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__symbol_continuity_reading, '18c1191a-b1d9-4a68-a1ec-a28d93412a3b').
narrative_ontology:cs_kernel_codification('18c1191a-b1d9-4a68-a1ec-a28d93412a3b', implicit).
narrative_ontology:cs_authority_grounding('18c1191a-b1d9-4a68-a1ec-a28d93412a3b', lineage).
narrative_ontology:cs_interpretation_layer_present('18c1191a-b1d9-4a68-a1ec-a28d93412a3b').
narrative_ontology:cs_reading_relation('18c1191a-b1d9-4a68-a1ec-a28d93412a3b', catastrophe_memory_transmission__operational_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('18c1191a-b1d9-4a68-a1ec-a28d93412a3b', catastrophe_memory_transmission__hybrid_embedded_reading, coexists_with).
narrative_ontology:cs_axiom('18c1191a-b1d9-4a68-a1ec-a28d93412a3b', foundational, symbolic_form_is_identity).
narrative_ontology:cs_axiom_status(symbolic_form_is_identity, holdable).
narrative_ontology:cs_axiom_grounding('18c1191a-b1d9-4a68-a1ec-a28d93412a3b', symbolic_form_is_identity, deontological).
narrative_ontology:cs_axiom('18c1191a-b1d9-4a68-a1ec-a28d93412a3b', secondary, fidelity_ensures_survival).
narrative_ontology:cs_axiom_status(fidelity_ensures_survival, holdable).
narrative_ontology:cs_axiom_grounding('18c1191a-b1d9-4a68-a1ec-a28d93412a3b', fidelity_ensures_survival, conventional).
narrative_ontology:cs_reference_frame('18c1191a-b1d9-4a68-a1ec-a28d93412a3b', unbroken_symbolic_transmission).
narrative_ontology:cs_drift_state('18c1191a-b1d9-4a68-a1ec-a28d93412a3b', contemporary_era_of_rapid_change, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('18c1191a-b1d9-4a68-a1ec-a28d93412a3b', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__symbol_continuity_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__symbol_continuity_reading, communal_identity).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__symbol_continuity_reading, ritual_specialists).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__symbol_continuity_reading, adaptive_capacity).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__symbol_continuity_reading, community_members).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_transmission__symbol_continuity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(catastrophe_memory_transmission__symbol_continuity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_transmission__symbol_continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_transmission__symbol_continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_transmission__symbol_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is high because the strict adherence to symbolic form often demands sacrificing practical adaptation to new threats or environments. Suppression (0.7) is also high, as deviation from prescribed ritual is often met with social pressure or exclusion to maintain fidelity. Theater ratio (0.2) is low, as the ritual is genuinely performed for its symbolic function, not merely for show. The rising extractiveness and suppression over time reflect an increasing rigidity in ritual transmission, potentially in response to external pressures or internal consolidation of authority.
 *
 * PERSPECTIVAL GAP:
 *   Ritual specialists and communal identity (beneficiaries) experience this as a necessary coordination mechanism for survival, ensuring the group's distinctiveness and continuity. Community members and adaptive capacity (victims) experience it as a burden, where the demands of symbolic fidelity limit their ability to respond effectively to changing circumstances. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Communal identity and ritual specialists are beneficiaries (low d) as they are sustained and empowered by the constraint. Adaptive capacity and community members are targets (high d) as they bear the costs of rigidity and suppressed innovation. The constraint subsidizes identity and ritual authority by extracting from the community's flexibility.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure Rope (ignoring the cost to adaptive capacity) or a pure Snare (ignoring the genuine coordination of identity). It highlights the inherent trade-off: the mandate to preserve identity through symbolic form is live, but its method (strict fidelity) creates an extractive dynamic by limiting other forms of communal 'survival'. The constraint's persistence is tied to the perceived necessity of symbolic continuity for identity, even as the costs accumulate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    symbol_vs_operational_necessity,
    'Is the strict transmission of symbolic form genuinely necessary for communal identity, or does it suppress adaptive capacity beyond what is required for identity maintenance?',
    'Comparative study of communities with similar catastrophe histories but different ritual adaptation rates: if communities with higher adaptation maintain identity, the strict symbolic fidelity is not strictly necessary.',
    'If not strictly necessary, the constraint''s extractiveness (sacrifice of adaptive capacity) is higher than justified by the coordination function, pushing it closer to a Snare. If necessary, it reinforces the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbol_vs_operational_necessity, empirical, 'Ambiguity between symbolic necessity and adaptive cost.').

omega_variable(
    kernel_reading_difference,
    'This constraint is the ''symbol_continuity_reading'' of the ''catastrophe_memory_transmission'' kernel. How would the classification change under the ''operational_competence_reading'' or ''hybrid_embedded_reading''?',
    'Adopting the ''operational_competence_reading'' would shift focus to the functional transmission of survival skills, likely lowering perceived extractiveness if the ritual is effective. The ''hybrid_embedded_reading'' would acknowledge both, potentially leading to a more balanced Tangled Rope or even Rope if the embedded competence is high.',
    'The ''operational_competence_reading'' would likely reduce the perceived ''victim'' status of adaptive capacity, potentially lowering extractiveness and suppression, pushing the constraint towards a Rope. The ''hybrid_embedded_reading'' would acknowledge both, potentially leading to a more balanced Tangled Rope or even Rope if the embedded competence is high.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_difference, conceptual, 'Impact of alternative kernel readings on constraint classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__symbol_continuity_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_transmission__symbol_continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__symbol_continuity_reading, catastrophe_memory_transmission__operational_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__symbol_continuity_reading, catastrophe_memory_transmission__hybrid_embedded_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'catastrophe_memory_transmission' kernel, focusing on symbolic continuity. The other readings emphasize operational competence or a hybrid of both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
