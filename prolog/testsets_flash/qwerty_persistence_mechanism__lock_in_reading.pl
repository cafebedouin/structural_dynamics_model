% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_mechanism__lock_in_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_mechanism__lock_in_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: qwerty_persistence_mechanism__lock_in_reading
 *   human_readable: QWERTY Keyboard Layout Persistence (Lock-in Reading)
 *   domain: economic_history/technology_studies/path_dependence
 *
 * SUMMARY:
 *   This constraint describes the persistence of the QWERTY keyboard layout,
 *   interpreted through the 'lock-in' lens of path dependence theory. It
 *   argues that QWERTY, despite its technical inferiority for modern typing,
 *   persists due to collective coordination failure and high individual
 *   switching costs, rather than active enforcement or extraction by a
 *   specific beneficiary. The constraint is claimed as a Rope because it
 *   provides a universal coordination function, but with a non-trivial,
 *   collectively borne 'extraction' cost due to its suboptimality.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_mechanism__lock_in_reading, 0.3).
domain_priors:suppression_score(qwerty_persistence_mechanism__lock_in_reading, 0.4).
domain_priors:theater_ratio(qwerty_persistence_mechanism__lock_in_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_mechanism__lock_in_reading, rope).
narrative_ontology:human_readable(qwerty_persistence_mechanism__lock_in_reading, "QWERTY Keyboard Layout Persistence (Lock-in Reading)").
narrative_ontology:topic_domain(qwerty_persistence_mechanism__lock_in_reading, "economic_history/technology_studies/path_dependence").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_mechanism__lock_in_reading, 'c3c83b52-b240-474b-adfa-056001d60dff').
narrative_ontology:cs_kernel_codification('c3c83b52-b240-474b-adfa-056001d60dff', implicit).
narrative_ontology:cs_authority_grounding('c3c83b52-b240-474b-adfa-056001d60dff', practice).
narrative_ontology:cs_reading_relation('c3c83b52-b240-474b-adfa-056001d60dff', qwerty_persistence_mechanism__naturalization_reading, coexists_with).
narrative_ontology:cs_reading_relation('c3c83b52-b240-474b-adfa-056001d60dff', qwerty_persistence_mechanism__beneficiary_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('c3c83b52-b240-474b-adfa-056001d60dff', foundational, collective_action_failure_is_primary_driver).
narrative_ontology:cs_axiom_status(collective_action_failure_is_primary_driver, holdable).
narrative_ontology:cs_axiom_grounding('c3c83b52-b240-474b-adfa-056001d60dff', collective_action_failure_is_primary_driver, empirically_contingent).
narrative_ontology:cs_axiom('c3c83b52-b240-474b-adfa-056001d60dff', foundational, no_single_active_extractor).
narrative_ontology:cs_axiom_status(no_single_active_extractor, holdable).
narrative_ontology:cs_axiom_grounding('c3c83b52-b240-474b-adfa-056001d60dff', no_single_active_extractor, empirically_contingent).
narrative_ontology:cs_reference_frame('c3c83b52-b240-474b-adfa-056001d60dff', initial_arbitrary_choice_with_network_effects).
narrative_ontology:cs_drift_state('c3c83b52-b240-474b-adfa-056001d60dff', contemporary_digital_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('c3c83b52-b240-474b-adfa-056001d60dff', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_mechanism__lock_in_reading, qwerty_persistence_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__lock_in_reading, all_typists).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__lock_in_reading, keyboard_manufacturers).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__lock_in_reading, all_typists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a universal standard that allows them to use any keyboard, but collectively pay the cost of suboptimal typing speed and ergonomic strain due to the layout's design flaws. Individually, switching is too costly due to retraining and lack of alternative keyboards.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, all_typists, beneficiary,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence_mechanism__lock_in_reading, all_typists, payer).

% Benefit from a stable, universally accepted standard that simplifies production and marketing. They face no pressure to innovate on layout due to typist lock-in, but also do not actively enforce QWERTY's dominance.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, keyboard_manufacturers, beneficiary,
    powerful, generational, mobile, global).

% Develop technically superior keyboard layouts (e.g., Dvorak, Colemak) but cannot gain market traction due to the overwhelming installed base and switching costs for typists. Their innovations are effectively suppressed by the lock-in.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, alternative_layout_designers, excluded,
    powerless, generational, trapped, global).

% Analyze the QWERTY phenomenon as a classic case of path dependence and market failure, where initial arbitrary choices lead to long-term suboptimal outcomes without active extraction by a single party.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, economic_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universal standard for keyboard layouts, allowing typists to use any keyboard and manufacturers to produce a single dominant design, facilitating widespread adoption of typewriters and computers.
% TRANSFER_FUNCTION: Transfers the cost of suboptimal design (slower typing, ergonomic issues) from the collective of typists to themselves, in exchange for the coordination benefit of a universal standard. No direct monetary transfer to a specific agent for this suboptimality.
% ABSENT_VOICES: The 'voice' of collective efficiency and ergonomic optimization is absent, as individual typists cannot coordinate to switch to a superior layout, and manufacturers have no incentive to force a change. Alternative layout designers are also effectively silenced by market inertia.
% DISAPPEARANCE_RATIONALE: If QWERTY vanished overnight, the entire typing ecosystem would be thrown into chaos. A new standard would eventually emerge, likely a more efficient one, but the transition would be immensely disruptive, requiring retraining for billions and retooling for manufacturers.
% FOUNDING_PROBLEM: The need for a standardized keyboard layout to prevent typebar jamming in early mechanical typewriters and to facilitate widespread adoption of typing technology.
% FOUNDING_PROBLEM_CORROBORATION: The original mechanical problem (typebar jamming) is long dead with electronic keyboards. Economic historians and cognitive scientists corroborate that the layout is suboptimal for modern typing, but its persistence is due to network effects and switching costs, not the original problem. Keyboard manufacturers, while benefiting from the standard, do not claim the original problem is live.
narrative_ontology:disappearance_verdict(qwerty_persistence_mechanism__lock_in_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_mechanism__lock_in_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_mechanism__lock_in_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(qwerty_persistence_mechanism__lock_in_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_mechanism__lock_in_reading_tests).
:- end_tests(qwerty_persistence_mechanism__lock_in_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.3) is moderate, representing the collective cost of suboptimal efficiency and ergonomic strain. Suppression (0.4) is also moderate, reflecting the structural barriers to switching (network effects, retraining costs, lack of alternative hardware) rather than active coercion. Theater ratio (0.1) is low, as there's little performative maintenance; the system simply persists due to inertia. Accessibility collapse (0.6) is high because, once the QWERTY standard is established, viable alternatives for individual typists or manufacturers are severely limited. Resistance (0.2) is low because collective action to switch is difficult, and individual resistance is futile.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of an individual typist, the constraint feels like a 'trap' (identity_locked exit) due to the high personal cost of switching. From a manufacturer's perspective, it's a 'mobile' choice to continue producing QWERTY, as there's no market pressure to do otherwise. The collective suboptimality is not experienced as extraction by a single, identifiable agent, but as a diffuse social cost.
 *
 * DIRECTIONALITY LOGIC:
 *   All typists are both beneficiaries (universal standard) and victims (suboptimal layout, switching costs), leading to a directionality near symmetric. Keyboard manufacturers are beneficiaries of the stable standard, facing low pressure to change. Alternative layout designers are excluded, bearing the cost of market closure. Economic historians are analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing typebar jamming) is dead, but the constraint persists due to network effects and switching costs. This is a classic case of mandatrophy where the original mandate is obsolete, but the structure remains due to lock-in. The 'lock-in' reading prevents mislabeling this as a Snare, as there's no single, active extractor, or a Mountain, as it's a human-made standard, not a natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_mechanism_dominance,
    'Is QWERTY''s persistence primarily due to lock-in (coordination failure) or active extraction by beneficiaries (e.g., manufacturers protecting investments)?',
    'Detailed historical analysis of manufacturer lobbying efforts, patent enforcement, and marketing strategies compared to the observed magnitude of individual switching costs and network effects.',
    'If active extraction is dominant, the constraint would reclassify towards a Tangled Rope or Snare, with higher extractiveness and suppression. If lock-in is dominant, the Rope classification holds, emphasizing collective suboptimality over individual exploitation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_mechanism_dominance, empirical, 'Distinguishing between coordination failure and active rent-seeking as the primary driver of QWERTY''s persistence.').

omega_variable(
    suboptimality_measurement,
    'What is the precise, quantifiable social cost (e.g., lost productivity, ergonomic injury rates) attributable to QWERTY''s technical inferiority compared to alternative layouts?',
    'Large-scale, long-term empirical studies comparing typing speed, error rates, and health outcomes across different keyboard layouts in real-world settings.',
    'A higher quantifiable cost would increase the ''extractiveness'' metric, potentially pushing the classification towards a more extractive type, even without a clear beneficiary. A lower cost would support the ''naturalization'' reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suboptimality_measurement, empirical, 'Quantifying the ''extraction'' of suboptimal design in terms of social cost.').

omega_variable(
    naturalization_vs_lock_in,
    'To what extent has QWERTY''s long-term use led to its ''naturalization'' as the ''best'' layout, obscuring its historical contingency and technical flaws?',
    'Cognitive psychology studies on user perception and adaptation to QWERTY, and analysis of public discourse regarding keyboard layouts.',
    'If naturalization is strong, it implies a form of internalized suppression, making the constraint more resilient and harder to challenge, even if the ''lock-in'' mechanism weakens. This would shift the ''suppression'' metric upwards.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalization_vs_lock_in, conceptual, 'Assessing the degree to which QWERTY''s persistence is due to perceived naturalness rather than pure lock-in.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_mechanism__lock_in_reading, 1873, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t1873, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 1873, 0.05).
narrative_ontology:measurement(qwer_tr_t1920, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 1920, 0.08).
narrative_ontology:measurement(qwer_tr_t1950, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(qwer_tr_t1980, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(qwer_tr_t2024, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(qwer_be_t1873, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 1873, 0.1).
narrative_ontology:measurement(qwer_be_t1920, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 1920, 0.2).
narrative_ontology:measurement(qwer_be_t1950, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 1950, 0.25).
narrative_ontology:measurement(qwer_be_t1980, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 1980, 0.3).
narrative_ontology:measurement(qwer_be_t2024, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 2024, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t1873, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 1873, 0.1).
narrative_ontology:measurement(qwer_su_t1920, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 1920, 0.2).
narrative_ontology:measurement(qwer_su_t1950, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 1950, 0.3).
narrative_ontology:measurement(qwer_su_t1980, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 1980, 0.4).
narrative_ontology:measurement(qwer_su_t2024, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_mechanism__lock_in_reading, information_standard).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__lock_in_reading, qwerty_persistence_mechanism__naturalization_reading).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__lock_in_reading, qwerty_persistence_mechanism__beneficiary_extraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'QWERTY persistence mechanism' kernel. This 'lock-in' reading emphasizes path dependence and coordination failure, distinct from the 'naturalization' (adequacy) and 'beneficiary extraction' (active rent-seeking) readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
