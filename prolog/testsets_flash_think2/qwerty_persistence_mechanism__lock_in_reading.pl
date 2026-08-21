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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
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
 *   This constraint describes the persistence of the QWERTY keyboard layout
 *   through path-dependent lock-in, despite the existence of technically
 *   superior alternatives. It is a 'piton' because its original function
 *   (preventing mechanical jams) has atrophied, and it persists due to
 *   inertia (muscle memory, training costs, manufacturing tooling) rather
 *   than active, concentrated benefit. The 'lock-in' reading emphasizes
 *   collective suboptimality and market failure without attributing
 *   persistence to active, concentrated extraction by specific beneficiaries.
 *   The metrics reflect a diffuse, increasing cost over time as the gap
 *   between QWERTY and optimal alternatives widened.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_mechanism__lock_in_reading, 0.45).
domain_priors:suppression_score(qwerty_persistence_mechanism__lock_in_reading, 0.6).
domain_priors:theater_ratio(qwerty_persistence_mechanism__lock_in_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_mechanism__lock_in_reading, piton).
narrative_ontology:human_readable(qwerty_persistence_mechanism__lock_in_reading, "QWERTY Keyboard Layout Persistence (Lock-in Reading)").
narrative_ontology:topic_domain(qwerty_persistence_mechanism__lock_in_reading, "economic_history/technology_studies/path_dependence").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_mechanism__lock_in_reading, '9731d4ad-1e64-4a8d-a8c7-df99752f95bf').
narrative_ontology:cs_kernel_codification('9731d4ad-1e64-4a8d-a8c7-df99752f95bf', implicit).
narrative_ontology:cs_authority_grounding('9731d4ad-1e64-4a8d-a8c7-df99752f95bf', practice).
narrative_ontology:cs_reading_relation('9731d4ad-1e64-4a8d-a8c7-df99752f95bf', qwerty_persistence_mechanism__beneficiary_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('9731d4ad-1e64-4a8d-a8c7-df99752f95bf', qwerty_persistence_mechanism__naturalization_reading, forecloses).
narrative_ontology:cs_axiom('9731d4ad-1e64-4a8d-a8c7-df99752f95bf', foundational, collective_suboptimality_without_active_extraction).
narrative_ontology:cs_axiom_status(collective_suboptimality_without_active_extraction, holdable).
narrative_ontology:cs_axiom_grounding('9731d4ad-1e64-4a8d-a8c7-df99752f95bf', collective_suboptimality_without_active_extraction, empirically_contingent).
narrative_ontology:cs_axiom('9731d4ad-1e64-4a8d-a8c7-df99752f95bf', secondary, network_effects_as_self_enforcing_suppression).
narrative_ontology:cs_axiom_status(network_effects_as_self_enforcing_suppression, holdable).
narrative_ontology:cs_axiom_grounding('9731d4ad-1e64-4a8d-a8c7-df99752f95bf', network_effects_as_self_enforcing_suppression, empirically_contingent).
narrative_ontology:cs_reference_frame('9731d4ad-1e64-4a8d-a8c7-df99752f95bf', initial_network_formation).
narrative_ontology:cs_drift_state('9731d4ad-1e64-4a8d-a8c7-df99752f95bf', contemporary_digital_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('9731d4ad-1e64-4a8d-a8c7-df99752f95bf', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_mechanism__lock_in_reading, qwerty_persistence_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__lock_in_reading, keyboard_users).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__lock_in_reading, alternative_keyboard_manufacturers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__lock_in_reading, qwerty_manufacturers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the diffuse costs of a suboptimal keyboard layout through slower typing speeds and increased training effort. Their muscle memory and the ubiquity of QWERTY devices make switching to alternatives prohibitively costly for individuals, effectively locking them in.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, keyboard_users, payer,
    powerless, biographical, identity_locked, global).

% Benefit from the existing standard by avoiding R&D costs for new layouts and leveraging established tooling and supply chains. While they could theoretically switch, the collective cost of retooling and retraining the market makes individual defection from QWERTY impractical.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, qwerty_manufacturers, beneficiary,
    organized, generational, constrained, global).

% Produce technically superior keyboard layouts (e.g., Dvorak) but struggle to gain market share due to the overwhelming network effects and user lock-in to QWERTY. They are effectively excluded from the mainstream market by the persistence mechanism.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, alternative_keyboard_manufacturers, excluded,
    powerless, biographical, trapped, global).

% Analyze the historical development and persistence of QWERTY as a case study in path dependence and market failure. They identify the social costs and the mechanisms of lock-in without directly participating in the market.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, economic_historians, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence_mechanism__lock_in_reading, diffuse).
narrative_ontology:fixing_cost_class(qwerty_persistence_mechanism__lock_in_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes keyboard layout, enabling universal touch-typing skills, interoperability across devices, and simplified manufacturing processes.
% TRANSFER_FUNCTION: Transfers efficiency costs (slower typing, higher error rates) from the collective to individual users, while transferring stability and reduced R&D benefits to manufacturers of the dominant layout.
% ABSENT_VOICES: Designers and proponents of technically superior alternative layouts, along with potential users who would benefit from them, are marginalized by the lock-in. They would advocate for a shift based on efficiency but lack the collective power to overcome network effects.
% DISAPPEARANCE_RATIONALE: If QWERTY vanished overnight, the entire ecosystem of keyboard manufacturing, user training, and muscle memory would be disrupted. A new standard would eventually emerge, likely after significant short-term chaos and long-term efficiency gains, fundamentally reorganizing human-computer interaction.
% FOUNDING_PROBLEM: To prevent mechanical typewriter keys from jamming by separating commonly used letter pairs and to provide a standardized layout for mass adoption of typewriters.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians and cognitive scientists (outside the benefiting manufacturers) corroborate that the original mechanical jamming problem is obsolete in modern digital contexts, and that the layout's current persistence is due to path dependence rather than optimal design. Independent studies consistently show alternative layouts offer superior efficiency.
narrative_ontology:disappearance_verdict(qwerty_persistence_mechanism__lock_in_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_mechanism__lock_in_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_mechanism__lock_in_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(qwerty_persistence_mechanism__lock_in_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_mechanism__lock_in_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is moderate (0.45) because the cost is diffuse across all users in terms of lost efficiency, rather than concentrated extraction by a single party. Suppression is high (0.6) due to the powerful network effects, established muscle memory, and high switching costs that effectively 'suppress' the adoption of alternatives. Theater ratio is low (0.2) as the constraint is maintained more by inertia than by active, performative justification. Accessibility collapse is high (0.8) because, while alternatives exist, the practical barriers to adoption make them largely inaccessible for most users. Resistance is low (0.3) because individual efforts to switch have little impact on the overall standard.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of QWERTY users, the layout is simply 'the way things are,' often without awareness of its suboptimality or the existence of superior alternatives. Manufacturers benefit from the stability but may not actively perceive it as an extractive mechanism. Analytical observers, however, clearly identify the collective suboptimality and the mechanisms of lock-in.
 *
 * DIRECTIONALITY LOGIC:
 *   Keyboard users are the primary payers, bearing the costs of suboptimality. QWERTY manufacturers are diffuse beneficiaries, gaining from market stability and reduced R&D. Alternative manufacturers are excluded, unable to penetrate the locked-in market. Economic historians serve as analytical observers. The lock-in mechanism itself, rather than active enforcement, drives the directionality.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    active_extraction_vs_passive_lock_in,
    'To what extent does QWERTY''s persistence result from active, strategic maintenance by manufacturers to protect their investments (beneficiary_extraction_reading), versus passive lock-in due to network effects and switching costs (lock_in_reading)?',
    'Historical analysis of manufacturer lobbying efforts, patent defense strategies, and marketing campaigns specifically aimed at suppressing alternative layouts, compared against the independent growth of network effects.',
    'If active extraction is dominant, the constraint would reclassify closer to a Snare or Tangled Rope, with higher extractiveness and identifiable beneficiaries. If passive lock-in is dominant, the Piton classification holds, emphasizing diffuse costs and inertial persistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(active_extraction_vs_passive_lock_in, empirical, 'Distinguishing between active rent-seeking and passive market failure in QWERTY''s persistence.').

omega_variable(
    suboptimality_vs_adequacy,
    'Is QWERTY genuinely suboptimal in modern contexts, or has it become ''good enough'' such that the costs of switching outweigh the benefits of alternatives (naturalization_reading)?',
    'Further empirical studies comparing typing efficiency, error rates, and user satisfaction across QWERTY and alternative layouts, coupled with a comprehensive cost-benefit analysis of a hypothetical market-wide transition.',
    'If QWERTY is found to be ''good enough,'' the extractiveness and suppression metrics would decrease, potentially reclassifying it as a Rope or even a Mountain (if its adequacy is truly naturalized). If suboptimality is confirmed, the current Piton classification is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suboptimality_vs_adequacy, empirical, 'Assessing the true performance and adequacy of the QWERTY layout in contemporary use.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_mechanism__lock_in_reading, 1870, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t1870, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 1870, 0.05).
narrative_ontology:measurement(qwer_tr_t1900, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 1900, 0.1).
narrative_ontology:measurement(qwer_tr_t1930, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 1930, 0.15).
narrative_ontology:measurement(qwer_tr_t1960, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 1960, 0.2).
narrative_ontology:measurement(qwer_tr_t1990, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(qwer_tr_t2020, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 2020, 0.2).

% Extraction over time
narrative_ontology:measurement(qwer_be_t1870, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 1870, 0.1).
narrative_ontology:measurement(qwer_be_t1900, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 1900, 0.2).
narrative_ontology:measurement(qwer_be_t1930, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 1930, 0.3).
narrative_ontology:measurement(qwer_be_t1960, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 1960, 0.4).
narrative_ontology:measurement(qwer_be_t1990, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 1990, 0.43).
narrative_ontology:measurement(qwer_be_t2020, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 2020, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t1870, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 1870, 0.1).
narrative_ontology:measurement(qwer_su_t1900, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 1900, 0.3).
narrative_ontology:measurement(qwer_su_t1930, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 1930, 0.5).
narrative_ontology:measurement(qwer_su_t1960, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 1960, 0.6).
narrative_ontology:measurement(qwer_su_t1990, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(qwer_su_t2020, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 2020, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_mechanism__lock_in_reading, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
