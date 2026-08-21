% ============================================================================
% CONSTRAINT STORY: imposition_mechanism_kernel__hybrid_legitimation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_mechanism_kernel__hybrid_legitimation_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: imposition_mechanism_kernel__hybrid_legitimation_reading
 *   human_readable: Hybrid Legitimation of New Norms (Imperial Context)
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This constraint describes the process by which new norms gain legitimacy
 *   in an imperial context, combining top-down symbolic authority transfer
 *   (e.g., an emperor adopting a new religion or legal code) with
 *   institutional incentives for adoption. It is neither a purely coercive
 *   imposition nor a purely endogenous, bottom-up cultural shift. The claimed
 *   type is 'tangled_rope' because it involves both a genuine coordination
 *   function (unifying norms) and asymmetric extraction (costs borne by
 *   traditionalists and local communities).
 *
 * KEY AGENTS:
 *   - imperial_court: Agenda setter (institutional/arbitrage) — initiates and benefits from new norms
 *   - state_bureaucracy: Agenda setter (organized/constrained) — implements and benefits from new norms
 *   - elite_adopters: Beneficiary (powerful/mobile) — adopts early, gains status
 *   - traditionalist_factions: Payer (moderate/constrained) — resists, bears costs
 *   - local_communities: Payer (powerless/identity_locked) — adapts, bears costs
 *   - historical_analysts: Observer (analytical/analytical) — studies the process
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.45).
domain_priors:suppression_score(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.55).
domain_priors:theater_ratio(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_mechanism_kernel__hybrid_legitimation_reading, tangled_rope).
narrative_ontology:human_readable(imposition_mechanism_kernel__hybrid_legitimation_reading, "Hybrid Legitimation of New Norms (Imperial Context)").
narrative_ontology:topic_domain(imposition_mechanism_kernel__hybrid_legitimation_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(imposition_mechanism_kernel__hybrid_legitimation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_mechanism_kernel__hybrid_legitimation_reading, '5129461a-efd3-433c-a0ad-01f78ed8eab1').
narrative_ontology:cs_kernel_codification('5129461a-efd3-433c-a0ad-01f78ed8eab1', formalized).
narrative_ontology:cs_authority_grounding('5129461a-efd3-433c-a0ad-01f78ed8eab1', lineage).
narrative_ontology:cs_interpretation_layer_present('5129461a-efd3-433c-a0ad-01f78ed8eab1').
narrative_ontology:cs_reading_relation('5129461a-efd3-433c-a0ad-01f78ed8eab1', imposition_mechanism_kernel__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('5129461a-efd3-433c-a0ad-01f78ed8eab1', imposition_mechanism_kernel__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('5129461a-efd3-433c-a0ad-01f78ed8eab1', foundational, legitimacy_flows_from_imperial_center).
narrative_ontology:cs_axiom_status(legitimacy_flows_from_imperial_center, holdable).
narrative_ontology:cs_axiom_grounding('5129461a-efd3-433c-a0ad-01f78ed8eab1', legitimacy_flows_from_imperial_center, conventional).
narrative_ontology:cs_axiom('5129461a-efd3-433c-a0ad-01f78ed8eab1', foundational, incentives_shape_cultural_adoption).
narrative_ontology:cs_axiom_status(incentives_shape_cultural_adoption, holdable).
narrative_ontology:cs_axiom_grounding('5129461a-efd3-433c-a0ad-01f78ed8eab1', incentives_shape_cultural_adoption, empirically_contingent).
narrative_ontology:cs_reference_frame('5129461a-efd3-433c-a0ad-01f78ed8eab1', imperial_cultural_unification).
narrative_ontology:cs_drift_state('5129461a-efd3-433c-a0ad-01f78ed8eab1', post_imperial_decline, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('5129461a-efd3-433c-a0ad-01f78ed8eab1', '').
narrative_ontology:cs_kernel_id(imposition_mechanism_kernel__hybrid_legitimation_reading, imposition_mechanism_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__hybrid_legitimation_reading, imperial_court).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__hybrid_legitimation_reading, state_bureaucracy).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__hybrid_legitimation_reading, elite_adopters).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__hybrid_legitimation_reading, traditionalist_factions).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__hybrid_legitimation_reading, local_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Initiates and champions the new norms, leveraging the emperor's symbolic authority. Benefits from increased social cohesion and administrative control, consolidating power through cultural alignment.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, imperial_court, agenda_setter,
    institutional, generational, arbitrage, national).

% Implements the new norms through administrative decrees and incentives. Benefits from a clearer, more standardized legal and social framework, enhancing its efficiency and reach.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, state_bureaucracy, agenda_setter,
    organized, biographical, constrained, national).

% Adopt the new norms early, often due to proximity to imperial power or institutional incentives (e.g., tax breaks, social advancement). They benefit from aligning with the new power structure and setting an example for others.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, elite_adopters, beneficiary,
    powerful, biographical, mobile, regional).

% Resist the new norms, viewing them as an erosion of established customs and local autonomy. They bear the cost of social pressure, loss of traditional status, and potential punitive measures for non-compliance.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, traditionalist_factions, payer,
    moderate, generational, constrained, local).

% Are gradually brought into compliance through a mix of elite example, institutional incentives, and soft coercion. They experience disruption to existing social structures and may internalize the new norms over time, but initially bear the cost of adaptation.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, local_communities, payer,
    powerless, biographical, identity_locked, local).

% Examine the historical record to understand the mechanisms of norm imposition and legitimation, distinguishing between top-down coercion, bottom-up adoption, and hybrid processes.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, historical_analysts, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified set of social and legal norms across a diverse empire, reducing internal friction and facilitating centralized governance by aligning disparate local practices with imperial ideals.
% TRANSFER_FUNCTION: Transfers social capital and legitimacy from the imperial center to the new norms, and compliance from local populations to the state. It also transfers administrative burden from the imperial court to the bureaucracy for enforcement, and cultural adaptation costs to traditionalist groups.
% ABSENT_VOICES: Subaltern groups whose traditional practices are suppressed or marginalized by the new norms, and whose perspectives are not recorded in official histories. Their resistance is often expressed through passive non-compliance or localized cultural preservation rather than overt political challenge.
% DISAPPEARANCE_RATIONALE: If the hybrid legitimation mechanism vanished, the new norms would likely unravel, leading to a resurgence of local traditions, fragmentation of imperial authority, and potential social unrest as the underlying coordination problem re-emerges without a clear solution.
% FOUNDING_PROBLEM: The imperial state faced challenges in governing a vast, culturally diverse territory with disparate local customs, leading to administrative inefficiencies and potential for internal dissent.
% FOUNDING_PROBLEM_CORROBORATION: Historical records from imperial administrators and contemporary scholars attest to the ongoing challenge of maintaining cultural unity and administrative control across diverse populations, even after initial norm establishment. The problem is considered live as long as the empire exists and seeks to maintain cohesion.
narrative_ontology:disappearance_verdict(imposition_mechanism_kernel__hybrid_legitimation_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_mechanism_kernel__hybrid_legitimation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_mechanism_kernel__hybrid_legitimation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(imposition_mechanism_kernel__hybrid_legitimation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_mechanism_kernel__hybrid_legitimation_reading_tests).
:- end_tests(imposition_mechanism_kernel__hybrid_legitimation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate because while there are clear costs to non-compliance and benefits to early adoption, the process relies on a degree of voluntary (incentivized) adoption rather than pure force. Suppression (0.55) is also moderate, reflecting the need for active enforcement and the suppression of alternative traditional norms, but not at the level of overt, widespread violence. Theater ratio (0.2) is low, as the symbolic authority transfer is a genuine component of legitimation, not merely a cover for pure coercion. The stratified adoption pattern (elites first, masses later) is key to this hybrid mechanism.
 *
 * PERSPECTIVAL GAP:
 *   The imperial court and state bureaucracy would perceive this as a successful coordination mechanism, essential for state building and social order. Traditionalist factions and local communities would experience it as an imposition, albeit one with softer edges than pure coercion, leading to a gradual erosion of their cultural autonomy. The engine's per-seat classification would reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   The imperial court and state bureaucracy are clear beneficiaries (low d) as they gain administrative control and legitimacy. Elite adopters also benefit (low d) by aligning with the new power structure. Traditionalist factions and local communities are targets (high d) as they bear the costs of cultural adaptation and suppression of their existing norms. The 'identity_locked' exit for local communities reflects the deep cultural ties that make outright rejection difficult, even in the face of costs.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the process as either pure coordination (ignoring the costs to traditional groups) or pure extraction (ignoring the genuine coordination function and the role of symbolic authority). The 'tangled_rope' accurately captures the hybrid nature, where a legitimate state-building function is intertwined with asymmetric power dynamics and cultural imposition. The 'live' status of the founding problem suggests the constraint's mandate has not atrophied, but its methods are contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    symbolic_vs_material_leverage,
    'What was the precise weighting of symbolic authority (emperor''s charisma) versus material incentives (tax breaks, administrative benefits) in driving norm adoption?',
    'Detailed historical case studies comparing adoption rates and resistance levels in regions with varying exposure to imperial charisma versus direct institutional incentives.',
    'If symbolic authority was dominant, the constraint leans closer to a ''rope'' (voluntary coordination through shared belief); if material incentives were dominant, it leans closer to a ''snare'' (coercion through economic leverage).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbolic_vs_material_leverage, empirical, 'Quantifying the relative influence of symbolic vs. material factors in norm legitimation.').

omega_variable(
    internalized_vs_external_compliance,
    'To what extent did local communities genuinely internalize the new norms versus merely complying due to external pressure?',
    'Archaeological evidence of private practice, analysis of folk narratives, or long-term persistence of traditional customs in the absence of direct state oversight.',
    'If internalization was high, the ''suppression'' metric might be overstated, and the constraint''s long-term stability is higher. If compliance was mostly external, the constraint remains a ''tangled_rope'' or ''snare'' requiring continuous enforcement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internalized_vs_external_compliance, empirical, 'Distinguishing between genuine cultural shift and superficial compliance.').

omega_variable(
    hybrid_legitimation_framing_validity,
    'Is the ''hybrid legitimation'' framing a distinct mechanism, or merely a transitional phase between ''endogenous climb'' and ''exogenous override''?',
    'Comparative historical analysis across multiple empires and norm-setting events, seeking recurring patterns of stable hybridity versus transient states.',
    'If it''s a distinct mechanism, this reading''s classification holds. If it''s a transient, the constraint might be reclassified as a ''scaffold'' leading to one of the other two readings, or as a ''piton'' if the hybridity itself becomes an inertial performance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_legitimation_framing_validity, conceptual, 'Assessing the conceptual distinctiveness of the hybrid legitimation mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_mechanism_kernel__hybrid_legitimation_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t0, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(impo_tr_t25, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 25, 0.15).
narrative_ontology:measurement(impo_tr_t50, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 50, 0.2).
narrative_ontology:measurement(impo_tr_t75, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 75, 0.18).
narrative_ontology:measurement(impo_tr_t100, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 100, 0.2).

% Extraction over time
narrative_ontology:measurement(impo_be_t0, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(impo_be_t25, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 25, 0.4).
narrative_ontology:measurement(impo_be_t50, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 50, 0.45).
narrative_ontology:measurement(impo_be_t75, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 75, 0.42).
narrative_ontology:measurement(impo_be_t100, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 100, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t0, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(impo_su_t25, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 25, 0.5).
narrative_ontology:measurement(impo_su_t50, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 50, 0.55).
narrative_ontology:measurement(impo_su_t75, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 75, 0.52).
narrative_ontology:measurement(impo_su_t100, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 100, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_mechanism_kernel__hybrid_legitimation_reading, identity_coordination).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__hybrid_legitimation_reading, imposition_mechanism_kernel__endogenous_climb_reading).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__hybrid_legitimation_reading, imposition_mechanism_kernel__exogenous_override_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'imposition_mechanism_kernel', focusing on hybrid legitimation. It is linked to the 'endogenous_climb_reading' and 'exogenous_override_reading' as part of a constraint family exploring different mechanisms of norm imposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
