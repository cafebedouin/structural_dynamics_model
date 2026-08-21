% ============================================================================
% CONSTRAINT STORY: qwerty_persistence__incumbent_preservation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence__incumbent_preservation_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: qwerty_persistence__incumbent_preservation_reading
 *   human_readable: QWERTY Keyboard Layout (Incumbent Preservation Reading)
 *   domain: Technology History / Industrial Standards / Path Dependence
 *
 * SUMMARY:
 *   This constraint story analyzes the QWERTY keyboard layout from the
 *   perspective of incumbent preservation. It argues that QWERTY's
 *   persistence, long after its original technical justifications became
 *   obsolete, is maintained by active defense from beneficiaries
 *   (manufacturers, trained typists, training institutions) protecting their
 *   capital investments and established positions. This reading frames QWERTY
 *   as a Tangled Rope, providing a coordination function but primarily
 *   serving as a vehicle for asymmetric extraction and suppression of
 *   alternatives. This is one reading of the 'qwerty_persistence' kernel,
 *   contrasting with a 'lapsed_alternatives_reading' that would emphasize
 *   passive network effects.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence__incumbent_preservation_reading, 0.78).
domain_priors:suppression_score(qwerty_persistence__incumbent_preservation_reading, 0.72).
domain_priors:theater_ratio(qwerty_persistence__incumbent_preservation_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence__incumbent_preservation_reading, tangled_rope).
narrative_ontology:human_readable(qwerty_persistence__incumbent_preservation_reading, "QWERTY Keyboard Layout (Incumbent Preservation Reading)").
narrative_ontology:topic_domain(qwerty_persistence__incumbent_preservation_reading, "Technology History / Industrial Standards / Path Dependence").

domain_priors:requires_active_enforcement(qwerty_persistence__incumbent_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence__incumbent_preservation_reading, '0e211ed0-c093-4047-9003-22e9c0a6af73').
narrative_ontology:cs_kernel_codification('0e211ed0-c093-4047-9003-22e9c0a6af73', formalized).
narrative_ontology:cs_authority_grounding('0e211ed0-c093-4047-9003-22e9c0a6af73', extraction).
narrative_ontology:cs_interpretation_layer_present('0e211ed0-c093-4047-9003-22e9c0a6af73').
narrative_ontology:cs_reading_relation('0e211ed0-c093-4047-9003-22e9c0a6af73', qwerty_persistence__lapsed_alternatives_reading, coexists_with).
narrative_ontology:cs_axiom('0e211ed0-c093-4047-9003-22e9c0a6af73', foundational, incumbent_investment_protection).
narrative_ontology:cs_axiom_status(incumbent_investment_protection, holdable).
narrative_ontology:cs_axiom_grounding('0e211ed0-c093-4047-9003-22e9c0a6af73', incumbent_investment_protection, instrumental).
narrative_ontology:cs_axiom('0e211ed0-c093-4047-9003-22e9c0a6af73', secondary, network_effect_as_barrier).
narrative_ontology:cs_axiom_status(network_effect_as_barrier, holdable).
narrative_ontology:cs_axiom_grounding('0e211ed0-c093-4047-9003-22e9c0a6af73', network_effect_as_barrier, empirically_contingent).
narrative_ontology:cs_reference_frame('0e211ed0-c093-4047-9003-22e9c0a6af73', qwerty_as_dominant_standard).
narrative_ontology:cs_drift_state('0e211ed0-c093-4047-9003-22e9c0a6af73', contemporary_digital_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('0e211ed0-c093-4047-9003-22e9c0a6af73', '').
narrative_ontology:cs_kernel_id(qwerty_persistence__incumbent_preservation_reading, qwerty_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, qwerty_keyboard_manufacturers).
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, trained_qwerty_typists).
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, typing_training_institutions).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, alternative_keyboard_manufacturers).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, efficiency_seeking_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from existing tooling, supply chains, and a vast pool of trained typists. They actively resist the adoption of alternative layouts by maintaining market dominance and leveraging network effects, protecting their significant capital investments in QWERTY-centric production.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, qwerty_keyboard_manufacturers, agenda_setter,
    institutional, generational, arbitrage, global).

% Benefit from their existing QWERTY skills being universally applicable. They face high retraining costs and social friction if they were to switch to an alternative layout, making their 'choice' to stick with QWERTY a form of identity-locked path dependence.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, trained_qwerty_typists, beneficiary,
    moderate, biographical, identity_locked, global).

% Benefit from a stable, universally accepted curriculum. They resist changes to the standard that would invalidate their existing training materials and instructor expertise, thereby preserving their market position.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, typing_training_institutions, beneficiary,
    organized, generational, constrained, national).

% Bear the costs of market entry, facing significant network effects, consumer inertia, and active resistance from QWERTY incumbents. Their products, though potentially more efficient, struggle to gain widespread adoption.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, alternative_keyboard_manufacturers, payer,
    powerful, biographical, constrained, global).

% Bear the opportunity cost of lower typing speeds and ergonomic disadvantages inherent in the QWERTY layout. They face high switching costs (retraining time, social friction) and limited availability of alternative hardware, making exit difficult despite potential benefits.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, efficiency_seeking_users, payer,
    powerless, immediate, constrained, global).

% Investigate whether the persistence of QWERTY constitutes anti-competitive behavior or market failure. They gather evidence on incumbent practices and market dynamics, and can propose remedies to foster competition or promote more efficient standards.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, competition_authorities, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence__incumbent_preservation_reading, qwerty_keyboard_manufacturers).
narrative_ontology:fixing_cost_class(qwerty_persistence__incumbent_preservation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universally recognized and compatible input method for typewriters and digital keyboards, ensuring interoperability across devices and users.
% TRANSFER_FUNCTION: Transfers market dominance, associated profits, and reduced innovation costs to QWERTY manufacturers and training institutions, while imposing switching costs and efficiency losses on users and manufacturers of alternative layouts.
% ABSENT_VOICES: Users who would benefit from more ergonomically efficient layouts but are unaware of alternatives, or are unable to switch due to the overwhelming network effects and incumbent defense. Their potential demand for alternatives is suppressed.
% DISAPPEARANCE_RATIONALE: If QWERTY vanished overnight, the entire global typing ecosystem would face immense disruption. Hardware would need redesign, software interfaces would change, and billions of typists would need retraining. A new standard would eventually emerge, likely more efficient, but the transition would be chaotic.
% FOUNDING_PROBLEM: The original problem was to prevent mechanical key jamming on early typewriters and to facilitate telegraphers' work by separating common letter pairs.
% FOUNDING_PROBLEM_CORROBORATION: Historians of technology and ergonomic researchers widely corroborate that the mechanical and operational problems QWERTY was designed to solve are obsolete in the digital age. QWERTY manufacturers and some traditionalists might contest this, citing familiarity and training costs as ongoing 'problems' it still solves, but this is not corroborated by independent sources.
narrative_ontology:disappearance_verdict(qwerty_persistence__incumbent_preservation_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence__incumbent_preservation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence__incumbent_preservation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(qwerty_persistence__incumbent_preservation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence__incumbent_preservation_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence__incumbent_preservation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qwerty_persistence__incumbent_preservation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qwerty_persistence__incumbent_preservation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.78) reflects the ongoing costs borne by efficiency-seeking users and alternative manufacturers due to QWERTY's suboptimal design and the suppression of superior alternatives. Suppression (0.72) is high because incumbents actively lobby, market, and leverage network effects to prevent the widespread adoption of alternatives, effectively 'enforcing' QWERTY's dominance. The theater ratio (0.20) is low because the defense of QWERTY is genuinely functional for incumbents, protecting real investments, rather than being purely performative. The founding problem is dead, but the constraint persists due to active defense, making it a clear case of mandatrophy leading to a Tangled Rope classification.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of QWERTY manufacturers and training institutions, the layout is a stable, beneficial standard that ensures compatibility and leverages existing skills. From the perspective of alternative manufacturers and efficiency-seeking users, it is an entrenched barrier that extracts value and suppresses innovation. The engine's per-seat classification will reflect this divergence, with beneficiaries experiencing it as a Rope and victims as a Snare.
 *
 * DIRECTIONALITY LOGIC:
 *   QWERTY keyboard manufacturers, trained typists, and typing training institutions are beneficiaries (low directionality) as they profit from or are locked into the existing standard. Alternative keyboard manufacturers and efficiency-seeking users are victims (high directionality) as they bear the costs of QWERTY's dominance. Competition authorities are observers, analyzing the structural dynamics.
 *
 * MANDATROPHY ANALYSIS:
 *   The original mandate for QWERTY (preventing key jamming on mechanical typewriters) is entirely dead in the digital era. Its persistence is now driven by the active defense of incumbent beneficiaries protecting their capital investments and the inertia of trained typists. This shift from a functional coordination solution to a mechanism for incumbent preservation and extraction is the core of its mandatrophy, leading to its classification as a Tangled Rope rather than a benign Rope or a degraded Piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    active_defense_vs_passive_network_effects,
    'To what extent does QWERTY''s persistence stem from active defense by incumbents (as this reading claims) versus passive network effects and user inertia (as the ''lapsed_alternatives_reading'' might emphasize)?',
    'Empirical studies analyzing lobbying efforts, marketing spend, and anti-competitive practices by QWERTY incumbents, compared against the adoption curves of alternative layouts in the absence of such interventions.',
    'If active defense is the dominant factor, the constraint is more extractive and suppressive (Tangled Rope/Snare). If passive network effects are dominant, it leans more towards a Rope or Piton, reflecting a less coercive persistence mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(active_defense_vs_passive_network_effects, empirical, 'Distinguishing active incumbent preservation from passive market dynamics in QWERTY''s persistence.').

omega_variable(
    counterfactual_incumbent_withdrawal,
    'What would be the market trajectory of alternative keyboard layouts if QWERTY incumbents ceased all active defense and promotion, allowing a ''level playing field''?',
    'A large-scale, controlled social experiment or a detailed historical analysis of similar standard transitions where incumbent defense was absent or significantly reduced.',
    'If alternatives rapidly gained traction, it would strongly support the high extractiveness and suppression claims of this reading. If adoption remained slow, it would suggest a lower baseline extractiveness and suppression, even without incumbent intervention.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_incumbent_withdrawal, empirical, 'Assessing the impact of removing incumbent defense on alternative layout adoption.').

omega_variable(
    typist_suppression_mechanism_ambiguity,
    'Is the suppression experienced by trained QWERTY typists (e.g., high retraining costs, social friction) primarily structural (lack of alternative hardware, software defaults) or internalized (belief that QWERTY is ''normal'' or ''good enough'', fear of change)?',
    'Post-exposure studies: if typists, after being exposed to and trained on alternatives, still resist switching even when structural barriers are lowered, it suggests a higher internalized component.',
    'If internalized, the effective suppression is higher than structural measures suggest, as typists carry the suppression with them. If purely structural, lowering external barriers would be sufficient to enable exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(typist_suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for QWERTY typists.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence__incumbent_preservation_reading, 1878, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t1878, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 1878, 0.1).
narrative_ontology:measurement(qwer_tr_t1920, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 1920, 0.12).
narrative_ontology:measurement(qwer_tr_t1960, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 1960, 0.15).
narrative_ontology:measurement(qwer_tr_t1990, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(qwer_tr_t2010, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 2010, 0.19).
narrative_ontology:measurement(qwer_tr_t2024, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(qwer_be_t1878, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 1878, 0.4).
narrative_ontology:measurement(qwer_be_t1920, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 1920, 0.55).
narrative_ontology:measurement(qwer_be_t1960, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 1960, 0.65).
narrative_ontology:measurement(qwer_be_t1990, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 1990, 0.7).
narrative_ontology:measurement(qwer_be_t2010, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 2010, 0.75).
narrative_ontology:measurement(qwer_be_t2024, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t1878, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 1878, 0.3).
narrative_ontology:measurement(qwer_su_t1920, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 1920, 0.45).
narrative_ontology:measurement(qwer_su_t1960, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 1960, 0.58).
narrative_ontology:measurement(qwer_su_t1990, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(qwer_su_t2010, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement(qwer_su_t2024, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence__incumbent_preservation_reading, information_standard).
narrative_ontology:affects_constraint(qwerty_persistence__incumbent_preservation_reading, typing_speed_norms).
narrative_ontology:affects_constraint(qwerty_persistence__incumbent_preservation_reading, keyboard_manufacturing_supply_chains).
narrative_ontology:affects_constraint(qwerty_persistence__incumbent_preservation_reading, digital_literacy_curricula).

% DUAL FORMULATION NOTE:
% This constraint is the 'incumbent_preservation_reading' of the 'qwerty_persistence' kernel, which also includes the 'lapsed_alternatives_reading'. Both are distinct analyses of the same phenomenon, linked here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
