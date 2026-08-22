% ============================================================================
% CONSTRAINT STORY: qwerty_persistence__lapsed_alternatives_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence__lapsed_alternatives_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: qwerty_persistence__lapsed_alternatives_reading
 *   human_readable: QWERTY Persistence via Lapsed Alternatives (Coordination Reading)
 *   domain: technology history / industrial standards / path dependence
 *
 * SUMMARY:
 *   The QWERTY keyboard layout has persisted since the 1870s. This constraint
 *   story instantiates the 'lapsed_alternatives_reading' of the contested
 *   kernel 'qwerty_persistence': the claim that the layout persists not
 *   because incumbents actively defend it, but because decentralized
 *   coordination value and network effects created a self-sustaining
 *   equilibrium. Once a critical mass of users, manufacturers, and training
 *   institutions converged on QWERTY, switching costs for any individual
 *   actor became prohibitive, causing competing standards to lapse. No
 *   identifiable party extracts asymmetric rents; the constraint is a pure
 *   coordination equilibrium.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence__lapsed_alternatives_reading, 0.28).
domain_priors:suppression_score(qwerty_persistence__lapsed_alternatives_reading, 0.05).
domain_priors:theater_ratio(qwerty_persistence__lapsed_alternatives_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence__lapsed_alternatives_reading, rope).
narrative_ontology:human_readable(qwerty_persistence__lapsed_alternatives_reading, "QWERTY Persistence via Lapsed Alternatives (Coordination Reading)").
narrative_ontology:topic_domain(qwerty_persistence__lapsed_alternatives_reading, "technology history / industrial standards / path dependence").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence__lapsed_alternatives_reading, '2cada23b-521a-4a76-8f63-bd67b67f7233').
narrative_ontology:cs_kernel_codification('2cada23b-521a-4a76-8f63-bd67b67f7233', formalized).
narrative_ontology:cs_authority_grounding('2cada23b-521a-4a76-8f63-bd67b67f7233', self_enforcing).
narrative_ontology:cs_reading_relation('2cada23b-521a-4a76-8f63-bd67b67f7233', qwerty_persistence__incumbent_preservation_reading, coexists_with).
narrative_ontology:cs_axiom('2cada23b-521a-4a76-8f63-bd67b67f7233', foundational, decentralized_coordination_suffices).
narrative_ontology:cs_axiom_status(decentralized_coordination_suffices, holdable).
narrative_ontology:cs_axiom_grounding('2cada23b-521a-4a76-8f63-bd67b67f7233', decentralized_coordination_suffices, empirically_contingent).
narrative_ontology:cs_axiom('2cada23b-521a-4a76-8f63-bd67b67f7233', secondary, no_extraction_necessary_for_persistence).
narrative_ontology:cs_axiom_status(no_extraction_necessary_for_persistence, holdable).
narrative_ontology:cs_axiom_grounding('2cada23b-521a-4a76-8f63-bd67b67f7233', no_extraction_necessary_for_persistence, empirically_contingent).
narrative_ontology:cs_reference_frame('2cada23b-521a-4a76-8f63-bd67b67f7233', decentralized_coordination_equilibrium).
narrative_ontology:cs_drift_state('2cada23b-521a-4a76-8f63-bd67b67f7233', contemporary_digital_era, gap(stable, minor, false)).
narrative_ontology:cs_created_at('2cada23b-521a-4a76-8f63-bd67b67f7233', '').
narrative_ontology:cs_kernel_id(qwerty_persistence__lapsed_alternatives_reading, qwerty_persistence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, shared keyboard layout that solves the coordination problem of text input across all users, devices, and software without requiring prior bilateral negotiation.
% TRANSFER_FUNCTION: No asymmetric transfer; all parties bear identical coordination costs (opportunity cost of not using a potentially more efficient layout), and all parties receive identical interoperability benefits.
% ABSENT_VOICES: Alternative-layout advocates (Dvorak, Colemak) and ergonomics researchers argue that ongoing efficiency and injury-prevention losses are ignored; they are structurally excluded because mainstream hardware procurement, operating system defaults, and typing pedagogy never put them on the choice menu.
% DISAPPEARANCE_RATIONALE: Text input is a global infrastructure embedded in billions of devices and billions of trained users; its disappearance would force immediate, costly re-coordination across hardware, software, and human capital.
% FOUNDING_PROBLEM: The need for a shared, predictable text-input interface that does not require per-device or per-user negotiation, originally driven by typewriter mechanical constraints and now sustained by network effects.
% FOUNDING_PROBLEM_CORROBORATION: Information economists and technology historians (e.g., David, Arthur, Liebowitz and Margolis) attest from an analytical seat that network-effects coordination problems remain live; no concentrated beneficiary lobbies for the standard, corroborating the absence of extraction.
narrative_ontology:disappearance_verdict(qwerty_persistence__lapsed_alternatives_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence__lapsed_alternatives_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence__lapsed_alternatives_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qwerty_persistence__lapsed_alternatives_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence__lapsed_alternatives_reading, 0.28, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence__lapsed_alternatives_reading_tests).
:- end_tests(qwerty_persistence__lapsed_alternatives_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is modest (0.28) and driven entirely by symmetric switching costs: every user would need to relearn, every manufacturer retool, and every software stack remap, but no party captures the transfer. Suppression is negligible (0.05) because alternatives are not actively suppressed; they simply fail to reach critical mass against network effects. Theater is minimal (0.05) because persistence is inertial, not performative. Accessibility collapse is high (0.72) because once the standard is understood as dominant, viable alternatives effectively vanish from mainstream retail and education channels. Resistance is near-zero (0.02) because alternative-layout advocates lack the leverage to overcome the coordination equilibrium.
 *
 * PERSPECTIVAL GAP:
 *   Because all parties bear costs symmetrically and no party extracts, there is minimal perspectival divergence; effective directionality is neutral (d â 0.5) for all participants. The few alternative-layout advocates experience higher d, but they are not structurally victimized by an extracting agent.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries or victims are declared, so the engine derives directionality structurally as symmetric for all agents. No overrides are needed. The moderate base extractiveness is damped to low effective extraction (Ï) across all seats because directionality sits near 0.5.
 *
 * MANDATROPHY ANALYSIS:
 *   The absence of a declared beneficiary set prevents the mandatrophy trap of misclassifying coordination as extraction. If evidence later shows that keyboard manufacturers or educational institutions actively suppress alternatives to protect capital investments, the reading would collapse into the sibling 'incumbent_preservation_reading' (Tangled Rope or Snare). Until then, the Rope classification is structurally warranted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    symmetric_cost_or_extraction,
    'Does the base extractiveness of 0.28 represent pure symmetric switching-cost friction, or does it mask hidden asymmetric extraction by hardware manufacturers or educational institutions?',
    'Empirical analysis of keyboard manufacturer margins, typing-course provider contracts, and hardware-software bundling arrangements to detect supracompetitive returns tied to layout lock-in.',
    'If asymmetric returns are found, this constraint dissolves into the sibling incumbent-preservation reading (Tangled Rope or Snare); if absent, the Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symmetric_cost_or_extraction, empirical, 'Ambiguity between symmetric coordination cost and hidden extraction').

omega_variable(
    qwerty_kernel_reading_contest,
    'Is QWERTY persistence best explained by decentralized coordination value or by active incumbent preservation?',
    'Comparative structural analysis of path-dependence models versus incumbent profit-and-lobbying data across the interval.',
    'Resolves which reading of the qwerty_persistence kernel is structurally dominant; determines whether intervention is warranted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(qwerty_kernel_reading_contest, conceptual, 'Kernel-level contest between coordination and extraction readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence__lapsed_alternatives_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwerty_lapsed_alt_tr_t0, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 0, 0.02).
narrative_ontology:measurement(qwerty_lapsed_alt_tr_t25, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 25, 0.02).
narrative_ontology:measurement(qwerty_lapsed_alt_tr_t50, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 50, 0.03).
narrative_ontology:measurement(qwerty_lapsed_alt_tr_t75, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 75, 0.04).
narrative_ontology:measurement(qwerty_lapsed_alt_tr_t100, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 100, 0.05).
narrative_ontology:measurement(qwerty_lapsed_alt_tr_t125, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 125, 0.05).
narrative_ontology:measurement(qwerty_lapsed_alt_tr_t150, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 150, 0.05).

% Extraction over time
narrative_ontology:measurement(qwerty_lapsed_alt_be_t0, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(qwerty_lapsed_alt_be_t25, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 25, 0.14).
narrative_ontology:measurement(qwerty_lapsed_alt_be_t50, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 50, 0.18).
narrative_ontology:measurement(qwerty_lapsed_alt_be_t75, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 75, 0.22).
narrative_ontology:measurement(qwerty_lapsed_alt_be_t100, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 100, 0.25).
narrative_ontology:measurement(qwerty_lapsed_alt_be_t125, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 125, 0.27).
narrative_ontology:measurement(qwerty_lapsed_alt_be_t150, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 150, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(qwerty_persistence__lapsed_alternatives_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence__lapsed_alternatives_reading, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
