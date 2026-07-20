% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_mechanism__naturalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_mechanism__naturalization_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: qwerty_persistence_mechanism__naturalization_reading
 *   human_readable: QWERTY Persistence via Functional Adequacy (Naturalization Reading)
 *   domain: economic_history/technology_studies
 *
 * SUMMARY:
 *   This constraint instantiates the naturalization reading of the
 *   qwerty_persistence_mechanism kernel: the claim that the QWERTY keyboard
 *   layout persists not through lock-in or incumbent extraction, but because
 *   it became and remains genuinely adequate for general-purpose typing,
 *   while alternatives lapsed through fair competition. No systematic
 *   beneficiary extracts rents from its persistence; switching costs reflect
 *   real human-capital investment rather than artificial barriers. The
 *   constraint is authored as a rope (pure coordination) with low
 *   extractiveness and negligible suppression, independent of the contested
 *   sibling readings.
 *
 * KEY AGENTS:
 *   - Keyboard users (organized/biographical/constrained): Diffuse beneficiaries of global interoperability; bear only the opportunity cost of alternatives they rationally decline.
 *   - Hardware vendors (organized/mobile): Passive producers following demand; no structural role in suppressing alternatives.
 *   - Ergonomic reform advocates (moderate/constrained): Promote alternatives on merit; lost in open competition, not excluded by coercion.
 *   - Economic historians (institutional/analytical): Analytical observers evaluating adequacy versus lock-in claims.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_mechanism__naturalization_reading, 0.12).
domain_priors:suppression_score(qwerty_persistence_mechanism__naturalization_reading, 0.08).
domain_priors:theater_ratio(qwerty_persistence_mechanism__naturalization_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_mechanism__naturalization_reading, rope).
narrative_ontology:human_readable(qwerty_persistence_mechanism__naturalization_reading, "QWERTY Persistence via Functional Adequacy (Naturalization Reading)").
narrative_ontology:topic_domain(qwerty_persistence_mechanism__naturalization_reading, "economic_history/technology_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_mechanism__naturalization_reading, 'b5488cf2-4b0c-4790-bba4-cfbbdfbc725e').
narrative_ontology:cs_kernel_codification('b5488cf2-4b0c-4790-bba4-cfbbdfbc725e', distributed).
narrative_ontology:cs_authority_grounding('b5488cf2-4b0c-4790-bba4-cfbbdfbc725e', distributed).
narrative_ontology:cs_reading_relation('b5488cf2-4b0c-4790-bba4-cfbbdfbc725e', qwerty_persistence_mechanism__lock_in_reading, forecloses).
narrative_ontology:cs_reading_relation('b5488cf2-4b0c-4790-bba4-cfbbdfbc725e', qwerty_persistence_mechanism__beneficiary_extraction_reading, forecloses).
narrative_ontology:cs_axiom('b5488cf2-4b0c-4790-bba4-cfbbdfbc725e', foundational, adequacy_as_sufficient_cause).
narrative_ontology:cs_axiom_status(adequacy_as_sufficient_cause, holdable).
narrative_ontology:cs_axiom_grounding('b5488cf2-4b0c-4790-bba4-cfbbdfbc725e', adequacy_as_sufficient_cause, empirically_contingent).
narrative_ontology:cs_axiom('b5488cf2-4b0c-4790-bba4-cfbbdfbc725e', foundational, skill_investment_is_real_cost).
narrative_ontology:cs_axiom_status(skill_investment_is_real_cost, holdable).
narrative_ontology:cs_axiom_grounding('b5488cf2-4b0c-4790-bba4-cfbbdfbc725e', skill_investment_is_real_cost, empirically_contingent).
narrative_ontology:cs_reference_frame('b5488cf2-4b0c-4790-bba4-cfbbdfbc725e', decentralized_standard_equilibrium).
narrative_ontology:cs_drift_state('b5488cf2-4b0c-4790-bba4-cfbbdfbc725e', digital_era_globalization, gap(stable, minor, false)).
narrative_ontology:cs_created_at('b5488cf2-4b0c-4790-bba4-cfbbdfbc725e', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_mechanism__naturalization_reading, qwerty_persistence_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__naturalization_reading, keyboard_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% They have invested years of muscle memory in QWERTY and benefit from being able to use any keyboard worldwide without relearning. They see no pressing reason to switch because the layout is fast enough for their work and the cost of retraining would be real lost productivity.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, keyboard_users, beneficiary,
    organized, biographical, constrained, global).

% Study the evolution of keyboard standards and evaluate claims of lock-in versus functional adequacy. They observe that QWERTY's persistence tracks its adequacy for general-purpose typing and that empirical evidence for massive alternative superiority is weak.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, economic_historians, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, interoperable keyboard layout standard that allows any trained operator to use any compliant device without relearning, solving the coordination problem of fragmented input standards across manufacturers and user populations.
% TRANSFER_FUNCTION: No systematic transfer of rents. The arrangement moves only the diffuse convenience of interoperability to users; any 'cost' is the opportunity cost of alternative layouts, which users rationally decline because QWERTY remains functionally adequate.
% ABSENT_VOICES: Alternative-layout advocates are present in public discourse but are not structurally silenced; their arguments have been heard and tested in the market and in empirical studies, and have not persuaded a critical mass because the claimed superiority remains contested.
% DISAPPEARANCE_RATIONALE: If QWERTY vanished overnight, billions of users would face immediate typing friction, hardware would lack a common interface, and a scramble to coordinate a replacement standard would ensue. The rearrangement would reflect the collapse of a genuine coordination equilibrium, not the fall of an extractive barrier.
% FOUNDING_PROBLEM: How to establish a functional, interoperable keyboard layout for mass-produced typing equipment that operators could learn once and deploy across devices from different manufacturers.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians and ergonomic researchers outside the layout-advocacy community attest that the original typewriter mechanical constraints were real and that QWERTY's early adoption reflected functional adequacy; contemporary cross-layout studies continue to support the claim that QWERTY performs adequately for general text entry.
narrative_ontology:disappearance_verdict(qwerty_persistence_mechanism__naturalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_mechanism__naturalization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_mechanism__naturalization_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qwerty_persistence_mechanism__naturalization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_mechanism__naturalization_reading, 0.12, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_mechanism__naturalization_reading_tests).
:- end_tests(qwerty_persistence_mechanism__naturalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.12) because no party captures rents from the layout's persistence. Suppression is minimal (0.08) because alternatives were not structurally excluded; Dvorak and Colemak remain installable and teachable. Theater ratio is near zero (0.05) because there is no institutional machinery performing maintenance of a failing standard. Accessibility collapse is moderate (0.40): once a user invests in QWERTY skill, alternatives naturally collapse for that individual, but this is a consequence of genuine human capital, not external barrier construction. Resistance is negligible (0.05) because the standard is widely accepted. The flat measurement series reflect a stable coordination equilibrium without enforcement drift.
 *
 * PERSPECTIVAL GAP:
 *   Keyboard users experience the constraint as transparent infrastructure; ergonomic reform advocates experience it as a suboptimal equilibrium that could be improved. The gap is epistemic and evaluative (how large is the alternative advantage?) rather than extractive: no seat collects from the gap.
 *
 * DIRECTIONALITY LOGIC:
 *   Keyboard users are the sole declared beneficiaries; their directionality sits near the beneficiary end (low d) because the constraint subsidizes their interoperability. No victim group is declared, so no seat sits at the target end. Hardware vendors are not declared as beneficiaries because they do not systematically profit from the layout itself; they are downstream of user demand. The absence of an agenda-setter or rent-capturer is the structural signature that distinguishes this reading from its siblings.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents the mandatrophy error of labeling a genuine coordination standard as a snare or piton. If the founding problem (interoperable text input) is still live and the standard is maintained by diffuse mutual expectation rather than by an extracting party, then classifying it as extractive would misread coordination cost as rent. The rope classification captures that the arrangement solves a real problem, persists because participants are net beneficiaries, and lacks the enforcement machinery or victim structure required for tangled_rope or snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint is the naturalization reading of the qwerty_persistence_mechanism kernel. How would classification change if the lock-in reading (path-dependent coordination failure despite inferiority) or the beneficiary-extraction reading (active incumbent maintenance) were adopted instead?',
    'Compare the structural data across the three sibling constraints in the compiled corpus; the divergence in extractiveness, suppression, and beneficiary/victim structure locates the disagreement.',
    'Adopting the lock-in reading would reclassify as tangled_rope or piton with higher extraction and theater; adopting the extraction reading would reclassify as snare with identified victims and agenda-setter beneficiaries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Committer-frame location of this reading within the contested QWERTY kernel.').

omega_variable(
    dvorak_superiority_contested,
    'Do alternative keyboard layouts demonstrate a sufficiently large, robust, and generalizable advantage in speed or ergonomics over QWERTY to overcome genuine retraining costs?',
    'Meta-analysis of randomized controlled trials and longitudinal workplace studies comparing QWERTY to Dvorak and Colemak, controlling for training intensity and task type.',
    'A large, robust advantage would weaken the naturalization reading and support lock-in; a null or small advantage would corroborate the adequacy claim and sustain the rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dvorak_superiority_contested, empirical, 'Empirical status of alternative layout superiority.').

omega_variable(
    switching_cost_nature,
    'Does the cost of switching from QWERTY reflect genuine depreciable human capital (biologically real skill), or is it an artificial coordination friction that would dissolve if a critical mass adopted an alternative?',
    'Neuroplasticity and motor-learning research on procedural memory transfer between keyboard layouts; natural experiments from populations that have switched (e.g., post-Soviet keyboard changes).',
    'If switching cost is primarily genuine human capital, the constraint is a benign rope; if it is primarily artificial friction, the lock-in reading gains structural support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(switching_cost_nature, conceptual, 'Whether switching costs are natural skill investment or artificial barrier.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_mechanism__naturalization_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwerty_nat_tr_t0, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 0, 0.04).
narrative_ontology:measurement(qwerty_nat_tr_t10, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 10, 0.04).
narrative_ontology:measurement(qwerty_nat_tr_t20, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 20, 0.05).
narrative_ontology:measurement(qwerty_nat_tr_t30, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 30, 0.05).
narrative_ontology:measurement(qwerty_nat_tr_t40, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 40, 0.05).
narrative_ontology:measurement(qwerty_nat_tr_t50, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 50, 0.05).

% Extraction over time
narrative_ontology:measurement(qwerty_nat_be_t0, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(qwerty_nat_be_t10, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 10, 0.1).
narrative_ontology:measurement(qwerty_nat_be_t20, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 20, 0.11).
narrative_ontology:measurement(qwerty_nat_be_t30, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 30, 0.11).
narrative_ontology:measurement(qwerty_nat_be_t40, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 40, 0.12).
narrative_ontology:measurement(qwerty_nat_be_t50, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 50, 0.12).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(qwerty_persistence_mechanism__naturalization_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_mechanism__naturalization_reading, information_standard).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__naturalization_reading, lock_in_reading).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__naturalization_reading, beneficiary_extraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the qwerty_persistence_mechanism kernel. The naturalization reading decomposes the colloquial 'QWERTY lock-in' into a distinct structural claim: persistence through functional adequacy and fair competition, with negligible extraction. The kernel separates into three sibling constraints because the lock-in, extraction, and naturalization framings produce structurally different epsilon values, beneficiary/victim structures, and directionality profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
