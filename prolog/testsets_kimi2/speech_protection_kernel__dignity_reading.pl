% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__dignity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_kernel__dignity_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: speech_protection_kernel__dignity_reading
 *   human_readable: Speech Protection Conditional on Equal Dignity (Dignity Reading)
 *   domain: constitutional/law/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the dignity reading of the
 *   speech_protection_kernel: speech is constitutionally protected only
 *   insofar as it does not function as structural subordination of target
 *   groups. It is contested by absolutist readings (categorical protection),
 *   marketplace readings (counterspeech remedy), harm-threshold readings
 *   (individual demonstrable harm), and democratic-participation readings
 *   (heightened protection for self-governance speech). The kernel decomposes
 *   into separate constraints because each reading produces a different
 *   epsilon and stakeholder configuration.
 *
 * KEY AGENTS:
 *   - constitutional_court: Primary agenda setter (institutional/analytical) â administers the dignity test and binds lower courts.
 *   - protected_groups: Primary beneficiary (powerless/identity_locked) â receive legal shelter against subordinating expression.
 *   - speech_restricted_actors: Primary payer (moderate/constrained) â bear expressive restrictions and legal penalties.
 *   - civil_liberties_advocates: Excluded voice (organized/mobile) â structurally sidelined in dignity-first jurisprudential architecture.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__dignity_reading, 0.6).
domain_priors:suppression_score(speech_protection_kernel__dignity_reading, 0.68).
domain_priors:theater_ratio(speech_protection_kernel__dignity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__dignity_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__dignity_reading, "Speech Protection Conditional on Equal Dignity (Dignity Reading)").
narrative_ontology:topic_domain(speech_protection_kernel__dignity_reading, "constitutional/law/political_philosophy").

domain_priors:requires_active_enforcement(speech_protection_kernel__dignity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__dignity_reading, '8b659183-5b10-4472-b174-d66edb89fdc1').
narrative_ontology:cs_kernel_codification('8b659183-5b10-4472-b174-d66edb89fdc1', fixed_text).
narrative_ontology:cs_authority_grounding('8b659183-5b10-4472-b174-d66edb89fdc1', lineage).
narrative_ontology:cs_interpretation_layer_present('8b659183-5b10-4472-b174-d66edb89fdc1').
narrative_ontology:cs_reading_relation('8b659183-5b10-4472-b174-d66edb89fdc1', speech_protection_kernel__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('8b659183-5b10-4472-b174-d66edb89fdc1', speech_protection_kernel__harm_threshold_reading, coexists_with).
narrative_ontology:cs_reading_relation('8b659183-5b10-4472-b174-d66edb89fdc1', speech_protection_kernel__marketplace_reading, influences).
narrative_ontology:cs_reading_relation('8b659183-5b10-4472-b174-d66edb89fdc1', speech_protection_kernel__democratic_participation_reading, influences).
narrative_ontology:cs_axiom('8b659183-5b10-4472-b174-d66edb89fdc1', foundational, equal_dignity_as_speech_precondition).
narrative_ontology:cs_axiom_status(equal_dignity_as_speech_precondition, holdable).
narrative_ontology:cs_axiom_grounding('8b659183-5b10-4472-b174-d66edb89fdc1', equal_dignity_as_speech_precondition, deontological).
narrative_ontology:cs_axiom('8b659183-5b10-4472-b174-d66edb89fdc1', foundational, group_subordination_distinct_from_individual_harm).
narrative_ontology:cs_axiom_status(group_subordination_distinct_from_individual_harm, holdable).
narrative_ontology:cs_axiom_grounding('8b659183-5b10-4472-b174-d66edb89fdc1', group_subordination_distinct_from_individual_harm, deontological).
narrative_ontology:cs_reference_frame('8b659183-5b10-4472-b174-d66edb89fdc1', constitutional_dignity_order).
narrative_ontology:cs_drift_state('8b659183-5b10-4472-b174-d66edb89fdc1', contemporary_digital_public_sphere, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8b659183-5b10-4472-b174-d66edb89fdc1', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__dignity_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__dignity_reading, protected_groups).
narrative_ontology:constraint_victim(speech_protection_kernel__dignity_reading, speech_restricted_actors).
narrative_ontology:constraint_vindicates(speech_protection_kernel__dignity_reading, equal_dignity_doctrine).
narrative_ontology:constraint_vindicates(speech_protection_kernel__dignity_reading, group_libel_framework).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the constitutional speech guarantee through the lens of human dignity and group equality; sets doctrinal tests for when expression constitutes structural subordination; its decisions bind legislatures and lower courts, making it the authoritative architect of the constraint.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, constitutional_court, agenda_setter,
    institutional, civilizational, analytical, national).

% Receive legal shelter against expression that systematically demeans, marginalizes, or denies their equal civic standing; their security and participation depend on judicial and administrative enforcement of the dignity condition; they cannot exit the identity that makes them targets of subordination.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, protected_groups, beneficiary,
    powerless, generational, identity_locked, national).

% Bear the cost of expressive restriction when their speech is adjudicated as structurally subordinating; face criminal penalties, civil liability, injunctions, or platform deplatforming under the dignity framework; their alternatives are self-censorship, legal challenge, or emigration to jurisdictions with weaker dignity commitments.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, speech_restricted_actors, payer,
    moderate, biographical, constrained, national).

% Argue that dignity-based restrictions empower majoritarian institutions to police viewpoint and suppress minority dissent under the guise of equality; they appear in dissenting opinions, academic critique, and comparative constitutional debate but are not the authoritative interpreters of the constraint.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, civil_liberties_advocates, excluded,
    organized, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_kernel__dignity_reading, protected_groups).
narrative_ontology:fixing_cost_class(speech_protection_kernel__dignity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents the public sphere from becoming an engine of group-based domination by conditioning speech protection on respect for equal dignity; coordinates plural coexistence by removing the most severe forms of subordinating expression that would otherwise drive marginalized groups out of democratic participation.
% TRANSFER_FUNCTION: Moves expressive liberty away from speakers whose expression functions as structural subordination of target groups, and moves legal protection and civic standing toward those groups.
% ABSENT_VOICES: Free speech absolutists and some minority dissenters are structurally sidelined in dignity-first frameworks; they would argue that majoritarian institutions cannot be trusted to define subordination without suppressing legitimate dissent, and that counterspeech is a less extractive remedy.
% DISAPPEARANCE_RATIONALE: If the dignity condition vanished overnight, subordinating speech would regain legal protection, target groups would lose a specific jurisprudential shield against systemic vilification, and the doctrinal equilibrium between speech and equality would collapse into either absolutism or ad hoc majoritarian balancing.
% FOUNDING_PROBLEM: How to guarantee freedom of expression in a diverse society while preventing speech from becoming a mechanism for entrenching the social and political subordination of historically marginalized groups.
% FOUNDING_PROBLEM_CORROBORATION: Equality-seeking social movements and international human rights bodies attest to the problem from outside the judiciary. Free speech advocates and some dissident minorities contest the framing. Independent comparative sociology on hate crime and group inequality provides partial corroboration, though causal attribution to speech regimes remains disputed.
narrative_ontology:disappearance_verdict(speech_protection_kernel__dignity_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__dignity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__dignity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_protection_kernel__dignity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__dignity_reading, 0.6, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__dignity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_kernel__dignity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_kernel__dignity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60) is moderate-to-high because the constraint genuinely withholds legal protection from a class of expression, extracting expressive liberty from speakers. Suppression (0.68) is high because persistence depends on active judicial and administrative enforcement, including penalties and platform liability. Theater_ratio (0.42) reflects that some enforcement is performative (symbolic prosecutions, ritual condemnation) while the underlying subordination persists in less visible channels. Accessibility_collapse (0.50) is moderate: alternative frameworks (absolutist, marketplace) remain visible and operationally present in other jurisdictions, so alternatives do not fully collapse. Resistance (0.55) captures sustained opposition from civil liberties organizations, dissenting judges, and comparative constitutional critics.
 *
 * PERSPECTIVAL GAP:
 *   From the constitutional court's seat, the constraint is necessary coordination to preserve plural democratic coexistence against domination; from the protected groups' seat, it is a rope preventing forced exit from the public sphere; from the speech-restricted actors' seat, it is extraction of expressive liberty enforced by state power. The engine computes these divergences from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Protected_groups are structural beneficiaries (identity_locked, powerless) and sit near d=0.0, yielding negative or negligible effective extraction. Speech_restricted_actors are structural targets (payer, constrained exit) and sit near d=1.0, yielding amplified effective extraction. The constitutional_court is neither beneficiary nor victim; its directionality reverts to the institutional power-atom fallback, situating it as a low-extraction administrator. Civil_liberties_advocates bear ideological costs but are not direct payers, placing them in a mid-range observer position.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the tangled_rope classification, this constraint would be misread as either a pure snare (ignoring the genuine dignitarian coordination it provides to protected groups) or a pure rope (ignoring the asymmetric extraction from speakers). The temporal measurements show extraction accumulation as the doctrine expanded from narrow group-libel exceptions to broader hate-speech frameworks, confirming that coordination and extraction are co-present and co-evolving.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dignity_reading_kernel_location,
    'This constraint is the dignity reading of the speech_protection_kernel. Sibling readings (absolutist, harm-threshold, marketplace, democratic-participation) share the same kernel but assign different structural priority to dignity, harm, truth-discovery, and political self-governance. What changes if the absolutist reading were adopted instead?',
    'Comparative constitutional analysis across jurisdictions with near-absolutist regimes (e.g., U.S. First Amendment doctrine) versus dignity-based regimes (e.g., Germany, Canada): compare rates of group subordination, minority civic participation, and state censorship.',
    'If absolutist regimes produce comparable minority dignity without restricting speech, the dignity reading''s coordination claim weakens and extraction dominates; if dignity regimes show superior equality outcomes, the coordination function is vindicated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dignity_reading_kernel_location, conceptual, 'Kernel reading ambiguity: whether dignity-based restriction is necessary coordination or extraction.').

omega_variable(
    group_subordination_operationalization,
    'Can group subordination be operationalized in adjudication without collapsing into subjective offense, institutional bias, or majoritarian preference?',
    'Empirical study of judicial outcomes: inter-rater reliability among judges applying dignity tests; correlation between judicial findings and independent sociological measures of group subordination.',
    'If operationalization fails, the constraint''s extraction is arbitrary and suppressive; if it succeeds, the coordination function is structurally validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(group_subordination_operationalization, empirical, 'Whether the dignity test can be applied objectively or inherently licenses viewpoint suppression.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (legal penalties and state enforcement) or internalized (chilling effect, self-censorship driven by unpredictable doctrinal boundaries)?',
    'Post-decision speech trajectory analysis: measure changes in speaker behavior after high-profile dignity-based restrictions; if suppressed speech patterns persist below the legal threshold, suppression is partially internalized.',
    'If internalized, the constraint''s effective suppression exceeds the structural measure â speakers self-censor beyond the legal boundary, amplifying extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism in dignitarian speech regimes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__dignity_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spkd_tr_t0, speech_protection_kernel__dignity_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(spkd_tr_t12, speech_protection_kernel__dignity_reading, theater_ratio, 12, 0.15).
narrative_ontology:measurement(spkd_tr_t24, speech_protection_kernel__dignity_reading, theater_ratio, 24, 0.22).
narrative_ontology:measurement(spkd_tr_t36, speech_protection_kernel__dignity_reading, theater_ratio, 36, 0.3).
narrative_ontology:measurement(spkd_tr_t48, speech_protection_kernel__dignity_reading, theater_ratio, 48, 0.36).
narrative_ontology:measurement(spkd_tr_t60, speech_protection_kernel__dignity_reading, theater_ratio, 60, 0.4).
narrative_ontology:measurement(spkd_tr_t70, speech_protection_kernel__dignity_reading, theater_ratio, 70, 0.42).

% Extraction over time
narrative_ontology:measurement(spkd_be_t0, speech_protection_kernel__dignity_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(spkd_be_t12, speech_protection_kernel__dignity_reading, base_extractiveness, 12, 0.3).
narrative_ontology:measurement(spkd_be_t24, speech_protection_kernel__dignity_reading, base_extractiveness, 24, 0.4).
narrative_ontology:measurement(spkd_be_t36, speech_protection_kernel__dignity_reading, base_extractiveness, 36, 0.5).
narrative_ontology:measurement(spkd_be_t48, speech_protection_kernel__dignity_reading, base_extractiveness, 48, 0.56).
narrative_ontology:measurement(spkd_be_t60, speech_protection_kernel__dignity_reading, base_extractiveness, 60, 0.58).
narrative_ontology:measurement(spkd_be_t70, speech_protection_kernel__dignity_reading, base_extractiveness, 70, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(spkd_su_t0, speech_protection_kernel__dignity_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(spkd_su_t12, speech_protection_kernel__dignity_reading, suppression_requirement, 12, 0.35).
narrative_ontology:measurement(spkd_su_t24, speech_protection_kernel__dignity_reading, suppression_requirement, 24, 0.45).
narrative_ontology:measurement(spkd_su_t36, speech_protection_kernel__dignity_reading, suppression_requirement, 36, 0.55).
narrative_ontology:measurement(spkd_su_t48, speech_protection_kernel__dignity_reading, suppression_requirement, 48, 0.62).
narrative_ontology:measurement(spkd_su_t60, speech_protection_kernel__dignity_reading, suppression_requirement, 60, 0.66).
narrative_ontology:measurement(spkd_su_t70, speech_protection_kernel__dignity_reading, suppression_requirement, 70, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__dignity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, speech_protection_kernel__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, speech_protection_kernel__harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, speech_protection_kernel__marketplace_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, speech_protection_kernel__democratic_participation_reading).

% DUAL FORMULATION NOTE:
% The speech_protection_kernel decomposes into multiple structurally distinct constraints because each reading assigns a different priority to dignity, harm, truth-discovery, and democratic governance, producing different epsilon values, beneficiary/victim structures, and directionality profiles. This story is the dignity reading; siblings represent alternative readings of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
