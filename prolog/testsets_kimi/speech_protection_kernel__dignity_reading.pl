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
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint story instantiates the dignity reading of the
 *   speech_protection_kernel. Under this reading, speech is constitutionally
 *   protected only insofar as it does not function as structural
 *   subordination of target groups. Group harm is treated as distinct from
 *   individual harm, and hate speech or group libel is excluded from
 *   protection. The reading is formalized in jurisdictions such as Canada,
 *   Germany, and South Africa, and stands in direct tension with the
 *   absolutist reading dominant in the United States. As a kernel reading, it
 *   is one of five structurally distinct constraints emitted by the same
 *   natural-language label ('free speech').
 *
 * KEY AGENTS:
 *   - historically_marginalized_groups: Primary beneficiary (organized/constrained) â receive protection from subordinating expression
 *   - purveyors_of_subordinating_speech: Primary payer (moderate/constrained) â bear expressive restrictions and legal sanctions
 *   - constitutional_courts: Agenda-setter (institutional/arbitrage) â adjudicate the dignity boundary and enforce through precedent
 *   - free_speech_advocates: Excluded voice (organized/constrained) â structurally outside this reading's legitimating framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__dignity_reading, 0.62).
domain_priors:suppression_score(speech_protection_kernel__dignity_reading, 0.74).
domain_priors:theater_ratio(speech_protection_kernel__dignity_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__dignity_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__dignity_reading, "Speech Protection Conditional on Equal Dignity (Dignity Reading)").
narrative_ontology:topic_domain(speech_protection_kernel__dignity_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(speech_protection_kernel__dignity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__dignity_reading, 'd50ec824-f31e-40f6-9353-73dbaa2556f4').
narrative_ontology:cs_kernel_codification('d50ec824-f31e-40f6-9353-73dbaa2556f4', formalized).
narrative_ontology:cs_authority_grounding('d50ec824-f31e-40f6-9353-73dbaa2556f4', lineage).
narrative_ontology:cs_interpretation_layer_present('d50ec824-f31e-40f6-9353-73dbaa2556f4').
narrative_ontology:cs_reading_relation('d50ec824-f31e-40f6-9353-73dbaa2556f4', speech_protection_kernel__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('d50ec824-f31e-40f6-9353-73dbaa2556f4', speech_protection_kernel__harm_threshold_reading, coexists_with).
narrative_ontology:cs_reading_relation('d50ec824-f31e-40f6-9353-73dbaa2556f4', speech_protection_kernel__marketplace_reading, coexists_with).
narrative_ontology:cs_reading_relation('d50ec824-f31e-40f6-9353-73dbaa2556f4', speech_protection_kernel__democratic_participation_reading, coexists_with).
narrative_ontology:cs_axiom('d50ec824-f31e-40f6-9353-73dbaa2556f4', foundational, equal_dignity_as_speech_precondition).
narrative_ontology:cs_axiom_status(equal_dignity_as_speech_precondition, holdable).
narrative_ontology:cs_axiom_grounding('d50ec824-f31e-40f6-9353-73dbaa2556f4', equal_dignity_as_speech_precondition, deontological).
narrative_ontology:cs_axiom('d50ec824-f31e-40f6-9353-73dbaa2556f4', foundational, group_subordination_distinct_from_individual_offense).
narrative_ontology:cs_axiom_status(group_subordination_distinct_from_individual_offense, holdable).
narrative_ontology:cs_axiom_grounding('d50ec824-f31e-40f6-9353-73dbaa2556f4', group_subordination_distinct_from_individual_offense, deontological).
narrative_ontology:cs_reference_frame('d50ec824-f31e-40f6-9353-73dbaa2556f4', equal_dignity_constitutional_order).
narrative_ontology:cs_drift_state('d50ec824-f31e-40f6-9353-73dbaa2556f4', contemporary_populist_free_speech_resurgence, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('d50ec824-f31e-40f6-9353-73dbaa2556f4', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__dignity_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__dignity_reading, historically_marginalized_groups).
narrative_ontology:constraint_victim(speech_protection_kernel__dignity_reading, purveyors_of_subordinating_speech).
narrative_ontology:constraint_vindicates(speech_protection_kernel__dignity_reading, equal_dignity_doctrine).
narrative_ontology:constraint_vindicates(speech_protection_kernel__dignity_reading, group_libel_exception).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive constitutional protection from expression that would further their social subordination; their equal dignity is the explicit condition for the speech regime's legitimacy. Exit from this protection would mean leaving the constitutional order or accepting subordination.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, historically_marginalized_groups, beneficiary,
    organized, generational, constrained, national).

% Bear the cost of criminal or civil sanctions when their expression is judged to function as structural subordination of target groups; their speech is actively suppressed under this reading.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, purveyors_of_subordinating_speech, payer,
    moderate, biographical, constrained, national).

% Adjudicate the boundary between protected expression and subordinating speech; set precedents that determine which groups and which expressive acts fall under the dignity exception; enforce through legal sanction.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, constitutional_courts, agenda_setter,
    institutional, generational, arbitrage, national).

% Argue for categorical or near-categorical protection; their preferred absolutist framework is treated as illegitimate within the dignity reading's structure and positioned outside its legitimating logic.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, free_speech_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates pluralistic democratic coexistence by establishing that speech protection does not extend to expression which functions to entrench the structural subordination of historically marginalized groups, thereby attempting to secure equal dignity as a precondition for legitimate public discourse.
% TRANSFER_FUNCTION: Transfers the burden of expressive restriction from targeted groups (who would otherwise absorb the harms of subordination) to speakers whose expression is adjudicated as subordinating; transfers interpretive authority over the boundary to constitutional courts and legislative bodies.
% ABSENT_VOICES: Free speech absolutists and marketplace-of-ideas proponents are present in the broader debate but structurally excluded from this reading's framework; they would argue that any viewpoint-based restriction corrupts the speech regime, but the dignity reading treats their position as outside its legitimating logic.
% DISAPPEARANCE_RATIONALE: The constitutional order would lose its explicit protection against group subordination; historically marginalized groups would face renewed exposure to hate speech and group libel without this shield; courts would revert to alternative readings (absolutist or marketplace); the jurisprudential architecture of speech rights would reorganize around a different core premise.
% FOUNDING_PROBLEM: How to sustain robust speech protection in a constitutional democracy without permitting speech to be used as an instrument for the systematic silencing, degradation, and structural subordination of historically marginalized groups.
% FOUNDING_PROBLEM_CORROBORATION: Historically marginalized groups and international human rights institutions corroborate the ongoing reality of group subordination through speech. Free speech scholars and absolutist jurists corroborate that the problem is either overstated or that dignity-based restrictions produce worse harms (chilling effects, viewpoint discrimination). No corroboration exists from a seat entirely outside the dispute; the question itself is the axis of contest.
narrative_ontology:disappearance_verdict(speech_protection_kernel__dignity_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__dignity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__dignity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_protection_kernel__dignity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__dignity_reading, 0.62, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.62) reflects the active suppression of entire categories of expression. Suppression (0.74) is high because the constraint requires continuous state enforcement through courts and criminal/civil law. Theater ratio (0.45) is moderate: dignity rhetoric performs important legitimation work, but the harms addressed are materially real. Accessibility collapse (0.50) is moderate because alternative readings (absolutist, marketplace) remain culturally and institutionally visible, especially in comparative constitutional discourse. Resistance (0.65) is substantial due to persistent legal and philosophical challenge from free speech advocates and the difficulty of drawing stable doctrinal lines.
 *
 * PERSPECTIVAL GAP:
 *   Historically marginalized groups experience the constraint as protective coordination â a rope-like guarantee of equal standing â while restricted speakers experience it as direct extraction. Constitutional courts occupy an agenda-setting seat with arbitrage exit (they can shift doctrine), whereas free speech advocates are structurally excluded from the framework's internal legitimacy. The engine computes this divergence from the structural data; the author does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically marginalized groups are declared beneficiaries (low directionality, subsidized by the constraint's protection). Purveyors of subordinating speech are declared victims/payers (high directionality, extraction via legal suppression). Courts are agenda-setters with maneuver room (arbitrage exit). No override is needed: the derivation chain produces accurate d values from beneficiary/victim declarations plus exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is classified as tangled rope because it possesses both a genuine coordination function (securing equal dignity in a pluralistic society) and asymmetric extraction (active suppression of speakers). It is not a snare because the coordination is not cover for extraction â the dignity rationale is doctrinally central and institutionally sincere. It is not a piton because its function is not atrophied; enforcement is active and contested. It is not a mountain because it is plainly constructed and enforced, not a natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subordination_empirical_efficacy,
    'Does restricting subordinating speech under the dignity reading actually reduce structural subordination, or does it displace expression without altering group hierarchies?',
    'Comparative longitudinal studies of jurisdictions with dignity readings versus marketplace readings, measuring subordination indicators (employment discrimination, hate crime, political representation) while controlling for confounders.',
    'If ineffective, the coordination function is performative and the constraint slides toward snare; if effective, the tangled rope classification is supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subordination_empirical_efficacy, empirical, 'Empirical efficacy of dignity-based speech restrictions in reducing subordination.').

omega_variable(
    group_individual_harm_boundary,
    'Can the dignity reading distinguish group-based subordination from individual offense or political criticism without collapsing into viewpoint regulation?',
    'Jurisprudential audit of court decisions: rate of consistent application, political alignment of restricted speech, reversal rates on appeal.',
    'If the boundary is unstable, the constraint''s extraction is arbitrary and its coordination function undermined; classification may shift toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(group_individual_harm_boundary, conceptual, 'Stability of the group-subordination boundary against viewpoint-regulation collapse.').

omega_variable(
    absolutist_foreclosure_nature,
    'Is the foreclosure of the absolutist reading by the dignity reading a logical necessity of the doctrine, or a contingent political choice within specific national histories?',
    'Comparative constitutional history: whether post-genocide or post-apartheid transitions were necessary conditions for the dignity reading''s adoption, or if it is derivable from general constitutional principles.',
    'If contingent, the dignity reading''s claim to universal constitutional validity is weakened; it may be a scaffold for transitional justice rather than a permanent equilibrium.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(absolutist_foreclosure_nature, conceptual, 'Whether the dignity reading''s foreclosure of absolutism is logical or historically contingent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__dignity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spk_dignity_tr_t0, speech_protection_kernel__dignity_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(spk_dignity_tr_t5, speech_protection_kernel__dignity_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(spk_dignity_tr_t10, speech_protection_kernel__dignity_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(spk_dignity_tr_t20, speech_protection_kernel__dignity_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement(spk_dignity_tr_t30, speech_protection_kernel__dignity_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement(spk_dignity_tr_t40, speech_protection_kernel__dignity_reading, theater_ratio, 40, 0.45).

% Extraction over time
narrative_ontology:measurement(spk_dignity_be_t0, speech_protection_kernel__dignity_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(spk_dignity_be_t5, speech_protection_kernel__dignity_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(spk_dignity_be_t10, speech_protection_kernel__dignity_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(spk_dignity_be_t20, speech_protection_kernel__dignity_reading, base_extractiveness, 20, 0.54).
narrative_ontology:measurement(spk_dignity_be_t30, speech_protection_kernel__dignity_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(spk_dignity_be_t40, speech_protection_kernel__dignity_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(spk_dignity_su_t0, speech_protection_kernel__dignity_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(spk_dignity_su_t5, speech_protection_kernel__dignity_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(spk_dignity_su_t10, speech_protection_kernel__dignity_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement(spk_dignity_su_t20, speech_protection_kernel__dignity_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(spk_dignity_su_t30, speech_protection_kernel__dignity_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(spk_dignity_su_t40, speech_protection_kernel__dignity_reading, suppression_requirement, 40, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__dignity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, speech_protection_kernel__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, speech_protection_kernel__harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, speech_protection_kernel__marketplace_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, speech_protection_kernel__democratic_participation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the speech_protection_kernel. The natural-language concept 'free speech' or 'speech protection' decomposes into at least five structurally distinct constraints. The dignity reading and the absolutist reading have mutually exclusive core premises (foreclosure relation), while the dignity reading coexists with harm-threshold, marketplace, and democratic-participation readings in comparative constitutional practice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
