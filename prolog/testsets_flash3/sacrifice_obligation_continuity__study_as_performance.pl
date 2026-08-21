% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_continuity__study_as_performance, []).

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
 *   constraint_id: sacrifice_obligation_continuity__study_as_performance
 *   human_readable: Sacrifice Obligation Continuity: Study as Performance
 *   domain: religious_law/ritual_studies/textual_tradition
 *
 * SUMMARY:
 *   This constraint represents the reading within Jewish law that the study
 *   of the laws of sacrifice is itself considered a fulfillment of the
 *   commandment to offer sacrifices, particularly in the absence of the
 *   Temple. This reading emerged after the destruction of the Second Temple,
 *   providing a mechanism for halakhic continuity and spiritual engagement.
 *   It is classified as a Rope because it genuinely solves a collective
 *   action problem (how to fulfill a central commandment when physical
 *   performance is impossible) with minimal extraction, as study is broadly
 *   accessible and benefits participants. The low extractiveness reflects
 *   that this reading provides a spiritual 'gain' without imposing
 *   significant costs or suppressing alternatives for those who accept its
 *   premise.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__study_as_performance, 0.15).
domain_priors:suppression_score(sacrifice_obligation_continuity__study_as_performance, 0.05).
domain_priors:theater_ratio(sacrifice_obligation_continuity__study_as_performance, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, extractiveness, 0.15).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__study_as_performance, rope).
narrative_ontology:human_readable(sacrifice_obligation_continuity__study_as_performance, "Sacrifice Obligation Continuity: Study as Performance").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__study_as_performance, "religious_law/ritual_studies/textual_tradition").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__study_as_performance, 'b3665422-f336-456d-8ad3-abee41ba5a29').
narrative_ontology:cs_kernel_codification('b3665422-f336-456d-8ad3-abee41ba5a29', fixed_text).
narrative_ontology:cs_authority_grounding('b3665422-f336-456d-8ad3-abee41ba5a29', lineage).
narrative_ontology:cs_interpretation_layer_present('b3665422-f336-456d-8ad3-abee41ba5a29').
narrative_ontology:cs_reading_relation('b3665422-f336-456d-8ad3-abee41ba5a29', sacrifice_obligation_continuity__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('b3665422-f336-456d-8ad3-abee41ba5a29', sacrifice_obligation_continuity__messianic_suspension, coexists_with).
narrative_ontology:cs_reading_relation('b3665422-f336-456d-8ad3-abee41ba5a29', sacrifice_obligation_continuity__archival_preservation, coexists_with).
narrative_ontology:cs_axiom('b3665422-f336-456d-8ad3-abee41ba5a29', foundational, study_is_equivalent_to_performance).
narrative_ontology:cs_axiom_status(study_is_equivalent_to_performance, holdable).
narrative_ontology:cs_axiom_grounding('b3665422-f336-456d-8ad3-abee41ba5a29', study_is_equivalent_to_performance, theological).
narrative_ontology:cs_axiom('b3665422-f336-456d-8ad3-abee41ba5a29', foundational, commandment_remains_binding_in_absence_of_temple).
narrative_ontology:cs_axiom_status(commandment_remains_binding_in_absence_of_temple, holdable).
narrative_ontology:cs_axiom_grounding('b3665422-f336-456d-8ad3-abee41ba5a29', commandment_remains_binding_in_absence_of_temple, deontological).
narrative_ontology:cs_reference_frame('b3665422-f336-456d-8ad3-abee41ba5a29', post_temple_rabbinic_consensus).
narrative_ontology:cs_drift_state('b3665422-f336-456d-8ad3-abee41ba5a29', contemporary_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('b3665422-f336-456d-8ad3-abee41ba5a29', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__study_as_performance, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__study_as_performance, religious_scholars).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__study_as_performance, observant_community).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__study_as_performance, halakhic_continuity_doctrine).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__study_as_performance, textual_engagement_as_worship).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Their intellectual and spiritual work is validated as direct fulfillment of a core religious commandment. They benefit from the continuity of the tradition and the elevated status of textual study.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, religious_scholars, beneficiary,
    institutional, generational, identity_locked, global).

% Finds a path to fulfill ancient religious obligations in the absence of a physical temple or ritual. This reading provides spiritual comfort and a tangible practice for continuity, alleviating guilt or a sense of incompleteness.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, observant_community, beneficiary,
    organized, biographical, constrained, local).

% Believe that only physical sacrifice can fulfill the commandment and that study is merely preparation. They are excluded from the 'fulfillment' aspect of this reading, seeing it as a compromise that delays true restoration.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, messianic_restoration_advocates, excluded,
    moderate, generational, identity_locked, regional).

% Analyze the evolution of religious law and practice, noting how interpretations adapt to changing historical circumstances. They observe the sociological function of this reading in maintaining communal identity and tradition.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, secular_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent and accessible means for the observant community to engage with and fulfill ancient religious obligations related to sacrifice, ensuring continuity of practice and identity in the absence of a physical temple.
% TRANSFER_FUNCTION: Transfers the locus of ritual fulfillment from physical performance to intellectual and spiritual engagement (study), from a geographically fixed site to any place where study can occur, and from a priestly class to any learned individual.
% ABSENT_VOICES: Those who believe only physical performance constitutes fulfillment (e.g., 'performance_only' advocates) are absent from the conversation about 'fulfillment' through study. They would argue that this reading dilutes the commandment's true meaning.
% DISAPPEARANCE_RATIONALE: If this reading vanished, a significant portion of contemporary religious practice would lose its normative grounding. The observant community would face a profound crisis of how to fulfill central commandments, potentially leading to widespread disengagement or the emergence of new, more radical interpretations. The continuity of the tradition would be severely challenged.
% FOUNDING_PROBLEM: The destruction of the Temple and the cessation of physical sacrifices left a central religious commandment unfulfillable, creating a crisis of religious practice and continuity for the observant community.
% FOUNDING_PROBLEM_CORROBORATION: Historical texts and rabbinic commentaries from the post-Temple era corroborate the existential crisis faced by the community. Contemporary religious leaders and scholars outside the immediate beneficiaries continue to affirm the ongoing challenge of maintaining religious practice without the Temple.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__study_as_performance, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__study_as_performance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__study_as_performance, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(sacrifice_obligation_continuity__study_as_performance, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_continuity__study_as_performance, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity__study_as_performance_tests).
:- end_tests(sacrifice_obligation_continuity__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the 'cost' of study is primarily intellectual effort, which is itself considered a spiritual benefit. Suppression is negligible (0.05) as there are no coercive mechanisms to enforce this interpretation; adherence is voluntary. Theater ratio is zero because the act of study is genuinely considered the performance, not a substitute or a pretense. Accessibility collapse is high (0.85) because, for adherents, this reading provides a nearly complete solution to the problem of unfulfillable commandments. Resistance is low (0.02) from within this reading's adherents, though other readings (e.g., 'performance_only') offer conceptual resistance.
 *
 * PERSPECTIVAL GAP:
 *   For adherents of this reading, it is a genuine Rope, providing a vital spiritual and communal function. For those who adhere to a 'performance_only' reading, this constraint might be seen as a conceptual compromise, but it does not directly extract from them. The engine's classification as Rope aligns with the internal logic of this reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious scholars and the observant community are beneficiaries, as this reading validates their spiritual practices and provides a path for commandment fulfillment. There are no direct 'victims' within this reading, as the obligation is considered satisfied. Those who hold alternative readings (e.g., 'messianic_restoration_advocates') might feel excluded from this particular mode of fulfillment, but they are not 'extracted from' by this constraint itself.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves a potential mandatrophy by re-interpreting the mandate of sacrifice in a way that remains 'live' despite changed circumstances. It prevents the original commandment from becoming a Piton (an inert, unfulfillable obligation) by providing a viable, non-theatrical means of fulfillment. The 'founding_problem_status' is 'live' because the problem of unfulfillable physical sacrifice persists, and this reading actively addresses it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    normative_force_of_study,
    'To what extent does textual study genuinely carry the full normative weight and spiritual efficacy of physical sacrifice, or is it a rabbinic accommodation?',
    'Theological and philosophical analysis of primary texts, and comparative study of how other religious traditions adapt core rituals to changed circumstances. No empirical resolution.',
    'If it is a full normative equivalent, the Rope classification holds. If it is primarily an accommodation, it might lean towards a Scaffold (temporary support) or even a Piton (theatrical maintenance of a dead mandate) from an external, more critical perspective, though not from within this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(normative_force_of_study, conceptual, 'Ambiguity regarding the equivalence of study to physical performance.').

omega_variable(
    suppression_of_alternative_fulfillments,
    'Does the widespread acceptance of ''study as performance'' subtly suppress the development or advocacy for alternative, potentially more ''active'' forms of commandment fulfillment (e.g., rebuilding efforts, symbolic rituals)?',
    'Sociological study of religious communities, analysis of internal debates and dissenting voices within the tradition, and historical examination of periods where alternative approaches gained traction.',
    'If significant suppression is found, the ''suppression'' metric would need to be re-evaluated upwards, potentially shifting the classification towards a Tangled Rope, as the coordination function would come with an unacknowledged cost of foreclosed alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_alternative_fulfillments, empirical, 'Whether this reading inadvertently suppresses other forms of religious expression.').

omega_variable(
    kernel_reading_distinction,
    'Is this constraint a genuine reading of the ''sacrifice_obligation_continuity'' kernel, or does it represent a distinct, new commandment that merely references the old?',
    'Deep textual analysis of the interpretive chain from original commandment to this reading, focusing on whether the claim is one of continuity or substitution. This is a matter of internal theological coherence.',
    'If it''s a new commandment, the ''kernel'' framing is less relevant, and the constraint stands on its own. If it''s a reading, the relationships to sibling readings (forecloses/coexists_with) are critical for understanding the broader commitment system.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'Clarifying whether ''study as performance'' is an interpretation or a new mandate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__study_as_performance, 0, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 0, 0.0).
narrative_ontology:measurement(sacr_tr_t500, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 500, 0.0).
narrative_ontology:measurement(sacr_tr_t1000, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 1000, 0.0).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 1500, 0.0).
narrative_ontology:measurement(sacr_tr_t1950, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 1950, 0.0).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(sacr_be_t500, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 500, 0.12).
narrative_ontology:measurement(sacr_be_t1000, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 1000, 0.13).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 1500, 0.14).
narrative_ontology:measurement(sacr_be_t1950, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 1950, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(sacr_su_t500, sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 500, 0.05).
narrative_ontology:measurement(sacr_su_t1000, sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 1000, 0.05).
narrative_ontology:measurement(sacr_su_t1500, sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 1500, 0.05).
narrative_ontology:measurement(sacr_su_t1950, sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 1950, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__study_as_performance, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__study_as_performance, halakhic_continuity_doctrine).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__study_as_performance, textual_engagement_as_worship).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'sacrifice_obligation_continuity' kernel. Its classification as a Rope is specific to this reading, which provides a viable path for commandment fulfillment through study. Other readings of the same kernel yield different classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
