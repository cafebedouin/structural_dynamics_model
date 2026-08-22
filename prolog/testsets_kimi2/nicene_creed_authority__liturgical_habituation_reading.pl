% ============================================================================
% CONSTRAINT STORY: nicene_creed_authority__liturgical_habituation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_creed_authority__liturgical_habituation_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: nicene_creed_authority__liturgical_habituation_reading
 *   human_readable: Nicene Creed as Liturgical Identity Boundary Marker
 *   domain: systematic_theology/ecclesiology
 *
 * SUMMARY:
 *   This constraint treats the Nicene Creed not as a metaphysical contract
 *   demanding propositional assent, but as a liturgical performance that
 *   habituates group identity. Recitation marks the boundary between
 *   belonging and non-belonging; the repeated act itself is the authority
 *   source, independent of whether every participant cognitively affirms
 *   every clause. This is one reading of the contested kernel
 *   nicene_creed_authority; sibling readings treat the same text as either a
 *   strict metaphysical bond (strict_orthodox_reading) or a historically
 *   contingent community witness (symbolic_confessional_reading). The low
 *   extraction and low suppression metrics are authored descriptively; the
 *   claimed type is rope.
 *
 * KEY AGENTS:
 *   - liturgical_participants (beneficiary/moderate/constrained): receive belonging through ritual performance without doctrinal examination
 *   - liturgical_presiders (agenda_setter/organized/constrained): administer the liturgical recitation and maintain the practice
 *   - orthodox_guardians (excluded/institutional/analytical): would demand metaphysical assent but are backgrounded in this reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_creed_authority__liturgical_habituation_reading, 0.08).
domain_priors:suppression_score(nicene_creed_authority__liturgical_habituation_reading, 0.12).
domain_priors:theater_ratio(nicene_creed_authority__liturgical_habituation_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, accessibility_collapse, 0.22).
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, resistance, 0.06).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_creed_authority__liturgical_habituation_reading, rope).
narrative_ontology:human_readable(nicene_creed_authority__liturgical_habituation_reading, "Nicene Creed as Liturgical Identity Boundary Marker").
narrative_ontology:topic_domain(nicene_creed_authority__liturgical_habituation_reading, "systematic_theology/ecclesiology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_creed_authority__liturgical_habituation_reading, 'c615f243-035b-4170-8a48-2e2d7817a39c').
narrative_ontology:cs_kernel_codification('c615f243-035b-4170-8a48-2e2d7817a39c', fixed_text).
narrative_ontology:cs_authority_grounding('c615f243-035b-4170-8a48-2e2d7817a39c', practice).
narrative_ontology:cs_interpretation_layer_present('c615f243-035b-4170-8a48-2e2d7817a39c').
narrative_ontology:cs_reading_relation('c615f243-035b-4170-8a48-2e2d7817a39c', nicene_creed_authority__strict_orthodox_reading, influences).
narrative_ontology:cs_reading_relation('c615f243-035b-4170-8a48-2e2d7817a39c', nicene_creed_authority__symbolic_confessional_reading, influences).
narrative_ontology:cs_axiom('c615f243-035b-4170-8a48-2e2d7817a39c', foundational, liturgical_performance_suffices_for_identity).
narrative_ontology:cs_axiom_status(liturgical_performance_suffices_for_identity, holdable).
narrative_ontology:cs_axiom_grounding('c615f243-035b-4170-8a48-2e2d7817a39c', liturgical_performance_suffices_for_identity, conventional).
narrative_ontology:cs_axiom('c615f243-035b-4170-8a48-2e2d7817a39c', foundational, cognitive_assent_non_dispositive_for_belonging).
narrative_ontology:cs_axiom_status(cognitive_assent_non_dispositive_for_belonging, holdable).
narrative_ontology:cs_axiom_grounding('c615f243-035b-4170-8a48-2e2d7817a39c', cognitive_assent_non_dispositive_for_belonging, conventional).
narrative_ontology:cs_reference_frame('c615f243-035b-4170-8a48-2e2d7817a39c', liturgical_participatory_identity).
narrative_ontology:cs_drift_state('c615f243-035b-4170-8a48-2e2d7817a39c', contemporary_ecumenical_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('c615f243-035b-4170-8a48-2e2d7817a39c', '').
narrative_ontology:cs_kernel_id(nicene_creed_authority__liturgical_habituation_reading, nicene_creed_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_creed_authority__liturgical_habituation_reading, liturgical_participants).
narrative_ontology:constraint_vindicates(nicene_creed_authority__liturgical_habituation_reading, liturgical_identity_sufficiency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Recite the creed in communal worship; gain stable ecclesial identity and group belonging through repeated ritual performance without being subjected to doctrinal examination or required to affirm every metaphysical proposition cognitively.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, liturgical_participants, beneficiary,
    moderate, biographical, constrained, global).

% Lead the liturgical recitation, determine the ritual context, and maintain the communal practice across time. Their authority rests on ordination and tradition rather than on extracting resources or enforcing metaphysical conformity from participants.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, liturgical_presiders, agenda_setter,
    organized, generational, constrained, global).

% Uphold that the creed binds believers to specific metaphysical ontologies and that deviation warrants sanction. In a liturgical-habituation framework their assent requirement is backgrounded, so they are not the seated parties to this arrangement.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, orthodox_guardians, excluded,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates group identity and intergenerational boundary maintenance by providing a shared, repeatable verbal performance that marks membership without requiring individual theological interrogation or metaphysical unanimity.
% TRANSFER_FUNCTION: Moves status, belonging, and identity continuity from the liturgical assembly to participants through ritual participation; does not extract material resources or enforce propositional conformity.
% ABSENT_VOICES: Orthodox guardians who would require cognitive metaphysical assent and heresy policing as a condition of belonging; their seat is backgrounded in this reading though central to sibling readings of the same kernel.
% DISAPPEARANCE_RATIONALE: If the creed vanished as a habitual liturgical performance, these communities would lose a primary coordinate of group identity and would need to construct alternative boundary markers; the specific social-coordination function would dissolve and reorganize around other rituals or confessional mechanisms.
% FOUNDING_PROBLEM: How to maintain coherent Christian group identity across participants with diverse theological education, cultural backgrounds, and intellectual capacities without continuous fragmentation over metaphysical details.
% FOUNDING_PROBLEM_CORROBORATION: Liturgical historians and sociologists of religion attest that the creed functioned as a boundary marker before full doctrinal unanimity was achieved; mainstream denominational liturgists outside the strict orthodox seat corroborate that the practice persists for identity coordination rather than for heresy policing.
narrative_ontology:disappearance_verdict(nicene_creed_authority__liturgical_habituation_reading, world_rearranges).
narrative_ontology:founding_problem_status(nicene_creed_authority__liturgical_habituation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_creed_authority__liturgical_habituation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nicene_creed_authority__liturgical_habituation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nicene_creed_authority__liturgical_habituation_reading, 0.08, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_creed_authority__liturgical_habituation_reading_tests).
:- end_tests(nicene_creed_authority__liturgical_habituation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is very low (0.08) because the constraint does not take material resources or enforce propositional conformity; it offers inclusion. Suppression is low (0.12) because alternatives to this identity marker are not actively suppressedâother rituals or communities could in principle substitute, though habituation creates path dependence. Theater ratio is minimal (0.05) because the liturgical function is genuine and not performative maintenance of an atrophied role. Accessibility collapse is modest (0.22): once one is habituated, the creed feels natural, but alternatives are cognitively available. Resistance is negligible (0.06) because the coordinated parties generally accept the arrangement.
 *
 * PERSPECTIVAL GAP:
 *   From the participant seat, the creed is a low-friction route to belonging. From the presider seat, it is a tradition to be maintained for communal continuity. From the orthodox guardian seat (backgrounded here), the same practice is an insufficient substitute for doctrinal enforcement. The engine will compute different per-seat types because the structural relationship differs: participants and presiders experience coordination, while excluded enforcers experience the absence of the constraint they would prefer.
 *
 * DIRECTIONALITY LOGIC:
 *   Liturgical participants are beneficiaries (low directionality): the constraint subsidizes their belonging and reduces the cost of group membership by removing a theological entrance exam. Liturgical presiders are near-symmetric agenda setters: they invest in maintenance but receive non-monetary authority and continuity. Orthodox guardians are excluded from this reading's seat distribution; their high-directionality relationship to the constraint is not activated here because the liturgical reading does not enforce their assent demands.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâmaintaining identity across theological diversityâis still live, so the constraint does not read as piton. Because there is no active enforcement extracting from identifiable victims, it does not read as snare. The rope classification is supported by the live coordination function and the absence of asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_committer_nicene_creed,
    'This constraint instantiates the liturgical_habituation_reading of kernel nicene_creed_authority; sibling readings include strict_orthodox_reading and symbolic_confessional_reading. Does construing the creed as liturgical performance independent of assent structurally foreclose the strict orthodox demand for metaphysical sanction?',
    'Comparative historical analysis of communities that treat liturgical participation as sufficient versus those that enforce assent; sociological measurement of whether the same ritual substrate can sustain both enforcement and pluralist reinterpretation simultaneously.',
    'If the liturgical reading does not foreclose the orthodox reading, they coexist as parallel instantiations of the same kernel; if it structurally undermines orthodox enforcement by removing the social necessity of assent, the kernel generates downstream foreclosure pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_committer_nicene_creed, conceptual, 'Committer structure for nicene_creed_authority kernel readings.').

omega_variable(
    liturgical_vs_metaphysical_authority,
    'Does the creed''s authority in this reading derive solely from its liturgical performance function, or does metaphysical truth content continue to operate as a hidden enforcement mechanism?',
    'Ethnographic observation of liturgical communities to determine whether non-assenting participants are informally sanctioned despite the official habituation framework.',
    'If metaphysical assent is enforced informally, the effective extraction rises and the constraint drifts toward tangled_rope; if performance genuinely suffices, the low extraction metric holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liturgical_vs_metaphysical_authority, empirical, 'Whether the liturgical function is genuinely independent of metaphysical enforcement.').

omega_variable(
    sibling_reading_boundary,
    'Where exactly does the structural disagreement between this reading and the symbolic_confessional_reading locateâon the necessity of the creed itself, or on the source of its authority?',
    'Analyze whether symbolic_confessional communities could abandon the fixed creed while retaining the same social coordinate, or whether the fixed text is structurally necessary even for the symbolic reading.',
    'If the fixed text is structurally necessary for both, the disagreement is located in authority grounding; if the symbolic reading could dispense with the creed, the disagreement is located in the necessity of the kernel itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_boundary, conceptual, 'Location of structural disagreement with symbolic confessional sibling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_creed_authority__liturgical_habituation_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nice_tr_t0, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(nice_tr_t10, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 10, 0.05).
narrative_ontology:measurement(nice_tr_t20, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 20, 0.05).
narrative_ontology:measurement(nice_tr_t30, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 30, 0.05).
narrative_ontology:measurement(nice_tr_t40, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 40, 0.05).
narrative_ontology:measurement(nice_tr_t50, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 50, 0.05).

% Extraction over time
narrative_ontology:measurement(nice_be_t0, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(nice_be_t10, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 10, 0.08).
narrative_ontology:measurement(nice_be_t20, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 20, 0.08).
narrative_ontology:measurement(nice_be_t30, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 30, 0.08).
narrative_ontology:measurement(nice_be_t40, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 40, 0.08).
narrative_ontology:measurement(nice_be_t50, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 50, 0.08).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(nicene_creed_authority__liturgical_habituation_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(nicene_creed_authority__liturgical_habituation_reading, strict_orthodox_reading).
narrative_ontology:affects_constraint(nicene_creed_authority__liturgical_habituation_reading, symbolic_confessional_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the nicene_creed_authority kernel family, decomposed per the epsilon-invariance principle because the label 'Nicene Creed authority' conflates liturgical-habituation, strict-orthodox, and symbolic-confessional functions that have distinct epsilon values, stakeholder structures, and directionality profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
