% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_commitment__partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_commitment__partition_reading, []).

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
 *   constraint_id: shinbutsu_ontological_commitment__partition_reading
 *   human_readable: Shinto-Buddhist Domain Partition (Life-Cycle vs. Afterlife)
 *   domain: religious_studies/japanese_history/ontology_of_practice
 *
 * SUMMARY:
 *   This story instantiates the PARTITION reading of the shinbutsu-shugo
 *   (kami-buddha combination) kernel: the claim that Shinto and Buddhism in
 *   premodern and modern Japan occupy functionally separate ritual domains —
 *   Shinto for life-cycle and this-worldly purity concerns, Buddhism for
 *   death and the afterlife — without requiring or achieving ontological
 *   integration between the two cosmologies. This is distinct from the
 *   syncretic reading (which holds kami and buddhas were understood as
 *   aspects of one unified honji-suijaku cosmological order) and the
 *   incoherence reading (which holds no stable commitment existed at all and
 *   the arrangement was institutionally tolerated incoherence rather than a
 *   coherent partition). Under this reading, extraction is low and largely
 *   limited to the ordinary fee-for-service structure of ritual specialists;
 *   the coordination function (dividing ritual labor by life-stage domain) is
 *   genuine and does not require suppressing either institution's core
 *   claims.
 *
 * KEY AGENTS:
 *   - shrine_priests: agenda_setter/beneficiary (moderate/constrained) — administer life-cycle rites, no need to contest afterlife jurisdiction
 *   - temple_clergy: agenda_setter/beneficiary (moderate/constrained) — administer death/afterlife rites, no need to contest life-cycle jurisdiction
 *   - lay_households: beneficiary (moderate/constrained) — move freely between institutions across a lifetime without felt contradiction
 *   - doctrinal_systematizers: excluded (powerless/analytical) — the partition reading treats their integrative project as unnecessary
 *   - meiji_state_shinto_architects: excluded (institutional/trapped) — their coercive historical separation complicates the reading's claim to describe premodern practice
 *   - comparative_religion_scholars: observer (analytical/analytical) — adjudicate between the three sibling readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_commitment__partition_reading, 0.22).
domain_priors:suppression_score(shinbutsu_ontological_commitment__partition_reading, 0.15).
domain_priors:theater_ratio(shinbutsu_ontological_commitment__partition_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_commitment__partition_reading, rope).
narrative_ontology:human_readable(shinbutsu_ontological_commitment__partition_reading, "Shinto-Buddhist Domain Partition (Life-Cycle vs. Afterlife)").
narrative_ontology:topic_domain(shinbutsu_ontological_commitment__partition_reading, "religious_studies/japanese_history/ontology_of_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_commitment__partition_reading, '3e1b6967-3f0e-48d4-ace7-8d980f2e53e6').
narrative_ontology:cs_kernel_codification('3e1b6967-3f0e-48d4-ace7-8d980f2e53e6', distributed).
narrative_ontology:cs_authority_grounding('3e1b6967-3f0e-48d4-ace7-8d980f2e53e6', practice).
narrative_ontology:cs_interpretation_layer_present('3e1b6967-3f0e-48d4-ace7-8d980f2e53e6').
narrative_ontology:cs_reading_relation('3e1b6967-3f0e-48d4-ace7-8d980f2e53e6', shinbutsu_ontological_commitment__syncretic_reading, coexists_with).
narrative_ontology:cs_reading_relation('3e1b6967-3f0e-48d4-ace7-8d980f2e53e6', shinbutsu_ontological_commitment__incoherence_reading, coexists_with).
narrative_ontology:cs_axiom('3e1b6967-3f0e-48d4-ace7-8d980f2e53e6', foundational, domain_bounded_jurisdiction_suffices).
narrative_ontology:cs_axiom_status(domain_bounded_jurisdiction_suffices, holdable).
narrative_ontology:cs_axiom_grounding('3e1b6967-3f0e-48d4-ace7-8d980f2e53e6', domain_bounded_jurisdiction_suffices, conventional).
narrative_ontology:cs_axiom('3e1b6967-3f0e-48d4-ace7-8d980f2e53e6', foundational, ontological_integration_not_required_for_functional_coexistence).
narrative_ontology:cs_axiom_status(ontological_integration_not_required_for_functional_coexistence, holdable).
narrative_ontology:cs_axiom_grounding('3e1b6967-3f0e-48d4-ace7-8d980f2e53e6', ontological_integration_not_required_for_functional_coexistence, instrumental).
narrative_ontology:cs_reference_frame('3e1b6967-3f0e-48d4-ace7-8d980f2e53e6', premodern_jingu_ji_practical_division).
narrative_ontology:cs_drift_state('3e1b6967-3f0e-48d4-ace7-8d980f2e53e6', post_meiji_shinbutsu_bunri, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3e1b6967-3f0e-48d4-ace7-8d980f2e53e6', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_commitment__partition_reading, shinbutsu_ontological_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__partition_reading, shrine_priests).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__partition_reading, temple_clergy).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__partition_reading, lay_households).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_commitment__partition_reading, functional_domain_separation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer kami rites tied to birth, coming-of-age, marriage, harvest, and community protection. Under the partition reading they hold undisputed jurisdiction over life-affirming and purity rites, and do not need to argue for supremacy over death ritual because that domain is understood as belonging elsewhere. Their institutional position is secure without displacing Buddhist clergy.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, shrine_priests, agenda_setter,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_commitment__partition_reading, shrine_priests, beneficiary).

% Administer funerary rites, ancestor memorialization, and afterlife doctrine. Under the partition reading their authority over death and post-mortem fate is uncontested by shrine institutions, since kami practice is understood to concern this-worldly life rather than the afterlife. They collect fees and maintain grave registries without needing a unified cosmology to justify their role.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, temple_clergy, agenda_setter,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_commitment__partition_reading, temple_clergy, beneficiary).

% Move between shrine and temple as life stages require: shrine visits for birth and marriage, temple rites for death and memorial services. Under the partition reading they experience no felt contradiction because the two institutions are understood to answer different questions; they are free to patronize both without adjudicating whose cosmology is 'true.'
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, lay_households, beneficiary,
    moderate, generational, constrained, local).

% Historical and contemporary theologians who sought (in the honji-suijaku tradition) or seek (in modern comparative theology) a single integrated metaphysics unifying kami and buddhas. The partition reading treats their totalizing project as unnecessary and largely absent from the object-level practice it describes; their voice is structurally sidelined because the reading's whole point is that no integration was required or occurred at the practitioner level.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, doctrinal_systematizers, excluded,
    powerless, civilizational, analytical, national).

% 19th-century state actors who forcibly separated Shinto and Buddhist institutions (shinbutsu bunri) on the premise that the two were falsely conflated and needed disentangling. Their coercive separation is a different historical event from the partition reading's descriptive claim about premodern lived practice, but their project retroactively lends the partition reading an appearance of historical inevitability it may not deserve; the reading brackets this complication rather than confronting it.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, meiji_state_shinto_architects, excluded,
    institutional, generational, trapped, national).

% Study shinbutsu-shugo as a case in the general theory of religious syncretism. They adjudicate between the partition, syncretic, and incoherence readings using textual, institutional, and ethnographic evidence, and can shift which reading holds explanatory dominance in the field.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows two distinct religious institutions to divide ritual labor by life-stage domain — Shinto governing birth, growth, and this-worldly purity; Buddhism governing death, memorialization, and the afterlife — so that neither institution needs to resolve or even address the other's metaphysical claims to function.
% TRANSFER_FUNCTION: No net transfer of resources or authority between the two institutions is required by the partition itself; each retains its own fee structure, patronage base, and ritual monopoly within its domain. What moves is patronage flow across a person's lifetime: birth and marriage rites to shrines, funerary and memorial rites to temples.
% ABSENT_VOICES: Systematizing theologians (premodern honji-suijaku thinkers and their intellectual descendants) who regard domain-partition as an unsatisfying non-answer to the question of which cosmology is actually true are not represented in the partition reading's account, because the reading's claim is precisely that ordinary practitioners and most clergy did not need or seek that resolution.
% DISAPPEARANCE_RATIONALE: If the domain partition dissolved, the parties disagree on what would happen: shrine and temple institutions would likely contest jurisdiction over ambiguous rites (e.g., mizuko kuyo, protective amulets that straddle life and death), and either a new division would be renegotiated or one institution would attempt to absorb the other's functions. Whether this counts as 'the world rearranging' or merely a boundary renegotiation within an unchanged underlying practice is itself part of what separates this reading from the incoherence reading.
% FOUNDING_PROBLEM: Historically, kami cults and Buddhist institutions arrived in Japan under different cosmological premises and needed a workable modus vivendi that let both persist without either community abandoning its core practice or forcing doctrinal capitulation.
% FOUNDING_PROBLEM_CORROBORATION: Shrine and temple institutions themselves attest that the division of ritual labor remains functionally intact today (most Japanese households still practice this bifurcation). Independent religious-studies scholarship (outside either institution's own self-description) is divided: some ethnographic and institutional-history work corroborates a genuine functional partition with minimal doctrinal friction, while other scholars (see the incoherence and syncretic readings) argue the partition is a retrospective simplification imposed after Meiji-era forced separation, not a native premodern feature.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_commitment__partition_reading, contested).
narrative_ontology:founding_problem_status(shinbutsu_ontological_commitment__partition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_commitment__partition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(shinbutsu_ontological_commitment__partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_commitment__partition_reading, 0.22, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_commitment__partition_reading_tests).
:- end_tests(shinbutsu_ontological_commitment__partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored low (0.22 at interval end) because the partition reading describes a genuine division of ritual labor rather than an extractive hierarchy: neither institution captures rents from the other, and lay households are not coerced into patronizing one over the other beyond ordinary ritual fees. Suppression is low (0.15) because practitioner autonomy is explicitly preserved under this reading — households choose freely which rites to observe where. The slow upward drift in both metrics across the interval reflects the gradual professionalization and fee-formalization of both shrine and temple institutions over centuries, not an intensifying extractive mechanism. Accessibility collapse is moderate (0.35): the domain boundary is understood as customary rather than logically necessary, so alternative arrangements (full integration, full separation) remained conceptually available even as the partition became practically dominant.
 *
 * DIRECTIONALITY LOGIC:
 *   Both institutional seats (shrine priests, temple clergy) are coded as beneficiaries with moderate power and constrained exit — each depends on the continued social legitimacy of its half of the domain, and neither could unilaterally exit the arrangement without abandoning its institutional base. Lay households are also beneficiaries: the partition reading holds they experience no forced choice and lose nothing from the domain division. There are no victims under this reading — this is a structural marker of the reading itself: the partition reading's central claim is precisely that the arrangement produces no losers, in contrast to the incoherence reading, where institutional tolerance of unresolved contradiction may itself impose an interpretive cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (need for a working modus vivendi between rival cosmologies) is read as CONTESTED rather than cleanly live or dead: shrine and temple institutions maintain the partition still functions today, while some scholars read the current stability as retrospectively imposed by Meiji-era forced separation (shinbutsu bunri) rather than a continuous premodern feature. This prevents the reading from being naively certified as an eternally-solved coordination problem — the R5 corroboration surfaces exactly the ambiguity the sibling readings dispute.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    partition_vs_syncretic_evidentiary_basis,
    'Does the historical and textual record support a genuine functional partition (this reading) or does it better support an underlying unified cosmology (honji-suijaku, the syncretic reading) that merely manifests as domain-specialized practice?',
    'Close comparative reading of premodern ritual manuals, temple-shrine complex (jingu-ji) administrative records, and doctrinal treatises to determine whether practitioners and clergy articulated an explicit unifying metaphysics or operated with domain-bounded practical reasoning only.',
    'If the record shows widespread explicit honji-suijaku doctrinal commitment among clergy, the partition reading understates the degree of ontological integration and the syncretic reading would be the more structurally accurate account for that period.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_vs_syncretic_evidentiary_basis, empirical, 'Whether premodern practice was doctrinally unified (syncretic) or merely domain-divided (partition).').

omega_variable(
    meiji_retroactive_construction,
    'Is the clean life-cycle/afterlife partition an accurate description of premodern shinbutsu-shugo, or is it a retrospective simplification imposed by the Meiji-era forced separation (shinbutsu bunri) that reshaped how the prior arrangement is remembered and institutionally organized?',
    'Compare pre-Meiji jingu-ji (combined shrine-temple) records against post-Meiji institutional self-descriptions; look for evidence of ambiguous or contested rites (e.g., protective/afterlife-adjacent practices) that a clean partition model would not predict.',
    'If the partition is substantially a post-Meiji retrofit, this reading''s ε and claimed_type should be understood as describing modern institutional practice rather than a continuous premodern arrangement — a live threat to R5 founding-problem status being read as ''still live in original form.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_retroactive_construction, empirical, 'Whether the domain partition is a premodern feature or a Meiji-era institutional retrofit.').

omega_variable(
    partition_vs_incoherence_practitioner_experience,
    'Do ordinary practitioners experience the shrine/temple division as a coherent, principled domain boundary (this reading), or as an unreflective, potentially incoherent juxtaposition that simply was never forced to resolve (the incoherence reading)?',
    'Ethnographic and historical study of lay testimony, diaries, and popular religious literature for evidence of explicit reasoning about why kami and buddhas govern different domains, versus evidence of simple habitual practice without articulated rationale.',
    'If lay practitioners show no articulated rationale at all, the incoherence reading better captures the phenomenon and this reading overstates the coherence of the boundary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(partition_vs_incoherence_practitioner_experience, conceptual, 'Whether the life-cycle/afterlife boundary reflects principled partition or unreflective coexistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_commitment__partition_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(shin_tr_t200, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 200, 0.12).
narrative_ontology:measurement(shin_tr_t400, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 400, 0.14).
narrative_ontology:measurement(shin_tr_t700, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 700, 0.16).
narrative_ontology:measurement(shin_tr_t900, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 900, 0.18).
narrative_ontology:measurement(shin_tr_t1200, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 1200, 0.2).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(shin_be_t200, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 200, 0.12).
narrative_ontology:measurement(shin_be_t400, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 400, 0.15).
narrative_ontology:measurement(shin_be_t700, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 700, 0.18).
narrative_ontology:measurement(shin_be_t900, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 900, 0.2).
narrative_ontology:measurement(shin_be_t1200, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 1200, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(shinbutsu_ontological_commitment__partition_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_commitment__partition_reading, identity_coordination).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__partition_reading, shinbutsu_ontological_commitment__syncretic_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__partition_reading, shinbutsu_ontological_commitment__incoherence_reading).

% DUAL FORMULATION NOTE:
% Three constraints share the shinbutsu_ontological_commitment kernel: this partition_reading (functional domain division, low doctrinal integration, no single beneficiary, low extraction), syncretic_reading (unified honji-suijaku cosmology, higher doctrinal integration), and incoherence_reading (no stable ontological commitment, institutionally tolerated incoherence). Each reading authors its own epsilon over the same underlying historical arrangement — assessed by that reading's own lights — rather than averaging across readings. The partition reading is the structural middle position: less integrative than syncretic, more principled than incoherence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
