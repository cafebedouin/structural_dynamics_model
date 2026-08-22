% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__hybrid_preparatory
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__hybrid_preparatory, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: temple_sacrifice_commitment__hybrid_preparatory
 *   human_readable: Temple Sacrifice Study as Suspended Preparatory Commitment
 *   domain: religious_law/halakhic_tradition/commitment_system_theory
 *
 * SUMMARY:
 *   This story instantiates the 'hybrid_preparatory' reading of the
 *   temple_sacrifice_commitment kernel: study of sacrificial law is neither
 *   the commandment's living performance (study_as_exercise) nor mere
 *   archival preservation of a defunct practice (performance_only), but a
 *   deliberately suspended, preparatory occupation of the commitment held
 *   open pending messianic restoration. Under this reading, the institutional
 *   apparatus that sustains Kodashim study extracts real present resources —
 *   donor funds, student years, household labor-market opportunity — against
 *   a benefit (readiness for an uncertain future restoration) that cannot be
 *   verified or dated. The coordination function (preserving continuity and
 *   expertise against the possibility of restoration) is genuine but is
 *   bundled with asymmetric extraction from payers who have little
 *   institutional voice over how much study-time and funding this specific
 *   specialty commands relative to applied halakhic fields.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__hybrid_preparatory, 0.42).
domain_priors:suppression_score(temple_sacrifice_commitment__hybrid_preparatory, 0.38).
domain_priors:theater_ratio(temple_sacrifice_commitment__hybrid_preparatory, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, extractiveness, 0.42).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__hybrid_preparatory, tangled_rope).
narrative_ontology:human_readable(temple_sacrifice_commitment__hybrid_preparatory, "Temple Sacrifice Study as Suspended Preparatory Commitment").
narrative_ontology:topic_domain(temple_sacrifice_commitment__hybrid_preparatory, "religious_law/halakhic_tradition/commitment_system_theory").

domain_priors:requires_active_enforcement(temple_sacrifice_commitment__hybrid_preparatory).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__hybrid_preparatory, '72c4312f-d8a1-4c1a-b497-069c1284c944').
narrative_ontology:cs_kernel_codification('72c4312f-d8a1-4c1a-b497-069c1284c944', fixed_text).
narrative_ontology:cs_authority_grounding('72c4312f-d8a1-4c1a-b497-069c1284c944', lineage).
narrative_ontology:cs_interpretation_layer_present('72c4312f-d8a1-4c1a-b497-069c1284c944').
narrative_ontology:cs_reading_relation('72c4312f-d8a1-4c1a-b497-069c1284c944', temple_sacrifice_commitment__study_as_exercise, coexists_with).
narrative_ontology:cs_reading_relation('72c4312f-d8a1-4c1a-b497-069c1284c944', temple_sacrifice_commitment__performance_only, influences).
narrative_ontology:cs_reading_relation('72c4312f-d8a1-4c1a-b497-069c1284c944', temple_sacrifice_commitment__symbolic_transformation, coexists_with).
narrative_ontology:cs_axiom('72c4312f-d8a1-4c1a-b497-069c1284c944', foundational, suspension_is_active_occupation_not_vacancy).
narrative_ontology:cs_axiom_status(suspension_is_active_occupation_not_vacancy, holdable).
narrative_ontology:cs_axiom_grounding('72c4312f-d8a1-4c1a-b497-069c1284c944', suspension_is_active_occupation_not_vacancy, conventional).
narrative_ontology:cs_axiom('72c4312f-d8a1-4c1a-b497-069c1284c944', foundational, restoration_certainty_justifies_indefinite_preparatory_cost).
narrative_ontology:cs_axiom_status(restoration_certainty_justifies_indefinite_preparatory_cost, holdable).
narrative_ontology:cs_axiom_grounding('72c4312f-d8a1-4c1a-b497-069c1284c944', restoration_certainty_justifies_indefinite_preparatory_cost, theological).
narrative_ontology:cs_reference_frame('72c4312f-d8a1-4c1a-b497-069c1284c944', second_temple_sacrificial_praxis).
narrative_ontology:cs_drift_state('72c4312f-d8a1-4c1a-b497-069c1284c944', post_destruction_rabbinic_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('72c4312f-d8a1-4c1a-b497-069c1284c944', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__hybrid_preparatory, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__hybrid_preparatory, yeshiva_institutions).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__hybrid_preparatory, kodashim_scholars).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__hybrid_preparatory, messianic_restoration_movements).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__hybrid_preparatory, community_donors_funding_kodashim_study).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__hybrid_preparatory, students_diverted_from_applied_halakha).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__hybrid_preparatory, practical_livelihood_track_families).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__hybrid_preparatory, divine_command_permanence_doctrine).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__hybrid_preparatory, eventual_restoration_certainty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set curricula that allocate substantial study time to Seder Kodashim (sacrificial law), administer the institutional framework that credentials this study as equal in prestige to practicable law, and draw donor and tuition revenue partly on the premise that this study holds the practice in trust for restoration. They can redirect curricular emphasis but rarely do, since the arrangement anchors institutional identity and funding streams.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, yeshiva_institutions, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_commitment__hybrid_preparatory, yeshiva_institutions, beneficiary).

% Build scholarly reputations, publish commentary, and occupy teaching posts specifically around sacrificial law study. Their professional identity and communal standing are constituted by mastery of a legal corpus with no current applied venue; leaving the specialty would mean abandoning years of accumulated distinction with no equivalent transfer to applied halakhic fields.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, kodashim_scholars, beneficiary,
    moderate, biographical, identity_locked, national).

% Contribute charitable funds and tuition on the understanding that supporting Torah study broadly, including Kodashim, sustains the covenant and hastens redemption. They rarely itemize where funds go by tractate; redirecting support toward applied-law study or social services would require overriding institutional fundraising narratives that treat all study as equally meritorious.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, community_donors_funding_kodashim_study, payer,
    moderate, generational, constrained, national).

% Spend formative study years mastering laws of animal sacrifice, Temple architecture, and priestly purity with no applicable venue, at the expense of time that could build competence in areas with present-day application (family law, financial law, communal governance). Their exit is constrained by curricular sequencing that treats Kodashim mastery as a prerequisite marker of serious scholarship.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, students_diverted_from_applied_halakha, payer,
    powerless, biographical, constrained, regional).

% Depend on household earners whose years in intensive study (including substantial Kodashim components) delay entry into the labor market; the household bears the deferred-income cost of an institutional structure whose stated payoff (readiness for restoration) has no verifiable date and no observable milestone against which the delay can be judged worthwhile.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, practical_livelihood_track_families, payer,
    powerless, biographical, trapped, local).

% Draw legitimacy and continuity from a living body of scholars maintaining sacrificial law in readiness, using the existence of active Kodashim study as evidence that the tradition treats restoration as imminent and structurally provided-for rather than abandoned. Their institutional purpose depends on the suspended-commitment reading remaining plausible.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, messianic_restoration_movements, beneficiary,
    organized, civilizational, identity_locked, global).

% Hold that sacrificial law has undergone authorized transformation into prayer, and are largely absent from the institutional conversation that funds and credentials Kodashim study as suspended-but-live; their competing account of what happened to the commitment is not represented in the yeshiva curricular decision or the donor solicitation narrative.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, reform_and_conservative_halakhic_authorities, excluded,
    organized, generational, mobile, national).

% Study how the suspended-commitment framing emerged after the Temple's destruction and how it has been sustained institutionally across two millennia; document the resource flows and identity investments involved without themselves collecting from either side.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, halakhic_historians, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains scholarly and communal continuity with sacrificial law across an indefinite hiatus, so that if conditions for restoration ever obtain, a trained body of practitioners and a preserved legal corpus already exist rather than needing reconstruction from scratch.
% TRANSFER_FUNCTION: Moves donor charity, tuition, and years of student and household labor-market opportunity into the maintenance of a scholarly specialty with no current applied output, in exchange for institutional prestige, scholarly identity, and a claim of readiness for restoration.
% ABSENT_VOICES: Reform and Conservative authorities who hold the commitment already transformed into prayer, and secular-labor-oriented community members who would prefer curricular time redirected toward applied skills, are structurally outside the yeshiva funding and curricular conversation that sustains this specific reading.
% DISAPPEARANCE_RATIONALE: Beneficiary institutions and scholars would say the world rearranges catastrophically — the tradition's continuity claim and the scholarly specialty vanish. Payer households and historians more skeptical of the suspended-preparatory framing would say the practical world (labor allocation, donor spend, applied halakhic competence) barely changes, since the study currently produces no material output anyway; only the symbolic and institutional-identity layer would be disrupted.
% FOUNDING_PROBLEM: After the Temple's destruction, the tradition faced the problem of how a divine commandment could remain binding and coherent when the physical conditions for its performance had vanished — abandoning the commandment risked conceding divine law was contingent on circumstance; treating it as simply performable risked absurdity.
% FOUNDING_PROBLEM_CORROBORATION: Kodashim scholars and yeshiva leadership attest the founding problem (preserving the commandment's bindingness pending restoration) remains fully live. Halakhic historians, outside the institutions that fund and credential this study, document that the suspended-preparatory framing itself is a post-destruction interpretive innovation rather than a directly transmitted mandate, and note that alternative traditions (Reform, Reconstructionist, and some Conservative authorities) resolved the same founding problem via symbolic transformation rather than suspension — indicating the 'still live, still preparatory' status is a contested reading, not a settled fact even among halakhically observant communities.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__hybrid_preparatory, contested).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__hybrid_preparatory, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__hybrid_preparatory, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(temple_sacrifice_commitment__hybrid_preparatory, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__hybrid_preparatory, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__hybrid_preparatory_tests).
:- end_tests(temple_sacrifice_commitment__hybrid_preparatory_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.42 at interval end) rather than high: the study does produce genuine scholarly and communal-identity goods even absent restoration, so it is not pure extraction, but the cognitive and financial resources it consumes have no verifiable return path, which the performance_only sibling reading would treat as archival waste and the study_as_exercise sibling would treat as zero-cost because the study itself IS the fulfillment. Suppression is moderate (0.38): no one is coerced into Kodashim specialization, but curricular sequencing and communal prestige structures create strong soft pressure. Theater ratio rises gently over the interval (0.20 to 0.30) as institutional emphasis on 'readiness for restoration' as a fundraising and identity narrative has grown relative to the practical scholarly output of Kodashim study.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (yeshiva institutions), the arrangement reads as rope-like coordination preserving an eternal commandment in trust. From the payer seats (donor households, diverted students, livelihood-track families), the same structure reads as extraction — real years and money committed to a specialty with no applied output and no accountability mechanism for whether the allocation is proportionate. This divergence is exactly what tangled_rope is built to register: both a genuine coordination story and asymmetric extraction riding the same structure, with active institutional enforcement (curricular sequencing, prestige hierarchies) required to sustain the allocation.
 *
 * DIRECTIONALITY LOGIC:
 *   Yeshiva institutions and Kodashim scholars sit near the beneficiary end: they administer and collect prestige/funding from the arrangement (low d). Community donors and diverted students sit nearer the target end: their resources and years fund a study whose payoff is unverifiable and whose allocation they do not control (high d). Practical-livelihood-track families are the most target-like: their household income is directly delayed by the institutional time allocation, with no exit that does not mean abandoning communal standing (trapped exit_options).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (preserving a divine commandment's bindingness across a hiatus in performability) may or may not still be live depending on eschatological premises the story cannot adjudicate; what the classification captures is that regardless of whether restoration ever occurs, the institutional structure that has grown up to sustain suspended study now has its own beneficiaries (scholars, institutions, restoration movements) whose interests in continuing the arrangement are independent of whether it is still serving its original preparatory function well, or serving primarily as an identity-preservation and fundraising narrative. The tangled_rope classification, rather than mountain or rope, flags that this divergence between founding purpose and current beneficiary structure deserves ongoing scrutiny rather than being resolved by appeal to tradition alone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suspension_versus_alternative_readings_of_kernel,
    'Is the ''suspended preparatory'' framing itself a load-bearing theological claim, or is it better read as one interpretive strategy among several (study_as_exercise, performance_only, symbolic_transformation) that different communities have adopted for institutional and identity reasons independent of the underlying legal question?',
    'Comparative analysis of how each reading''s institutional adherents allocate resources: if resource allocation patterns track theological conviction rather than institutional self-interest, the suspended reading is more likely a genuine theological position; if allocation patterns track institutional survival needs regardless of stated theology, the framing is more likely serving as post-hoc justification for existing scholarly and fundraising structures.',
    'If the suspended-preparatory reading is primarily institutional self-justification, the coordination function claimed here is substantially cover for extraction, pushing the classification toward snare. If it is a genuine, independently-arrived-at theological conviction that happens to also sustain institutions, tangled_rope with real coordination function is the more accurate reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suspension_versus_alternative_readings_of_kernel, conceptual, 'Whether the hybrid_preparatory framing is theologically load-bearing or institutionally self-serving.').

omega_variable(
    restoration_timeline_indeterminacy,
    'Because messianic restoration has no predictable date, is there any observable evidence that could ever demonstrate this study is disproportionate to its preparatory purpose, or is the framing structurally unfalsifiable?',
    'Examine whether the tradition has any internal mechanism for periodically reassessing curricular proportion (e.g., historical councils that adjusted study emphasis), versus whether the current allocation has simply persisted unexamined since its establishment.',
    'If no reassessment mechanism exists and none has ever operated, the extraction measured here is closer to permanent and structurally unaccountable, supporting a higher extractiveness score and possibly a snare rather than tangled_rope reading. If reassessment mechanisms exist and have operated historically, the coordination function is more genuinely self-correcting.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restoration_timeline_indeterminacy, empirical, 'Whether the suspended-commitment arrangement has any internal accountability mechanism given the unfalsifiable timeline.').

omega_variable(
    donor_awareness_of_allocation,
    'Do community donors funding yeshiva institutions have meaningful awareness of and control over how much of their contribution specifically supports Kodashim study versus applied halakhic fields?',
    'Survey donor understanding of institutional budget allocation by tractate/subject area; compare stated donor intent against actual curricular time allocation.',
    'Low donor awareness would support treating community_donors_funding_kodashim_study as a genuine victim group bearing costs they did not knowingly consent to at this granularity; high awareness and informed consent would weaken the victim characterization and shift the story toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(donor_awareness_of_allocation, empirical, 'Whether donors knowingly and specifically fund Kodashim study or fund it as an unexamined default within general Torah-study giving.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__hybrid_preparatory, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 0, 0.2).
narrative_ontology:measurement(temp_tr_t20, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 20, 0.22).
narrative_ontology:measurement(temp_tr_t40, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 40, 0.24).
narrative_ontology:measurement(temp_tr_t60, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 60, 0.26).
narrative_ontology:measurement(temp_tr_t80, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 80, 0.28).
narrative_ontology:measurement(temp_tr_t100, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 100, 0.3).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(temp_be_t20, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 20, 0.33).
narrative_ontology:measurement(temp_be_t40, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 40, 0.36).
narrative_ontology:measurement(temp_be_t60, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 60, 0.38).
narrative_ontology:measurement(temp_be_t80, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 80, 0.4).
narrative_ontology:measurement(temp_be_t100, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 100, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(temple_sacrifice_commitment__hybrid_preparatory, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__hybrid_preparatory, identity_coordination).
narrative_ontology:boltzmann_floor_override(temple_sacrifice_commitment__hybrid_preparatory, 0.1).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__hybrid_preparatory, temple_sacrifice_commitment__study_as_exercise).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__hybrid_preparatory, temple_sacrifice_commitment__performance_only).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__hybrid_preparatory, temple_sacrifice_commitment__symbolic_transformation).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the temple_sacrifice_commitment kernel, each authored as a separate constraint with its own epsilon per the ε-invariance principle. hybrid_preparatory (this story) authors moderate extraction from a coordination-plus-extraction structure (tangled_rope); study_as_exercise would author near-zero extraction (the study fully occupies the commandment costlessly — mountain-like); performance_only would author low extraction with high theater_ratio (archival preservation with little ongoing cost or claim — piton-like); symbolic_transformation would author near-zero extraction under a claim that the commitment has already been fulfilled through transformation, making further 'preparatory' expenditure moot by that reading's own lights. The four readings are linked bidirectionally in principle; this file declares the outbound edges to the three siblings it was generated alongside.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
