% ============================================================================
% CONSTRAINT STORY: kodashim_obligation__study_as_archive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_obligation__study_as_archive, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: kodashim_obligation__study_as_archive
 *   human_readable: Kodashim Study Obligation: Historical Archive Reading
 *   domain: religious/textual
 *
 * SUMMARY:
 *   In Jewish law, Kodashim (the Mishnaic tractates of Temple sacrificial
 *   law) documents a system whose material performance has been impossible
 *   for nearly 2000 years. This constraint story models ONE READING of the
 *   kodashim_obligation kernel: the reading that treats Kodashim study as
 *   historical preservation and communal identity-maintenance rather than as
 *   performance of cosmic function or preparation for future restoration.
 *   Under this reading, the obligation to study Kodashim extracts
 *   intellectual resources from applicable law toward a defunct system,
 *   sustained by institutional inertia and identity-vindicating framing
 *   rather than by functional necessity. The constraint is classified as a
 *   Piton: a former coordination mechanism (rabbinic response to the Temple's
 *   destruction) whose primary function has atrophied but whose
 *   administrative structure persists through theological reframing and
 *   communal practice.
 *
 * KEY AGENTS:
 *   - kodashim_study_practitioners: Maintain the daily obligation through institutional infrastructure and theological framing.
 *   - jewish_communal_identity: Non-agent analytical beneficiary—the constraint vindicates cultural continuity.
 *   - halakhic_innovators: Scholars diverting intellectual effort from applicable law toward a non-performable system.
 *   - foundational_rabbinic_authority: Institutional agenda-setter whose authority structure both enforces and benefits from the obligation.
 *   - messianic_restoration_advocates: Competing reading's constituency—hold that study prepares for future Temple restoration.
 *   - alternative_jewish_law_movements: Excluded parties who would argue for obligation relaxation or reframing.
 *   - comparative_textual_scholars: Analytical observers treating Kodashim as historical archive without obligation framing.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_obligation__study_as_archive, 0.58).
domain_priors:suppression_score(kodashim_obligation__study_as_archive, 0.42).
domain_priors:theater_ratio(kodashim_obligation__study_as_archive, 0.71).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, extractiveness, 0.58).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, theater_ratio, 0.71).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_obligation__study_as_archive, piton).
narrative_ontology:human_readable(kodashim_obligation__study_as_archive, "Kodashim Study Obligation: Historical Archive Reading").
narrative_ontology:topic_domain(kodashim_obligation__study_as_archive, "religious/textual").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_obligation__study_as_archive, 'd4d6732a-5462-44b8-98a2-7dc83a6fd72a').
narrative_ontology:cs_kernel_codification('d4d6732a-5462-44b8-98a2-7dc83a6fd72a', distributed).
narrative_ontology:cs_authority_grounding('d4d6732a-5462-44b8-98a2-7dc83a6fd72a', lineage).
narrative_ontology:cs_interpretation_layer_present('d4d6732a-5462-44b8-98a2-7dc83a6fd72a').
narrative_ontology:cs_reading_relation('d4d6732a-5462-44b8-98a2-7dc83a6fd72a', kodashim_obligation__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('d4d6732a-5462-44b8-98a2-7dc83a6fd72a', kodashim_obligation__study_as_preparation, coexists_with).
narrative_ontology:cs_axiom('d4d6732a-5462-44b8-98a2-7dc83a6fd72a', foundational, temple_restoration_indefinitely_deferred).
narrative_ontology:cs_axiom_status(temple_restoration_indefinitely_deferred, holdable).
narrative_ontology:cs_axiom_grounding('d4d6732a-5462-44b8-98a2-7dc83a6fd72a', temple_restoration_indefinitely_deferred, conventional).
narrative_ontology:cs_axiom('d4d6732a-5462-44b8-98a2-7dc83a6fd72a', foundational, textual_continuity_substitutes_for_performable_function).
narrative_ontology:cs_axiom_status(textual_continuity_substitutes_for_performable_function, holdable).
narrative_ontology:cs_axiom_grounding('d4d6732a-5462-44b8-98a2-7dc83a6fd72a', textual_continuity_substitutes_for_performable_function, deontological).
narrative_ontology:cs_reference_frame('d4d6732a-5462-44b8-98a2-7dc83a6fd72a', rabbinic_continuity_post_temple).
narrative_ontology:cs_drift_state('d4d6732a-5462-44b8-98a2-7dc83a6fd72a', contemporary_jewish_legal_pluralism, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d4d6732a-5462-44b8-98a2-7dc83a6fd72a', '').
narrative_ontology:cs_kernel_id(kodashim_obligation__study_as_archive, kodashim_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_archive, jewish_communal_identity).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_archive, intellectual_resources_diverted_from_applicable_law).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_archive, foundational_rabbinic_authority).
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_archive, messianic_restoration_advocates).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_archive, halakhic_innovators).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_archive, textual_continuity_as_identity_anchor).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the daily obligation to study sacrificial law despite the Temple's absence for nearly 2000 years. They frame the practice as preserving historical knowledge and communal identity. The study infrastructure—dedicated texts, commentary traditions, study schedules—is maintained through institutional inertia and theological framing, not because the outcome is applied to any functioning legal or ritual system.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, kodashim_study_practitioners, agenda_setter,
    organized, generational, identity_locked, global).

% The constraint vindicates communal continuity: practicing study of Temple law despite its material impossibility becomes a ritual anchor for Jewish identity across diaspora and time. The benefit is purely symbolic—the constraint certifies cultural persistence rather than enabling legal or functional outcomes.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, jewish_communal_identity, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(kodashim_obligation__study_as_archive, jewish_communal_identity).

% Invest scholarly resources in understanding and extending Kodashim law despite knowing its legal incompleteness and non-performability. These scholars could redirect their effort to applicable rabbinic law (monetary law, family law, purity law, festival law) but the obligation framework channels significant intellectual effort toward a defunct system. Their exit is constrained by peer expectation and institutional curriculum requirements.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, halakhic_innovators, payer,
    moderate, biographical, constrained, regional).

% The Talmud and earlier sources established Kodashim study as obligatory despite the Temple's destruction. This institutional authority both enforces the obligation and benefits from it: treating Kodashim as permanently binding vindicates the textual corpus's completeness and the rabbinic tradition's continuity with Temple-era law. Changing the obligation would implicitly revise the authority structure itself.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, foundational_rabbinic_authority, agenda_setter,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(kodashim_obligation__study_as_archive, foundational_rabbinic_authority, beneficiary).

% Frame Kodashim study as preparation for actual Temple restoration, which they believe will occur. For them, the study is not archival but functional—preserving technical knowledge for a future performable system. This reading coexists with but competes for legitimacy against the archival reading.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, messianic_restoration_advocates, beneficiary,
    moderate, civilizational, identity_locked, regional).

% Reform, Conservative, and Reconstructionist movements question whether Kodashim obligation remains binding after Temple destruction. They would argue for reallocating study effort to applicable law and for revising the obligation framework itself. Their positions are excluded from the Orthodox institutional setting where Kodashim study obligation is most strongly enforced.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, alternative_jewish_law_movements, excluded,
    powerful, biographical, mobile, regional).

% Document and analyze the Kodashim corpus as historical evidence of Temple-era law and Jewish thought. They treat the texts as archival without claiming ongoing obligation, examining them for their intrinsic historical value rather than their role in modern Jewish practice.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, comparative_textual_scholars, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains textual knowledge and commentary traditions surrounding Temple sacrificial law: a coordinated scholarly infrastructure that preserves, interprets, and transmits a complex legal corpus across centuries and diaspora communities despite the system's legal non-functionality.
% TRANSFER_FUNCTION: Moves scholarly attention and institutional curriculum slots away from applicable rabbinic law toward a system that cannot be legally performed or applied. The transfer is from intellectual resources (scholar-hours, pedagogical emphasis) toward communal identity vindication (textual continuity as cultural anchor).
% ABSENT_VOICES: Non-Orthodox Jewish movements and secular Jewish scholars who question the obligation's continued binding force are excluded from the institutional enforcement of Kodashim study within Orthodox frameworks. They would argue for relaxing or reframing the obligation; their positions are structurally absent from the decision-making authority that maintains the obligation.
% DISAPPEARANCE_RATIONALE: If the obligation to study Kodashim disappeared, Orthodox Jewish institutions would reallocate significant study time and pedagogical resources toward applicable law, Jewish philosophy, and contemporary ethical questions. Communal practice would lose one of its major identity-reinforcement mechanisms, and the textual transmission infrastructure around Kodashim would degrade. The intellectual order of Jewish scholarship would substantially reorganize.
% FOUNDING_PROBLEM: After the Temple's destruction (70 CE), rabbinic Judaism faced an existential continuity crisis: all sacrificial law became materially impossible to perform. The rabbinic response established the obligation to STUDY sacrificial law as a substitute for performance, grounding post-Temple Judaism in textual continuity with Temple-era law and preventing wholesale loss of sacrificial knowledge.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic sources unanimously attest that the founding problem (continuity after Temple destruction) was live and that Kodashim study was established as a response. Secular historical scholarship and comparative-law analysis confirm the Temple's destruction and the rabbinic innovation. However, Orthodox and non-Orthodox movements disagree on whether the founding problem remains live: Orthodox practitioners argue continuity and identity are perpetual concerns; non-Orthodox movements argue the founding problem is historically resolved and the obligation no longer serves its original function. The corroboration for the constraint's persistence comes primarily from within the benefiting institutional parties themselves.
narrative_ontology:disappearance_verdict(kodashim_obligation__study_as_archive, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_obligation__study_as_archive, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_obligation__study_as_archive, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(kodashim_obligation__study_as_archive, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_obligation__study_as_archive_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kodashim_obligation__study_as_archive, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kodashim_obligation__study_as_archive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58) because the constraint diverts real resources (study time, pedagogical emphasis) from applicable law toward a defunct system, but the diversion is not violent or coercive—it is sustained by identity-fusion and institutional legitimacy claims. Theater ratio is high (0.71), the defining characteristic of a Piton: the bulk of Kodashim activity is now performative maintenance of a practice whose functional output has disappeared. Suppression is moderate (0.42) because the obligation is enforced primarily through institutional expectations, peer commitment, and identity-lockedness rather than through coercion; alternatives (relaxing the obligation, reframing it) exist but are institutionally suppressed. Accessibility collapse (0.48) and resistance (0.55) both reflect the constraint's contentious status: alternatives are not fully collapsed (non-Orthodox movements exist), and real intellectual resistance to the obligation's continued binding force exists within the scholarly community, but institutional suppression keeps these alternatives from cascading into widespread practice change. The measurement series shows slow extraction-creep over the interval (0.48→0.58) as the constraint becomes increasingly theatrical relative to its functional output—this pattern is diagnostic for a Piton under degradation pressure.
 *
 * PERSPECTIVAL GAP:
 *   From the kodashim_study_practitioners seat (identity-locked, organized), the constraint appears as meaningful cultural continuity and obligation fulfillment—a Rope coordinating a practice of deep communal value. From the halakhic_innovators seat (moderate power, constrained exit), the same constraint appears as Piton: diversion of effort toward a defunct system justified by identity claims. From the foundational_rabbinic_authority seat (institutional), the constraint appears as both governance and vindication of textual authority—its enforcement protects the authority structure itself. These seat divergences are not data errors; they are the measurement the framework exists to take. The engine computes a per-seat type from the structural data; the authored claim (Piton) represents the reading's theoretical position, independent of metric value.
 *
 * DIRECTIONALITY LOGIC:
 *   Kodashim_study_practitioners have low directionality (d~0.25, moderate power, mobile escape to non-Orthodox practice, but identity-locked within Orthodox frameworks). Halakhic_innovators have moderate-high directionality (d~0.65, moderate power, constrained exit due to curriculum requirements and peer expectation, divert resources without capturing gains). Jewish_communal_identity has low directionality as an agent=false analytical beneficiary (d~0.0, analytical power, no exit—benefits from the constraint's identity-vindicating function without bearing costs). Foundational_rabbinic_authority has low directionality (d~0.2, institutional power, captures legitimacy benefit from the constraint's existence, maintains it voluntarily). Messianic_restoration_advocates have low directionality (d~0.15, moderate power, mobile between readings, benefit from the obligation through their preferred interpretation). Alternative_jewish_law_movements have high directionality (d~0.8, powerful institutional actors, trapped by the constraint's enforcement within Orthodox institutional settings where they are excluded). No directionality overrides are necessary; the derivation chain from beneficiary/victim + exit options produces the correct d profile for each seat.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits clear mandatrophy: the founding problem (continuity after Temple destruction in 70 CE) is historically resolved—Jewish communities have maintained robust textual and cultural traditions for 2000 years without requiring the specific obligation to study Kodashim. Yet the obligation persists, not because it solves the founding problem but because it vindicates the rabbinic authority structure that established it and because it serves identity functions. The classification as Piton (degraded, inertial, mostly performative) captures this dynamic: the mandate (study Kodashim as necessary for cultural continuity and obligation fulfillment) has outlived its functional grounding (Temple restoration is structurally unwanted and theologically deferred indefinitely), but the administrative structure persists through reframing. The theater_ratio's trajectory (0.62→0.71) models this degradation: as the constraint's functional output has become purely symbolic, the ratio of performative maintenance to genuine function has risen.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    functional_vs_identity_boundary,
    'Is the ''study obligation'' structured as a functional obligation (Temple restoration remains theologically possible and morally incumbent) or an identity-maintenance practice (Temple restoration is deferred indefinitely and the obligation serves primarily communal continuity)?',
    'Textual analysis of contemporary responsa and rabbinic teaching: do authorities present Temple restoration as a practical obligation or a deferred theological claim? Analysis of curriculum allocation: is Kodashim study given weight proportional to applied-law subjects, and how is that weighting justified? Survey of practitioners: do they frame the obligation as functional or symbolic?',
    'If the obligation is structured as functional (preparation for restoration), reclassify toward Rope or Tangled Rope (genuine coordination with a deferred but expected outcome); if structured as identity-maintenance, the Piton classification holds. A mixed structure (some practitioners, some authorities holding each framing) would support Tangled Rope (coordination for some, extraction for others).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_vs_identity_boundary, conceptual, 'Whether the obligation frames Temple restoration as practically possible/obligatory or defers it indefinitely as identity-anchor.').

omega_variable(
    reading_identity_fusion_mechanism,
    'Among practitioners of Kodashim study under the archive reading, how is identity fusion to the obligation achieved—is it professional identity (scholar''s role), relational identity (community membership), ideological identity (worldview fusing Jewish identity with textual continuity), or institutional identity (the practice has become constitutive of the institution)?',
    'Ethnographic analysis of practitioners'' language and framing; historical analysis of how Kodashim study became institutionalized in yeshiva curricula; survey of exit barriers—why do practitioners who intellectually recognize the system''s non-functionality continue the practice?',
    'Different identity-fusion mechanisms produce different exit-cost profiles. Ideological identity fusion (worldview-level) produces highest exit cost (identity_locked); institutional identity fusion produces moderate exit cost (constrained). If identity fusion is primarily professional (scholar identity), exit is more mobile. The measured suppression (0.42) would increase if identity fusion is primarily ideological.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_identity_fusion_mechanism, empirical, 'What mechanism binds practitioners to the archive-reading obligation despite its non-functionality?').

omega_variable(
    sibling_reading_foreclosure,
    'Do the three readings of the kodashim_obligation kernel logically foreclose each other, or can they coexist as genuinely live positions held by different constituencies?',
    'Textual analysis of whether each reading requires denying the others'' core premises within a single framework. Analysis of historical transmission: have all three readings had sustained institutional expression, or has one dominated and marginalized the others?',
    'If any reading structurally forecloses another (the core premises contradict within one framework), the reading_relations should reflect forecloses rather than coexists_with. If all three remain live, the cs_structure.reading_relations should be coexists_with and the constraint family should remain three separate stories. A hybrid (one reading foreclosing another, but both coexisting with a third) would require asymmetric reading_relations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'The logical and institutional relationship between the three readings of the binding character of Kodashim post-Temple.').

omega_variable(
    non_orthodox_institutional_capture,
    'Are non-Orthodox movements'' exclusion from the Kodashim obligation framing a side effect of institutional separation, or an active suppression by Orthodox authority structures defending their reading?',
    'Historical analysis of non-Orthodox development: when and why did Reform, Conservative, and Reconstructionist movements relax or reframe Kodashim obligation? Analysis of Orthodox institutional responses: have they actively excluded non-Orthodox scholarship from canonical Kodashim discussion, or has separation occurred naturally through divergent institutional development?',
    'If active suppression, the suppression metric (0.42) underestimates institutional coercion and should rise toward 0.55+. If institutional separation is largely organic, suppression remains moderate. This distinction affects whether the constraint is better classified as Snare (active suppression of excluded alternatives) or Piton (inertial persistence without coercive defense).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(non_orthodox_institutional_capture, empirical, 'Whether non-Orthodox Kodashim reframing is excluded through active suppression or organic institutional divergence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_obligation__study_as_archive, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_obligation__study_as_archive, theater_ratio, 0, 0.62).
narrative_ontology:measurement_basis(koda_tr_t0, observed).
narrative_ontology:measurement(koda_tr_t4, kodashim_obligation__study_as_archive, theater_ratio, 4, 0.65).
narrative_ontology:measurement_basis(koda_tr_t4, observed).
narrative_ontology:measurement(koda_tr_t8, kodashim_obligation__study_as_archive, theater_ratio, 8, 0.67).
narrative_ontology:measurement_basis(koda_tr_t8, observed).
narrative_ontology:measurement(koda_tr_t12, kodashim_obligation__study_as_archive, theater_ratio, 12, 0.69).
narrative_ontology:measurement_basis(koda_tr_t12, observed).
narrative_ontology:measurement(koda_tr_t16, kodashim_obligation__study_as_archive, theater_ratio, 16, 0.7).
narrative_ontology:measurement_basis(koda_tr_t16, observed).
narrative_ontology:measurement(koda_tr_t20, kodashim_obligation__study_as_archive, theater_ratio, 20, 0.71).
narrative_ontology:measurement_basis(koda_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_obligation__study_as_archive, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(koda_be_t0, observed).
narrative_ontology:measurement(koda_be_t4, kodashim_obligation__study_as_archive, base_extractiveness, 4, 0.51).
narrative_ontology:measurement_basis(koda_be_t4, observed).
narrative_ontology:measurement(koda_be_t8, kodashim_obligation__study_as_archive, base_extractiveness, 8, 0.54).
narrative_ontology:measurement_basis(koda_be_t8, observed).
narrative_ontology:measurement(koda_be_t12, kodashim_obligation__study_as_archive, base_extractiveness, 12, 0.56).
narrative_ontology:measurement_basis(koda_be_t12, observed).
narrative_ontology:measurement(koda_be_t16, kodashim_obligation__study_as_archive, base_extractiveness, 16, 0.57).
narrative_ontology:measurement_basis(koda_be_t16, observed).
narrative_ontology:measurement(koda_be_t20, kodashim_obligation__study_as_archive, base_extractiveness, 20, 0.58).
narrative_ontology:measurement_basis(koda_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_obligation__study_as_archive, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(koda_su_t0, observed).
narrative_ontology:measurement(koda_su_t4, kodashim_obligation__study_as_archive, suppression_requirement, 4, 0.37).
narrative_ontology:measurement_basis(koda_su_t4, observed).
narrative_ontology:measurement(koda_su_t8, kodashim_obligation__study_as_archive, suppression_requirement, 8, 0.39).
narrative_ontology:measurement_basis(koda_su_t8, observed).
narrative_ontology:measurement(koda_su_t12, kodashim_obligation__study_as_archive, suppression_requirement, 12, 0.4).
narrative_ontology:measurement_basis(koda_su_t12, observed).
narrative_ontology:measurement(koda_su_t16, kodashim_obligation__study_as_archive, suppression_requirement, 16, 0.41).
narrative_ontology:measurement_basis(koda_su_t16, observed).
narrative_ontology:measurement(koda_su_t20, kodashim_obligation__study_as_archive, suppression_requirement, 20, 0.42).
narrative_ontology:measurement_basis(koda_su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_obligation__study_as_archive, identity_coordination).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_archive, kodashim_obligation__study_as_performance).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_archive, kodashim_obligation__study_as_preparation).

% DUAL FORMULATION NOTE:
% The kodashim_obligation kernel decomposes into three structurally distinct constraints, each instantiating a different reading of the binding character of sacrificial law post-Temple destruction. This story (study_as_archive) treats Kodashim as historical preservation and identity-maintenance—extraction of resources toward a defunct system sustained by institutional inertia. Sibling reading 'study_as_performance' claims studying the law enacts cosmic function (low ε, no extraction). Sibling reading 'study_as_preparation' holds the law binding but unperformable, study preserves knowledge for future restoration (different beneficiary/victim structure, deferred functionality). Each reading has distinct ε, distinct beneficiary/victim set, distinct victim claim justification. All three are live positions within Jewish law traditions; they coexist as institutional and theological positions held by different constituencies. Decomposition follows DP-001 ε-invariance principle: the readings' ε values differ materially because their functional claims about Kodashim differ materially.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kodashim_obligation__study_as_archive, analytical, 0.0).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
