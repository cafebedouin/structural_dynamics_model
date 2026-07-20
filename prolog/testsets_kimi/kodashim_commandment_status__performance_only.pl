% ============================================================================
% CONSTRAINT STORY: kodashim_commandment_status__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_commandment_status__performance_only, []).

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
 *   constraint_id: kodashim_commandment_status__performance_only
 *   human_readable: Kodashim Study Under Performance-Only Reading
 *   domain: religious/halakhic_theory
 *
 * SUMMARY:
 *   The halakhic status of sacrificial commandments (Kodashim) is contested
 *   across three readings of a single kernel. The performance_only reading
 *   treats the commandment as structurally contingent on the Temple altar:
 *   without altar, the commandment is suspended, leaving a husk. Yet the
 *   contemporary yeshiva world extracts massive scholarly investment in this
 *   husk through curriculum requirements and gatekeeping. This constraint
 *   story models the institutional arrangement under the performance_only
 *   reading: a genuine coordination function (preserving textual continuity
 *   and communal identity) is coupled with asymmetric extraction (diverting
 *   scholarly labor from contemporary applied halakha to obsolete sacrificial
 *   law).
 *
 * KEY AGENTS:
 *   - yeshiva_curriculum_authorities (institutional/identity_locked): Set and enforce the requirement that advanced students master Kodashim; their authority depends on preserving the complete traditional corpus.
 *   - advanced_talmud_students (moderate/identity_locked): Bear the cost of years of study in currently inapplicable law; their career path is bound to the traditional curriculum.
 *   - rabbinic_establishment (organized/identity_locked): Collects status and legitimacy from maintenance of the full halakhic corpus including the suspended sacrificial order.
 *   - contemporary_halakhic_communities (moderate/constrained): Victims of diverted scholarly attention; excluded from curriculum design and underserved by applied halakhic innovation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_commandment_status__performance_only, 0.72).
domain_priors:suppression_score(kodashim_commandment_status__performance_only, 0.68).
domain_priors:theater_ratio(kodashim_commandment_status__performance_only, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, extractiveness, 0.72).
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_commandment_status__performance_only, tangled_rope).
narrative_ontology:human_readable(kodashim_commandment_status__performance_only, "Kodashim Study Under Performance-Only Reading").
narrative_ontology:topic_domain(kodashim_commandment_status__performance_only, "religious/halakhic_theory").

domain_priors:requires_active_enforcement(kodashim_commandment_status__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_commandment_status__performance_only, 'bc3c2d38-fba1-4c6a-9c41-2ae9b67fdce8').
narrative_ontology:cs_kernel_codification('bc3c2d38-fba1-4c6a-9c41-2ae9b67fdce8', fixed_text).
narrative_ontology:cs_authority_grounding('bc3c2d38-fba1-4c6a-9c41-2ae9b67fdce8', lineage).
narrative_ontology:cs_interpretation_layer_present('bc3c2d38-fba1-4c6a-9c41-2ae9b67fdce8').
narrative_ontology:cs_reading_relation('bc3c2d38-fba1-4c6a-9c41-2ae9b67fdce8', kodashim_commandment_status__study_as_performance, forecloses).
narrative_ontology:cs_reading_relation('bc3c2d38-fba1-4c6a-9c41-2ae9b67fdce8', kodashim_commandment_status__messianic_deferral, coexists_with).
narrative_ontology:cs_axiom('bc3c2d38-fba1-4c6a-9c41-2ae9b67fdce8', foundational, sacrifice_requires_altar).
narrative_ontology:cs_axiom_status(sacrifice_requires_altar, holdable).
narrative_ontology:cs_axiom_grounding('bc3c2d38-fba1-4c6a-9c41-2ae9b67fdce8', sacrifice_requires_altar, conventional).
narrative_ontology:cs_axiom('bc3c2d38-fba1-4c6a-9c41-2ae9b67fdce8', foundational, study_is_not_fulfillment).
narrative_ontology:cs_axiom_status(study_is_not_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('bc3c2d38-fba1-4c6a-9c41-2ae9b67fdce8', study_is_not_fulfillment, conventional).
narrative_ontology:cs_reference_frame('bc3c2d38-fba1-4c6a-9c41-2ae9b67fdce8', altar_contingency_framework).
narrative_ontology:cs_drift_state('bc3c2d38-fba1-4c6a-9c41-2ae9b67fdce8', contemporary_yeshiva_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bc3c2d38-fba1-4c6a-9c41-2ae9b67fdce8', '').
narrative_ontology:cs_kernel_id(kodashim_commandment_status__performance_only, kodashim_commandment_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__performance_only, rabbinic_establishment).
narrative_ontology:constraint_victim(kodashim_commandment_status__performance_only, advanced_talmud_students).
narrative_ontology:constraint_victim(kodashim_commandment_status__performance_only, contemporary_halakhic_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% They mandate Kodashim as a required tractate for advanced certification and rabbinic ordination, justifying its centrality by appeal to tradition and corpus completeness. Their institutional authority depends on being guardians of an unbroken halakhic chain; altering the curriculum would fracture their identity and legitimacy.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, yeshiva_curriculum_authorities, agenda_setter,
    institutional, generational, identity_locked, global).

% They invest years mastering intricate sacrificial lawâZevachim, Menachot, Tamidâdespite the formal suspension of the commandment in the absence of the Temple. Their progression, peer standing, and eventual credentials depend on this study; opting out means leaving the yeshiva path and its identity community.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, advanced_talmud_students, payer,
    moderate, biographical, identity_locked, national).

% They derive status, publishing platforms, and communal authority from mastery and teaching of the entire halakhic corpus, including the suspended sacrificial order. The preservation of Kodashim expertise reinforces their role as indispensable custodians of tradition and gatekeepers of rabbinic identity.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, rabbinic_establishment, beneficiary,
    organized, generational, identity_locked, global).

% Communities confronting urgent modern halakhic questionsâmedical ethics, family law, economic justiceâreceive disproportionately less scholarly attention because the curriculum and prestige economy prioritize ancient sacrificial law. They are absent from curriculum-design conversations and their needs are structurally deprioritized.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, contemporary_halakhic_communities, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_commandment_status__performance_only, rabbinic_establishment).
narrative_ontology:fixing_cost_class(kodashim_commandment_status__performance_only, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves textual continuity of the sacrificial corpus across generations and maintains a shared, standardized curriculum that bonds the transnational yeshiva community through a common object of study.
% TRANSFER_FUNCTION: Moves years of specialized scholarly labor and institutional resources from applied contemporary halakhic inquiry into the maintenance of expertise in currently inapplicable sacrificial law, while redirecting the attention of rabbinic leadership toward textual preservation.
% ABSENT_VOICES: Progressive halakhic educators advocating curricular reform, students who would prefer intensive applied study but are never consulted, and contemporary communities whose pressing legal questions are starved of scholarly talent.
% DISAPPEARANCE_RATIONALE: If the institutional imperative to study Kodashim as a central obligation vanished, yeshiva curricula would reallocate toward contemporary applied halakha, rabbinic training timelines would compress, and scholarly attention would shift to live legal frontiers; the traditionalist authority structure would weaken measurably.
% FOUNDING_PROBLEM: The destruction of the Second Temple created a crisis of covenantal continuity: how to preserve the sacrificial commandments and priestly expertise during an indefinite period without cultic infrastructure.
% FOUNDING_PROBLEM_CORROBORATION: Academic historians of Judaism and modern Orthodox educators attest that the original urgency was preservation for imminent restoration, a problem now two millennia stale. Contemporary curriculum authorities reframe the founding problem as the eternal value of Torah studyâa narrative corroborated primarily by the beneficiary class. Critical scholars and reform-minded rabbis outside the beneficiary set attest the original problem is functionally dead.
narrative_ontology:disappearance_verdict(kodashim_commandment_status__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_commandment_status__performance_only, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_commandment_status__performance_only, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kodashim_commandment_status__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_commandment_status__performance_only, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_commandment_status__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kodashim_commandment_status__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kodashim_commandment_status__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72) because the institution continues to extract years of specialized labor from students for a commandment it formally regards as suspended. Suppression (0.68) reflects active curricular gatekeeping and the social and identity costs of exiting the traditional study path. Theater ratio (0.45) captures the proportion of study activity that functions as performative maintenance of tradition rather than preparation for actual performance. Accessibility collapse (0.60) indicates that once inside the yeshiva system, alternatives collapse: the curriculum is fixed and prestige attaches to Kodashim mastery. Resistance (0.35) is modest because dissent is channeled into other movements but rarely destabilizes the core institution.
 *
 * PERSPECTIVAL GAP:
 *   From the curriculum authority's seat, the arrangement is a rope: preserving an unbroken chain of expertise across exile. From the student's seat and the contemporary community's seat, the same structure operates as enforced extraction: their resources are captured by a commandment explicitly declared suspended. The engine computes this divergence from the structural data â the asymmetry of exit options and beneficiary declarations â without requiring the author to reconcile the seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The rabbinic_establishment is the structural beneficiary: it collects authority and continuity (low d, damped effective extraction). The advanced_talmud_students and contemporary_halakhic_communities are the targets: their labor and attention are extracted (high d, amplified effective extraction). The yeshiva_curriculum_authorities sit near the beneficiary end but are identity-locked; they enforce the constraint not because they individually capture the gains but because their institutional identity is fused with the tradition's completeness.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling because it retains a genuine coordination function: the textual corpus of Kodashim would indeed be lost without dedicated transmission, and the community is genuinely coordinated around a shared curriculum. However, the coordination story does not explain the scale of extraction or the suppression of curricular alternatives. A pure rope would allow modular study or parallel tracks; the active enforcement of Kodashim centrality amid explicit suspension of the commandment creates the tangled structure. It is not a piton because the rabbinic_establishment still actively benefits and enforces; it is not a snare because the coordination residue is non-zero and historically legible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    study_as_performance_validity,
    'Does the study of sacrificial law constitute a halakhically valid form of commandment-fulfillment independent of physical performance?',
    'Comparative analysis of halakhic responsa and meta-halakhic framework adoption rates across different communities; identification of whether a single jurist can consistently hold both performance-only and study-as-performance axioms.',
    'If study is a valid fulfillment mode, the effective extractiveness of the performance-only reading collapses because the scholarly investment is substantively occupied; if study is not fulfillment, the extraction reading is structurally grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_as_performance_validity, conceptual, 'Whether study counts as performance in the halakhic kernel').

omega_variable(
    curriculum_suppression_mechanism,
    'Is the suppression of alternative curricular pathways primarily structural (institutional gatekeeping and ordination requirements) or internalized (identity-locked scholarly self-conception)?',
    'Tracking post-exit trajectories of students who leave traditional yeshiva pathways for contemporary halakhic institutes; if suppression persists through social sanction and self-exclusion, the mechanism is partially internalized.',
    'If internalized, the constraint''s effective suppression exceeds the institutional measure â the students carry the curriculum preference with them after structural exit, amplifying extraction beyond the scalar suppression score.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(curriculum_suppression_mechanism, empirical, 'Structural vs internalized suppression in halakhic education').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_commandment_status__performance_only, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_commandment_status__performance_only, theater_ratio, 0, 0.2).
narrative_ontology:measurement(koda_tr_t10, kodashim_commandment_status__performance_only, theater_ratio, 10, 0.25).
narrative_ontology:measurement(koda_tr_t20, kodashim_commandment_status__performance_only, theater_ratio, 20, 0.32).
narrative_ontology:measurement(koda_tr_t30, kodashim_commandment_status__performance_only, theater_ratio, 30, 0.38).
narrative_ontology:measurement(koda_tr_t40, kodashim_commandment_status__performance_only, theater_ratio, 40, 0.42).
narrative_ontology:measurement(koda_tr_t50, kodashim_commandment_status__performance_only, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_commandment_status__performance_only, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(koda_be_t10, kodashim_commandment_status__performance_only, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(koda_be_t20, kodashim_commandment_status__performance_only, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(koda_be_t30, kodashim_commandment_status__performance_only, base_extractiveness, 30, 0.6).
narrative_ontology:measurement(koda_be_t40, kodashim_commandment_status__performance_only, base_extractiveness, 40, 0.67).
narrative_ontology:measurement(koda_be_t50, kodashim_commandment_status__performance_only, base_extractiveness, 50, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_commandment_status__performance_only, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(koda_su_t10, kodashim_commandment_status__performance_only, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(koda_su_t20, kodashim_commandment_status__performance_only, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(koda_su_t30, kodashim_commandment_status__performance_only, suppression_requirement, 30, 0.6).
narrative_ontology:measurement(koda_su_t40, kodashim_commandment_status__performance_only, suppression_requirement, 40, 0.65).
narrative_ontology:measurement(koda_su_t50, kodashim_commandment_status__performance_only, suppression_requirement, 50, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_commandment_status__performance_only, identity_coordination).
narrative_ontology:affects_constraint(kodashim_commandment_status__performance_only, kodashim_commandment_status__study_as_performance).
narrative_ontology:affects_constraint(kodashim_commandment_status__performance_only, kodashim_commandment_status__messianic_deferral).

% DUAL FORMULATION NOTE:
% Part of the kodashim_commandment_status kernel family; decomposed from the colloquial label 'status of sacrifice laws' into three structurally distinct constraints per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
