% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Study of Sacrifice Law as Performative Fulfillment
 *   domain: religious_law/ritual_studies/textual_tradition
 *
 * SUMMARY:
 *   This constraint instantiates the study_as_performance reading of the
 *   sacrifice_obligation_continuity kernel. After the destruction of the
 *   Second Temple, physical animal sacrifice became impossible. This reading
 *   resolves the crisis by ruling that the study of sacrificial law itself
 *   fulfills the biblical commandment, maintaining normative continuity
 *   through textual engagement. It treats study not as preparation for future
 *   restoration, nor as mere cultural memory, but as a presently operative,
 *   legitimate fulfillment mechanism. The reading enters the beneficiary set
 *   as accessible fulfillment and carries no victim set because the
 *   obligation is satisfied rather than suspended or violated.
 *
 * KEY AGENTS:
 *   - torah_students: Primary beneficiary (moderate/mobile) â receive accessible fulfillment through daily study of sacrificial law
 *   - rabbinic_authority: Agenda-setter (institutional/arbitrage) â administers the interpretive substitution and transmits it through codes and curricula
 *   - observant_community: Secondary beneficiary (organized/constrained) â receives maintained covenantal continuity via the community's scholars
 *   - temple_mount_advocates: Excluded voice (moderate/constrained) â demands physical performance and is structurally marginalized by the dominant textual reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__study_as_performance, 0.15).
domain_priors:suppression_score(sacrifice_obligation_continuity__study_as_performance, 0.2).
domain_priors:theater_ratio(sacrifice_obligation_continuity__study_as_performance, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, extractiveness, 0.15).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__study_as_performance, rope).
narrative_ontology:human_readable(sacrifice_obligation_continuity__study_as_performance, "Study of Sacrifice Law as Performative Fulfillment").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__study_as_performance, "religious_law/ritual_studies/textual_tradition").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__study_as_performance, '5c08f63c-efca-4e07-b2c1-a096a3c2c387').
narrative_ontology:cs_kernel_codification('5c08f63c-efca-4e07-b2c1-a096a3c2c387', fixed_text).
narrative_ontology:cs_authority_grounding('5c08f63c-efca-4e07-b2c1-a096a3c2c387', lineage).
narrative_ontology:cs_interpretation_layer_present('5c08f63c-efca-4e07-b2c1-a096a3c2c387').
narrative_ontology:cs_reading_relation('5c08f63c-efca-4e07-b2c1-a096a3c2c387', sacrifice_obligation_continuity__archival_preservation, influences).
narrative_ontology:cs_reading_relation('5c08f63c-efca-4e07-b2c1-a096a3c2c387', sacrifice_obligation_continuity__messianic_suspension, coexists_with).
narrative_ontology:cs_reading_relation('5c08f63c-efca-4e07-b2c1-a096a3c2c387', sacrifice_obligation_continuity__performance_only, coexists_with).
narrative_ontology:cs_axiom('5c08f63c-efca-4e07-b2c1-a096a3c2c387', foundational, textual_engagement_generates_ritual_fulfillment).
narrative_ontology:cs_axiom_status(textual_engagement_generates_ritual_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('5c08f63c-efca-4e07-b2c1-a096a3c2c387', textual_engagement_generates_ritual_fulfillment, conventional).
narrative_ontology:cs_axiom('5c08f63c-efca-4e07-b2c1-a096a3c2c387', foundational, commandment_persists_as_active_study_obligation).
narrative_ontology:cs_axiom_status(commandment_persists_as_active_study_obligation, holdable).
narrative_ontology:cs_axiom_grounding('5c08f63c-efca-4e07-b2c1-a096a3c2c387', commandment_persists_as_active_study_obligation, deontological).
narrative_ontology:cs_reference_frame('5c08f63c-efca-4e07-b2c1-a096a3c2c387', textual_fulfillment_framework).
narrative_ontology:cs_drift_state('5c08f63c-efca-4e07-b2c1-a096a3c2c387', post_temple_destruction_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('5c08f63c-efca-4e07-b2c1-a096a3c2c387', '2026-06-19T12:00:00Z').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__study_as_performance, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__study_as_performance, torah_students).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__study_as_performance, observant_community).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__study_as_performance, oral_torah_authority).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__study_as_performance, normative_continuity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Engage in daily study of sacrificial law as a form of divine service. The interpretive tradition holds that this intellectual engagement generates covenantal value equivalent to physical sacrifice, allowing fulfillment of otherwise impossible commandments.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, torah_students, beneficiary,
    moderate, generational, mobile, global).

% Administers the ruling that textual study constitutes valid fulfillment of the sacrificial commandments. Transmits the framework through legal codes, curricular design, and responsa, maintaining interpretive continuity across diaspora communities.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, rabbinic_authority, agenda_setter,
    institutional, generational, arbitrage, global).

% Maintains covenantal relationship and collective identity through the community's scholars; benefits from the normative continuity provided by the study substitution, even though most members do not personally engage in advanced sacrificial law study.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, observant_community, beneficiary,
    organized, generational, constrained, global).

% Reject the substitution of study for physical performance and advocate for immediate restoration of animal sacrifice on the Temple Mount. Structurally marginalized by the dominant halakhic tradition that validates textual engagement as the current legitimate form of fulfillment.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, temple_mount_advocates, excluded,
    moderate, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains covenantal continuity and the active status of divine commandments after the destruction of the Temple by substituting accessible textual study for impossible physical sacrificial performance, preventing normative collapse of the sacrificial framework.
% TRANSFER_FUNCTION: Moves the locus of religious obligation from the physical cultic site to the study hall; the currency is scholarly labor and intellectual engagement, and the receipt is maintained covenantal standing and spiritual fulfillment distributed across the community.
% ABSENT_VOICES: Temple Mount activists who demand immediate physical sacrifice, adherents of messianic suspension who regard the obligation as frozen, and secular archival scholars who deny normative force to the texts â they are present in the broader discourse but excluded from the normative halakhic framework that validates study as performative.
% DISAPPEARANCE_RATIONALE: If the ruling vanished, observant communities would lose the primary halakhic mechanism for maintaining the sacrificial commandments; the normative structure would shift toward messianic suspension, archival preservation, or a crisis of unfulfilled obligations.
% FOUNDING_PROBLEM: The destruction of the Second Temple eliminated the physical and institutional infrastructure for animal sacrifice, creating an apparent impossibility of fulfilling a central set of biblical commandments.
% FOUNDING_PROBLEM_CORROBORATION: Extra-biblical historical sources (Josephus, Roman historians) and archaeology attest the Temple's destruction. The specific solution â study as fulfillment â is primarily corroborated by the rabbinic literary tradition itself (Mishnah, Talmud, medieval codes); no purely external corroborator exists for the normative substitution, though the persistence of study institutions across two millennia is materially evident.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__study_as_performance, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__study_as_performance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__study_as_performance, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_obligation_continuity__study_as_performance, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_continuity__study_as_performance, 0.15, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is low (0.15 at interval end) because the constraint transfers obligation into an accessible intellectual domain rather than extracting material rents or coerced labor for a third party. Suppression is low-moderate (0.20) because the reading dominates normative halakhic discourse but does not rely on active coercion; alternative readings persist in adjacent communities. Theater ratio is very low (0.10) because the study function is substantive and generative of legal and spiritual value, not performative maintenance. Accessibility collapse is moderate (0.45): once one accepts the rabbinic framework, physical sacrifice becomes structurally inaccessible (no Temple), but alternative readings remain cognitively available. Resistance is low (0.20) because the reading is broadly accepted within the tradition that maintains it, though excluded voices contest it. The measurement series use a single shared time grid to prevent misalignment.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (rabbinic authority) and beneficiary (Torah student) seats should compute similarly as low-extraction coordination: both are embedded in the same normative framework and experience the constraint as sustaining rather than extracting. The excluded seat (Temple Mount advocate) experiences the constraint as suppression of the authentic fulfillment mode; from that external position, the computed type would skew toward higher extraction and suppression because the constraint forecloses their preferred practice. The engine derives this divergence from the structural data rather than from the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Torah students and the observant community are beneficiaries: the constraint subsidizes their covenantal continuity by providing an accessible fulfillment path (low d, low effective extraction). Rabbinic authority sits near symmetric: they administer the constraint and derive institutional continuity from it, but also bear the labor of interpretation. Temple Mount advocates are structurally excluded: the constraint's dominance crowds out their preferred reading, giving them high directionality if they were seated as targets, though they are not governed by the constraint so much as silenced by its hegemony.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as rope prevents mislabeling this as piton or snare: there is no concentrated extraction, no victim set, and the coordination function (maintaining covenantal practice post-Temple) is genuine and ongoing. A mandatrophy-resolved reading would require showing that the Temple has been rebuilt and the study substitution persists out of inertia; since the founding problem (Temple absence) is still live, the constraint is not piton. The low theater ratio confirms that the activity is substantive rather than theatrical.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    study_accessibility_asymmetry,
    'Does the claim that study is universally accessible mask asymmetries of literacy, time, gender, and economic capacity that effectively exclude some community members from the primary fulfillment mechanism?',
    'Demographic and ethnographic analysis of Talmud-study access across socioeconomic and gender lines within observant communities, paired with testimony from excluded subgroups.',
    'If substantial asymmetry exists, the constraint''s beneficiary set contracts and its extractiveness rises for excluded subgroups, potentially shifting the computed type toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_accessibility_asymmetry, empirical, 'Whether study access is genuinely universal or structurally asymmetric.').

omega_variable(
    kernel_reading_contestation,
    'Does the study-as-performance reading represent a genuine continuity of the sacrificial institution, or is it a post-hoc rationalization that allows the community to maintain institutional coherence in the face of historical rupture?',
    'Historical analysis of the emergence of the study-as-fulfillment ruling in early rabbinic literature, tracing whether it predates the Temple''s destruction or appears as a response to it.',
    'If it is a post-hoc rationalization, the constraint''s coordination function is retroactive narrative rather than genuine continuity, raising theater_ratio and potentially reclassifying toward piton if the founding problem is later resolved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Whether the reading is genuine continuity or post-hoc rationalization.').

omega_variable(
    normative_force_of_kernel,
    'Is the kernel (the sacrificial commandment) genuinely sustained as a live normative obligation by this reading, or has it been effectively converted into an institutional practice whose original normative anchor is no longer retrievable?',
    'Phenomenological and jurisprudential analysis of whether practitioners experience the study as fulfilling an external command or as maintaining an internalized communal habit.',
    'If the normative anchor is lost, the constraint drifts toward identity_coordination with higher theater_ratio, and the rope classification becomes suspect.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(normative_force_of_kernel, conceptual, 'Whether the commandment retains live normative force or has become habitual practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__study_as_performance, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacrifice_study_perf_tr_t0, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 0, 0.05).
narrative_ontology:measurement(sacrifice_study_perf_tr_t400, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 400, 0.06).
narrative_ontology:measurement(sacrifice_study_perf_tr_t800, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 800, 0.07).
narrative_ontology:measurement(sacrifice_study_perf_tr_t1200, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 1200, 0.08).
narrative_ontology:measurement(sacrifice_study_perf_tr_t1600, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 1600, 0.09).
narrative_ontology:measurement(sacrifice_study_perf_tr_t2000, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 2000, 0.1).

% Extraction over time
narrative_ontology:measurement(sacrifice_study_perf_be_t0, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(sacrifice_study_perf_be_t400, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 400, 0.22).
narrative_ontology:measurement(sacrifice_study_perf_be_t800, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 800, 0.2).
narrative_ontology:measurement(sacrifice_study_perf_be_t1200, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 1200, 0.18).
narrative_ontology:measurement(sacrifice_study_perf_be_t1600, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 1600, 0.16).
narrative_ontology:measurement(sacrifice_study_perf_be_t2000, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 2000, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(sacrifice_study_perf_su_t0, sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(sacrifice_study_perf_su_t400, sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 400, 0.27).
narrative_ontology:measurement(sacrifice_study_perf_su_t800, sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 800, 0.25).
narrative_ontology:measurement(sacrifice_study_perf_su_t1200, sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 1200, 0.23).
narrative_ontology:measurement(sacrifice_study_perf_su_t1600, sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 1600, 0.21).
narrative_ontology:measurement(sacrifice_study_perf_su_t2000, sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 2000, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__study_as_performance, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__study_as_performance, sacrifice_obligation_continuity__archival_preservation).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__study_as_performance, sacrifice_obligation_continuity__performance_only).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__study_as_performance, sacrifice_obligation_continuity__messianic_suspension).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the sacrifice_obligation_continuity kernel. The natural-language concept of 'sacrificial obligation after the Temple' decomposes into four structurally distinct constraints (study_as_performance, performance_only, messianic_suspension, archival_preservation) per the Îµ-invariance principle. Each has a distinct beneficiary/victim structure and extractiveness profile.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
