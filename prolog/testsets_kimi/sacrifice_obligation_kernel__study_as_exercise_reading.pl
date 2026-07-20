% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__study_as_exercise_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_kernel__study_as_exercise_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: sacrifice_obligation_kernel__study_as_exercise_reading
 *   human_readable: Study of Sacrifice Law as Mitzvah Exercise (Study-as-Exercise Reading)
 *   domain: religious_law/halakhic_authority
 *
 * SUMMARY:
 *   This constraint instantiates the study-as-exercise reading of the
 *   sacrifice obligation kernel in rabbinic Judaism. After the destruction of
 *   the Second Temple, the biblical commandment of animal sacrifice became
 *   physically impossible. This reading transforms the obligation into
 *   intellectual engagement: study of sacrificial law constitutes the genuine
 *   exercise of the mitzvah. The reading coordinates Jewish religious
 *   continuity under radically changed conditions while concentrating
 *   interpretive authority in rabbinic hands. It is claimed as pure
 *   coordination (zero extractiveness, authorized transformation), but the
 *   structural data records a mild asymmetry: rabbinic authority captures
 *   deference and institutional legitimacy as the gatekeeper of what counts
 *   as fulfillment.
 *
 * KEY AGENTS:
 *   - rabbinic_authority: Agenda-setter and beneficiary â institutional power, arbitrage-grade exit options, global scope across the Jewish diaspora
 *   - torah_students: Primary participants â moderate power, identity-locked exit, bear the labor of study while gaining a viable path to fulfillment
 *   - performance_advocates: Excluded challengers â hold the performance-only reading, structurally marginalized within normative halakhah
 *   - messianic_activists: Excluded challengers â hold the suspension reading, contest the active-status of the obligation under current conditions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__study_as_exercise_reading, 0.22).
domain_priors:suppression_score(sacrifice_obligation_kernel__study_as_exercise_reading, 0.28).
domain_priors:theater_ratio(sacrifice_obligation_kernel__study_as_exercise_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__study_as_exercise_reading, rope).
narrative_ontology:human_readable(sacrifice_obligation_kernel__study_as_exercise_reading, "Study of Sacrifice Law as Mitzvah Exercise (Study-as-Exercise Reading)").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__study_as_exercise_reading, "religious_law/halakhic_authority").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__study_as_exercise_reading, 'a40f6eef-4753-489e-a1d8-d38ee035b64a').
narrative_ontology:cs_kernel_codification('a40f6eef-4753-489e-a1d8-d38ee035b64a', fixed_text).
narrative_ontology:cs_authority_grounding('a40f6eef-4753-489e-a1d8-d38ee035b64a', lineage).
narrative_ontology:cs_interpretation_layer_present('a40f6eef-4753-489e-a1d8-d38ee035b64a').
narrative_ontology:cs_reading_relation('a40f6eef-4753-489e-a1d8-d38ee035b64a', sacrifice_obligation_kernel__performance_only_reading, forecloses).
narrative_ontology:cs_reading_relation('a40f6eef-4753-489e-a1d8-d38ee035b64a', sacrifice_obligation_kernel__messianic_suspension_reading, forecloses).
narrative_ontology:cs_reading_relation('a40f6eef-4753-489e-a1d8-d38ee035b64a', sacrifice_obligation_kernel__symbolic_archive_reading, influences).
narrative_ontology:cs_axiom('a40f6eef-4753-489e-a1d8-d38ee035b64a', foundational, study_fulfills_sacrifice_mitzvah).
narrative_ontology:cs_axiom_status(study_fulfills_sacrifice_mitzvah, holdable).
narrative_ontology:cs_axiom_grounding('a40f6eef-4753-489e-a1d8-d38ee035b64a', study_fulfills_sacrifice_mitzvah, deontological).
narrative_ontology:cs_axiom('a40f6eef-4753-489e-a1d8-d38ee035b64a', foundational, rabbinic_transformation_authority).
narrative_ontology:cs_axiom_status(rabbinic_transformation_authority, holdable).
narrative_ontology:cs_axiom_grounding('a40f6eef-4753-489e-a1d8-d38ee035b64a', rabbinic_transformation_authority, conventional).
narrative_ontology:cs_reference_frame('a40f6eef-4753-489e-a1d8-d38ee035b64a', study_based_fulfillment_active).
narrative_ontology:cs_drift_state('a40f6eef-4753-489e-a1d8-d38ee035b64a', contemporary_diaspora_practice, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a40f6eef-4753-489e-a1d8-d38ee035b64a', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__study_as_exercise_reading, rabbinic_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__study_as_exercise_reading, torah_students).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__study_as_exercise_reading, rabbinic_oral_law_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the halakhic framework that redefines Temple sacrifice obligation as fulfilled through intellectual study of sacrificial law. Controls the interpretive boundaries of what textual engagement counts as adequate fulfillment. Derives institutional legitimacy, deference, and continuity from this mediating role between biblical commandment and post-Temple practice.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, rabbinic_authority, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_kernel__study_as_exercise_reading, rabbinic_authority, beneficiary).

% Engage in study of Temple sacrifice tractates as the active fulfillment of the biblical mitzvah under current conditions. Their cognitive labor and time constitute the obligation. Exit from this framework requires relinquishing the rabbinic interpretive paradigm, which is fused with religious identity.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, torah_students, beneficiary,
    moderate, biographical, identity_locked, local).

% Maintain that physical sacrifice is the sole valid fulfillment and reject the rabbinic transformation of the obligation into study. Their position is structurally marginalized within normative halakhic discourse that has accepted the study-as-exercise reading, though it persists in certain traditionalist and academic-theological circles.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, performance_advocates, excluded,
    moderate, biographical, constrained, national).

% Hold that the sacrifice obligation is divinely suspended rather than transformed, and await messianic Temple restoration. They contest the study-as-exercise reading's claim that intellectual engagement presently fulfills an active obligation, arguing instead that study maintains operational readiness for an eventual restoration.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, messianic_activists, excluded,
    moderate, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_kernel__study_as_exercise_reading, rabbinic_authority).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves continuity of the biblical sacrifice mitzvah across the rupture of Temple destruction by transforming the obligation into a practice (Torah study) that remains universally accessible, preventing widespread religious obsolescence and communal discontinuity.
% TRANSFER_FUNCTION: Transfers the locus of divine service from physical priestly performance in Jerusalem to decentralized intellectual engagement with sacrificial texts; simultaneously transfers normative authority to rabbinic interpreters who certify what constitutes adequate study-fulfillment.
% ABSENT_VOICES: Performance-only advocates who regard study as preparatory but halakhically insufficient; messianic suspension advocates who reject any present-day fulfillment mechanism; secular-symbolic readers who bracket halakhic obligation entirely and treat the texts as cultural archive.
% DISAPPEARANCE_RATIONALE: If the study-as-exercise reading vanished, the Jewish community would face a halakhic vacuum regarding Temple sacrifices: either revert to messianic passivity, adopt purely memorial practices, or generate radical new frameworks. Rabbinic authority's central mediating role would weaken, and the organizational energy devoted to sacrificial study would shift.
% FOUNDING_PROBLEM: The Roman destruction of the Second Temple in 70 CE eliminated the physical site, priesthood, and ritual infrastructure required for biblical sacrifice, apparently rendering numerous Torah commandments unfulfillable and threatening the continuity of covenantal practice.
% FOUNDING_PROBLEM_CORROBORATION: Academic historians of ancient Judaism corroborate the historical rupture of 70 CE and the structural necessity of religious transformation. Tannaitic and Amoraic literature documents the rabbinic shift. However, the specific normative claim that study constitutes genuine exercise rather than memorial or suspension is contested by Karaite, some messianic, and revisionist historiographical sources outside the rabbinic beneficiary set.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__study_as_exercise_reading, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__study_as_exercise_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__study_as_exercise_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_obligation_kernel__study_as_exercise_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__study_as_exercise_reading, 0.22, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__study_as_exercise_reading_tests).
:- end_tests(sacrifice_obligation_kernel__study_as_exercise_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.22 at interval end) because the constraint operates primarily through legitimacy and identity rather than material extraction; study is open to all and the spiritual benefit is internal to the participant. Suppression is moderate-low (0.28) because alternative readings are socially marginalized but not violently suppressed. Theater ratio rises gradually (0.32) as the study practice becomes routinized and partially performative of rabbinic authority maintenance. Accessibility collapse is moderate (0.40): within the rabbinic framework, physical performance alternatives are inaccessible (no Temple), but alternative readings (suspension, archive) remain intellectually accessible. Resistance is low (0.25) because the reading has achieved broad normative stability, though minority sects persist.
 *
 * PERSPECTIVAL GAP:
 *   The rabbinic authority seat experiences this constraint as genuine coordination that saved the tradition from obsolescence; the engine should compute a low directionality near the beneficiary pole. The torah-student seat experiences the same structure as a labor-intensive obligation fused with religious identity; while not victimized, their identity-locked exit and the asymmetry of interpretive control push directionality toward the target side. The excluded performance and messianic seats experience it as an illegitimate usurpation. The engine computes these divergences from the structural data rather than the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authority is the declared beneficiary and agenda-setter, deriving low directionality from beneficiary status plus arbitrage exit. Torah students are not declared victims, but their identity-locked exit and the moderate power asymmetry produce a derived directionality closer to symmetric or mild-target. No victim declarations mean the engine does not register a high-extraction target seat, consistent with the rope claim and the low base extractiveness. The excluded seats are not in the beneficiary/victim derivation chain but contribute to resistance.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the rabbinic transformation as a snare or piton. While rabbinic authority benefits, there is no concentrated extraction, no victim set, and the coordination function (preserving obligation continuity) is genuine and live. Mandatrophy would occur if the study practice persisted after the Temple were rebuilt and sacrifice resumed; in that counterfactual, the constraint would likely drift toward tangled_rope or piton as its coordination function atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sacrifice_kernel_reading_location,
    'This constraint is the study-as-exercise reading of the sacrifice_obligation_kernel. How would classification change if the symbolic_archive_reading or performance_only_reading were adopted as the dominant framework?',
    'Comparative analysis of the sibling constraints in the same kernel family; each reading carries a different epsilon, beneficiary structure, and enforcement profile.',
    'Adopting performance_only would introduce a victim set (those unable to sacrifice) and likely raise extractiveness if the constraint were enforced; symbolic_archive would eliminate the rabbinic beneficiary and collapse extractiveness to near zero, reclassifying as a low-extraction identity_coordination mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sacrifice_kernel_reading_location, conceptual, 'This constraint is one reading of a contested kernel; siblings emit different structural profiles.').

omega_variable(
    interpretive_monopoly_extraction,
    'Does rabbinic authority''s interpretive monopoly on what counts as fulfillment constitute extraction from the community, or is it the necessary coordinative cost of maintaining halakhic continuity?',
    'Assess whether alternative interpretive authorities (Karaite, individualist, academic) are structurally suppressed or merely minority positions; measure community exit costs from rabbinic frameworks.',
    'If the monopoly suppresses alternatives, effective extraction is higher than the low base metric suggests and the constraint may compute as tangled_rope; if alternatives coexist freely, the coordination is closer to pure rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_monopoly_extraction, conceptual, 'Whether rabbinic interpretive monopoly is coordinative or extractive.').

omega_variable(
    study_performance_efficacy,
    'Is intellectual engagement with sacrifice law structurally equivalent to physical performance in producing the religious goods (atonement, communion, divine acceptance) the mitzvah is understood to effect?',
    'Theological analysis within the rabbinic tradition; empirical measurement is inapplicable to the metaphysical claims at stake.',
    'If study is not structurally equivalent, the coordination function may be cover for institutional authority maintenance; if equivalent, the rope classification is strongly supported and the reading''s low extractiveness is warranted.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(study_performance_efficacy, preference, 'Theological equivalence of study and sacrifice as modes of fulfillment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__study_as_exercise_reading, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(study_exercise_tr_t0, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(study_exercise_tr_t400, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 400, 0.15).
narrative_ontology:measurement(study_exercise_tr_t800, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 800, 0.2).
narrative_ontology:measurement(study_exercise_tr_t1200, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 1200, 0.25).
narrative_ontology:measurement(study_exercise_tr_t1600, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 1600, 0.3).
narrative_ontology:measurement(study_exercise_tr_t2000, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 2000, 0.32).

% Extraction over time
narrative_ontology:measurement(study_exercise_be_t0, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(study_exercise_be_t400, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 400, 0.18).
narrative_ontology:measurement(study_exercise_be_t800, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 800, 0.2).
narrative_ontology:measurement(study_exercise_be_t1200, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 1200, 0.22).
narrative_ontology:measurement(study_exercise_be_t1600, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 1600, 0.24).
narrative_ontology:measurement(study_exercise_be_t2000, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 2000, 0.25).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sacrifice_obligation_kernel__study_as_exercise_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__study_as_exercise_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_obligation_kernel__study_as_exercise_reading, 0.08).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel__performance_only_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel__messianic_suspension_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel__symbolic_archive_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the sacrifice_obligation_kernel, which decomposes into four structurally distinct claims about the status of the biblical Temple-sacrifice mitzvah after 70 CE. Each reading carries a different epsilon, beneficiary structure, and halakhic force. This file instantiates the study-as-exercise reading; siblings are separate constraints linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
