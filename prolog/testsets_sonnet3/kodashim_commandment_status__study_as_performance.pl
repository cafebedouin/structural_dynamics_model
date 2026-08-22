% ============================================================================
% CONSTRAINT STORY: kodashim_commandment_status__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_commandment_status__study_as_performance, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: kodashim_commandment_status__study_as_performance
 *   human_readable: Talmudic Study of Kodashim as Commandment Fulfillment
 *   domain: religious/halakhic/commitment_system
 *
 * SUMMARY:
 *   This story instantiates one reading among three of a contested kernel
 *   concerning the status of biblical sacrificial commandments (Kodashim)
 *   after the destruction of the Second Temple made physical performance
 *   impossible. Under the study_as_performance reading, the act of studying
 *   the relevant legal texts is held to constitutively fulfill the
 *   commandment — not as a stand-in, consolation, or preparatory exercise,
 *   but as the commandment's actual discharge. This produces a structurally
 *   low-extraction, low-suppression constraint: there is no performance gap
 *   because there is no unfulfilled obligation, and no identifiable victim of
 *   non-performance because performance was redefined rather than merely
 *   excused. The sibling readings (performance_only, which treats the
 *   commandment as suspended/husk pending an altar; messianic_deferral, which
 *   treats it as suspended-but-not-obsolete, held in reserve for future
 *   restoration) are NOT described here — they are separate constraints with
 *   their own ε and structural profiles, linked via
 *   network.affects_constraints and cs_structure.reading_relations.
 *
 * KEY AGENTS:
 *   - yeshiva_scholars: Primary beneficiary and agenda-setter (organized/identity_locked) — vocation gains full religious weight through this reading
 *   - torah_study_communities: Institutional beneficiary (organized/constrained) — communal study structures sustained by the doctrine
 *   - diaspora_practitioners: Individual beneficiary (moderate/mobile) — retain commandment-completion status despite exile
 *   - performance_only_adherents: Excluded/sibling-reading holders (organized/analytical) — hold a coexisting but different framework, not addressed by this constraint
 *   - halakhic_authorities: Analytical observer (institutional/analytical) — adjudicate transmission without resolving the underlying kernel contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_commandment_status__study_as_performance, 0.04).
domain_priors:suppression_score(kodashim_commandment_status__study_as_performance, 0.08).
domain_priors:theater_ratio(kodashim_commandment_status__study_as_performance, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, extractiveness, 0.04).
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_commandment_status__study_as_performance, rope).
narrative_ontology:human_readable(kodashim_commandment_status__study_as_performance, "Talmudic Study of Kodashim as Commandment Fulfillment").
narrative_ontology:topic_domain(kodashim_commandment_status__study_as_performance, "religious/halakhic/commitment_system").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_commandment_status__study_as_performance, '51d18201-c86a-4a2b-81ec-64210e6e8a7e').
narrative_ontology:cs_kernel_codification('51d18201-c86a-4a2b-81ec-64210e6e8a7e', fixed_text).
narrative_ontology:cs_authority_grounding('51d18201-c86a-4a2b-81ec-64210e6e8a7e', lineage).
narrative_ontology:cs_interpretation_layer_present('51d18201-c86a-4a2b-81ec-64210e6e8a7e').
narrative_ontology:cs_reading_relation('51d18201-c86a-4a2b-81ec-64210e6e8a7e', kodashim_commandment_status__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('51d18201-c86a-4a2b-81ec-64210e6e8a7e', kodashim_commandment_status__messianic_deferral, influences).
narrative_ontology:cs_axiom('51d18201-c86a-4a2b-81ec-64210e6e8a7e', foundational, study_constitutes_fulfillment).
narrative_ontology:cs_axiom_status(study_constitutes_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('51d18201-c86a-4a2b-81ec-64210e6e8a7e', study_constitutes_fulfillment, conventional).
narrative_ontology:cs_axiom('51d18201-c86a-4a2b-81ec-64210e6e8a7e', secondary, commandment_discharge_is_present_not_deferred).
narrative_ontology:cs_axiom_status(commandment_discharge_is_present_not_deferred, holdable).
narrative_ontology:cs_axiom_grounding('51d18201-c86a-4a2b-81ec-64210e6e8a7e', commandment_discharge_is_present_not_deferred, conventional).
narrative_ontology:cs_reference_frame('51d18201-c86a-4a2b-81ec-64210e6e8a7e', temple_era_sacrificial_praxis).
narrative_ontology:cs_drift_state('51d18201-c86a-4a2b-81ec-64210e6e8a7e', post_destruction_rabbinic_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('51d18201-c86a-4a2b-81ec-64210e6e8a7e', '').
narrative_ontology:cs_kernel_id(kodashim_commandment_status__study_as_performance, kodashim_commandment_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__study_as_performance, torah_study_communities).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__study_as_performance, yeshiva_scholars).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__study_as_performance, diaspora_practitioners).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__study_as_performance, study_equals_performance_doctrine).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__study_as_performance, continuous_commandment_occupation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Devote substantial study hours to the tractates governing sacrificial law (Zevachim, Menachot, and the wider Kodashim order) despite no Temple existing to perform the rites. Under this reading, the study itself discharges the commandment, so their scholarly labor is not preparatory or symbolic but constitutive — they are actively fulfilling the mitzvah in the study hall. This gives their vocation full religious weight independent of any future restoration.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, yeshiva_scholars, beneficiary,
    organized, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(kodashim_commandment_status__study_as_performance, yeshiva_scholars, agenda_setter).

% Structure communal learning schedules (daf yomi cycles, seder kodashim study groups) around the premise that engaging these texts is itself religiously complete action, not a placeholder. This sustains institutional life — study halls, publishing of commentary, curricular continuity — around a legal corpus that has no operative altar-based application.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, torah_study_communities, beneficiary,
    organized, generational, constrained, global).

% Live with no access to sacrificial performance by definition of exile, yet this reading tells them their engagement with the relevant legal text is not a substitute or consolation but the commandment's actual fulfillment. They lose nothing relative to a hypothetical performer, because performance and study are structurally equated.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, diaspora_practitioners, beneficiary,
    moderate, biographical, mobile, global).

% Hold that the commandment is contingent on an operative Temple and altar, and that study, however valuable, does not discharge an obligation whose performance conditions are absent. They are not persecuted or coerced by the study_as_performance reading, but their framework is simply a different, coexisting claim about the same kernel; this constraint does not address or refute them.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, performance_only_adherents, excluded,
    organized, generational, analytical, global).

% Adjudicate which readings of commandment status are transmitted with what authority in various communities; they observe and sometimes endorse the study_as_performance doctrine (rooted in classical sources treating Torah study of sacrificial law as equivalent to offering) without needing to resolve the underlying kernel contest definitively.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, halakhic_authorities, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a religious community's continued relationship to a body of law that has no current operative application, preventing an entire legal corpus (and the identity, scholarship, and communal practice built around it) from going dormant or being treated as dead letter.
% TRANSFER_FUNCTION: Moves nothing extractive between parties; what is 'transferred' is standing and completeness — practitioners who study receive the same commandment-fulfillment status as one who would perform the rite, redistributing religious efficacy from physical action to intellectual engagement.
% ABSENT_VOICES: Adherents of the performance_only reading would object that equating study with performance dissolves a meaningful distinction the law itself draws elsewhere between contemplation and action; they are not silenced, merely holding a structurally different position within the same tradition, addressed by a sibling constraint rather than this one.
% DISAPPEARANCE_RATIONALE: If the study_as_performance doctrine vanished, study of Kodashim would likely continue as a matter of intellectual and curricular tradition, but its religious status would downgrade for adherents from 'fulfillment' to 'preparation or scholarship,' which some communities would experience as a real loss of standing and others would consider a harmless reclassification — hence contested rather than a clean rearrangement.
% FOUNDING_PROBLEM: With the Temple destroyed, an entire tranche of biblical commandments concerning sacrifice had no possible performance venue; without some doctrine addressing their status, they risked being treated as obsolete, and study of them as merely antiquarian rather than religiously significant.
% FOUNDING_PROBLEM_CORROBORATION: Classical rabbinic sources (e.g., statements attributed to the Amoraim in Menachot and Taanit regarding study substituting for offering) are cited by later halakhic authorities across denominational lines, including scholars outside the yeshiva-institutional beneficiary set who study the doctrine's history without personally practicing intensive Kodashim study; comparative religion scholars documenting rabbinic Judaism's post-Temple adaptation independently corroborate that the founding problem (maintaining relevance of sacrificial law after 70 CE) was real and unresolved by any other single mechanism.
narrative_ontology:disappearance_verdict(kodashim_commandment_status__study_as_performance, contested).
narrative_ontology:founding_problem_status(kodashim_commandment_status__study_as_performance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_commandment_status__study_as_performance, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kodashim_commandment_status__study_as_performance, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_commandment_status__study_as_performance, 0.04, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_commandment_status__study_as_performance_tests).
:- end_tests(kodashim_commandment_status__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored very low (0.04) because under this reading's own lights there is no gap between obligation and discharge — the constraint does not extract anything from anyone since the commandment is, by the reading's own logic, fully satisfied by study. Suppression is low (0.08): no one is coerced into accepting the doctrine, and dissenting readings persist openly within the tradition. Theater ratio is modest (0.1) reflecting the genuine intellectual and communal labor involved, not performative display. Accessibility collapse is moderate (0.35) rather than low, because once a community internalizes the study_as_performance framework, the alternative readings become harder to entertain seriously within that community's own practice, even though they remain visible in the wider tradition. Resistance is low (0.15): the doctrine is broadly accepted wherever it is transmitted, though it coexists with real alternative positions elsewhere.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (scholars, study communities, diaspora practitioners) sit near the full-beneficiary end of directionality: the constraint subsidizes their religious standing by equating their actual activity (study) with the commandment's performance, at zero cost to them. There is no victim group under this reading — the entire point of the expected structural delta is that the victim set is empty, since no one is harmed by non-performance when performance has been redefined as achieved through study. This is structurally distinct from the performance_only sibling, which would treat non-performers as bearing an unfulfilled (if suspended) obligation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (maintaining religious relevance of sacrificial law after Temple destruction) is live and openly corroborated by sources outside the immediate beneficiary community (comparative religious scholarship, cross-denominational halakhic citation), which prevents this from reading as a mere self-serving mandatrophy cover story — the doctrine was not invented to protect a shrinking institution's mandate after its function died, but responds to a genuine and enduring adaptation problem. Because the disappearance_verdict is contested rather than world_unchanged, the classification correctly resists forcing this into either 'obviously coordination' or 'obviously extraction' — it is read here as low-extraction Rope precisely because the redefinition of performance is internally coherent and imposes no identifiable cost on anyone, which a Snare or Tangled Rope classification would require.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    study_as_performance_versus_deferral,
    'Is the study_as_performance doctrine a genuine, independently-grounded halakhic position, or a pragmatic accommodation that functions similarly to messianic_deferral but claims stronger (present-tense, constitutive) status than the sources fully support?',
    'Close textual analysis of the classical sources (e.g., Talmud Menachot 110a, Taanit 27b) cited for the doctrine, compared against how those same sources are read by adherents of messianic_deferral, adjudicated by historians of halakha rather than by current practitioners of either reading.',
    'If study_as_performance is found to be a later, stronger gloss on sources that originally supported only messianic_deferral''s weaker ''readiness maintenance'' claim, the zero-extraction structural delta claimed here would be less secure, and this constraint would sit closer to its sibling on the extraction axis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_as_performance_versus_deferral, conceptual, 'Whether the doctrine''s strong constitutive claim is independently grounded or an inflation of the deferral reading''s weaker claim.').

omega_variable(
    cross_framing_kernel_disagreement,
    'Two coherent framings of the kernel are available: (a) the kernel is the commandment itself (a legal obligation whose status is contested), and (b) the kernel is the interpretive authority to declare what counts as fulfillment (a meta-level claim about who gets to redefine performance). Framing (a) treats this constraint as a rope resolving a genuine coordination problem (keeping the corpus alive); framing (b) raises the question of whether declaring study equivalent to performance is itself an exercise of interpretive power that could be extractive toward performance_only adherents by delegitimizing their position over time.',
    'Track whether communities holding study_as_performance historically or currently exert institutional pressure (curricular exclusion, social sanction) against performance_only or messianic_deferral adherents, versus simple peaceful coexistence of readings.',
    'Under framing (a), this constraint remains a low-extraction rope as authored. Under framing (b), if interpretive dominance is used to marginalize sibling readings, the classification could shift toward tangled_rope with performance_only adherents as an emergent victim class.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cross_framing_kernel_disagreement, conceptual, 'Whether the kernel is best framed as the commandment''s substantive status or as the interpretive authority to define fulfillment, which yield different classification implications.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_commandment_status__study_as_performance, 0, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_commandment_status__study_as_performance, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(koda_tr_t0, observed).
narrative_ontology:measurement(koda_tr_t300, kodashim_commandment_status__study_as_performance, theater_ratio, 300, 0.12).
narrative_ontology:measurement_basis(koda_tr_t300, observed).
narrative_ontology:measurement(koda_tr_t700, kodashim_commandment_status__study_as_performance, theater_ratio, 700, 0.1).
narrative_ontology:measurement_basis(koda_tr_t700, observed).
narrative_ontology:measurement(koda_tr_t1100, kodashim_commandment_status__study_as_performance, theater_ratio, 1100, 0.1).
narrative_ontology:measurement_basis(koda_tr_t1100, observed).
narrative_ontology:measurement(koda_tr_t1500, kodashim_commandment_status__study_as_performance, theater_ratio, 1500, 0.1).
narrative_ontology:measurement_basis(koda_tr_t1500, observed).
narrative_ontology:measurement(koda_tr_t1900, kodashim_commandment_status__study_as_performance, theater_ratio, 1900, 0.1).
narrative_ontology:measurement_basis(koda_tr_t1900, observed).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_commandment_status__study_as_performance, base_extractiveness, 0, 0.06).
narrative_ontology:measurement_basis(koda_be_t0, observed).
narrative_ontology:measurement(koda_be_t300, kodashim_commandment_status__study_as_performance, base_extractiveness, 300, 0.05).
narrative_ontology:measurement_basis(koda_be_t300, observed).
narrative_ontology:measurement(koda_be_t700, kodashim_commandment_status__study_as_performance, base_extractiveness, 700, 0.05).
narrative_ontology:measurement_basis(koda_be_t700, observed).
narrative_ontology:measurement(koda_be_t1100, kodashim_commandment_status__study_as_performance, base_extractiveness, 1100, 0.04).
narrative_ontology:measurement_basis(koda_be_t1100, observed).
narrative_ontology:measurement(koda_be_t1500, kodashim_commandment_status__study_as_performance, base_extractiveness, 1500, 0.04).
narrative_ontology:measurement_basis(koda_be_t1500, observed).
narrative_ontology:measurement(koda_be_t1900, kodashim_commandment_status__study_as_performance, base_extractiveness, 1900, 0.04).
narrative_ontology:measurement_basis(koda_be_t1900, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(kodashim_commandment_status__study_as_performance, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_commandment_status__study_as_performance, identity_coordination).
narrative_ontology:boltzmann_floor_override(kodashim_commandment_status__study_as_performance, 0.05).
narrative_ontology:affects_constraint(kodashim_commandment_status__study_as_performance, kodashim_commandment_status__performance_only).
narrative_ontology:affects_constraint(kodashim_commandment_status__study_as_performance, kodashim_commandment_status__messianic_deferral).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language concept 'the status of sacrifice commandments after Temple destruction' (the kodashim_commandment_status kernel) per the ε-invariance principle. study_as_performance (this file) authors near-zero extraction and an empty victim set because it treats study as full, present-tense discharge of the commandment. performance_only authors the commandment as a currently-suspended legal husk contingent on an absent altar. messianic_deferral authors the commandment as suspended-but-latent, with study functioning as readiness-maintenance rather than present fulfillment. All three share the same underlying textual kernel but diverge on whether/how the commandment is presently satisfied, producing three distinct ε values and three distinct classifications rather than one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
