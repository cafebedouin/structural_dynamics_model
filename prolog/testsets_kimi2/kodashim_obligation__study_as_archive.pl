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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: kodashim_obligation__study_as_archive
 *   human_readable: Kodashim Study as Historical Archive and Identity Maintenance
 *   domain: religious_studies/jewish_law
 *
 * SUMMARY:
 *   This constraint is the study_as_archive reading of the
 *   kodashim_obligation kernel. It treats the Order of Kodashimâthe
 *   Talmudic corpus governing Temple sacrifices and priestly lawâas a
 *   defunct system whose study serves historical preservation and communal
 *   identity maintenance rather than legal obligation or cosmic function.
 *   Within traditional yeshiva curricula, Kodashim remains a compulsory
 *   object of study, diverting intellectual resources from applicable law and
 *   generating legitimacy for the institutions that administer the
 *   comprehensive canon. Sibling readings (study_as_performance,
 *   study_as_preparation) instantiate structurally distinct constraints from
 *   the same textual kernel.
 *
 * KEY AGENTS:
 *   - yeshiva_network: Primary agenda-setter and beneficiary (institutional/constrained) â administers the curriculum and captures legitimacy from comprehensive study.
 *   - talmudic_students: Primary target (powerless/identity_locked) â bear the extraction of their intellectual labor and career time.
 *   - lay_observant_community: Secondary target (powerless/constrained) â bears diffuse costs of a rabbinic class trained in defunct law.
 *   - academic_jewish_studies: Analytical observer (analytical/analytical) â sees the constraint as historical object rather than normative commitment.
 *   - progressive_halakhic_circles: Excluded voice (moderate/constrained) â advocates curricular reform but is kept out of authority.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_obligation__study_as_archive, 0.45).
domain_priors:suppression_score(kodashim_obligation__study_as_archive, 0.38).
domain_priors:theater_ratio(kodashim_obligation__study_as_archive, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, extractiveness, 0.45).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_archive, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_obligation__study_as_archive, tangled_rope).
narrative_ontology:human_readable(kodashim_obligation__study_as_archive, "Kodashim Study as Historical Archive and Identity Maintenance").
narrative_ontology:topic_domain(kodashim_obligation__study_as_archive, "religious_studies/jewish_law").

domain_priors:requires_active_enforcement(kodashim_obligation__study_as_archive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_obligation__study_as_archive, '7cce3748-2779-4e18-a85b-ec5620dc5ca6').
narrative_ontology:cs_kernel_codification('7cce3748-2779-4e18-a85b-ec5620dc5ca6', fixed_text).
narrative_ontology:cs_authority_grounding('7cce3748-2779-4e18-a85b-ec5620dc5ca6', lineage).
narrative_ontology:cs_interpretation_layer_present('7cce3748-2779-4e18-a85b-ec5620dc5ca6').
narrative_ontology:cs_reading_relation('7cce3748-2779-4e18-a85b-ec5620dc5ca6', kodashim_obligation__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('7cce3748-2779-4e18-a85b-ec5620dc5ca6', kodashim_obligation__study_as_preparation, forecloses).
narrative_ontology:cs_axiom('7cce3748-2779-4e18-a85b-ec5620dc5ca6', foundational, kodashim_archive_not_cosmic).
narrative_ontology:cs_axiom_status(kodashim_archive_not_cosmic, holdable).
narrative_ontology:cs_axiom_grounding('7cce3748-2779-4e18-a85b-ec5620dc5ca6', kodashim_archive_not_cosmic, conventional).
narrative_ontology:cs_axiom('7cce3748-2779-4e18-a85b-ec5620dc5ca6', foundational, temple_restoration_undesired).
narrative_ontology:cs_axiom_status(temple_restoration_undesired, holdable).
narrative_ontology:cs_axiom_grounding('7cce3748-2779-4e18-a85b-ec5620dc5ca6', temple_restoration_undesired, deontological).
narrative_ontology:cs_reference_frame('7cce3748-2779-4e18-a85b-ec5620dc5ca6', historical_archive_framework).
narrative_ontology:cs_drift_state('7cce3748-2779-4e18-a85b-ec5620dc5ca6', contemporary_yeshiva_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('7cce3748-2779-4e18-a85b-ec5620dc5ca6', '').
narrative_ontology:cs_kernel_id(kodashim_obligation__study_as_archive, kodashim_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_archive, yeshiva_network).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_archive, talmudic_students).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_archive, lay_observant_community).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_archive, full_corpus_normative_equality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the traditional Talmudic curriculum and sets the cycle of tractate study. Derives institutional legitimacy and communal prestige from maintaining comprehensive study of the entire Talmud, including the defunct sacrificial order. Could in principle reallocate curricular emphasis but would risk undermining its authority as guardian of the complete tradition.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, yeshiva_network, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(kodashim_obligation__study_as_archive, yeshiva_network, beneficiary).

% Devote years of cognitive labor to mastering Kodashim tractates as a requirement of advanced Talmudic study. Their time and attention are diverted from contemporary applicable law. Exit means abandoning the identity of the comprehensive Talmudist and the social standing it confers within the yeshiva world.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, talmudic_students, payer,
    powerless, biographical, identity_locked, national).

% Bears the diffuse cost of a rabbinic class trained disproportionately in defunct law rather than in the civil, dietary, and family law that governs their daily religious lives. They do not set curricula and have no direct voice in yeshiva allocation decisions.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, lay_observant_community, payer,
    powerless, biographical, constrained, national).

% Studies Kodashim as historical source material for ancient Israelite religion. They observe the yeshiva curricular constraint from outside the normative commitment, treating it as an object of historiographical analysis rather than a binding obligation.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, academic_jewish_studies, observer,
    analytical, civilizational, analytical, global).

% Advocate for reallocating Talmudic study toward tractates with direct contemporary legal application. They are structurally excluded from traditional yeshiva curriculum councils and their proposals are treated as external to the normative tradition.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_archive, progressive_halakhic_circles, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_obligation__study_as_archive, yeshiva_network).
narrative_ontology:fixing_cost_class(kodashim_obligation__study_as_archive, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves textual continuity and communal memory of the defunct Temple sacrificial system across a generational rupture, solving the coordination problem of maintaining group identity after the loss of its central cultic institution.
% TRANSFER_FUNCTION: Moves intellectual labor from applicable contemporary law to archival defunct law; moves rabbinic prestige and institutional legitimacy from legal applicability to comprehensive textual mastery.
% ABSENT_VOICES: Progressive halakhists seeking applied-law curriculum reform, academic historians who treat Kodashim as antiquity rather than living archive, and messianic voices arguing for technical preservation are structurally excluded from yeshiva curriculum authority.
% DISAPPEARANCE_RATIONALE: If the obligation to study Kodashim vanished, yeshiva curricula would reallocate toward Nashim, Nezikin, and contemporary applied law; the symbolic claim of the entire Talmud's equal normative status would weaken; communal identity would shift away from comprehensive textual continuity as a core value.
% FOUNDING_PROBLEM: The destruction of the Second Temple and the cessation of sacrifices created a rupture in which the sacrificial laws became unperformable; the community needed a mechanism to preserve the textual corpus and maintain continuity with the pre-destruction normative order.
% FOUNDING_PROBLEM_CORROBORATION: Academic historians of Judaism corroborate that the textual preservation crisis was resolved by the redaction of the Mishnah and Talmud centuries ago; the continued curricular emphasis is defended by yeshiva authorities but external Jewish studies scholars attest the arrangement now serves identity-maintenance rather than preservation.
narrative_ontology:disappearance_verdict(kodashim_obligation__study_as_archive, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_obligation__study_as_archive, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_obligation__study_as_archive, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kodashim_obligation__study_as_archive, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_obligation__study_as_archive, 0.45, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_obligation__study_as_archive_tests).
:- end_tests(kodashim_obligation__study_as_archive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because the curricular obligation diverts substantial intellectual resources from applicable law without producing functional legal output, while generating concentrated legitimacy for the administering institutions. Suppression is moderate (0.38) because persistence depends on active curricular enforcement and identity-lock within the yeshiva system, not on natural necessity. Theater ratio is moderate (0.40) because a growing share of Kodashim study is performative maintenance of the claim that 'we study everything,' outstripping genuine historical inquiry. Accessibility collapse is high (0.65) because within the traditional yeshiva framework, the alternative of simply not studying Kodashim is nearly unthinkable. Resistance is low (0.20) because modernizing voices exist but are marginalized and excluded from curriculum authority.
 *
 * PERSPECTIVAL GAP:
 *   The yeshiva network experiences the constraint as legitimate identity-maintenance and authority-preservation; the student and lay payer seats experience it as resource diversion and opportunity cost. The engine computes this divergence from the structural asymmetry in beneficiary/victim declarations and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The yeshiva_network is the structural beneficiary of the legitimacy extracted by comprehensive study (low d). Talmudic_students and lay_observant_community are the structural victims whose resources are diverted (high d, amplified by identity_locked and constrained exit respectively). Academic observers sit at the analytical pole. Progressive circles are excluded from the conversation and do not feed directionality derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâpreserving the sacrificial legal corpus after the Temple's destructionâwas solved by the redaction of the Mishnah and Talmud centuries ago. The arrangement persists beyond its original preservation function. Classifying as tangled_rope prevents mislabeling the constraint as pure rope (which would ignore the ongoing resource diversion) or pure snare (which would deny the genuine communal identity function). The archive reading acknowledges the coordination value of memory while tracking the asymmetric extraction it sustains.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kodashim_legal_vitality,
    'Does Kodashim retain latent legal authority such that its study maintains applicable jurisprudential capacity, or is it fully defunct?',
    'Close reading of contemporary responsa and halakhic literature to determine whether Kodashim concepts are treated as live legal sources or as sealed historical archive.',
    'If latent authority is found, the extraction metric should be adjusted downward and the coordination function reframed as legal maintenance; if fully defunct, the archive reading is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kodashim_legal_vitality, conceptual, 'Whether Kodashim study preserves live legal capacity or only historical memory.').

omega_variable(
    suppression_mechanism_curricular,
    'Is the measured suppression structural (explicit yeshiva curriculum requirements and gatekeeping) or internalized (students fuse identity with comprehensive Talmud study)?',
    'Post-exit trajectory analysis: if students who leave the yeshiva system continue to feel compelled to study Kodashim, suppression is partially internalized; if the compulsion drops sharply upon institutional exit, it is structural.',
    'If internalized, effective suppression exceeds the structural measure and the identity_locked exit option should carry heavier weight in directionality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_curricular, empirical, 'Structural versus internalized suppression mechanism in curricular enforcement.').

omega_variable(
    beneficiary_concentration,
    'Does the legitimacy extracted by Kodashim study diffuse across the entire observant community, or concentrate in the yeshiva and rabbinic establishment that administers the curriculum?',
    'Institutional ethnography and funding-flow analysis to determine whether prestige and material support accrue to specific yeshiva institutions or are evenly distributed.',
    'If concentrated, the constraint trends toward snare-like capture; if diffuse, the tangled_rope classification is reinforced with a more distributed beneficiary structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_concentration, empirical, 'Concentration of extracted legitimacy in administering institutions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_obligation__study_as_archive, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kodashim_archive_tr_t0, kodashim_obligation__study_as_archive, theater_ratio, 0, 0.1).
narrative_ontology:measurement(kodashim_archive_tr_t400, kodashim_obligation__study_as_archive, theater_ratio, 400, 0.18).
narrative_ontology:measurement(kodashim_archive_tr_t800, kodashim_obligation__study_as_archive, theater_ratio, 800, 0.25).
narrative_ontology:measurement(kodashim_archive_tr_t1200, kodashim_obligation__study_as_archive, theater_ratio, 1200, 0.32).
narrative_ontology:measurement(kodashim_archive_tr_t1600, kodashim_obligation__study_as_archive, theater_ratio, 1600, 0.37).
narrative_ontology:measurement(kodashim_archive_tr_t2000, kodashim_obligation__study_as_archive, theater_ratio, 2000, 0.4).

% Extraction over time
narrative_ontology:measurement(kodashim_archive_be_t0, kodashim_obligation__study_as_archive, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(kodashim_archive_be_t400, kodashim_obligation__study_as_archive, base_extractiveness, 400, 0.22).
narrative_ontology:measurement(kodashim_archive_be_t800, kodashim_obligation__study_as_archive, base_extractiveness, 800, 0.3).
narrative_ontology:measurement(kodashim_archive_be_t1200, kodashim_obligation__study_as_archive, base_extractiveness, 1200, 0.36).
narrative_ontology:measurement(kodashim_archive_be_t1600, kodashim_obligation__study_as_archive, base_extractiveness, 1600, 0.41).
narrative_ontology:measurement(kodashim_archive_be_t2000, kodashim_obligation__study_as_archive, base_extractiveness, 2000, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(kodashim_archive_su_t0, kodashim_obligation__study_as_archive, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(kodashim_archive_su_t400, kodashim_obligation__study_as_archive, suppression_requirement, 400, 0.18).
narrative_ontology:measurement(kodashim_archive_su_t800, kodashim_obligation__study_as_archive, suppression_requirement, 800, 0.24).
narrative_ontology:measurement(kodashim_archive_su_t1200, kodashim_obligation__study_as_archive, suppression_requirement, 1200, 0.3).
narrative_ontology:measurement(kodashim_archive_su_t1600, kodashim_obligation__study_as_archive, suppression_requirement, 1600, 0.34).
narrative_ontology:measurement(kodashim_archive_su_t2000, kodashim_obligation__study_as_archive, suppression_requirement, 2000, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_obligation__study_as_archive, identity_coordination).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_archive, study_as_performance).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_archive, study_as_preparation).

% DUAL FORMULATION NOTE:
% The kodashim_obligation kernel decomposes into three structurally distinct constraint stories: study_as_archive (historical preservation, moderate extraction), study_as_performance (cosmic efficacy, contested empirical premise), and study_as_preparation (messianic technical preservation, binding-but-unperformed framing). Each carries a different epsilon, beneficiary structure, and normative premise. They are not the same constraint viewed from different angles but competing instantiations of the same textual kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
