% ============================================================================
% CONSTRAINT STORY: kodashim_obligation__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_obligation__study_as_performance, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: kodashim_obligation__study_as_performance
 *   human_readable: Kodashim Study as Cosmic Performance
 *   domain: religious/jewish_law/textual_preservation
 *
 * SUMMARY:
 *   This constraint instantiates the study_as_performance reading of the
 *   kodashim_obligation kernel. In Jewish law and mysticism, the tractates
 *   concerning sacrifices (Kodashim) continue to be studied and legislated
 *   despite the absence of the Temple. This reading claims that the study of
 *   sacrificial law is not merely preparation for a future restoration nor a
 *   historical archive, but an active performance of cosmic repair. The
 *   constraint is structurally invariant: no human party extracts value, no
 *   victim set exists, and the Temple's physical absence is irrelevant to
 *   spiritual efficacy. The classification as mountain reflects the reading's
 *   own theological ontology: the law is a fixed feature of divine creation,
 *   not a negotiable social arrangement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_obligation__study_as_performance, 0.02).
domain_priors:suppression_score(kodashim_obligation__study_as_performance, 0.0).
domain_priors:theater_ratio(kodashim_obligation__study_as_performance, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, extractiveness, 0.02).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_obligation__study_as_performance, mountain).
narrative_ontology:human_readable(kodashim_obligation__study_as_performance, "Kodashim Study as Cosmic Performance").
narrative_ontology:topic_domain(kodashim_obligation__study_as_performance, "religious/jewish_law/textual_preservation").

domain_priors:emerges_naturally(kodashim_obligation__study_as_performance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_obligation__study_as_performance, '62271f3c-4b82-4400-923f-c036d6b7e87d').
narrative_ontology:cs_kernel_codification('62271f3c-4b82-4400-923f-c036d6b7e87d', fixed_text).
narrative_ontology:cs_authority_grounding('62271f3c-4b82-4400-923f-c036d6b7e87d', lineage).
narrative_ontology:cs_interpretation_layer_present('62271f3c-4b82-4400-923f-c036d6b7e87d').
narrative_ontology:cs_reading_relation('62271f3c-4b82-4400-923f-c036d6b7e87d', kodashim_obligation__study_as_preparation, forecloses).
narrative_ontology:cs_reading_relation('62271f3c-4b82-4400-923f-c036d6b7e87d', kodashim_obligation__study_as_archive, forecloses).
narrative_ontology:cs_axiom('62271f3c-4b82-4400-923f-c036d6b7e87d', foundational, study_as_cosmic_performance).
narrative_ontology:cs_axiom_status(study_as_cosmic_performance, holdable).
narrative_ontology:cs_axiom_grounding('62271f3c-4b82-4400-923f-c036d6b7e87d', study_as_cosmic_performance, theological).
narrative_ontology:cs_axiom('62271f3c-4b82-4400-923f-c036d6b7e87d', foundational, material_temple_non_necessity).
narrative_ontology:cs_axiom_status(material_temple_non_necessity, holdable).
narrative_ontology:cs_axiom_grounding('62271f3c-4b82-4400-923f-c036d6b7e87d', material_temple_non_necessity, theological).
narrative_ontology:cs_reference_frame('62271f3c-4b82-4400-923f-c036d6b7e87d', torah_study_cosmic_performance).
narrative_ontology:cs_drift_state('62271f3c-4b82-4400-923f-c036d6b7e87d', post_temple_destruction_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('62271f3c-4b82-4400-923f-c036d6b7e87d', '').
narrative_ontology:cs_kernel_id(kodashim_obligation__study_as_performance, kodashim_obligation).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains cosmic order and spiritual equilibrium through the intellectual engagement with sacrificial law, substituting study for material sacrifice to preserve the universe's covenantal function.
% TRANSFER_FUNCTION: Transfers spiritual merit or cosmic energy from the act of study to the divine order; no material transfer occurs between human agents.
% ABSENT_VOICES: Those who hold that sacrifice requires physical performance in the Temple, and that study without ritual action is spiritually inert, are excluded from this reading's framework. Similarly, voices arguing that study is merely historical preservation or technical preparation for restoration are structurally absent from the performative reading.
% DISAPPEARANCE_RATIONALE: If the spiritual efficacy of Kodashim study vanished, the cosmic equilibrium maintained by this study would destabilize; the constraint is held to be a structural feature of divine creation, not a human arrangement that could disappear without cosmic consequence.
% FOUNDING_PROBLEM: The destruction of the Second Temple eliminated the material possibility of sacrificial worship, creating an apparent gap in the covenantal mechanism for maintaining cosmic order.
% FOUNDING_PROBLEM_CORROBORATION: Traditional rabbinic sources outside the narrow mystical tradition (e.g., Talmud Bavli Megillah 3b, Menachot 110a) attest to the shift toward study as a substitute for sacrifice in the post-Temple era, though they do not uniformly assert the full ontological identity of study and performance. Academic historians of religion corroborate the Temple destruction as the historical trigger for textualization of sacrificial law, though they do not attest to its spiritual efficacy.
narrative_ontology:disappearance_verdict(kodashim_obligation__study_as_performance, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_obligation__study_as_performance, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_obligation__study_as_performance, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kodashim_obligation__study_as_performance, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_obligation__study_as_performance, 0.02, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_obligation__study_as_performance_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, ExtMetricName, E),
    domain_priors:suppression_score(kodashim_obligation__study_as_performance, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(kodashim_obligation__study_as_performance),
    narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(kodashim_obligation__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near-zero because no human agent captures the transfer; the only beneficiary is cosmic order, which is not a rent-collecting party. Suppression is zero because the constraint operates without coercionânon-study does not trigger human enforcement, though theological consequence may be inferred. Theater ratio is minimal because the study is functionally constitutive of cosmic order, not performative maintenance of a hollowed institution. Accessibility collapse is high because, within this reading's framework, no alternative to Torah study maintains the cosmic function of sacrifice. Resistance is near-zero because the constraint is not contested by those who operate within its framework; contestation comes from outside readings (siblings), not from targeted agents.
 *
 * PERSPECTIVAL GAP:
 *   From within this reading, the constraint is a mountainâan unchangeable divine law. From the study_as_archive reading, the same textual practice is a rope or scaffold maintaining cultural identity. From the study_as_preparation reading, it is a rope coordinating present community toward future restoration. The engine computes these divergences from the sibling constraints' structural data; this story does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   No directionality overrides are necessary. There are no human beneficiaries or victims declared; the structural derivation chain finds no parties to assign directionality to. The constraint is self-grounding in divine authority and applies universally to those who engage it, without asymmetric extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resists mandatrophy classification because its mandate (study as cosmic performance) is identical to its ongoing function; there is no gap between founding purpose and current operation. The Temple's destruction did not obsolete the law but revealed its deeper operating mode. Mandatrophy would require the founding problem to be solved or the arrangement to persist theatrically after function lossâneither applies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    study_as_performance_reading_identity,
    'Is this constraint cleanly separable from its sibling readings of the kodashim_obligation kernel, such that its epsilon and structural claims do not collapse into study_as_preparation or study_as_archive under reinterpretation?',
    'Cross-reference with compiled sibling constraints to verify disjoint beneficiary/victim structures and non-overlapping epsilon ranges; inspect whether the same textual evidence is cited by multiple readings.',
    'If the readings are not structurally disjoint, the epsilon-invariance principle is violated and the kernel requires further decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(study_as_performance_reading_identity, conceptual, 'Kernel reading identity and structural separation verification').

omega_variable(
    temple_absence_efficacy_ambiguity,
    'Does the physical absence of the Temple render sacrificial law inert, or does study alone maintain cosmic efficacy regardless of material conditions?',
    'Textual analysis of Talmudic and mystical sources (e.g., Menachot 110a, Zohar) and comparison with historical-critical scholarship; no empirical resolution is possible outside the theological framework.',
    'If Temple absence negates efficacy, this reading collapses into study_as_archive or study_as_preparation; if study sustains efficacy independently, the mountain classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temple_absence_efficacy_ambiguity, conceptual, 'Whether spiritual efficacy is materially dependent on the Temple').

omega_variable(
    natural_law_vs_constructed_theology,
    'Is the claim that study enacts sacrifice a discovered divine constant, or a post-hoc rabbinic construction enabling institutional continuity after the Temple''s destruction?',
    'Historical analysis of the emergence of this doctrine in rabbinic and medieval mystical literature versus claims of Sinaitic origin.',
    'If historically constructed, the constraint is a false-summit mountain serving identity_coordination; if genuinely divine, it remains mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_theology, empirical, 'Divine natural law versus constructed theological continuity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_obligation__study_as_performance, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_obligation__study_as_performance, theater_ratio, 0, 0.05).
narrative_ontology:measurement(koda_tr_t2, kodashim_obligation__study_as_performance, theater_ratio, 2, 0.05).
narrative_ontology:measurement(koda_tr_t4, kodashim_obligation__study_as_performance, theater_ratio, 4, 0.05).
narrative_ontology:measurement(koda_tr_t6, kodashim_obligation__study_as_performance, theater_ratio, 6, 0.05).
narrative_ontology:measurement(koda_tr_t8, kodashim_obligation__study_as_performance, theater_ratio, 8, 0.05).
narrative_ontology:measurement(koda_tr_t10, kodashim_obligation__study_as_performance, theater_ratio, 10, 0.05).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_obligation__study_as_performance, base_extractiveness, 0, 0.02).
narrative_ontology:measurement(koda_be_t2, kodashim_obligation__study_as_performance, base_extractiveness, 2, 0.02).
narrative_ontology:measurement(koda_be_t4, kodashim_obligation__study_as_performance, base_extractiveness, 4, 0.02).
narrative_ontology:measurement(koda_be_t6, kodashim_obligation__study_as_performance, base_extractiveness, 6, 0.02).
narrative_ontology:measurement(koda_be_t8, kodashim_obligation__study_as_performance, base_extractiveness, 8, 0.02).
narrative_ontology:measurement(koda_be_t10, kodashim_obligation__study_as_performance, base_extractiveness, 10, 0.02).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_obligation__study_as_performance, suppression_requirement, 0, 0.0).
narrative_ontology:measurement(koda_su_t2, kodashim_obligation__study_as_performance, suppression_requirement, 2, 0.0).
narrative_ontology:measurement(koda_su_t4, kodashim_obligation__study_as_performance, suppression_requirement, 4, 0.0).
narrative_ontology:measurement(koda_su_t6, kodashim_obligation__study_as_performance, suppression_requirement, 6, 0.0).
narrative_ontology:measurement(koda_su_t8, kodashim_obligation__study_as_performance, suppression_requirement, 8, 0.0).
narrative_ontology:measurement(koda_su_t10, kodashim_obligation__study_as_performance, suppression_requirement, 10, 0.0).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(kodashim_obligation__study_as_performance, kodashim_obligation__study_as_preparation).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_performance, kodashim_obligation__study_as_archive).

% DUAL FORMULATION NOTE:
% The kodashim_obligation kernel decomposes into three structurally distinct constraints under the epsilon-invariance principle: study_as_archive (low extraction, identity coordination), study_as_preparation (moderate extraction, temporal deferral), and study_as_performance (zero extraction, cosmic function). Their epsilon values differ because they describe different ontological claims about the status of Torah study.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
