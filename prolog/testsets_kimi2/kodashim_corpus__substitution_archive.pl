% ============================================================================
% CONSTRAINT STORY: kodashim_corpus__substitution_archive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_corpus__substitution_archive, []).

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
 *   constraint_id: kodashim_corpus__substitution_archive
 *   human_readable: Kodashim Corpus as Substitution Archive
 *   domain: religious/rabbinic_judaism
 *
 * SUMMARY:
 *   The Kodashim corpusâMishnah and Talmud tractates devoted to sacrificial
 *   lawâpersists in Rabbinic Judaism long after the destruction of the
 *   Second Temple rendered its prescriptions inoperable. In the
 *   substitution_archive reading, this corpus is not a blueprint for future
 *   restoration nor a living performance via study, but a memorial archive
 *   documenting what prayer and Torah study have superseded. The constraint
 *   claims continuity with the sacrificial past while structurally denying
 *   its return, concentrating religious authority in rabbinic academies that
 *   curate the archive. This reading instantiates one pole of a three-way
 *   contested kernel about the ontological status of Jewish sacrificial law.
 *
 * KEY AGENTS:
 *   - Rabbinic text-study institutions: Agenda-setter and beneficiary (institutional/identity_locked/global) â administer the archive and derive curricular centrality from it.
 *   - Restorationist practitioners: Payer (moderate/constrained/national) â bear the cost of exclusion from sacrificial practice under the substitution doctrine.
 *   - Critical religious studies scholars: Observer (analytical/analytical/global) â external analysts tracking the archive's legitimizing function.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_corpus__substitution_archive, 0.58).
domain_priors:suppression_score(kodashim_corpus__substitution_archive, 0.65).
domain_priors:theater_ratio(kodashim_corpus__substitution_archive, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, extractiveness, 0.58).
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_corpus__substitution_archive, tangled_rope).
narrative_ontology:human_readable(kodashim_corpus__substitution_archive, "Kodashim Corpus as Substitution Archive").
narrative_ontology:topic_domain(kodashim_corpus__substitution_archive, "religious/rabbinic_judaism").

domain_priors:requires_active_enforcement(kodashim_corpus__substitution_archive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_corpus__substitution_archive, 'd0597cd3-8b13-4666-8128-827272fdd58e').
narrative_ontology:cs_kernel_codification('d0597cd3-8b13-4666-8128-827272fdd58e', fixed_text).
narrative_ontology:cs_authority_grounding('d0597cd3-8b13-4666-8128-827272fdd58e', lineage).
narrative_ontology:cs_interpretation_layer_present('d0597cd3-8b13-4666-8128-827272fdd58e').
narrative_ontology:cs_reading_relation('d0597cd3-8b13-4666-8128-827272fdd58e', kodashim_corpus__study_as_exercise, forecloses).
narrative_ontology:cs_reading_relation('d0597cd3-8b13-4666-8128-827272fdd58e', kodashim_corpus__performance_only, forecloses).
narrative_ontology:cs_axiom('d0597cd3-8b13-4666-8128-827272fdd58e', foundational, sacrifice_permanently_superseded_by_prayer_and_study).
narrative_ontology:cs_axiom_status(sacrifice_permanently_superseded_by_prayer_and_study, holdable).
narrative_ontology:cs_axiom_grounding('d0597cd3-8b13-4666-8128-827272fdd58e', sacrifice_permanently_superseded_by_prayer_and_study, theological).
narrative_ontology:cs_axiom('d0597cd3-8b13-4666-8128-827272fdd58e', foundational, kodashim_is_memorial_not_manual).
narrative_ontology:cs_axiom_status(kodashim_is_memorial_not_manual, holdable).
narrative_ontology:cs_axiom_grounding('d0597cd3-8b13-4666-8128-827272fdd58e', kodashim_is_memorial_not_manual, conventional).
narrative_ontology:cs_reference_frame('d0597cd3-8b13-4666-8128-827272fdd58e', post_temple_substitution_framework).
narrative_ontology:cs_drift_state('d0597cd3-8b13-4666-8128-827272fdd58e', modern_yeshiva_era, gap(repudiation_pressure, minor, false)).
narrative_ontology:cs_created_at('d0597cd3-8b13-4666-8128-827272fdd58e', '').
narrative_ontology:cs_kernel_id(kodashim_corpus__substitution_archive, kodashim_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_corpus__substitution_archive, rabbinic_text_study_institutions).
narrative_ontology:constraint_victim(kodashim_corpus__substitution_archive, restorationist_practitioners).
narrative_ontology:constraint_vindicates(kodashim_corpus__substitution_archive, prayer_substitution_doctrine).
narrative_ontology:constraint_vindicates(kodashim_corpus__substitution_archive, torah_study_efficacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the interpretive tradition that frames prayer and Torah study as functional replacements for Temple sacrifice. Curates the Kodashim corpus as a textual archive rather than an operational manual, deriving institutional authority, curricular centrality, and continuity claims from the assertion that study memorializes and supersedes the altar.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, rabbinic_text_study_institutions, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(kodashim_corpus__substitution_archive, rabbinic_text_study_institutions, beneficiary).

% Seek to perform or restore physical sacrificial worship in accordance with Torah commandments. Are blocked by the absence of the Temple and by the rabbinic doctrine that prayer and study have permanently superseded sacrifice. Within the rabbinic framework their theological position is treated as obsolete or transgressive; their exit is constrained by religious identity and physical barriers.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, restorationist_practitioners, payer,
    moderate, biographical, constrained, national).

% Analyze the Kodashim corpus and rabbinic substitution theology from an external historiographical perspective, tracing how the archive functions to legitimize post-Temple Judaism while preserving the memory of the sacrificial system it replaced.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, critical_religious_studies_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_corpus__substitution_archive, rabbinic_text_study_institutions).
narrative_ontology:fixing_cost_class(kodashim_corpus__substitution_archive, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates post-Temple Jewish religious life by providing a textual framework that channels the commanded sacrificial impulse into prayer and Torah study, maintaining communal cohesion and covenantal identity after the destruction of the central cultic site.
% TRANSFER_FUNCTION: Moves religious authority, curricular focus, and the locus of valid worship from the Temple priesthood and altar service to rabbinic academies and the liturgical-textual sphere; transfers the status of Torah-obedience from blood-offering to verbal-cognitive practice.
% ABSENT_VOICES: Priestly lineages seeking to restore sacrificial jurisdiction, Jewish sects regarding rabbinic substitution as a departure from Torah commandment, and indigenous Near Eastern sacrificial traditions are excluded from normative conversation by the archive's framing of sacrifice as a superseded epoch.
% DISAPPEARANCE_RATIONALE: If the Kodashim archive were removed from the living curriculum and the substitution doctrine dissolved, the rabbinic justification for prayer-as-replacement would lose its primary textual anchor. Restorationist movements would gain immediate legitimacy, and the boundary between obsolete and temporarily suspended sacrifice would become the central theological battleground.
% FOUNDING_PROBLEM: The destruction of the Second Temple in 70 CE eliminated the central site and priesthood for Torah-mandated sacrifice, creating a crisis of continuity for Jewish practice and cosmology.
% FOUNDING_PROBLEM_CORROBORATION: Secular historians of Second Temple Judaism and archaeologists attest to the destruction event and the subsequent liturgical shift. Non-rabbinic Jewish movements and certain Temple Mount activist groups outside the academy corroborate that the problem of post-Temple practice was real, though they dispute that rabbinic substitution is the legitimate resolution.
narrative_ontology:disappearance_verdict(kodashim_corpus__substitution_archive, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_corpus__substitution_archive, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_corpus__substitution_archive, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kodashim_corpus__substitution_archive, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_corpus__substitution_archive, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_corpus__substitution_archive_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kodashim_corpus__substitution_archive, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kodashim_corpus__substitution_archive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58) because the archive channels genuine religious energy into study and prayer while simultaneously extracting authority from the memory of sacrifice for institutions that no longer perform it. Suppression is substantial (0.65) because the constraint's persistence depends on active rabbinic enforcement of the supersession doctrine against restorationist movements. Theater is moderate (0.45): the archival function is real, but an increasing share of curricular activity performs continuity rather than functional necessity. Accessibility collapse is high (0.72) because once the substitution framework is accepted, restoration appears theologically incoherent; resistance is moderate-low (0.35) because restorationists are visible but politically and theologically marginalized within mainstream Judaism.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences the archive as a necessary and pious response to catastrophe, a preservation of memory that prevents loss. The payer seat experiences the same structure as an institutional block on commanded practice, a theological coup that replaces altar with academy. The engine computes this divergence from the structural data: identical text, opposite directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic institutions are the declared beneficiary and agenda-setter; their identity-locked exit and global scope damp their effective extraction to near-zero or negative (subsidized by the constraint). Restorationist practitioners are the declared victims; their constrained exit and national scope amplify their effective extraction. The divergence is driven by beneficiary/victim role plus exit asymmetry, not by metric tuning.
 *
 * MANDATROPHY ANALYSIS:
 *   The R5 genealogy identifies the founding problem as the Temple's destruction. The status is contested because restorationist movements argue the problem demands rebuilding, not substitution. The mismatch between contested status and world_rearranges disappearance verdict signals that the archive may have partially atrophied from its original coordination function into a structure that now primarily extracts authority from a dead past. However, the active enforcement and genuine coordination of post-Temple Jewish life prevent pure piton classification; the result is tangled rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_occupancy_ambiguity,
    'Is the Kodashim corpus a living legal framework awaiting reactivation, or a memorial archive marking a permanently superseded practice?',
    'Comparative analysis of halakhic responsa and curricular practice against historical-critical scholarship; detection of performative versus descriptive language in the text''s traditional reception.',
    'If the kernel is occupied, the constraint is a scaffold or rope for future restoration; if it is an archive, the constraint is a tangled rope where substitution claims obscure the extraction of authority from a superseded past.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_occupancy_ambiguity, conceptual, 'Ambiguity about whether the text is active manual or memorial archive').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the block on sacrificial practice primarily structural (absence of Temple, political barriers) or internalized (theological acceptance of supersession)?',
    'Observation of restorationist group behavior when structural barriers are lowered, such as access to the Temple Mount under different political conditions.',
    'If internalized, the constraint''s effective suppression exceeds its structural measure because the target carries the prohibition even after external barriers are removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism').

omega_variable(
    substitution_continuity_sincerity,
    'Does the rabbinic institution genuinely experience the substitution as continuity, or is the continuity claim a legitimizing narrative for a novel post-Temple formation?',
    'Historical-source analysis of early rabbinic literature for anxiety markers about the substitution; sociological study of rabbinic identity formation.',
    'If the continuity claim is sincerely held, the coordination component is stronger and extraction may be incidental; if strategic, the constraint leans toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(substitution_continuity_sincerity, conceptual, 'Sincerity of continuity claim versus legitimizing narrative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_corpus__substitution_archive, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_corpus__substitution_archive, theater_ratio, 0, 0.2).
narrative_ontology:measurement(koda_tr_t400, kodashim_corpus__substitution_archive, theater_ratio, 400, 0.3).
narrative_ontology:measurement(koda_tr_t800, kodashim_corpus__substitution_archive, theater_ratio, 800, 0.38).
narrative_ontology:measurement(koda_tr_t1200, kodashim_corpus__substitution_archive, theater_ratio, 1200, 0.42).
narrative_ontology:measurement(koda_tr_t1600, kodashim_corpus__substitution_archive, theater_ratio, 1600, 0.44).
narrative_ontology:measurement(koda_tr_t2000, kodashim_corpus__substitution_archive, theater_ratio, 2000, 0.45).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_corpus__substitution_archive, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(koda_be_t400, kodashim_corpus__substitution_archive, base_extractiveness, 400, 0.35).
narrative_ontology:measurement(koda_be_t800, kodashim_corpus__substitution_archive, base_extractiveness, 800, 0.42).
narrative_ontology:measurement(koda_be_t1200, kodashim_corpus__substitution_archive, base_extractiveness, 1200, 0.48).
narrative_ontology:measurement(koda_be_t1600, kodashim_corpus__substitution_archive, base_extractiveness, 1600, 0.52).
narrative_ontology:measurement(koda_be_t2000, kodashim_corpus__substitution_archive, base_extractiveness, 2000, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_corpus__substitution_archive, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(koda_su_t400, kodashim_corpus__substitution_archive, suppression_requirement, 400, 0.45).
narrative_ontology:measurement(koda_su_t800, kodashim_corpus__substitution_archive, suppression_requirement, 800, 0.55).
narrative_ontology:measurement(koda_su_t1200, kodashim_corpus__substitution_archive, suppression_requirement, 1200, 0.6).
narrative_ontology:measurement(koda_su_t1600, kodashim_corpus__substitution_archive, suppression_requirement, 1600, 0.62).
narrative_ontology:measurement(koda_su_t2000, kodashim_corpus__substitution_archive, suppression_requirement, 2000, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_corpus__substitution_archive, identity_coordination).
narrative_ontology:affects_constraint(kodashim_corpus__substitution_archive, kodashim_corpus__study_as_exercise).
narrative_ontology:affects_constraint(kodashim_corpus__substitution_archive, kodashim_corpus__performance_only).

% DUAL FORMULATION NOTE:
% The kodashim_corpus kernel decomposes into three structurally distinct constraints. This substitution_archive reading treats the corpus as memorializing a superseded practice; the study_as_exercise reading treats study as performative occupation; the performance_only reading treats the text as a blueprint for future restoration. Each reading has a distinct epsilon, beneficiary structure, and classification, linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
