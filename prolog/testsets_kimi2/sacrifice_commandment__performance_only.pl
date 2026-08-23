% ============================================================================
% CONSTRAINT STORY: sacrifice_commandment__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_commandment__performance_only, []).

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
 *   constraint_id: sacrifice_commandment__performance_only
 *   human_readable: Performance-Only Reading of the Sacrifice Commandment
 *   domain: religious/halakhic/commitment_system
 *
 * SUMMARY:
 *   The sacrifice_commandment kernel concerns how Jewish law treats the
 *   biblical commandment of animal sacrifice after the destruction of the
 *   Second Temple in 70 CE. This constraint instantiates the performance_only
 *   reading: the commandment requires physical execution and is suspended,
 *   not fulfilled by study, in the absence of the Temple. Despite this formal
 *   suspension, the traditional rabbinic curriculum dedicates massive
 *   scholarly resources to mastering these unperformable laws, diverting
 *   attention from living halakhic questions. The constraint is authored as a
 *   tangled rope because it carries genuine coordination value in preserving
 *   textual continuity, yet it asymmetrically extracts rabbinic training
 *   capacity from contemporary communal needs. This is one reading of a
 *   three-way contested kernel; sibling readings treat study as fulfillment
 *   or as archive maintenance.
 *
 * KEY AGENTS:
 *   - rabbinic_institutions: Primary agenda-setter (institutional/constrained) â administer curriculum and ordination standards that enforce the full corpus
 *   - talmudic_students: Primary payer (powerless/constrained) â bear the cost of diverted training time and opportunity
 *   - contemporary_communities: Secondary payer (moderate/constrained) â receive less applied halakhic attention because scholar training is absorbed by archaic material
 *   - temple_advocacy_groups: Beneficiary (organized/mobile) â gain a technically trained scholarly corps for prospective restoration
 *   - reform_halakhic_movements: Excluded voice (moderate/mobile) â advocate curriculum reform but are marginalized in traditional settings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_commandment__performance_only, 0.84).
domain_priors:suppression_score(sacrifice_commandment__performance_only, 0.72).
domain_priors:theater_ratio(sacrifice_commandment__performance_only, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, extractiveness, 0.84).
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, theater_ratio, 0.75).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_commandment__performance_only, tangled_rope).
narrative_ontology:human_readable(sacrifice_commandment__performance_only, "Performance-Only Reading of the Sacrifice Commandment").
narrative_ontology:topic_domain(sacrifice_commandment__performance_only, "religious/halakhic/commitment_system").

domain_priors:requires_active_enforcement(sacrifice_commandment__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_commandment__performance_only, '599aa494-950f-4632-a7a3-8a844a83f608').
narrative_ontology:cs_kernel_codification('599aa494-950f-4632-a7a3-8a844a83f608', fixed_text).
narrative_ontology:cs_authority_grounding('599aa494-950f-4632-a7a3-8a844a83f608', lineage).
narrative_ontology:cs_interpretation_layer_present('599aa494-950f-4632-a7a3-8a844a83f608').
narrative_ontology:cs_reading_relation('599aa494-950f-4632-a7a3-8a844a83f608', sacrifice_commandment__study_as_performance, forecloses).
narrative_ontology:cs_reading_relation('599aa494-950f-4632-a7a3-8a844a83f608', sacrifice_commandment__archive_maintenance, coexists_with).
narrative_ontology:cs_axiom('599aa494-950f-4632-a7a3-8a844a83f608', foundational, physical_execution_essential_to_sacrifice).
narrative_ontology:cs_axiom_status(physical_execution_essential_to_sacrifice, holdable).
narrative_ontology:cs_axiom_grounding('599aa494-950f-4632-a7a3-8a844a83f608', physical_execution_essential_to_sacrifice, theological).
narrative_ontology:cs_axiom('599aa494-950f-4632-a7a3-8a844a83f608', foundational, temple_absence_suspends_not_transmutes).
narrative_ontology:cs_axiom_status(temple_absence_suspends_not_transmutes, holdable).
narrative_ontology:cs_axiom_grounding('599aa494-950f-4632-a7a3-8a844a83f608', temple_absence_suspends_not_transmutes, theological).
narrative_ontology:cs_reference_frame('599aa494-950f-4632-a7a3-8a844a83f608', temple_centric_torah_obedience).
narrative_ontology:cs_drift_state('599aa494-950f-4632-a7a3-8a844a83f608', contemporary_diaspora, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('599aa494-950f-4632-a7a3-8a844a83f608', '').
narrative_ontology:cs_kernel_id(sacrifice_commandment__performance_only, sacrifice_commandment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_commandment__performance_only, rabbinic_institutions).
narrative_ontology:constraint_beneficiary(sacrifice_commandment__performance_only, temple_advocacy_groups).
narrative_ontology:constraint_victim(sacrifice_commandment__performance_only, talmudic_students).
narrative_ontology:constraint_victim(sacrifice_commandment__performance_only, contemporary_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the yeshiva curriculum and ordination requirements that mandate mastery of Temple sacrifice law across generations. They transmit the full Talmudic corpus and derive authority from comprehensive textual expertise. Their ability to drop Temple-law requirements is limited by the traditional expectation that a qualified rabbi must know the entire Torah, including the unperformable sections.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, rabbinic_institutions, agenda_setter,
    institutional, generational, constrained, global).

% Spend years of study on tractates concerning Temple sacrifices, ritual purity, and priestly procedure that have no physical application in the absence of the Temple. This study is required for ordination and peer standing, displacing time that could otherwise go to contemporary family law, economic ethics, or applied adjudication.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, talmudic_students, payer,
    powerless, biographical, constrained, local).

% Rely on rabbinic graduates for halakhic guidance on daily questions of kashrut, family law, and business ethics. They receive less scholarly innovation and applied attention on these living issues because the training system prioritizes mastery of archaic material.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, contemporary_communities, payer,
    moderate, biographical, constrained, national).

% Seek to maintain readiness for messianic Temple restoration. They benefit from a scholarly class trained in the precise technical details of sacrificial procedure, which the performance-only reading preserves as technical law rather than spiritualizing into abstract study.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, temple_advocacy_groups, beneficiary,
    organized, generational, mobile, national).

% Argue that rabbinic training should prioritize applicable law, contemporary ethics, and pastoral skill over unperformable Temple rituals. They are excluded from curriculum-setting bodies in traditional yeshiva networks and their proposals are treated as outside the discourse.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, reform_halakhic_movements, excluded,
    moderate, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_commandment__performance_only, rabbinic_institutions).
narrative_ontology:fixing_cost_class(sacrifice_commandment__performance_only, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves technical textual knowledge of the sacrificial cult across generations in the absence of a functioning Temple, maintaining rabbinic continuity and potential future operational readiness for a restored cult.
% TRANSFER_FUNCTION: Moves scholarly attention and training labor from living applicable law to the study of unperformable Temple rituals, transferring the cost of full-corpus maintenance to students and the communities they eventually serve.
% ABSENT_VOICES: Reform movements and contemporary applied ethicists who would redirect rabbinic training toward presently pressing legal and moral questions; they are excluded from traditional yeshiva curriculum committees and marginalized in halakhic discourse.
% DISAPPEARANCE_RATIONALE: If the performance-only reading and its curricular enforcement vanished, rabbinic training would restructure around living law, the social authority of Temple-law mastery would collapse, ordination standards would shift, and communities would receive adjudication shaped by contemporary rather than archaic expertise.
% FOUNDING_PROBLEM: The destruction of the Second Temple in 70 CE eliminated the physical site for biblical sacrifice, creating a crisis of practice: how to maintain the Torah's sacrificial commandments when the central cultic locus no longer exists.
% FOUNDING_PROBLEM_CORROBORATION: Academic historians of Judaism and non-Orthodox halakhic movements attest that the immediate post-destruction crisis has been superseded by 1,900 years of diaspora law, and that the present arrangement persists by institutional inertia rather than ongoing necessity. Traditional yeshiva authorities assert the problem remains live through messianic expectation. External corroboration from secular Jewish studies departments and reform seminaries supports the dead/inertial reading.
narrative_ontology:disappearance_verdict(sacrifice_commandment__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_commandment__performance_only, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_commandment__performance_only, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_commandment__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_commandment__performance_only, 0.84, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_commandment__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_commandment__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sacrifice_commandment__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.84) because roughly half of the Talmudic corpus concerns Temple practice with no physical outlet, displacing study of applied law. Suppression (0.72) reflects the curricular enforcement and social cost of challenging the canonical study requirement. Theater_ratio (0.75) is high because the study functions increasingly as a credentialing gate and identity performance rather than practical preparation. Accessibility_collapse (0.45) is moderate: alternatives (reformed curricula) exist in non-Orthodox settings but are institutionally marginalized within the traditional yeshiva world. Resistance (0.55) reflects ongoing debate in modern Orthodoxy and robust criticism from non-Orthodox movements. The claim/metric independence is maintained: the reading is claimed as tangled_rope because genuine coordination (preservation, continuity) coexists with extraction, but the metrics describe heavily extractive operation.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinic institutional seat, the curriculum maintains the integrity of the Torah and ensures a ready expert class for a restored Temple; from the student and contemporary community seats, the same structure operates as a massive displacement of practical legal attention onto ceremonial archaeology. The engine will compute different per-seat classifications from these structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   rabbinic_institutions sit near the beneficiary end: they collect authority, continuity, and gatekeeping power from maintenance of the full corpus. talmudic_students sit near the target end: they bear the cost of diverted attention and opportunity. temple_advocacy_groups are incidental beneficiaries. contemporary_communities are diffuse targets. The primary asymmetry is institutional power over student time and communal adjudicative output.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling: pure coordination (rope) would ignore the 1,900-year diversion of scholarly labor from living questions; pure extraction (snare) would ignore the genuine preservation function and the sincere commitment of some communities to future Temple readiness. The founding problem (Temple destruction) is ancient, and the persistence of intensive study long after suspension suggests the coordination story has partially atrophied into institutional habit, though not entirely into piton because identifiable beneficiaries still collect from the structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    temple_law_study_justification,
    'Is the intensive study of unperformable Temple law justified by genuine coordination needs (messianic preparedness, textual integrity) or by institutional inertia and gatekeeping?',
    'Comparative analysis of rabbinic training outcomes across denominations: if communities without Temple-law requirements produce equally competent living-law adjudicators, the coordination need is overstated.',
    'If justified, extraction is lower and the constraint approaches rope; if inertia, extraction is purer and the constraint approaches snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temple_law_study_justification, conceptual, 'Coordination justification vs institutional inertia for Temple-law study').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the persistence of Temple-law study enforced by structural curriculum barriers or by internalized identity fusion with rabbinic tradition?',
    'Post-exit trajectory analysis: do scholars who leave the traditional yeshiva system continue to value Temple-law study, or do they redirect attention immediately toward living law?',
    'If internalized, effective suppression exceeds structural measures; the constraint operates at the identity level and removal of curricular requirements would not immediately change behavior.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').

omega_variable(
    kernel_reading_contest,
    'This constraint is the performance_only reading of the sacrifice_commandment kernel. How would classification change if the study_as_performance reading were adopted as dominant?',
    'Compare the effective extraction under each reading: study_as_performance would redirect the extraction metric toward the cognitive burden of study rather than the displacement of living law, potentially lowering effective extraction because the payer becomes simultaneous beneficiary (fulfillment).',
    'Under study_as_performance, the same curriculum might compute as rope or scaffold rather than tangled rope, because the student seat would receive symmetric value (study as worship).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Classification sensitivity to sibling reading adoption').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_commandment__performance_only, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacrifice_performance_only_tr_t0, sacrifice_commandment__performance_only, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sacrifice_performance_only_tr_t20, sacrifice_commandment__performance_only, theater_ratio, 20, 0.25).
narrative_ontology:measurement(sacrifice_performance_only_tr_t40, sacrifice_commandment__performance_only, theater_ratio, 40, 0.4).
narrative_ontology:measurement(sacrifice_performance_only_tr_t60, sacrifice_commandment__performance_only, theater_ratio, 60, 0.55).
narrative_ontology:measurement(sacrifice_performance_only_tr_t80, sacrifice_commandment__performance_only, theater_ratio, 80, 0.68).
narrative_ontology:measurement(sacrifice_performance_only_tr_t100, sacrifice_commandment__performance_only, theater_ratio, 100, 0.75).

% Extraction over time
narrative_ontology:measurement(sacrifice_performance_only_be_t0, sacrifice_commandment__performance_only, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(sacrifice_performance_only_be_t20, sacrifice_commandment__performance_only, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(sacrifice_performance_only_be_t40, sacrifice_commandment__performance_only, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(sacrifice_performance_only_be_t60, sacrifice_commandment__performance_only, base_extractiveness, 60, 0.68).
narrative_ontology:measurement(sacrifice_performance_only_be_t80, sacrifice_commandment__performance_only, base_extractiveness, 80, 0.78).
narrative_ontology:measurement(sacrifice_performance_only_be_t100, sacrifice_commandment__performance_only, base_extractiveness, 100, 0.84).

% Suppression requirement over time
narrative_ontology:measurement(sacrifice_performance_only_su_t0, sacrifice_commandment__performance_only, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(sacrifice_performance_only_su_t20, sacrifice_commandment__performance_only, suppression_requirement, 20, 0.35).
narrative_ontology:measurement(sacrifice_performance_only_su_t40, sacrifice_commandment__performance_only, suppression_requirement, 40, 0.5).
narrative_ontology:measurement(sacrifice_performance_only_su_t60, sacrifice_commandment__performance_only, suppression_requirement, 60, 0.6).
narrative_ontology:measurement(sacrifice_performance_only_su_t80, sacrifice_commandment__performance_only, suppression_requirement, 80, 0.7).
narrative_ontology:measurement(sacrifice_performance_only_su_t100, sacrifice_commandment__performance_only, suppression_requirement, 100, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_commandment__performance_only, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_commandment__performance_only, sacrifice_commandment__study_as_performance).
narrative_ontology:affects_constraint(sacrifice_commandment__performance_only, sacrifice_commandment__archive_maintenance).

% DUAL FORMULATION NOTE:
% The sacrifice_commandment kernel decomposes into three structurally distinct constraints under the Îµ-invariance principle: performance_only (this file, high extraction via suspended-commandment study), study_as_performance (study fulfills the commandment, reducing extraction by making study symmetric), and archive_maintenance (future-oriented preservation). Each carries its own Îµ, stakeholders, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
