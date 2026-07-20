% ============================================================================
% CONSTRAINT STORY: quran_hadith_substrate__state_hybrid
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_hadith_substrate__state_hybrid, []).

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
 *   constraint_id: quran_hadith_substrate__state_hybrid
 *   human_readable: State Hybrid Sharia Instrumentalization
 *   domain: legal/political/religious
 *
 * SUMMARY:
 *   In numerous Muslim-majority states, the legal system selectively codifies
 *   classical fiqh rulingsâparticularly in family law (marriage, divorce,
 *   inheritance) and criminal law (hudud, qisas)âwhile simultaneously
 *   applying secular or reformist legal frameworks in commercial,
 *   administrative, and constitutional domains. This hybrid arrangement is
 *   not justified by hermeneutic fidelity to any single madhhab or by a
 *   consistent theory of legal change (such as maslaha or ijtihad). Instead,
 *   its legitimacy is openly or implicitly grounded in political sovereignty:
 *   the state claims the authority to decide which religious rulings enter
 *   positive law and which remain inoperative. State elites benefit from the
 *   arrangement by harvesting Islamic legitimacy from the family/criminal
 *   domain while preserving the economic and administrative flexibility
 *   required for global integration and regime stability. Traditionalist
 *   scholars bear the cost of truncationâtheir comprehensive vision of
 *   sharia governance is reduced to a narrow personal-status and penal
 *   residue. Reformist critics bear the cost of suppressionâtheir calls for
 *   consistent ethical re-interpretation threaten the regime's selective
 *   legitimacy and are silenced or excluded. The constraint is a Tangled
 *   Rope: it genuinely coordinates family and criminal disputes through
 *   recognizable classical mechanisms, but the same structure asymmetrically
 *   extracts political legitimacy and suppresses alternative jurisprudential
 *   readings.
 *
 * KEY AGENTS:
 *   - state_elites: Primary beneficiary/agenda_setter (institutional/constrained) â harvest legitimacy and preserve policy flexibility
 *   - regime_apparatus: Secondary beneficiary (institutional/constrained) â enforces the hybrid order
 *   - commercial_elites: Tertiary beneficiary (powerful/mobile) â gain secular commercial frameworks
 *   - traditionalist_scholars: Primary victim (organized/constrained) â comprehensive vision truncated
 *   - reformist_critics: Secondary victim (moderate/constrained) â critical readings suppressed
 *   - comparative_legal_scholars: Analytical observer (analytical/analytical) â studies the divergence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_hadith_substrate__state_hybrid, 0.36).
domain_priors:suppression_score(quran_hadith_substrate__state_hybrid, 0.55).
domain_priors:theater_ratio(quran_hadith_substrate__state_hybrid, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, extractiveness, 0.36).
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_hadith_substrate__state_hybrid, tangled_rope).
narrative_ontology:human_readable(quran_hadith_substrate__state_hybrid, "State Hybrid Sharia Instrumentalization").
narrative_ontology:topic_domain(quran_hadith_substrate__state_hybrid, "legal/political/religious").

domain_priors:requires_active_enforcement(quran_hadith_substrate__state_hybrid).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_hadith_substrate__state_hybrid, '4b2dbaad-3db4-479a-9e0a-c38ad6d07e31').
narrative_ontology:cs_kernel_codification('4b2dbaad-3db4-479a-9e0a-c38ad6d07e31', formalized).
narrative_ontology:cs_authority_grounding('4b2dbaad-3db4-479a-9e0a-c38ad6d07e31', extraction).
narrative_ontology:cs_interpretation_layer_present('4b2dbaad-3db4-479a-9e0a-c38ad6d07e31').
narrative_ontology:cs_reading_relation('4b2dbaad-3db4-479a-9e0a-c38ad6d07e31', quran_hadith_substrate__traditionalist_taqlid, influences).
narrative_ontology:cs_reading_relation('4b2dbaad-3db4-479a-9e0a-c38ad6d07e31', quran_hadith_substrate__reformist_ijtihad, coexists_with).
narrative_ontology:cs_axiom('4b2dbaad-3db4-479a-9e0a-c38ad6d07e31', foundational, state_sovereignty_over_jurisprudential_selection).
narrative_ontology:cs_axiom_status(state_sovereignty_over_jurisprudential_selection, holdable).
narrative_ontology:cs_axiom_grounding('4b2dbaad-3db4-479a-9e0a-c38ad6d07e31', state_sovereignty_over_jurisprudential_selection, conventional).
narrative_ontology:cs_axiom('4b2dbaad-3db4-479a-9e0a-c38ad6d07e31', foundational, sharia_as_state_legitimacy_substrate).
narrative_ontology:cs_axiom_status(sharia_as_state_legitimacy_substrate, holdable).
narrative_ontology:cs_axiom_grounding('4b2dbaad-3db4-479a-9e0a-c38ad6d07e31', sharia_as_state_legitimacy_substrate, instrumental).
narrative_ontology:cs_reference_frame('4b2dbaad-3db4-479a-9e0a-c38ad6d07e31', political_sovereignty_framework).
narrative_ontology:cs_drift_state('4b2dbaad-3db4-479a-9e0a-c38ad6d07e31', contemporary_nation_state_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('4b2dbaad-3db4-479a-9e0a-c38ad6d07e31', '').
narrative_ontology:cs_kernel_id(quran_hadith_substrate__state_hybrid, quran_hadith_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__state_hybrid, state_elites).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__state_hybrid, regime_apparatus).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__state_hybrid, commercial_elites).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, traditionalist_scholars).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, reformist_critics).
narrative_ontology:constraint_vindicates(quran_hadith_substrate__state_hybrid, political_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(quran_hadith_substrate__state_hybrid, selective_religious_adoption).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Selectively adopt classical fiqh rulings in family and criminal law while applying secular or reformist frameworks in commercial, administrative, and constitutional domains. Claim sovereign authority to determine which religious norms enter positive law. Harvest Islamic legitimacy from the population while preserving policy flexibility and economic integration.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, state_elites, agenda_setter,
    institutional, generational, constrained, national).

% Bureaucratic, judicial, and religious-affairs machinery that enforces the hybrid legal order. Benefits from the stability and legitimacy the framework provides. Maintains the interpretive infrastructure that makes sovereign selectivity appear systematic rather than arbitrary.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, regime_apparatus, beneficiary,
    institutional, generational, constrained, national).

% Operate under secular commercial and administrative codes that permit interest-based finance, modern corporate forms, and international contractual standards. Would face restrictive classical sharia commercial rules if the state applied fiqh comprehensively.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, commercial_elites, beneficiary,
    powerful, biographical, mobile, national).

% Uphold classical madhhab consensus as binding across all domains of life. Their comprehensive sharia vision is truncated by the state's selective adoption: family and criminal law are preserved, but commercial, administrative, and constitutional governance are removed from religious authority. They bear the cost of seeing divine law subordinated to state convenience.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, traditionalist_scholars, payer,
    organized, generational, constrained, national).

% Advocate contextual ijtihad, human-rights alignment, and consistent ethical re-interpretation across all legal domains. Their critical readings threaten the regime's selective legitimacy by exposing the arbitrariness of the hybrid framework. Subject to exclusion from official institutions, surveillance, or legal harassment.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, reformist_critics, payer,
    moderate, biographical, constrained, national).

% Study the structural divergence between classical fiqh, state-adopted family law, and secular commercial codes. Document patterns of instrumentalization and compare hybrid frameworks across jurisdictions.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, comparative_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_hadith_substrate__state_hybrid, state_elites).
narrative_ontology:fixing_cost_class(quran_hadith_substrate__state_hybrid, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates family-status and criminal-justice relations through classical fiqh mechanisms widely recognized by the population, while simultaneously coordinating commercial and administrative relations through secular legal frameworks compatible with global economic integration.
% TRANSFER_FUNCTION: Transfers religious legitimacy from classical sharia sources to the state apparatus; transfers policy flexibility from the religious domain to state elites and commercial actors; transfers the costs of truncation to traditionalist scholars and the costs of suppression to reformist critics.
% ABSENT_VOICES: Traditionalist scholars demanding comprehensive madhhab-based governance across all legal domains, and reformist critics demanding consistent contextual ijtihad and human-rights alignment, are structurally excluded from codification committees, high judicial appointments, and official religious institutions.
% DISAPPEARANCE_RATIONALE: Family and criminal law would revert to either comprehensive classical fiqh (empowering traditionalists) or uniform modern codes (empowering reformists). The state's legitimacy mechanism would lose its sharia substrate, forcing an explicit re-grounding in pure nationalism, ideology, or democracy. Commercial and administrative law would need independent legitimization. The political-religious settlement would unravel.
% FOUNDING_PROBLEM: Post-colonial and modernizing Muslim-majority states needed to govern populations that expected Islamic legitimacy in law, while simultaneously building administrative and commercial systems compatible with a global order dominated by secular legal frameworks.
% FOUNDING_PROBLEM_CORROBORATION: State elites and regime apparatus attest the problem remains live, citing popular religious sentiment and the risk of Islamist opposition. Traditionalist scholars attest the founding problem was manufactured by colonial and post-colonial elites to subordinate sharia. Reformist critics attest the problem could have been solved by genuine ijtihad rather than sovereign selectivity. Post-colonial legal historians and comparative constitutional scholars outside the benefiting parties corroborate the governance dilemma but dispute that the hybrid framework was the only viable response.
narrative_ontology:disappearance_verdict(quran_hadith_substrate__state_hybrid, world_rearranges).
narrative_ontology:founding_problem_status(quran_hadith_substrate__state_hybrid, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_hadith_substrate__state_hybrid, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quran_hadith_substrate__state_hybrid, 'none', 1).
narrative_ontology:epsilon_provenance(quran_hadith_substrate__state_hybrid, 0.36, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_hadith_substrate__state_hybrid_tests).
:- end_tests(quran_hadith_substrate__state_hybrid_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.36) is moderate because the constraint transfers substantial legitimacy and policy autonomy to state elites, but it also delivers genuine coordination in family and criminal law. Suppression (0.55) is moderate-to-high and variable: the state must actively suppress both traditionalist demands for comprehensive sharia and reformist demands for consistent ijtihad. Theater ratio (0.45) reflects that a growing share of the state's Islamic legal performance serves legitimacy theatre rather than substantive governanceâespecially as commercial law becomes fully secular. Accessibility collapse (0.50) is middling because alternatives (full traditionalism, full reformism) are structurally marginalized but not fully erased. Resistance (0.60) is significant because both traditionalist and reformist camps actively contest the hybrid frame, though they are often unable to overcome state power.
 *
 * PERSPECTIVAL GAP:
 *   From the state elite seat, the constraint is necessary governance: it prevents sectarian fragmentation, satisfies popular religious expectations, and permits economic modernization. From the traditionalist seat, it is a mutilation of divine law, truncating sharia to whichever parts serve state convenience. From the reformist seat, it is a hypocrisy that instrumentalizes scripture to block genuine ethical progress. The engine computes this divergence from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   State elites and regime apparatus are structural beneficiaries: the constraint subsidizes their legitimacy and policy flexibility (low d). Commercial elites are incidental beneficiaries of the secular commercial framework (low d). Traditionalist scholars and reformist critics are structural targets: the constraint extracts their authority and voice to feed state legitimacy (high d). The derivation is straightforward from beneficiary/victim declarations plus exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by preserving the genuine coordination function in family and criminal law: courts operate, marriages are registered, inheritance shares are allocated according to classical rules. This coordination is real and would need to be replaced. However, the same structure is not a Rope because the coordination is asymmetrically paired with extractionâstate elites collect legitimacy and flexibility that the classical framework does not inherently require. It is not a Snare because the coordination story is not merely cover; the family-law function would survive even if the legitimacy-extraction layer were removed. It is not a Piton because the beneficiaries (state elites) are actively invested in maintaining the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regime_variability_extraction,
    'Does the extractiveness of this constraint derive from the legal structure itself, or from regime-specific incentives that vary across states?',
    'Cross-regime comparison controlling for regime type (monarchy, republic, military) measuring divergence between family-law codification and commercial-law secularization.',
    'If variability is high and regime-dependent, the constraint is better modeled as a family of state-specific constraints rather than one reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regime_variability_extraction, empirical, 'Whether extraction is structurally inherent or regime-contingent').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (state violence, legal bars, institutional exclusion) or internalized (religious deference to state-framed sharia)?',
    'Post-regime-change trajectory: if demand for comprehensive sharia or consistent ijtihad remains suppressed after the state apparatus is removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests â the target populations carry the suppression with them after formal exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').

omega_variable(
    committer_reading_boundary,
    'Is state hybridity a genuine reading of the quran_hadith_substrate, or a meta-constraint of political sovereignty that uses the substrate instrumentally?',
    'Analysis of state legal texts and constitutional provisions â do they claim hermeneutic continuity with the kernel, or explicit sovereignty override?',
    'If purely instrumental with no hermeneutic claim, the constraint may be a Snare rather than Tangled Rope, because the coordination story would be cover for pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_reading_boundary, conceptual, 'Whether the constraint is a kernel reading or a sovereignty mechanism using the kernel as cover').

omega_variable(
    traditionalist_reformist_victim_symmetry,
    'Do traditionalists and reformists experience the same constraint as victims, or are they governed by structurally different mechanisms of truncation and suppression?',
    'Separate per-stakeholder directionality and exit analysis for each victim group.',
    'If structurally different, the constraint may decompose into two sub-constraints (family-law truncation vs public-order suppression) with distinct epsilon values.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(traditionalist_reformist_victim_symmetry, conceptual, 'Whether victim groups experience a unified constraint or distinct mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_hadith_substrate__state_hybrid, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qhs_sh_tr_t0, quran_hadith_substrate__state_hybrid, theater_ratio, 0, 0.3).
narrative_ontology:measurement(qhs_sh_tr_t10, quran_hadith_substrate__state_hybrid, theater_ratio, 10, 0.32).
narrative_ontology:measurement(qhs_sh_tr_t20, quran_hadith_substrate__state_hybrid, theater_ratio, 20, 0.35).
narrative_ontology:measurement(qhs_sh_tr_t30, quran_hadith_substrate__state_hybrid, theater_ratio, 30, 0.38).
narrative_ontology:measurement(qhs_sh_tr_t40, quran_hadith_substrate__state_hybrid, theater_ratio, 40, 0.42).
narrative_ontology:measurement(qhs_sh_tr_t50, quran_hadith_substrate__state_hybrid, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(qhs_sh_be_t0, quran_hadith_substrate__state_hybrid, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(qhs_sh_be_t10, quran_hadith_substrate__state_hybrid, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(qhs_sh_be_t20, quran_hadith_substrate__state_hybrid, base_extractiveness, 20, 0.3).
narrative_ontology:measurement(qhs_sh_be_t30, quran_hadith_substrate__state_hybrid, base_extractiveness, 30, 0.32).
narrative_ontology:measurement(qhs_sh_be_t40, quran_hadith_substrate__state_hybrid, base_extractiveness, 40, 0.34).
narrative_ontology:measurement(qhs_sh_be_t50, quran_hadith_substrate__state_hybrid, base_extractiveness, 50, 0.36).

% Suppression requirement over time
narrative_ontology:measurement(qhs_sh_su_t0, quran_hadith_substrate__state_hybrid, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(qhs_sh_su_t10, quran_hadith_substrate__state_hybrid, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(qhs_sh_su_t20, quran_hadith_substrate__state_hybrid, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(qhs_sh_su_t30, quran_hadith_substrate__state_hybrid, suppression_requirement, 30, 0.52).
narrative_ontology:measurement(qhs_sh_su_t40, quran_hadith_substrate__state_hybrid, suppression_requirement, 40, 0.58).
narrative_ontology:measurement(qhs_sh_su_t50, quran_hadith_substrate__state_hybrid, suppression_requirement, 50, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_hadith_substrate__state_hybrid, enforcement_mechanism).
narrative_ontology:affects_constraint(quran_hadith_substrate__state_hybrid, traditionalist_taqlid).
narrative_ontology:affects_constraint(quran_hadith_substrate__state_hybrid, reformist_ijtihad).

% DUAL FORMULATION NOTE:
% The quran_hadith_substrate kernel decomposes into three structurally distinct constraints. Traditionalist_taqlid treats the kernel as mandating comprehensive madhhab compliance. Reformist_ijtihad treats it as mandating contextual ethical re-interpretation. State_hybrid treats it as subject to sovereign political selection. These are not the same constraint viewed from different angles; they have different beneficiary structures, different enforcement mechanisms, and different empirical footprints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
