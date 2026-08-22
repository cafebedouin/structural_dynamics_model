% ============================================================================
% CONSTRAINT STORY: montevideo_statehood_criteria__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_montevideo_statehood_criteria__hybrid_reading, []).

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
 *   constraint_id: montevideo_statehood_criteria__hybrid_reading
 *   human_readable: Montevideo Statehood Criteria â Hybrid Reading (Objective Criteria Plus Normative Legitimacy)
 *   domain: international_law/political_philosophy
 *
 * SUMMARY:
 *   The Montevideo Convention of 1933 proposed four objective criteria for
 *   statehood: permanent population, defined territory, government, and
 *   capacity to enter relations with other states. The hybrid reading adds a
 *   normative legitimacy layerâdemocratic governance, human rights
 *   compliance, and non-aggressionâtreating these as necessary conditions
 *   alongside the objective criteria. This reading is advanced primarily by
 *   liberal democratic states and human rights institutions. It functions as
 *   both a coordination mechanism (unifying the international community
 *   around liberal norms) and an extraction mechanism (denying sovereignty to
 *   non-liberal secessionists and authorizing intervention against
 *   authoritarian regimes). The constraint is a contested reading of the
 *   statehood kernel: it is neither the classical declaratory doctrine
 *   (objective criteria alone suffice) nor the constitutive doctrine
 *   (recognition by others creates statehood), but a normative overlay that
 *   modifies the declaratory framework. Non-liberal secessionists that meet
 *   objective criteria are excluded from statehood; liberal states gain
 *   normative justification for recognition denial; and interventionist
 *   coalitions acquire legal cover for regime change. The engine will compute
 *   per-seat classifications from these structural facts; the authored claim
 *   is tangled_rope because the arrangement simultaneously coordinates
 *   legitimate governance standards and asymmetrically extracts sovereignty
 *   from marginalized aspirants.
 *
 * KEY AGENTS:
 *   - liberal_democratic_states (agenda_setter, institutional power, arbitrage exit) â set recognition criteria and benefit from gatekeeping authority
 *   - non_liberal_secessionists (primary target, powerless, trapped exit) â meet objective criteria but are denied recognition on normative grounds
 *   - interventionist_coalitions (beneficiary, powerful, mobile exit) â gain legal cover for coercive action against normatively deficient regimes
 *   - authoritarian_regimes (target, powerful but constrained exit) â face delegitimation and intervention justified by the hybrid criteria
 *   - international_legal_scholars (analytical observer) â document the doctrinal contest
 *   - excluded_peoples (excluded, powerless, trapped) â bear the consequences without voice in the criteria
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(montevideo_statehood_criteria__hybrid_reading, 0.62).
domain_priors:suppression_score(montevideo_statehood_criteria__hybrid_reading, 0.68).
domain_priors:theater_ratio(montevideo_statehood_criteria__hybrid_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(montevideo_statehood_criteria__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(montevideo_statehood_criteria__hybrid_reading, "Montevideo Statehood Criteria â Hybrid Reading (Objective Criteria Plus Normative Legitimacy)").
narrative_ontology:topic_domain(montevideo_statehood_criteria__hybrid_reading, "international_law/political_philosophy").

domain_priors:requires_active_enforcement(montevideo_statehood_criteria__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(montevideo_statehood_criteria__hybrid_reading, 'c4520bd4-7132-468e-bbed-eba3165e8591').
narrative_ontology:cs_kernel_codification('c4520bd4-7132-468e-bbed-eba3165e8591', formalized).
narrative_ontology:cs_authority_grounding('c4520bd4-7132-468e-bbed-eba3165e8591', lineage).
narrative_ontology:cs_interpretation_layer_present('c4520bd4-7132-468e-bbed-eba3165e8591').
narrative_ontology:cs_reading_relation('c4520bd4-7132-468e-bbed-eba3165e8591', montevideo_statehood_criteria__declaratory_reading, forecloses).
narrative_ontology:cs_reading_relation('c4520bd4-7132-468e-bbed-eba3165e8591', montevideo_statehood_criteria__constitutive_reading, coexists_with).
narrative_ontology:cs_axiom('c4520bd4-7132-468e-bbed-eba3165e8591', foundational, democratic_governance_statehood_prerequisite).
narrative_ontology:cs_axiom_status(democratic_governance_statehood_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('c4520bd4-7132-468e-bbed-eba3165e8591', democratic_governance_statehood_prerequisite, conventional).
narrative_ontology:cs_axiom('c4520bd4-7132-468e-bbed-eba3165e8591', foundational, human_rights_compliance_statehood_condition).
narrative_ontology:cs_axiom_status(human_rights_compliance_statehood_condition, holdable).
narrative_ontology:cs_axiom_grounding('c4520bd4-7132-468e-bbed-eba3165e8591', human_rights_compliance_statehood_condition, deontological).
narrative_ontology:cs_reference_frame('c4520bd4-7132-468e-bbed-eba3165e8591', liberal_legitimacy_framework).
narrative_ontology:cs_drift_state('c4520bd4-7132-468e-bbed-eba3165e8591', contemporary_multipolar_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c4520bd4-7132-468e-bbed-eba3165e8591', '').
narrative_ontology:cs_kernel_id(montevideo_statehood_criteria__hybrid_reading, montevideo_statehood_criteria).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__hybrid_reading, liberal_democratic_states).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__hybrid_reading, interventionist_coalitions).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__hybrid_reading, non_liberal_secessionists).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__hybrid_reading, authoritarian_regimes).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__hybrid_reading, liberal_international_order).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__hybrid_reading, human_rights_universality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control the recognition process and international legal discourse. They formulated and promote the hybrid criteria that add democratic governance and human rights compliance to the Montevideo objective requirements. They gain gatekeeping power over which entities achieve statehood, normative justification for withholding recognition from non-liberal secessionists, and legal cover for humanitarian intervention and regime change operations.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, liberal_democratic_states, agenda_setter,
    institutional, generational, arbitrage, global).

% Seek independent statehood and may meet the four objective Montevideo criteria but are denied recognition because they fail the normative legitimacy tests imposed by powerful states. Their alternatives collapse because without recognition they cannot access international legal personality, UN membership, or sovereign lending.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, non_liberal_secessionists, payer,
    powerless, biographical, trapped, regional).

% States or alliances that undertake humanitarian intervention or regime change. They benefit from the hybrid reading because it provides legal and moral cover for actions against authoritarian or rights-abusing regimes by framing non-compliance with normative criteria as a legitimacy deficit that justifies external coercion.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, interventionist_coalitions, beneficiary,
    powerful, immediate, mobile, global).

% Govern incumbents that control territory and population but are targeted by the normative legitimacy criteria. They face delegitimation, sanctions, and potential intervention justified by their human rights record or lack of democratic governance. Their exit is constrained because abandoning authoritarian control often means losing power entirely, yet retaining it invites external pressure.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, authoritarian_regimes, payer,
    powerful, biographical, constrained, national).

% Analyze and debate the statehood criteria. They document the tension between the declaratory tradition and the normative turn, track recognition practice, and evaluate whether the hybrid criteria reflect law or policy.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, international_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% Peoples under authoritarian rule or within unrecognized secessionist territories who would form states if purely objective criteria applied. They are not party to the recognition decisions that determine their political future and have no institutional voice in the normative criteria-setting process.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, excluded_peoples, excluded,
    powerless, generational, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(montevideo_statehood_criteria__hybrid_reading, liberal_democratic_states).
narrative_ontology:fixing_cost_class(montevideo_statehood_criteria__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the community of states around a shared liberal normative framework, reducing arbitrary recognition by great powers and providing a common standard for legitimate statehood that theoretically limits predatory state creation.
% TRANSFER_FUNCTION: Transfers sovereignty, recognition, and legal standing from non-liberal secessionist movements and authoritarian regimes to liberal democratic states and interventionist coalitions; the costs of norm enforcement and exclusion are borne by the denied entities.
% ABSENT_VOICES: Non-liberal secessionist movements, authoritarian regimes, and peoples in the global South who reject conditional sovereignty are structurally excluded from the recognition conversation; anti-interventionist and pluralist legal scholars are marginalized in mainstream international fora.
% DISAPPEARANCE_RATIONALE: If the hybrid criteria vanished, non-liberal entities meeting objective Montevideo requirements would likely gain recognition, the legal architecture for humanitarian intervention and regime change would lose its primary foundation, and the liberal democratic community would forfeit its gatekeeping authority over international membership.
% FOUNDING_PROBLEM: The pure declaratory doctrine failed to prevent abusive states from gaining equal standing and did not address human rights outrages by sovereigns; the pure constitutive doctrine gave great powers arbitrary veto authority over new states based on political convenience.
% FOUNDING_PROBLEM_CORROBORATION: Liberal democratic states and human rights NGOs attest the problem remains live, citing ongoing authoritarianism. Post-colonial scholars and non-aligned movement representatives attest the hybrid solution reproduces great-power hierarchy under normative cover; historical evidence from the decolonization period and contemporary contested recognitions corroborates the critical reading from outside the beneficiary set.
narrative_ontology:disappearance_verdict(montevideo_statehood_criteria__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(montevideo_statehood_criteria__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(montevideo_statehood_criteria__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(montevideo_statehood_criteria__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(montevideo_statehood_criteria__hybrid_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(montevideo_statehood_criteria__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(montevideo_statehood_criteria__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(montevideo_statehood_criteria__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is set at 0.62 because recognition denial is a severe deprivation of sovereignty, moderated only by the partial reality of governance coordination. Suppression is 0.68 because the constraint depends on active enforcement: diplomatic non-recognition, sanctions, and occasionally military intervention to uphold the normative criteria. Theater ratio is 0.45 because the criteria are applied with visible selectivityâstrategic allies enjoy leniency while adversaries face strict scrutinyâindicating that a substantial share of enforcement activity serves political rather than principled ends. Accessibility collapse is 0.72 because once an entity is categorized as normatively illegitimate, virtually no alternative pathway to legal statehood remains. Resistance is 0.58 because targeted states and global South coalitions actively contest the legitimacy of conditional sovereignty in UN fora and regional bodies.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of liberal democratic states, the hybrid criteria are legitimate coordination that prevents abusive statehood and protects human rights. From the seat of non-liberal secessionists, the same criteria are enforced exclusion that bars them from legal personality regardless of factual statehood. The engine computes this divergence: the agenda_setter and beneficiary seats will derive low directionality and damped effective extraction, while the payer seatsâpowerless secessionists with trapped exit and constrained authoritarian regimesâwill derive high directionality and amplified extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   liberal_democratic_states are declared beneficiaries with institutional power and arbitrage-grade exit (they can recognize or withhold recognition strategically), placing them near the full-beneficiary end. non_liberal_secessionists are declared victims with powerless status and trapped exit, placing them near the full-target end. interventionist_coalitions are beneficiaries with powerful status and mobile exit, placing them near the beneficiary end. authoritarian_regimes are declared victims with powerful status but constrained exit; despite their power, the structural combination of victim role and constrained exit pushes their directionality toward the target end, though less extremely than the powerless secessionists.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading prevents pure mandatrophy mislabeling because it names both the coordination function (preventing abusive statehood and unifying standards) and the asymmetric extraction (denying sovereignty to non-liberal actors). If classified as a pure rope, the victim set would be invisible. If classified as a pure snare, the genuine governance-coordination function would be denied. The tangled_rope classification is warranted only because both functions are structurally present and enforced through the same recognition mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    selective_enforcement_ambiguity,
    'Are democratic governance and human rights conditions applied consistently across all candidate states, or selectively to serve geopolitical interests of powerful states?',
    'Comparative case analysis of recognition decisions (Kosovo vs. Palestine vs. Northern Cyprus vs. Taiwan) and correlation with recognizer-state strategic interests.',
    'If selective, the constraint''s extraction is higher and its coordination function is cover for strategic denial; if consistent, the extraction is more evenly distributed and the coordination claim strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_enforcement_ambiguity, empirical, 'Whether normative criteria are enforced evenly or as political cover').

omega_variable(
    cs_framing_alternative,
    'Does the hybrid reading instantiate a commitment system grounded in the Montevideo text, or a normative framework that has superseded the text?',
    'Analysis of judicial citation patterns and treaty interpretation methodology in international courts and tribunals.',
    'If the latter, the kernel is not the Montevideo criteria but the liberal international order narrative layered above it, changing the authority_grounding and classification profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_alternative, conceptual, 'Alternative framing of the commitment system''s kernel').

omega_variable(
    normative_criteria_universalism,
    'Are the normative legitimacy requirements genuinely universal legal standards, or culturally particular Western values imposed as universal?',
    'Voting patterns in UN bodies, regional organization practice, and state accession to human rights instruments across civilizational boundaries.',
    'If particularist, the constraint functions as cultural extraction and the coordination claim collapses for non-Western parties; if universalist, the coordination function is genuinely global.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(normative_criteria_universalism, preference, 'Universalism vs. cultural particularity of legitimacy criteria').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(montevideo_statehood_criteria__hybrid_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(montevideo_hybrid_tr_t0, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(montevideo_hybrid_tr_t6, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 6, 0.24).
narrative_ontology:measurement(montevideo_hybrid_tr_t12, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement(montevideo_hybrid_tr_t18, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 18, 0.36).
narrative_ontology:measurement(montevideo_hybrid_tr_t24, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 24, 0.42).
narrative_ontology:measurement(montevideo_hybrid_tr_t30, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement(montevideo_hybrid_tr_t35, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 35, 0.52).

% Extraction over time
narrative_ontology:measurement(montevideo_hybrid_be_t0, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(montevideo_hybrid_be_t6, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 6, 0.35).
narrative_ontology:measurement(montevideo_hybrid_be_t12, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 12, 0.42).
narrative_ontology:measurement(montevideo_hybrid_be_t18, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 18, 0.5).
narrative_ontology:measurement(montevideo_hybrid_be_t24, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 24, 0.56).
narrative_ontology:measurement(montevideo_hybrid_be_t30, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 30, 0.6).
narrative_ontology:measurement(montevideo_hybrid_be_t35, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 35, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(montevideo_hybrid_su_t0, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(montevideo_hybrid_su_t6, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 6, 0.42).
narrative_ontology:measurement(montevideo_hybrid_su_t12, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 12, 0.5).
narrative_ontology:measurement(montevideo_hybrid_su_t18, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 18, 0.58).
narrative_ontology:measurement(montevideo_hybrid_su_t24, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 24, 0.63).
narrative_ontology:measurement(montevideo_hybrid_su_t30, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 30, 0.66).
narrative_ontology:measurement(montevideo_hybrid_su_t35, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 35, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(montevideo_statehood_criteria__hybrid_reading, identity_coordination).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, declaratory_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, constitutive_reading).

% DUAL FORMULATION NOTE:
% The Montevideo statehood criteria kernel decomposes into three structurally distinct claims: the declaratory reading (objective criteria suffice), the constitutive reading (recognition creates statehood), and the hybrid reading (objective criteria plus normative legitimacy are required). Each has different epsilon values, stakeholders, and classifications. This reading adds normative conditions to the objective criteria, producing distinct victim and beneficiary sets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
