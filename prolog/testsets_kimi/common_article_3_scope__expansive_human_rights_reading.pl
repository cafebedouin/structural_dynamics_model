% ============================================================================
% CONSTRAINT STORY: common_article_3_scope__expansive_human_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_article_3_scope__expansive_human_rights_reading, []).

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
 *   constraint_id: common_article_3_scope__expansive_human_rights_reading
 *   human_readable: Common Article 3 Expansive Human Rights Floor
 *   domain: international_law/humanitarian
 *
 * SUMMARY:
 *   This constraint captures the expansive human rights reading of Common
 *   Article 3 of the 1949 Geneva Conventions, which holds that the Article
 *   applies as a minimum floor of humanitarian standards to any organized
 *   armed violence regardless of classification. The reading removes the
 *   traditional state-centric thresholds of intensity and organization,
 *   subjecting state security operations and non-state armed group activity
 *   alike to external monitoring and potential prosecution. It is one reading
 *   of a contested kernel; the state-centric reading applies threshold
 *   requirements, while the ICRC customary reading derives scope from
 *   evolving state practice and opinio juris.
 *
 * KEY AGENTS:
 *   - International judicial bodies (agenda_setter / institutional / analytical) â administer the expansive interpretation through indictments and judgments.
 *   - Detained persons (beneficiary / powerless / trapped) â receive protection against mistreatment under the broad reading.
 *   - Civilian populations affected (beneficiary / powerless / trapped) â gain protected status even in denied-conflict situations.
 *   - State security forces (payer / institutional / constrained) â lose operational autonomy and face external prosecutorial risk.
 *   - Non-state armed groups (payer / organized / constrained) â bound by humanitarian obligations and individual criminal responsibility.
 *   - State sovereignty advocates (excluded / institutional / constrained) â argue for threshold-based application but are structurally marginalized by the expansive frame.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_article_3_scope__expansive_human_rights_reading, 0.67).
domain_priors:suppression_score(common_article_3_scope__expansive_human_rights_reading, 0.68).
domain_priors:theater_ratio(common_article_3_scope__expansive_human_rights_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, extractiveness, 0.67).
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_article_3_scope__expansive_human_rights_reading, tangled_rope).
narrative_ontology:human_readable(common_article_3_scope__expansive_human_rights_reading, "Common Article 3 Expansive Human Rights Floor").
narrative_ontology:topic_domain(common_article_3_scope__expansive_human_rights_reading, "international_law/humanitarian").

domain_priors:requires_active_enforcement(common_article_3_scope__expansive_human_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_article_3_scope__expansive_human_rights_reading, 'b7906a99-e633-4e08-b93f-e7c788f9ef79').
narrative_ontology:cs_kernel_codification('b7906a99-e633-4e08-b93f-e7c788f9ef79', fixed_text).
narrative_ontology:cs_authority_grounding('b7906a99-e633-4e08-b93f-e7c788f9ef79', lineage).
narrative_ontology:cs_interpretation_layer_present('b7906a99-e633-4e08-b93f-e7c788f9ef79').
narrative_ontology:cs_reading_relation('b7906a99-e633-4e08-b93f-e7c788f9ef79', common_article_3_scope__state_centric_reading, forecloses).
narrative_ontology:cs_reading_relation('b7906a99-e633-4e08-b93f-e7c788f9ef79', common_article_3_scope__icrc_customary_reading, coexists_with).
narrative_ontology:cs_axiom('b7906a99-e633-4e08-b93f-e7c788f9ef79', foundational, universal_humanitarian_floor).
narrative_ontology:cs_axiom_status(universal_humanitarian_floor, holdable).
narrative_ontology:cs_axiom_grounding('b7906a99-e633-4e08-b93f-e7c788f9ef79', universal_humanitarian_floor, deontological).
narrative_ontology:cs_axiom('b7906a99-e633-4e08-b93f-e7c788f9ef79', foundational, classification_irrelevance).
narrative_ontology:cs_axiom_status(classification_irrelevance, holdable).
narrative_ontology:cs_axiom_grounding('b7906a99-e633-4e08-b93f-e7c788f9ef79', classification_irrelevance, conventional).
narrative_ontology:cs_reference_frame('b7906a99-e633-4e08-b93f-e7c788f9ef79', universal_humanitarian_floor).
narrative_ontology:cs_drift_state('b7906a99-e633-4e08-b93f-e7c788f9ef79', contemporary_human_rights_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b7906a99-e633-4e08-b93f-e7c788f9ef79', '').
narrative_ontology:cs_kernel_id(common_article_3_scope__expansive_human_rights_reading, common_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_article_3_scope__expansive_human_rights_reading, detained_persons).
narrative_ontology:constraint_beneficiary(common_article_3_scope__expansive_human_rights_reading, civilian_populations_affected).
narrative_ontology:constraint_victim(common_article_3_scope__expansive_human_rights_reading, state_security_forces).
narrative_ontology:constraint_victim(common_article_3_scope__expansive_human_rights_reading, non_state_armed_groups).
narrative_ontology:constraint_vindicates(common_article_3_scope__expansive_human_rights_reading, universal_human_rights_in_armed_conflict).
narrative_ontology:constraint_vindicates(common_article_3_scope__expansive_human_rights_reading, humanitarian_law_human_rights_convergence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercise jurisdiction over alleged war crimes and crimes against humanity, interpreting Common Article 3 expansively to cover organized armed violence regardless of whether states classify the situation as armed conflict or internal security operations. Their judgments and indictments directly determine whether state and non-state actors face prosecution for mistreatment of detainees.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, international_judicial_bodies, agenda_setter,
    institutional, civilizational, analytical, global).

% Held by state or non-state forces during organized armed violence. They receive protection against torture, cruel treatment, and arbitrary execution, and gain the right to a fair trial if prosecuted, provided the expansive reading is applied to the situation.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, detained_persons, beneficiary,
    powerless, immediate, trapped, local).

% Live in areas where organized armed violence occurs. They receive protection against direct violence and arbitrary detention under the expansive reading, even when the territorial state denies the existence of an armed conflict and characterizes the violence as law enforcement.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, civilian_populations_affected, beneficiary,
    powerless, immediate, trapped, local).

% Conduct internal security and counter-insurgency operations. Under the expansive reading, their tactics, detention practices, and targeting decisions are subject to international humanitarian law standards and potential external prosecution, regardless of governmental conflict classification or consent.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, state_security_forces, payer,
    institutional, immediate, constrained, national).

% Engage in organized armed violence against states or other groups. They are bound by Common Article 3 obligations to treat detainees humanely and are exposed to individual criminal responsibility under international tribunals when the expansive reading is applied to their operations.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, non_state_armed_groups, payer,
    organized, immediate, constrained, regional).

% Represent state interests in diplomatic and legal forums. They argue that Common Article 3 applies only above intensity and organization thresholds, resisting external monitoring and prosecutorial oversight of domestic security operations. Their preferred reading is structurally marginalized by the expansive interpretation.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, state_sovereignty_advocates, excluded,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes minimum standards of humane treatment for all persons detained or affected by organized armed violence, filling the protection gap left by strict conflict classification and providing a single referent for disparate actors.
% TRANSFER_FUNCTION: Transfers obligations of humane treatment and fair trial from state and non-state belligerents to detained persons and affected populations; transfers monitoring and prosecutorial authority to international judicial and human rights bodies.
% ABSENT_VOICES: States asserting sovereign prerogative over internal security and law enforcement; military commanders arguing that operational necessity and effective counter-terrorism require threshold-based classification to avoid judicial encumbrance.
% DISAPPEARANCE_RATIONALE: If the expansive reading vanished, states would regain broad autonomy to classify internal violence as law enforcement exempt from humanitarian rules; detainees in ambiguous conflicts would lose treaty-based protections; international tribunals would see their jurisdiction over internal security operations contract sharply, and the global legal architecture would shift toward state-determined classification.
% FOUNDING_PROBLEM: The 1949 Geneva Conventions left victims of non-international armed conflict and ambiguously classified violence without explicit treaty protection, creating an incentive for states to deny conflict status to avoid legal constraints.
% FOUNDING_PROBLEM_CORROBORATION: Human rights NGOs and international criminal tribunal jurisprudence (e.g., ICTY Tadic) corroborate the protection gap and the expansive reading's role in filling it. States and some military legal advisors contest that the gap is real or that Common Article 3 was intended to operate without intensity and organization thresholds.
narrative_ontology:disappearance_verdict(common_article_3_scope__expansive_human_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(common_article_3_scope__expansive_human_rights_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_article_3_scope__expansive_human_rights_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(common_article_3_scope__expansive_human_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(common_article_3_scope__expansive_human_rights_reading, 0.67, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_article_3_scope__expansive_human_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_article_3_scope__expansive_human_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_article_3_scope__expansive_human_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderately high (0.67) because the reading extracts significant operational autonomy and sovereignty from state and non-state belligerents, transferring prosecutorial exposure and compliance costs to them. Suppression is high (0.68) because the reading persists only through active institutional enforcement by international tribunals and universal jurisdiction; states resist it vigorously. Theater is moderate (0.35): much activity is substantive prosecution and monitoring, but an increasing share consists of performative compliance and declaratory acceptance without operational change. Accessibility collapse is low (0.30) because the state-centric alternative remains widely available and actively defended. Resistance is substantial (0.62) due to persistent state opposition in diplomatic, military, and judicial forums.
 *
 * PERSPECTIVAL GAP:
 *   The judicial bodies and protected populations compute the constraint as coordination with low directionality: it establishes a predictable humanitarian floor and empowers adjudication. State security forces and non-state armed groups compute it as extractive with high directionality: it removes their classification prerogatives, exposes commanders to prosecution, and imposes external oversight on internal operations. The divergence arises from the same structural facts â classification removal benefits the powerless and costs the organized violent actors.
 *
 * DIRECTIONALITY LOGIC:
 *   Detained persons and affected civilians are declared beneficiaries (d near 0.0, low effective extraction) because the constraint subsidizes their protection and legal status. State security forces and non-state armed groups are declared victims (d near 1.0, high effective extraction) because the constraint removes their authority to define the legal framework applicable to their own operations and exposes them to criminal liability. International judicial bodies sit near the beneficiary end (d low) because the expansive reading expands their jurisdiction and institutional relevance. State sovereignty advocates are excluded and bear high directional cost structurally, though they are not formally within the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The expansive reading prevents mandatrophy mislabeling by preserving the genuine coordination function: without a minimum humanitarian floor, detainees in ambiguous conflicts have no treaty-based protection against torture and summary execution. However, it is not a pure rope because the extraction is asymmetric â belligerents pay in sovereignty and autonomy while protected persons do not share the cost. The active enforcement requirement (international tribunals, universal jurisdiction) confirms it as tangled rope rather than mountain or piton. If enforcement were withdrawn, states would revert to classification games, indicating the constraint is held by institutional work rather than natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ca3_expansive_kernel_position,
    'This constraint instantiates the expansive_human_rights_reading of kernel common_article_3_scope; does its broad application survive if the state_centric_reading captures domestic and international judicial practice?',
    'Comparative analysis of domestic and international tribunal decisions adopting threshold-based versus threshold-free Common Article 3 application over a ten-year window.',
    'If state-centric readings dominate tribunal practice, the expansive reading''s effective extraction diminishes and its victim set contracts to classical non-international armed conflicts, shifting classification toward a weaker scaffold or rope dynamic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ca3_expansive_kernel_position, conceptual, 'Position of expansive reading within contested Common Article 3 scope kernel.').

omega_variable(
    enforcement_reality_vs_normative_claim,
    'Does the existence of universal jurisdiction and international tribunals create sufficient enforcement to sustain the expansive reading against state resistance, or is the constraint''s extraction largely symbolic?',
    'Empirical tracking of prosecutions and judgments applying Common Article 3 to low-intensity or law-enforcement contexts, cross-referenced with state compliance data.',
    'If enforcement is sparse, the constraint operates more as a normative aspiration than an active extraction mechanism, shifting metrics toward theater and piton dynamics rather than tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_reality_vs_normative_claim, empirical, 'Enforcement gap between normative claim and actual prosecutorial capacity.').

omega_variable(
    human_rights_law_convergence,
    'Does the expansive reading derive its force from Common Article 3 as treaty law, or from convergent human rights law norms that would persist even if Common Article 3 were narrowly construed?',
    'Jurisprudential mapping of tribunals that cite Common Article 3 independently versus as shorthand for human rights obligations.',
    'If force derives principally from human rights law, the constraint''s kernel is misidentified as Common Article 3 alone; decomposition would shift classification toward a distinct human rights constraint family with different structural properties.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(human_rights_law_convergence, conceptual, 'Source of normative force for the expansive reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_article_3_scope__expansive_human_rights_reading, 0, 29).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ca3_expansive_tr_t0, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(ca3_expansive_tr_t5, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement(ca3_expansive_tr_t10, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 10, 0.23).
narrative_ontology:measurement(ca3_expansive_tr_t15, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 15, 0.26).
narrative_ontology:measurement(ca3_expansive_tr_t20, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 20, 0.29).
narrative_ontology:measurement(ca3_expansive_tr_t25, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 25, 0.32).
narrative_ontology:measurement(ca3_expansive_tr_t29, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 29, 0.35).

% Extraction over time
narrative_ontology:measurement(ca3_expansive_be_t0, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ca3_expansive_be_t5, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(ca3_expansive_be_t10, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 10, 0.53).
narrative_ontology:measurement(ca3_expansive_be_t15, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(ca3_expansive_be_t20, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(ca3_expansive_be_t25, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 25, 0.65).
narrative_ontology:measurement(ca3_expansive_be_t29, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 29, 0.67).

% Suppression requirement over time
narrative_ontology:measurement(ca3_expansive_su_t0, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(ca3_expansive_su_t5, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 5, 0.5).
narrative_ontology:measurement(ca3_expansive_su_t10, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(ca3_expansive_su_t15, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 15, 0.6).
narrative_ontology:measurement(ca3_expansive_su_t20, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 20, 0.63).
narrative_ontology:measurement(ca3_expansive_su_t25, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 25, 0.66).
narrative_ontology:measurement(ca3_expansive_su_t29, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 29, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_article_3_scope__expansive_human_rights_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(common_article_3_scope__expansive_human_rights_reading, state_centric_reading).
narrative_ontology:affects_constraint(common_article_3_scope__expansive_human_rights_reading, icrc_customary_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the common_article_3_scope kernel. The expansive human rights reading treats CA3 as applying to all organized armed violence regardless of classification, whereas the state-centric reading applies intensity and organization thresholds and the ICRC customary reading derives scope from evolving state practice and opinio juris. Each reading instantiates a structurally distinct constraint with different beneficiary/victim profiles, directionalities, and epsilon values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
