% ============================================================================
% CONSTRAINT STORY: ihl_distinction_proportionality__outcomes_based_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ihl_distinction_proportionality__outcomes_based_reading, []).

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
 *   constraint_id: ihl_distinction_proportionality__outcomes_based_reading
 *   human_readable: IHL Distinction/Proportionality: Outcomes-Based Compliance
 *   domain: international_humanitarian_law/military_ethics/technology_governance
 *
 * SUMMARY:
 *   This constraint represents the 'outcomes-based' reading of International
 *   Humanitarian Law (IHL) obligations regarding autonomous weapons systems.
 *   It posits that IHL's requirements for distinction and proportionality are
 *   satisfied if autonomous systems can demonstrably achieve performance
 *   equal to or exceeding human operators. This reading emphasizes
 *   technology-neutrality, focusing on the 'what' (outcomes) rather than the
 *   'how' (means or human agency). It is a contested interpretation within
 *   the broader debate on autonomous weapons.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ihl_distinction_proportionality__outcomes_based_reading, 0.45).
domain_priors:suppression_score(ihl_distinction_proportionality__outcomes_based_reading, 0.3).
domain_priors:theater_ratio(ihl_distinction_proportionality__outcomes_based_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ihl_distinction_proportionality__outcomes_based_reading, rope).
narrative_ontology:human_readable(ihl_distinction_proportionality__outcomes_based_reading, "IHL Distinction/Proportionality: Outcomes-Based Compliance").
narrative_ontology:topic_domain(ihl_distinction_proportionality__outcomes_based_reading, "international_humanitarian_law/military_ethics/technology_governance").

domain_priors:requires_active_enforcement(ihl_distinction_proportionality__outcomes_based_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ihl_distinction_proportionality__outcomes_based_reading, 'c310d122-fdb1-40de-bd30-97cb70d7020d').
narrative_ontology:cs_kernel_codification('c310d122-fdb1-40de-bd30-97cb70d7020d', formalized).
narrative_ontology:cs_authority_grounding('c310d122-fdb1-40de-bd30-97cb70d7020d', lineage).
narrative_ontology:cs_interpretation_layer_present('c310d122-fdb1-40de-bd30-97cb70d7020d').
narrative_ontology:cs_reading_relation('c310d122-fdb1-40de-bd30-97cb70d7020d', ihl_distinction_proportionality__human_agency_reading, coexists_with).
narrative_ontology:cs_reading_relation('c310d122-fdb1-40de-bd30-97cb70d7020d', ihl_distinction_proportionality__categorical_prohibition_reading, coexists_with).
narrative_ontology:cs_axiom('c310d122-fdb1-40de-bd30-97cb70d7020d', foundational, ihl_is_outcome_focused).
narrative_ontology:cs_axiom_status(ihl_is_outcome_focused, holdable).
narrative_ontology:cs_axiom_grounding('c310d122-fdb1-40de-bd30-97cb70d7020d', ihl_is_outcome_focused, conventional).
narrative_ontology:cs_axiom('c310d122-fdb1-40de-bd30-97cb70d7020d', foundational, technology_neutrality_is_paramount).
narrative_ontology:cs_axiom_status(technology_neutrality_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('c310d122-fdb1-40de-bd30-97cb70d7020d', technology_neutrality_is_paramount, conventional).
narrative_ontology:cs_reference_frame('c310d122-fdb1-40de-bd30-97cb70d7020d', traditional_ihl_compliance_framework).
narrative_ontology:cs_drift_state('c310d122-fdb1-40de-bd30-97cb70d7020d', contemporary_ai_advancement_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c310d122-fdb1-40de-bd30-97cb70d7020d', '').
narrative_ontology:cs_kernel_id(ihl_distinction_proportionality__outcomes_based_reading, ihl_distinction_proportionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__outcomes_based_reading, military_efficiency_advocates).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__outcomes_based_reading, defense_contractors).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__outcomes_based_reading, ihl_interpretive_authority).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__outcomes_based_reading, civilian_populations_at_risk).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the flexibility to deploy autonomous systems that can potentially reduce risk to own forces and achieve precision targets, provided they meet or exceed human performance metrics. Views IHL as outcome-focused.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, military_efficiency_advocates, beneficiary,
    institutional, generational, mobile, global).

% Benefits from the legal pathway to develop and sell autonomous weapons systems, provided they can demonstrate compliance with IHL performance standards. Their business model depends on this interpretation.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, defense_contractors, beneficiary,
    organized, biographical, arbitrage, global).

% Bears the cost of potentially losing interpretive control over IHL principles, as the focus shifts from human agency and moral judgment to quantifiable technical performance. Fears a 'race to the bottom' on ethical standards.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, ihl_interpretive_authority, payer,
    institutional, civilizational, constrained, global).

% Potentially victimized if the technical metrics for distinction and proportionality fail in real-world scenarios, or if the 'equal to or exceeding human' standard is set too low. Their safety depends on the robustness of the technical compliance.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, civilian_populations_at_risk, payer,
    powerless, immediate, trapped, local).

% Would argue that human moral judgment is an irreducible component of IHL compliance, and that an outcomes-based approach risks dehumanizing warfare. Their concerns are often marginalized in technical compliance discussions.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, human_rights_advocates, excluded,
    organized, generational, constrained, global).

% Advise on the legality of new weapons systems. They must reconcile the outcomes-based reading with existing IHL principles and the concerns of other stakeholders, navigating a complex and evolving legal landscape.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, military_legal_advisors, observer,
    institutional, biographical, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for assessing the legality of autonomous weapons systems by focusing on measurable performance outcomes, allowing for technological advancement while maintaining IHL compliance.
% TRANSFER_FUNCTION: Transfers the burden of IHL compliance from human moral judgment to demonstrable technical performance metrics, from human operators to autonomous systems developers.
% ABSENT_VOICES: Human rights advocates and ethicists who argue for an irreducible human element in lethal decision-making are often excluded from the technical discussions that define 'equivalent performance'.
% DISAPPEARANCE_RATIONALE: If this outcomes-based reading vanished, the development and deployment of autonomous weapons systems would face significant legal uncertainty or outright prohibition, forcing militaries and defense contractors to fundamentally rethink their approach to future warfare.
% FOUNDING_PROBLEM: How to integrate advanced autonomous technologies into military operations while upholding the fundamental principles of International Humanitarian Law, particularly distinction and proportionality.
% FOUNDING_PROBLEM_CORROBORATION: Military legal departments and defense contractors attest that the problem is live, as technological advancements continue to outpace legal frameworks. Humanitarian organizations and ethicists corroborate the need for clear legal guidance, though they dispute the proposed solution.
narrative_ontology:disappearance_verdict(ihl_distinction_proportionality__outcomes_based_reading, world_rearranges).
narrative_ontology:founding_problem_status(ihl_distinction_proportionality__outcomes_based_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ihl_distinction_proportionality__outcomes_based_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ihl_distinction_proportionality__outcomes_based_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ihl_distinction_proportionality__outcomes_based_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ihl_distinction_proportionality__outcomes_based_reading_tests).
:- end_tests(ihl_distinction_proportionality__outcomes_based_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate because this reading permits the use of autonomous systems, which benefits military efficiency and defense contractors, but it also imposes a significant burden of proof for performance. Suppression (0.3) is relatively low as this is an active area of legal and ethical debate, not a fully enforced prohibition. Theater ratio (0.1) is low because the focus is on demonstrable technical performance, which is less susceptible to performative compliance than other interpretations. The constraint is claimed as a 'rope' because it aims to coordinate technological advancement with legal compliance, but its moderate extractiveness and the existence of victims suggest it leans towards a 'tangled rope' from certain perspectives.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of military planners and defense contractors, this is a pragmatic 'rope' that enables innovation while respecting IHL. From the perspective of humanitarian law custodians and human rights advocates, it is a 'tangled rope' or even a 'snare' that risks undermining fundamental principles by reducing complex moral judgments to technical metrics. The engine's classification will reflect these divergent experiences based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Military efficiency advocates and defense contractors are beneficiaries, as this reading provides a legal pathway for their interests. IHL interpretive authority and civilian populations at risk are victims, as their concerns about human agency and potential harm are either marginalized or directly impacted by the technical focus. The outcomes-based approach shifts the locus of responsibility and control, creating a moderate extractive dynamic.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    performance_metric_validity,
    'Can ''distinction'' and ''proportionality'' be adequately captured by technical performance metrics, and can these metrics be reliably verified in real-world combat environments?',
    'Extensive empirical testing, independent validation of performance metrics, and post-conflict analysis of autonomous system deployments. International consensus on verification standards.',
    'If metrics are found to be inadequate or unverifiable, the outcomes-based reading''s legitimacy collapses, pushing towards human_agency_reading or categorical_prohibition_reading. If robust, it strengthens this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_metric_validity, empirical, 'Uncertainty regarding the technical feasibility and ethical adequacy of performance metrics for IHL.').

omega_variable(
    human_agency_irreducibility,
    'Is there an irreducible element of human moral judgment required for IHL compliance that cannot be replicated or exceeded by autonomous systems, regardless of technical performance?',
    'Philosophical and legal consensus on the interpretation of Martens Clause principles and the nature of moral responsibility in warfare. This is a conceptual, not empirical, resolution.',
    'If human moral judgment is deemed irreducible, this outcomes-based reading is foreclosed by the human_agency_reading. If not, this reading gains stronger conceptual grounding.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(human_agency_irreducibility, conceptual, 'The conceptual debate over the necessity of human moral judgment in lethal decision-making.').

omega_variable(
    standard_setting_bias,
    'Who sets the ''equal to or exceeding human'' performance standard, and is there a risk of regulatory capture by military or industry interests that could lower the effective standard?',
    'Establishment of independent, multi-stakeholder bodies with transparent processes for setting and reviewing performance standards, with strong oversight from international humanitarian law experts.',
    'If bias is confirmed, the extractiveness of this reading increases significantly, as the ''outcomes-based'' claim becomes a cover for lower ethical thresholds, pushing it towards a Snare. If robustly independent, its legitimacy as a Rope is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(standard_setting_bias, preference, 'Risk of bias in setting performance standards for autonomous systems.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ihl_distinction_proportionality__outcomes_based_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ihl__tr_t0, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(ihl__tr_t5, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 5, 0.07).
narrative_ontology:measurement(ihl__tr_t10, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 10, 0.08).
narrative_ontology:measurement(ihl__tr_t15, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 15, 0.09).
narrative_ontology:measurement(ihl__tr_t20, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(ihl__be_t0, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ihl__be_t5, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(ihl__be_t10, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 10, 0.43).
narrative_ontology:measurement(ihl__be_t15, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 15, 0.44).
narrative_ontology:measurement(ihl__be_t20, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(ihl__su_t0, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(ihl__su_t5, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 5, 0.23).
narrative_ontology:measurement(ihl__su_t10, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 10, 0.26).
narrative_ontology:measurement(ihl__su_t15, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 15, 0.28).
narrative_ontology:measurement(ihl__su_t20, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 20, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ihl_distinction_proportionality__outcomes_based_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__outcomes_based_reading, human_agency_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__outcomes_based_reading, categorical_prohibition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'ihl_distinction_proportionality' kernel. This outcomes-based reading focuses on technical performance, while sibling readings emphasize human agency or categorical prohibition. All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
