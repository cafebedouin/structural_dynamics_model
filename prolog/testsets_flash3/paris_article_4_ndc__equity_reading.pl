% ============================================================================
% CONSTRAINT STORY: paris_article_4_ndc__equity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_paris_article_4_ndc__equity_reading, []).

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
 *   constraint_id: paris_article_4_ndc__equity_reading
 *   human_readable: Paris Agreement Article 4 NDCs: Equity Reading (CBDR-RC)
 *   domain: international_climate_governance/treaty_law/political_economy
 *
 * SUMMARY:
 *   This constraint represents the 'equity reading' of the Paris Agreement's
 *   Article 4 on Nationally Determined Contributions (NDCs), emphasizing
 *   Common But Differentiated Responsibilities and Respective Capabilities
 *   (CBDR-RC). It mandates structural distinctions between developed and
 *   developing states, requiring developed states to undertake more ambitious
 *   mitigation and provide support, while allowing developing states greater
 *   flexibility. This reading is a core tenet for many developing nations and
 *   equity coalitions, shaping their participation and demands within the
 *   international climate regime.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paris_article_4_ndc__equity_reading, 0.45).
domain_priors:suppression_score(paris_article_4_ndc__equity_reading, 0.3).
domain_priors:theater_ratio(paris_article_4_ndc__equity_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paris_article_4_ndc__equity_reading, tangled_rope).
narrative_ontology:human_readable(paris_article_4_ndc__equity_reading, "Paris Agreement Article 4 NDCs: Equity Reading (CBDR-RC)").
narrative_ontology:topic_domain(paris_article_4_ndc__equity_reading, "international_climate_governance/treaty_law/political_economy").

domain_priors:requires_active_enforcement(paris_article_4_ndc__equity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(paris_article_4_ndc__equity_reading, 'b114edd4-1f46-4163-83ff-e6bfef6cde20').
narrative_ontology:cs_kernel_codification('b114edd4-1f46-4163-83ff-e6bfef6cde20', fixed_text).
narrative_ontology:cs_authority_grounding('b114edd4-1f46-4163-83ff-e6bfef6cde20', lineage).
narrative_ontology:cs_interpretation_layer_present('b114edd4-1f46-4163-83ff-e6bfef6cde20').
narrative_ontology:cs_reading_relation('b114edd4-1f46-4163-83ff-e6bfef6cde20', paris_article_4_ndc__sovereigntist_reading, coexists_with).
narrative_ontology:cs_reading_relation('b114edd4-1f46-4163-83ff-e6bfef6cde20', paris_article_4_ndc__supranational_reading, coexists_with).
narrative_ontology:cs_axiom('b114edd4-1f46-4163-83ff-e6bfef6cde20', foundational, historical_responsibility_for_emissions).
narrative_ontology:cs_axiom_status(historical_responsibility_for_emissions, holdable).
narrative_ontology:cs_axiom_grounding('b114edd4-1f46-4163-83ff-e6bfef6cde20', historical_responsibility_for_emissions, deontological).
narrative_ontology:cs_axiom('b114edd4-1f46-4163-83ff-e6bfef6cde20', foundational, differentiated_capabilities_for_action).
narrative_ontology:cs_axiom_status(differentiated_capabilities_for_action, holdable).
narrative_ontology:cs_axiom_grounding('b114edd4-1f46-4163-83ff-e6bfef6cde20', differentiated_capabilities_for_action, empirically_contingent).
narrative_ontology:cs_reference_frame('b114edd4-1f46-4163-83ff-e6bfef6cde20', unfccc_cbdr_rc_principle).
narrative_ontology:cs_drift_state('b114edd4-1f46-4163-83ff-e6bfef6cde20', contemporary_climate_negotiations, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b114edd4-1f46-4163-83ff-e6bfef6cde20', '').
narrative_ontology:cs_kernel_id(paris_article_4_ndc__equity_reading, paris_article_4_ndc).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__equity_reading, developing_states).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__equity_reading, equity_coalitions).
narrative_ontology:constraint_victim(paris_article_4_ndc__equity_reading, developed_states).
narrative_ontology:constraint_victim(paris_article_4_ndc__equity_reading, fossil_fuel_industries_developed).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Expected to bear greater responsibility for emissions reductions and provide financial and technological support to developing states, reflecting historical emissions and capacity. They face pressure to increase ambition and transfer resources.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, developed_states, payer,
    institutional, generational, constrained, global).

% Retain policy space for economic development, receive support, and have less stringent emissions reduction targets. They advocate for historical responsibility and climate justice.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, developing_states, beneficiary,
    organized, generational, mobile, global).

% Advocate for the CBDR-RC principle, influencing negotiations and holding developed states accountable for their commitments. They gain leverage and veto power over supranational enforcement mechanisms that do not reflect equity.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, equity_coalitions, agenda_setter,
    organized, generational, constrained, global).

% Face increasing regulatory pressure and divestment campaigns in developed states due to more stringent NDC targets and carbon pricing mechanisms. Their business model is directly challenged by the equity reading's implications for developed states.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, fossil_fuel_industries_developed, payer,
    powerful, biographical, constrained, global).

% Facilitates the NDC process, compiles reports, and supports negotiations, but its enforcement capacity is limited by the consensus-based nature of international climate law and the strong equity demands of developing states.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, international_climate_secretariat, observer,
    institutional, generational, analytical, global).

% Advocate for stronger, legally binding international enforcement mechanisms for NDCs, but their proposals are often blocked by equity coalitions and sovereigntist states who prioritize national sovereignty or differentiated responsibilities.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, supranational_enforcement_advocates, excluded,
    moderate, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global climate action by establishing a framework where states contribute to emissions reductions based on their historical responsibility and respective capabilities, fostering trust and participation from developing nations.
% TRANSFER_FUNCTION: Transfers financial resources, technology, and emissions reduction burdens from developed states to developing states, while also transferring policy space and development flexibility to developing states.
% ABSENT_VOICES: Advocates for a purely technocratic, 'ratcheting' approach to NDCs without explicit equity considerations are marginalized, as are those who would push for a global carbon tax or a single, undifferentiated emissions budget. Their proposals are often seen as undermining national sovereignty or ignoring historical injustices.
% DISAPPEARANCE_RATIONALE: If the CBDR-RC principle vanished overnight, the Paris Agreement's delicate balance would collapse. Developing states would likely withdraw or refuse to participate meaningfully without differentiated responsibilities and support, leading to a fragmented and ineffective global climate regime. Developed states would face less pressure but also lose the legitimacy of a globally coordinated effort.
% FOUNDING_PROBLEM: The UNFCCC framework struggled to achieve universal participation and ambitious climate action due to deep divisions between developed and developing nations over historical responsibility for emissions and the burden of mitigation.
% FOUNDING_PROBLEM_CORROBORATION: Developing states and climate justice organizations consistently attest that the problem of historical injustice and unequal capacity remains live. Developed states acknowledge the principle but often contest its contemporary application, seeking more convergence in responsibilities. Independent analyses of climate vulnerability and historical emissions corroborate the ongoing relevance of differentiated responsibilities.
narrative_ontology:disappearance_verdict(paris_article_4_ndc__equity_reading, world_rearranges).
narrative_ontology:founding_problem_status(paris_article_4_ndc__equity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(paris_article_4_ndc__equity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(paris_article_4_ndc__equity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(paris_article_4_ndc__equity_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(paris_article_4_ndc__equity_reading_tests).
:- end_tests(paris_article_4_ndc__equity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate, reflecting the burden placed on developed states and their industries, but also the policy space granted to developing states. Suppression (0.30) is relatively low because the constraint relies on political pressure and negotiation rather than direct coercive enforcement, and developing states retain significant exit options if their demands for equity are not met. Theater ratio (0.20) is present as some developed states may rhetorically commit to CBDR-RC while seeking to minimize actual transfers or mitigation efforts. The claimed type is Tangled Rope because it genuinely coordinates global action while imposing asymmetric burdens and benefits, requiring active enforcement (political pressure, negotiation) to maintain.
 *
 * PERSPECTIVAL GAP:
 *   Developed states experience this as a moderately extractive constraint, limiting their economic choices and requiring transfers. Developing states experience it as a beneficial coordination mechanism that rectifies historical injustices and enables sustainable development. The engine's per-seat classification will reflect this divergence based on the declared roles and positional atoms.
 *
 * DIRECTIONALITY LOGIC:
 *   Developed states and their fossil fuel industries are targets (payers) due to increased mitigation and transfer obligations. Developing states and equity coalitions are beneficiaries, gaining policy space, financial support, and a stronger negotiating position. The international climate secretariat acts as an observer, facilitating but not directly enforcing. Supranational enforcement advocates are excluded, as their vision for binding, undifferentiated commitments is foreclosed by this equity-focused reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cbdr_rc_operationalization_ambiguity,
    'How precisely are ''common but differentiated responsibilities and respective capabilities'' (CBDR-RC) to be operationalized in practice, particularly regarding specific financial and technological transfer obligations?',
    'Negotiated agreements on specific targets for climate finance and technology transfer, or a clear methodology for assessing ''respective capabilities'' that is accepted by all parties.',
    'Clearer operationalization would reduce the ''theater ratio'' and ''resistance'' from developed states, potentially increasing ''extractiveness'' on them but also improving the overall effectiveness of the constraint. Ambiguity allows for continued negotiation and potential under-delivery.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cbdr_rc_operationalization_ambiguity, empirical, 'Ambiguity in the practical application of CBDR-RC.').

omega_variable(
    equity_vs_ambition_tradeoff,
    'To what extent does prioritizing equity (CBDR-RC) inherently limit the overall ambition and stringency of global climate action, and vice versa?',
    'Empirical analysis of climate policy outcomes in regimes that prioritize one over the other, or a conceptual framework that reconciles both goals without trade-offs.',
    'If a strong trade-off exists, the ''extractiveness'' on developed states might be lower than optimal for climate goals, or the ''policy space'' for developing states might be too broad. Resolving this could shift the balance of burdens and benefits, potentially altering the constraint''s classification towards a more purely coordinative (Rope) or more purely extractive (Snare) outcome depending on which is prioritized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equity_vs_ambition_tradeoff, conceptual, 'The inherent tension between equity principles and the ambition of climate targets.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine reading of the Paris Agreement kernel, or a political interpretation that over-claims textual grounding?',
    'Legal-textual analysis of the Paris Agreement''s language, combined with historical negotiation records and state practice, to assess the fidelity of this reading to the kernel''s original intent and subsequent evolution.',
    'If it''s a genuine reading, its legitimacy is reinforced. If it''s an over-claimed interpretation, its ''theater_ratio'' and ''resistance'' would increase, and its ''authority_grounding'' might shift from ''lineage'' to ''extraction'' or ''practice'' if maintained primarily by political power or customary action rather than textual fidelity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is one reading (''equity_reading'') of the ''paris_article_4_ndc'' kernel. Sibling readings (''sovereigntist_reading'', ''supranational_reading'') offer alternative interpretations of NDC obligations, differing on the role of national sovereignty, the stringency of commitments, and the extent of international accountability. This omega documents the irreducible uncertainty of which reading is ''correct'' or most textually grounded.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paris_article_4_ndc__equity_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pari_tr_t0, paris_article_4_ndc__equity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(pari_tr_t5, paris_article_4_ndc__equity_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(pari_tr_t10, paris_article_4_ndc__equity_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(pari_tr_t15, paris_article_4_ndc__equity_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement(pari_tr_t20, paris_article_4_ndc__equity_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(pari_be_t0, paris_article_4_ndc__equity_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(pari_be_t5, paris_article_4_ndc__equity_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(pari_be_t10, paris_article_4_ndc__equity_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(pari_be_t15, paris_article_4_ndc__equity_reading, base_extractiveness, 15, 0.46).
narrative_ontology:measurement(pari_be_t20, paris_article_4_ndc__equity_reading, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(pari_su_t0, paris_article_4_ndc__equity_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(pari_su_t5, paris_article_4_ndc__equity_reading, suppression_requirement, 5, 0.28).
narrative_ontology:measurement(pari_su_t10, paris_article_4_ndc__equity_reading, suppression_requirement, 10, 0.3).
narrative_ontology:measurement(pari_su_t15, paris_article_4_ndc__equity_reading, suppression_requirement, 15, 0.31).
narrative_ontology:measurement(pari_su_t20, paris_article_4_ndc__equity_reading, suppression_requirement, 20, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paris_article_4_ndc__equity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(paris_article_4_ndc__equity_reading, paris_article_4_ndc__sovereigntist_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__equity_reading, paris_article_4_ndc__supranational_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__equity_reading, global_climate_finance_mechanisms).
narrative_ontology:affects_constraint(paris_article_4_ndc__equity_reading, technology_transfer_regimes).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'paris_article_4_ndc' kernel. The 'equity_reading' emphasizes CBDR-RC, while the 'sovereigntist_reading' prioritizes national sovereignty and voluntary pledges, and the 'supranational_reading' focuses on binding commitments and international accountability. Each reading constitutes a structurally distinct constraint with different beneficiaries, victims, and classification profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
