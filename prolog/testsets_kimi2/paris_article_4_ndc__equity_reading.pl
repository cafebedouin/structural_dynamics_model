% ============================================================================
% CONSTRAINT STORY: paris_article_4_ndc__equity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: paris_article_4_ndc__equity_reading
 *   human_readable: Paris Article 4 NDC Equity Reading (CBDR-Structured Differentiation)
 *   domain: international_climate_governance/treaty_law/political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates the equity_reading of the
 *   paris_article_4_ndc kernel. Under this reading, NDCs are interpreted
 *   through Common But Differentiated Responsibilities (CBDR) as requiring
 *   structural distinctions between developed and developing states.
 *   Developed states face binding constraints and transfer obligations;
 *   developing states retain policy space; equity coalitions gain veto power
 *   over supranational enforcement. This reading structurally competes with
 *   the sovereigntist_reading (voluntary pledges preserving national energy
 *   sovereignty for all) and the supranational_reading (binding net-zero
 *   commitments with international accountability for all major emitters).
 *   The authored metrics describe a moderately extractive, asymmetrically
 *   enforced coordination mechanism â the engine measures the divergence
 *   from the pure coordination claim.
 *
 * KEY AGENTS:
 *   - developed_states: Primary target (institutional/constrained) â bears binding obligations, financial transfers, and constrained policy autonomy
 *   - developing_states: Primary beneficiary (institutional/constrained) â retains policy space and eligibility for support
 *   - equity_coalitions: Agenda-setter and secondary beneficiary (organized/constrained) â administers CBDR interpretation and wields veto over enforcement
 *   - small_island_developing_states: Excluded voice (moderate/constrained) â overridden by equity coalition veto
 *   - independent_climate_science: Analytical observer (analytical/analytical) â assesses the emissions gap
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paris_article_4_ndc__equity_reading, 0.55).
domain_priors:suppression_score(paris_article_4_ndc__equity_reading, 0.48).
domain_priors:theater_ratio(paris_article_4_ndc__equity_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paris_article_4_ndc__equity_reading, tangled_rope).
narrative_ontology:human_readable(paris_article_4_ndc__equity_reading, "Paris Article 4 NDC Equity Reading (CBDR-Structured Differentiation)").
narrative_ontology:topic_domain(paris_article_4_ndc__equity_reading, "international_climate_governance/treaty_law/political_economy").

domain_priors:requires_active_enforcement(paris_article_4_ndc__equity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(paris_article_4_ndc__equity_reading, '7de2dd53-2378-4a18-8359-897d3af9c118').
narrative_ontology:cs_kernel_codification('7de2dd53-2378-4a18-8359-897d3af9c118', formalized).
narrative_ontology:cs_authority_grounding('7de2dd53-2378-4a18-8359-897d3af9c118', lineage).
narrative_ontology:cs_interpretation_layer_present('7de2dd53-2378-4a18-8359-897d3af9c118').
narrative_ontology:cs_reading_relation('7de2dd53-2378-4a18-8359-897d3af9c118', paris_article_4_ndc__sovereigntist_reading, coexists_with).
narrative_ontology:cs_reading_relation('7de2dd53-2378-4a18-8359-897d3af9c118', paris_article_4_ndc__supranational_reading, forecloses).
narrative_ontology:cs_axiom('7de2dd53-2378-4a18-8359-897d3af9c118', foundational, developed_asymmetric_binding).
narrative_ontology:cs_axiom_status(developed_asymmetric_binding, holdable).
narrative_ontology:cs_axiom_grounding('7de2dd53-2378-4a18-8359-897d3af9c118', developed_asymmetric_binding, conventional).
narrative_ontology:cs_axiom('7de2dd53-2378-4a18-8359-897d3af9c118', foundational, developing_state_enforcement_veto).
narrative_ontology:cs_axiom_status(developing_state_enforcement_veto, holdable).
narrative_ontology:cs_axiom_grounding('7de2dd53-2378-4a18-8359-897d3af9c118', developing_state_enforcement_veto, conventional).
narrative_ontology:cs_reference_frame('7de2dd53-2378-4a18-8359-897d3af9c118', cbdr_equity_framework).
narrative_ontology:cs_drift_state('7de2dd53-2378-4a18-8359-897d3af9c118', post_paris_implementation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7de2dd53-2378-4a18-8359-897d3af9c118', '').
narrative_ontology:cs_kernel_id(paris_article_4_ndc__equity_reading, paris_article_4_ndc).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__equity_reading, developing_states).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__equity_reading, equity_coalitions).
narrative_ontology:constraint_victim(paris_article_4_ndc__equity_reading, developed_states).
narrative_ontology:constraint_vindicates(paris_article_4_ndc__equity_reading, common_but_differentiated_responsibilities_doctrine).
narrative_ontology:constraint_vindicates(paris_article_4_ndc__equity_reading, polluter_pays_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Assume asymmetric binding mitigation and financial obligations under the CBDR-interpreted NDC regime; constrained by diplomatic architecture, treaty ratification, and reputational costs from exiting the Paris framework.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, developed_states, payer,
    institutional, generational, constrained, global).

% Retain policy autonomy over energy and industrial development pathways; eligible for climate finance and technology transfer; shielded from symmetric binding targets by the CBDR firewall.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, developing_states, beneficiary,
    institutional, generational, constrained, global).

% Exercise interpretive authority over equitable effort-sharing; wield veto power over supranational enforcement proposals and ambition ratchets that would bind developing states symmetrically; gain institutional legitimacy from preserving the CBDR firewall.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, equity_coalitions, agenda_setter,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(paris_article_4_ndc__equity_reading, equity_coalitions, beneficiary).

% Demand higher ambition and survival-aligned binding targets for all major emitters; their preferred enforcement architecture is systematically overridden by larger developing-state equity coalitions invoking CBDR to protect policy space.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, small_island_developing_states, excluded,
    moderate, generational, constrained, global).

% Assess aggregate NDC trajectories against temperature targets; report that CBDR-based asymmetric ambition and the lack of supranational enforcement create a persistent emissions gap.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, independent_climate_science, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(paris_article_4_ndc__equity_reading, diffuse).
narrative_ontology:fixing_cost_class(paris_article_4_ndc__equity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global mitigation action across states with vastly different historical emissions, economic capacities, and development needs by allowing heterogeneous NDC commitments while preserving a common transparency framework.
% TRANSFER_FUNCTION: Moves binding mitigation effort, financial resources, and technology from developed states to developing states; moves veto power over enforcement architecture from supranational accountability mechanisms to developing-state equity coalitions.
% ABSENT_VOICES: Small island developing states and least developed countries that demand survival-aligned binding targets for all major emitters; future generations; advocates of symmetric supranational enforcement.
% DISAPPEARANCE_RATIONALE: If the CBDR-interpreted NDC equity framework vanished, developed states would abandon asymmetric obligations, the G77 coalition would fracture, climate finance flows would halt, and the Paris architecture would collapse into either universal voluntarism or symmetric binding rules.
% FOUNDING_PROBLEM: How to secure collective emissions reductions when states have unequal historical responsibility and unequal capacity to act, without freezing development pathways in the Global South.
% FOUNDING_PROBLEM_CORROBORATION: UNFCCC Secretariat and G77+China attest the problem remains live. Independent economists and developed-state negotiators (outside the primary beneficiary set) attest that the binary developed/developing distinction no longer tracks real economic capacity for major emerging economies, undermining the original justification.
narrative_ontology:disappearance_verdict(paris_article_4_ndc__equity_reading, world_rearranges).
narrative_ontology:founding_problem_status(paris_article_4_ndc__equity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(paris_article_4_ndc__equity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(paris_article_4_ndc__equity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(paris_article_4_ndc__equity_reading, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(paris_article_4_ndc__equity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(paris_article_4_ndc__equity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(paris_article_4_ndc__equity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.55) because the constraint genuinely coordinates heterogeneous capacities but extracts binding effort and finance from developed states. Suppression (0.48) reflects the active diplomatic blocking of supranational enforcement alternatives. Theater ratio (0.30) captures the performative dimension of equity rhetoric at COPs, where CBDR is invoked to defend inaction by major emerging economies. Accessibility collapse (0.50) reflects that uniform binding alternatives are structurally blocked but not erased from discourse. Resistance (0.45) is authored from sustained pushback by developed states and high-ambition parties against asymmetric obligation.
 *
 * PERSPECTIVAL GAP:
 *   The developed-state seat experiences the constraint as extractive and asymmetrically binding, while the developing-state seat experiences it as protective policy space and legitimate entitlement. The equity coalition seat experiences it as a source of institutional authority and veto power. The engine computes these divergent classifications from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Developed_states are declared victims/payers (d toward target end) because the constraint extracts binding obligations and transfers. Developing_states and equity_coalitions are declared beneficiaries (d toward beneficiary end) because the constraint delivers policy space, finance access, and veto authority. Small island states are excluded because their survival interest in universal binding action is overridden by the equity veto.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by requiring both coordination function (common transparency framework, heterogeneous NDCs) and asymmetric extraction (developed binding, developing shielded). Without the coordination function, it would be a snare of pure developing-state privilege; without the asymmetric extraction, it would be a rope of uniform voluntarism or a scaffold of transitional support.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    equity_reading_kernel_location,
    'This constraint is the equity_reading of kernel paris_article_4_ndc. How would classification change if the sovereigntist_reading (voluntary pledges preserving energy sovereignty for all states) or supranational_reading (binding net-zero commitments with international accountability for all major emitters) were adopted instead?',
    'Comparative analysis of sibling constraint stories in the same kernel family.',
    'The equity reading produces asymmetric extraction on developed states and veto power for equity coalitions; the sovereigntist reading would flatten extraction uniformly low across all states; the supranational reading would produce symmetric extraction on all major emitters.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(equity_reading_kernel_location, conceptual, 'Structural delta between equity reading and sibling readings of the same kernel').

omega_variable(
    developed_developing_binary_validity,
    'Does the structural distinction between developed and developing states under CBDR still track real capacity and responsibility differentials, or has economic emergence rendered the binary a legacy construct?',
    'Empirical analysis of per-capita income, cumulative emissions, and current mitigation capacity of major emerging economies relative to developed-state baselines.',
    'If the binary is obsolete, the equity reading''s differentiation becomes a Snare of legacy privilege; if still valid, the asymmetry remains a genuine coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(developed_developing_binary_validity, empirical, 'Empirical validity of the developed/developing binary under CBDR').

omega_variable(
    supranational_veto_scope,
    'Does the equity coalition''s veto power over supranational enforcement foreclose the supranational reading entirely, or merely create a structural barrier that could be overcome with sufficient political will?',
    'Treaty interpretation: whether Article 4 as read through CBDR leaves legal room for supranational enforcement, or structurally reserves compliance to national determination.',
    'If foreclosed, the equity reading and supranational reading are mutually exclusive within any single legal framework; if merely obstructed, they coexist as rival political projects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supranational_veto_scope, conceptual, 'Whether equity veto forecloses or merely obstructs supranational enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paris_article_4_ndc__equity_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(paris_equity_tr_t0, paris_article_4_ndc__equity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(paris_equity_tr_t2, paris_article_4_ndc__equity_reading, theater_ratio, 2, 0.22).
narrative_ontology:measurement(paris_equity_tr_t4, paris_article_4_ndc__equity_reading, theater_ratio, 4, 0.25).
narrative_ontology:measurement(paris_equity_tr_t6, paris_article_4_ndc__equity_reading, theater_ratio, 6, 0.28).
narrative_ontology:measurement(paris_equity_tr_t8, paris_article_4_ndc__equity_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(paris_equity_tr_t10, paris_article_4_ndc__equity_reading, theater_ratio, 10, 0.32).

% Extraction over time
narrative_ontology:measurement(paris_equity_be_t0, paris_article_4_ndc__equity_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(paris_equity_be_t2, paris_article_4_ndc__equity_reading, base_extractiveness, 2, 0.45).
narrative_ontology:measurement(paris_equity_be_t4, paris_article_4_ndc__equity_reading, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(paris_equity_be_t6, paris_article_4_ndc__equity_reading, base_extractiveness, 6, 0.5).
narrative_ontology:measurement(paris_equity_be_t8, paris_article_4_ndc__equity_reading, base_extractiveness, 8, 0.53).
narrative_ontology:measurement(paris_equity_be_t10, paris_article_4_ndc__equity_reading, base_extractiveness, 10, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(paris_equity_su_t0, paris_article_4_ndc__equity_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(paris_equity_su_t2, paris_article_4_ndc__equity_reading, suppression_requirement, 2, 0.43).
narrative_ontology:measurement(paris_equity_su_t4, paris_article_4_ndc__equity_reading, suppression_requirement, 4, 0.46).
narrative_ontology:measurement(paris_equity_su_t6, paris_article_4_ndc__equity_reading, suppression_requirement, 6, 0.48).
narrative_ontology:measurement(paris_equity_su_t8, paris_article_4_ndc__equity_reading, suppression_requirement, 8, 0.5).
narrative_ontology:measurement(paris_equity_su_t10, paris_article_4_ndc__equity_reading, suppression_requirement, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paris_article_4_ndc__equity_reading, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is one member of the paris_article_4_ndc kernel family, decomposed from the natural-language concept of NDCs under the Paris Agreement into three structurally distinct readings: equity_reading, sovereigntist_reading, and supranational_reading. Each carries a different epsilon, beneficiary structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
