% ============================================================================
% CONSTRAINT STORY: bretton_woods_treaty_substrate__neoliberal_convertibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bretton_woods_treaty_substrate__neoliberal_convertibility, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: bretton_woods_treaty_substrate__neoliberal_convertibility
 *   human_readable: Bretton Woods Neoliberal Convertibility Constraint
 *   domain: economic/institutional/political
 *
 * SUMMARY:
 *   The Bretton Woods system (1944–1971) and its successors institute a
 *   global regime of currency convertibility and capital account openness. In
 *   the neoliberal_convertibility reading, this constraint enables free
 *   international capital markets by prohibiting exchange controls and
 *   requiring convertibility. National governments that wish to retain
 *   capital controls, maintain exchange-rate pegs, or implement macroeconomic
 *   policies independent of capital market discipline are systematically
 *   pressured (via IMF conditionality, market discipline, or treaty
 *   obligation) to abandon these tools. The reading frames this as
 *   liberalization—freeing capital from artificial restriction. Alternative
 *   readings (keynesian_embedded_liberalism, sovereignty_defense) argue that
 *   the regime constrains legitimate policy autonomy and that the described
 *   'liberation' is extraction of policy space from democratically
 *   accountable governments to capital markets. This story instantiates the
 *   neoliberal_convertibility reading: capital mobility is the primary good,
 *   policy autonomy is the cost borne by nations with weaker currencies or
 *   less developed financial systems. The claim/metric gap is deliberate: the
 *   constraint is CLAIMED as tangled_rope (coordination + enforcement) while
 *   the measurements and omega variables document the growing extractiveness
 *   and theater ratios, inviting empirical evaluation of whether coordination
 *   function persists or has atrophied.
 *
 * KEY AGENTS:
 *   - international_financial_institutions (IMF/World Bank): agenda-setter, enforces convertibility regime via loan conditionality
 *   - capital_exporting_nations (US, Europe, Japan): beneficiaries, their firms profit from unrestricted capital deployment
 *   - multinational_corporations: beneficiaries, operate across borders via capital mobility rights
 *   - policy_autonomous_nations: victims/payers, constrained from using capital controls or industrial policy
 *   - capital_importing_nations: payers, must maintain openness despite financial vulnerability
 *   - domestic_labor_coalitions: payers and excluded, bear dislocation costs from capital reallocation, absent from governance
 *   - heterodox_economists: observers, document instability and redistributive effects outside the beneficiary narrative
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.68).
domain_priors:suppression_score(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.71).
domain_priors:theater_ratio(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, extractiveness, 0.68).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bretton_woods_treaty_substrate__neoliberal_convertibility, tangled_rope).
narrative_ontology:human_readable(bretton_woods_treaty_substrate__neoliberal_convertibility, "Bretton Woods Neoliberal Convertibility Constraint").
narrative_ontology:topic_domain(bretton_woods_treaty_substrate__neoliberal_convertibility, "economic/institutional/political").

domain_priors:requires_active_enforcement(bretton_woods_treaty_substrate__neoliberal_convertibility).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bretton_woods_treaty_substrate__neoliberal_convertibility, 'fba7ae27-5705-459d-965f-d17d8e842d3d').
narrative_ontology:cs_kernel_codification('fba7ae27-5705-459d-965f-d17d8e842d3d', fixed_text).
narrative_ontology:cs_authority_grounding('fba7ae27-5705-459d-965f-d17d8e842d3d', extraction).
narrative_ontology:cs_interpretation_layer_present('fba7ae27-5705-459d-965f-d17d8e842d3d').
narrative_ontology:cs_reading_relation('fba7ae27-5705-459d-965f-d17d8e842d3d', bretton_woods_treaty_substrate__keynesian_embedded_liberalism, forecloses).
narrative_ontology:cs_reading_relation('fba7ae27-5705-459d-965f-d17d8e842d3d', bretton_woods_treaty_substrate__sovereignty_defense, coexists_with).
narrative_ontology:cs_axiom('fba7ae27-5705-459d-965f-d17d8e842d3d', foundational, capital_mobility_universally_beneficial).
narrative_ontology:cs_axiom_status(capital_mobility_universally_beneficial, holdable).
narrative_ontology:cs_axiom_grounding('fba7ae27-5705-459d-965f-d17d8e842d3d', capital_mobility_universally_beneficial, empirically_contingent).
narrative_ontology:cs_axiom('fba7ae27-5705-459d-965f-d17d8e842d3d', foundational, policy_autonomy_subordinate_to_capital_discipline).
narrative_ontology:cs_axiom_status(policy_autonomy_subordinate_to_capital_discipline, holdable).
narrative_ontology:cs_axiom_grounding('fba7ae27-5705-459d-965f-d17d8e842d3d', policy_autonomy_subordinate_to_capital_discipline, deontological).
narrative_ontology:cs_reference_frame('fba7ae27-5705-459d-965f-d17d8e842d3d', liberal_international_order_via_capital_mobility).
narrative_ontology:cs_drift_state('fba7ae27-5705-459d-965f-d17d8e842d3d', contemporary_2025, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('fba7ae27-5705-459d-965f-d17d8e842d3d', '').
narrative_ontology:cs_kernel_id(bretton_woods_treaty_substrate__neoliberal_convertibility, bretton_woods_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__neoliberal_convertibility, international_financial_institutions).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__neoliberal_convertibility, capital_exporting_nations).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__neoliberal_convertibility, multinational_corporations).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__neoliberal_convertibility, policy_autonomous_nations).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__neoliberal_convertibility, capital_importing_nations).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__neoliberal_convertibility, domestic_labor_coalitions).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bretton_woods_treaty_substrate__neoliberal_convertibility, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(bretton_woods_treaty_substrate__neoliberal_convertibility, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bretton_woods_treaty_substrate__neoliberal_convertibility_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bretton_woods_treaty_substrate__neoliberal_convertibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bretton_woods_treaty_substrate__neoliberal_convertibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness trajectory shows a clear upward climb from 0.35 (1944: genuine coordination problem, many nations still in recovery, capital controls seen as temporary) through 0.68 (2025: convertibility widely achieved, yet constraint persists and intensifies despite the founding problem being solved). The suppression_requirement mirrors this: the regime must actively suppress capital-control deployments, labor-protective tariffs, and heterodox monetary policies. Theater_ratio rises from near-zero (early period: real coordination challenge) to 0.42 (contemporary: significant portion of enforcement activity defends ideological commitment to capital mobility rather than solving coordination problems). These measurements are authored on one shared time grid covering the full 1944–2025 interval. The interpretation: early extractiveness reflects genuine coordination costs + modest extraction; by 1995, extractiveness reflects primarily extraction masked by coordination rhetoric (theater). Rising suppression_requirement documents institutional hardening—the constraint requires more active defense as alternative development models gain evidence and appeal. This is NOT a sign the constraint is loosening; it is a sign the constraint's basis has shifted from solving a real problem to defending a redistributive arrangement. The beneficiary and victim declarations reflect power differentials: capital-exporting nations have arbitrage-grade exit (can move capital elsewhere), policy-autonomous nations have identity_locked exit (exiting the regime means ceasing to participate in international credit markets, which for many is institutionally impossible—the nation-state's legitimacy in modern capitalism depends on integration into the dollar-based financial system). This asymmetry in exit is the foundation for directionality divergence: the IFI agenda-setter and capital exporters sit at low d (beneficiary end); policy-autonomous nations sit at high d (target end).
 *
 * PERSPECTIVAL GAP:
 *   From the IFI and capital-exporting-nation seats, the constraint is genuine coordination: currency stability, capital mobility, and rule-based international finance solve real problems and enable growth. From policy-autonomous-nation and domestic-labor seats, the constraint is institutional extraction: policy tools that would address unemployment and inequality are categorically off-limits; capital flight is a weapon that cannot be countered; macro policy is constrained by market discipline rather than democratic choice. The engine should compute this as a seat divergence: the IFI seat experiences rope (real coordination maintained); the policy-autonomous-nation seat experiences snare (extraction defended by mandatory regime membership). The structural asymmetry is power: IFIs have organized power to enforce, nation-states have institutional power but asymmetric exit costs, labor has organized power but is excluded from governance.
 *
 * DIRECTIONALITY LOGIC:
 *   Capital-exporting nations and IFIs are structural beneficiaries: they control the rules, collect legitimacy from governance, and their capital benefits directly from openness. Directionality near 0.0 (beneficiary end). Policy-autonomous nations are victims/targets: they cannot deploy the policy tools that would serve their constituents, must maintain openness despite vulnerability, and face market discipline rather than democratic accountability. Their directionality approaches 1.0 (target end). Domestic labor coalitions are secondary victims: they experience unemployment from capital reallocation, are excluded from governance, and cannot advocate for capital controls. Directionality near 1.0. Heterodox economists observe but do not collect from the constraint; they occupy the analytical seat. The measurement series on suppression_requirement specifically tracks enforcement intensity: rising suppression from 0.42 to 0.71 reflects the regime's growing reliance on coercive institutional pressure (not voluntary coordination) to maintain the constraint as its founding-problem justification erodes.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status field is 'contested' because the regime's own beneficiaries (IFIs, capital exporters) claim the founding problem (Beggar Thy Neighbor currency wars) remains live, while heterodox observers and developing-nation policymakers claim it is solved. The rising theater_ratio (0.08 to 0.42) documents a shift from functional coordination to performance: in the early period, currency stability really did solve a coordination problem; by the contemporary period, enforcement activity increasingly defends ideological commitment (the belief that capital mobility is universally beneficial) rather than preventing currency wars (which are not a live threat). The measurement trajectory supports the mandatrophy hypothesis: the constraint persists because IFIs and capital-exporting interests have institutional power to maintain it, not because it solves a pressing coordination problem. The growing suppression_requirement reflects the constraint's increasing reliance on coercion (IMF conditionality, market discipline, institutional pressure) as voluntary coordination erodes. This is classic zombie-constraint behavior: the founding problem is dead, but the institutional beneficiaries maintain the constraint through organizational inertia and extracted rents. A genuine mandatrophy resolution would require acknowledging that capital controls are not violations but legitimate policy tools, and that convertibility guarantees can be decoupled from capital account openness requirements.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_kernel,
    'Is the measured constraint primarily a solution to a genuine coordination problem (preventing competitive devaluations, enabling stable international investment), or is it primarily an institutional mechanism for capital exporting nations to extract policy autonomy from others?',
    'Compare counterfactual welfare: if policy-autonomous nations could implement capital controls and industrial policy independently, would their economic outcomes improve or deteriorate relative to the convertibility regime? Examine whether capital mobility creates net productivity gains or primarily redistributes wealth and absorbs adjustment costs elsewhere.',
    'If coordination-dominant, the constraint should classify as rope with modest extraction. If extraction-dominant, it is tangled_rope (real coordination function layered with asymmetric extraction). This kernel contest between readings hinges on this empirical question.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_vs_extraction_kernel, empirical, 'Whether Bretton Woods convertibility solves a genuine coordination problem or serves primarily as an extraction mechanism.').

omega_variable(
    founding_problem_obsolescence,
    'The founding problem (preventing Beggar Thy Neighbor currency wars and devaluation spirals) is widely considered solved: modern international institutions, trade depth, and mutual economic interdependence make competitive devaluations self-defeating. Does the persistence of capital controls restrictions reflect the solution being maintained, or does it reflect the constraint''s original function being replaced by extractive institutional inertia?',
    'Historical comparison: identify the specific moment(s) when the foundational problem ceased to be active (candidate: the 1960s-1970s as global trade matured), and examine whether capital controls restrictions intensified, relaxed, or remained stable after that point. Track whether IMF conditionality on liberalization increased or decreased as the coordination rationale weakened.',
    'If the founding problem is dead and the constraint persists primarily through institutional inertia maintained by beneficiaries, it moves toward piton classification and mandatrophy resolution. The founding_problem_status omega and the theater_ratio measurement series both track this question.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether the founding problem persists or has been superseded by institutional capture and extraction.').

omega_variable(
    reading_foreclosure_via_development_outcomes,
    'The neoliberal_convertibility reading (this constraint) asserts that policy autonomy restrictions enable free capital markets and growth. The keynesian_embedded_liberalism reading asserts that capital controls protect policy space and domestic welfare. Can both readings be simultaneously valid in different institutional contexts, or does empirical evidence from development outcomes foreclose one reading?',
    'Longitudinal analysis: compare welfare outcomes (GDP growth, employment, inequality, financial stability) across nations grouped by their adherence to convertibility (open capital accounts) vs. those that deployed capital controls despite regime pressure. If capital-control deployers systematically outperform or underperform on welfare metrics, that evidence forecloses one reading''s core claim.',
    'If evidence systematically favors one outcome cluster, the foreclosed reading would need to reframe its foundation axiom (from ''capital mobility enables stable growth'' to ''capital mobility enables financial integration despite development costs''). This is the empirical-contingency pathway by which reading_relations can shift from coexists_with to forecloses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_via_development_outcomes, empirical, 'Whether the neoliberal_convertibility and keynesian_embedded_liberalism readings remain coexistent or whether empirical evidence forecloses one.').

omega_variable(
    structural_separation_of_coordination_and_exclusion,
    'Is the capital account openness requirement structurally inseparable from the currency convertibility coordination function, or is openness a separable extraction mechanism that could be removed while preserving the genuine coordination gains?',
    'Policy analysis: can a framework exist that provides convertibility guarantees for trade-related flows while permitting capital controls on speculative flows? If yes, the two mechanisms are separable and the constraint''s extraction component is distinguishable from its coordination component.',
    'If separable, the measured extractiveness reflects a policy choice to bundle them, not a structural necessity. The constraint would be better modeled as two constraints (one rope for trade-convertibility coordination, one snare for capital account openness enforcement). If inseparable, the high extractiveness is an intrinsic cost of the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_separation_of_coordination_and_exclusion, conceptual, 'Whether capital account openness is structurally necessary for convertibility coordination or a separable extraction mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_treaty_substrate__neoliberal_convertibility, 1944, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bret_tr_t1944, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1944, 0.08).
narrative_ontology:measurement(bret_tr_t1971, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1971, 0.12).
narrative_ontology:measurement(bret_tr_t1980, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1980, 0.22).
narrative_ontology:measurement(bret_tr_t1995, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1995, 0.36).
narrative_ontology:measurement(bret_tr_t2010, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(bret_tr_t2025, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(bret_be_t1944, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1944, 0.35).
narrative_ontology:measurement(bret_be_t1971, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1971, 0.42).
narrative_ontology:measurement(bret_be_t1980, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1980, 0.54).
narrative_ontology:measurement(bret_be_t1995, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1995, 0.61).
narrative_ontology:measurement(bret_be_t2010, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 2010, 0.66).
narrative_ontology:measurement(bret_be_t2025, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(bret_su_t1944, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1944, 0.42).
narrative_ontology:measurement(bret_su_t1971, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1971, 0.48).
narrative_ontology:measurement(bret_su_t1980, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1980, 0.58).
narrative_ontology:measurement(bret_su_t1995, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1995, 0.66).
narrative_ontology:measurement(bret_su_t2010, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 2010, 0.69).
narrative_ontology:measurement(bret_su_t2025, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 2025, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bretton_woods_treaty_substrate__neoliberal_convertibility, resource_allocation).
narrative_ontology:boltzmann_floor_override(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.18).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__neoliberal_convertibility, imf_conditionality_regime).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__neoliberal_convertibility, speculative_capital_flow_volatility).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__neoliberal_convertibility, financial_system_cascades).

% DUAL FORMULATION NOTE:
% This constraint is part of the bretton_woods_treaty_substrate family, which decomposes into three structurally distinct constraints corresponding to three readings of the same treaty foundation: keynesian_embedded_liberalism (reading 1: coordination-to-protect), neoliberal_convertibility (reading 2: coordination-to-enable, this constraint), sovereignty_defense (reading 3: external-discipline constraint). Each reading has different victim sets, beneficiaries, and foundational axioms. The epsilon values differ significantly across readings: keynesian reading assigns lower extraction (constraint seen as protective), neoliberal reading assigns higher extraction (constraint seen as liberatory but imposing costs on policy-autonomous nations), sovereignty reading focuses on external authority rather than extraction. The three constraints form a constraint family linked by network.affects_constraints; they share institutional substrate but diverge on normative framing and structural relationships.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bretton_woods_treaty_substrate__neoliberal_convertibility, institutional, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
