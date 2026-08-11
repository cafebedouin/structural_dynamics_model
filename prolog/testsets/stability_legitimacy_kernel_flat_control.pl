% ============================================================================
% CONSTRAINT STORY: stability_legitimacy_kernel_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_stability_legitimacy_kernel_flat_control, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:flat_control_of/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: stability_legitimacy_kernel_flat_control
 *   human_readable: Stability-Legitimacy Kernel: Rulers' Resource Distribution Must Be Accepted as Legitimate
 *   domain: political_economy/surveillance_studies/democratic_theory
 *
 * SUMMARY:
 *   Across radically different regimes — the GDR's Stasi-administered
 *   socialist state, Piketty's analysis of wealth concentration in liberal
 *   democracies, the Trump administration's populist economic nationalism,
 *   and contemporary conflicts over data-center siting — a single shared
 *   commitment recurs: that political stability depends on the ruled
 *   accepting the legitimacy of the rulers' distribution of resources. No
 *   actor in this space argues for abandoning the commitment itself; GDR
 *   leadership built an entire surveillance apparatus to manufacture the
 *   appearance of accepted legitimacy, Piketty argues the acceptance is
 *   eroding as concentration outpaces growth, Trump-era rhetoric attempts to
 *   relegitimate an unequal distribution through nationalist and populist
 *   framing, and data-center opponents contest specific applications of
 *   resource allocation (land, water, power) without challenging the
 *   underlying premise that some legitimating story is owed to them. This
 *   flat construction treats the commitment as one constraint with one ε,
 *   authored from the substrate rather than decomposed into separate readings
 *   — the contestation surfaces instead in how differently each stakeholder
 *   seat experiences the same kernel.
 *
 * KEY AGENTS:
 *   - incumbent_political_leadership: agenda_setter (institutional/arbitrage) — administers the legitimacy-manufacturing apparatus
 *   - concentrated_capital_holders: beneficiary (powerful/mobile) — profits from the accepted distribution, can relocate if narratives fail
 *   - surveilled_populations: payer (powerless/trapped) — bears the enforcement costs of maintaining perceived legitimacy
 *   - displaced_local_residents: payer (powerless/constrained) — bears siting costs justified by legitimacy narratives about development
 *   - economically_squeezed_middle_strata: payer/beneficiary (moderate/constrained) — the group whose continued belief the kernel most depends on
 *   - data_center_opponents: excluded (moderate/constrained) — contest specific distributions without standing to contest the kernel itself
 *   - critical_political_economists: observer (analytical/analytical) — document historical patterns of legitimacy success and failure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(stability_legitimacy_kernel_flat_control, 0.61).
domain_priors:suppression_score(stability_legitimacy_kernel_flat_control, 0.7).
domain_priors:theater_ratio(stability_legitimacy_kernel_flat_control, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(stability_legitimacy_kernel_flat_control, extractiveness, 0.61).
narrative_ontology:constraint_metric(stability_legitimacy_kernel_flat_control, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(stability_legitimacy_kernel_flat_control, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(stability_legitimacy_kernel_flat_control, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(stability_legitimacy_kernel_flat_control, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(stability_legitimacy_kernel_flat_control, tangled_rope).
narrative_ontology:human_readable(stability_legitimacy_kernel_flat_control, "Stability-Legitimacy Kernel: Rulers' Resource Distribution Must Be Accepted as Legitimate").
narrative_ontology:topic_domain(stability_legitimacy_kernel_flat_control, "political_economy/surveillance_studies/democratic_theory").

domain_priors:requires_active_enforcement(stability_legitimacy_kernel_flat_control).

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(stability_legitimacy_kernel_flat_control, stability_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(stability_legitimacy_kernel_flat_control, incumbent_political_leadership).
narrative_ontology:constraint_beneficiary(stability_legitimacy_kernel_flat_control, concentrated_capital_holders).
narrative_ontology:constraint_victim(stability_legitimacy_kernel_flat_control, surveilled_populations).
narrative_ontology:constraint_victim(stability_legitimacy_kernel_flat_control, displaced_local_residents).
narrative_ontology:constraint_victim(stability_legitimacy_kernel_flat_control, economically_squeezed_middle_strata).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(stability_legitimacy_kernel_flat_control, economically_squeezed_middle_strata).
narrative_ontology:constraint_vindicates(stability_legitimacy_kernel_flat_control, social_order_requires_perceived_fairness).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the terms of the resource distribution and administers the apparatus (propaganda, patronage, security services, or in the contemporary case, executive orders and regulatory capture) that produces perceived legitimacy for that distribution. Collects continued rule as the direct payoff; can adjust rhetoric or enforcement intensity but cannot abandon the legitimacy claim without losing the mandate to rule.
narrative_ontology:constraint_stakeholder(stability_legitimacy_kernel_flat_control, incumbent_political_leadership, agenda_setter,
    institutional, generational, arbitrage, national).

% Hold disproportionate shares of wealth and capital income (the Piketty r>g dynamic) and benefit from a political order that frames this concentration as earned, natural, or necessary for growth. Can relocate capital across jurisdictions if legitimacy narratives fail in one polity, giving them exit options the ruled population lacks.
narrative_ontology:constraint_stakeholder(stability_legitimacy_kernel_flat_control, concentrated_capital_holders, beneficiary,
    powerful, generational, mobile, global).

% Live under monitoring and information control (GDR Stasi files, contemporary data-broker aggregation) whose stated purpose is preserving order but whose function is suppressing contestation of the resource distribution. Cannot exit the jurisdiction or the data economy without extraordinary cost; bear the surveillance as the price of the regime's stability claim.
narrative_ontology:constraint_stakeholder(stability_legitimacy_kernel_flat_control, surveilled_populations, payer,
    powerless, biographical, trapped, national).

% Face land, water, and power costs imposed by large infrastructure (data centers) built under a legitimacy narrative of economic development and national competitiveness. Their objections are processed through zoning hearings and public comment that rarely alter siting decisions; leaving means abandoning homes and communities.
narrative_ontology:constraint_stakeholder(stability_legitimacy_kernel_flat_control, displaced_local_residents, payer,
    powerless, biographical, constrained, local).

% Experience stagnant wages and rising asset costs while being told the system rewards effort. They retain some legal and political exit (voting, litigation, media appeals) but lack the capital mobility of concentrated holders; they are the group whose continued belief in the fairness of the distribution the kernel most depends on.
narrative_ontology:constraint_stakeholder(stability_legitimacy_kernel_flat_control, economically_squeezed_middle_strata, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(stability_legitimacy_kernel_flat_control, economically_squeezed_middle_strata, beneficiary).

% Organize against specific infrastructure projects, arguing the resource distribution (land, water, electricity, tax breaks) underlying the project is illegitimate. Their objections are heard in local forums but rarely reach the level where the underlying legitimacy claim itself could be adjudicated; they contest the distribution without having standing to contest the kernel.
narrative_ontology:constraint_stakeholder(stability_legitimacy_kernel_flat_control, data_center_opponents, excluded,
    moderate, biographical, constrained, local).

% Study the historical pattern (Piketty's data on capital concentration, comparative regime studies of the GDR and contemporary populist administrations) documenting when and how legitimacy claims fail or hold. They have no material stake in the outcome but their analysis can either bolster or undermine the standing legitimacy narrative.
narrative_ontology:constraint_stakeholder(stability_legitimacy_kernel_flat_control, critical_political_economists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(stability_legitimacy_kernel_flat_control, diffuse).
narrative_ontology:fixing_cost_class(stability_legitimacy_kernel_flat_control, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A polity of any scale needs the ruled to accept, at least provisionally, that the existing distribution of resources and authority is not worth overturning through force — this genuinely solves a coordination problem (avoiding civil conflict, enabling investment horizons, permitting orderly succession of power) that a permanently contested distribution cannot solve.
% TRANSFER_FUNCTION: Moves continued compliance, tax revenue, labor, and tolerance of surveillance/enforcement costs from the ruled to the ruling and capital-holding strata, in exchange for a legitimacy narrative (ideological, procedural, or performance-based) that the distribution is fair, necessary, or inevitable.
% ABSENT_VOICES: Populations under the most intensive surveillance (GDR-era informants' targets, contemporary data-subject populations with no meaningful consent mechanism) and residents displaced by infrastructure sited under 'competitiveness' rhetoric rarely have a forum where the legitimacy claim itself, rather than its specific applications, can be contested.
% DISAPPEARANCE_RATIONALE: If the shared commitment that legitimacy must be maintained vanished — if rulers simply stopped bothering to justify the distribution and the ruled simply stopped expecting justification — the entire apparatus of propaganda, procedural legitimation, economic rhetoric, and consent-manufacturing would become pointless overhead; regimes would either collapse into naked coercion (a different, more expensive equilibrium) or negotiate genuinely redistributive settlements. The kernel's persistence is precisely why so much institutional energy goes into legitimacy maintenance rather than either abandonment.
% FOUNDING_PROBLEM: Rule by force alone is expensive, unstable, and vulnerable to elite defection and mass uprising; some legitimating story that the ruled accept the distribution reduces the enforcement burden and allows longer investment and governance horizons.
% FOUNDING_PROBLEM_CORROBORATION: Comparative regime scholars (documenting GDR collapse when legitimacy narratives lost credibility despite intact surveillance infrastructure) and economic historians outside any single regime's payroll (Piketty's own data-driven argument that legitimacy narratives about capital concentration are increasingly strained) corroborate that the founding problem remains live — the mechanisms that manufacture legitimacy keep being rebuilt because the underlying coordination problem, avoiding costly enforcement of an unaccepted distribution, has not gone away.
narrative_ontology:disappearance_verdict(stability_legitimacy_kernel_flat_control, world_rearranges).
narrative_ontology:founding_problem_status(stability_legitimacy_kernel_flat_control, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(stability_legitimacy_kernel_flat_control, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-11',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(stability_legitimacy_kernel_flat_control, 'none', 1).
narrative_ontology:epsilon_provenance(stability_legitimacy_kernel_flat_control, 0.61, 'claude-sonnet-5', 'surveillance_guillotines_2026_20260811_115130', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(stability_legitimacy_kernel_flat_control_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(stability_legitimacy_kernel_flat_control, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(stability_legitimacy_kernel_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.61 — substantial but not maximal, reflecting that the coordination function (avoiding costly enforcement of an unaccepted distribution) is real even as the specific distributions defended are frequently unfair. Suppression is authored higher (0.70) because maintaining the *appearance* of legitimacy consistently requires more active coercive and narrative-control effort than the underlying coordination problem strictly demands — this is where genuine stability-seeking slides into manufactured consent. Theater ratio (0.44) captures that a meaningful share of legitimacy-maintenance activity (surveillance dossiers nobody reads for security purposes, procedural hearings that do not change siting decisions, economic rhetoric detached from underlying wage and capital data) is performative rather than functional. Accessibility collapse is moderate (0.40) rather than high because genuine alternative distributions remain conceivable and are actively argued for (by Piketty, by displaced residents, by economists) — this is not a mountain; alternatives are visible and articulated, just costly to enact.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat, the arrangement is coordination: a functioning polity requires some accepted account of who gets what and why, and the apparatus that produces that acceptance is a public good, not an extraction mechanism. From the payer seats — surveilled populations, displaced residents — the same structure is experienced as enforced extraction wearing a coordination costume: the surveillance and siting decisions are not accepted, they are imposed, and the 'stability' being purchased is the leadership's stability, not theirs. The engine's per-seat computation should surface this divergence directly from the differing power/exit/scope declarations rather than requiring it be argued in prose.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent leadership and concentrated capital holders sit near the beneficiary end: they administer or profit from the distribution and bear little of its enforcement cost personally. Surveilled populations and displaced residents sit near the full-target end: they bear the surveillance, displacement, and enforcement costs the legitimacy narrative is built to manage, with minimal exit. The squeezed middle strata are structurally ambiguous — dual-positioned as both payer (stagnant wages, absorbing cost increases) and partial beneficiary (nominal political voice, some legal protections) — which is exactly the group whose belief in the fairness of the distribution the kernel depends on most heavily; if this group's acceptance erodes (as Piketty's data suggests it is), the whole legitimacy structure is under the most pressure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — that rule by naked force is expensive and unstable — remains live by the corroboration of comparative regime scholarship and independent economic historians, which is why this constraint is authored as tangled_rope rather than snare: there IS a genuine coordination function (avoiding costly permanent conflict over distribution) bundled with genuine extraction (specific distributions defended well past any plausible fairness claim). The mandatrophy risk is that the kernel's genuine historical function gets used to launder specific, currently unfair distributions as if defending THIS distribution were equivalent to solving the original coordination problem. The GDR case is instructive: the coordination function (avoiding civil war) persisted formally right up until the legitimacy-manufacturing apparatus collapsed in 1989, at which point it became clear the surveillance had been defending the specific arrangement, not the underlying coordination good, which regenerated fine after reunification under a different distribution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_ratio_across_regimes,
    'Does the ratio of genuine coordination function to extractive overhead in this kernel vary systematically by regime type (authoritarian surveillance state vs. liberal democracy vs. populist nationalist administration), or is the ratio roughly constant and only the visible mechanism (secret police vs. campaign finance vs. tariff rhetoric) differs?',
    'Comparative historical analysis measuring enforcement cost per unit of distributional stability achieved across regime types with roughly matched starting Gini coefficients or wealth-concentration measures.',
    'If the ratio is roughly constant, the flat single-constraint construction is well justified. If it varies sharply by regime type, this constraint may itself be a compression of structurally distinct claims that should be decomposed per the ε-invariance principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_ratio_across_regimes, empirical, 'Whether coordination/extraction ratio is regime-invariant or regime-dependent.').

omega_variable(
    legitimacy_belief_vs_compliance,
    'Is the acceptance the kernel requires genuine belief in fairness, or mere behavioral compliance regardless of internal belief (people comply while privately regarding the distribution as illegitimate)?',
    'Survey and behavioral data comparing stated legitimacy beliefs against compliance and resistance behaviors across the GDR (post-hoc, via opened Stasi archives and oral histories) and contemporary polities (via longitudinal attitude surveys).',
    'If compliance without belief is sufficient, the suppression metric should be weighted more heavily than accessibility_collapse — the constraint persists through enforcement, not internalized acceptance, which shifts it toward snare. If genuine belief is required and present, the tangled_rope classification with real coordination function is better supported.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_belief_vs_compliance, conceptual, 'Whether the kernel requires internalized legitimacy or only behavioral compliance.').

omega_variable(
    middle_strata_tipping_point,
    'At what point does erosion of the squeezed middle strata''s belief in distributional fairness (the Piketty r>g dynamic accelerating) cross a threshold that destabilizes the kernel entirely, as happened in the GDR?',
    'Comparative historical threshold analysis: what wealth-concentration or wage-stagnation levels preceded legitimacy collapse in prior cases, mapped against current trajectories.',
    'If a threshold is identifiable and currently being approached, the constraint''s temporal trajectory (rising extraction, rising suppression) should be read as a leading indicator of instability rather than a stable equilibrium.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(middle_strata_tipping_point, empirical, 'Whether a measurable threshold exists for kernel collapse via middle-strata belief erosion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(stability_legitimacy_kernel_flat_control, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stab_tr_t0, stability_legitimacy_kernel_flat_control, theater_ratio, 0, 0.3).
narrative_ontology:measurement(stab_tr_t8, stability_legitimacy_kernel_flat_control, theater_ratio, 8, 0.34).
narrative_ontology:measurement(stab_tr_t16, stability_legitimacy_kernel_flat_control, theater_ratio, 16, 0.38).
narrative_ontology:measurement(stab_tr_t24, stability_legitimacy_kernel_flat_control, theater_ratio, 24, 0.4).
narrative_ontology:measurement(stab_tr_t32, stability_legitimacy_kernel_flat_control, theater_ratio, 32, 0.42).
narrative_ontology:measurement(stab_tr_t40, stability_legitimacy_kernel_flat_control, theater_ratio, 40, 0.44).

% Extraction over time
narrative_ontology:measurement(stab_be_t0, stability_legitimacy_kernel_flat_control, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(stab_be_t8, stability_legitimacy_kernel_flat_control, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(stab_be_t16, stability_legitimacy_kernel_flat_control, base_extractiveness, 16, 0.53).
narrative_ontology:measurement(stab_be_t24, stability_legitimacy_kernel_flat_control, base_extractiveness, 24, 0.57).
narrative_ontology:measurement(stab_be_t32, stability_legitimacy_kernel_flat_control, base_extractiveness, 32, 0.6).
narrative_ontology:measurement(stab_be_t40, stability_legitimacy_kernel_flat_control, base_extractiveness, 40, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(stab_su_t0, stability_legitimacy_kernel_flat_control, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(stab_su_t8, stability_legitimacy_kernel_flat_control, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(stab_su_t16, stability_legitimacy_kernel_flat_control, suppression_requirement, 16, 0.63).
narrative_ontology:measurement(stab_su_t24, stability_legitimacy_kernel_flat_control, suppression_requirement, 24, 0.66).
narrative_ontology:measurement(stab_su_t32, stability_legitimacy_kernel_flat_control, suppression_requirement, 32, 0.68).
narrative_ontology:measurement(stab_su_t40, stability_legitimacy_kernel_flat_control, suppression_requirement, 40, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(stability_legitimacy_kernel_flat_control, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(stability_legitimacy_kernel_flat_control, 0.1).

% DUAL FORMULATION NOTE:
% This is the flat, undecomposed construction of the stability-legitimacy substrate, authored as a control against a reading-decomposed treatment of the same commitment. No sibling reading files exist for this construction; per the construction-perturbation design, cs_structure.reading_relations and axioms are intentionally omitted.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
