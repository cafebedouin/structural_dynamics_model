% ============================================================================
% CONSTRAINT STORY: climate_response_legitimacy__degrowth_transformation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_legitimacy__degrowth_transformation, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: climate_response_legitimacy__degrowth_transformation
 *   human_readable: Degrowth Transformation as Legitimate Climate Response
 *   domain: climate policy/political economy/intergenerational ethics
 *
 * SUMMARY:
 *   The degrowth-transformation reading holds that meeting climate
 *   stabilization targets is incompatible with continued GDP growth in
 *   wealthy economies given demonstrated decoupling rates, and that
 *   legitimate climate policy therefore requires deliberate economic
 *   contraction and redistribution: universal basic services replacing wage
 *   dependency, working-time reduction distributing available work and
 *   reducing throughput, and democratic firm ownership redistributing capital
 *   claims. The reading imposes concentrated, near-term costs on
 *   current-generation wealthy-nation workers, asset holders, and pension
 *   systems in exchange for diffuse, deferred benefits to future generations
 *   and globally vulnerable populations who have no seat in the decision.
 *   This intergenerational and cross-border asymmetry — a domestic political
 *   minority (or, where achieved, majority) imposing structural
 *   transformation whose principal beneficiaries cannot participate in
 *   authorizing it — is the structural core the classification tracks.
 *
 * KEY AGENTS:
 *   - current_generation_wealthy_nation_workers: primary cost-bearer (moderate/constrained) — bears income and structural transition costs within one working lifetime
 *   - incumbent_shareholders_and_asset_holders: primary cost-bearer (powerful/mobile) — capital devaluation, but retains exit via capital flight, driving enforcement need
 *   - future_generations: primary beneficiary (powerless/trapped) — inherits climate outcome, cannot compensate cost-bearers or participate in the bargain
 *   - climate_vulnerable_populations_globally: primary beneficiary (powerless/trapped) — near-term exposure reduction, no domestic political standing
 *   - degrowth_policy_coalition: agenda-setter (organized/mobile) — authors and advocates the platform without bearing its material costs
 *   - growth_coalition_incumbents: excluded party (organized/arbitrage) — structurally delegitimized within the reading's own framing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_legitimacy__degrowth_transformation, 0.58).
domain_priors:suppression_score(climate_response_legitimacy__degrowth_transformation, 0.42).
domain_priors:theater_ratio(climate_response_legitimacy__degrowth_transformation, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, extractiveness, 0.58).
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_legitimacy__degrowth_transformation, tangled_rope).
narrative_ontology:human_readable(climate_response_legitimacy__degrowth_transformation, "Degrowth Transformation as Legitimate Climate Response").
narrative_ontology:topic_domain(climate_response_legitimacy__degrowth_transformation, "climate policy/political economy/intergenerational ethics").

domain_priors:requires_active_enforcement(climate_response_legitimacy__degrowth_transformation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_legitimacy__degrowth_transformation, '37985a7f-f09b-40ae-9ed1-f6867786cb7d').
narrative_ontology:cs_kernel_codification('37985a7f-f09b-40ae-9ed1-f6867786cb7d', distributed).
narrative_ontology:cs_authority_grounding('37985a7f-f09b-40ae-9ed1-f6867786cb7d', distributed).
narrative_ontology:cs_reading_relation('37985a7f-f09b-40ae-9ed1-f6867786cb7d', climate_response_legitimacy__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('37985a7f-f09b-40ae-9ed1-f6867786cb7d', climate_response_legitimacy__adaptation_priority, influences).
narrative_ontology:cs_axiom('37985a7f-f09b-40ae-9ed1-f6867786cb7d', foundational, growth_imperative_is_incompatible_with_stabilization).
narrative_ontology:cs_axiom_status(growth_imperative_is_incompatible_with_stabilization, holdable).
narrative_ontology:cs_axiom_grounding('37985a7f-f09b-40ae-9ed1-f6867786cb7d', growth_imperative_is_incompatible_with_stabilization, empirically_contingent).
narrative_ontology:cs_axiom('37985a7f-f09b-40ae-9ed1-f6867786cb7d', foundational, present_generation_sacrifice_is_legitimately_compelled).
narrative_ontology:cs_axiom_status(present_generation_sacrifice_is_legitimately_compelled, holdable).
narrative_ontology:cs_axiom_grounding('37985a7f-f09b-40ae-9ed1-f6867786cb7d', present_generation_sacrifice_is_legitimately_compelled, deontological).
narrative_ontology:cs_reference_frame('37985a7f-f09b-40ae-9ed1-f6867786cb7d', post_growth_steady_state_economics).
narrative_ontology:cs_drift_state('37985a7f-f09b-40ae-9ed1-f6867786cb7d', contemporary_climate_policy_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('37985a7f-f09b-40ae-9ed1-f6867786cb7d', '').
narrative_ontology:cs_kernel_id(climate_response_legitimacy__degrowth_transformation, climate_response_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__degrowth_transformation, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__degrowth_transformation, climate_vulnerable_populations_globally).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__degrowth_transformation, low_income_households_under_ubs).
narrative_ontology:constraint_victim(climate_response_legitimacy__degrowth_transformation, current_generation_wealthy_nation_workers).
narrative_ontology:constraint_victim(climate_response_legitimacy__degrowth_transformation, incumbent_shareholders_and_asset_holders).
narrative_ontology:constraint_victim(climate_response_legitimacy__degrowth_transformation, growth_dependent_pension_systems).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(climate_response_legitimacy__degrowth_transformation, low_income_households_under_ubs).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__degrowth_transformation, growth_imperative_is_not_natural_law).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__degrowth_transformation, intergenerational_equity_requires_present_sacrifice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Would experience income restructuring, working-time reduction, and firm-ownership transformation within a single working lifetime. They bear the direct transition costs — retraining, sectoral displacement, altered consumption expectations — without the option to defer or opt out once the transformation is legislated. Exit means emigration to a non-transforming economy, which is available to some but not most.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, current_generation_wealthy_nation_workers, payer,
    moderate, biographical, constrained, national).

% Hold capital whose valuation depends on continued growth expectations and current firm-ownership structures. Democratic firm ownership and working-time reduction directly devalue their claims. Unlike workers, they have meaningful exit through capital flight and jurisdiction-shopping, which is precisely why enforcement (capital controls, wealth taxation) becomes central to the constraint's viability.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, incumbent_shareholders_and_asset_holders, payer,
    powerful, biographical, mobile, global).

% Pension funds and retirees whose income streams are actuarially built on compound growth assumptions. A deliberate slowdown of GDP growth threatens fund solvency and payout levels. They cannot exit the constraint's effects because their claims are already vested and non-portable.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, growth_dependent_pension_systems, payer,
    moderate, generational, trapped, national).

% Inherit whatever climate trajectory the present transformation produces. If degrowth transformation succeeds in wealthy nations, they receive a materially stabilized climate without dependence on unproven negative-emissions technology. They have no voice in the current bargain and no ability to compensate today's cost-bearers directly.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, future_generations, beneficiary,
    powerless, civilizational, trapped, global).

% Populations in low-lying, drought-prone, or storm-exposed regions who bear near-term physical climate impacts. Faster wealthy-nation emissions reduction through consumption contraction would reduce their exposure sooner than technology-dependent mitigation pathways, but they have no formal standing in wealthy-nation domestic policy debates that decide whether this reading is adopted.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, climate_vulnerable_populations_globally, beneficiary,
    powerless, biographical, trapped, global).

% Would receive universal basic services (housing, healthcare, transit, food security) decoupled from wage income, insulating them from the income volatility that working-time reduction and firm restructuring might otherwise impose. They also face transition risk if implementation falters partway, having restructured their expectations around a program that requires sustained political majorities.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, low_income_households_under_ubs, beneficiary,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_response_legitimacy__degrowth_transformation, low_income_households_under_ubs, payer).

% Political parties, academic economists, and social movements that author and advocate the degrowth transformation platform. They set the policy agenda, draft legislation, and build electoral coalitions, but do not themselves bear the structural economic costs they impose on workers and asset holders — their exposure is reputational and electoral rather than material.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, degrowth_policy_coalition, agenda_setter,
    organized, generational, mobile, national).

% Industry associations, mainstream economic institutions, and growth-oriented political parties whose entire policy apparatus assumes continued GDP expansion. Under a degrowth-transformation reading, their institutional relevance is directly threatened; they would object strenuously but their objections are treated by this reading's proponents as evidence of captured interest rather than legitimate counter-argument, effectively excluding them from good-faith engagement within the frame.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, growth_coalition_incumbents, excluded,
    organized, biographical, arbitrage, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_legitimacy__degrowth_transformation, diffuse).
narrative_ontology:fixing_cost_class(climate_response_legitimacy__degrowth_transformation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a wealthy-nation economy-wide shift away from GDP growth as the organizing metric of policy success, replacing it with universal basic services, shorter working time, and democratized firm ownership, on the premise that emissions reduction sufficient to meet climate targets is structurally incompatible with continued material throughput growth in already-affluent economies.
% TRANSFER_FUNCTION: Moves consumption capacity, asset value, and income expectations from current-generation wealthy-nation workers, shareholders, and pension holders to future generations and climate-vulnerable populations globally, mediated through deliberate economic contraction and redistribution via universal basic services.
% ABSENT_VOICES: Growth-coalition incumbents (industry associations, growth-oriented economists, financial sector actors) would object that the transformation is unnecessary given decoupling potential, but this reading's proponents frame their objections as interest-captured rather than substantive, excluding them from the legitimacy debate on its own terms. Climate-vulnerable populations abroad have no formal voice in wealthy-nation domestic political processes deciding whether this reading is adopted.
% DISAPPEARANCE_RATIONALE: Proponents hold that if the degrowth-transformation reading disappeared as a live political position, the world would rearrange toward continued growth-oriented mitigation, worsening the odds of meeting temperature targets and shifting cost onto future and vulnerable populations. Opponents hold the world would barely change, since the reading has never achieved governing implementation anywhere and mitigation-priority approaches already occupy the operative policy space — the dispute over which counterfactual is correct is itself unresolved.
% FOUNDING_PROBLEM: Standard emissions-reduction pathways (carbon pricing, technology substitution) were judged by degrowth theorists as mathematically insufficient to meet Paris-aligned carbon budgets given continued GDP growth in wealthy economies, absent speculative levels of decoupling and negative-emissions technology that have not been demonstrated at scale.
% FOUNDING_PROBLEM_CORROBORATION: Some ecological economists and IPCC working-group contributors outside the degrowth advocacy coalition corroborate that historical decoupling rates fall well short of what stabilization requires, supporting the founding problem's continued relevance. Mainstream growth economists and most sitting finance ministries, who are not beneficiaries of this reading's adoption, dispute the premise, arguing decoupling and green-growth technology pathways remain viable and that the founding problem as stated is overstated.
narrative_ontology:disappearance_verdict(climate_response_legitimacy__degrowth_transformation, contested).
narrative_ontology:founding_problem_status(climate_response_legitimacy__degrowth_transformation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_legitimacy__degrowth_transformation, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_response_legitimacy__degrowth_transformation, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_legitimacy__degrowth_transformation, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_legitimacy__degrowth_transformation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_legitimacy__degrowth_transformation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_legitimacy__degrowth_transformation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects genuine, substantial transfer from current-generation cost-bearers to future and globally distant beneficiaries — this is real redistribution, not incidental friction, and rises over the measurement interval as implementation (were it to proceed) would require deepening structural intervention (wealth taxation, capital controls, mandated working-time limits) to hold against capital mobility and political resistance. Suppression (0.42) is moderate rather than high: the reading's implementation depends on democratic mandate-building rather than covert coercion, but achieving and holding a transformation against powerful, mobile capital requires real enforcement machinery (the rising suppression_requirement series tracks this). Resistance (0.78) is high and honestly reflects the constraint's actual political situation: growth-coalition incumbents, asset holders, and pension-dependent constituencies mount substantial organized opposition, which is precisely why the reading has achieved no full-scale implementation anywhere to date. Accessibility collapse (0.35) is low-moderate — genuine policy alternatives (mitigation-priority, adaptation-priority) remain fully available and are in fact the dominant paradigms currently governing; this reading has not foreclosed them, it competes with them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (future generations, globally vulnerable populations, UBS recipients) sit near the low-d end: they receive climate stabilization or income security without bearing the structural transformation cost directly. Payers (current-generation workers, asset holders, pension systems) sit near the high-d end: the transfer is engineered to run through them specifically, via income reduction, capital devaluation, and altered work structures. Asset holders' 'mobile' exit option is a critical asymmetry — their capacity to relocate capital is exactly why the coordination function (if pursued) requires escalating enforcement (the suppression_requirement trend), which workers and pension-holders, lacking that exit, cannot access. This differential exit is a first-order driver of the constraint's classification as tangled_rope rather than a clean rope: the coordination function (climate stabilization) is genuine, but so is the asymmetric extraction machinery required to prevent capital flight from unwinding it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (insufficient decoupling to meet carbon budgets under continued growth) is contested rather than resolved or dead — some independent ecological economists and IPCC contributors corroborate it as still live, while mainstream growth economists dispute the premise entirely. This contestation is itself the reason the reading remains a political proposal rather than an implemented policy anywhere: classifying it as tangled_rope (rather than snare) preserves the genuine coordination claim — stabilizing climate for future generations is a real collective-action problem — while refusing to let that genuine claim launder the concentrated, non-consensual cost imposition on a specific present-generation cohort. A pure snare framing would deny the coordination function is real; a pure rope framing would deny that current-generation workers and pension-holders are structurally coerced into bearing costs they did not choose and cannot fully exit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_climate_legitimacy,
    'Among the three readings of climate_response_legitimacy (degrowth_transformation, mitigation_priority, adaptation_priority), which reading correctly identifies the legitimate response, and is the disagreement resolvable by evidence or only by prior value commitments about growth, technology risk, and intergenerational discounting?',
    'Empirical component: track realized decoupling rates against IPCC carbon budgets over the next decade — if decoupling accelerates sufficiently, mitigation_priority''s core empirical premise is vindicated and degrowth_transformation''s founding problem weakens. Normative component: no amount of decoupling data resolves the prior question of how much risk to impose on current workers versus future populations — that remains a preference-level disagreement.',
    'If decoupling data strongly favors mitigation_priority, degrowth_transformation''s founding_problem_status shifts toward ''dead'' and its extraction on current-generation payers loses legitimating cover, pushing its classification toward snare. If decoupling continues to underperform, the founding problem strengthens and the tangled_rope reading''s coordination claim gains empirical support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_climate_legitimacy, conceptual, 'Which kernel reading (degrowth, mitigation, adaptation) is structurally correct, and whether the disagreement is empirical or normative at its core.').

omega_variable(
    growth_imperative_naturalness,
    'Is the growth imperative in wealthy-nation economies a natural/structural feature of capitalist economic systems (making its dismantling a Mountain-scale transformation), or is it itself a constructed and contingent policy commitment (making it a Snare or Tangled Rope maintained by specific institutional actors who benefit from growth-oriented metrics)?',
    'Comparative economic history: examine whether any capitalist economies have sustained non-growth or degrowth trajectories without systemic collapse, and whether growth-imperative maintenance correlates with identifiable institutional beneficiaries (financial sector growth-dependency, pension fund structures, political incumbency tied to growth metrics).',
    'If the growth imperative is itself a constructed and beneficiary-serving arrangement, the degrowth_transformation reading is better understood as removing an existing snare/tangled_rope rather than imposing a new one — which would substantially lower its own extractiveness reading. If the growth imperative is structurally load-bearing for pension and financial systems as currently constituted, the transformation cost is real and this constraint''s extraction figure stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(growth_imperative_naturalness, conceptual, 'Whether the growth imperative being dismantled is itself natural or constructed — reframes what this constraint is actually removing.').

omega_variable(
    implementation_feasibility_gate,
    'Given that this reading has never achieved full governing implementation anywhere, is the authored extraction and suppression trajectory describing an actual operating constraint or a hypothetical one whose metrics are necessarily speculative?',
    'Track partial implementations (four-day work week trials, UBS pilots, cooperative-ownership mandates in specific jurisdictions) and measure realized cost distribution and enforcement intensity against the authored trajectory.',
    'If partial implementations show milder extraction and suppression than authored here, the trajectory should be revised downward; if pilot programs show sharper backlash and require more coercive maintenance than modeled, the suppression_requirement series understates the constraint''s true enforcement need.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(implementation_feasibility_gate, empirical, 'Whether the temporal metrics describe an operating constraint or a projected one, given the reading''s non-implementation to date.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_legitimacy__degrowth_transformation, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_legitimacy__degrowth_transformation, theater_ratio, 0, 0.15).
narrative_ontology:measurement(clim_tr_t8, climate_response_legitimacy__degrowth_transformation, theater_ratio, 8, 0.18).
narrative_ontology:measurement(clim_tr_t16, climate_response_legitimacy__degrowth_transformation, theater_ratio, 16, 0.21).
narrative_ontology:measurement(clim_tr_t24, climate_response_legitimacy__degrowth_transformation, theater_ratio, 24, 0.24).
narrative_ontology:measurement(clim_tr_t32, climate_response_legitimacy__degrowth_transformation, theater_ratio, 32, 0.26).
narrative_ontology:measurement(clim_tr_t40, climate_response_legitimacy__degrowth_transformation, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(clim_be_t8, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(clim_be_t16, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 16, 0.48).
narrative_ontology:measurement(clim_be_t24, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 24, 0.52).
narrative_ontology:measurement(clim_be_t32, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(clim_be_t40, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(clim_su_t8, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 8, 0.28).
narrative_ontology:measurement(clim_su_t16, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 16, 0.34).
narrative_ontology:measurement(clim_su_t24, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 24, 0.38).
narrative_ontology:measurement(clim_su_t32, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 32, 0.4).
narrative_ontology:measurement(clim_su_t40, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 40, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_legitimacy__degrowth_transformation, resource_allocation).
narrative_ontology:affects_constraint(climate_response_legitimacy__degrowth_transformation, climate_response_legitimacy_mitigation_priority).
narrative_ontology:affects_constraint(climate_response_legitimacy__degrowth_transformation, climate_response_legitimacy_adaptation_priority).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the climate_response_legitimacy kernel. mitigation_priority preserves growth and relies on technological decoupling and carbon pricing; adaptation_priority accepts the warming trajectory and prioritizes resilience infrastructure for vulnerable populations. Each reading has its own ε, beneficiary/victim structure, and classification — they are not measurement perspectives on one constraint but three structurally distinct constraints competing for the same legitimacy label. degrowth_transformation is authored here as tangled_rope: it carries a genuine coordination function (climate stabilization as collective-action problem) plus asymmetric extraction concentrated on current-generation wealthy-nation cost-bearers, requiring active enforcement against capital mobility to hold.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_legitimacy__degrowth_transformation, powerful, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
