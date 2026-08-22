% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__portfolio_pragmatism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_legitimacy__portfolio_pragmatism_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: climate_mitigation_legitimacy__portfolio_pragmatism_reading
 *   human_readable: Technology-Neutral Portfolio Pragmatism in Climate Mitigation Policy
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This is one reading of the contested kernel
 *   'climate_mitigation_legitimacy': the portfolio pragmatism reading. It
 *   asserts that optimal decarbonization requires a technology-neutral policy
 *   framework validating simultaneous investment in both nuclear and
 *   renewable energy infrastructure, with regional actors choosing the
 *   lowest-cost configuration for their jurisdiction. This reading coexists
 *   with three sibling readings: baseload_necessity_reading (emphasizing
 *   dispatchable baseload), renewable_primacy_reading (emphasizing speed and
 *   cost of renewables plus storage), and degrowth_sufficiency_reading
 *   (emphasizing demand reduction). The portfolio pragmatism reading is not
 *   the only reading of the legitimacy kernel; it is ONE structured claim
 *   about what legitimate climate policy must do. The constraint story models
 *   only this reading; its siblings are separate constraints linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - Diversified energy investors: capital formation across nuclear and renewable sectors
 *   - Nuclear industry: institutional beneficiary of technology-neutral framing that preserves capital deployment pathways
 *   - Renewable energy advocates: capacity-constrained by portfolio framing; argue faster renewable scaling is optimal
 *   - Climate scientists: provide empirical pathways; constrained to technical assessment, not technology prioritization
 *   - Regional energy planners: agenda-setters under portfolio reading; optimize locally within multi-technology menu
 *   - Working-class energy communities: bear transition costs asymmetrically; not primary beneficiaries of optimization
 *   - Developing economies: capital-constrained; portfolio neutrality obscures inequity in nuclear vs. distributed renewable finance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.42).
domain_priors:suppression_score(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.38).
domain_priors:theater_ratio(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, accessibility_collapse, 0.51).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, resistance, 0.67).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__portfolio_pragmatism_reading, "Technology-Neutral Portfolio Pragmatism in Climate Mitigation Policy").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__portfolio_pragmatism_reading, "energy_policy/climate_mitigation/technology_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__portfolio_pragmatism_reading, '0974d69a-a21c-429e-b17c-e3264ca49a23').
narrative_ontology:cs_kernel_codification('0974d69a-a21c-429e-b17c-e3264ca49a23', fixed_text).
narrative_ontology:cs_authority_grounding('0974d69a-a21c-429e-b17c-e3264ca49a23', distributed).
narrative_ontology:cs_reading_relation('0974d69a-a21c-429e-b17c-e3264ca49a23', climate_mitigation_legitimacy__baseload_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('0974d69a-a21c-429e-b17c-e3264ca49a23', climate_mitigation_legitimacy__renewable_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('0974d69a-a21c-429e-b17c-e3264ca49a23', climate_mitigation_legitimacy__degrowth_sufficiency_reading, coexists_with).
narrative_ontology:cs_axiom('0974d69a-a21c-429e-b17c-e3264ca49a23', foundational, technology_neutrality_legitimacy).
narrative_ontology:cs_axiom_status(technology_neutrality_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('0974d69a-a21c-429e-b17c-e3264ca49a23', technology_neutrality_legitimacy, instrumental).
narrative_ontology:cs_axiom('0974d69a-a21c-429e-b17c-e3264ca49a23', secondary, capital_optionality_coordination).
narrative_ontology:cs_axiom_status(capital_optionality_coordination, holdable).
narrative_ontology:cs_axiom_grounding('0974d69a-a21c-429e-b17c-e3264ca49a23', capital_optionality_coordination, empirically_contingent).
narrative_ontology:cs_reference_frame('0974d69a-a21c-429e-b17c-e3264ca49a23', paris_agreement_framework).
narrative_ontology:cs_drift_state('0974d69a-a21c-429e-b17c-e3264ca49a23', contemporary_2026, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('0974d69a-a21c-429e-b17c-e3264ca49a23', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, diversified_energy_investors).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, capital_equipment_manufacturers).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, policy_pragmatists).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, grid_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, nuclear_industry).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, regional_energy_planners).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, renewable_energy_advocates).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, working_class_energy_communities).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, developing_economies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a policy framework that validates investment in multiple technology paths simultaneously, reducing single-technology concentration risk. Nuclear projects, wind farms, solar installations, grid storage, and grid modernization all receive support signals under portfolio pragmatism. They collect returns across multiple asset classes rather than betting the capital stack on one technology trajectory.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, diversified_energy_investors, beneficiary,
    institutional, generational, arbitrage, global).

% Portfolio pragmatism legitimizes large capital commitments to new nuclear construction and extends operating licenses for existing plants. The reading protects nuclear from being rendered uneconomic by policy that privileges renewables, preserving institutional survival and justifying continued R&D investment in advanced reactor designs.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, nuclear_industry, beneficiary,
    organized, generational, constrained, global).

% Bear the cost of legitimizing nuclear as an co-equal technology path when they argue rapid renewable scaling plus storage could achieve decarbonization faster and cheaper. The portfolio framing delays full renewable deployment and dilutes advocacy pressure by treating renewable supremacy as one opinion among several, not as an empirical optimum.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, renewable_energy_advocates, payer,
    organized, biographical, mobile, global).

% Provide empirical analysis of decarbonization pathways and carbon budgets. Under portfolio pragmatism, their role is constrained to technical assessment (Can we reach X% by year Y with configuration Z?) rather than technology prioritization (Which path is optimal?). The reading preserves policy discretion by treating technology choice as political, not scientific.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_scientists, observer,
    institutional, generational, analytical, universal).

% Are structurally absent from the portfolio constraint itself — the constraint presupposes carbon-free options — but benefit from delays in portfolio implementation and from political friction the multi-technology debate generates. They are not seated in the constraint story but would object if present, as the portfolio reading commits firmly to fossil replacement, merely disagreeing on the replacement menu.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, fossil_fuel_incumbents, excluded,
    institutional, biographical, trapped, global).

% Portfolio pragmatism delegates optimization to regional actors with the mandate to choose the lowest-cost decarbonized pathway, subject to geography, existing infrastructure, and labor market conditions. They set the agenda for their jurisdiction but are beneficiaries in that the reading validates their technical discretion rather than imposing a one-size-fits-all technology mandate from above.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, regional_energy_planners, agenda_setter,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__portfolio_pragmatism_reading, regional_energy_planners, beneficiary).

% Bear the transition cost of shift to any decarbonized mix, but portfolio pragmatism offers no preferential transition support for fossil fuel workers or coal-dependent regions. The diversified portfolio framing can obscure the concentrated pain of pit closures and coal plant retirement by treating all technologies as equally valid, without addressing the asymmetric social cost of displacement.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, working_class_energy_communities, payer,
    powerless, biographical, identity_locked, regional).

% Face capital constraints for any large-scale generation technology. Portfolio pragmatism recommends regional optimization but does not confront the difference in capital requirements: nuclear demands concentrated large-scale finance (hard to access) while distributed renewables suit modular capital flows (easier for developing contexts). The technology-neutral framing obscures this structural inequity.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, developing_economies, payer,
    moderate, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_legitimacy__portfolio_pragmatism_reading, diversified_energy_investors).
narrative_ontology:fixing_cost_class(climate_mitigation_legitimacy__portfolio_pragmatism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the decarbonization coordination problem by validating investment and policy decisions across multiple carbon-free technologies simultaneously, reducing the risk that betting everything on one technology path leaves the system stranded if that path underperforms or faces political disruption.
% TRANSFER_FUNCTION: Transfers legitimacy, policy support, and capital allocation signals from singulartech advocacy (renewables-only, nuclear-only) toward a diversified technology set. The transfer is primarily one of narrative authority: the portfolio reading claims scientific-pragmatic standing over ideologically-driven technology selection.
% ABSENT_VOICES: Fossil fuel incumbents are structurally absent (the constraint presupposes their elimination, not their inclusion). Radical decarbonization movements arguing for near-term rapid renewable scaling are present but constrained to advocacy rather than policy-setting. Developers-economy technology representatives participate only in regional optimization, not in the legitimacy structure itself.
% DISAPPEARANCE_RATIONALE: If portfolio pragmatism as a legitimacy claim vanished and were replaced by a single-technology mandate (pure renewable, pure nuclear, degrowth), capital flows would reorganize, regional optimization would become impossible, and the decarbonization pathway would narrow to whichever technology the mandate privileged. Existing infrastructure, labor supply, and investment commitments would face acute repricing.
% FOUNDING_PROBLEM: Climate change requires rapid decarbonization, but early policy debates treated technology choice as a binary (nuclear vs. renewables) or ideological (fossil vs. green). No single technology can decarbonize all sectors cost-effectively everywhere; forcing a global uniform technology mix wastes capital.
% FOUNDING_PROBLEM_CORROBORATION: The Intergovernmental Panel on Climate Change (IPCC) assessment reports endorse portfolio approaches as necessary for meeting Paris targets, specifically stating that all limiting-warming pathways rely on multiple zero-carbon technologies. This corroboration is external to energy-industry advocates — it comes from an international scientific body with no technology preference.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__portfolio_pragmatism_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__portfolio_pragmatism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_legitimacy__portfolio_pragmatism_reading_tests).
:- end_tests(climate_mitigation_legitimacy__portfolio_pragmatism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end) because the constraint delivers genuine coordination value — it solves the problem of capital paralysis when technology choice is ideologically contested — but it also legitimizes capital commitments that renewable-only advocates argue are suboptimal, creating a selective legitimacy transfer. Suppression is moderate (0.38) because the constraint does not coercively bar any technology; rather, it uses narrative authority to constrain advocacy to 'technical optimization' rather than 'technology privileging.' Theater is low-moderate (0.28) because the scientific case for portfolio approaches is real, but the portfolio framing also obscures the concentrated social costs of transition in fossil-dependent regions by treating all technologies as equivalent. The measurement series show extractiveness rising slightly in the early interval (as the reading becomes policy-institutionalized) then stabilizing, while theater_ratio rises initially then settles, suggesting the reading stabilizes in institutional use after a period of higher persuasion-work. Suppression_requirement peaks midway, then declines as resistance moderates once policies are in place.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (diversified investors, nuclear incumbents, regional planners) compute the constraint as a genuine coordination solution that preserves capital optionality and respects regional expertise. The payer seats (renewable advocates, developing economies, energy communities) compute it as a constraint that dilutes advocacy pressure, obscures capital inequities, and delays urgent scaling decisions. The engine computes these divergent directionalities from the structural data: beneficiaries have low directionality (the constraint subsidizes their capital flows), payers have high directionality (the constraint narrows their advocacy space). Climate scientists sit as observers with low power — their technical input is not foreclosed, but the portfolio reading preserves policy discretion by treating technology choice as political rather than scientific.
 *
 * DIRECTIONALITY LOGIC:
 *   Diversified investors (d ≈ 0.15, full beneficiary) receive legitimacy signals and capital coordination across multiple technologies without needing to forecast a single winner. Nuclear industry (d ≈ 0.10, strong beneficiary) is protected from renewable-only policy displacement. Renewable advocates (d ≈ 0.70, targets) find their preferred technology path constrained to co-equal status rather than privileged. Developing economies (d ≈ 0.75, targets) face capital requirements unchanged but legitimacy framing shifted to obscure inequity. Energy communities (d ≈ 0.85, identity-locked targets) bear transition costs with no preferential transition support embedded in the portfolio framework. Regional planners (d ≈ 0.30, moderate) are agenda-setters within the portfolio but constrained to regional optimization, unable to set global legitimacy terms. This directionality distribution is stable across the interval; no overrides were necessary.
 *
 * MANDATROPHY ANALYSIS:
 *   Portfolio pragmatism addresses a live founding problem (decarbonization requires capital coordination across multiple technology paths) but carries risk of mandate atrophy if the foundational problem changes. If a single technology (e.g., fusion, or advanced renewables with sufficient storage) demonstrably became the clear optimum, portfolio pragmatism could become a zombie constraint preserving legitimacy for the dominated technology. However, the founding_problem_status is 'live' because no single technology has yet achieved full decarbonization dominance across all regional and sectoral contexts. The constraint would exhibit mandatrophy signals if: (a) measurement data showed theater_ratio rising above 0.50 (performative technology debates replacing real cost competition), (b) disappearance_verdict shifted to 'world_unchanged' (capital flows would not reorganize if portfolio legitimacy vanished), or (c) a sibling reading's empirical case became overwhelmingly supported while portfolio framing persisted. Current measurements do not trigger mandatrophy, but the field should monitor whether portfolio pragmatism becomes ritualized debate cover for delayed renewable scaling.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technology_neutrality_vs_optimum,
    'Is technology-neutral portfolio policy genuinely neutral, or does it embed hidden advantages for nuclear by legitimizing high-capital-intensity pathways that developing economies cannot access?',
    'Comparative capital-flow analysis: examine whether portfolio pragmatism policies allocate capital equally per unit of decarbonization achieved, or whether nuclear projects receive disproportionate support. Monitor whether renewable-dominant regions compete on cost while nuclear-dominant regions compete on legitimacy.',
    'If the portfolio is genuinely neutral on capital accessibility, it solves coordination and is Rope. If it systematically biases toward high-capital technologies, it is Tangled Rope (coordination cover for capital-accessibility extraction). If this omega resolves to the latter, reclassify and split the story into technology-capital-inequity as a separate constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_neutrality_vs_optimum, empirical, 'Whether portfolio neutrality masks capital-accessibility inequity').

omega_variable(
    regional_optimization_vs_global_mandate,
    'Portfolio pragmatism delegates technology choice to regional planners. But if one technology genuinely dominates (e.g., renewables + storage become clearly superior), does portfolio pragmatism create institutional path-dependency that perpetuates inferior regional choices?',
    'Monitor regional policy decisions over 10+ years: if dominated technologies persist in regions where dominance is clear, the reading has atrophied into zombification. Conduct counterfactual analysis: would policy have shifted faster under a renewable-priority framing?',
    'If regional optimization consistently produces suboptimal choices, the constraint becomes inertial (Piton). If it produces genuinely good regional decisions, it remains Rope. The reading''s legitimacy depends on its claim that regions optimize better than global mandates, which is an empirical claim that can be falsified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regional_optimization_vs_global_mandate, empirical, 'Whether decentralized optimization produces better outcomes than a global technology hierarchy').

omega_variable(
    sibling_reading_foreclosure,
    'Does portfolio pragmatism logically foreclose the renewable_primacy_reading, or do they coexist as different framings of the same legitimacy kernel?',
    'Logical analysis: portfolio pragmatism says ''both are valid options and regions choose.'' Renewable primacy says ''renewables are optimal and regions should deploy them fastest.'' These can both be true in one framework (regions are *able* to choose renewables under portfolio pragmatism). They foreclose only if one says ''regions must not have the option the other provides.''',
    'If readings coexist, the relation is ''coexists_with.'' If portfolio pragmatism somehow prevents renewable-priority policy (it does not — regions can privilege renewables within pragmatism), then it forecloses. The current cs_structure.reading_relations is set to ''coexists_with'' based on this logic; if later analysis suggests foreclosure, the relation updates but the constraint''s classification does not change.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Logical structure of sibling reading relationships').

omega_variable(
    energy_community_transition_suppression,
    'Working-class energy communities bear identity-locked suppression: their entire professional identity and regional economic infrastructure are fused with fossil fuels. Portfolio pragmatism offers no preferential transition support (it does not suppress the people, but it supplies no counter-frame). Is this suppression structural (the constraint caused it) or contextual (fossil fuel economics caused it, and portfolio pragmatism merely fails to address it)?',
    'Compare transition outcomes in regions with portfolio pragmatism policy vs. regions with targeted energy-community support policies (retraining, wage insurance, regional development). If support policies measurably improve transition trajectories, the suppression is partially structural (addressable by the constraint itself). If outcomes are unchanged, it is primarily contextual.',
    'If structural, the constraint is Tangled Rope (coordination for investors, suppression for communities). If contextual, it remains Rope. The directionality assigned to energy_communities (0.85, identity-locked target) already reflects this uncertainty; an omega resolution would clarify whether the targeting is active or passive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(energy_community_transition_suppression, empirical, 'Whether portfolio pragmatism''s neutrality on transition support constitutes active or passive suppression of fossil-fuel workers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(clim_tr_t0, observed).
narrative_ontology:measurement(clim_tr_t5, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 5, 0.21).
narrative_ontology:measurement_basis(clim_tr_t5, observed).
narrative_ontology:measurement(clim_tr_t10, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement_basis(clim_tr_t10, observed).
narrative_ontology:measurement(clim_tr_t15, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 15, 0.27).
narrative_ontology:measurement_basis(clim_tr_t15, observed).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement_basis(clim_tr_t20, observed).
narrative_ontology:measurement(clim_tr_t25, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 25, 0.29).
narrative_ontology:measurement_basis(clim_tr_t25, observed).
narrative_ontology:measurement(clim_tr_t30, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement_basis(clim_tr_t30, observed).
narrative_ontology:measurement(clim_tr_t35, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 35, 0.28).
narrative_ontology:measurement_basis(clim_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(clim_be_t0, observed).
narrative_ontology:measurement(clim_be_t5, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement_basis(clim_be_t5, observed).
narrative_ontology:measurement(clim_be_t10, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 10, 0.39).
narrative_ontology:measurement_basis(clim_be_t10, observed).
narrative_ontology:measurement(clim_be_t15, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 15, 0.41).
narrative_ontology:measurement_basis(clim_be_t15, observed).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 20, 0.43).
narrative_ontology:measurement_basis(clim_be_t20, observed).
narrative_ontology:measurement(clim_be_t25, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 25, 0.44).
narrative_ontology:measurement_basis(clim_be_t25, observed).
narrative_ontology:measurement(clim_be_t30, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement_basis(clim_be_t30, observed).
narrative_ontology:measurement(clim_be_t35, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 35, 0.42).
narrative_ontology:measurement_basis(clim_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement_basis(clim_su_t0, observed).
narrative_ontology:measurement(clim_su_t5, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 5, 0.35).
narrative_ontology:measurement_basis(clim_su_t5, observed).
narrative_ontology:measurement(clim_su_t10, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 10, 0.37).
narrative_ontology:measurement_basis(clim_su_t10, observed).
narrative_ontology:measurement(clim_su_t15, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 15, 0.4).
narrative_ontology:measurement_basis(clim_su_t15, observed).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 20, 0.39).
narrative_ontology:measurement_basis(clim_su_t20, observed).
narrative_ontology:measurement(clim_su_t25, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 25, 0.39).
narrative_ontology:measurement_basis(clim_su_t25, observed).
narrative_ontology:measurement(clim_su_t30, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 30, 0.38).
narrative_ontology:measurement_basis(clim_su_t30, observed).
narrative_ontology:measurement(clim_su_t35, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 35, 0.38).
narrative_ontology:measurement_basis(clim_su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__portfolio_pragmatism_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.18).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_mitigation_legitimacy__baseload_necessity_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_mitigation_legitimacy__renewable_primacy_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_mitigation_legitimacy__degrowth_sufficiency_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a contested kernel (climate_mitigation_legitimacy). The kernel is the persisting commitment: 'climate change requires decarbonization, and legitimate policy must determine how to decarbonize.' Different readings parse this kernel into different constraints. Portfolio pragmatism (this story) asserts that legitimacy requires technology neutrality and regional optimization. Baseload necessity asserts legitimacy requires dispatchable baseload. Renewable primacy asserts legitimacy requires speed and cost minimization. Degrowth sufficiency asserts legitimacy requires demand reduction. Each reading has a different ε, different beneficiary/victim set, and different founding problem. The readings coexist as different interpretations held by different institutional actors (energy investors, climate advocates, policymakers, scientists), not as alternative empirical hypotheses about a single world-state. This constrains the kernel reading does not contradict the others' empirical claims; rather, it claims legitimacy grounds for one technology portfolio over another.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
