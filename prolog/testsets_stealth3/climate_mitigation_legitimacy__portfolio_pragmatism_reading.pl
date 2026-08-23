% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__portfolio_pragmatism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: climate_mitigation_legitimacy__portfolio_pragmatism_reading
 *   human_readable: Technology-Neutral Decarbonization Portfolio Mandate (Portfolio Pragmatism Reading)
 *   domain: energy policy/climate mitigation/technology governance
 *
 * SUMMARY:
 *   The technology-neutral portfolio claim answers the kernel question (what
 *   does legitimate decarbonization require?) with: a balanced generation
 *   portfolio containing both nuclear and renewables, no technology
 *   privileged a priori, and the optimal mix set regionally. As a standing
 *   arrangement it operates through integrated resource plans, portfolio
 *   standards, capacity-market reliability rules, and subsidy architectures
 *   that reserve a protected share of the decarbonization buildout for
 *   nuclear while keeping renewable procurement inside a utility-administered
 *   balance. The claim coordinates real diversification under deep
 *   uncertainty; the same structure channels above-least-cost capital to the
 *   nuclear complex and disciplines single-technology strategies as ideology.
 *   This file instantiates the portfolio_pragmatism_reading only; the sibling
 *   readings are separate constraints with their own epsilon values over
 *   their own referents. Because this reading endorses the frame, it authors
 *   moderate epsilon for the frame's operation; a renewable-primacy or
 *   degrowth reading of the same arrangement would author substantially
 *   higher extraction. The referent is the standing technology-neutral
 *   portfolio arrangement as it actually operates, never the idealized
 *   optimization this reading endorses in the abstract. The claim/metric
 *   relationship follows the independence rule: claimed_type is what I judge
 *   structurally true (tangled_rope: genuine coordination function plus
 *   asymmetric extraction through the same structure); the metrics describe
 *   observed operation and are not tuned to the claim or to any predicted
 *   engine output.
 *
 * KEY AGENTS:
 *   - nuclear_operators_and_suppliers: primary beneficiary (institutional / identity_locked) — collects the nuclear premium the frame legitimizes
 *   - diversified_investor_owned_utilities: agenda_setter and beneficiary (institutional / arbitrage) — administers the balance and earns returns under any mix
 *   - ratepayers: primary target (powerless / trapped) — bears the tariff premium of mandated balance
 *   - renewable_independent_developers: secondary target (organized / constrained) — competes inside a frame that reserves protected share for nuclear
 *   - taxpayers: secondary target (powerless / constrained) — underwrites guarantees, credits, and overrun socialization
 *   - nuclear_skilled_trades: secondary beneficiary (organized / identity_locked) — skills and identity bound to reactor work
 *   - future_generations: absent party (powerless / trapped) — inherits climate and fiscal legacies, seated nowhere
 *   - integrated_assessment_modelers: analytical observer — produces the scenario evidence every reading cites
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.52).
domain_priors:suppression_score(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.45).
domain_priors:theater_ratio(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__portfolio_pragmatism_reading, "Technology-Neutral Decarbonization Portfolio Mandate (Portfolio Pragmatism Reading)").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__portfolio_pragmatism_reading, "energy policy/climate mitigation/technology governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__portfolio_pragmatism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 'b265d627-aea9-4ebd-9247-a698925835f7').
narrative_ontology:cs_kernel_codification('b265d627-aea9-4ebd-9247-a698925835f7', distributed).
narrative_ontology:cs_authority_grounding('b265d627-aea9-4ebd-9247-a698925835f7', expertise).
narrative_ontology:cs_interpretation_layer_present('b265d627-aea9-4ebd-9247-a698925835f7').
narrative_ontology:cs_reading_relation('b265d627-aea9-4ebd-9247-a698925835f7', climate_mitigation_legitimacy__baseload_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('b265d627-aea9-4ebd-9247-a698925835f7', climate_mitigation_legitimacy__renewable_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('b265d627-aea9-4ebd-9247-a698925835f7', climate_mitigation_legitimacy__degrowth_sufficiency_reading, forecloses).
narrative_ontology:cs_axiom('b265d627-aea9-4ebd-9247-a698925835f7', foundational, no_apriori_technology_privilege).
narrative_ontology:cs_axiom_status(no_apriori_technology_privilege, holdable).
narrative_ontology:cs_axiom_grounding('b265d627-aea9-4ebd-9247-a698925835f7', no_apriori_technology_privilege, instrumental).
narrative_ontology:cs_axiom('b265d627-aea9-4ebd-9247-a698925835f7', foundational, regional_optimality_determines_mix).
narrative_ontology:cs_axiom_status(regional_optimality_determines_mix, holdable).
narrative_ontology:cs_axiom_grounding('b265d627-aea9-4ebd-9247-a698925835f7', regional_optimality_determines_mix, empirically_contingent).
narrative_ontology:cs_reference_frame('b265d627-aea9-4ebd-9247-a698925835f7', technology_neutral_regional_optimization).
narrative_ontology:cs_drift_state('b265d627-aea9-4ebd-9247-a698925835f7', post_renewable_cost_revolution, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b265d627-aea9-4ebd-9247-a698925835f7', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, nuclear_operators_and_suppliers).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, diversified_investor_owned_utilities).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, nuclear_skilled_trades).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, ratepayers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, renewable_independent_developers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, taxpayers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, future_generations).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__portfolio_pragmatism_reading, portfolio_diversification_principle).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__portfolio_pragmatism_reading, technology_neutrality_doctrine).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__portfolio_pragmatism_reading, robust_decisionmaking_under_uncertainty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate existing reactor fleets and develop new units. Collect production tax credits, capacity payments, mandated offtake, and loan-guarantee-backed financing that the portfolio frame legitimizes. Their capital is sunk in reactor technology, their workforce and regulatory relationships assume continued operation, and their institutional purpose is bound up with the fleet; exit means stranded assets and a dissolved organizational identity rather than a portfolio reallocation.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, nuclear_operators_and_suppliers, beneficiary,
    institutional, generational, identity_locked, national).

% Write integrated resource plans, administer portfolio standards, and occupy the committees and regulatory dockets where 'balance' is defined. They earn regulated returns on rate base regardless of which technology the balance favors, so any capital-intensive mix serves them; they translate the neutrality norm into procurement rules, tariff design, and capacity-market positions. Their flexibility across technologies means the frame's fate matters less to them than its continuation.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, diversified_investor_owned_utilities, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__portfolio_pragmatism_reading, diversified_investor_owned_utilities, beneficiary).

% Pay retail tariffs set to recover portfolio costs. In most regulated jurisdictions they cannot choose their supplier and cannot opt out of the generation mix approved in the resource plan. They bear above-least-cost premiums wherever the mandated balance preserves higher-cost firm capacity, and their stakes are individually too small to organize against.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, ratepayers, payer,
    powerless, biographical, trapped, regional).

% Compete for interconnection queue positions, transmission capacity, and policy attention inside a frame that reserves a protected share of the decarbonization buildout for nuclear. Their pipelines depend on procurement rules the utilities write; they can develop in other markets, but the frame travels through model legislation and shared consulting practice.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, renewable_independent_developers, payer,
    organized, biographical, constrained, national).

% Welders, licensed operators, and reactor engineers whose certification, wages, and professional identity are specific to reactor work, often concentrated in plant towns. The portfolio frame guarantees demand for their skills. Exit means retraining out of their trade and community, which few do.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, nuclear_skilled_trades, beneficiary,
    organized, biographical, identity_locked, regional).

% Underwrite loan guarantees, production tax credits, and construction-overrun socialization behind new reactor builds. They cannot decline the exposure and receive only diffuse, unpriced benefits through claimed system reliability. Like ratepayers, their per-capita stakes are dispersed below organizing thresholds.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, taxpayers, payer,
    powerless, biographical, constrained, national).

% Inherit both the climate outcome and the fiscal, decommissioning, and waste legacies of the chosen portfolio. If the mandated balance slows least-cost decarbonization, they bear additional warming; they are present at no table where the balance is set.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, future_generations, payer,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__portfolio_pragmatism_reading, future_generations, excluded).

% Run the scenario ensembles and capacity-expansion models that all four readings of the kernel cite as evidence. They hold no capital and collect no rents; their scenario assumptions on discount rates, storage costs, and nuclear overnight costs largely determine which reading the models find, which gives them quiet agenda power without a stake.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, integrated_assessment_modelers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_legitimacy__portfolio_pragmatism_reading, nuclear_operators_and_suppliers).
narrative_ontology:fixing_cost_class(climate_mitigation_legitimacy__portfolio_pragmatism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates capital allocation across generation technologies under deep uncertainty: diversification hedges against any single technology failing on cost, reliability, or supply chain; accommodates regional resource variation; and preserves optionality while technology cost trajectories resolve. Secondarily it coordinates the climate coalition itself, giving nuclear and renewable constituencies a shared frame that avoids a zero-sum internal war over subsidy and market share.
% TRANSFER_FUNCTION: Moves ratepayer and taxpayer funds into nuclear and diversified-utility capital programs (production tax credits, loan guarantees, capacity payments, mandated offtake) above what least-cost procurement would allocate; moves market share and policy attention away from single-technology strategies toward balanced portfolios; and moves legitimacy itself, coding acceptance of the balance as 'pragmatic' and resistance to it as 'ideological.'
% ABSENT_VOICES: Degrowth advocates are in public discourse but their reading is foreclosed from the frame's legitimacy, so they would contest the expansion premise itself from outside. Future generations bear the climate and fiscal legacies and sit at no table. Ratepayers are diffuse and unorganized exactly where tariffs are set. Energy planners in capital-constrained regions, for whom OECD-style nuclear inclusion is unaffordable, are largely outside the modeling communities that define the frame's evidence base.
% DISAPPEARANCE_RATIONALE: If the neutrality norm vanished overnight, procurement would reorganize around least-cost decarbonization, which in most regions is overwhelmingly renewable-plus-storage; nuclear fleets without cost cover would face accelerated retirement debates; the climate coalition's nuclear-renewable truce would dissolve into an open fight over subsidy and market share; and the pragmatic center the frame anchors would lose its organizing identity and its claim to stand above the technology factions.
% FOUNDING_PROBLEM: In the early 2000s no single technology offered a proven path to deep decarbonization: renewables were expensive and intermittency at scale was unsolved, nuclear was stalled by cost overruns and public opposition, and climate policy needed a framework that hedged technology risk, accommodated regional variation, and held the climate coalition together across its nuclear and renewable factions.
% FOUNDING_PROBLEM_CORROBORATION: Partially corroborated from outside the benefiting parties: multi-institution net-zero modeling studies and transmission-system reliability assessments attest that firm-power provision and diversification address real, unresolved reliability problems in many regions. The corroboration is itself contested: 100-percent-renewable-feasibility research programs attest the founding uncertainty has been substantially resolved by storage cost declines, and degrowth scholarship rejects the expansion premise outright. No body outside the nuclear-benefiting set attests the problem's liveness unambiguously; the status is genuinely disputed.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__portfolio_pragmatism_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__portfolio_pragmatism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_legitimacy__portfolio_pragmatism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_legitimacy__portfolio_pragmatism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_legitimacy__portfolio_pragmatism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.52: the frame channels a real premium to the nuclear complex (tax credits, capacity payments, guaranteed financing, protected offtake that least-cost procurement would not allocate) while its diversification function delivers genuine value (firm capacity, hedging, optionality) that partly offsets the transfer from the payer seats. Suppression 0.45: enforcement is structural and active (portfolio standards, IRP approval, capacity-market reliability rules, subsidy lock-ins) supplemented by a discursive mechanism that codes single-technology strategies as ideology; the mechanism is roughly 70 percent structural and 30 percent internalized by the analyst and regulatory class the frame funds. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and scope, in the engine's computation. Theater_ratio 0.35 and rising across the series: balance increasingly functions rhetorically (commissions, all-of-the-above platforms, diversified R&D announcements) while actual allocation concentrates. Accessibility_collapse 0.30: the alternative strategies remain fully live and institutionally staffed; understanding the frame does not close them. Resistance 0.60: the claim is fought simultaneously by renewable-primacy advocates (nuclear inclusion contested), degrowth scholars (expansion premise rejected), and cost-focused analysts (premium unjustified). All three tracked metrics run on one shared time grid (T0 approximates 2000, when portfolio language enters mainstream climate policy discourse; T25 approximates 2025) so every metric is authored at every examined point; the suppression series is included because the story specifically traces the frame's enforcement machinery maturing from build-the-portfolio mandates into defend-the-nuclear-share rules. Coalition note: the payer seats are structurally blocked from coalition formation (dispersed per-capita stakes, captive retail markets), which is itself part of the frame's stability and is why trapped powerlessness persists here without a snare-level enforcement apparatus.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter seat (diversified utilities) should compute a low-d, near-beneficiary classification: the frame is stewardship they administer and profit from under any mix, with arbitrage exit that makes them indifferent to its content. The payer seats compute differently: ratepayers (trapped, powerless) experience enforced tariff premiums; renewable developers (constrained, organized) experience market share allocated away by rule; taxpayers experience unchosen liability. The nuclear beneficiary seat experiences the frame as survival rather than windfall: identity_locked exit means even a favorable reading feels existential. The engine computes these per-seat divergences from power, exit, and role; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries sit near the beneficiary end: nuclear operators (identity_locked; exit would strand the fleet and dissolve institutional purpose) derive d near 0, as do nuclear skilled trades. Diversified utilities, as agenda_setters with arbitrage exit, sit near 0 but with less lock-in: they profit from the frame's administration rather than from nuclear specificity, so their d is low but their stake is in continuation, not content. Payers sit near the target end: ratepayers (trapped) and taxpayers (constrained) near full target; renewable developers (constrained) at high d because the frame allocates protected share away from them. Future generations carry full-target d with no seat at any table. Modelers are analytical with no stake (d near symmetric). No directionality overrides are authored: the beneficiary and victim declarations plus exit options produce the correct d for every seat, and the derivation chain handles the dual-positioned utility seat through its beneficiary declaration.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unresolved technology uncertainty circa 2000) is partially resolved: renewable and storage cost declines have closed part of the original uncertainty while firm-power and reliability problems remain live in many regions, hence founding_problem_status contested rather than dead. The tangled_rope classification is what prevents mandatrophy mislabeling in both directions: reading the frame as pure rope would hide the growing nuclear premium and subsidy lock-in; reading it as pure snare would erase the genuine reliability and hedging function it still performs and that independent modeling corroborates. The trajectory to watch is piton-ward: if storage costs keep falling, the frame's uncertainty-hedging justification atrophies while the subsidy architecture persists, maintained by rising theater (the theater_ratio series rises monotonically) and by coalition-peace maintenance rather than technical function. The measurement series is designed to let the engine date any such transition rather than have the claim pre-empt it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Is the technology-neutral portfolio reading the correct answer to what legitimate decarbonization requires, or does one of the sibling readings (baseload_necessity_reading, renewable_primacy_reading, degrowth_sufficiency_reading) better instantiate the climate_mitigation_legitimacy kernel?',
    'Comparative evaluation of reading-specific policy portfolios against multi-objective criteria (cost, reliability, speed, political durability) across regions, using each sibling file''s own epsilon and structural data for cross-reading comparison.',
    'If renewable_primacy is correct, this frame''s nuclear inclusion is extraction riding a fading coordination story; if degrowth_sufficiency is correct, the frame''s expansion premise fails and the portfolio question is mis-posed; if baseload_necessity is correct, the frame''s neutrality is incoherent because one component is required rather than optional.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'This constraint is one reading of a contested kernel; the reading contest itself is unresolved.').

omega_variable(
    nuclear_premium_cost_justification,
    'Does nuclear''s full system value (firm capacity, ancillary services, land footprint, supply-chain diversity) cover its cost premium over the renewable-plus-storage alternative in the regions where the frame protects it?',
    'System-value-inclusive cost comparisons (not LCOE-only) across protected jurisdictions, with sensitivity analysis on storage cost trajectories and discount rates.',
    'If the premium is not covered by system value, the frame''s extraction component dominates and the arrangement drifts snare-ward; if covered, the frame''s coordination claim is strengthened and epsilon is overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nuclear_premium_cost_justification, empirical, 'Whether the nuclear component of the mandated portfolio is robustness or captured subsidy.').

omega_variable(
    neutrality_fossil_spillover,
    'Does the neutrality rhetoric, once enacted as all-of-the-above procurement and capacity-market design, extend fossil generation operations beyond what this reading endorses?',
    'Compare fossil retirement schedules and capacity-market awards in jurisdictions adopting neutrality language against comparable jurisdictions with explicit decarbonization-only procurement standards.',
    'If systematic spillover exists, the frame''s effective epsilon is understated here and this reading is complicit in delay it does not intend; the boundary between this constraint and its fossil-exploiting neighbors would need redrawing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neutrality_fossil_spillover, empirical, 'Whether technology neutrality functions in practice as a fossil delay vehicle.').

omega_variable(
    coordination_vs_coalition_peace,
    'Is the frame''s persistence explained by genuine technical coordination value (hedging technology risk under uncertainty) or by political coalition maintenance (keeping nuclear and renewable constituencies inside the climate coalition)?',
    'Counterfactual analysis: model coalition stability and decarbonization outcomes with and without the neutrality norm; test whether the norm survives in polities without the faction structure it pacifies.',
    'If coalition peace is the binding function, the frame is a political truce rather than a technical optimization; its classification weight shifts from coordination toward enforced settlement, and its extraction reads as tribute to a faction rather than hedging cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_coalition_peace, conceptual, 'Whether the coordination function is technical hedging or faction management.').

omega_variable(
    regional_variation_operationalization,
    'Does the frame''s regional-variation premise actually operate, producing regionally differentiated portfolios, or does neutrality collapse into a uniform all-of-the-above allocation in practice?',
    'Cross-jurisdiction comparison of actual portfolio compositions against region-specific cost-optimal mixes from capacity-expansion models.',
    'If uniform in practice, the frame''s distinctive claim (regional variation) is theatrical, its theater_ratio is understated, and the arrangement is closer to a fixed subsidy architecture than an optimization discipline.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_variation_operationalization, empirical, 'Whether the reading''s core regional-variation premise is operational or rhetorical.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(clim_tr_t0, observed).
narrative_ontology:measurement(clim_tr_t5, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 5, 0.24).
narrative_ontology:measurement_basis(clim_tr_t5, observed).
narrative_ontology:measurement(clim_tr_t10, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement_basis(clim_tr_t10, observed).
narrative_ontology:measurement(clim_tr_t15, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 15, 0.32).
narrative_ontology:measurement_basis(clim_tr_t15, observed).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 20, 0.34).
narrative_ontology:measurement_basis(clim_tr_t20, observed).
narrative_ontology:measurement(clim_tr_t25, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 25, 0.35).
narrative_ontology:measurement_basis(clim_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(clim_be_t0, observed).
narrative_ontology:measurement(clim_be_t5, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 5, 0.4).
narrative_ontology:measurement_basis(clim_be_t5, observed).
narrative_ontology:measurement(clim_be_t10, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement_basis(clim_be_t10, observed).
narrative_ontology:measurement(clim_be_t15, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 15, 0.5).
narrative_ontology:measurement_basis(clim_be_t15, observed).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement_basis(clim_be_t20, observed).
narrative_ontology:measurement(clim_be_t25, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 25, 0.52).
narrative_ontology:measurement_basis(clim_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement_basis(clim_su_t0, observed).
narrative_ontology:measurement(clim_su_t5, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 5, 0.34).
narrative_ontology:measurement_basis(clim_su_t5, observed).
narrative_ontology:measurement(clim_su_t10, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement_basis(clim_su_t10, observed).
narrative_ontology:measurement(clim_su_t15, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 15, 0.44).
narrative_ontology:measurement_basis(clim_su_t15, observed).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement_basis(clim_su_t20, observed).
narrative_ontology:measurement(clim_su_t25, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 25, 0.45).
narrative_ontology:measurement_basis(clim_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__portfolio_pragmatism_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__portfolio_pragmatism_reading, baseload_necessity_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__portfolio_pragmatism_reading, renewable_primacy_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__portfolio_pragmatism_reading, degrowth_sufficiency_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'balanced decarbonization strategy' decomposes into four structurally distinct kernel readings with different epsilon values, victim sets, and enforcement profiles: this file (portfolio_pragmatism_reading, moderate epsilon, endorsed by its own reading), baseload_necessity_reading (necessity claim privileging firm power), renewable_primacy_reading (sufficiency claim under which nuclear is unnecessary), and degrowth_sufficiency_reading (rejects the generation-expansion frame entirely). They form one constraint family linked by affects_constraints; each sibling carries its own epsilon over its own referent per the epsilon-invariance principle. The upstream shared evidence base (scenario ensembles) is cited by all four readings, which is where contamination would propagate if the modeling layer's assumptions were shown to embed one reading's priors.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
