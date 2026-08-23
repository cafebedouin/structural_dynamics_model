% ============================================================================
% CONSTRAINT STORY: climate_response_legitimacy__degrowth_transformation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint story captures the degrowth_transformation reading of the
 *   contested kernel 'climate_response_legitimacy.' The reading asserts that
 *   legitimate climate response requires wealthy nations to dismantle their
 *   growth imperative through structural transformation: universal basic
 *   services (decommodifying survival), working time reduction
 *   (redistributing necessary labor), and democratic firm ownership
 *   (redirecting surplus from capital to stakeholders). The constraint is a
 *   tangled_rope: it performs a genuine coordination function (solving the
 *   intergenerational free-rider problem of climate change) while extracting
 *   asymmetrically from current wealthy-nation populations (income, time,
 *   capital returns) for the benefit of future generations and the globally
 *   vulnerable. Political feasibility is the central tension — the
 *   suppression requirement (0.68) reflects the institutional resistance from
 *   capital owners and high-income workers, while the extraction (0.72)
 *   reflects the material sacrifice demanded. The measurement series projects
 *   from current policy discourse (low extraction, low suppression) through
 *   escalating climate impacts driving policy adoption (rising extraction and
 *   suppression) to a transformed steady state. The claim/metric independence
 *   is maintained: the reading CLAIMS tangled_rope (coordination +
 *   extraction) while the metrics describe the operational reality of
 *   implementing such a transformation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_legitimacy__degrowth_transformation, 0.72).
domain_priors:suppression_score(climate_response_legitimacy__degrowth_transformation, 0.68).
domain_priors:theater_ratio(climate_response_legitimacy__degrowth_transformation, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, extractiveness, 0.72).
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_legitimacy__degrowth_transformation, tangled_rope).
narrative_ontology:human_readable(climate_response_legitimacy__degrowth_transformation, "Degrowth Transformation as Legitimate Climate Response").
narrative_ontology:topic_domain(climate_response_legitimacy__degrowth_transformation, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_legitimacy__degrowth_transformation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_legitimacy__degrowth_transformation, 'd3168b9d-24e4-4860-b927-7023db78d3c3').
narrative_ontology:cs_kernel_codification('d3168b9d-24e4-4860-b927-7023db78d3c3', implicit).
narrative_ontology:cs_authority_grounding('d3168b9d-24e4-4860-b927-7023db78d3c3', extraction).
narrative_ontology:cs_reading_relation('d3168b9d-24e4-4860-b927-7023db78d3c3', climate_response_legitimacy__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('d3168b9d-24e4-4860-b927-7023db78d3c3', climate_response_legitimacy__adaptation_priority, coexists_with).
narrative_ontology:cs_axiom('d3168b9d-24e4-4860-b927-7023db78d3c3', foundational, intergenerational_justice_requires_prevention).
narrative_ontology:cs_axiom_status(intergenerational_justice_requires_prevention, holdable).
narrative_ontology:cs_axiom_grounding('d3168b9d-24e4-4860-b927-7023db78d3c3', intergenerational_justice_requires_prevention, deontological).
narrative_ontology:cs_axiom('d3168b9d-24e4-4860-b927-7023db78d3c3', foundational, growth_decoupling_is_empirically_insufficient).
narrative_ontology:cs_axiom_status(growth_decoupling_is_empirically_insufficient, holdable).
narrative_ontology:cs_axiom_grounding('d3168b9d-24e4-4860-b927-7023db78d3c3', growth_decoupling_is_empirically_insufficient, empirically_contingent).
narrative_ontology:cs_reference_frame('d3168b9d-24e4-4860-b927-7023db78d3c3', intergenerational_justice_prevention_framework).
narrative_ontology:cs_drift_state('d3168b9d-24e4-4860-b927-7023db78d3c3', post_paris_agreement_implementation_gap, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d3168b9d-24e4-4860-b927-7023db78d3c3', '').
narrative_ontology:cs_kernel_id(climate_response_legitimacy__degrowth_transformation, climate_response_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__degrowth_transformation, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__degrowth_transformation, vulnerable_populations_global_south).
narrative_ontology:constraint_victim(climate_response_legitimacy__degrowth_transformation, current_generation_wealthy_nations).
narrative_ontology:constraint_victim(climate_response_legitimacy__degrowth_transformation, high_income_workers_wealthy_nations).
narrative_ontology:constraint_victim(climate_response_legitimacy__degrowth_transformation, capital_owners_wealthy_nations).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__degrowth_transformation, intergenerational_justice_requires_prevention).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__degrowth_transformation, growth_decoupling_is_empirically_insufficient).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the costs of structural transformation: reduced income growth, shorter working hours without full wage compensation, loss of investment returns from capital restructuring. Exit is constrained by national borders, path-dependent institutions, and the global nature of climate risk — they cannot individually opt out of the climate system but can resist the policy.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, current_generation_wealthy_nations, payer,
    powerful, biographical, constrained, global).

% Receive the benefit of avoided catastrophic warming without technological dependency on unproven negative-emissions technologies. They have no voice in current decisions and no exit from the climate system they inherit. Their benefit is conditional on the transformation actually succeeding.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, future_generations, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(climate_response_legitimacy__degrowth_transformation, future_generations).

% Gain from wealthy-nations' emissions reduction creating atmospheric space for their development, and from universal basic services models that could be adapted globally. Currently trapped in climate vulnerability with minimal historical responsibility; their exit options are blocked by global inequality.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, vulnerable_populations_global_south, beneficiary,
    powerless, generational, trapped, global).

% Face working time reduction and income compression in high-carbon sectors; may gain from universal basic services (healthcare, housing, transport) but lose status and consumption capacity. Exit is constrained by sector-specific human capital and national labor markets.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, high_income_workers_wealthy_nations, payer,
    moderate, biographical, constrained, national).

% Bear the brunt of democratic firm ownership and capital restructuring; returns shift from financial extraction to stakeholder governance. Exit is relatively mobile via capital flight, tax havens, and jurisdictional arbitrage — making them the most resistant constituency with the strongest threat credibility.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, capital_owners_wealthy_nations, payer,
    powerful, biographical, mobile, global).

% Design and advocate the transformation package (UBS, working time reduction, democratic ownership). They set the agenda through IPCC synthesis reports, UNFCCC negotiations, and national climate law drafting. Their authority derives from scientific consensus and institutional mandate; they neither pay the costs nor receive the direct benefits.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, climate_policy_architects, agenda_setter,
    institutional, generational, analytical, global).

% Advocate for technological decoupling and carbon pricing as the legitimate climate response. They are excluded from this reading's legitimacy claim — the degrowth reading asserts their approach is empirically insufficient. They would object that degrowth is politically impossible and unnecessarily costly. Their institutional base: IEA, OECD, major corporate sustainability initiatives.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, mitigation_priority_advocates, excluded,
    organized, biographical, constrained, global).

% Argue that warming trajectory is locked in and legitimacy requires protecting the vulnerable through resilience infrastructure. Excluded by this reading's prevention-first framing. They would object that degrowth sacrifices near-term adaptation funding for uncertain long-term prevention. Institutional base: World Bank adaptation finance, disaster risk reduction agencies, some Global South negotiators.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, adaptation_priority_advocates, excluded,
    organized, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinating a legitimate climate response that prevents catastrophic warming while distributing the costs of prevention justly across generations and nations — solving the intergenerational free-rider problem where current emitters externalize costs to future non-emitters.
% TRANSFER_FUNCTION: Moves economic capacity (income growth, working time, capital returns, consumption space) from current wealthy-nation populations to future generations globally, via reduced atmospheric carbon concentration and avoided climate damages. The transfer is mediated by universal basic services (decommodifying survival needs), working time reduction (redistributing labor), and democratic firm ownership (redirecting surplus from capital to stakeholders).
% ABSENT_VOICES: Global South populations who need carbon space for development but are not represented in wealthy-nation degrowth debates; workers in carbon-intensive industries whose livelihoods are slated for managed decline; political representatives accountable to current voters who bear costs but cannot claim future benefits; indigenous communities whose land-based mitigation is often co-opted by green-growth agendas.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, wealthy nations would continue growth-as-usual with technological mitigation only. Emissions would exceed carbon budgets, locking in 2.5-3°C+ warming. Future generations would face catastrophic damages without the technological capacity to adapt. The global economic order would rearrange around climate disruption rather than planned transformation — migration crises, food system collapse, and geopolitical conflict would replace structured degrowth.
% FOUNDING_PROBLEM: The climate crisis is caused by the growth imperative in wealthy nations: an economic system that requires endless throughput expansion on a finite planet, externalizing ecological costs to future generations and the Global South. The illegitimacy arises because those who benefit from growth (current wealthy) do not bear its climate consequences (future/vulnerable).
% FOUNDING_PROBLEM_CORROBORATION: IPCC AR6 WGIII (2022) confirms emissions continue rising despite decoupling claims; Hickel & Kallis (2020) document empirical insufficiency of green growth; Global South negotiators at COP27/28 (Loss and Damage fund demands) corroborate that wealthy nations' growth causes their climate vulnerability; climate justice movements (Fridays for Future, Climate Justice Alliance) attest from outside the beneficiary set of current wealthy-nation growth.
narrative_ontology:disappearance_verdict(climate_response_legitimacy__degrowth_transformation, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_legitimacy__degrowth_transformation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_legitimacy__degrowth_transformation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_legitimacy__degrowth_transformation, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_legitimacy__degrowth_transformation, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extraction (0.72) is high because the constraint demands material sacrifice from current wealthy populations: GDP contraction in high-income countries, working time reduction without full wage compensation, capital restructuring via democratic ownership. Suppression (0.68) is high because enacting this requires overcoming powerful incumbent resistance (capital mobility, political capture, media opposition) through binding legislation, carbon rationing, and institutional restructuring. Theater ratio (0.25) is low because the proposed measures are substantive structural changes, not performative gestures — though implementation gaps could raise this. Accessibility collapse (0.55) is moderate: alternatives (mitigation_priority, adaptation_priority) exist and are advocated by organized groups, but this reading declares them illegitimate. Resistance (0.75) is very high — the most powerful agents (capital owners, wealthy-nation governments) are payers with mobile or constrained exit.
 *
 * PERSPECTIVAL GAP:
 *   From the climate_policy_architect (agenda_setter) seat, the constraint appears as necessary coordination: the only legitimate response to an existential threat. From the capital_owner (payer) seat, it appears as existential expropriation: a political project using climate as pretext for structural redistribution. From the high_income_worker (payer) seat, it is ambiguous: loss of income/status vs. gain in decommodified services and leisure. The engine computes these divergent seat types from the structural data — the authored claim does not adjudicate them. The tangled_rope classification emerges precisely from this seat divergence: genuine coordination for some, genuine extraction for others.
 *
 * DIRECTIONALITY LOGIC:
 *   Current generation wealthy nations are full targets (d ≈ 0.9): they bear the extraction, have constrained exit (cannot leave climate system), and hold power to resist but not to escape. Future generations are full beneficiaries (d ≈ 0.0): they receive the climate stabilization benefit with zero agency. Vulnerable Global South populations are beneficiaries (d ≈ 0.1) but powerless and trapped. Capital owners are payers with mobile exit (d ≈ 0.7) — their capital mobility dampens but doesn't eliminate extraction. High-income workers are payers with constrained exit (d ≈ 0.8). Climate policy architects are agenda_setters with analytical position (d ≈ 0.5). Mitigation/adaptation advocates are excluded — their directionality is undefined as they are not subject to this constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (growth-imperative climate crisis) is live and worsening — emissions rise, damages accelerate. The mandate has not atrophied; if anything, the urgency has increased. However, the political feasibility barrier creates a mandatrophy risk: if the transformation cannot be enacted, the constraint becomes a performative demand (piton) — advocated ritually but never implemented. The omega on political feasibility captures this. The constraint is not a degraded snare; it is a live contested proposal with real coordination function and real extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    political_feasibility_implementation_gap,
    'Can the structural transformation (universal basic services, working time reduction, democratic firm ownership) be enacted in wealthy democracies given current political economy?',
    'Historical analysis of structural reform episodes (post-war welfare state, neoliberal transition) combined with institutional veto-player mapping in current OECD polities.',
    'If infeasible, the constraint is a performative demand (piton trajectory); if feasible under crisis conditions, it is a live tangled_rope with high suppression requirement during transition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(political_feasibility_implementation_gap, empirical, 'Whether the coordination function can overcome the extraction resistance from powerful incumbents.').

omega_variable(
    technological_decoupling_alternative,
    'Is absolute decoupling of economic growth from emissions physically possible at the speed and scale required for 1.5°C/2°C targets?',
    'Integrated assessment model comparison with empirical decoupling rates; engineering feasibility of full-sector decarbonization without demand reduction.',
    'If decoupling is feasible, the extraction from current wealthy is unnecessary (mitigation_priority reading gains legitimacy); if infeasible, the extraction is the price of coordination (this reading''s claim vindicated).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_decoupling_alternative, empirical, 'Whether the mitigation_priority sibling reading''s core premise holds.').

omega_variable(
    future_generation_net_benefit,
    'Do future generations net-benefit from degrowth transformation versus a high-growth high-damage trajectory, accounting for lost innovation capacity?',
    'Long-run integrated assessment with endogenous innovation; comparison of climate damages avoided vs. technology forgone under degrowth vs. green-growth pathways.',
    'If net benefit is negative, the beneficiary declaration for future_generations is invalid — the constraint extracts from present without delivering to future (snare signature).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_generation_net_benefit, conceptual, 'Whether the claimed beneficiary actually benefits intergenerationally.').

omega_variable(
    kernel_reading_boundary_degrowth_vs_mitigation,
    'Does the degrowth_transformation reading logically foreclose the mitigation_priority reading, or do they coexist as competing policy frameworks?',
    'Logical analysis of premise compatibility: can a polity simultaneously pursue maximum feasible decoupling AND structural degrowth, or does commitment to one undermine the political coalition for the other?',
    'If forecloses, the readings cannot be held in one framework (kernel splits); if coexists_with, they are rival but compatible positions in democratic contestation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary_degrowth_vs_mitigation, conceptual, 'Structural relationship between this reading and the mitigation_priority sibling.').

omega_variable(
    suppression_mechanism_state_vs_civil_society,
    'Is the suppression required to enforce degrowth transformation primarily state coercion (carbon rationing, firm restructuring mandates) or civil-society internalization (norm shift, voluntary simplicity)?',
    'Policy design analysis: which instruments carry the binding constraint — hard regulation or soft norm diffusion? Historical analogy to wartime mobilization vs. cultural shifts.',
    'If state coercion dominates, suppression metric is structural and high; if norm internalization dominates, suppression is partially internalized and persists post-policy (omega suppression_mechanism_ambiguity applies).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_state_vs_civil_society, empirical, 'Whether enforcement relies on external barriers or internalized constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_legitimacy__degrowth_transformation, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(crl_dt_tr_t0, climate_response_legitimacy__degrowth_transformation, theater_ratio, 0, 0.1).
narrative_ontology:measurement(crl_dt_tr_t6, climate_response_legitimacy__degrowth_transformation, theater_ratio, 6, 0.15).
narrative_ontology:measurement(crl_dt_tr_t12, climate_response_legitimacy__degrowth_transformation, theater_ratio, 12, 0.2).
narrative_ontology:measurement(crl_dt_tr_t18, climate_response_legitimacy__degrowth_transformation, theater_ratio, 18, 0.22).
narrative_ontology:measurement(crl_dt_tr_t24, climate_response_legitimacy__degrowth_transformation, theater_ratio, 24, 0.24).
narrative_ontology:measurement(crl_dt_tr_t30, climate_response_legitimacy__degrowth_transformation, theater_ratio, 30, 0.25).

% Extraction over time
narrative_ontology:measurement(crl_dt_be_t0, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(crl_dt_be_t6, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(crl_dt_be_t12, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(crl_dt_be_t18, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 18, 0.65).
narrative_ontology:measurement(crl_dt_be_t24, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 24, 0.7).
narrative_ontology:measurement(crl_dt_be_t30, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 30, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(crl_dt_su_t0, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(crl_dt_su_t6, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 6, 0.52).
narrative_ontology:measurement(crl_dt_su_t12, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 12, 0.6).
narrative_ontology:measurement(crl_dt_su_t18, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 18, 0.65).
narrative_ontology:measurement(crl_dt_su_t24, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 24, 0.67).
narrative_ontology:measurement(crl_dt_su_t30, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_legitimacy__degrowth_transformation, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_legitimacy__degrowth_transformation, 0.18).
narrative_ontology:affects_constraint(climate_response_legitimacy__degrowth_transformation, climate_response_legitimacy__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_legitimacy__degrowth_transformation, climate_response_legitimacy__adaptation_priority).
narrative_ontology:affects_constraint(climate_response_legitimacy__degrowth_transformation, global_carbon_budget_allocation).
narrative_ontology:affects_constraint(climate_response_legitimacy__degrowth_transformation, universal_basic_services_implementation).
narrative_ontology:affects_constraint(climate_response_legitimacy__degrowth_transformation, democratic_firm_ownership_transition).

% DUAL FORMULATION NOTE:
% This constraint and its siblings (mitigation_priority, adaptation_priority) form a constraint family decomposing the 'legitimate climate response' kernel. They share the same referent (the climate crisis) but instantiate different constraints with different ε, different beneficiary/victim structures, and different legitimacy claims. The degrowth reading has the highest ε (0.72) because it demands structural sacrifice from the powerful; mitigation_priority has lower ε (claims tech decoupling avoids sacrifice); adaptation_priority has moderate ε (resilience investment costs) but different victim set (those unprotected).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_legitimacy__degrowth_transformation, powerful, 0.7).
constraint_indexing:directionality_override(climate_response_legitimacy__degrowth_transformation, moderate, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
