% ============================================================================
% CONSTRAINT STORY: technology_legitimacy_kernel__velocity_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_legitimacy_kernel__velocity_primacy_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: technology_legitimacy_kernel__velocity_primacy_reading
 *   human_readable: Deployment-Velocity Legitimacy Test for Climate Technology
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the technology_legitimacy_kernel:
 *   the claim that a climate technology is legitimate if and only if it can
 *   be deployed at scale within the remaining carbon budget window
 *   (2030/2050). This reading systematically favors technologies with short
 *   construction-to-generation timelines — solar, wind, battery storage — and
 *   marginalizes technologies whose deployment curve is slow relative to the
 *   budget clock, regardless of their dispatchability or lifecycle risk
 *   profile. It is a distinct constraint from the reliability_primacy_reading
 *   (which tests for dispatchable, baseload-capable generation) and the
 *   precautionary_reading (which tests for bounded, reversible worst-case
 *   failure modes) — those are sibling constraints in other files, not
 *   alternate measurements of this one. The three readings have different
 *   beneficiary/victim sets and different epsilon values because they are
 *   different constraints sharing a contested kernel, not one constraint
 *   viewed three ways.
 *
 * KEY AGENTS:
 *   - solar_and_wind_developers: primary beneficiary (organized/arbitrage) — technology automatically certified legitimate by the timeline test
 *   - renewables_finance_sector: beneficiary and agenda-setter (institutional/arbitrage) — prices technology worthiness on deployment-speed models
 *   - carbon_budget_advocacy_coalitions: agenda-setter (organized/analytical) — administers the carbon-budget framing that grounds the criterion
 *   - grid_operators_managing_intermittency: primary payer (institutional/trapped) — absorbs the operational cost the criterion excludes from consideration
 *   - nuclear_developers: primary victim (powerful/trapped) — excluded by construction-timeline screening regardless of other merits
 *   - climate_policy_analysts: analytical observer (analytical/analytical) — models the tradeoffs among competing kernel readings without adjudicating them
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_legitimacy_kernel__velocity_primacy_reading, 0.58).
domain_priors:suppression_score(technology_legitimacy_kernel__velocity_primacy_reading, 0.62).
domain_priors:theater_ratio(technology_legitimacy_kernel__velocity_primacy_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_legitimacy_kernel__velocity_primacy_reading, tangled_rope).
narrative_ontology:human_readable(technology_legitimacy_kernel__velocity_primacy_reading, "Deployment-Velocity Legitimacy Test for Climate Technology").
narrative_ontology:topic_domain(technology_legitimacy_kernel__velocity_primacy_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(technology_legitimacy_kernel__velocity_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_legitimacy_kernel__velocity_primacy_reading, '198af3e3-d2bb-42a1-b4ad-a7e32519b0a0').
narrative_ontology:cs_kernel_codification('198af3e3-d2bb-42a1-b4ad-a7e32519b0a0', distributed).
narrative_ontology:cs_authority_grounding('198af3e3-d2bb-42a1-b4ad-a7e32519b0a0', distributed).
narrative_ontology:cs_reading_relation('198af3e3-d2bb-42a1-b4ad-a7e32519b0a0', technology_legitimacy_kernel__reliability_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('198af3e3-d2bb-42a1-b4ad-a7e32519b0a0', technology_legitimacy_kernel__precautionary_reading, influences).
narrative_ontology:cs_axiom('198af3e3-d2bb-42a1-b4ad-a7e32519b0a0', foundational, deployment_speed_dominates_technology_worthiness).
narrative_ontology:cs_axiom_status(deployment_speed_dominates_technology_worthiness, holdable).
narrative_ontology:cs_axiom_grounding('198af3e3-d2bb-42a1-b4ad-a7e32519b0a0', deployment_speed_dominates_technology_worthiness, instrumental).
narrative_ontology:cs_axiom('198af3e3-d2bb-42a1-b4ad-a7e32519b0a0', secondary, remaining_carbon_budget_is_the_binding_constraint).
narrative_ontology:cs_axiom_status(remaining_carbon_budget_is_the_binding_constraint, holdable).
narrative_ontology:cs_axiom_grounding('198af3e3-d2bb-42a1-b4ad-a7e32519b0a0', remaining_carbon_budget_is_the_binding_constraint, empirically_contingent).
narrative_ontology:cs_reference_frame('198af3e3-d2bb-42a1-b4ad-a7e32519b0a0', carbon_budget_timeline_primacy).
narrative_ontology:cs_drift_state('198af3e3-d2bb-42a1-b4ad-a7e32519b0a0', post_2015_paris_ratchet_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('198af3e3-d2bb-42a1-b4ad-a7e32519b0a0', '').
narrative_ontology:cs_kernel_id(technology_legitimacy_kernel__velocity_primacy_reading, technology_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__velocity_primacy_reading, solar_and_wind_developers).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__velocity_primacy_reading, battery_storage_manufacturers).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__velocity_primacy_reading, renewables_finance_sector).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__velocity_primacy_reading, carbon_budget_advocacy_coalitions).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, grid_operators_managing_intermittency).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, nuclear_developers).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, advanced_geothermal_developers).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, ratepayers_in_high_penetration_grids).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, manufacturing_regions_dependent_on_dispatchable_power).
narrative_ontology:constraint_vindicates(technology_legitimacy_kernel__velocity_primacy_reading, carbon_budget_timeline_is_the_binding_constraint).
narrative_ontology:constraint_vindicates(technology_legitimacy_kernel__velocity_primacy_reading, speed_of_deployment_dominates_technology_choice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build projects with short permitting-to-generation timelines relative to nuclear or large hydro. Under the velocity test, their technology is automatically legitimate because it can be racked up in modular increments within a single election or budget cycle. They receive subsidy priority, streamlined permitting, and preferential grid interconnection queues justified explicitly by the 2030/2050 deployment math.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, solar_and_wind_developers, beneficiary,
    organized, biographical, arbitrage, global).

% Sell the buffering technology that the velocity reading treats as an acceptable supplement rather than a precondition — storage is financed and scaled as fast-follow infrastructure, riding the same urgency logic that legitimizes the generation technology it supports.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, battery_storage_manufacturers, beneficiary,
    organized, biographical, mobile, global).

% Underwrites and structures the capital flows that presuppose deployment speed as the legitimacy criterion; helps set the policy agenda through lobbying, model portfolios, and rating methodologies that treat construction-timeline risk as the master variable pricing technology worthiness.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, renewables_finance_sector, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(technology_legitimacy_kernel__velocity_primacy_reading, renewables_finance_sector, agenda_setter).

% Publish and defend the remaining-carbon-budget framing itself, translating IPCC timelines into the legitimacy test applied to technologies. They administer the discourse that makes 'can it be built by 2030/2050' the dispositive question, crowding out other criteria from serious policy consideration.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, carbon_budget_advocacy_coalitions, agenda_setter,
    organized, generational, analytical, global).

% Must balance a rapidly variable generation mix in real time, absorbing the reliability costs the velocity test does not price into technology legitimacy. They cannot simply refuse fast-deployed intermittent capacity once policy has declared it legitimate and financed it; their operational burden rises without a corresponding say in the criterion that created it.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, grid_operators_managing_intermittency, payer,
    institutional, immediate, trapped, regional).

% Offer dispatchable, low-carbon generation but require 10-15 year construction and licensing timelines that fail the velocity test outright, regardless of eventual output or lifecycle emissions. Capital, permitting priority, and political legitimacy are systematically diverted away from them because the clock, not the technology's other merits, is the test.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, nuclear_developers, payer,
    powerful, civilizational, trapped, national).

% Pursue dispatchable enhanced geothermal systems still in pilot-to-commercial transition; their multi-year scale-up curve reads as too slow under the velocity criterion even though the underlying resource is durable and load-following, so they are marginalized in near-term procurement and financing decisions.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, advanced_geothermal_developers, payer,
    moderate, generational, constrained, national).

% Bear the cost of curtailment payments, backup capacity contracts, and price volatility that follow from prioritizing deployment speed over dispatchability. They have no seat in setting the legitimacy criterion and encounter its consequences only as line items on utility bills.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, ratepayers_in_high_penetration_grids, payer,
    powerless, biographical, trapped, regional).

% Host energy-intensive industry that needs firm, continuous power; when the fastest-deployable technologies are intermittent, these regions face reliability risk or must fund parallel firming capacity themselves, a cost the velocity framing does not attribute to the technologies it certifies as legitimate.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, manufacturing_regions_dependent_on_dispatchable_power, payer,
    moderate, biographical, constrained, regional).

% Model competing legitimacy criteria and their tradeoffs, publishing comparative deployment-speed-versus-reliability-versus-risk analyses that inform, but do not resolve, which reading of the kernel prevails in a given jurisdiction's policy.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, climate_policy_analysts, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, legible criterion — deployable at scale within the remaining carbon budget window — that lets policymakers, financiers, and permitting bodies converge quickly on which technologies to fund and fast-track, avoiding paralysis-by-technology-debate while emissions accumulate.
% TRANSFER_FUNCTION: Moves capital, permitting priority, and political legitimacy toward technologies with short construction timelines (solar, wind, storage) and away from technologies with long lead times (nuclear, advanced geothermal), while shifting the operational and cost burden of managing the resulting intermittent generation mix onto grid operators and ratepayers.
% ABSENT_VOICES: Nuclear and advanced geothermal developers argue their technologies' slower deployment curve is offset by superior dispatchability and lower lifecycle land/material footprint, but the velocity criterion excludes lifecycle and dispatchability considerations from the legitimacy test by construction; grid engineers who must operationally reconcile the resulting mix are rarely present when the criterion itself is set by advocacy coalitions and financiers.
% DISAPPEARANCE_RATIONALE: If the velocity-primacy test stopped governing legitimacy determinations, capital and permitting priority would reallocate toward technologies scored on dispatchability or bounded risk instead, nuclear and enhanced geothermal projects currently marginalized by construction-timeline screening would become financeable again, and grid operators would gain leverage to demand firming capacity be priced into what counts as a 'legitimate' technology.
% FOUNDING_PROBLEM: Faced with a rapidly closing carbon budget and a 2030/2050 target structure, policymakers needed a tractable way to triage which technologies could plausibly contribute enough emissions reduction in time to matter, rather than funding technologies whose payoff arrives after the budget is already exhausted.
% FOUNDING_PROBLEM_CORROBORATION: IPCC-aligned climate scientists and renewables advocates attest the timeline constraint is real and binding — remaining budgets are genuinely narrow. Grid engineering bodies and nuclear-sector economists, positioned outside the renewables beneficiary coalition, corroborate that the timeline pressure is real but contest that deployment speed alone is the correct proxy for climate benefit, noting that fast-but-intermittent buildout can extend fossil peaking-plant reliance and that lifecycle abatement-per-dollar sometimes favors slower dispatchable technology — an argument absent from the velocity criterion as administered.
narrative_ontology:disappearance_verdict(technology_legitimacy_kernel__velocity_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_legitimacy_kernel__velocity_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_legitimacy_kernel__velocity_primacy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(technology_legitimacy_kernel__velocity_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_legitimacy_kernel__velocity_primacy_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_legitimacy_kernel__velocity_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(technology_legitimacy_kernel__velocity_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(technology_legitimacy_kernel__velocity_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects that the velocity criterion transfers real capital and permitting priority away from slower dispatchable technologies toward fast-deployable ones, independent of comparative climate benefit — this is a genuine transfer, not merely a coordination convenience. Suppression (0.62) is high because the criterion actively forecloses serious consideration of alternatives once codified into subsidy design, permitting statute, and financing rating methodology; a nuclear project cannot simply present its lifecycle case within this framework, because the framework's dispositive test excludes lifecycle and dispatchability variables by construction. Theater ratio is comparatively low (0.28) because the coordination function is real: triaging technologies under a genuinely closing timeline is not empty performance, it solves an actual allocation problem under time pressure. Accessibility collapse (0.52) is moderate — alternative criteria (reliability, precaution) remain visible and contested in policy discourse, they have not been fully erased, which is why this is authored as tangled_rope rather than snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Solar/wind developers, storage manufacturers, and renewables finance sit near the beneficiary end: they collect subsidy, permitting priority, and capital under a criterion they helped construct and that certifies their technology class as legitimate nearly by definition. Grid operators, ratepayers, and manufacturing regions sit near the target end: they bear the intermittency-management costs and price volatility that the criterion's narrow timeline focus does not internalize, with limited exit (grid operators cannot opt out of balancing the grid they are handed). Nuclear and advanced geothermal developers are targets in a different sense — not bearing an operating cost but bearing exclusion from capital and legitimacy despite offering the dispatchability the grid operators need; their exit option is trapped because the criterion, once embedded in financing and permitting law, cannot be exited by producing a better technology, only by changing the criterion itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a genuinely closing carbon budget requiring fast triage — remains partly live (the timeline pressure is real), which is why founding_problem_status is authored as contested rather than dead. This prevents mislabeling the constraint as pure extraction: the coordination function (triage under real time pressure) is genuine and would need to be replaced by something if this criterion disappeared. But the criterion has also outrun its original triage function by embedding itself into rating methodologies and statutory permitting priority in ways that now foreclose reconsideration of dispatchability tradeoffs — this is the tangled_rope signature: real coordination function plus asymmetric extraction requiring active enforcement (permitting statute, subsidy design) to hold, not a case where dropping to snare or elevating to pure rope would be accurate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_dominance_ambiguity,
    'Which reading of the technology_legitimacy_kernel (velocity_primacy, reliability_primacy, precautionary) will dominate actual policy and financing decisions in a given jurisdiction, and is that dominance itself contestable or settling?',
    'Track which criterion is explicitly codified into binding instruments — subsidy eligibility statutes, grid interconnection rules, credit rating methodologies — across jurisdictions and over time; a jurisdiction moving from implicit to explicit codification of one reading signals that reading''s practical dominance.',
    'If velocity_primacy becomes the entrenched, codified criterion, the extraction from excluded dispatchable technologies becomes harder to reverse (accessibility_collapse rises toward mountain-adjacent territory); if reliability_primacy or precautionary readings gain codified ground instead, this constraint''s victim set could become its beneficiary set under a sibling reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_dominance_ambiguity, conceptual, 'Which kernel reading becomes dominant in binding policy instruments, and whether that dominance is reversible.').

omega_variable(
    timeline_pressure_genuineness,
    'Is the underlying carbon-budget timeline pressure that grounds this reading a genuine physical constraint (remaining atmospheric headroom) or a constructed policy artifact (specific target-setting conventions like 2030/2050 that could be renegotiated)?',
    'Compare IPCC physical carbon budget estimates (grounded in climate sensitivity and cumulative emissions science) against the specific political target dates (2030/2050), which are negotiated policy conventions layered on top of the physical budget.',
    'If the physical budget is the binding element, the velocity criterion has a genuine mountain-adjacent core (the physics of remaining headroom) wrapped in a constructed layer (target-date conventions); if the target dates themselves are what''s driving technology exclusion rather than the physical budget, the criterion is more purely a policy artifact serving the interests of already-scalable technology incumbents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(timeline_pressure_genuineness, empirical, 'Whether the timeline pressure grounding this reading is physical or a negotiated policy convention.').

omega_variable(
    grid_cost_internalization_omega,
    'Should the intermittency-management costs borne by grid operators and ratepayers be priced into the legitimacy test itself, rather than treated as an externality to a criterion that only measures deployment speed?',
    'Full-system-cost accounting studies that compare levelized cost of electricity including firming, curtailment, and balancing costs for fast-deployed intermittent technologies against slower dispatchable alternatives, over the same time horizon.',
    'If full-system costs are internalized, some technologies currently certified legitimate under the pure deployment-speed test may fail a cost-inclusive version of the same reading, narrowing the gap between this reading and reliability_primacy_reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(grid_cost_internalization_omega, preference, 'Whether grid-management costs belong inside the legitimacy criterion or remain an externality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_legitimacy_kernel__velocity_primacy_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t0, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(tech_tr_t4, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 4, 0.15).
narrative_ontology:measurement(tech_tr_t8, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 8, 0.19).
narrative_ontology:measurement(tech_tr_t12, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(tech_tr_t16, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 16, 0.25).
narrative_ontology:measurement(tech_tr_t20, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(tech_tr_t24, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(tech_be_t0, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(tech_be_t4, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 4, 0.4).
narrative_ontology:measurement(tech_be_t8, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(tech_be_t12, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 12, 0.51).
narrative_ontology:measurement(tech_be_t16, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(tech_be_t20, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(tech_be_t24, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t0, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(tech_su_t4, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 4, 0.46).
narrative_ontology:measurement(tech_su_t8, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 8, 0.51).
narrative_ontology:measurement(tech_su_t12, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 12, 0.55).
narrative_ontology:measurement(tech_su_t16, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 16, 0.58).
narrative_ontology:measurement(tech_su_t20, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(tech_su_t24, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 24, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_legitimacy_kernel__velocity_primacy_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(technology_legitimacy_kernel__velocity_primacy_reading, 0.12).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__velocity_primacy_reading, reliability_primacy_reading).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__velocity_primacy_reading, precautionary_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposed from the natural-language 'technology legitimacy for climate mitigation' claim, per the epsilon-invariance principle. Each sibling reading (velocity_primacy, reliability_primacy, precautionary) has a distinct beneficiary/victim structure and a distinct epsilon because they test technologies against structurally different criteria. velocity_primacy_reading extracts from dispatchable-but-slow technologies (nuclear, advanced geothermal) and from grid operators who must absorb intermittency; reliability_primacy_reading would instead extract from intermittent-but-fast technologies and their financiers; precautionary_reading would extract from technologies with large but currently-tolerated tail risk regardless of deployment speed or dispatchability. All three readings share the same contested kernel (technology_legitimacy_kernel) and are linked here via affects_constraints because the readings compete for institutional dominance — a jurisdiction that codifies one reading structurally disadvantages projects built for a sibling reading's assumptions, creating real downstream influence between the sibling constraints even though none of them logically forecloses the others outright.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(technology_legitimacy_kernel__velocity_primacy_reading, powerful, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
