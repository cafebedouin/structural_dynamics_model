% ============================================================================
% CONSTRAINT STORY: technology_legitimacy_kernel__reliability_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_legitimacy_kernel__reliability_primacy_reading, []).

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
 *   constraint_id: technology_legitimacy_kernel__reliability_primacy_reading
 *   human_readable: Reliability-Primacy Reading of Climate Technology Legitimacy
 *   domain: energy policy / climate mitigation / technology governance
 *
 * SUMMARY:
 *   This story instantiates the reliability-primacy reading of the technology
 *   legitimacy kernel: a technology counts as climate-mitigation-legitimate
 *   only if it delivers dispatchable, baseload-capable generation. As
 *   variable renewable penetration rose over the past two decades, grid
 *   reliability councils and capacity-market designers operationalized
 *   dispatchability as the gating criterion for capacity payments,
 *   interconnection priority, and often for formal climate-finance
 *   eligibility. Nuclear and gas-peaker incumbents clear this test
 *   structurally, by asset design, without additional investment;
 *   intermittent wind and solar developers must either finance costly storage
 *   pairing or accept exclusion from legitimacy credit and capacity
 *   compensation. Ratepayers fund the resulting capacity premiums. The
 *   coordination function (genuine grid-stability engineering) is real and
 *   does not fully explain the extraction pattern — the specific choice to
 *   gate on baseload dispatchability, rather than on a broader firm-capacity
 *   definition including storage, demand response, and transmission
 *   flexibility, systematically favors incumbent asset classes.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_legitimacy_kernel__reliability_primacy_reading, 0.58).
domain_priors:suppression_score(technology_legitimacy_kernel__reliability_primacy_reading, 0.52).
domain_priors:theater_ratio(technology_legitimacy_kernel__reliability_primacy_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_legitimacy_kernel__reliability_primacy_reading, tangled_rope).
narrative_ontology:human_readable(technology_legitimacy_kernel__reliability_primacy_reading, "Reliability-Primacy Reading of Climate Technology Legitimacy").
narrative_ontology:topic_domain(technology_legitimacy_kernel__reliability_primacy_reading, "energy policy / climate mitigation / technology governance").

domain_priors:requires_active_enforcement(technology_legitimacy_kernel__reliability_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_legitimacy_kernel__reliability_primacy_reading, '17b93830-3d31-4bf7-982b-53bb96288f7a').
narrative_ontology:cs_kernel_codification('17b93830-3d31-4bf7-982b-53bb96288f7a', distributed).
narrative_ontology:cs_authority_grounding('17b93830-3d31-4bf7-982b-53bb96288f7a', distributed).
narrative_ontology:cs_reading_relation('17b93830-3d31-4bf7-982b-53bb96288f7a', technology_legitimacy_kernel__velocity_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('17b93830-3d31-4bf7-982b-53bb96288f7a', technology_legitimacy_kernel__precautionary_reading, influences).
narrative_ontology:cs_axiom('17b93830-3d31-4bf7-982b-53bb96288f7a', foundational, grid_stability_is_the_binding_constraint).
narrative_ontology:cs_axiom_status(grid_stability_is_the_binding_constraint, holdable).
narrative_ontology:cs_axiom_grounding('17b93830-3d31-4bf7-982b-53bb96288f7a', grid_stability_is_the_binding_constraint, instrumental).
narrative_ontology:cs_axiom('17b93830-3d31-4bf7-982b-53bb96288f7a', secondary, dispatchability_is_the_correct_proxy_for_reliability).
narrative_ontology:cs_axiom_status(dispatchability_is_the_correct_proxy_for_reliability, holdable).
narrative_ontology:cs_axiom_grounding('17b93830-3d31-4bf7-982b-53bb96288f7a', dispatchability_is_the_correct_proxy_for_reliability, empirically_contingent).
narrative_ontology:cs_reference_frame('17b93830-3d31-4bf7-982b-53bb96288f7a', grid_engineering_adequacy_standard).
narrative_ontology:cs_drift_state('17b93830-3d31-4bf7-982b-53bb96288f7a', post_variable_renewable_capacity_market_reform_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('17b93830-3d31-4bf7-982b-53bb96288f7a', '').
narrative_ontology:cs_kernel_id(technology_legitimacy_kernel__reliability_primacy_reading, technology_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, incumbent_nuclear_operators).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, gas_peaker_fleet_owners).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, grid_reliability_regulators).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__reliability_primacy_reading, ratepayers_bearing_capacity_charges).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__reliability_primacy_reading, distributed_solar_developers).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__reliability_primacy_reading, wind_developers_without_storage_capital).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, grid_operators).
narrative_ontology:constraint_vindicates(technology_legitimacy_kernel__reliability_primacy_reading, grid_stability_is_the_binding_constraint_on_decarbonization).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate high-capacity-factor plants that automatically clear the dispatchable/baseload test this reading imposes. Qualify for capacity payments, favorable interconnection queuing, and climate-finance labeling under reliability-primacy rules without having to build new storage or firming assets. Their existing fleets appreciate in policy value the moment reliability becomes the legitimacy gate.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, incumbent_nuclear_operators, beneficiary,
    institutional, generational, arbitrage, national).

% Gas peaking and combined-cycle plants are dispatchable on demand and frequently classified as 'transition-legitimate' bridge technology under reliability-primacy criteria, despite continuing carbon emissions. They benefit from capacity-market payments structured around dispatchability rather than emissions intensity.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, gas_peaker_fleet_owners, beneficiary,
    powerful, biographical, mobile, national).

% Write and enforce interconnection standards, capacity-market rules, and resource-adequacy tests that operationalize the dispatchable/baseload legitimacy criterion. Their institutional mandate is grid stability, which they administer through reliability metrics that determine which generation sources are treated as bankable, insurable, and policy-favored.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, grid_reliability_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Pay capacity charges and reliability premiums embedded in electricity bills that fund the dispatchable-generation requirement, whether or not marginal reliability risk justifies the specific spend. Cannot choose their utility's resource mix or opt out of capacity-market cost allocation; bear the cost of a legitimacy standard set by regulators and cleared by incumbents.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, ratepayers_bearing_capacity_charges, payer,
    powerless, biographical, trapped, regional).

% Build low-capacity-factor, non-dispatchable generation that fails the reliability-primacy legitimacy test on its own terms. Must either pair with storage they cannot always finance, accept curtailment and reduced interconnection priority, or be excluded from climate-mitigation credit and capacity-market compensation altogether — despite genuine carbon displacement.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, distributed_solar_developers, payer,
    moderate, biographical, constrained, regional).

% Face the same structural exclusion as solar developers: intermittent output disqualifies projects from being counted as 'legitimate' baseload contributors unless firmed with storage or paired contracts, raising capital costs and slowing deployment relative to what the carbon-budget timeline would otherwise allow.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, wind_developers_without_storage_capital, payer,
    moderate, biographical, constrained, regional).

% Manage real-time balancing and have an operational stake in dispatchability that is genuine and not merely rhetorical — frequency and voltage stability are real engineering constraints. They both validate the reliability-primacy framing from direct operational experience and benefit from a legitimacy standard that simplifies their dispatch and adequacy planning.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, grid_operators, observer,
    institutional, immediate, analytical, regional).
narrative_ontology:stakeholder_secondary_role(technology_legitimacy_kernel__reliability_primacy_reading, grid_operators, beneficiary).

% Argue that gating legitimacy on dispatchability slows deployment of the fastest-scaling, cheapest carbon-displacing technologies during the decade that matters most for cumulative emissions. Their objection is structurally excluded from capacity-market and interconnection rulemaking, which is conducted primarily among incumbents, regulators, and grid operators.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, climate_advocates_prioritizing_deployment_speed, excluded,
    organized, civilizational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(technology_legitimacy_kernel__reliability_primacy_reading, diffuse).
narrative_ontology:fixing_cost_class(technology_legitimacy_kernel__reliability_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared technical standard so that grid planners, financiers, and regulators can agree on which generation sources can be relied upon to keep the lights on, preventing a decarbonized grid from becoming an unreliable one.
% TRANSFER_FUNCTION: Moves capacity-market payments and favorable interconnection treatment toward dispatchable incumbents (nuclear, gas peakers) and moves compliance costs, curtailment risk, and exclusion from legitimacy credit onto intermittent renewable developers and, ultimately, onto ratepayers who fund the capacity premium.
% ABSENT_VOICES: Deployment-speed climate advocates and low-income ratepayer advocates are largely absent from the technical rulemaking bodies (reliability councils, capacity-market design committees) that operationalize this legitimacy test; their objection that reliability criteria entrench slower-scaling incumbents is raised in academic and advocacy literature but rarely inside the standard-setting process itself.
% DISAPPEARANCE_RATIONALE: Grid operators and regulators would say the physical requirement for dispatchable capacity does not disappear even if the legitimacy label does — frequency and adequacy constraints are real engineering facts. Renewable developers and deployment-speed advocates would say the LABEL's disappearance would rearrange capital flows substantially, freeing intermittent projects from storage-pairing requirements and capacity-market exclusion, accelerating deployment even if operators had to solve stability through other, less legitimacy-gated mechanisms (demand response, transmission buildout, market redesign).
% FOUNDING_PROBLEM: As variable renewable penetration rose, grid operators faced genuine new stability risks (frequency response, ramping, resource adequacy) that legacy baseload-dominated systems had not needed to manage explicitly; the reliability-primacy standard was built to ensure decarbonization did not come at the cost of blackout risk.
% FOUNDING_PROBLEM_CORROBORATION: Grid operators and independent system reliability councils (outside the beneficiary set of nuclear and gas incumbents) corroborate that dispatchability is a genuine, live engineering constraint, not merely a pretext — documented in resource-adequacy studies and blackout post-mortems. However, energy economists and storage-technology analysts, also outside the incumbent beneficiary set, corroborate that the specific dispatchable/baseload framing (rather than a broader firm-capacity-including-storage-and-demand-response framing) systematically favors incumbent asset classes beyond what the underlying engineering problem requires.
narrative_ontology:disappearance_verdict(technology_legitimacy_kernel__reliability_primacy_reading, contested).
narrative_ontology:founding_problem_status(technology_legitimacy_kernel__reliability_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_legitimacy_kernel__reliability_primacy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(technology_legitimacy_kernel__reliability_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_legitimacy_kernel__reliability_primacy_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_legitimacy_kernel__reliability_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(technology_legitimacy_kernel__reliability_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(technology_legitimacy_kernel__reliability_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction rises from 0.38 to 0.58 over the interval as capacity-market mechanisms matured and became the dominant vehicle for translating the reliability standard into cash flows toward dispatchable incumbents. Theater ratio is moderate (0.31 at T=20) — the underlying engineering concern about frequency and adequacy is genuine, not primarily performative, but a growing share of resource-adequacy proceedings function as venue for incumbents to relitigate legitimacy criteria in their favor rather than to solve new stability problems. Suppression climbs from 0.34 to 0.52 as capacity-market rules hardened into binding eligibility gates that intermittent developers cannot bypass without storage capital.
 *
 * PERSPECTIVAL GAP:
 *   From the grid-operator seat, this constraint reads as necessary engineering discipline: someone has to guarantee frequency response and adequacy, and dispatchability is the physically grounded way to do it. From the intermittent-developer seat, the identical rule reads as a legitimacy test rigged by asset-class definition — carbon displacement that doesn't fit the dispatchable mold is treated as provisionally illegitimate regardless of its actual climate benefit. The engine should compute these as structurally different experiences of the same rule: the reliability function is real (hence tangled_rope rather than snare), but the specific gate chosen concentrates benefit on incumbents whose asset base predates the climate mandate.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear operators and gas-peaker owners are declared beneficiaries because their existing asset characteristics — not new investment — satisfy the legitimacy test; the reading rewards a pre-existing capital stock. Ratepayers and intermittent-renewable developers are victims: ratepayers pay capacity premiums they did not choose and cannot avoid (trapped exit), while renewable developers pay through exclusion, curtailment, and forced storage-pairing costs (constrained exit — they can build storage if capital allows, but this is not free entry). Grid reliability regulators sit as agenda_setter: they administer the standard and could, in principle, redefine the eligibility criterion to include storage-paired and demand-response resources as equally 'legitimate,' but the cost of redesigning capacity-market rules against entrenched incumbent expectations is high, which is why gain_flow is authored diffuse rather than naming regulators as capturers — the capacity payments flow to asset owners, not to the rule-writers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — genuine new stability risk from renewable penetration — remains partially live (grid operators corroborate this independently), which is why founding_problem_status is authored contested rather than dead. This prevents misclassifying the constraint as pure extraction: there is a real coordination function still operating. But the specific operationalization (dispatchable/baseload rather than broader firm-capacity-inclusive-of-storage) has drifted toward favoring incumbent asset classes beyond what the engineering problem strictly requires, which is the extraction the tangled_rope classification is built to capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dispatchability_vs_firm_capacity_framing,
    'Is ''dispatchable, baseload-capable generation'' the narrowest defensible operationalization of the genuine grid-stability coordination problem, or is it a framing choice that could equally be satisfied by a broader ''firm capacity'' standard (storage-paired renewables, demand response, transmission-enabled imports) without loss of reliability?',
    'Comparative resource-adequacy modeling: compare system reliability outcomes under a dispatchable-only eligibility standard versus a firm-capacity-inclusive standard at equivalent cost, using existing interconnection studies from jurisdictions that have adopted storage-inclusive capacity accreditation (e.g., ERCOT, CAISO reforms).',
    'If firm-capacity-inclusive standards achieve equivalent reliability at lower system cost, the dispatchable/baseload gate is revealed as narrower than the coordination problem requires — strengthening the case that incumbent-favoring extraction, not stability necessity, drives the specific framing. If dispatchable-only standards are shown necessary for adequacy at current storage cost curves, the reading''s coordination claim is substantially vindicated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dispatchability_vs_firm_capacity_framing, empirical, 'Whether the specific dispatchable/baseload criterion is engineering-necessary or an incumbent-favoring framing choice among functionally equivalent reliability standards.').

omega_variable(
    kernel_reading_selection_bias,
    'Given that three readings of the technology legitimacy kernel exist (reliability-primacy, velocity-primacy, precautionary), what determines which reading a given regulatory body, financier, or advocacy coalition adopts — and is that selection itself capturable by the constituency that benefits from a given reading?',
    'Trace which institutional actors (capacity-market designers vs. carbon-budget modelers vs. environmental-risk agencies) championed each reading historically, and whether personnel or funding overlap exists between reading-advocates and the beneficiary sets each reading produces.',
    'If reading-selection correlates strongly with which actors benefit from the resulting legitimacy test, the kernel-reading structure itself becomes evidence of motivated framing rather than genuine normative disagreement about climate mitigation strategy — this would apply symmetrically across all three sibling readings, not uniquely to this one.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_bias, conceptual, 'Whether kernel-reading selection tracks genuine normative disagreement or beneficiary self-selection across the three sibling readings.').

omega_variable(
    storage_cost_curve_trajectory,
    'As battery and long-duration storage costs continue to fall, at what point does the dispatchable/baseload distinction become moot because storage-paired intermittent generation achieves cost-competitive dispatchability on its own terms?',
    'Track levelized cost of storage-paired renewable firm capacity against nuclear and gas-peaker capacity payments over a rolling five-year window; identify the crossover point if any.',
    'If storage costs fall below the crossover threshold, the reliability-primacy reading''s practical extraction (excluding renewables from legitimacy credit) becomes self-liquidating — renewables qualify on the same terms without policy change. This would resolve much of the current victim-class harm through market dynamics rather than through kernel reinterpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(storage_cost_curve_trajectory, empirical, 'Whether falling storage costs will dissolve the reliability-primacy reading''s exclusionary effect without requiring a change in the legitimacy standard itself.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_legitimacy_kernel__reliability_primacy_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t0, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(tech_tr_t4, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 4, 0.21).
narrative_ontology:measurement(tech_tr_t8, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement(tech_tr_t12, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 12, 0.27).
narrative_ontology:measurement(tech_tr_t16, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 16, 0.29).
narrative_ontology:measurement(tech_tr_t20, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 20, 0.31).

% Extraction over time
narrative_ontology:measurement(tech_be_t0, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(tech_be_t4, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 4, 0.44).
narrative_ontology:measurement(tech_be_t8, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 8, 0.49).
narrative_ontology:measurement(tech_be_t12, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 12, 0.53).
narrative_ontology:measurement(tech_be_t16, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 16, 0.56).
narrative_ontology:measurement(tech_be_t20, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t0, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 0, 0.34).
narrative_ontology:measurement(tech_su_t4, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 4, 0.39).
narrative_ontology:measurement(tech_su_t8, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 8, 0.44).
narrative_ontology:measurement(tech_su_t12, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 12, 0.47).
narrative_ontology:measurement(tech_su_t16, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 16, 0.5).
narrative_ontology:measurement(tech_su_t20, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_legitimacy_kernel__reliability_primacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(technology_legitimacy_kernel__reliability_primacy_reading, 0.12).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__reliability_primacy_reading, technology_legitimacy_kernel__velocity_primacy_reading).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__reliability_primacy_reading, technology_legitimacy_kernel__precautionary_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the technology_legitimacy_kernel. Each reading gates climate-mitigation legitimacy on a different criterion (reliability/dispatchability here; deployment velocity in the sibling; bounded reversibility of failure modes in the other sibling), producing different beneficiary and victim sets from the same underlying policy question of 'which technologies count as legitimate climate mitigation.' They are linked, not merged, per the ε-invariance principle: forcing them into one constraint would average away the structurally distinct winners and losers each reading produces.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
