% ============================================================================
% CONSTRAINT STORY: technology_legitimacy_kernel__velocity_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   This story instantiates the velocity-primacy reading of the technology
 *   legitimacy kernel: a technology counts as legitimate climate mitigation
 *   if and only if it can be deployed at scale within the remaining carbon
 *   budget timeline (2030/2050). This reading structurally favors
 *   fast-construction renewables and storage manufacturing over
 *   slow-construction nuclear and over long-horizon storage research, and it
 *   externalizes the cost of the resulting intermittency onto grid operators
 *   and baseload-dependent industry, who did not choose the criterion and
 *   cannot exit it. The coordination function is real — the carbon budget
 *   genuinely is finite and shrinking, and SOME prioritization criterion
 *   among competing mitigation technologies is needed — but the specific
 *   criterion chosen concentrates legitimacy, capital, and policy priority on
 *   a subset of stakeholders (renewables developers, storage manufacturers,
 *   renewables finance) while imposing borne, uncompensated reliability and
 *   stranded-asset costs on another (grid operators, nuclear developers,
 *   industrial regions, and future generations who inherit the resulting
 *   system).
 *
 * KEY AGENTS:
 *   - utility_scale_solar_developers: Primary beneficiary (organized/mobile) — captures policy legitimacy and financing preference from the velocity criterion
 *   - grid_operators: Primary payer (institutional/trapped) — absorbs the real-time reliability cost of a fast-deploying but intermittent generation mix
 *   - nuclear_developers: Structurally excluded payer (powerful/constrained) — technology performance is irrelevant if construction timeline alone disqualifies it
 *   - climate_policy_bodies: Agenda-setter (institutional/analytical) — sets and enforces the timeline criterion as the operative legitimacy test
 *   - future_generations_post_2050: Excluded (powerless/trapped) — inherit consequences of the criterion with no seat in its construction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_legitimacy_kernel__velocity_primacy_reading, 0.58).
domain_priors:suppression_score(technology_legitimacy_kernel__velocity_primacy_reading, 0.52).
domain_priors:theater_ratio(technology_legitimacy_kernel__velocity_primacy_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_legitimacy_kernel__velocity_primacy_reading, tangled_rope).
narrative_ontology:human_readable(technology_legitimacy_kernel__velocity_primacy_reading, "Deployment-Velocity Legitimacy Test for Climate Technology").
narrative_ontology:topic_domain(technology_legitimacy_kernel__velocity_primacy_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(technology_legitimacy_kernel__velocity_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_legitimacy_kernel__velocity_primacy_reading, '2cfc4518-43c9-45ad-abdf-2f3c05efd8c7').
narrative_ontology:cs_kernel_codification('2cfc4518-43c9-45ad-abdf-2f3c05efd8c7', distributed).
narrative_ontology:cs_authority_grounding('2cfc4518-43c9-45ad-abdf-2f3c05efd8c7', distributed).
narrative_ontology:cs_reading_relation('2cfc4518-43c9-45ad-abdf-2f3c05efd8c7', technology_legitimacy_kernel__reliability_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('2cfc4518-43c9-45ad-abdf-2f3c05efd8c7', technology_legitimacy_kernel__precautionary_reading, influences).
narrative_ontology:cs_axiom('2cfc4518-43c9-45ad-abdf-2f3c05efd8c7', foundational, timeline_urgency_overrides_dispatchability).
narrative_ontology:cs_axiom_status(timeline_urgency_overrides_dispatchability, holdable).
narrative_ontology:cs_axiom_grounding('2cfc4518-43c9-45ad-abdf-2f3c05efd8c7', timeline_urgency_overrides_dispatchability, empirically_contingent).
narrative_ontology:cs_axiom('2cfc4518-43c9-45ad-abdf-2f3c05efd8c7', secondary, construction_speed_is_the_binding_constraint_on_mitigation).
narrative_ontology:cs_axiom_status(construction_speed_is_the_binding_constraint_on_mitigation, holdable).
narrative_ontology:cs_axiom_grounding('2cfc4518-43c9-45ad-abdf-2f3c05efd8c7', construction_speed_is_the_binding_constraint_on_mitigation, instrumental).
narrative_ontology:cs_reference_frame('2cfc4518-43c9-45ad-abdf-2f3c05efd8c7', paris_aligned_carbon_budget_framework).
narrative_ontology:cs_drift_state('2cfc4518-43c9-45ad-abdf-2f3c05efd8c7', post_2020s_grid_integration_evidence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2cfc4518-43c9-45ad-abdf-2f3c05efd8c7', '').
narrative_ontology:cs_kernel_id(technology_legitimacy_kernel__velocity_primacy_reading, technology_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__velocity_primacy_reading, utility_scale_solar_developers).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__velocity_primacy_reading, wind_developers).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__velocity_primacy_reading, battery_storage_manufacturers).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__velocity_primacy_reading, renewables_finance_sector).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, grid_operators).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, nuclear_developers).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, baseload_dependent_industrial_regions).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, long_duration_storage_researchers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build projects that can go from permit to megawatt-hours in 18-36 months. Under the velocity test they are automatically legitimate climate technology, which channels subsidy, permitting priority, and capital toward them regardless of the grid integration costs their intermittency creates elsewhere.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, utility_scale_solar_developers, beneficiary,
    organized, biographical, mobile, global).

% Share the same fast-deployment profile as solar. Benefit from policy frameworks, auctions, and investment tax treatment that explicitly reward construction speed as the legitimacy criterion.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, wind_developers, beneficiary,
    organized, biographical, mobile, global).

% Positioned as the fix for intermittency, which lets them capture both the renewables buildout and the mitigation-cost problem it creates. Fast manufacturing scaling fits the velocity criterion even though duration and grid-scale performance are unresolved.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, battery_storage_manufacturers, beneficiary,
    organized, biographical, mobile, global).

% Prices climate-technology legitimacy directly into capital allocation: projects that clear the velocity test get cheaper financing, green-bond eligibility, and faster permitting support. Helps write and lobby for the deployment-timeline framing because it de-risks their existing renewables portfolios.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, renewables_finance_sector, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(technology_legitimacy_kernel__velocity_primacy_reading, renewables_finance_sector, agenda_setter).

% Must keep the lights on with an increasingly intermittent generation mix that was legitimized by deployment speed rather than dispatchability. Absorb the real-time balancing costs, curtailment, and reserve-capacity procurement that the velocity criterion does not price into any developer's legitimacy score. Cannot opt out of the grid they operate.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, grid_operators, payer,
    institutional, immediate, trapped, national).

% Build technology with multi-decade construction timelines and long asset lives that structurally fail the 2030/2050 deployment-velocity test regardless of eventual carbon performance. Are excluded from green taxonomies, financing preference, and policy legitimacy on timeline grounds alone, even where lifecycle emissions are comparable to or better than renewables.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, nuclear_developers, payer,
    powerful, generational, constrained, national).

% Host heavy industry (steel, chemicals, aluminum smelting) that requires firm, continuous power. Face rising costs and reliability risk as policy legitimacy is redirected toward technologies that cannot yet guarantee the baseload their industries depend on. Cannot relocate industrial capital stock on the same timeline as the policy shift.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, baseload_dependent_industrial_regions, payer,
    moderate, biographical, trapped, regional).

% Work on technologies (iron-air batteries, thermal storage, hydrogen) that could resolve intermittency at the horizon the velocity test defers past, but whose own deployment timelines are longer than what current legitimacy criteria reward. Underfunded relative to near-term-deployable options because the kernel's own timeline horizon disqualifies slower-maturing solutions before they mature.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, long_duration_storage_researchers, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(technology_legitimacy_kernel__velocity_primacy_reading, long_duration_storage_researchers, excluded).

% Set and enforce the deployment-timeline legitimacy criterion through IPCC-aligned carbon budgets, national NDCs, and green taxonomy rules. Justify the criterion as the only defensible response to a fixed, shrinking carbon budget, but in practice determine which technologies receive institutional legitimacy and which are read out of the mitigation portfolio.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, climate_policy_bodies, agenda_setter,
    institutional, civilizational, analytical, global).

% Bear the consequences of whichever mitigation portfolio the 2030/2050-timeline legitimacy test selects for today, including any reliability gaps, stranded-asset costs, or missed lower-carbon-but-slower alternatives, without having had any seat in the criterion's construction.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, future_generations_post_2050, excluded,
    powerless, civilizational, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, auditable criterion — deployability within the remaining carbon budget window — that lets policymakers, investors, and grid planners agree on which technologies count as legitimate climate mitigation without adjudicating every technology's merits case by case against a shrinking, genuinely finite timeline.
% TRANSFER_FUNCTION: Moves policy legitimacy, subsidy eligibility, green financing terms, and permitting priority toward fast-construction generation technologies (solar, wind, batteries) and away from slow-construction technologies (nuclear, long-duration storage), while shifting the operational cost of managing the resulting intermittency onto grid operators and baseload-dependent industry.
% ABSENT_VOICES: Grid reliability engineers and long-horizon storage researchers are structurally under-weighted in the criterion's construction because the timeline itself excludes technologies that mature past 2030/2050; future generations who inherit whatever reliability or stranded-asset consequences follow have no seat in setting the criterion at all.
% DISAPPEARANCE_RATIONALE: If deployment-velocity ceased to be the operative legitimacy test, capital and policy priority would redistribute toward technologies scored on lifecycle emissions, dispatchability, or bounded risk instead — nuclear and long-duration storage projects currently excluded from green taxonomies would become financeable, and the current renewables-financing advantage would contract to reflect performance rather than construction speed alone.
% FOUNDING_PROBLEM: The remaining carbon budget to hold warming to agreed targets is genuinely fixed and shrinking; policymakers needed some way to prioritize among a large menu of candidate mitigation technologies under real time pressure, rather than funding everything indefinitely while emissions continued.
% FOUNDING_PROBLEM_CORROBORATION: Climate policy bodies and renewables financiers attest the timeline criterion remains necessary given the physical carbon budget. Grid reliability engineers, independent energy-system modelers, and nuclear-sector analysts outside the renewables-financing constituency attest that the criterion, applied strictly, produces reliability and stranded-cost problems the founding carbon-budget rationale never specified as an acceptable tradeoff — corroboration exists on both sides of the contest, which is why status is authored contested rather than resolved.
narrative_ontology:disappearance_verdict(technology_legitimacy_kernel__velocity_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_legitimacy_kernel__velocity_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_legitimacy_kernel__velocity_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored at 0.58 (rising from 0.30 over the interval) because as the velocity criterion has hardened into taxonomy and financing rules, the transfer from excluded/burdened technologies and grid operators to favored developers has become larger and more concentrated. Suppression at 0.52 reflects active enforcement through green-taxonomy classification, subsidy eligibility rules, and permitting priority that actively excludes slow-construction alternatives rather than merely failing to reward them. Theater ratio is comparatively low (0.28) because the underlying coordination function — prioritizing among technologies given a genuinely fixed carbon budget — is real and substantially functional, not primarily performative; the extraction rides on top of a genuine coordination problem rather than replacing it.
 *
 * DIRECTIONALITY LOGIC:
 *   Renewables developers, storage manufacturers, and renewables finance sit near the beneficiary end: the criterion was substantially shaped by and for their deployment profile, and they capture the resulting legitimacy premium with mobile/arbitrage exit options if policy shifts. Grid operators sit near the full-target end: institutional power but trapped exit — they cannot decline to operate the grid the criterion produces, and the reliability costs land on them directly. Nuclear developers and long-duration storage researchers are targets by structural exclusion rather than by extraction of ongoing rents — the criterion's timeline horizon disqualifies them by construction regardless of their technology's merits. Future generations are the most powerless targets: civilizational time horizon, trapped exit, zero voice in criterion construction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a genuinely fixed, shrinking carbon budget requiring some prioritization criterion — remains live in the strict physical sense; this prevents a naive story of first-generation extraction. But the specific instantiation (deployment-velocity-by-2030/2050 as necessary AND sufficient legitimacy) has drifted into serving concentrated deployment-ready capital interests beyond what the founding physical constraint alone requires. Grid reliability and lifecycle-emissions considerations that were part of the original mitigation problem have been progressively de-weighted in favor of the single deployability axis. The contested founding_problem_status captures this: the underlying carbon-budget math is undisputed, but whether THIS criterion is the correct or sole legitimate translation of that math into technology policy is actively contested by parties outside the beneficiary set.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    timeline_versus_lifecycle_tradeoff,
    'Does prioritizing deployment velocity over dispatchability or lifecycle risk actually minimize cumulative emissions within the carbon budget, or does it optimize a proxy (construction speed) that diverges from the actual target (cumulative emissions avoided) once grid-balancing fossil backup and storage manufacturing emissions are counted?',
    'Full-system lifecycle and grid-integration modeling comparing cumulative emissions under a velocity-primacy portfolio versus a mixed-technology portfolio including slower-build nuclear, across the 2030-2050 window, including backup generation and curtailment effects.',
    'If velocity-primacy systematically produces higher cumulative emissions than a mixed portfolio once integration costs are counted, the criterion is a Goodhart substitution — legitimating the proxy (speed) rather than the actual target (avoided emissions) — which would reclassify much of the measured extraction as pure rent capture rather than genuine climate coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(timeline_versus_lifecycle_tradeoff, empirical, 'Whether deployment velocity is a valid proxy for or a substitution away from actual cumulative emissions reduction.').

omega_variable(
    kernel_reading_indeterminacy,
    'Is the technology_legitimacy_kernel genuinely indeterminate between its readings — such that reasonable policy communities can hold velocity-primacy, reliability-primacy, or precautionary readings simultaneously without one being objectively correct — or does the physical carbon-budget constraint itself favor one reading over the others?',
    'This is the committer-frame ambiguity for this constraint: the sibling readings (reliability_primacy_reading, precautionary_reading) are authored as separate constraints with their own beneficiary/victim structures per the ε-invariance principle. Resolution would require an authoritative meta-framework establishing whether ''legitimacy'' in climate technology governance is properly a matter of deployment speed, dispatchability, or bounded risk — a normative question the carbon-budget physics alone does not settle.',
    'If the kernel is genuinely indeterminate, this reading''s classification as tangled_rope is a structural fact about ANY one reading being chosen and enforced as if it were the sole legitimate translation of a physical constraint, rather than a fact about velocity-primacy specifically being wrong. If the physics does favor one reading, the other readings'' extraction becomes harder to defend as reasonable disagreement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether the kernel''s multiple readings reflect genuine value pluralism or whether the underlying physical constraint determines a correct reading.').

omega_variable(
    grid_operator_powerlessness_despite_institutional_power,
    'Grid operators are institutional-power actors in most classification schemes, yet they are authored here as trapped-exit payers bearing the extraction. Is institutional power atom classification adequately capturing their actual bargaining position relative to the policy bodies and financiers who set the legitimacy criterion?',
    'Comparative case analysis of grid operator regulatory standing versus renewables financing sector lobbying influence in the jurisdictions where green taxonomies were drafted — did grid operators have effective veto or amendment power over the criteria that now bind their operations?',
    'If grid operators had genuine influence over criterion-setting and simply lost the policy contest, this is ordinary contested policy-making, not extraction. If grid operators were structurally excluded from criterion-setting despite nominal institutional power, the tangled_rope classification is reinforced — institutional power did not translate into voice on this specific structural feature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(grid_operator_powerlessness_despite_institutional_power, empirical, 'Whether grid operators'' institutional power atom accurately reflects their actual influence over the legitimacy criterion that binds them.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_legitimacy_kernel__velocity_primacy_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t0, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(tech_tr_t4, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 4, 0.16).
narrative_ontology:measurement(tech_tr_t8, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(tech_tr_t12, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 12, 0.23).
narrative_ontology:measurement(tech_tr_t16, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement(tech_tr_t20, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 20, 0.28).

% Extraction over time
narrative_ontology:measurement(tech_be_t0, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(tech_be_t4, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 4, 0.38).
narrative_ontology:measurement(tech_be_t8, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(tech_be_t12, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 12, 0.52).
narrative_ontology:measurement(tech_be_t16, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 16, 0.56).
narrative_ontology:measurement(tech_be_t20, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t0, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(tech_su_t4, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 4, 0.32).
narrative_ontology:measurement(tech_su_t8, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 8, 0.39).
narrative_ontology:measurement(tech_su_t12, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 12, 0.44).
narrative_ontology:measurement(tech_su_t16, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 16, 0.49).
narrative_ontology:measurement(tech_su_t20, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_legitimacy_kernel__velocity_primacy_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(technology_legitimacy_kernel__velocity_primacy_reading, 0.15).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__velocity_primacy_reading, technology_legitimacy_kernel__reliability_primacy_reading).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__velocity_primacy_reading, technology_legitimacy_kernel__precautionary_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of technology_legitimacy_kernel, decomposed per the epsilon-invariance principle because the natural-language concept 'legitimate climate technology' resolves to structurally distinct claims with different beneficiary/victim sets and different epsilon values depending on which criterion (deployment speed, dispatchability, or bounded risk) is treated as the legitimacy test. velocity_primacy_reading legitimates renewables/storage and marginalizes nuclear/long-duration storage on construction-timeline grounds; reliability_primacy_reading inverts this by legitimating dispatchable baseload sources and marginalizing intermittent renewables; precautionary_reading cuts across both by testing worst-case reversibility rather than speed or dispatchability. Each reading is authored as its own constraint with its own epsilon and stakeholder structure; they are linked here rather than merged because merging them would violate epsilon-invariance (a single 'technology legitimacy' constraint would need an unstable epsilon that shifts depending on which observable/criterion the observer selects).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
