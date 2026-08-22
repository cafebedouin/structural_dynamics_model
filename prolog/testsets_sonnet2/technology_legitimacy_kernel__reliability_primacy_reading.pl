% ============================================================================
% CONSTRAINT STORY: technology_legitimacy_kernel__reliability_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   constraint_id: technology_legitimacy_kernel__reliability_primacy_reading
 *   human_readable: Reliability-Primacy Reading of Climate Technology Legitimacy
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   Grid operators and reliability regulators, responding to genuine
 *   resource-adequacy events during high-renewable-penetration periods, have
 *   converged on dispatchability as the litmus test for whether a generation
 *   technology counts as a legitimate climate solution. This reframes nuclear
 *   and gas-with-capture as premier climate assets (high capacity factor,
 *   controllable output) while requiring wind and solar developers to
 *   internalize the cost of firming their output with storage or contracted
 *   backup to be treated as equally legitimate. The standard is defended
 *   purely on engineering grounds by its administrators, but its practical
 *   effect is to redirect capacity payments and preferential interconnection
 *   toward incumbent baseload owners and transmission utilities, while
 *   ratepayers and intermittent-resource developers bear the reliability
 *   premium.
 *
 * KEY AGENTS:
 *   - incumbent_nuclear_operators: institutional beneficiary reclassified as premier climate technology under this reading
 *   - gas_peaker_owners: powerful beneficiary collecting capacity-market revenue for firming service
 *   - grid_reliability_regulators: institutional agenda-setter administering the dispatchability gate as settled engineering
 *   - ratepayers_bearing_capacity_charges: powerless, trapped payer of the reliability premium
 *   - wind_developers_without_storage and distributed_solar_developers: moderate-power payers forced into costly storage pairing to qualify
 *   - velocity_primacy_advocates: excluded voice arguing deployment speed should be the test instead
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_legitimacy_kernel__reliability_primacy_reading, 0.58).
domain_priors:suppression_score(technology_legitimacy_kernel__reliability_primacy_reading, 0.62).
domain_priors:theater_ratio(technology_legitimacy_kernel__reliability_primacy_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_legitimacy_kernel__reliability_primacy_reading, tangled_rope).
narrative_ontology:human_readable(technology_legitimacy_kernel__reliability_primacy_reading, "Reliability-Primacy Reading of Climate Technology Legitimacy").
narrative_ontology:topic_domain(technology_legitimacy_kernel__reliability_primacy_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(technology_legitimacy_kernel__reliability_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_legitimacy_kernel__reliability_primacy_reading, '91afecc1-6d94-447f-9bbf-e9b638d9244a').
narrative_ontology:cs_kernel_codification('91afecc1-6d94-447f-9bbf-e9b638d9244a', distributed).
narrative_ontology:cs_authority_grounding('91afecc1-6d94-447f-9bbf-e9b638d9244a', practice).
narrative_ontology:cs_interpretation_layer_present('91afecc1-6d94-447f-9bbf-e9b638d9244a').
narrative_ontology:cs_reading_relation('91afecc1-6d94-447f-9bbf-e9b638d9244a', technology_legitimacy_kernel__velocity_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('91afecc1-6d94-447f-9bbf-e9b638d9244a', technology_legitimacy_kernel__precautionary_reading, influences).
narrative_ontology:cs_axiom('91afecc1-6d94-447f-9bbf-e9b638d9244a', foundational, grid_stability_requires_dispatchable_generation).
narrative_ontology:cs_axiom_status(grid_stability_requires_dispatchable_generation, holdable).
narrative_ontology:cs_axiom_grounding('91afecc1-6d94-447f-9bbf-e9b638d9244a', grid_stability_requires_dispatchable_generation, empirically_contingent).
narrative_ontology:cs_axiom('91afecc1-6d94-447f-9bbf-e9b638d9244a', secondary, capacity_factor_is_the_climate_legitimacy_metric).
narrative_ontology:cs_axiom_status(capacity_factor_is_the_climate_legitimacy_metric, holdable).
narrative_ontology:cs_axiom_grounding('91afecc1-6d94-447f-9bbf-e9b638d9244a', capacity_factor_is_the_climate_legitimacy_metric, instrumental).
narrative_ontology:cs_reference_frame('91afecc1-6d94-447f-9bbf-e9b638d9244a', engineering_reliability_consensus).
narrative_ontology:cs_drift_state('91afecc1-6d94-447f-9bbf-e9b638d9244a', post_storage_cost_decline_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('91afecc1-6d94-447f-9bbf-e9b638d9244a', '').
narrative_ontology:cs_kernel_id(technology_legitimacy_kernel__reliability_primacy_reading, technology_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, incumbent_nuclear_operators).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, gas_peaker_owners).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, grid_reliability_regulators).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, transmission_utility_shareholders).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__reliability_primacy_reading, ratepayers_bearing_capacity_charges).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__reliability_primacy_reading, distributed_solar_developers).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__reliability_primacy_reading, wind_developers_without_storage).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__reliability_primacy_reading, low_income_electricity_customers).
narrative_ontology:constraint_vindicates(technology_legitimacy_kernel__reliability_primacy_reading, grid_stability_is_the_binding_constraint_on_decarbonization).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own baseload nuclear fleets with high capacity factors. Under this reading their assets are reclassified from stranded-risk legacy generation to premier climate technology, unlocking capacity payments, life-extension subsidies, and preferential interconnection queue placement. They actively lobby standards bodies and utility commissions to codify dispatchability as the legitimacy test.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, incumbent_nuclear_operators, beneficiary,
    institutional, generational, arbitrage, national).

% Operate natural gas peaking plants that provide the dispatchable backup this reading requires whenever intermittent generation cannot self-supply reliability. They collect capacity-market revenue precisely because the standard defines reliability as the gate, and their political position strengthens each time an intermittent project is rejected or forced to add costly firming capacity.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, gas_peaker_owners, beneficiary,
    powerful, biographical, mobile, regional).

% Write and enforce interconnection standards, capacity-market rules, and reliability-must-run designations. They administer the dispatchability test as a technical necessity, citing blackout risk, and can revise or waive the standard but treat it as settled engineering rather than contested policy.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, grid_reliability_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Profit from rate-based investment in firming infrastructure, transmission upgrades, and capacity contracts justified by the reliability standard. Every megawatt of intermittent capacity that must be paired with paid firming resources expands their regulated asset base and guaranteed return.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, transmission_utility_shareholders, beneficiary,
    institutional, generational, arbitrage, national).

% Pay monthly capacity charges and firming surcharges passed through by utilities to satisfy the dispatchability requirement, regardless of whether the marginal reliability gain is worth the cost. They have no meaningful say in interconnection standards and cannot opt out of the regulated tariff structure.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, ratepayers_bearing_capacity_charges, payer,
    powerless, biographical, trapped, regional).

% Must pair projects with storage or curtailment agreements to satisfy dispatchability tests that were not designed with distributed, variable resources in mind. The added storage cost frequently makes otherwise-viable projects uneconomic, and appeals against the standard go through the same regulators who authored it.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, distributed_solar_developers, payer,
    moderate, biographical, constrained, regional).

% Operate large capacity factor but non-dispatchable fleets. Under this reading their generation is treated as a lesser climate contribution unless firmed, so they are pushed toward expensive battery pairings or accept below-market capacity credit, eroding project economics that were underwritten assuming full climate-legitimacy status.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, wind_developers_without_storage, payer,
    moderate, biographical, constrained, national).

% Bear a disproportionate share of capacity and firming surcharges as a percentage of income, with no ability to install their own storage or self-supply reliability. They experience the standard purely as a bill line item with no visible connection to grid outcomes they can observe or influence.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, low_income_electricity_customers, payer,
    powerless, immediate, trapped, local).

% Argue that deployment speed within the remaining carbon budget should be the legitimacy test, not dispatchability, and that the reliability standard is being used to slow-walk cheap intermittent buildout in favor of incumbent baseload assets. They are not seated on the standards bodies that set interconnection and capacity-market rules.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, velocity_primacy_advocates, excluded,
    organized, biographical, constrained, national).

% Study actual reliability margins, curtailment data, and storage cost trajectories independent of both incumbent and challenger lobbies. Their technical findings are cited by all sides but do not by themselves set the standard.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, independent_grid_engineers, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(technology_legitimacy_kernel__reliability_primacy_reading, diffuse).
narrative_ontology:fixing_cost_class(technology_legitimacy_kernel__reliability_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Grid stability genuinely requires some resources capable of matching supply to demand on short notice; a legitimacy test tied to dispatchability solves the real coordination problem of preventing frequency deviation and blackout risk in a system with rising variable generation.
% TRANSFER_FUNCTION: Moves capacity payments, firming surcharges, and preferential interconnection access from ratepayers and intermittent-generation developers to nuclear operators, gas peaker owners, and transmission utilities whose assets or business models satisfy the dispatchability test.
% ABSENT_VOICES: Velocity-primacy advocates and distributed-resource aggregators who would argue that storage costs are falling faster than the standard assumes, and that treating dispatchability as the sole legitimacy gate slows decarbonization, are not seated on the reliability regulators' rulemaking bodies.
% DISAPPEARANCE_RATIONALE: If the dispatchability-primacy standard vanished overnight, capacity markets built around firm-resource credit would need restructuring, nuclear life-extension subsidies tied to reliability framing would lose their justification, and intermittent developers currently paying for storage pairing to qualify would see project economics improve substantially; utility rate cases premised on firming investment would face immediate challenge.
% FOUNDING_PROBLEM: As variable renewable penetration rose, grid operators faced genuine frequency-stability and resource-adequacy events (e.g., regional near-misses and blackouts during extreme weather) that were plausibly linked to insufficient dispatchable capacity, prompting reliability-focused interconnection and capacity-market reform.
% FOUNDING_PROBLEM_CORROBORATION: Grid reliability regulators and incumbent generators attest the problem remains fully live, citing resource-adequacy assessments. Independent grid engineers and several regional reliability studies note that battery storage, demand response, and improved forecasting have narrowed the actual reliability gap considerably faster than the standard's cost assumptions reflect, suggesting the founding problem is partially but not wholly resolved and the standard now exceeds what current reliability needs require.
narrative_ontology:disappearance_verdict(technology_legitimacy_kernel__reliability_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_legitimacy_kernel__reliability_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_legitimacy_kernel__reliability_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.58) reflects a genuine coordination function — grid stability is a real technical constraint — layered with asymmetric extraction: the specific threshold chosen (dispatchability as necessary and sufficient) systematically favors asset classes incumbents already own while imposing a firming tax on cheaper, faster-to-deploy intermittent resources. Suppression (0.62) is moderate-high because alternative legitimacy framings (speed, reversibility) are structurally excluded from the rulemaking body that administers this standard, not because dispatchability itself is indefensible. Theater ratio (0.42) is elevated because a rising share of 'reliability' rhetoric in rate cases and lobbying now serves incumbent asset protection rather than measured grid need, as independent engineering studies show storage and demand-response closing much of the gap the standard assumes remains open.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear operators, gas peaker owners, and transmission utilities sit near the full-beneficiary end: the standard was drafted around their operational profile and each qualifies without added cost. Ratepayers and low-income customers sit near the full-target end: trapped exit, no voice in rulemaking, and a mandatory pass-through charge. Intermittent developers occupy an intermediate but target-leaning position — they can still participate but only after absorbing a storage-pairing cost the standard imposes specifically because their generation profile does not match the dispatchability test.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (real resource-adequacy risk during high-penetration events) was genuinely live when the standard was adopted. Its status is now contested: independent grid engineering evidence suggests falling storage costs and improved forecasting have narrowed the reliability gap considerably, while the standard's cost assumptions and capacity-market structures have not been correspondingly revised. This is the classic mandatrophy signature — a coordination structure whose founding technical justification is eroding while its extractive machinery (capacity payments, firming surcharges) continues unchanged. Classifying this as tangled_rope rather than snare or mountain preserves the genuine residual coordination function (some dispatchability need still exists) while flagging the asymmetric extraction riding on it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reliability_primacy_reading_identity,
    'This constraint is one reading (reliability_primacy_reading) of the technology_legitimacy_kernel. The sibling readings — velocity_primacy_reading and precautionary_reading — are separate constraint stories with independently authored ε, beneficiary/victim sets, and classifications. Where is the actual disagreement located structurally?',
    'The disagreement is located at the definition of the legitimacy test itself: reliability_primacy gates on dispatchability/baseload capability; velocity_primacy gates on deployable-at-scale-within-carbon-budget; precautionary gates on bounded/reversible failure modes. These are not three measurements of one constraint but three different admission criteria that select different technology portfolios (nuclear+gas favored here; solar+wind+storage favored under velocity_primacy; small-scale/reversible technologies favored under precautionary). Resolving which reading should govern actual policy requires a political/normative choice, not further measurement.',
    'If reliability_primacy is adopted as the governing legitimacy standard, nuclear and gas-with-firming are vindicated as climate technologies and intermittent renewables face structural cost penalties; if velocity_primacy governs instead, the beneficiary and victim sets substantially invert.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reliability_primacy_reading_identity, conceptual, 'Location of kernel disagreement: the admission criterion itself, not an empirical fact about any one technology.').

omega_variable(
    reliability_gap_magnitude,
    'How large is the actual residual reliability gap that dispatchable-only resources are needed to fill, given current and projected storage costs, demand response, and transmission interconnection?',
    'Independent regional reliability studies comparing modeled resource adequacy under storage-and-demand-response-heavy portfolios versus dispatchable-baseload-heavy portfolios, using updated (not decade-old) cost curves.',
    'A small residual gap would support reclassifying much of the current capacity-payment structure as extraction riding on an outdated technical premise (pushing toward snare); a large persistent gap would support the coordination function as genuinely load-bearing (supporting rope or tangled_rope with lower extraction weighting).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reliability_gap_magnitude, empirical, 'Whether the dispatchability requirement still tracks a real, sizeable reliability gap.').

omega_variable(
    nuclear_beneficiary_naturalness,
    'Is nuclear''s beneficiary status under this reading a natural consequence of genuine dispatchability physics, or is the standard''s specific threshold and cost-allocation design partly shaped by nuclear industry lobbying to produce that outcome?',
    'Trace the legislative and regulatory drafting history of capacity-market and interconnection dispatchability standards for evidence of nuclear-industry input relative to independent grid-engineering input.',
    'If the threshold was substantially shaped by beneficiary lobbying, the coordination story is weaker cover for extraction than claimed; if the threshold reflects independent engineering consensus that nuclear happens to satisfy, the coordination function is more genuine.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(nuclear_beneficiary_naturalness, empirical, 'Whether nuclear''s favorable treatment under this reading reflects physics or lobbying-shaped design.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_legitimacy_kernel__reliability_primacy_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t0, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(tech_tr_t4, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 4, 0.26).
narrative_ontology:measurement(tech_tr_t8, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement(tech_tr_t12, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(tech_tr_t16, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 16, 0.39).
narrative_ontology:measurement(tech_tr_t20, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 20, 0.42).

% Extraction over time
narrative_ontology:measurement(tech_be_t0, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(tech_be_t4, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 4, 0.42).
narrative_ontology:measurement(tech_be_t8, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(tech_be_t12, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 12, 0.52).
narrative_ontology:measurement(tech_be_t16, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 16, 0.56).
narrative_ontology:measurement(tech_be_t20, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t0, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(tech_su_t4, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 4, 0.46).
narrative_ontology:measurement(tech_su_t8, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 8, 0.51).
narrative_ontology:measurement(tech_su_t12, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 12, 0.55).
narrative_ontology:measurement(tech_su_t16, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 16, 0.59).
narrative_ontology:measurement(tech_su_t20, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 20, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_legitimacy_kernel__reliability_primacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(technology_legitimacy_kernel__reliability_primacy_reading, 0.12).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__reliability_primacy_reading, technology_legitimacy_kernel__velocity_primacy_reading).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__reliability_primacy_reading, technology_legitimacy_kernel__precautionary_reading).

% DUAL FORMULATION NOTE:
% This constraint, velocity_primacy_reading, and precautionary_reading form a three-member constraint family decomposing the colloquial 'technology legitimacy for climate mitigation' claim per the ε-invariance principle. Each reading applies a structurally distinct admission test (dispatchability vs. deployment speed vs. bounded reversibility) and therefore has its own ε, beneficiary/victim structure, and classification. This reading (reliability_primacy) authors ε=0.58 reflecting real but incumbent-skewed coordination cost; the sibling readings are expected to diverge substantially given their different beneficiary sets (fast-deploying intermittent developers under velocity_primacy; small-reversible-technology developers under precautionary). Do not average or reconcile ε across the family — link via network edges instead.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
